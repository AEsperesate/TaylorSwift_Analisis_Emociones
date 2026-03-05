# =====================================================
# Proyecto: Análisis de emociones de la discografía de Taylor Swift
# Script: lyrics_analysis.py
# Propósito:
#   Análisis de las emociones presentes en las letras de las canciones
#   de Taylor Swift con un modelo preentrenado. Los resultados se exportan
#   para su posterior análisis en R.
# Autor: Alicia esperesate
# Fecha: 2022-02-20
# =====================================================


# =====================================================
# CONFIGURACIÓN
# =====================================================

import re
from pathlib import Path
import pandas as pd
from transformers import pipeline, AutoTokenizer

BASE_DIR = Path(__file__).resolve().parent.parent

data_path = BASE_DIR / "data"
raw_path = data_path / "raw"
processed_path = data_path / "processed"

songs_clean_path = processed_path / "songs_clean"
albums_clean_path = processed_path / "albums_clean"
results_path = processed_path / "results"

# Crear carpetas si no existen
for p in [songs_clean_path, albums_clean_path, results_path]:
    p.mkdir(parents=True, exist_ok=True)

artist = "Taylor Swift"

MODEL_NAME = "j-hartmann/emotion-english-distilroberta-base"


# =====================================================
# FUNCIÓN DE LIMPIEZA
# =====================================================

def clean_lyrics(text: str) -> str:
    text = re.sub(r"\[.*?\]", "", text)   # eliminar [Chorus], [Verse]
    text = re.sub(r"\n+", " ", text)      # normalizar saltos de línea
    text = text.lower()                   # lowercasing
    return text.strip()


# =====================================================
# LIMPIEZA DE CANCIONES
# =====================================================

songs_data = []
song_id = 1

for album_folder in raw_path.iterdir():
    if not album_folder.is_dir():
        continue

    album_texts = []

    for song_file in sorted(album_folder.glob("*.txt")):
        clean_text = clean_lyrics(song_file.read_text(encoding="utf-8"))

        (songs_clean_path / song_file.name).write_text(clean_text, encoding="utf-8")

        # Guardar canción limpia

        songs_data.append({
            "song_id": song_id,
            "song": song_file.stem,
            "album": album_folder.name,
            "artist": artist,
            "clean_lyrics": clean_text
        })

        album_texts.append(clean_text)
        song_id += 1

    # Crear archivo limpio por álbum
    (albums_clean_path / f"{album_folder.name}_clean.txt") \
        .write_text("\n".join(album_texts), encoding="utf-8")

    # Crear DataFrame de canciones
songs_df = pd.DataFrame(songs_data)


# =====================================================
# MODELO DE EMOCIONES (PREENTRENADO)
# =====================================================

tokenizer = AutoTokenizer.from_pretrained(MODEL_NAME)
emotion_model = pipeline(
    "text-classification",
                         model=MODEL_NAME,
                         top_k=None
)

def get_emotions_batch(text):
    # Tokenizar con overflow
    inputs = tokenizer(
        text,
        max_length=tokenizer.model_max_length,
        truncation=True,
        return_overflowing_tokens=True
    )

    num_chunks = len(inputs["input_ids"])
    chunk_texts = [tokenizer.decode(ids, skip_special_tokens=True) for ids in inputs["input_ids"]]

    # Procesar todos los chunks de golpe
    outputs_list = emotion_model(chunk_texts)  # Devuelve lista de listas de diccionarios

    # Acumular scores
    emotion_accumulator = {}
    for outputs in outputs_list:  # outputs es una lista de diccionarios para cada chunk
        if not emotion_accumulator:
            emotion_accumulator = {o["label"]: 0.0 for o in outputs}
        for o in outputs:
            emotion_accumulator[o["label"]] += o["score"]

    # Promediar por número de chunks
    return {k: v / num_chunks for k, v in emotion_accumulator.items()}


# =====================================================
# FUNCIÓN GENÉRICA PARA EMOCIONES
# =====================================================

def compute_emotions(df, text_column, extra_cols):
    rows = []

    for _, row in df.iterrows():
        emotions = get_emotions_batch(row[text_column])
        for col in extra_cols:
            emotions[col] = row[col]
        rows.append(emotions)

    return pd.DataFrame(rows)

# Emociones por canción
emotions_songs_df = compute_emotions(
    songs_df,
    text_column="clean_lyrics",
    extra_cols=["song_id", "song", "album"]
)

# Emociones por álbum
album_rows = []

for album_file in albums_clean_path.glob("*_clean.txt"):
    album_name = album_file.stem.replace("_clean", "")
    text = album_file.read_text(encoding="utf-8")

    emotions = get_emotions_batch(text)
    album_rows.append({**emotions, "album": album_name})
emotions_album_df = pd.DataFrame(album_rows)


# =====================================================
# EXPORTAR RESULTADOS
# =====================================================

songs_df.to_csv(results_path / "songs_clean.csv", index=False)
emotions_songs_df.to_csv(results_path / "emotions_by_song.csv", index=False)
emotions_album_df.to_csv(results_path / "emotions_by_album.csv", index=False)

print("✅ Pipeline completado correctamente")
