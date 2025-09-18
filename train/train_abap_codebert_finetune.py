import json
import torch
from torch.utils.data import Dataset
from transformers import RobertaTokenizer, RobertaForSequenceClassification, Trainer, TrainingArguments

# 데이터셋 정의
class AbapDataset(Dataset):
    def __init__(self, path, tokenizer, label2id, max_len=256):
        self.samples = []
        with open(path, "r", encoding="utf-8") as f:
            for line in f:
                line = line.strip()
                if not line:
                    continue
                item = json.loads(line)
                self.samples.append(item)
        self.tokenizer = tokenizer
        self.label2id = label2id
        self.max_len = max_len

    def __len__(self):
        return len(self.samples)

    def __getitem__(self, idx):
        code = self.samples[idx]["code"]
        label = self.samples[idx]["label"]
        encoding = self.tokenizer(
            code,
            max_length=self.max_len,
            padding="max_length",
            truncation=True,
            return_tensors="pt"
        )
        return {
            "input_ids": encoding["input_ids"].squeeze(),
            "attention_mask": encoding["attention_mask"].squeeze(),
            "labels": torch.tensor(self.label2id[label])
        }

# 라벨 매핑 정의
labels = ["safe", "CWE-89"]
label2id = {label: i for i, label in enumerate(labels)}
id2label = {i: label for label, i in label2id.items()}

# 토크나이저/모델 불러오기
checkpoint_path = "../abap_codebert_final"
tokenizer = RobertaTokenizer.from_pretrained(checkpoint_path)
model = RobertaForSequenceClassification.from_pretrained(
    checkpoint_path,
    num_labels=len(labels),
    id2label=id2label,
    label2id=label2id
)

# 데이터 로드
train_dataset = AbapDataset("../dataset/abap_dataset.jsonl", tokenizer, label2id)

# 학습 설정
training_args = TrainingArguments(
    output_dir="../abap_codebert_continued",
    learning_rate=2e-5,
    per_device_train_batch_size=8,
    num_train_epochs=5,
    weight_decay=0.01,
    logging_dir="./logs",
    logging_steps=1,
    save_strategy="epoch"
)

# 평가 지표
trainer = Trainer(
    model=model,
    args=training_args,
    train_dataset=train_dataset
)

trainer.train()

trainer.save_model("../abap_codebert_final")
tokenizer.save_pretrained("../abap_codebert_final")
