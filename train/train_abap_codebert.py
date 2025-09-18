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
# labels = ["safe", "CWE-89", "CWE-79"]
labels = ["safe", "vulnerable"]
label2id = {label: i for i, label in enumerate(labels)}
id2label = {i: label for label, i in label2id.items()}

# 토크나이저/모델 불러오기
model_name = "microsoft/codebert-base"
tokenizer = RobertaTokenizer.from_pretrained(model_name)
model = RobertaForSequenceClassification.from_pretrained(
    model_name,
    num_labels=len(labels),
    id2label=id2label,
    label2id=label2id
)

# 데이터 로드
train_dataset = AbapDataset("../dataset/abap_dataset.jsonl", tokenizer, label2id)
# valid_dataset = AbapDataset("valid.jsonl", tokenizer, label2id)

# 학습 설정
# training_args = TrainingArguments(
#     output_dir = "./abap_codebert",
#     evaluation_strategy="epoch",
#     save_strategy="epoch",
#     learning_rate=2e-5,
#     per_device_train_batch_size=8,
#     per_device_eval_batch_size=8,
#     num_train_epochs=5,
#     weight_decay=0.01,
#     logging_dir="./logs",
#     logging_steps=50,
#     load_best_model_at_end=True,
#     metric_for_best_model="f1"
# )
training_args = TrainingArguments(
    output_dir="../abap_codebert",
    learning_rate=2e-5,
    per_device_train_batch_size=8,
    num_train_epochs=5,
    weight_decay=0.01,
    logging_dir="./logs",
    logging_steps=1
)

# 평가 지표
# from sklearn.metrics import accuracy_score, precision_recall_fscore_support
#
# def compute_metrics(pred):
#     labels = pred.label_ids
#     preds = pred.predictions.argmax(-1)
#     precision, recall, f1, _ = precision_recall_fscore_support(labels, preds, average="marco")
#     acc = accuracy_score(labels, preds)
#     return {"accuracy": acc, "precision": precision, "recall": recall, "f1": f1}

# Trainer 정의 및 학습
# trainer = Trainer(
#     model=model,
#     args=training_args,
#     train_dataset=train_dataset,
#     eval_dataset=valid_dataset,
#     compute_metrics=compute_metrics
# )
trainer = Trainer(
    model=model,
    args=training_args,
    train_dataset=train_dataset
)

trainer.train()

trainer.save_model("../abap_codebert_final")
tokenizer.save_pretrained("../abap_codebert_final")

# 추론
# import torch
#
# device = torch.device("mps") if torch.backends.mps.is_available() else torch.device("cpu")
# model.to(device)
#
# test_code = "EXEC SQL.\n SELECT * FROM users WHERE name = :username\nENDEXEC."
# inputs = tokenizer(test_code, return_tensors="pt", truncation=True, padding=True).to(device)
#
# model.eval()
# with torch.no_grad():
#     outputs = model(**inputs)
#
# predicted_label = id2label[torch.argmax(outputs.logits).item()]
# print("Predicted:", predicted_label)
