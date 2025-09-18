from transformers import RobertaTokenizer, RobertaForSequenceClassification
import torch

labels = ["safe", "vulnerable"]
id2label = {i: label for i, label in enumerate(labels)}

# 학습된 모델 불러오기
model_path = "../abap_codebert_final"
model = RobertaForSequenceClassification.from_pretrained(model_path)

tokenizer = RobertaTokenizer.from_pretrained("microsoft/codebert-base")

device = torch.device("mps") if torch.backends.mps.is_available() else torch.device("cpu")
model.to(device)

test_code = "EXEC SQL.\n SELECT * FROM users WHERE name = :username\n ENDEXEC." # vulnerable
# test_code = "DATA : GV_TEXT1 VALUE 'A'.\n DATA : GV_TEXT2 LENGTH 3 TYPE C VALUE 'ABC'. \nDATA : GV_TEXT3(5) TYPE C VALUE 'ABCDE'.\n DATA : GV_TEXT4 TYPE STRING VALUE 'ABCDE'. \n DATA : GV_LEN TYPE i. \n DATA : GV_TIME TYPE T.\n DATA : GV_DATE TYPE D.\n GV_TIME = SY-UZEIT. \n GV_DATE = SY-DATUM. \n GV_LEN = STRLEN( GV_TEXT3 ). \n WRITE : GV_TEXT1. \n WRITE :/ GV_TEXT2. \n WRITE :/ GV_TEXT3. \n WRITE :/ GV_TEXT4. \n WRITE :/ GV_LEN. \n WRITE :/ GV_TIME. \n WRITE :/ GV_DATE."
inputs = tokenizer(test_code, return_tensors="pt", truncation=True, padding=True).to(device)

model.eval()
with torch.no_grad():
    outputs = model(**inputs)

predicted_label = id2label[torch.argmax(outputs.logits).item()]
print("Predicted:", predicted_label)