import json

file_path = "java/megavul_simple.json"

with open(file_path, "r", encoding="utf-8") as f:
    data = json.load(f)

print("총 샘플 개수:", len(data))

for item in data[:3]:
    print(json.dumps(item, indent=2)[:500])