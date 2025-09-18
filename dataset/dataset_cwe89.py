import json

dataset = []
vulnerable_templates = [
    "EXEC SQL.\n SELECT * FROM {table} WHERE name = :{var}\nENDEXEC.",
    "EXEC SQL.\n DELETE FROM {table} WHERE kunnr = :{var}\nENDEXEC.",
    "EXEC SQL.\n UPDATE {table} SET passwd = :{var} WHERE user = :{var}\nENDEXEC.",
    "EXEC SQL.\n EXECUTE IMMEDIATE 'SELECT * FROM {table} WHERE name = ''{var}''' \nENDEXEC.",
    "EXEC SQL.\n SELECT * FROM {table} WHERE email = '{var}'\nENDEXEC.",
    "EXEC SQL.\n SELECT * FROM {table} WHERE id IN ({var})\nENDEXEC."
    "EXEC SQL.\n SELECT * FROM {table} WHERE kunnr LIKE '{var}%'\nENDEXEC.",
    "SELECT * FROM ({table}) INTO TABLE lt_result WHERE name = {var}.",
    
    "DATA(lv_sql) = |SELECT * FROM {table} WHERE name = '{var}' OR 1=1|.\nEXEC SQL.\n  EXECUTE IMMEDIATE :lv_sql\nENDEXEC.",
    "DATA(lv_sql) = |DELETE FROM {table} WHERE id = {var} OR 'a'='a'|.\nEXEC SQL.\n  EXECUTE IMMEDIATE :lv_sql\nENDEXEC.",
    "DATA(lv_sql) = |UPDATE {table} SET passwd = '{var}' WHERE user = '{var}'|.\nEXEC SQL.\n  EXECUTE IMMEDIATE :lv_sql\nENDEXEC.",
    "DATA(lv_sql) = |SELECT * FROM {table} WHERE email = '{var}' -- comment|.\nEXEC SQL.\n  EXECUTE IMMEDIATE :lv_sql\nENDEXEC.",

    "DATA(lv_sql) = |SELECT * FROM {table} WHERE name LIKE '%{var}%'|.\nEXEC SQL.\n  EXECUTE IMMEDIATE :lv_sql\nENDEXEC.",
    "DATA(lv_sql) = |SELECT * FROM {table} WHERE name LIKE '%{var}%'|.\nEXEC SQL.\n  EXECUTE IMMEDIATE :lv_sql\nENDEXEC.",
    "DATA(lv_sql) = |SELECT * FROM {table} WHERE id IN ({var})|.\nEXEC SQL.\n  EXECUTE IMMEDIATE :lv_sql\nENDEXEC.",
    "DATA(lv_sql) = |SELECT * FROM {var} WHERE user = 'test'|.\nEXEC SQL.\n  EXECUTE IMMEDIATE :lv_sql\nENDEXEC.",
    "SELECT * FROM {table} INTO TABLE lt_result WHERE username = {var}.",
    "DATA(lv_sql) = |DELETE FROM {table} WHERE id = {var} OR 'x'='x'|.\nEXEC SQL.\n  EXECUTE IMMEDIATE :lv_sql\nENDEXEC."
]

safe_templates = [
    "EXEC SQL.\n SELECT * FROM {table} INTO :lv_result WHERE name = :{var}\nENDEXEC.",
    "EXEC SQL.\n DELETE FROM {table} WHERE kunnr = :{var} INTO :lv_result\nENDEXEC.",
    "EXEC SQL.\n UPDATE {table} SET passwd = :lv_new_passwd WHERE user = :lv_user\nENDEXEC.",
    "EXEC SQL.\n SELECT * FROM {table} INTO :lv_result WHERE email = :lv_email\nENDEXEC.",
    "EXEC SQL.\n SELECT * FROM {table} INTO :lv_result WHERE id = :lv_id\nENDEXEC.",
    "SELECT * FROM users INTO TABLE lt_result WHERE name = @lv_name.",
    
    "DATA(lv_name) = {var}.\nEXEC SQL.\n  SELECT * FROM {table} INTO :lv_result WHERE name = :lv_name\nENDEXEC.",
    "DATA(lv_id) = {var}.\nEXEC SQL.\n  DELETE FROM {table} WHERE id = :lv_id\nENDEXEC.",
    "DATA(lv_pass) = {var}.\nDATA(lv_user) = {var}.\nEXEC SQL.\n  UPDATE {table} SET passwd = :lv_pass WHERE user = :lv_user\nENDEXEC.",
    "DATA(lv_email) = {var}.\nEXEC SQL.\n  SELECT * FROM {table} INTO :lv_result WHERE email = :lv_email\nENDEXEC.",

    "DATA(lv_name) = {var}.\nEXEC SQL.\n  SELECT * FROM {table} INTO :lv_result WHERE name LIKE :lv_name\nENDEXEC.",
    "DATA(lv_id) = {var}.\nEXEC SQL.\n  SELECT * FROM {table} INTO :lv_result WHERE id = :lv_id\nENDEXEC.",
    "IF {var} = 'zusers' OR {var} = 'users'.\n  DATA(lv_table) = {var}.\n  EXEC SQL.\n    SELECT * FROM :lv_table INTO :lv_result WHERE user = 'test'\n  ENDEXEC.\nENDIF.",
    "SELECT * FROM {table} INTO TABLE lt_result WHERE username = @lv_user.",
    "DATA(lv_id) = {var}.\nEXEC SQL.\n  DELETE FROM {table} WHERE id = :lv_id\nENDEXEC."
]

tables = ["users", "zusers", "vbak", "kna1"]
vars = ["username", "lv_user", "p_name"]

id_counter = 1
for t in tables:
    for v in vars:
        for code in vulnerable_templates:
            dataset.append({
                "id": f"ABAP-cwe89-{id_counter}",
                "cwe": "CWE-89",
                "label": "CWE-89",
                "code": code.format(table=t, var=v)
            })
            id_counter += 1

        for code in safe_templates:
            dataset.append({
                "id": f"ABAP-cwe89-{id_counter}",
                "cwe": "CWE-89",
                "label": "safe",
                "code": code.format(table=t, var=v)
            })
            id_counter += 1

with open("abap_dataset.jsonl", "a", encoding="utf-8") as f:
    for item in dataset:
        f.write(json.dumps(item, ensure_ascii=False) + "\n")