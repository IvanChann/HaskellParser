gb = float(3.14159)

mb = (gb * 1024)
kb = (mb * 1024)
b = (kb * 1024)

r_b = int(b % 1024)
r_kb = int(kb % 1024)
r_mb = int(mb % 1024)

print(r_kb)
print(int(gb), "GB", r_mb, "MB", r_kb, "KB", r_b, "B")