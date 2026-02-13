<script>
document.addEventListener("DOMContentLoaded", function () {

  function decodeHtml(html) {
    const txt = document.createElement("textarea");
    txt.innerHTML = html;
    return txt.value;
  }

  document.querySelectorAll('[data-bibtex]').forEach(btn => {
    btn.addEventListener('click', function () {

      let text = this.getAttribute('data-bibtex');
      text = decodeHtml(text);

      // Normalize indentation: 2 spaces for all inner lines
      const lines = text.split("\n");

      const formatted = lines.map((line, i) => {
        if (i === 0 || line.trim() === "}") return line.trim();
        return "  " + line.trim();
      }).join("\n");

      navigator.clipboard.writeText(formatted).then(() => {
        const original = this.textContent;
        this.textContent = "✓ Copied";
        setTimeout(() => {
          this.textContent = original;
        }, 1200);
      }).catch(err => {
        console.error("Clipboard error:", err);
      });

    });
  });
});
</script>
