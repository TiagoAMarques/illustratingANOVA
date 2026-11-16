# ANOVA & Kruskal–Wallis Explorer

An interactive teaching app for generating grouped data, understanding the one-way ANOVA decomposition of sums of squares, and comparing ANOVA with the Kruskal–Wallis rank test.

The app runs entirely in the browser through [Shinylive](https://posit-dev.github.io/r-shinylive/) and webR. It does not require a Shiny server and can therefore be hosted on GitHub Pages.

## What students can explore

1. **Generate data** with 2–6 groups, varying sample size, effect size, spread, distribution shape, and random seed.
2. **Run one-way ANOVA** and see every deviation split visually into between-group and within-group components, alongside the identity `SS total = SS between + SS within`.
3. **Run Kruskal–Wallis** and inspect the pooled ranks, mean ranks, test statistic, and comparison with ANOVA.

## Deploy to GitHub Pages

1. Create an empty GitHub repository and push this folder to its `main` branch.
2. In the repository, open **Settings → Pages**.
3. Under **Build and deployment**, choose **GitHub Actions** as the source.
4. Open the **Actions** tab and wait for “Build and deploy Shinylive app” to finish.
5. The deployment URL appears in the completed workflow and in **Settings → Pages**.

The workflow in `.github/workflows/pages.yml` installs the exporter, converts `app/` to a static Shinylive site, and publishes it. Subsequent pushes to `main` redeploy automatically.

## Run locally with R

```r
install.packages("shiny")
shiny::runApp("app")
```

To test the exact static version locally:

```r
install.packages("shinylive", repos = c(
  "https://posit-dev.r-universe.dev",
  "https://cloud.r-project.org"
))
shinylive::export("app", "site")
httpuv::runStaticServer("site")
```

## Repository structure

```text
app/app.R                 Shiny application
app/www/styles.css        Visual design
.github/workflows/pages.yml  GitHub Pages deployment
```

## License

MIT
