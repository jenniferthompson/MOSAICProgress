# MOSAICProgress
Dashboard for MOSAIC study monitoring.

To create the dashboards automatically, set up a cron job in the terminal like
this [(one `cron` reference guide)](https://www.ostechnix.com/a-beginners-guide-to-cron-jobs/):

```
30 9 * * 1 cd /my/file/path && Rscript mosaic_makedash.R
```

The above sets the working directory and runs `mosaic_makedash.R` every Monday
morning at 9:30.
