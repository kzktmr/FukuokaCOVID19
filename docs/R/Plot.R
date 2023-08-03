library(ggplot2)

dat |> filter(name == "per_sentinel", region == "福岡県") |> 
  mutate(year_week = str_glue("{year}_{sprintf('%02d', week)}")) |> 
  ggplot() + aes(x = year_week, y = value) + geom_col() + 
  theme_bw() + labs(title = "福岡県新型コロナウイルス感染症定点当り報告数") +
  theme(plot.title = element_text(family = "筑紫A丸ゴシック"),
        axis.title = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5))

ggsave(paste("figure/figure_", today, ".png"), width = 1920, height = 1080, units = "px",
       dpi = "retina")

dat |> filter(name == "per_sentinel", region == "福岡県") |> 
  mutate(year_week = str_glue("{year}_{sprintf('%02d', week)}"),
         growth = value / lag(value)) |> drop_na() |> 
  ggplot() + aes(x = year_week, y = growth) + geom_line(aes(group = region), color = "navy") + 
  geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
  theme_bw() + labs(title = "福岡県新型コロナウイルス感染症定点当り報告数前週比") +
  scale_y_log10() +
  theme(plot.title = element_text(family = "筑紫A丸ゴシック"),
        axis.title = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5))

ggsave(paste("figure/growth_", today, ".png"), width = 1920, height = 1080, units = "px",
       dpi = "retina")
