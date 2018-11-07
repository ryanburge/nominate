library(building)
library(car)

nom <- read_csv("115cong.csv")

nom$last <- gsub("([A-Za-z]+).*", "\\1", nom$bioname)

nom$last <-  tolower(nom$last)

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

nom$last <- firstup(nom$last)


font_add_google("Abel", "font")
showtext_auto()


nom %>% 
  filter(chamber == "Senate") %>% 
  ggplot(., aes(x= nominate_dim1, y =1, label = last)) +
  theme_minimal() +
  geom_point() +
  geom_line() +
  theme(text=element_text(size=45, family="font")) +
  geom_text_repel(data=subset(nom, nominate_dim1 > .75 & chamber == "Senate"),
                  aes(nominate_dim1,1,label=last), size = 11, family = "font") +
  geom_text_repel(data=subset(nom, nominate_dim1 < -.5 & chamber == "Senate"),
                  aes(nominate_dim1,1,label=last), size = 11, family = "font") +
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  labs(x = "DW-Nominate (First Dimension)", y = "", title = "Political Partisanship of Senators") +
  ggsave("first_nom.png", height = 2)


mod <- nom %>% 
  filter(nominate_dim1 < .01 & nominate_dim1 > -.194 & chamber == "Senate") %>% 
  select(last, nominate_dim1)

mod$grp <- c("Not on Ballot", "Won", "Lost", "Lost", "Won", "Lost", "Lost", "Won")

nom2 <- nom %>% 
  filter(chamber == "Senate") %>% 
  select(last, nominate_dim1)

nom2 <- anti_join(nom2, mod) %>% 
  mutate(grp = "WWW")

mod2 <- bind_rows(nom2, mod)

mod2 %>% 
  ggplot(., aes(x= nominate_dim1, y =1, color = grp)) +
  geom_hline(yintercept = 1) +
  theme_minimal() +
  geom_point() +
  geom_line() +
  theme(text=element_text(size=45, family="font")) +
  scale_x_continuous(limits = c(-1, 1)) +
  geom_text_repel(data = subset(mod2, grp != "WWW"),
                aes(nominate_dim1,1,label=last, color = grp), size = 14, family = "font", direction = "x", nudge_y = .01, show.legend = FALSE) +
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  labs(x = "DW-Nominate (First Dimension)", y = "", title = "Four of the Eight Most Moderate Democrat Senators Lost") +
  scale_fill_manual(values = c("firebrick3", "azure3", "dodgerblue3", "black")) +
  scale_color_manual(values = c("firebrick3", "azure4", "dodgerblue3", "black")) +
  theme(legend.position = c(0.8,0.8)) +
  theme(legend.title = element_blank()) +
  ggsave("moderates.png", height = 4)

