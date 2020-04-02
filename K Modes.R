##Steam Cluster Analysis 
library(cluster)
library(dplyr)
library(ggplot2)
library(readr)
library(Rtsne)
library(caret)
library(Amelia)
require(Amelia)
library(mice)
library(VIM)
library(psych)

setwd("C:/Users/rober/Dropbox/Masters Program/Data Mining")
Steam<-read.csv("Steam .csv")


##keeping variables I will use to cluster

Keeps<-c('year_bucket',
         'english','platform_1','platform_2','platform_3','required_age',
         'single_player_0','categories_1',	'categories_2','genres_1','steamspy_tags_1',
         'steamspy_tags_2',	'steamspy_tags_3','achievements','X._neg_of_total',
         'average_playtime','median_playtime','price')

Steam_Filtered<-Steam[Keeps]



Keep_Platform<-c('platform_1','platform_2','platform_3')

Steam_Platforms<-Steam_Filtered[Keep_Platform]
###dataframe cleaning for cluster analysis 
##coding platforms with ones where they have their type 
##platform 1 is windows
##platform 2 is mac
##platform 3 is linux

Steam_Platforms$platform_1.r<-recode(Steam_Platforms$platform_1, 'windows'=1)
Steam_Platforms$platform_2.r<-recode(Steam_Platforms$platform_2, 'mac'=1)
Steam_Platforms$platform_3.r<-recode(Steam_Platforms$platform_3, 'linux'=1)

Steam_Platforms$Row_ID <- seq.int(nrow(Steam_Platforms))


##one hot encoding to the following variables
##Year_Bucket
##categories_1
##categories_2
##genres_1
##steamspy_tags_1
##steamspy_tags_2
##steamspy_tags_3

Keeps_One_Hot<-c('year_bucket','categories_1',
                 'categories_2','genres_1','steamspy_tags_1',
                 'steamspy_tags_2','steamspy_tags_3')
Steam_One_Hot<-Steam[Keeps_One_Hot]

##checking variable types before onehot encoding
sapply(Steam_One_Hot,class)

##checking for any NA's
apply(Steam_One_Hot, 2, function(x) any(is.na(x)))
##one hot encoding variables

# dummify the data
dmy <- dummyVars(" ~ .", data = Steam_One_Hot)
Data_Post_onehot <- data.frame(predict(dmy, newdata = Steam_One_Hot))

##add id to join back to other dataframe
Data_Post_onehot$Row_ID <- seq.int(nrow(Data_Post_onehot))
##convert numeric values to z scores

##'achievements'
##'X._neg_of_total',
##'average_playtime'
##'median_playtime'
##''price'
##'required_age'

Keep_Numeric<-c('achievements','X._neg_of_total',
                'average_playtime','median_playtime',
                'price','required_age')

Steam_Numeric<-Steam[Keep_Numeric]

Steam_Z_Score<-scale(Steam_Numeric)

DF_Z_Scores<-as.data.frame(Steam_Z_Score)

##add id to join back to other dataframe
DF_Z_Scores$Row_ID <- seq.int(nrow(DF_Z_Scores))


Keeps_Remaining<-c('english','single_player_0')

Steam_Remaining<-Steam[Keeps_Remaining]

##add id to join back to other dataframe
Steam_Remaining$Row_ID <- seq.int(nrow(Steam_Remaining))


##joining the dataframes together in row id for cluster analysis 
Platform_onehot<- merge(Steam_Platforms,Data_Post_onehot,by="Row_ID")
Steam_Platforms
Data_Post_onehot
Platform_Onehot_Z<-merge(Platform_onehot,DF_Z_Scores, by="Row_ID")
DF_Z_Scores
Platform_onehot_z_remaining<-merge(Platform_Onehot_Z,Steam_Remaining,by="Row_ID")
Steam_Remaining

View(Platform_onehot_z_remaining)

Drops<-c("platform_1","platform_2","platform_3","Row_ID")
data<-Platform_onehot_z_remaining[ , !(names(Platform_onehot_z_remaining) %in% Drops)]

write.csv(data,"DF_ALL.csv")
####Missing data pattern exploration
str(data)
summary(data)

p <- function(x) {sum(is.na(x))/length(x)*100}
##applying the fuction to the filtered dataframe
apply(data, 2, p)
###seeing the pattern of missing data 
Pattern<-md.pattern(data)
##seeing the pair matchings of missing data
Pairs<-md.pairs(data)
###making the pattern and pair results dataframes and saving them to csvs
Pairs_DF<-as.data.frame(Pairs)
Pattern_DF<-as.data.frame(Pattern)
write.csv(Pattern_DF,"MD_Pattern.csv")
write.csv(Pairs_DF,"MD_Pairs.csv")

##removing variables that are not commonc (less than 20 people who participate in the category or genre)
Drops_Unpopular<-c(
  'categories_1.Captions.available',
  'categories_1.Co.op',
  'categories_1.Cross.Platform.Multiplayer',
  'categories_1.Full.controller.support',
  'categories_1.In.App.Purchases',
  'categories_1.Includes.level.editor',
  'categories_1.Includes.Source.SDK',
  'categories_1.Local.Co.op',
  'categories_1.Online.Co.op',
  'categories_1.Shared.Split.Screen',
  'categories_1.Steam.Cloud',
  'categories_1.Steam.Leaderboards',
  'categories_1.Steam.Trading.Cards',
  'categories_1.VR.Support',
  'categories_2.Commentary.available',
  'categories_2.Includes.Source.SDK',
  'categories_2.SteamVR.Collectibles',
  'categories_2.Valve.Anti.Cheat.enabled',
  'genres_1.Accounting',
  'genres_1.Audio.Production',
  'genres_1.Early.Access',
  'genres_1.Education',
  'genres_1.Massively.Multiplayer',
  'genres_1.Photo.Editing',
  'genres_1.Software.Training',
  'genres_1.Video.Production',
  'genres_1.Web.Publishing',
  'steamspy_tags_1.1980s',
  'steamspy_tags_1.1990.s',
  'steamspy_tags_1.2D',
  'steamspy_tags_1.2D.Fighter',
  'steamspy_tags_1.3D.Platformer',
  'steamspy_tags_1.4.Player.Local',
  'steamspy_tags_1.4X',
  'steamspy_tags_1.Action.RPG',
  'steamspy_tags_1.America',
  'steamspy_tags_1.Animation...Modeling',
  'steamspy_tags_1.Arcade',
  'steamspy_tags_1.Arena.Shooter',
  'steamspy_tags_1.Assassin',
  'steamspy_tags_1.Atmospheric',
  'steamspy_tags_1.Audio.Production',
  'steamspy_tags_1.Base.Building',
  'steamspy_tags_1.Baseball',
  'steamspy_tags_1.Basketball',
  'steamspy_tags_1.Batman',
  'steamspy_tags_1.Battle.Royale',
  'steamspy_tags_1.Beat..em.up',
  'steamspy_tags_1.Benchmark',
  'steamspy_tags_1.Bikes',
  'steamspy_tags_1.Board.Game',
  'steamspy_tags_1.Bowling',
  'steamspy_tags_1.Building',
  'steamspy_tags_1.Capitalism',
  'steamspy_tags_1.Cats',
  'steamspy_tags_1.Character.Customization',
  'steamspy_tags_1.Chess',
  'steamspy_tags_1.Choices.Matter',
  'steamspy_tags_1.Choose.Your.Own.Adventure',
  'steamspy_tags_1.City.Builder',
  'steamspy_tags_1.Classic',
  'steamspy_tags_1.Clicker',
  'steamspy_tags_1.Co.op',
  'steamspy_tags_1.Cold.War',
  'steamspy_tags_1.Comedy',
  'steamspy_tags_1.Crafting',
  'steamspy_tags_1.Crime',
  'steamspy_tags_1.Cult.Classic',
  'steamspy_tags_1.Cute',
  'steamspy_tags_1.Cyberpunk',
  'steamspy_tags_1.Dark.Fantasy',
  'steamspy_tags_1.Dark.Humor',
  'steamspy_tags_1.Dating.Sim',
  'steamspy_tags_1.Destruction',
  'steamspy_tags_1.Detective',
  'steamspy_tags_1.Difficult',
  'steamspy_tags_1.Dinosaurs',
  'steamspy_tags_1.Documentary',
  'steamspy_tags_1.Dog',
  'steamspy_tags_1.Dragons',
  'steamspy_tags_1.Driving',
  'steamspy_tags_1.Dungeon.Crawler',
  'steamspy_tags_1.Education',
  'steamspy_tags_1.Exploration',
  'steamspy_tags_1.Family.Friendly',
  'steamspy_tags_1.Female.Protagonist',
  'steamspy_tags_1.First.Person',
  'steamspy_tags_1.Fishing',
  'steamspy_tags_1.Flight',
  'steamspy_tags_1.FMV',
  'steamspy_tags_1.Football',
  'steamspy_tags_1.Funny',
  'steamspy_tags_1.Game.Development',
  'steamspy_tags_1.God.Game',
  'steamspy_tags_1.Golf',
  'steamspy_tags_1.Grand.Strategy',
  'steamspy_tags_1.Great.Soundtrack',
  'steamspy_tags_1.Hack.and.Slash',
  'steamspy_tags_1.Hacking',
  'steamspy_tags_1.Hand.drawn',
  'steamspy_tags_1.Historical',
  'steamspy_tags_1.Hockey',
  'steamspy_tags_1.Horses',
  'steamspy_tags_1.Hunting',
  'steamspy_tags_1.Illuminati',
  'steamspy_tags_1.Intentionally.Awkward.Controls',
  'steamspy_tags_1.Interactive.Fiction',
  'steamspy_tags_1.Inventory.Management',
  'steamspy_tags_1.Lemmings',
  'steamspy_tags_1.Local.Co.Op',
  'steamspy_tags_1.Local.Multiplayer',
  'steamspy_tags_1.Lovecraftian',
  'steamspy_tags_1.Magic',
  'steamspy_tags_1.Management',
  'steamspy_tags_1.Match.3',
  'steamspy_tags_1.Mature',
  'steamspy_tags_1.Mechs',
  'steamspy_tags_1.Medieval',
  'steamspy_tags_1.Mini.Golf',
  'steamspy_tags_1.MMORPG',
  'steamspy_tags_1.MOBA',
  'steamspy_tags_1.Motocross',
  'steamspy_tags_1.Motorbike',
  'steamspy_tags_1.Movie',
  'steamspy_tags_1.Multiplayer',
  'steamspy_tags_1.Music',
  'steamspy_tags_1.Mystery',
  'steamspy_tags_1.Noir',
  'steamspy_tags_1.Offroad',
  'steamspy_tags_1.On.Rails.Shooter',
  'steamspy_tags_1.Otome',
  'steamspy_tags_1.Parkour',
  'steamspy_tags_1.Pinball',
  'steamspy_tags_1.Pirates',
  'steamspy_tags_1.Post.apocalyptic',
  'steamspy_tags_1.Programming',
  'steamspy_tags_1.Psychological.Horror',
  'steamspy_tags_1.Puzzle.Platformer',
  'steamspy_tags_1.PvP',
  'steamspy_tags_1.Quick.Time.Events',
  'steamspy_tags_1.Realistic',
  'steamspy_tags_1.Relaxing',
  'steamspy_tags_1.Retro',
  'steamspy_tags_1.Rhythm',
  'steamspy_tags_1.Rogue.lite',
  'steamspy_tags_1.Romance',
  'steamspy_tags_1.Sci.fi',
  'steamspy_tags_1.Shooter',
  'steamspy_tags_1.Short',
  'steamspy_tags_1.Singleplayer',
  'steamspy_tags_1.Skateboarding',
  'steamspy_tags_1.Sniper',
  'steamspy_tags_1.Snow',
  'steamspy_tags_1.Software',
  'steamspy_tags_1.Software.Training',
  'steamspy_tags_1.Sokoban',
  'steamspy_tags_1.Souls.like',
  'steamspy_tags_1.Space.Sim',
  'steamspy_tags_1.Spelling',
  'steamspy_tags_1.Star.Wars',
  'steamspy_tags_1.Steampunk',
  'steamspy_tags_1.Story.Rich',
  'steamspy_tags_1.Strategy.RPG',
  'steamspy_tags_1.Surreal',
  'steamspy_tags_1.Survival.Horror',
  'steamspy_tags_1.Tactical',
  'steamspy_tags_1.Tanks',
  'steamspy_tags_1.Tennis',
  'steamspy_tags_1.Time.Management',
  'steamspy_tags_1.Top.Down.Shooter',
  'steamspy_tags_1.Trading.Card.Game',
  'steamspy_tags_1.Turn.Based',
  'steamspy_tags_1.Turn.Based.Combat',
  'steamspy_tags_1.Turn.Based.Strategy',
  'steamspy_tags_1.Turn.Based.Tactics',
  'steamspy_tags_1.Twin.Stick.Shooter',
  'steamspy_tags_1.Typing',
  'steamspy_tags_1.Vampire',
  'steamspy_tags_1.Video.Production',
  'steamspy_tags_1.Walking.Simulator',
  'steamspy_tags_1.Wargame',
  'steamspy_tags_1.Warhammer.40K',
  'steamspy_tags_1.Web.Publishing',
  'steamspy_tags_1.Western',
  'steamspy_tags_1.Word.Game',
  'steamspy_tags_1.World.War.I',
  'steamspy_tags_1.World.War.II',
  'steamspy_tags_1.Wrestling',
  'steamspy_tags_2.1980s',
  'steamspy_tags_2.1990.s',
  'steamspy_tags_2.2D.Fighter',
  'steamspy_tags_2.3D.Platformer',
  'steamspy_tags_2.4.Player.Local',
  'steamspy_tags_2.4X',
  'steamspy_tags_2.6DOF',
  'steamspy_tags_2.Abstract',
  'steamspy_tags_2.Action.Adventure',
  'steamspy_tags_2.Action.RPG',
  'steamspy_tags_2.Agriculture',
  'steamspy_tags_2.Arena.Shooter',
  'steamspy_tags_2.Assassin',
  'steamspy_tags_2.Audio.Production',
  'steamspy_tags_2.Base.Building',
  'steamspy_tags_2.Baseball',
  'steamspy_tags_2.Basketball',
  'steamspy_tags_2.Batman',
  'steamspy_tags_2.Battle.Royale',
  'steamspy_tags_2.Beat..em.up',
  'steamspy_tags_2.Benchmark',
  'steamspy_tags_2.Bikes',
  'steamspy_tags_2.BMX',
  'steamspy_tags_2.Cartoon',
  'steamspy_tags_2.Cats',
  'steamspy_tags_2.Character.Customization',
  'steamspy_tags_2.Chess',
  'steamspy_tags_2.Choices.Matter',
  'steamspy_tags_2.Choose.Your.Own.Adventure',
  'steamspy_tags_2.Cold.War',
  'steamspy_tags_2.Colorful',
  'steamspy_tags_2.Comedy',
  'steamspy_tags_2.Comic.Book',
  'steamspy_tags_2.Competitive',
  'steamspy_tags_2.Controller',
  'steamspy_tags_2.Crafting',
  'steamspy_tags_2.Crime',
  'steamspy_tags_2.Cute',
  'steamspy_tags_2.Cyberpunk',
  'steamspy_tags_2.Dark',
  'steamspy_tags_2.Dark.Fantasy',
  'steamspy_tags_2.Dark.Humor',
  'steamspy_tags_2.Dating.Sim',
  'steamspy_tags_2.Demons',
  'steamspy_tags_2.Destruction',
  'steamspy_tags_2.Detective',
  'steamspy_tags_2.Difficult',
  'steamspy_tags_2.Dinosaurs',
  'steamspy_tags_2.Diplomacy',
  'steamspy_tags_2.Documentary',
  'steamspy_tags_2.Dragons',
  'steamspy_tags_2.Drama',
  'steamspy_tags_2.Dystopian.',
  'steamspy_tags_2.Early.Access',
  'steamspy_tags_2.Economy',
  'steamspy_tags_2.Experimental',
  'steamspy_tags_2.Exploration',
  'steamspy_tags_2.Family.Friendly',
  'steamspy_tags_2.Fast.Paced',
  'steamspy_tags_2.First.Person',
  'steamspy_tags_2.Fishing',
  'steamspy_tags_2.Football',
  'steamspy_tags_2.Funny',
  'steamspy_tags_2.Futuristic',
  'steamspy_tags_2.Gambling',
  'steamspy_tags_2.Game.Development',
  'steamspy_tags_2.GameMaker',
  'steamspy_tags_2.Games.Workshop',
  'steamspy_tags_2.God.Game',
  'steamspy_tags_2.Golf',
  'steamspy_tags_2.Grand.Strategy',
  'steamspy_tags_2.Gun.Customization',
  'steamspy_tags_2.Hacking',
  'steamspy_tags_2.Hand.drawn',
  'steamspy_tags_2.Heist',
  'steamspy_tags_2.Hex.Grid',
  'steamspy_tags_2.Hockey',
  'steamspy_tags_2.Horses',
  'steamspy_tags_2.Hunting',
  'steamspy_tags_2.Illuminati',
  'steamspy_tags_2.Interactive.Fiction',
  'steamspy_tags_2.Isometric',
  'steamspy_tags_2.LEGO',
  'steamspy_tags_2.Lemmings',
  'steamspy_tags_2.Level.Editor',
  'steamspy_tags_2.Linear',
  'steamspy_tags_2.Local.Co.Op',
  'steamspy_tags_2.Local.Multiplayer',
  'steamspy_tags_2.Lovecraftian',
  'steamspy_tags_2.Magic',
  'steamspy_tags_2.Mars',
  'steamspy_tags_2.Martial.Arts',
  'steamspy_tags_2.Masterpiece',
  'steamspy_tags_2.Mature',
  'steamspy_tags_2.Mechs',
  'steamspy_tags_2.Medieval',
  'steamspy_tags_2.Memes',
  'steamspy_tags_2.Metroidvania',
  'steamspy_tags_2.Military',
  'steamspy_tags_2.Mini.Golf',
  'steamspy_tags_2.Minimalist',
  'steamspy_tags_2.Mining',
  'steamspy_tags_2.MMORPG',
  'steamspy_tags_2.MOBA',
  'steamspy_tags_2.Moddable',
  'steamspy_tags_2.Motocross',
  'steamspy_tags_2.Motorbike',
  'steamspy_tags_2.Mouse.only',
  'steamspy_tags_2.Multiple.Endings',
  'steamspy_tags_2.Mystery',
  'steamspy_tags_2.Mythology',
  'steamspy_tags_2.Narration',
  'steamspy_tags_2.Ninja',
  'steamspy_tags_2.Noir',
  'steamspy_tags_2.NSFW',
  'steamspy_tags_2.Offroad',
  'steamspy_tags_2.Old.School',
  'steamspy_tags_2.On.Rails.Shooter',
  'steamspy_tags_2.Online.Co.Op',
  'steamspy_tags_2.Otome',
  'steamspy_tags_2.Parkour',
  'steamspy_tags_2.Parody.',
  'steamspy_tags_2.Party.Based.RPG',
  'steamspy_tags_2.Philisophical',
  'steamspy_tags_2.Photo.Editing',
  'steamspy_tags_2.Physics',
  'steamspy_tags_2.Pinball',
  'steamspy_tags_2.Pirates',
  'steamspy_tags_2.Political',
  'steamspy_tags_2.Politics',
  'steamspy_tags_2.Post.apocalyptic',
  'steamspy_tags_2.Procedural.Generation',
  'steamspy_tags_2.Programming',
  'steamspy_tags_2.Psychedelic',
  'steamspy_tags_2.Psychological',
  'steamspy_tags_2.Puzzle.Platformer',
  'steamspy_tags_2.PvP',
  'steamspy_tags_2.Real.Time',
  'steamspy_tags_2.Realistic',
  'steamspy_tags_2.Relaxing',
  'steamspy_tags_2.Remake',
  'steamspy_tags_2.Replay.Value',
  'steamspy_tags_2.Resource.Management',
  'steamspy_tags_2.Rhythm',
  'steamspy_tags_2.Robots',
  'steamspy_tags_2.Rogue.like',
  'steamspy_tags_2.Rogue.lite',
  'steamspy_tags_2.Romance',
  'steamspy_tags_2.Rome',
  'steamspy_tags_2.Runner',
  'steamspy_tags_2.Science',
  'steamspy_tags_2.Short',
  'steamspy_tags_2.Side.Scroller',
  'steamspy_tags_2.Singleplayer',
  'steamspy_tags_2.Skateboarding',
  'steamspy_tags_2.Sniper',
  'steamspy_tags_2.Snow',
  'steamspy_tags_2.Snowboarding',
  'steamspy_tags_2.Soccer',
  'steamspy_tags_2.Software',
  'steamspy_tags_2.Software.Training',
  'steamspy_tags_2.Sokoban',
  'steamspy_tags_2.Souls.like',
  'steamspy_tags_2.Space.Sim',
  'steamspy_tags_2.Spectacle.fighter',
  'steamspy_tags_2.Split.Screen',
  'steamspy_tags_2.Steampunk',
  'steamspy_tags_2.Submarine',
  'steamspy_tags_2.Superhero',
  'steamspy_tags_2.Supernatural',
  'steamspy_tags_2.Surreal',
  'steamspy_tags_2.Survival.Horror',
  'steamspy_tags_2.Swordplay',
  'steamspy_tags_2.Tactical',
  'steamspy_tags_2.Tanks',
  'steamspy_tags_2.Team.Based',
  'steamspy_tags_2.Tennis',
  'steamspy_tags_2.Text.Based',
  'steamspy_tags_2.Third.Person.Shooter',
  'steamspy_tags_2.Third.Person',
  'steamspy_tags_2.Time.Attack',
  'steamspy_tags_2.Time.Management',
  'steamspy_tags_2.Time.Manipulation',
  'steamspy_tags_2.Top.Down',
  'steamspy_tags_2.Top.Down.Shooter',
  'steamspy_tags_2.Trading',
  'steamspy_tags_2.Trading.Card.Game',
  'steamspy_tags_2.Turn.Based.Combat',
  'steamspy_tags_2.Turn.Based.Tactics',
  'steamspy_tags_2.Twin.Stick.Shooter',
  'steamspy_tags_2.Typing',
  'steamspy_tags_2.Underwater',
  'steamspy_tags_2.Vampire',
  'steamspy_tags_2.Villain.Protagonist',
  'steamspy_tags_2.Voice.Control',
  'steamspy_tags_2.VR.Only',
  'steamspy_tags_2.Walking.Simulator',
  'steamspy_tags_2.War',
  'steamspy_tags_2.Wargame',
  'steamspy_tags_2.Web.Publishing',
  'steamspy_tags_2.Werewolves',
  'steamspy_tags_2.Western',
  'steamspy_tags_2.Word.Game',
  'steamspy_tags_2.World.War.I',
  'steamspy_tags_2.Wrestling',
  'steamspy_tags_3.1980s',
  'steamspy_tags_3.1990.s',
  'steamspy_tags_3.2.5D',
  'steamspy_tags_3.360.Video',
  'steamspy_tags_3.3D',
  'steamspy_tags_3.3D.Vision',
  'steamspy_tags_3.4X',
  'steamspy_tags_3.6DOF',
  'steamspy_tags_3.Abstract',
  'steamspy_tags_3.Action.Adventure',
  'steamspy_tags_3.Action.RPG',
  'steamspy_tags_3.Agriculture',
  'steamspy_tags_3.Aliens',
  'steamspy_tags_3.Alternate.History',
  'steamspy_tags_3.America',
  'steamspy_tags_3.Animation...Modeling',
  'steamspy_tags_3.Arena.Shooter',
  'steamspy_tags_3.Assassin',
  'steamspy_tags_3.Audio.Production',
  'steamspy_tags_3.Base.Building',
  'steamspy_tags_3.Baseball',
  'steamspy_tags_3.Basketball',
  'steamspy_tags_3.Batman',
  'steamspy_tags_3.Battle.Royale',
  'steamspy_tags_3.Beautiful',
  'steamspy_tags_3.Bikes',
  'steamspy_tags_3.Blood',
  'steamspy_tags_3.BMX',
  'steamspy_tags_3.Bowling',
  'steamspy_tags_3.Bullet.Time',
  'steamspy_tags_3.Capitalism',
  'steamspy_tags_3.Cartoon',
  'steamspy_tags_3.Cartoony',
  'steamspy_tags_3.Cats',
  'steamspy_tags_3.Character.Action.Game',
  'steamspy_tags_3.Character.Customization',
  'steamspy_tags_3.Chess',
  'steamspy_tags_3.Choices.Matter',
  'steamspy_tags_3.Choose.Your.Own.Adventure',
  'steamspy_tags_3.Cinematic',
  'steamspy_tags_3.Class.Based',
  'steamspy_tags_3.Cold.War',
  'steamspy_tags_3.Colorful',
  'steamspy_tags_3.Comic.Book',
  'steamspy_tags_3.Competitive',
  'steamspy_tags_3.Controller',
  'steamspy_tags_3.Conversation',
  'steamspy_tags_3.Crafting',
  'steamspy_tags_3.Crime',
  'steamspy_tags_3.CRPG',
  'steamspy_tags_3.Cult.Classic',
  'steamspy_tags_3.Cycling',
  'steamspy_tags_3.Dark',
  'steamspy_tags_3.Dark.Fantasy',
  'steamspy_tags_3.Dark.Humor',
  'steamspy_tags_3.Demons',
  'steamspy_tags_3.Design...Illustration',
  'steamspy_tags_3.Destruction',
  'steamspy_tags_3.Dinosaurs',
  'steamspy_tags_3.Dog',
  'steamspy_tags_3.Dragons',
  'steamspy_tags_3.Drama',
  'steamspy_tags_3.Dungeons...Dragons',
  'steamspy_tags_3.Dystopian.',
  'steamspy_tags_3.e.sports',
  'steamspy_tags_3.Early.Access',
  'steamspy_tags_3.Economy',
  'steamspy_tags_3.Epic',
  'steamspy_tags_3.Experimental',
  'steamspy_tags_3.Faith',
  'steamspy_tags_3.Fast.Paced',
  'steamspy_tags_3.Fishing',
  'steamspy_tags_3.FMV',
  'steamspy_tags_3.Football',
  'steamspy_tags_3.Futuristic',
  'steamspy_tags_3.Gambling',
  'steamspy_tags_3.Game.Development',
  'steamspy_tags_3.Games.Workshop',
  'steamspy_tags_3.God.Game',
  'steamspy_tags_3.Golf',
  'steamspy_tags_3.Gothic',
  'steamspy_tags_3.Grand.Strategy',
  'steamspy_tags_3.Grid.Based.Movement',
  'steamspy_tags_3.Hacking',
  'steamspy_tags_3.Hand.drawn',
  'steamspy_tags_3.Heist',
  'steamspy_tags_3.Hex.Grid',
  'steamspy_tags_3.Hockey',
  'steamspy_tags_3.Horses',
  'steamspy_tags_3.Hunting',
  'steamspy_tags_3.Illuminati',
  'steamspy_tags_3.Interactive.Fiction',
  'steamspy_tags_3.Investigation',
  'steamspy_tags_3.Isometric',
  'steamspy_tags_3.Kickstarter',
  'steamspy_tags_3.Lara.Croft',
  'steamspy_tags_3.LEGO',
  'steamspy_tags_3.Level.Editor',
  'steamspy_tags_3.Logic',
  'steamspy_tags_3.Loot',
  'steamspy_tags_3.Lovecraftian',
  'steamspy_tags_3.Magic',
  'steamspy_tags_3.Mars',
  'steamspy_tags_3.Martial.Arts',
  'steamspy_tags_3.Masterpiece',
  'steamspy_tags_3.Mechs',
  'steamspy_tags_3.Military',
  'steamspy_tags_3.Mini.Golf',
  'steamspy_tags_3.Mining',
  'steamspy_tags_3.MMORPG',
  'steamspy_tags_3.MOBA',
  'steamspy_tags_3.Mod',
  'steamspy_tags_3.Moddable',
  'steamspy_tags_3.Motocross',
  'steamspy_tags_3.Motorbike',
  'steamspy_tags_3.Mouse.only',
  'steamspy_tags_3.Multiple.Endings',
  'steamspy_tags_3.Music.Based.Procedural.Generation',
  'steamspy_tags_3.Mystery.Dungeon',
  'steamspy_tags_3.Mythology',
  'steamspy_tags_3.Narration',
  'steamspy_tags_3.Naval',
  'steamspy_tags_3.Ninja',
  'steamspy_tags_3.Noir',
  'steamspy_tags_3.Nonlinear',
  'steamspy_tags_3.Offroad',
  'steamspy_tags_3.Old.School',
  'steamspy_tags_3.On.Rails.Shooter',
  'steamspy_tags_3.Online.Co.Op',
  'steamspy_tags_3.Otome',
  'steamspy_tags_3.Parkour',
  'steamspy_tags_3.Party.Based.RPG',
  'steamspy_tags_3.Perma.Death',
  'steamspy_tags_3.Philisophical',
  'steamspy_tags_3.Photo.Editing',
  'steamspy_tags_3.Pinball',
  'steamspy_tags_3.Pirates',
  'steamspy_tags_3.Political',
  'steamspy_tags_3.Politics',
  'steamspy_tags_3.Pool',
  'steamspy_tags_3.Post.apocalyptic',
  'steamspy_tags_3.Procedural.Generation',
  'steamspy_tags_3.Programming',
  'steamspy_tags_3.Psychedelic',
  'steamspy_tags_3.PvP',
  'steamspy_tags_3.Quick.Time.Events',
  'steamspy_tags_3.Real.Time',
  'steamspy_tags_3.Real.Time.with.Pause',
  'steamspy_tags_3.Real.Time.Tactics',
  'steamspy_tags_3.Realistic',
  'steamspy_tags_3.Remake',
  'steamspy_tags_3.Replay.Value',
  'steamspy_tags_3.Resource.Management',
  'steamspy_tags_3.Robots',
  'steamspy_tags_3.Rogue.lite',
  'steamspy_tags_3.Romance',
  'steamspy_tags_3.Rome',
  'steamspy_tags_3.Runner',
  'steamspy_tags_3.Sailing',
  'steamspy_tags_3.Satire',
  'steamspy_tags_3.Science',
  'steamspy_tags_3.Score.Attack',
  'steamspy_tags_3.Short',
  'steamspy_tags_3.Side.Scroller',
  'steamspy_tags_3.Skating',
  'steamspy_tags_3.Sniper',
  'steamspy_tags_3.Snow',
  'steamspy_tags_3.Soccer',
  'steamspy_tags_3.Software',
  'steamspy_tags_3.Software.Training',
  'steamspy_tags_3.Sokoban',
  'steamspy_tags_3.Souls.like',
  'steamspy_tags_3.Space.Sim',
  'steamspy_tags_3.Spelling',
  'steamspy_tags_3.Split.Screen',
  'steamspy_tags_3.Star.Wars',
  'steamspy_tags_3.Steampunk',
  'steamspy_tags_3.Stylized',
  'steamspy_tags_3.Submarine',
  'steamspy_tags_3.Superhero',
  'steamspy_tags_3.Supernatural',
  'steamspy_tags_3.Surreal',
  'steamspy_tags_3.Swordplay',
  'steamspy_tags_3.Tactical.RPG',
  'steamspy_tags_3.Tanks',
  'steamspy_tags_3.Team.Based',
  'steamspy_tags_3.Tennis',
  'steamspy_tags_3.Text.Based',
  'steamspy_tags_3.Thriller',
  'steamspy_tags_3.Time.Attack',
  'steamspy_tags_3.Time.Management',
  'steamspy_tags_3.Time.Manipulation',
  'steamspy_tags_3.Time.Travel',
  'steamspy_tags_3.Top.Down',
  'steamspy_tags_3.Touch.Friendly',
  'steamspy_tags_3.Trading',
  'steamspy_tags_3.Trading.Card.Game',
  'steamspy_tags_3.Trains',
  'steamspy_tags_3.Turn.Based.Combat',
  'steamspy_tags_3.Turn.Based.Tactics',
  'steamspy_tags_3.Typing',
  'steamspy_tags_3.Underwater',
  'steamspy_tags_3.Vampire',
  'steamspy_tags_3.Video.Production',
  'steamspy_tags_3.Villain.Protagonist',
  'steamspy_tags_3.Voice.Control',
  'steamspy_tags_3.Voxel',
  'steamspy_tags_3.War',
  'steamspy_tags_3.Wargame',
  'steamspy_tags_3.Warhammer.40K',
  'steamspy_tags_3.Web.Publishing',
  'steamspy_tags_3.Werewolves',
  'steamspy_tags_3.Western',
  'steamspy_tags_3.Word.Game',
  'steamspy_tags_3.World.War.I',
  'steamspy_tags_3.Wrestling'
)

df<-data[ , !(names(data) %in% Drops_Unpopular)]

##checking for any NA's
apply(df, 2, function(x) any(is.na(x)))

##making na's 0

df[is.na(df)] <- 0

View(df)

df$column[df$column < 0] <- 0

##convert z scores to 1's and 0's for K-Modes clustering
##if positive z score it gets a 1
##0 and negative z scores get 0

df$achievements[df$achievements<=0]<-0
df$achievements[df$achievements>0]<-1

df$X._neg_of_total[df$X._neg_of_total<=0]<-0
df$X._neg_of_total[df$X._neg_of_total>0]<-1

df$average_playtime[df$average_playtime<=0]<-0
df$average_playtime[df$average_playtime>0]<-1

df$median_playtime[df$median_playtime<=0]<-0
df$median_playtime[df$median_playtime>0]<-1

df$price[df$price<=0]<-0
df$price[df$price>0]<-1

df$required_age[df$required_age<=0]<-0
df$required_age[df$required_age>0]<-1

###K-Modes clustering on the new dataframe

library(klaR)

cl<-klaR::kmodes(df,6)

Df_Cluster<-cbind(df,cl$cluster)

##exporting the dataframe that includes the cluster number

write.csv(Df_Cluster,"kmodes_cluster.csv")




