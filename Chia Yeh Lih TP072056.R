# Import dplyr
install.packages("dplyr")
library(dplyr)

# Import ggplot2
install.packages(("ggplot2"))
library(ggplot2)

# Install plotrix
install.packages("plotrix")
library(plotrix)

#Install treemap
install.packages("treemap")
library(treemap)

# Change maximum printable lines
options(max.print= 99999)

# Import dataset
student_data = read.csv("C:/Users/yehli/Downloads/student_prediction.csv", header = TRUE)
summary(student_data)
View(student_data)

### Data Cleaning ###
# Exclude duplicate
student_data <- unique(student_data)

# List out column names
colnames(student_data)

# Remove commas from numeric values
student_data[, colnames(student_data)] <- lapply(student_data[, colnames(student_data)], function(x) gsub(",", "", x))
student_data

# Remove White Space from Column Names
colnames(student_data) <- gsub("\\s", "_", colnames(student_data))
student_data

# Remove null data
student_data <- na.omit(student_data)
student_data

# Hypothesis:
# Students who graduated from STATE high-school, who did not involve in additional work, 
# who always attend classes, and sometimes listens in class 
# will have a higher output grade (graded >5)

# View selected student data
View(student_data[c("STUDENTID", "AGE", "GENDER", "HS_TYPE", "WORK", "ATTEND", "LISTENS", "GRADE")])

# Selected data input into a data frame and
# Select the specific columns you want to keep
selected_data <- student_data %>%
  select(STUDENTID, AGE, GENDER, HS_TYPE, WORK, ATTEND, LISTENS, GRADE)
selected_data

# Rename header
renamed_data <- selected_data %>%
  rename(
    GRADUATED_HS = HS_TYPE,
    ADD_WORK = WORK,
    ATTENDANCE = ATTEND,
    LISTENS_IN_CLASS = LISTENS
  )
renamed_data
View(renamed_data)

# Change output from 1,2,3 to description
renamed_data$AGE <- factor(renamed_data$AGE, levels= c(1,2,3), labels= c("18 to 21","22 to 25","26 and above"))
renamed_data$GENDER <- factor(renamed_data$GENDER, levels= c(1,2), labels= c("Female","Male"))
renamed_data$GRADUATED_HS <- factor(renamed_data$GRADUATED_HS, levels= c(1,2,3), labels= c("Private","State","Other"))
renamed_data$ADD_WORK <- factor(renamed_data$ADD_WORK, levels= c(1,2), labels= c("Yes", "No"))
renamed_data$ATTENDANCE <- factor(renamed_data$ATTENDANCE, levels= c(1,2,3), labels= c("Always","Sometimes","Never"))
renamed_data$LISTENS_IN_CLASS <- factor(renamed_data$LISTENS_IN_CLASS, levels= c(1,2,3), labels= c("Never","Sometimes","Always"))
renamed_data$GRADE <- factor(renamed_data$GRADE, levels=c(0,1,2,3,4,5,6,7), labels=c("Fail","DD","DC","CC","CB","BB","BA","AA"))

View(renamed_data)

### Chia Yeh Lih ###
## Objective 2: ##
# The impact of students' study productivity with additional work that will affect students output grade.



# 1.Additional work
addWork <- renamed_data%>% group_by(ADD_WORK)
addWork

SaddWork<- addWork%>% summarize(SumOfHighGrade= sum(GRADE %in% c("BB","BA","AA")))
SaddWork # 105 (No work); 44 (Got work) 


## 3D Pie chart for add work
# Insert the summarized data into a dataframe 
SaddWork <- data.frame(
  ADD_WORK = c("No work", "Got work"),
  SumOfHighGrade = c(105, 44)
)

# Define colors for the slices
colors <- c("purple", "yellow")  

# Generate 3D pie chart
pie3D(SaddWork$SumOfHighGrade, labels = paste(SaddWork$ADD_WORK, "\n", SaddWork$SumOfHighGrade),
      explode = 0.5, main = "Sum of High Grades by Work Status", col = colors, radius = 2)



# 2.State high school
Shigh <- renamed_data %>% group_by(GRADUATED_HS)
Shigh

SShigh <- Shigh %>% summarize(SumOfHighGrade= sum(GRADE %in% c("BB","BA","AA")))
SShigh # 70 (State); 40 (Private); 39(Other)


## Stacked bar chart for high school
ggplot(SShigh, aes(x = "", y = SumOfHighGrade, fill = GRADUATED_HS)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Sum of High Grades by High School Type",
    x = NULL,
    y = "Sum of High Grades",
    fill= "High School Type"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +  # Adjust legend position if necessary
  geom_text(aes(label=SumOfHighGrade), position = position_stack(vjust = 0.5))



# 3.Always attend classes
Aattend <-  renamed_data%>% group_by(ATTENDANCE)
Aattend

SAattend <- Aattend %>% summarize(SumOfHighGrade= sum(GRADE %in% c("BB","BA","AA")))
SAattend # 116 (Always) ; 33 (Sometimes); 0(never)


## Segmented bar chart for attendance
ggplot(SAattend, aes(x = "", y = SumOfHighGrade, fill = ATTENDANCE)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Sum of High Grades by Attendance Status",
    x = NULL,
    y = "Sum of High Grades"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +  # Adjust legend position if necessary
  geom_text(aes(label= SumOfHighGrade), position = position_stack(vjust=0.5))



# 4.Sometimes listen in class
Slisten <- renamed_data %>% group_by(LISTENS_IN_CLASS)
Slisten

SSlisten <- Slisten %>% summarize(SumOfHighGrade= sum(GRADE %in% c("BB","BA","AA")))
SSlisten # 94 (Sometimes); 32 (Always); 23 (Never)


## Create a 3D pie chart
colors <- c("purple", "coral", "lightgreen")  # Define colors for the slices
pie3D(SSlisten$SumOfHighGrade, 
      labels =paste( SSlisten$LISTENS_IN_CLASS, "\n",SSlisten$SumOfHighGrade) 
      , explode = 0.1, main = "Sum of High Grades by Listening in Class Frequency", col = colors)



# Work + High school 
# Using not working data comparing high schools
noWorkdata <- renamed_data %>% filter(ADD_WORK == "No", GRADE %in% c("BB","BA","AA"))
nrow(noWorkdata)

# Summarize it
NoWork <- noWorkdata %>% group_by (GRADUATED_HS) %>% summarize (Count = n())
NoWork # 55 (State), 27 (Private), 23 (Other)

# Subset the data to display students who not working only
noWorkSummary <- SaddWork[SaddWork$ADD_WORK == "No", ] 

# Display total students 
print(noWorkSummary) # 105 students


## Create a donut chart
# Insert data into a dataframe
NoWork <- data.frame(
  GRADUATED_HS = c("State", "Private", "Other"),
  Count = c(55, 27, 23)
)

# Compute percentages
NoWork$fraction <- NoWork$Count / sum(NoWork$Count)

# Compute the cumulative percentages (top of each rectangle)
NoWork$ymax <- cumsum(NoWork$fraction)

# Compute the bottom of each rectangle
NoWork$ymin <- c(0, head(NoWork$ymax, n=-1))

# Compute label position
NoWork$labelPosition <- (NoWork$ymax + NoWork$ymin) / 2

# Compute a label
NoWork$label <- paste0(NoWork$GRADUATED_HS, "\n Students: ", NoWork$Count)

# Make the plot
ggplot(NoWork, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = GRADUATED_HS)) +
  geom_rect() +
  geom_text(aes(y = labelPosition, label = label), size = 6, x = 2, color = "black") +
  scale_fill_brewer(palette = 3) +
  scale_color_brewer(palette = 3) +
  coord_polar(theta = "y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none") +
  labs(title = "Distribution of Students with No Work and High Grades by High School Type")


  
# Work + High school + Attendance 
# Using not working, state high school, comparing attendance
stateNoWorkData <- renamed_data %>% 
  filter(ADD_WORK == "No", GRADUATED_HS == "State", GRADE %in% c("BB", "BA", "AA"))

# Summarize attendance for the filtered data
stateNoWorkAttendance <- stateNoWorkData %>% 
  group_by(ATTENDANCE) %>% 
  summarize(SumOfHighGrade = sum(GRADE %in% c("BB", "BA", "AA")))

# Display the summary
print(stateNoWorkAttendance) # Output is 45 (always attend),  10 (sometimes attend), 0 (never attend)

# Subset the data to display students always attend 
noAttendSummary <- stateNoWorkAttendance[stateNoWorkAttendance$ATTENDANCE == "Always", ] 
print(noAttendSummary) # 45 students


# Stacked bar chart for attendance distribution; 45:10(55)
ggplot(stateNoWorkData, aes(x = GRADUATED_HS, fill = ATTENDANCE)) +
  geom_bar() +
  theme_minimal() +
  labs(title = "Attendance Distribution for State High School Graduates Not Working") +
  scale_fill_manual(values = c("Always" = "yellow", "Sometimes" = "purple", "Never" = "green"))+
  geom_text(aes(label = stat(count)),
                stat = "count",, position = position_stack(vjust=0.5))



# Hypothesis (No work + State + Always attend + Sometimes listen)
# Using not working, state high school, always attend, comparing listening in class
nwStateAaSListen <- renamed_data %>% 
  filter(ADD_WORK == "No", GRADUATED_HS == "State",
         ATTENDANCE == "Always", GRADE %in% c("BB", "BA", "AA"))
nrow(nwStateAaSListen) # Double check output is 45

# Summarize listens for the filtered data
sumNSAS <- nwStateAaSListen %>% 
  group_by(LISTENS_IN_CLASS) %>% 
  summarize(SumOfHighGrade = sum(GRADE %in% c("BB", "BA", "AA")))

# Display the output
sumNSAS # 35 (sometimes listen), 6 (always listen), 4 (never listen)

# Subset the data to display students sometimes listen and 
sometimesListenSummary <- sumNSAS[sumNSAS$LISTENS_IN_CLASS == "Sometimes",]
print(sometimesListenSummary)

## Total of 35 students not working, from state high school, always attend class,
## and sometimes listening who have good grades.

# Students who graduated from STATE high-school, 
# who did not involve in additional work,
# who always attend classes and 
# sometimes listens in class will have a higher output grade (graded >5). (HYPOTHESIS PROVEN!)


## Create a donut graph with the data
listening_data <- data.frame(
  LISTENS_IN_CLASS = c("Sometimes", "Always", "Never"),
  SumOfHighGrade = c(35, 6, 4)
)

# Compute percentages
listening_data$fraction <- listening_data$SumOfHighGrade / sum(listening_data$SumOfHighGrade)

# Compute the cumulative percentages (top of each rectangle)
listening_data$ymax <- cumsum(listening_data$fraction)
listening_data$ymin <- c(0, head(listening_data$ymax, n = -1))

# Compute label position
listening_data$labelPosition <- (listening_data$ymax + listening_data$ymin) / 2

# Compute a good label
listening_data$label <- paste0(listening_data$LISTENS_IN_CLASS, "\n Students: ", listening_data$SumOfHighGrade)

# Make the plot
ggplot(listening_data, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = LISTENS_IN_CLASS)) +
  geom_rect() +
  geom_text(aes(y = labelPosition, label = label), size = 6, x = 2, color = "black") +
  scale_fill_brewer(palette = 2) +
  scale_color_brewer(palette = 2) +
  coord_polar(theta = "y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none") +
  labs(title = "       Listening in Class Distribution for Not Working
       State High School Graduates who Always Attend Classes ")


# Creating a Treemap using the data 
data <-
  data.frame(
  LISTENS_IN_CLASS = c("Sometimes", "Always", "Never"),
  SumOfHighGrade = c(35, 6, 4)
)

treemap(
  data,
  index = "LISTENS_IN_CLASS",
  vSize = "SumOfHighGrade",
  title = "Listening in Class Distribution",
  fontsize.labels = c(12, 8),
  fontfamily.labels = c("Arial", "Courier"),
  align.labels = list(c("center", "center")),
  vColor = "LISTENS_IN_CLASS",
  palette = "RdYlBu",
  border.col = "black"
)


# Chi-sq test
chisq.test(renamed_data$ADD_WORK, renamed_data$GRADE, correct = FALSE)
