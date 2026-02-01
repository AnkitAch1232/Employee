#Ankit
#NPI000140
# Installing required packages
install.packages("dplyr")
install.packages("ggplot2")
install.packages("plotly")
install.packages("officer")
install.packages("crayon")
install.packages("na.tools")

# Opening packages
library(dplyr)
library(ggplot2)
library(plotly)
library(officer)
library(crayon)
library(na.tools)

# Importing data
employee_attrition<-read.csv("F:/A STUDY MATERIAL/4th sem/employee_attrition.csv",header= TRUE)
data_attributes<- read_docx("F:/A STUDY MATERIAL/4th sem/Data Attributes.docx")



# Deleting 'gender_short' column
employee_attrition$gender_short <- NULL

# Modifying incorrect names
employee_attrition <- employee_attrition %>%
  mutate(city_name = ifelse(city_name == "New Westminister", "New Westminster", city_name))

employee_attrition <- employee_attrition %>%
  mutate(termreason_desc = ifelse(termreason_desc == "Resignaton", "Resignation", termreason_desc))

employee_attrition <- employee_attrition %>%
  mutate(job_title = ifelse(job_title == "CHief Infromation officer", "Chief Information Officer", job_title))



#analysis and overview of employee_attrition
# Display the dataset
View(employee_attrition)

# Summarize the dataset
summary(employee_attrition)

# Read data attributes from a docx file and display
content <- docx_summary(data_attributes)
View(content)

# Clean the data by keeping only the latest records for each employee
cleandata <- employee_attrition %>%
  group_by(EmployeeID) %>%
  dplyr::arrange(desc(recorddate_key), .by_group = TRUE) %>%
  distinct(EmployeeID, .keep_all = TRUE)

# View the cleaned data
View(cleandata)

# Rename columns in the cleaned data
fcdata <- cleandata
names(fcdata) <- c("Employee.ID", "Record.Date", "Birth.Date", "Hire.Date", "Termination.Date",
                   "Age", "Length.of.Service", "City.Name", "Department.Name", "Job.Title", "Store.Id",
                   "Gender", "Term.Reason", "Term.Type", "STATUS.YEAR", 'STATUS', "Business.Unit")

# Convert categorical variables to factors
fcdata$City.Name <- as.factor(fcdata$City.Name)
fcdata$Department.Name <- as.factor(fcdata$Department.Name)
fcdata$Job.Title <- as.factor(fcdata$Job.Title)
fcdata$Gender <- as.factor(fcdata$Gender)
fcdata$Term.Reason <- as.factor(fcdata$Term.Reason)
fcdata$Term.Type <- as.factor(fcdata$Term.Type)
fcdata$STATUS <- as.factor(fcdata$STATUS)
fcdata$Business.Unit <- as.factor(fcdata$Business.Unit)

# View the final cleaned data
View(fcdata)

# Display the structure of the final cleaned data
str(fcdata)

# Filtering data for different departments within each category
executive_data = fcdata %>% filter(Department.Name == "Executive")
hr_technology_data = fcdata %>% filter(Department.Name == "HR Technology")
employee_record_data = fcdata %>% filter(Department.Name == "Employee Record")
investment_data = fcdata %>% filter(Department.Name == "Investment")
legal_data = fcdata %>% filter(Department.Name == "Legal")
it_data = fcdata %>% filter(Department.Name == "IT")
store_management_data = fcdata %>% filter(Department.Name == "Store Management")

recruitment_data = fcdata %>% filter(Department.Name == "Recruitment")
training_data = fcdata %>% filter(Department.Name == "Training")
customer_service_data = fcdata %>% filter(Department.Name == "Customer Service")
labor_relations_data = fcdata %>% filter(Department.Name == "Labor Relations")

accounting_data = fcdata %>% filter(Department.Name == "Accounting")
audit_data = fcdata %>% filter(Department.Name == "Audit")
accounts_receivable_data = fcdata %>% filter(Department.Name == "Accounts Receivable")
accounts_payable_data = fcdata %>% filter(Department.Name == "Accounts Payable")
compensation_data = fcdata %>% filter(Department.Name == "Compensation")

dairy_data = fcdata %>% filter(Department.Name == "Dairy")
meats_data = fcdata %>% filter(Department.Name == "Meats")

produce_data = fcdata %>% filter(Department.Name == "Produce")
bakery_data = fcdata %>% filter(Department.Name == "Bakery")
processed_foods_data = fcdata %>% filter(Department.Name == "Processed Foods")

# Combining the filtered data for each category
category1 = rbind(executive_data, hr_technology_data, employee_record_data, 
                  investment_data, legal_data, it_data, store_management_data)
category2 = rbind(recruitment_data, training_data, customer_service_data, 
                  labor_relations_data)
category3 = rbind(accounting_data, audit_data, accounts_receivable_data, accounts_payable_data,
                  compensation_data)
category4 = rbind(dairy_data, meats_data)
category5 = rbind(produce_data, bakery_data, processed_foods_data)



#Q1
# A1.1: Male vs Female Analysis
male_vs_female_plot <- cleandata %>%
  filter(STATUS != "TERMINATED") %>%
  select(gender_full, STATUS) %>%
  ggplot(aes(x = gender_full, fill = STATUS)) +
  geom_bar() +
  labs(y = "Number of Employees", title = "Male vs Female") +
  theme(plot.background = element_rect(fill = "#00FFFF")) +
  theme(plot.background = element_rect(fill = "bisque3"))
ggplotly(male_vs_female_plot)

# A1.2: Male/Female vs Stores Analysis
male_female_vs_stores_plot <- cleandata %>%
  filter(STATUS != "TERMINATED") %>%
  select(store_name, gender_full) %>%
  ggplot(aes(x = factor(store_name), fill = gender_full)) +
  geom_bar() +
  labs(x = "Store ID", y = "Number of Employees", 
       title = "Male/Female vs Stores") +
  theme(plot.background = element_rect(fill = "#00FFFF")) +
  theme(plot.background = element_rect(fill = "bisque3"))
ggplotly(male_female_vs_stores_plot)

# A1.3: Male/Female vs Department Name Analysis
male_female_vs_department_plot <- cleandata %>%
  filter(STATUS != "TERMINATED", BUSINESS_UNIT == "STORES") %>%
  ggplot(aes(y = factor(department_name), fill = gender_full)) +
  geom_bar() +
  labs(x = "Number of Employees", y = "Department", 
       title = "Male/Female vs Department Name") +
  theme(plot.background = element_rect(fill = "#00FFFF")) +
  theme(plot.background = element_rect(fill = "bisque3"))
ggplotly(male_female_vs_department_plot)

# A1.4: Head office department name vs Gender Analysis
head_office_vs_gender_plot <- cleandata %>%
  filter(STATUS != "TERMINATED", BUSINESS_UNIT == "STORES") %>%
  ggplot(aes(x = department_name, fill = gender_full)) +
  geom_bar() +
  labs(y = "Number of Employees", x = "Department name", 
       title = "Head office department name vs Gender") +
  theme(plot.background = element_rect(fill = "#00FFFF")) +
  theme(plot.background = element_rect(fill = "bisque3"))
ggplotly(head_office_vs_gender_plot)

#Q2
# A2.1: Age vs Termination Reason Analysis
age_vs_termination_reason_plot <- cleandata %>%
  filter(termreason_desc != "NOT Applicable") %>%
  ggplot(aes(x = age, fill = termreason_desc)) +
  geom_histogram(binwidth = 5) +
  labs(x = "Age", y = "Number of Employees", 
       title = "Age vs Termination Reason") +
  theme(plot.background = element_rect(fill = "#00FFFF")) +
  theme(plot.background = element_rect(fill = "bisque3"))
ggplotly(age_vs_termination_reason_plot)

# A2.2: Gender vs Termination Reason Analysis
gender_vs_termination_reason_plot <- cleandata %>%
  filter(termreason_desc != "NOT Applicable") %>%
  ggplot(aes(x = gender_full, fill = termreason_desc)) +
  geom_bar() +
  labs(x = "Gender", y = "Number of Employees", 
       title = "Gender vs Termination Reason") +
  theme(plot.background = element_rect(fill = "#00FFFF")) +
  theme(plot.background = element_rect(fill = "bisque3"))
ggplotly(gender_vs_termination_reason_plot)

# A2.3: Length of Service vs Termination Reason Analysis
length_of_service_vs_termination_reason_plot <- cleandata %>%
  filter(STATUS != "ACTIVE") %>%
  ggplot(aes(x = length_of_service, fill = termreason_desc)) +
  geom_bar() +
  labs(y = "Number of Employees", x = "Number of Service", 
       title = "Length of Service vs Termination Reason") +
  theme(plot.background = element_rect(fill = "#00FFFF")) +
  theme(plot.background = element_rect(fill = "bisque3"))
ggplotly(length_of_service_vs_termination_reason_plot)

# A2.4: Cities vs Termination Reason Analysis
cities_vs_termination_reason_plot <- cleandata %>%
  filter(termreason_desc != "NOT Applicable") %>%
  ggplot(aes(y = city_name, fill = termreason_desc)) +
  geom_bar() +
  labs(y = "City Name", x = "Number of Employees", 
       title = "Cities vs Termination Reason") +
  theme(plot.background = element_rect(fill = "#00FFFF")) +
  theme(plot.background = element_rect(fill = "bisque3"))
ggplotly(cities_vs_termination_reason_plot)


#q3
# A3.1: Head Office vs Termination Reason Analysis
# For BUSINESS_UNIT "HEADOFFICE"
head_office_termination_plot <- cleandata %>%
  filter(BUSINESS_UNIT == "HEADOFFICE") %>%
  ggplot(aes(x = BUSINESS_UNIT, fill = STATUS)) +
  geom_bar(aes(fill = STATUS), position = "dodge") +
  labs(y = "Number of Employees", x = "HEAD OFFICE", 
       title = "HEAD OFFICE VS Termination Reason") +
  theme(plot.background = element_rect(fill = "#00FFFF")) +
  theme(plot.background = element_rect(fill = "bisque3"))
ggplotly(head_office_termination_plot)

# For BUSINESS_UNIT "STORES"
stores_termination_plot <- cleandata %>%
  filter(BUSINESS_UNIT == "STORES") %>%
  ggplot(aes(x = BUSINESS_UNIT, fill = STATUS)) +
  geom_bar(aes(fill = STATUS), position = "dodge") +
  labs(y = "Number of Employees", x = "Stores", 
       title = "Stores VS Termination Reason") +
  theme(plot.background = element_rect(fill = "#00FFFF")) +
  theme(plot.background = element_rect(fill = "bisque3"))
ggplotly(stores_termination_plot)

# A3.2: Growth of Jobs in Head Office and Stores
# For BUSINESS_UNIT "HEADOFFICE"
head_office_job_growth_plot <- employee_attrition %>%
  filter(BUSINESS_UNIT == "HEADOFFICE") %>%
  ggplot(aes(x = BUSINESS_UNIT, fill = job_title)) +
  geom_bar(aes(fill = job_title), position = "dodge") +
  facet_wrap("~STATUS_YEAR") +
  labs(y = "Number of Employees", x = "HEAD OFFICE", 
       title = "Checking Growth of Head office Jobs") +
  theme(plot.background = element_rect(fill = "#00FFFF")) +
  theme(plot.background = element_rect(fill = "bisque3"))
ggplotly(head_office_job_growth_plot)

# For BUSINESS_UNIT "STORES"
stores_job_growth_plot <- employee_attrition %>%
  filter(BUSINESS_UNIT == "STORES") %>%
  ggplot(aes(x = BUSINESS_UNIT, fill = job_title)) +
  geom_bar(aes(fill = job_title), position = "dodge") +
  facet_wrap("~STATUS_YEAR") +
  labs(y = "Number of Employees", x = "STORE", 
       title = "Checking Growth of Store Jobs") +
  theme(plot.background = element_rect(fill = "#00FFFF")) +
  theme(plot.background = element_rect(fill = "bisque3"))
ggplotly(stores_job_growth_plot)

# A3.3: Store and Head Office Employees per City in 2006 and 2015
# For BUSINESS_UNIT "STORES", STATUS_YEAR "2006", STATUS "ACTIVE"
store_employees_city_2006_plot <- employee_attrition %>%
  filter(BUSINESS_UNIT == "STORES", STATUS_YEAR == "2006", STATUS == "ACTIVE") %>%
  ggplot(aes(x = BUSINESS_UNIT, fill = city_name)) +
  geom_bar(aes(fill = city_name), position = "dodge") +
  labs(y = "Number of Employees", title = "Store employees per CITY 2006") +
  theme(plot.background = element_rect(fill = "#00FFFF")) +
  theme(plot.background = element_rect(fill = "bisque3"))
ggplotly(store_employees_city_2006_plot)

# For BUSINESS_UNIT "STORES", STATUS_YEAR "2015", STATUS "ACTIVE"
store_employees_city_2015_plot <- employee_attrition %>%
  filter(BUSINESS_UNIT == "STORES", STATUS_YEAR == "2015", STATUS == "ACTIVE") %>%
  ggplot(aes(x = BUSINESS_UNIT), fill = city_name) +
  geom_bar(aes(fill = city_name), position = "dodge") +
  labs(y = "Number of Employees", title = "Store employees per CITY 2015") +
  theme(plot.background = element_rect(fill = "#00FFFF")) +
  theme(plot.background = element_rect(fill = "bisque3"))
ggplotly(store_employees_city_2015_plot)

# For BUSINESS_UNIT "HEADOFFICE", STATUS_YEAR "2006", STATUS "ACTIVE"
head_office_employees_2006_plot <- employee_attrition %>%
  filter(BUSINESS_UNIT == "HEADOFFICE", STATUS_YEAR == "2006", STATUS == "ACTIVE") %>%
  ggplot(aes(x = BUSINESS_UNIT, fill = city_name)) +
  geom_bar(aes(fill = city_name), position = "dodge") +
  labs(y = "Number of Employees", title = "Head Office employees in 2006") +
  theme(plot.background = element_rect(fill = "#00FFFF")) +
  theme(plot.background = element_rect(fill = "bisque3"))
ggplotly(head_office_employees_2006_plot)

# For BUSINESS_UNIT "HEADOFFICE", STATUS_YEAR "2015", STATUS "ACTIVE"
head_office_employees_2015_plot <- employee_attrition %>%
  filter(BUSINESS_UNIT == "HEADOFFICE", STATUS_YEAR == "2015", STATUS == "ACTIVE") %>%
  ggplot(aes(x = BUSINESS_UNIT, fill = city_name)) +
  geom_bar(aes(fill = city_name), position = "dodge") +
  labs(y = "Number of Employees", title = "Head Office employees in 2015") +
  theme(plot.background = element_rect(fill = "#00FFFF")) +
  theme(plot.background = element_rect(fill = "bisque3"))
(head_office_employees_2015_plot)

#q4
# A4.1: Category vs Status Analysis for the year 2015
# Category 1 Analysis
category411_plot <- category1 %>%
  filter(STATUS.YEAR == "2015") %>%
  ggplot(aes(x = STATUS, fill = STATUS)) +
  geom_bar(aes(), position = "dodge") +
  labs(y = "Number of Employees", x = "Status", 
       title = "Category 1 vs Status") +
  theme(plot.background = element_rect(fill = "#00FFFF")) +
  theme(plot.background = element_rect(fill = "bisque3"))
ggplotly(category411_plot)

# Category 2 Analysis
category412_plot <- category2 %>%
  filter(STATUS.YEAR == "2015") %>%
  ggplot(aes(x = STATUS, fill = STATUS)) +
  geom_bar(aes(), position = "dodge") +
  labs(y = "Number of Employees", x = "Status", 
       title = "Category 2 vs Status") +
  theme(plot.background = element_rect(fill = "#00FFFF")) +
  theme(plot.background = element_rect(fill = "bisque3"))
ggplotly(category412_plot)

# Category 3 Analysis
category413_plot <- category3 %>%
  filter(STATUS.YEAR == "2015") %>%
  ggplot(aes(x = STATUS, fill = STATUS)) +
  geom_bar(aes(), position = "dodge") +
  labs(y = "Number of Employees", x = "Status", 
       title = "Category 3 vs Status") +
  theme(plot.background = element_rect(fill = "#00FFFF")) +
  theme(plot.background = element_rect(fill = "bisque3"))
ggplotly(category413_plot)

# Category 4 Analysis
category414_plot <- category4 %>%
  filter(STATUS.YEAR == "2015") %>%
  ggplot(aes(x = STATUS, fill = STATUS)) +
  geom_bar(aes(), position = "dodge") +
  labs(y = "Number of Employees", x = "Status", 
       title = "Category 4 vs Status") +
  theme(plot.background = element_rect(fill = "#00FFFF")) +
  theme(plot.background = element_rect(fill = "bisque3"))
ggplotly(category414_plot)

# Category 5 Analysis
category415_plot <- category5 %>%
  filter(STATUS.YEAR == "2015") %>%
  ggplot(aes(x = STATUS, fill = STATUS)) +
  geom_bar(aes(), position = "dodge") +
  labs(y = "Number of Employees", x = "Status", 
       title = "Category 5 vs Status") +
  theme(plot.background = element_rect(fill = "#00FFFF")) +
  theme(plot.background = element_rect(fill = "bisque3"))
ggplotly(category415_plot)

# A4.2: Gender vs Department Name Analysis for Categories 1 to 5
# Category 1
t_plot <- category1 %>%
  select(Gender,Department.Name)%>%
  ggplot(aes(y=Department.Name),fill=GENDER)+
  geom_bar(aes(),position="dodge")+
  labs(y="Department Name",x="Number of Employees",
       title="Gender vs Category 1")+
  theme(plot.background = element_rect(fill="#00FFFF"))+
  theme(plot.background = element_rect(fill = "bisque3"))
ggplotly(t_plot)

# Category 2
u_plot <- category2 %>%
  select(Gender,Department.Name)%>%
  ggplot(aes(y=Department.Name),fill=GENDER)+
  geom_bar(aes(),position="dodge")+
  labs(y="Department Name",x="Number of Employees",
       title="Gender vs Category 2")+
  theme(plot.background = element_rect(fill="#00FFFF"))+
  theme(plot.background = element_rect(fill = "bisque3"))
ggplotly(u_plot)

# Category 3
v_plot <- category3 %>%
  select(Gender,Department.Name)%>%
  ggplot(aes(y=Department.Name),fill=GENDER)+
  geom_bar(aes(),position="dodge")+
  labs(y="Department Name",x="Number of Employees",
       title="Gender vs Category 3")+
  theme(plot.background = element_rect(fill="#00FFFF"))+
  theme(plot.background = element_rect(fill = "bisque3"))
ggplotly(v_plot)

# Category 4
w_plot <- category4 %>%
  select(Gender,Department.Name)%>%
  ggplot(aes(y=Department.Name),fill=GENDER)+
  geom_bar(aes(),position="dodge")+
  labs(y="Department Name",x="Number of Employees",
       title="Gender vs Category 4")+
  theme(plot.background = element_rect(fill="#00FFFF"))+
  theme(plot.background = element_rect(fill = "bisque3"))
ggplotly(w_plot)

# Category 5
x_plot <- category5 %>%
  select(Gender,Department.Name)%>%
  ggplot(aes(y=Department.Name),fill=GENDER)+
  geom_bar(aes(),position="dodge")+
  labs(y="Department Name",x="Number of Employees",
       title="Gender vs Category 5")+
  theme(plot.background = element_rect(fill="#00FFFF"))+
  theme(plot.background = element_rect(fill = "bisque3"))
ggplotly(x_plot)

# a4.3: Analysis for Preferred Age of Employees for Different Categories

# Category 1
category431_plot <- category1 %>%
  select(Age) %>%
  ggplot(aes(x = Age)) +
  geom_histogram(binwidth = 5) +
  labs(y = "Number of employees", 
       title = "Preferred Age of employees for Category 1") +
  theme(plot.background = element_rect(fill = "#00FFFF")) +
  theme(plot.background = element_rect(fill = "bisque3"))
ggplotly(category431_plot)

# Category 2
category432_plot <- category2 %>%
  select(Age) %>%
  ggplot(aes(x = Age)) +
  geom_histogram(binwidth = 5) +
  labs(y = "Number of employees", 
       title = "Preferred Age of employees for Category 2") +
  theme(plot.background = element_rect(fill = "#00FFFF")) +
  theme(plot.background = element_rect(fill = "bisque3"))
ggplotly(category432_plot)

# Category 3
category433_plot <- category3 %>%
  select(Age) %>%
  ggplot(aes(x = Age)) +
  geom_histogram(binwidth = 5) +
  labs(y = "Number of employees", 
       title = "Preferred Age of employees for Category 3") +
  theme(plot.background = element_rect(fill = "#00FFFF")) +
  theme(plot.background = element_rect(fill = "bisque3"))
ggplotly(category433_plot)

# Category 4
category434_plot <- category4 %>%
  select(Age) %>%
  ggplot(aes(x = Age)) +
  geom_histogram(binwidth = 5) +
  labs(y = "Number of employees", 
       title = "Preferred Age of employees for Category 4") +
  theme(plot.background = element_rect(fill = "#00FFFF")) +
  theme(plot.background = element_rect(fill = "bisque3"))
ggplotly(category434_plot)

# Category 5
category435_plot <- category5 %>%
  select(Age) %>%
  ggplot(aes(x = Age)) +
  geom_histogram(binwidth = 5) +
  labs(y = "Number of employees", 
       title = "Preferred Age of employees for Category 5") +
  theme(plot.background = element_rect(fill = "#00FFFF")) +
  theme(plot.background = element_rect(fill = "bisque3"))
ggplotly(category435_plot)


#q5
# a5.1: Analysis for Hiring Age Preference for Different Categories
# Hiring Age Preference of Category 1
ad_plot <- category1 %>%
  filter(STATUS == "ACTIVE") %>%
  select(Age, Length.of.Service) %>%
  ggplot(aes(x = Age, y = Length.of.Service)) +
  geom_point() +
  labs(y = "Experience", 
       title = "Hiring age preference of category 1") +
  theme(plot.background = element_rect(fill = "#00FFFF")) +
  theme(plot.background = element_rect(fill = "bisque3"))
ggplotly(ad_plot)

# Hiring Age Preference of Category 2
ae_plot <- category2 %>%
  filter(STATUS == "ACTIVE") %>%
  select(Age, Length.of.Service) %>%
  ggplot(aes(x = Age, y = Length.of.Service)) +
  geom_point() +
  labs(y = "Experience", 
       title = "Hiring age preference of category 2") +
  theme(plot.background = element_rect(fill = "#00FFFF")) +
  theme(plot.background = element_rect(fill = "bisque3"))
ggplotly(ae_plot)

# Hiring Age Preference of Category 3
af_plot <- category3 %>%
  filter(STATUS == "ACTIVE") %>%
  select(Age, Length.of.Service) %>%
  ggplot(aes(x = Age, y = Length.of.Service)) +
  geom_point() +
  labs(y = "Experience", 
       title = "Hiring age preference of category 3") +
  theme(plot.background = element_rect(fill = "#00FFFF")) +
  theme(plot.background = element_rect(fill = "bisque3"))
ggplotly(af_plot)

# Hiring Age Preference of Category 4
ag_plot <- category4 %>%
  filter(STATUS == "ACTIVE") %>%
  select(Age, Length.of.Service) %>%
  ggplot(aes(x = Age, y = Length.of.Service)) +
  geom_point() +
  labs(y = "Experience", 
       title = "Hiring age preference of category 4") +
  theme(plot.background = element_rect(fill = "#00FFFF")) +
  theme(plot.background = element_rect(fill = "bisque3"))
ggplotly(ag_plot) 

# Hiring Age Preference of Category 5
ah_plot <- category5 %>%
  filter(STATUS == "ACTIVE") %>%
  select(Age, Length.of.Service) %>%
  ggplot(aes(x = Age, y = Length.of.Service)) +
  geom_point() +
  labs(y = "Experience", 
       title = "Hiring age preference of category 5") +
  theme(plot.background = element_rect(fill = "#00FFFF")) +
  theme(plot.background = element_rect(fill = "bisque3"))
ggplotly(ah_plot)



# A5.2: Hiring Gender Preference Analysis for Different Categories

# Gender preference analysis for category 1
ai_plot <- category1 %>%
  filter(STATUS == "ACTIVE") %>%
  select(Gender, Length.of.Service) %>%
  ggplot(aes(x = Gender, y = Length.of.Service)) +
  geom_point() +
  labs(y = "Experience", 
       title = "Hiring gender preference of category 1") +
  theme(plot.background = element_rect(fill = "#00FFFF")) +
  theme(plot.background = element_rect(fill = "bisque3"))
ggplotly(ai_plot)

# Gender preference analysis for category 2
aj_plot <- category2 %>%
  filter(STATUS == "ACTIVE") %>%
  select(Gender, Length.of.Service) %>%
  ggplot(aes(x = Gender, y = Length.of.Service)) +
  geom_point() +
  labs(y = "Experience", 
       title = "Hiring gender preference of category 2") +
  theme(plot.background = element_rect(fill = "#00FFFF")) +
  theme(plot.background = element_rect(fill = "bisque3"))
ggplotly(aj_plot)

# Gender preference analysis for category 3
ak_plot <- category3 %>%
  filter(STATUS == "ACTIVE") %>%
  select(Gender, Length.of.Service) %>%
  ggplot(aes(x = Gender, y = Length.of.Service)) +
  geom_point() +
  labs(y = "Experience", 
       title = "Hiring gender preference of category 3") +
  theme(plot.background = element_rect(fill = "#00FFFF")) +
  theme(plot.background = element_rect(fill = "bisque3"))
ggplotly(ak_plot)

# Gender preference analysis for category 4
al_plot <- category4 %>%
  filter(STATUS == "ACTIVE") %>%
  select(Gender, Length.of.Service) %>%
  ggplot(aes(x = Gender, y = Length.of.Service)) +
  geom_point() +
  labs(y = "Experience", 
       title = "Hiring gender preference of category 4") +
  theme(plot.background = element_rect(fill = "#00FFFF")) +
  theme(plot.background = element_rect(fill = "bisque3"))
ggplotly(al_plot)

# Gender preference analysis for category 5
am_plot <- category5 %>%
  filter(STATUS == "ACTIVE") %>%
  select(Gender, Length.of.Service) %>%
  ggplot(aes(x = Gender, y = Length.of.Service)) +
  geom_point() +
  labs(y = "Experience", 
       title = "Hiring gender preference of category 5") +
  theme(plot.background = element_rect(fill = "#00FFFF")) +
  theme(plot.background = element_rect(fill = "bisque3"))
ggplotly(am_plot)



# a5.3: Hiring Preference Analysis in Cities for Different Categories
# Category 1
category531_city_plot <- category1 %>%
  filter(STATUS == "ACTIVE") %>%
  select(City.Name, Length.of.Service) %>%
  ggplot(aes(y = City.Name, x = Length.of.Service)) +
  geom_point() +
  labs(x = "Experience", y = "City Name", 
       title = "Hiring preference in city of category 1") +
  theme(plot.background = element_rect(fill = "#00FFFF")) +
  theme(plot.background = element_rect(fill = "bisque3"))
ggplotly(category531_city_plot)

# Category 2
category532_city_plot <- category2 %>%
  filter(STATUS == "ACTIVE") %>%
  select(City.Name, Length.of.Service) %>%
  ggplot(aes(y = City.Name, x = Length.of.Service)) +
  geom_point() +
  labs(x = "Experience", y = "City Name", 
       title = "Hiring preference in city of category 2") +
  theme(plot.background = element_rect(fill = "#00FFFF")) +
  theme(plot.background = element_rect(fill = "bisque3"))
ggplotly(category532_city_plot)

# Category 3
category533_city_plot <- category3 %>%
  filter(STATUS == "ACTIVE") %>%
  select(City.Name, Length.of.Service) %>%
  ggplot(aes(y = City.Name, x = Length.of.Service)) +
  geom_point() +
  labs(x = "Experience", y = "City Name", 
       title = "Hiring preference in city of category 3") +
  theme(plot.background = element_rect(fill = "#00FFFF")) +
  theme(plot.background = element_rect(fill = "bisque3"))
ggplotly(category533_city_plot)

# Category 4
category534_city_plot <- category4 %>%
  filter(STATUS == "ACTIVE") %>%
  select(City.Name, Length.of.Service) %>%
  ggplot(aes(y = City.Name, x = Length.of.Service)) +
  geom_point() +
  labs(x = "Experience", y = "City Name", 
       title = "Hiring preference in city of category 4") +
  theme(plot.background = element_rect(fill = "#00FFFF")) +
  theme(plot.background = element_rect(fill = "bisque3"))
ggplotly(category534_city_plot)

# Category 5
category535_city_plot <- category5 %>%
  filter(STATUS == "ACTIVE") %>%
  select(City.Name, Length.of.Service) %>%
  ggplot(aes(y = City.Name, x = Length.of.Service)) +
  geom_point() +
  labs(x = "Experience", y = "City Name", 
       title = "Hiring preference in city of category 5") +
  theme(plot.background = element_rect(fill = "#00FFFF")) +
  theme(plot.background = element_rect(fill = "bisque3"))
ggplotly(category535_city_plot)

