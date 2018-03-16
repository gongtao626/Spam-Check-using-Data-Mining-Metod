detect_spam<-function(path, emailspam, model){
	options(warn=-1)#suppress the waring output

	textscan<-scan(path, character(0), quote = NULL, quiet=T) # input the text
	
	#declare the variable for this text
	word_freq_make <- 0
	word_freq_address <- 0
	word_freq_all <- 0
	word_freq_3d <- 0
	word_freq_our<- 0
	word_freq_over<- 0
	word_freq_remove<- 0
	word_freq_internet<- 0
	word_freq_order<- 0
	word_freq_mail<- 0
	word_freq_receive<- 0
	word_freq_will<- 0
	word_freq_people<- 0
	word_freq_report<- 0
	word_freq_addresses<- 0
	word_freq_free<- 0
	word_freq_business<- 0
	word_freq_email<- 0
	word_freq_you<- 0
	word_freq_credit<- 0
	word_freq_your<- 0
	word_freq_font<- 0
	word_freq_000<- 0
	word_freq_money<- 0
	word_freq_hp<- 0
	word_freq_hpl<- 0
	word_freq_george<- 0
	word_freq_650<- 0
	word_freq_lab<- 0
	word_freq_labs<- 0
	word_freq_telnet<- 0
	word_freq_857<- 0
	word_freq_data<- 0
	word_freq_415<- 0
	word_freq_85<- 0
	word_freq_technology<- 0
	word_freq_1999<- 0
	word_freq_parts<- 0
	word_freq_pm<- 0
	word_freq_direct<- 0
	word_freq_cs<- 0
	word_freq_meeting<- 0
	word_freq_original<- 0
	word_freq_project<- 0
	word_freq_re<- 0
	word_freq_edu<- 0
	word_freq_table<- 0
	word_freq_conference<- 0
	char_freq_fenhao<- 0
	char_freq_zuokuohao<- 0
	char_freq_zuozhongkuohao<- 0
	char_freq_tanhao<- 0
	char_freq_dollar<- 0
	char_freq_jinghao<- 0

	capital_run_no<-0
	capital_run_length_average<- 0
	capital_run_length_longest<- 0
	capital_run_length_total<- 0

	word_total<-0
	number_character <- 0
	#scan every word in textscan
	for(word in 1:length(textscan))
	{
		number_character <- number_character + nchar(textscan[word])+1#calculate all the chars including the space character
		if(grepl("([[:punct:]]+)",textscan[word]))#contains non-alphanumeric characters
		{
			characters<-unlist(strsplit(textscan[word],""))#get every char of the word
			for(index in 1:length(characters))
			{
				if(characters == ";")
				 char_freq_fenhao<-char_freq_fenhao+1
				else if(characters == "(")
				 char_freq_zuokuohao<-char_freq_zuokuohao+1
				else if(characters == "[")
				 char_freq_zuozhongkuohao<-char_freq_zuozhongkuohao+1
				else if(characters == "!")
				 char_freq_tanhao<-char_freq_tanhao+1
				else if(characters == "$")
				 char_freq_dollar<-char_freq_dollar+1
				else if(characters == "#")
				 char_freq_jinghao<-char_freq_jinghao+1
			}
		}
		else#alphanumeric strings
		{
			word_total = word_total+1#get the total words number
			if(grepl("([[:upper:]])",textscan[word]))#contains upper letters characters
			{
				tmp <- gsub("[[:lower:]]+",",",gsub("[[:digit:]]+", ",",textscan[word]))#change the digit and lower chars to ,
				tmp <- unlist(strsplit(tmp, "[,]+"))#get  no intterrupted capital list
				tmp <- sapply(tmp,nchar)#get length of each no-intterrupted capitals
				capital_run_no <- capital_run_no + length(tmp)
				
				if(tmp[1] == 0)
				{
					capital_run_no <- capital_run_no-1 #remove the first null value
				}
				
				for(i in 1:length(tmp)){#total length
					capital_run_length_total <- capital_run_length_total+tmp[i]
				}
				if(capital_run_length_longest < tmp[which.max(tmp)])#longest
					capital_run_length_longest = tmp[which.max(tmp)]
			}
			else#word comparision
			{
				if(textscan[word] == "make")
					word_freq_make = word_freq_make+1
				else if(textscan[word] == "address")
					word_freq_address = word_freq_address+1
				else if(textscan[word] == "all")
					word_freq_all = word_freq_all+1
				else if(textscan[word] == "3d")
					word_freq_3d = word_freq_3d+1
				else if(textscan[word] == "our")
					word_freq_our = word_freq_our+1
				else if(textscan[word] == "over")
					word_freq_over = word_freq_over+1
				else if(textscan[word] == "remove")
					word_freq_remove = word_freq_remove+1
				else if(textscan[word] == "internet")
					word_freq_internet = word_freq_internet+1
				else if(textscan[word] == "order")
					word_freq_order = word_freq_order+1
				else if(textscan[word] == "mail")
					word_freq_mail = word_freq_mail+1
				else if(textscan[word] == "receive")
					word_freq_receive = word_freq_receive+1
				else if(textscan[word] == "will")
					word_freq_will = word_freq_will+1
				else if(textscan[word] == "people")
					word_freq_people = word_freq_people+1
				else if(textscan[word] == "report")
					word_freq_report = word_freq_report+1
				else if(textscan[word] == "addresses")
					word_freq_addresses = word_freq_addresses+1
				else if(textscan[word] == "free")
					word_freq_free = word_freq_free+1
				else if(textscan[word] == "business")
					word_freq_business = word_freq_business+1
				else if(textscan[word] == "email")
					word_freq_email = word_freq_email+1
				else if(textscan[word] == "you")
					word_freq_you = word_freq_you+1
				else if(textscan[word] == "credit")
					word_freq_credit = word_freq_credit+1
				else if(textscan[word] == "your")
					word_freq_your = word_freq_your+1
				else if(textscan[word] == "font")
					word_freq_font = word_freq_font+1
				else if(textscan[word] == "000")
					word_freq_000 = word_freq_000+1
				else if(textscan[word] == "money")
					word_freq_money = word_freq_money+1
				else if(textscan[word] == "hp")
					word_freq_hp = word_freq_hp+1
				else if(textscan[word] == "hpl")
					word_freq_hpl = word_freq_hpl+1
				else if(textscan[word] == "george")
					word_freq_george = word_freq_george+1
				else if(textscan[word] == "650")
					word_freq_650 = word_freq_650+1
				else if(textscan[word] == "lab")
					word_freq_lab = word_freq_lab+1
				else if(textscan[word] == "labs")
					word_freq_labs = word_freq_labs+1
				else if(textscan[word] == "telnet")
					word_freq_telnet = word_freq_telnet+1
				else if(textscan[word] == "857")
					word_freq_857 = word_freq_857+1
				else if(textscan[word] == "data")
					word_freq_data = word_freq_data+1
				else if(textscan[word] == "415")
					word_freq_415 = word_freq_415+1
				else if(textscan[word] == "85")
					word_freq_85 = word_freq_85+1
				else if(textscan[word] == "technology")
					word_freq_technology = word_freq_technology+1
				else if(textscan[word] == "1999")
					word_freq_1999 = word_freq_1999+1
				else if(textscan[word] == "parts")
					word_freq_parts = word_freq_parts+1
				else if(textscan[word] == "pm")
					word_freq_pm = word_freq_pm+1
				else if(textscan[word] == "direct")
					word_freq_direct = word_freq_direct+1
				else if(textscan[word] == "cs")
					word_freq_cs = word_freq_cs+1
				else if(textscan[word] == "meeting")
					word_freq_meeting = word_freq_meeting+1
				else if(textscan[word] == "original")
					word_freq_original = word_freq_original+1
				else if(textscan[word] == "project")
					word_freq_project = word_freq_project+1
				else if(textscan[word] == "re")
					word_freq_re = word_freq_re+1
				else if(textscan[word] == "edu")
					word_freq_edu = word_freq_edu+1
				else if(textscan[word] == "table")
					word_freq_table = word_freq_table+1
				else if(textscan[word] == "conference")
					word_freq_conference = word_freq_conference+1
			}
		}#End of else#alphanumeric strings
	}#END OF FOR
	#avoid divide by zero
	if(capital_run_no > 0){
		capital_run_length_average <- capital_run_length_total/capital_run_no
	}
	else{
		capital_run_length_average = 0
	}

	features=c(100*(word_freq_make/word_total),
	100*(word_freq_address /word_total),
	100*(word_freq_all /word_total),
	100*(word_freq_3d /word_total),
	100*(word_freq_our/word_total),
	100*(word_freq_over/word_total),
	100*(word_freq_remove/word_total),
	100*(word_freq_internet/word_total),
	100*(word_freq_order/word_total),
	100*(word_freq_mail/word_total),
	100*(word_freq_receive/word_total),
	100*(word_freq_will/word_total),
	100*(word_freq_people/word_total),
	100*(word_freq_report/word_total),
	100*(word_freq_addresses/word_total),
	100*(word_freq_free/word_total),
	100*(word_freq_business/word_total),
	100*(word_freq_email/word_total),
	100*(word_freq_you/word_total),
	100*(word_freq_credit/word_total),
	100*(word_freq_your/word_total),
	100*(word_freq_font/word_total),
	100*(word_freq_000/word_total),
	100*(word_freq_money/word_total),
	100*(word_freq_hp/word_total),
	100*(word_freq_hpl/word_total),
	100*(word_freq_george/word_total),
	100*(word_freq_650/word_total),
	100*(word_freq_lab/word_total),
	100*(word_freq_labs/word_total),
	100*(word_freq_telnet/word_total),
	100*(word_freq_857/word_total),
	100*(word_freq_data/word_total),
	100*(word_freq_415/word_total),
	100*(word_freq_85/word_total),
	100*(word_freq_technology/word_total),
	100*(word_freq_1999/word_total),
	100*(word_freq_parts/word_total),
	100*(word_freq_pm/word_total),
	100*(word_freq_direct/word_total),
	100*(word_freq_cs/word_total),
	100*(word_freq_meeting/word_total),
	100*(word_freq_original/word_total),
	100*(word_freq_project/word_total),
	100*(word_freq_re/word_total),
	100*(word_freq_edu/word_total),
	100*(word_freq_table/word_total),
	100*(word_freq_conference/word_total),
	100*(char_freq_fenhao/number_character),
	100*(char_freq_zuokuohao/number_character),
	100*(char_freq_zuozhongkuohao/number_character),
	100*(char_freq_tanhao/number_character),
	100*(char_freq_dollar/number_character),
	100*(char_freq_jinghao/number_character),
	capital_run_length_average,
	capital_run_length_longest,
	capital_run_length_total,0)
	
	#convert the features to the same format data frame as emailspam
	newemail<-matrix(features,1,58)
	newemail<-as.data.frame(newemail)
	colnames(newemail)=names(emailspam)
	
	if(predict(model,newemail) == 1)#apply model to the new row 
		print("spam")
	else
		print("non-spam")
}