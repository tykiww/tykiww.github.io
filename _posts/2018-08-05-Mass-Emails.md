---
layout: post
title: "Mass Emails Using AppScript (A Non R Post)"
fb-img: 
tags: [Appscript, Google, Gmail, Mass emails, Personalized Emails]
comments: true
---



Receiving too many emails isn't very fun. Worse than that, having to send way too many emails is even more of a pain. When you don't have software to automate tasks for you, how do you go about to do this on your own?

![](https://firemeibegyou.com/wp-content/uploads/2016/05/inbox-zero.jpg)

Lately, I've been messing with the AppScript within the Google product realm. I didn't realize that macros and scripts even existed in google until pretty recently (Even in word or in forms!). In an effort to cut down wasted time, I have recently been carving different macros for efficiency. The greatest part, the scripting language is very similar to javascript. In fact, it's pretty much the exact same. Just with a bit less functionality (hooray!). I'm not the biggest fan of VBA, so it really plays in my favor. 

Today, I'll show you a neat trick you can use (if you use the google service) with the email feature.

Let's create a pretend situation:

You're part of a new company hoping to be the next insurgent disruptor for baby diapers. As one of the digital marketers, you realize that you are in charge of building relationships and reflecting the product, placement, and price with your flavor of promotion. 

After gathering a sample from many of your distribution clients, you begin to see a pattern that reflects that your personal clients tend to respond because of 4 different reasons. Speed, connection, accesibility, and visual appeal (in descending in order). Yet, it seems like the top 3 reasons comprise most of the effect for your company.

You know that the historical response rate for sending non-personal messages is about 30%, but the response rate increases by another 30% for every personal message sent. All that really needs to change from mail to mail is maybe a personalized "How is your family doing?" or even just addressing it to "Jethro,".

You don't want to be keeping the clients waiting on your one by one responses, but you have a list of 150 client relationships you want to keep. It will take too long to write too many personal messages. What do you do?

![](https://tykiww.github.io/assets/mass-email/1.png)

The solution is to create your own mass email macro product (holy, I made a long winded situation for such a small problem).

There are only 2 main components to create this function:
  - Layout (using google sheets)
  - Script

As long as you know what you want to put on the page, all you do after is reference the cells and make a fancy button to run the script. 

Let's begin with the layout of the google sheet.

Mine looks a bit like this. You're welcome to organize this to your own liking. I put in the special message before and after just in case it fit my style and the way I communicated with them. I also randomly generated text from this [site](http://www.randomtextgenerator.com/) for the body, so don't judge me.

![](https://tykiww.github.io/assets/mass-email/2.png)

For recurring sections, I added an Email body and signature. I also didn't want to forget a comma after addressing their name, so I used `CONCATENATE()` to get the job done. That tab is hidden. Get creative with how you want to work it!

Our layout isn't done yet. Let's create a button that we will click to send out our mass emails. To do this, click on Insert > Image. I decided to do a quick google search for some diaper for my button (very fitting).

Now we'll move onto the scripting portion. To begin, go to the Tools > Script editor in the top bar. This will open up a new window for you with a blank script. Title this as what you like and let's begin with our code.

Since AppScript is a scripting language and doesn't have much of a check and proceed feel like R, we need to make sure to get the code as correct as possible before running the lines. We'll first begin by an empty function shell.

```r
function CustomEmail() {

}
```

Next, we will set variables and create our objects. Our first line gets us the sheet of interest, then we select our range to be from "A6:F506" to allow for at least 500 mass emails at a time. After getting all of our values, we are set to proceed.

```r
function CustomEmail() {
  var sheet = SpreadsheetApp.getActiveSheet();
  var range = sheet.getRange("A6:F506");
  var TheData = range.getValues();
  var um = TheData
}
```

Now, we want to loop through the dataset until it stops, so the best thing to do is create a break whenever the emails stop. Else, we will be concatenating the information we need from the selected cells of interest. Note here that the first object starts at 0, not 1. So make sure you are selecting the correct column when we specify row[x]. Col A = 0, Col B = 1... and so on. Col and row matrix indices are done through double square brackets.

```r
function CustomEmail() {
  var sheet = SpreadsheetApp.getActiveSheet();
  var range = sheet.getRange("A2:J500");
  var UserData = range.getValues();
  var range2 = sheet.getRange("I1:K4");
  var Data2 = range2.getValues;
  for (i in UserData) {
    if (UserData[i][1] == "") {
      break;
      } else {
      var row = UserData[i];
      var email = row[1];
      var text = row[3] + "\n" + "\n" + row[4] + "\n" + "\n" + Data2[1][8] + "\n" + row[5] + "\n" + Data2[3][8];
      MailApp.sendEmail(email,Data2[10][1],text);
    }
  }
}
```

The `Mailapp.sendEmail()` function is a part of google scripts. If I am certain, it may even be used in regular excel. I'll have to take a look at that. 

The final touches are made by clicking the three dots on the right corner of the image, then selecting "Assign script". This will take you to a text box where you enter the script name. Mine is "SendEmail".

![](https://tykiww.github.io/assets/mass-email/3.png)

Now what this button will do, is send an email to all your recipients with a custom and base message every single time you click on it. If you did it right, your email will look like this!

![](https://tykiww.github.io/assets/mass-email/4.png)

Now your company is happy, you're happy, and so are your clients. Win Win Win.

Aside from sending mass emails, AppScript has given me information on email traffic data, specific cell change notifications, app creation, automation of specific tasks and calculations, and much more. I'll be playing around with this much more in the future. Or maybe I won't need it so much. Who knows?

Thank you for reading!
