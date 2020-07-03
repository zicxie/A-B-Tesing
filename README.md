# Apple-Music A/B Testing
Lady Gaga is launching a new Album Chromatica,  we want to send push notifications to Apple Music Subscribers about this to increase user engagement, We also want to do A/B testing on this campaign.
Three Steps are included in this project:
1. The A/B Testing plan for Push Notification on any new album release.
2. The <a href="https://zicxie.shinyapps.io/AB_Testing_Samplesize_calculator/">Sample Size Calculator </a> for required sample sizes, required traffic or duration of data collection for the experiments. 
3. The results interpreter Python pacakage with visualizations based on statistical testings.

## Data Description of sample input data


### rate_metrics.csv

isHoldout: Boolean column indicate if test/control(holdout) group, 1 is control(holdout) group, 0 is test group;
population_user_count: Total unique user count assigned to each group;
banner_seen_user_count: Total unique user count that saw the banner in each group;
content_seen_user_count: Total unique user count that visit new album page in each group;
content_seen_user_count: Total unique user count that visit new album page in each group;
content_play_user_count: Total unique user count that play new album in each group;

### continious_metrics.csv

consumerId: unique identifier for user;
isHoldout: Boolean column indicate if test/control(holdout) group, 1 is control(holdout) group, 0 is test group;
listening_time_hrs: Apple Music overall average listening time in hours for each user within 7 days after the banner send in each group;
album_song_30s_play_count: Apple Music overall average plays (at least 30 seconds) counts for each user within 7 days after the banner send in each group;


