{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.GetTrailStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a JSON-formatted list of information about the specified trail. Fields include information on delivery errors, Amazon SNS and Amazon S3 errors, and start and stop logging times for each trail. This operation returns trail status from a single region. To return trail status from all regions, you must call the operation on each region.
module Network.AWS.CloudTrail.GetTrailStatus
  ( -- * Creating a request
    GetTrailStatus (..),
    mkGetTrailStatus,

    -- ** Request lenses
    gtsName,

    -- * Destructuring the response
    GetTrailStatusResponse (..),
    mkGetTrailStatusResponse,

    -- ** Response lenses
    gtsrsTimeLoggingStopped,
    gtsrsLatestDeliveryError,
    gtsrsLatestDigestDeliveryTime,
    gtsrsLatestNotificationAttemptSucceeded,
    gtsrsStartLoggingTime,
    gtsrsLatestNotificationError,
    gtsrsLatestDeliveryAttemptSucceeded,
    gtsrsIsLogging,
    gtsrsTimeLoggingStarted,
    gtsrsLatestDigestDeliveryError,
    gtsrsLatestDeliveryAttemptTime,
    gtsrsLatestDeliveryTime,
    gtsrsLatestCloudWatchLogsDeliveryTime,
    gtsrsLatestCloudWatchLogsDeliveryError,
    gtsrsLatestNotificationTime,
    gtsrsLatestNotificationAttemptTime,
    gtsrsStopLoggingTime,
    gtsrsResponseStatus,
  )
where

import Network.AWS.CloudTrail.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The name of a trail about which you want the current status.
--
-- /See:/ 'mkGetTrailStatus' smart constructor.
newtype GetTrailStatus = GetTrailStatus'
  { -- | Specifies the name or the CloudTrail ARN of the trail for which you are requesting status. To get the status of a shadow trail (a replication of the trail in another region), you must specify its ARN. The format of a trail ARN is:
    --
    -- @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTrailStatus' with the minimum fields required to make a request.
--
-- * 'name' - Specifies the name or the CloudTrail ARN of the trail for which you are requesting status. To get the status of a shadow trail (a replication of the trail in another region), you must specify its ARN. The format of a trail ARN is:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@
mkGetTrailStatus ::
  -- | 'name'
  Lude.Text ->
  GetTrailStatus
mkGetTrailStatus pName_ = GetTrailStatus' {name = pName_}

-- | Specifies the name or the CloudTrail ARN of the trail for which you are requesting status. To get the status of a shadow trail (a replication of the trail in another region), you must specify its ARN. The format of a trail ARN is:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsName :: Lens.Lens' GetTrailStatus Lude.Text
gtsName = Lens.lens (name :: GetTrailStatus -> Lude.Text) (\s a -> s {name = a} :: GetTrailStatus)
{-# DEPRECATED gtsName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest GetTrailStatus where
  type Rs GetTrailStatus = GetTrailStatusResponse
  request = Req.postJSON cloudTrailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetTrailStatusResponse'
            Lude.<$> (x Lude..?> "TimeLoggingStopped")
            Lude.<*> (x Lude..?> "LatestDeliveryError")
            Lude.<*> (x Lude..?> "LatestDigestDeliveryTime")
            Lude.<*> (x Lude..?> "LatestNotificationAttemptSucceeded")
            Lude.<*> (x Lude..?> "StartLoggingTime")
            Lude.<*> (x Lude..?> "LatestNotificationError")
            Lude.<*> (x Lude..?> "LatestDeliveryAttemptSucceeded")
            Lude.<*> (x Lude..?> "IsLogging")
            Lude.<*> (x Lude..?> "TimeLoggingStarted")
            Lude.<*> (x Lude..?> "LatestDigestDeliveryError")
            Lude.<*> (x Lude..?> "LatestDeliveryAttemptTime")
            Lude.<*> (x Lude..?> "LatestDeliveryTime")
            Lude.<*> (x Lude..?> "LatestCloudWatchLogsDeliveryTime")
            Lude.<*> (x Lude..?> "LatestCloudWatchLogsDeliveryError")
            Lude.<*> (x Lude..?> "LatestNotificationTime")
            Lude.<*> (x Lude..?> "LatestNotificationAttemptTime")
            Lude.<*> (x Lude..?> "StopLoggingTime")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetTrailStatus where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.GetTrailStatus" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetTrailStatus where
  toJSON GetTrailStatus' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Name" Lude..= name)])

instance Lude.ToPath GetTrailStatus where
  toPath = Lude.const "/"

instance Lude.ToQuery GetTrailStatus where
  toQuery = Lude.const Lude.mempty

-- | Returns the objects or data listed below if successful. Otherwise, returns an error.
--
-- /See:/ 'mkGetTrailStatusResponse' smart constructor.
data GetTrailStatusResponse = GetTrailStatusResponse'
  { -- | This field is no longer in use.
    timeLoggingStopped :: Lude.Maybe Lude.Text,
    -- | Displays any Amazon S3 error that CloudTrail encountered when attempting to deliver log files to the designated bucket. For more information see the topic <https://docs.aws.amazon.com/AmazonS3/latest/API/ErrorResponses.html Error Responses> in the Amazon S3 API Reference.
    latestDeliveryError :: Lude.Maybe Lude.Text,
    -- | Specifies the date and time that CloudTrail last delivered a digest file to an account's Amazon S3 bucket.
    latestDigestDeliveryTime :: Lude.Maybe Lude.Timestamp,
    -- | This field is no longer in use.
    latestNotificationAttemptSucceeded :: Lude.Maybe Lude.Text,
    -- | Specifies the most recent date and time when CloudTrail started recording API calls for an AWS account.
    startLoggingTime :: Lude.Maybe Lude.Timestamp,
    -- | Displays any Amazon SNS error that CloudTrail encountered when attempting to send a notification. For more information about Amazon SNS errors, see the <https://docs.aws.amazon.com/sns/latest/dg/welcome.html Amazon SNS Developer Guide> .
    latestNotificationError :: Lude.Maybe Lude.Text,
    -- | This field is no longer in use.
    latestDeliveryAttemptSucceeded :: Lude.Maybe Lude.Text,
    -- | Whether the CloudTrail is currently logging AWS API calls.
    isLogging :: Lude.Maybe Lude.Bool,
    -- | This field is no longer in use.
    timeLoggingStarted :: Lude.Maybe Lude.Text,
    -- | Displays any Amazon S3 error that CloudTrail encountered when attempting to deliver a digest file to the designated bucket. For more information see the topic <https://docs.aws.amazon.com/AmazonS3/latest/API/ErrorResponses.html Error Responses> in the Amazon S3 API Reference.
    latestDigestDeliveryError :: Lude.Maybe Lude.Text,
    -- | This field is no longer in use.
    latestDeliveryAttemptTime :: Lude.Maybe Lude.Text,
    -- | Specifies the date and time that CloudTrail last delivered log files to an account's Amazon S3 bucket.
    latestDeliveryTime :: Lude.Maybe Lude.Timestamp,
    -- | Displays the most recent date and time when CloudTrail delivered logs to CloudWatch Logs.
    latestCloudWatchLogsDeliveryTime :: Lude.Maybe Lude.Timestamp,
    -- | Displays any CloudWatch Logs error that CloudTrail encountered when attempting to deliver logs to CloudWatch Logs.
    latestCloudWatchLogsDeliveryError :: Lude.Maybe Lude.Text,
    -- | Specifies the date and time of the most recent Amazon SNS notification that CloudTrail has written a new log file to an account's Amazon S3 bucket.
    latestNotificationTime :: Lude.Maybe Lude.Timestamp,
    -- | This field is no longer in use.
    latestNotificationAttemptTime :: Lude.Maybe Lude.Text,
    -- | Specifies the most recent date and time when CloudTrail stopped recording API calls for an AWS account.
    stopLoggingTime :: Lude.Maybe Lude.Timestamp,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTrailStatusResponse' with the minimum fields required to make a request.
--
-- * 'timeLoggingStopped' - This field is no longer in use.
-- * 'latestDeliveryError' - Displays any Amazon S3 error that CloudTrail encountered when attempting to deliver log files to the designated bucket. For more information see the topic <https://docs.aws.amazon.com/AmazonS3/latest/API/ErrorResponses.html Error Responses> in the Amazon S3 API Reference.
-- * 'latestDigestDeliveryTime' - Specifies the date and time that CloudTrail last delivered a digest file to an account's Amazon S3 bucket.
-- * 'latestNotificationAttemptSucceeded' - This field is no longer in use.
-- * 'startLoggingTime' - Specifies the most recent date and time when CloudTrail started recording API calls for an AWS account.
-- * 'latestNotificationError' - Displays any Amazon SNS error that CloudTrail encountered when attempting to send a notification. For more information about Amazon SNS errors, see the <https://docs.aws.amazon.com/sns/latest/dg/welcome.html Amazon SNS Developer Guide> .
-- * 'latestDeliveryAttemptSucceeded' - This field is no longer in use.
-- * 'isLogging' - Whether the CloudTrail is currently logging AWS API calls.
-- * 'timeLoggingStarted' - This field is no longer in use.
-- * 'latestDigestDeliveryError' - Displays any Amazon S3 error that CloudTrail encountered when attempting to deliver a digest file to the designated bucket. For more information see the topic <https://docs.aws.amazon.com/AmazonS3/latest/API/ErrorResponses.html Error Responses> in the Amazon S3 API Reference.
-- * 'latestDeliveryAttemptTime' - This field is no longer in use.
-- * 'latestDeliveryTime' - Specifies the date and time that CloudTrail last delivered log files to an account's Amazon S3 bucket.
-- * 'latestCloudWatchLogsDeliveryTime' - Displays the most recent date and time when CloudTrail delivered logs to CloudWatch Logs.
-- * 'latestCloudWatchLogsDeliveryError' - Displays any CloudWatch Logs error that CloudTrail encountered when attempting to deliver logs to CloudWatch Logs.
-- * 'latestNotificationTime' - Specifies the date and time of the most recent Amazon SNS notification that CloudTrail has written a new log file to an account's Amazon S3 bucket.
-- * 'latestNotificationAttemptTime' - This field is no longer in use.
-- * 'stopLoggingTime' - Specifies the most recent date and time when CloudTrail stopped recording API calls for an AWS account.
-- * 'responseStatus' - The response status code.
mkGetTrailStatusResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetTrailStatusResponse
mkGetTrailStatusResponse pResponseStatus_ =
  GetTrailStatusResponse'
    { timeLoggingStopped = Lude.Nothing,
      latestDeliveryError = Lude.Nothing,
      latestDigestDeliveryTime = Lude.Nothing,
      latestNotificationAttemptSucceeded = Lude.Nothing,
      startLoggingTime = Lude.Nothing,
      latestNotificationError = Lude.Nothing,
      latestDeliveryAttemptSucceeded = Lude.Nothing,
      isLogging = Lude.Nothing,
      timeLoggingStarted = Lude.Nothing,
      latestDigestDeliveryError = Lude.Nothing,
      latestDeliveryAttemptTime = Lude.Nothing,
      latestDeliveryTime = Lude.Nothing,
      latestCloudWatchLogsDeliveryTime = Lude.Nothing,
      latestCloudWatchLogsDeliveryError = Lude.Nothing,
      latestNotificationTime = Lude.Nothing,
      latestNotificationAttemptTime = Lude.Nothing,
      stopLoggingTime = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | This field is no longer in use.
--
-- /Note:/ Consider using 'timeLoggingStopped' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrsTimeLoggingStopped :: Lens.Lens' GetTrailStatusResponse (Lude.Maybe Lude.Text)
gtsrsTimeLoggingStopped = Lens.lens (timeLoggingStopped :: GetTrailStatusResponse -> Lude.Maybe Lude.Text) (\s a -> s {timeLoggingStopped = a} :: GetTrailStatusResponse)
{-# DEPRECATED gtsrsTimeLoggingStopped "Use generic-lens or generic-optics with 'timeLoggingStopped' instead." #-}

-- | Displays any Amazon S3 error that CloudTrail encountered when attempting to deliver log files to the designated bucket. For more information see the topic <https://docs.aws.amazon.com/AmazonS3/latest/API/ErrorResponses.html Error Responses> in the Amazon S3 API Reference.
--
-- /Note:/ Consider using 'latestDeliveryError' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrsLatestDeliveryError :: Lens.Lens' GetTrailStatusResponse (Lude.Maybe Lude.Text)
gtsrsLatestDeliveryError = Lens.lens (latestDeliveryError :: GetTrailStatusResponse -> Lude.Maybe Lude.Text) (\s a -> s {latestDeliveryError = a} :: GetTrailStatusResponse)
{-# DEPRECATED gtsrsLatestDeliveryError "Use generic-lens or generic-optics with 'latestDeliveryError' instead." #-}

-- | Specifies the date and time that CloudTrail last delivered a digest file to an account's Amazon S3 bucket.
--
-- /Note:/ Consider using 'latestDigestDeliveryTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrsLatestDigestDeliveryTime :: Lens.Lens' GetTrailStatusResponse (Lude.Maybe Lude.Timestamp)
gtsrsLatestDigestDeliveryTime = Lens.lens (latestDigestDeliveryTime :: GetTrailStatusResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {latestDigestDeliveryTime = a} :: GetTrailStatusResponse)
{-# DEPRECATED gtsrsLatestDigestDeliveryTime "Use generic-lens or generic-optics with 'latestDigestDeliveryTime' instead." #-}

-- | This field is no longer in use.
--
-- /Note:/ Consider using 'latestNotificationAttemptSucceeded' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrsLatestNotificationAttemptSucceeded :: Lens.Lens' GetTrailStatusResponse (Lude.Maybe Lude.Text)
gtsrsLatestNotificationAttemptSucceeded = Lens.lens (latestNotificationAttemptSucceeded :: GetTrailStatusResponse -> Lude.Maybe Lude.Text) (\s a -> s {latestNotificationAttemptSucceeded = a} :: GetTrailStatusResponse)
{-# DEPRECATED gtsrsLatestNotificationAttemptSucceeded "Use generic-lens or generic-optics with 'latestNotificationAttemptSucceeded' instead." #-}

-- | Specifies the most recent date and time when CloudTrail started recording API calls for an AWS account.
--
-- /Note:/ Consider using 'startLoggingTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrsStartLoggingTime :: Lens.Lens' GetTrailStatusResponse (Lude.Maybe Lude.Timestamp)
gtsrsStartLoggingTime = Lens.lens (startLoggingTime :: GetTrailStatusResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {startLoggingTime = a} :: GetTrailStatusResponse)
{-# DEPRECATED gtsrsStartLoggingTime "Use generic-lens or generic-optics with 'startLoggingTime' instead." #-}

-- | Displays any Amazon SNS error that CloudTrail encountered when attempting to send a notification. For more information about Amazon SNS errors, see the <https://docs.aws.amazon.com/sns/latest/dg/welcome.html Amazon SNS Developer Guide> .
--
-- /Note:/ Consider using 'latestNotificationError' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrsLatestNotificationError :: Lens.Lens' GetTrailStatusResponse (Lude.Maybe Lude.Text)
gtsrsLatestNotificationError = Lens.lens (latestNotificationError :: GetTrailStatusResponse -> Lude.Maybe Lude.Text) (\s a -> s {latestNotificationError = a} :: GetTrailStatusResponse)
{-# DEPRECATED gtsrsLatestNotificationError "Use generic-lens or generic-optics with 'latestNotificationError' instead." #-}

-- | This field is no longer in use.
--
-- /Note:/ Consider using 'latestDeliveryAttemptSucceeded' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrsLatestDeliveryAttemptSucceeded :: Lens.Lens' GetTrailStatusResponse (Lude.Maybe Lude.Text)
gtsrsLatestDeliveryAttemptSucceeded = Lens.lens (latestDeliveryAttemptSucceeded :: GetTrailStatusResponse -> Lude.Maybe Lude.Text) (\s a -> s {latestDeliveryAttemptSucceeded = a} :: GetTrailStatusResponse)
{-# DEPRECATED gtsrsLatestDeliveryAttemptSucceeded "Use generic-lens or generic-optics with 'latestDeliveryAttemptSucceeded' instead." #-}

-- | Whether the CloudTrail is currently logging AWS API calls.
--
-- /Note:/ Consider using 'isLogging' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrsIsLogging :: Lens.Lens' GetTrailStatusResponse (Lude.Maybe Lude.Bool)
gtsrsIsLogging = Lens.lens (isLogging :: GetTrailStatusResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isLogging = a} :: GetTrailStatusResponse)
{-# DEPRECATED gtsrsIsLogging "Use generic-lens or generic-optics with 'isLogging' instead." #-}

-- | This field is no longer in use.
--
-- /Note:/ Consider using 'timeLoggingStarted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrsTimeLoggingStarted :: Lens.Lens' GetTrailStatusResponse (Lude.Maybe Lude.Text)
gtsrsTimeLoggingStarted = Lens.lens (timeLoggingStarted :: GetTrailStatusResponse -> Lude.Maybe Lude.Text) (\s a -> s {timeLoggingStarted = a} :: GetTrailStatusResponse)
{-# DEPRECATED gtsrsTimeLoggingStarted "Use generic-lens or generic-optics with 'timeLoggingStarted' instead." #-}

-- | Displays any Amazon S3 error that CloudTrail encountered when attempting to deliver a digest file to the designated bucket. For more information see the topic <https://docs.aws.amazon.com/AmazonS3/latest/API/ErrorResponses.html Error Responses> in the Amazon S3 API Reference.
--
-- /Note:/ Consider using 'latestDigestDeliveryError' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrsLatestDigestDeliveryError :: Lens.Lens' GetTrailStatusResponse (Lude.Maybe Lude.Text)
gtsrsLatestDigestDeliveryError = Lens.lens (latestDigestDeliveryError :: GetTrailStatusResponse -> Lude.Maybe Lude.Text) (\s a -> s {latestDigestDeliveryError = a} :: GetTrailStatusResponse)
{-# DEPRECATED gtsrsLatestDigestDeliveryError "Use generic-lens or generic-optics with 'latestDigestDeliveryError' instead." #-}

-- | This field is no longer in use.
--
-- /Note:/ Consider using 'latestDeliveryAttemptTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrsLatestDeliveryAttemptTime :: Lens.Lens' GetTrailStatusResponse (Lude.Maybe Lude.Text)
gtsrsLatestDeliveryAttemptTime = Lens.lens (latestDeliveryAttemptTime :: GetTrailStatusResponse -> Lude.Maybe Lude.Text) (\s a -> s {latestDeliveryAttemptTime = a} :: GetTrailStatusResponse)
{-# DEPRECATED gtsrsLatestDeliveryAttemptTime "Use generic-lens or generic-optics with 'latestDeliveryAttemptTime' instead." #-}

-- | Specifies the date and time that CloudTrail last delivered log files to an account's Amazon S3 bucket.
--
-- /Note:/ Consider using 'latestDeliveryTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrsLatestDeliveryTime :: Lens.Lens' GetTrailStatusResponse (Lude.Maybe Lude.Timestamp)
gtsrsLatestDeliveryTime = Lens.lens (latestDeliveryTime :: GetTrailStatusResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {latestDeliveryTime = a} :: GetTrailStatusResponse)
{-# DEPRECATED gtsrsLatestDeliveryTime "Use generic-lens or generic-optics with 'latestDeliveryTime' instead." #-}

-- | Displays the most recent date and time when CloudTrail delivered logs to CloudWatch Logs.
--
-- /Note:/ Consider using 'latestCloudWatchLogsDeliveryTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrsLatestCloudWatchLogsDeliveryTime :: Lens.Lens' GetTrailStatusResponse (Lude.Maybe Lude.Timestamp)
gtsrsLatestCloudWatchLogsDeliveryTime = Lens.lens (latestCloudWatchLogsDeliveryTime :: GetTrailStatusResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {latestCloudWatchLogsDeliveryTime = a} :: GetTrailStatusResponse)
{-# DEPRECATED gtsrsLatestCloudWatchLogsDeliveryTime "Use generic-lens or generic-optics with 'latestCloudWatchLogsDeliveryTime' instead." #-}

-- | Displays any CloudWatch Logs error that CloudTrail encountered when attempting to deliver logs to CloudWatch Logs.
--
-- /Note:/ Consider using 'latestCloudWatchLogsDeliveryError' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrsLatestCloudWatchLogsDeliveryError :: Lens.Lens' GetTrailStatusResponse (Lude.Maybe Lude.Text)
gtsrsLatestCloudWatchLogsDeliveryError = Lens.lens (latestCloudWatchLogsDeliveryError :: GetTrailStatusResponse -> Lude.Maybe Lude.Text) (\s a -> s {latestCloudWatchLogsDeliveryError = a} :: GetTrailStatusResponse)
{-# DEPRECATED gtsrsLatestCloudWatchLogsDeliveryError "Use generic-lens or generic-optics with 'latestCloudWatchLogsDeliveryError' instead." #-}

-- | Specifies the date and time of the most recent Amazon SNS notification that CloudTrail has written a new log file to an account's Amazon S3 bucket.
--
-- /Note:/ Consider using 'latestNotificationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrsLatestNotificationTime :: Lens.Lens' GetTrailStatusResponse (Lude.Maybe Lude.Timestamp)
gtsrsLatestNotificationTime = Lens.lens (latestNotificationTime :: GetTrailStatusResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {latestNotificationTime = a} :: GetTrailStatusResponse)
{-# DEPRECATED gtsrsLatestNotificationTime "Use generic-lens or generic-optics with 'latestNotificationTime' instead." #-}

-- | This field is no longer in use.
--
-- /Note:/ Consider using 'latestNotificationAttemptTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrsLatestNotificationAttemptTime :: Lens.Lens' GetTrailStatusResponse (Lude.Maybe Lude.Text)
gtsrsLatestNotificationAttemptTime = Lens.lens (latestNotificationAttemptTime :: GetTrailStatusResponse -> Lude.Maybe Lude.Text) (\s a -> s {latestNotificationAttemptTime = a} :: GetTrailStatusResponse)
{-# DEPRECATED gtsrsLatestNotificationAttemptTime "Use generic-lens or generic-optics with 'latestNotificationAttemptTime' instead." #-}

-- | Specifies the most recent date and time when CloudTrail stopped recording API calls for an AWS account.
--
-- /Note:/ Consider using 'stopLoggingTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrsStopLoggingTime :: Lens.Lens' GetTrailStatusResponse (Lude.Maybe Lude.Timestamp)
gtsrsStopLoggingTime = Lens.lens (stopLoggingTime :: GetTrailStatusResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {stopLoggingTime = a} :: GetTrailStatusResponse)
{-# DEPRECATED gtsrsStopLoggingTime "Use generic-lens or generic-optics with 'stopLoggingTime' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrsResponseStatus :: Lens.Lens' GetTrailStatusResponse Lude.Int
gtsrsResponseStatus = Lens.lens (responseStatus :: GetTrailStatusResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetTrailStatusResponse)
{-# DEPRECATED gtsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
