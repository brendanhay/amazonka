{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.GetTrailStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a JSON-formatted list of information about the specified trail.
-- Fields include information on delivery errors, Amazon SNS and Amazon S3
-- errors, and start and stop logging times for each trail. This operation
-- returns trail status from a single region. To return trail status from
-- all regions, you must call the operation on each region.
module Network.AWS.CloudTrail.GetTrailStatus
  ( -- * Creating a Request
    GetTrailStatus (..),
    newGetTrailStatus,

    -- * Request Lenses
    getTrailStatus_name,

    -- * Destructuring the Response
    GetTrailStatusResponse (..),
    newGetTrailStatusResponse,

    -- * Response Lenses
    getTrailStatusResponse_latestDigestDeliveryError,
    getTrailStatusResponse_timeLoggingStopped,
    getTrailStatusResponse_latestDeliveryAttemptTime,
    getTrailStatusResponse_latestDeliveryAttemptSucceeded,
    getTrailStatusResponse_latestNotificationError,
    getTrailStatusResponse_latestCloudWatchLogsDeliveryError,
    getTrailStatusResponse_latestNotificationAttemptSucceeded,
    getTrailStatusResponse_latestCloudWatchLogsDeliveryTime,
    getTrailStatusResponse_latestDigestDeliveryTime,
    getTrailStatusResponse_latestDeliveryError,
    getTrailStatusResponse_timeLoggingStarted,
    getTrailStatusResponse_isLogging,
    getTrailStatusResponse_stopLoggingTime,
    getTrailStatusResponse_latestNotificationAttemptTime,
    getTrailStatusResponse_startLoggingTime,
    getTrailStatusResponse_latestNotificationTime,
    getTrailStatusResponse_latestDeliveryTime,
    getTrailStatusResponse_httpStatus,
  )
where

import Network.AWS.CloudTrail.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The name of a trail about which you want the current status.
--
-- /See:/ 'newGetTrailStatus' smart constructor.
data GetTrailStatus = GetTrailStatus'
  { -- | Specifies the name or the CloudTrail ARN of the trail for which you are
    -- requesting status. To get the status of a shadow trail (a replication of
    -- the trail in another region), you must specify its ARN. The format of a
    -- trail ARN is:
    --
    -- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetTrailStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getTrailStatus_name' - Specifies the name or the CloudTrail ARN of the trail for which you are
-- requesting status. To get the status of a shadow trail (a replication of
-- the trail in another region), you must specify its ARN. The format of a
-- trail ARN is:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
newGetTrailStatus ::
  -- | 'name'
  Prelude.Text ->
  GetTrailStatus
newGetTrailStatus pName_ =
  GetTrailStatus' {name = pName_}

-- | Specifies the name or the CloudTrail ARN of the trail for which you are
-- requesting status. To get the status of a shadow trail (a replication of
-- the trail in another region), you must specify its ARN. The format of a
-- trail ARN is:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
getTrailStatus_name :: Lens.Lens' GetTrailStatus Prelude.Text
getTrailStatus_name = Lens.lens (\GetTrailStatus' {name} -> name) (\s@GetTrailStatus' {} a -> s {name = a} :: GetTrailStatus)

instance Prelude.AWSRequest GetTrailStatus where
  type Rs GetTrailStatus = GetTrailStatusResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTrailStatusResponse'
            Prelude.<$> (x Prelude..?> "LatestDigestDeliveryError")
            Prelude.<*> (x Prelude..?> "TimeLoggingStopped")
            Prelude.<*> (x Prelude..?> "LatestDeliveryAttemptTime")
            Prelude.<*> (x Prelude..?> "LatestDeliveryAttemptSucceeded")
            Prelude.<*> (x Prelude..?> "LatestNotificationError")
            Prelude.<*> (x Prelude..?> "LatestCloudWatchLogsDeliveryError")
            Prelude.<*> (x Prelude..?> "LatestNotificationAttemptSucceeded")
            Prelude.<*> (x Prelude..?> "LatestCloudWatchLogsDeliveryTime")
            Prelude.<*> (x Prelude..?> "LatestDigestDeliveryTime")
            Prelude.<*> (x Prelude..?> "LatestDeliveryError")
            Prelude.<*> (x Prelude..?> "TimeLoggingStarted")
            Prelude.<*> (x Prelude..?> "IsLogging")
            Prelude.<*> (x Prelude..?> "StopLoggingTime")
            Prelude.<*> (x Prelude..?> "LatestNotificationAttemptTime")
            Prelude.<*> (x Prelude..?> "StartLoggingTime")
            Prelude.<*> (x Prelude..?> "LatestNotificationTime")
            Prelude.<*> (x Prelude..?> "LatestDeliveryTime")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetTrailStatus

instance Prelude.NFData GetTrailStatus

instance Prelude.ToHeaders GetTrailStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.GetTrailStatus" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetTrailStatus where
  toJSON GetTrailStatus' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Prelude..= name)]
      )

instance Prelude.ToPath GetTrailStatus where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetTrailStatus where
  toQuery = Prelude.const Prelude.mempty

-- | Returns the objects or data listed below if successful. Otherwise,
-- returns an error.
--
-- /See:/ 'newGetTrailStatusResponse' smart constructor.
data GetTrailStatusResponse = GetTrailStatusResponse'
  { -- | Displays any Amazon S3 error that CloudTrail encountered when attempting
    -- to deliver a digest file to the designated bucket. For more information
    -- see the topic
    -- <https://docs.aws.amazon.com/AmazonS3/latest/API/ErrorResponses.html Error Responses>
    -- in the Amazon S3 API Reference.
    --
    -- This error occurs only when there is a problem with the destination S3
    -- bucket and will not occur for timeouts. To resolve the issue, create a
    -- new bucket and call @UpdateTrail@ to specify the new bucket, or fix the
    -- existing objects so that CloudTrail can again write to the bucket.
    latestDigestDeliveryError :: Prelude.Maybe Prelude.Text,
    -- | This field is no longer in use.
    timeLoggingStopped :: Prelude.Maybe Prelude.Text,
    -- | This field is no longer in use.
    latestDeliveryAttemptTime :: Prelude.Maybe Prelude.Text,
    -- | This field is no longer in use.
    latestDeliveryAttemptSucceeded :: Prelude.Maybe Prelude.Text,
    -- | Displays any Amazon SNS error that CloudTrail encountered when
    -- attempting to send a notification. For more information about Amazon SNS
    -- errors, see the
    -- <https://docs.aws.amazon.com/sns/latest/dg/welcome.html Amazon SNS Developer Guide>.
    latestNotificationError :: Prelude.Maybe Prelude.Text,
    -- | Displays any CloudWatch Logs error that CloudTrail encountered when
    -- attempting to deliver logs to CloudWatch Logs.
    latestCloudWatchLogsDeliveryError :: Prelude.Maybe Prelude.Text,
    -- | This field is no longer in use.
    latestNotificationAttemptSucceeded :: Prelude.Maybe Prelude.Text,
    -- | Displays the most recent date and time when CloudTrail delivered logs to
    -- CloudWatch Logs.
    latestCloudWatchLogsDeliveryTime :: Prelude.Maybe Prelude.POSIX,
    -- | Specifies the date and time that CloudTrail last delivered a digest file
    -- to an account\'s Amazon S3 bucket.
    latestDigestDeliveryTime :: Prelude.Maybe Prelude.POSIX,
    -- | Displays any Amazon S3 error that CloudTrail encountered when attempting
    -- to deliver log files to the designated bucket. For more information see
    -- the topic
    -- <https://docs.aws.amazon.com/AmazonS3/latest/API/ErrorResponses.html Error Responses>
    -- in the Amazon S3 API Reference.
    --
    -- This error occurs only when there is a problem with the destination S3
    -- bucket and will not occur for timeouts. To resolve the issue, create a
    -- new bucket and call @UpdateTrail@ to specify the new bucket, or fix the
    -- existing objects so that CloudTrail can again write to the bucket.
    latestDeliveryError :: Prelude.Maybe Prelude.Text,
    -- | This field is no longer in use.
    timeLoggingStarted :: Prelude.Maybe Prelude.Text,
    -- | Whether the CloudTrail is currently logging AWS API calls.
    isLogging :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the most recent date and time when CloudTrail stopped
    -- recording API calls for an AWS account.
    stopLoggingTime :: Prelude.Maybe Prelude.POSIX,
    -- | This field is no longer in use.
    latestNotificationAttemptTime :: Prelude.Maybe Prelude.Text,
    -- | Specifies the most recent date and time when CloudTrail started
    -- recording API calls for an AWS account.
    startLoggingTime :: Prelude.Maybe Prelude.POSIX,
    -- | Specifies the date and time of the most recent Amazon SNS notification
    -- that CloudTrail has written a new log file to an account\'s Amazon S3
    -- bucket.
    latestNotificationTime :: Prelude.Maybe Prelude.POSIX,
    -- | Specifies the date and time that CloudTrail last delivered log files to
    -- an account\'s Amazon S3 bucket.
    latestDeliveryTime :: Prelude.Maybe Prelude.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetTrailStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'latestDigestDeliveryError', 'getTrailStatusResponse_latestDigestDeliveryError' - Displays any Amazon S3 error that CloudTrail encountered when attempting
-- to deliver a digest file to the designated bucket. For more information
-- see the topic
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/ErrorResponses.html Error Responses>
-- in the Amazon S3 API Reference.
--
-- This error occurs only when there is a problem with the destination S3
-- bucket and will not occur for timeouts. To resolve the issue, create a
-- new bucket and call @UpdateTrail@ to specify the new bucket, or fix the
-- existing objects so that CloudTrail can again write to the bucket.
--
-- 'timeLoggingStopped', 'getTrailStatusResponse_timeLoggingStopped' - This field is no longer in use.
--
-- 'latestDeliveryAttemptTime', 'getTrailStatusResponse_latestDeliveryAttemptTime' - This field is no longer in use.
--
-- 'latestDeliveryAttemptSucceeded', 'getTrailStatusResponse_latestDeliveryAttemptSucceeded' - This field is no longer in use.
--
-- 'latestNotificationError', 'getTrailStatusResponse_latestNotificationError' - Displays any Amazon SNS error that CloudTrail encountered when
-- attempting to send a notification. For more information about Amazon SNS
-- errors, see the
-- <https://docs.aws.amazon.com/sns/latest/dg/welcome.html Amazon SNS Developer Guide>.
--
-- 'latestCloudWatchLogsDeliveryError', 'getTrailStatusResponse_latestCloudWatchLogsDeliveryError' - Displays any CloudWatch Logs error that CloudTrail encountered when
-- attempting to deliver logs to CloudWatch Logs.
--
-- 'latestNotificationAttemptSucceeded', 'getTrailStatusResponse_latestNotificationAttemptSucceeded' - This field is no longer in use.
--
-- 'latestCloudWatchLogsDeliveryTime', 'getTrailStatusResponse_latestCloudWatchLogsDeliveryTime' - Displays the most recent date and time when CloudTrail delivered logs to
-- CloudWatch Logs.
--
-- 'latestDigestDeliveryTime', 'getTrailStatusResponse_latestDigestDeliveryTime' - Specifies the date and time that CloudTrail last delivered a digest file
-- to an account\'s Amazon S3 bucket.
--
-- 'latestDeliveryError', 'getTrailStatusResponse_latestDeliveryError' - Displays any Amazon S3 error that CloudTrail encountered when attempting
-- to deliver log files to the designated bucket. For more information see
-- the topic
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/ErrorResponses.html Error Responses>
-- in the Amazon S3 API Reference.
--
-- This error occurs only when there is a problem with the destination S3
-- bucket and will not occur for timeouts. To resolve the issue, create a
-- new bucket and call @UpdateTrail@ to specify the new bucket, or fix the
-- existing objects so that CloudTrail can again write to the bucket.
--
-- 'timeLoggingStarted', 'getTrailStatusResponse_timeLoggingStarted' - This field is no longer in use.
--
-- 'isLogging', 'getTrailStatusResponse_isLogging' - Whether the CloudTrail is currently logging AWS API calls.
--
-- 'stopLoggingTime', 'getTrailStatusResponse_stopLoggingTime' - Specifies the most recent date and time when CloudTrail stopped
-- recording API calls for an AWS account.
--
-- 'latestNotificationAttemptTime', 'getTrailStatusResponse_latestNotificationAttemptTime' - This field is no longer in use.
--
-- 'startLoggingTime', 'getTrailStatusResponse_startLoggingTime' - Specifies the most recent date and time when CloudTrail started
-- recording API calls for an AWS account.
--
-- 'latestNotificationTime', 'getTrailStatusResponse_latestNotificationTime' - Specifies the date and time of the most recent Amazon SNS notification
-- that CloudTrail has written a new log file to an account\'s Amazon S3
-- bucket.
--
-- 'latestDeliveryTime', 'getTrailStatusResponse_latestDeliveryTime' - Specifies the date and time that CloudTrail last delivered log files to
-- an account\'s Amazon S3 bucket.
--
-- 'httpStatus', 'getTrailStatusResponse_httpStatus' - The response's http status code.
newGetTrailStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetTrailStatusResponse
newGetTrailStatusResponse pHttpStatus_ =
  GetTrailStatusResponse'
    { latestDigestDeliveryError =
        Prelude.Nothing,
      timeLoggingStopped = Prelude.Nothing,
      latestDeliveryAttemptTime = Prelude.Nothing,
      latestDeliveryAttemptSucceeded = Prelude.Nothing,
      latestNotificationError = Prelude.Nothing,
      latestCloudWatchLogsDeliveryError = Prelude.Nothing,
      latestNotificationAttemptSucceeded =
        Prelude.Nothing,
      latestCloudWatchLogsDeliveryTime = Prelude.Nothing,
      latestDigestDeliveryTime = Prelude.Nothing,
      latestDeliveryError = Prelude.Nothing,
      timeLoggingStarted = Prelude.Nothing,
      isLogging = Prelude.Nothing,
      stopLoggingTime = Prelude.Nothing,
      latestNotificationAttemptTime = Prelude.Nothing,
      startLoggingTime = Prelude.Nothing,
      latestNotificationTime = Prelude.Nothing,
      latestDeliveryTime = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Displays any Amazon S3 error that CloudTrail encountered when attempting
-- to deliver a digest file to the designated bucket. For more information
-- see the topic
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/ErrorResponses.html Error Responses>
-- in the Amazon S3 API Reference.
--
-- This error occurs only when there is a problem with the destination S3
-- bucket and will not occur for timeouts. To resolve the issue, create a
-- new bucket and call @UpdateTrail@ to specify the new bucket, or fix the
-- existing objects so that CloudTrail can again write to the bucket.
getTrailStatusResponse_latestDigestDeliveryError :: Lens.Lens' GetTrailStatusResponse (Prelude.Maybe Prelude.Text)
getTrailStatusResponse_latestDigestDeliveryError = Lens.lens (\GetTrailStatusResponse' {latestDigestDeliveryError} -> latestDigestDeliveryError) (\s@GetTrailStatusResponse' {} a -> s {latestDigestDeliveryError = a} :: GetTrailStatusResponse)

-- | This field is no longer in use.
getTrailStatusResponse_timeLoggingStopped :: Lens.Lens' GetTrailStatusResponse (Prelude.Maybe Prelude.Text)
getTrailStatusResponse_timeLoggingStopped = Lens.lens (\GetTrailStatusResponse' {timeLoggingStopped} -> timeLoggingStopped) (\s@GetTrailStatusResponse' {} a -> s {timeLoggingStopped = a} :: GetTrailStatusResponse)

-- | This field is no longer in use.
getTrailStatusResponse_latestDeliveryAttemptTime :: Lens.Lens' GetTrailStatusResponse (Prelude.Maybe Prelude.Text)
getTrailStatusResponse_latestDeliveryAttemptTime = Lens.lens (\GetTrailStatusResponse' {latestDeliveryAttemptTime} -> latestDeliveryAttemptTime) (\s@GetTrailStatusResponse' {} a -> s {latestDeliveryAttemptTime = a} :: GetTrailStatusResponse)

-- | This field is no longer in use.
getTrailStatusResponse_latestDeliveryAttemptSucceeded :: Lens.Lens' GetTrailStatusResponse (Prelude.Maybe Prelude.Text)
getTrailStatusResponse_latestDeliveryAttemptSucceeded = Lens.lens (\GetTrailStatusResponse' {latestDeliveryAttemptSucceeded} -> latestDeliveryAttemptSucceeded) (\s@GetTrailStatusResponse' {} a -> s {latestDeliveryAttemptSucceeded = a} :: GetTrailStatusResponse)

-- | Displays any Amazon SNS error that CloudTrail encountered when
-- attempting to send a notification. For more information about Amazon SNS
-- errors, see the
-- <https://docs.aws.amazon.com/sns/latest/dg/welcome.html Amazon SNS Developer Guide>.
getTrailStatusResponse_latestNotificationError :: Lens.Lens' GetTrailStatusResponse (Prelude.Maybe Prelude.Text)
getTrailStatusResponse_latestNotificationError = Lens.lens (\GetTrailStatusResponse' {latestNotificationError} -> latestNotificationError) (\s@GetTrailStatusResponse' {} a -> s {latestNotificationError = a} :: GetTrailStatusResponse)

-- | Displays any CloudWatch Logs error that CloudTrail encountered when
-- attempting to deliver logs to CloudWatch Logs.
getTrailStatusResponse_latestCloudWatchLogsDeliveryError :: Lens.Lens' GetTrailStatusResponse (Prelude.Maybe Prelude.Text)
getTrailStatusResponse_latestCloudWatchLogsDeliveryError = Lens.lens (\GetTrailStatusResponse' {latestCloudWatchLogsDeliveryError} -> latestCloudWatchLogsDeliveryError) (\s@GetTrailStatusResponse' {} a -> s {latestCloudWatchLogsDeliveryError = a} :: GetTrailStatusResponse)

-- | This field is no longer in use.
getTrailStatusResponse_latestNotificationAttemptSucceeded :: Lens.Lens' GetTrailStatusResponse (Prelude.Maybe Prelude.Text)
getTrailStatusResponse_latestNotificationAttemptSucceeded = Lens.lens (\GetTrailStatusResponse' {latestNotificationAttemptSucceeded} -> latestNotificationAttemptSucceeded) (\s@GetTrailStatusResponse' {} a -> s {latestNotificationAttemptSucceeded = a} :: GetTrailStatusResponse)

-- | Displays the most recent date and time when CloudTrail delivered logs to
-- CloudWatch Logs.
getTrailStatusResponse_latestCloudWatchLogsDeliveryTime :: Lens.Lens' GetTrailStatusResponse (Prelude.Maybe Prelude.UTCTime)
getTrailStatusResponse_latestCloudWatchLogsDeliveryTime = Lens.lens (\GetTrailStatusResponse' {latestCloudWatchLogsDeliveryTime} -> latestCloudWatchLogsDeliveryTime) (\s@GetTrailStatusResponse' {} a -> s {latestCloudWatchLogsDeliveryTime = a} :: GetTrailStatusResponse) Prelude.. Lens.mapping Prelude._Time

-- | Specifies the date and time that CloudTrail last delivered a digest file
-- to an account\'s Amazon S3 bucket.
getTrailStatusResponse_latestDigestDeliveryTime :: Lens.Lens' GetTrailStatusResponse (Prelude.Maybe Prelude.UTCTime)
getTrailStatusResponse_latestDigestDeliveryTime = Lens.lens (\GetTrailStatusResponse' {latestDigestDeliveryTime} -> latestDigestDeliveryTime) (\s@GetTrailStatusResponse' {} a -> s {latestDigestDeliveryTime = a} :: GetTrailStatusResponse) Prelude.. Lens.mapping Prelude._Time

-- | Displays any Amazon S3 error that CloudTrail encountered when attempting
-- to deliver log files to the designated bucket. For more information see
-- the topic
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/ErrorResponses.html Error Responses>
-- in the Amazon S3 API Reference.
--
-- This error occurs only when there is a problem with the destination S3
-- bucket and will not occur for timeouts. To resolve the issue, create a
-- new bucket and call @UpdateTrail@ to specify the new bucket, or fix the
-- existing objects so that CloudTrail can again write to the bucket.
getTrailStatusResponse_latestDeliveryError :: Lens.Lens' GetTrailStatusResponse (Prelude.Maybe Prelude.Text)
getTrailStatusResponse_latestDeliveryError = Lens.lens (\GetTrailStatusResponse' {latestDeliveryError} -> latestDeliveryError) (\s@GetTrailStatusResponse' {} a -> s {latestDeliveryError = a} :: GetTrailStatusResponse)

-- | This field is no longer in use.
getTrailStatusResponse_timeLoggingStarted :: Lens.Lens' GetTrailStatusResponse (Prelude.Maybe Prelude.Text)
getTrailStatusResponse_timeLoggingStarted = Lens.lens (\GetTrailStatusResponse' {timeLoggingStarted} -> timeLoggingStarted) (\s@GetTrailStatusResponse' {} a -> s {timeLoggingStarted = a} :: GetTrailStatusResponse)

-- | Whether the CloudTrail is currently logging AWS API calls.
getTrailStatusResponse_isLogging :: Lens.Lens' GetTrailStatusResponse (Prelude.Maybe Prelude.Bool)
getTrailStatusResponse_isLogging = Lens.lens (\GetTrailStatusResponse' {isLogging} -> isLogging) (\s@GetTrailStatusResponse' {} a -> s {isLogging = a} :: GetTrailStatusResponse)

-- | Specifies the most recent date and time when CloudTrail stopped
-- recording API calls for an AWS account.
getTrailStatusResponse_stopLoggingTime :: Lens.Lens' GetTrailStatusResponse (Prelude.Maybe Prelude.UTCTime)
getTrailStatusResponse_stopLoggingTime = Lens.lens (\GetTrailStatusResponse' {stopLoggingTime} -> stopLoggingTime) (\s@GetTrailStatusResponse' {} a -> s {stopLoggingTime = a} :: GetTrailStatusResponse) Prelude.. Lens.mapping Prelude._Time

-- | This field is no longer in use.
getTrailStatusResponse_latestNotificationAttemptTime :: Lens.Lens' GetTrailStatusResponse (Prelude.Maybe Prelude.Text)
getTrailStatusResponse_latestNotificationAttemptTime = Lens.lens (\GetTrailStatusResponse' {latestNotificationAttemptTime} -> latestNotificationAttemptTime) (\s@GetTrailStatusResponse' {} a -> s {latestNotificationAttemptTime = a} :: GetTrailStatusResponse)

-- | Specifies the most recent date and time when CloudTrail started
-- recording API calls for an AWS account.
getTrailStatusResponse_startLoggingTime :: Lens.Lens' GetTrailStatusResponse (Prelude.Maybe Prelude.UTCTime)
getTrailStatusResponse_startLoggingTime = Lens.lens (\GetTrailStatusResponse' {startLoggingTime} -> startLoggingTime) (\s@GetTrailStatusResponse' {} a -> s {startLoggingTime = a} :: GetTrailStatusResponse) Prelude.. Lens.mapping Prelude._Time

-- | Specifies the date and time of the most recent Amazon SNS notification
-- that CloudTrail has written a new log file to an account\'s Amazon S3
-- bucket.
getTrailStatusResponse_latestNotificationTime :: Lens.Lens' GetTrailStatusResponse (Prelude.Maybe Prelude.UTCTime)
getTrailStatusResponse_latestNotificationTime = Lens.lens (\GetTrailStatusResponse' {latestNotificationTime} -> latestNotificationTime) (\s@GetTrailStatusResponse' {} a -> s {latestNotificationTime = a} :: GetTrailStatusResponse) Prelude.. Lens.mapping Prelude._Time

-- | Specifies the date and time that CloudTrail last delivered log files to
-- an account\'s Amazon S3 bucket.
getTrailStatusResponse_latestDeliveryTime :: Lens.Lens' GetTrailStatusResponse (Prelude.Maybe Prelude.UTCTime)
getTrailStatusResponse_latestDeliveryTime = Lens.lens (\GetTrailStatusResponse' {latestDeliveryTime} -> latestDeliveryTime) (\s@GetTrailStatusResponse' {} a -> s {latestDeliveryTime = a} :: GetTrailStatusResponse) Prelude.. Lens.mapping Prelude._Time

-- | The response's http status code.
getTrailStatusResponse_httpStatus :: Lens.Lens' GetTrailStatusResponse Prelude.Int
getTrailStatusResponse_httpStatus = Lens.lens (\GetTrailStatusResponse' {httpStatus} -> httpStatus) (\s@GetTrailStatusResponse' {} a -> s {httpStatus = a} :: GetTrailStatusResponse)

instance Prelude.NFData GetTrailStatusResponse
