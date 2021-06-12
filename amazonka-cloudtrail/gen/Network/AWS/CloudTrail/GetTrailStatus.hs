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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
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
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  GetTrailStatus
newGetTrailStatus pName_ =
  GetTrailStatus' {name = pName_}

-- | Specifies the name or the CloudTrail ARN of the trail for which you are
-- requesting status. To get the status of a shadow trail (a replication of
-- the trail in another region), you must specify its ARN. The format of a
-- trail ARN is:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
getTrailStatus_name :: Lens.Lens' GetTrailStatus Core.Text
getTrailStatus_name = Lens.lens (\GetTrailStatus' {name} -> name) (\s@GetTrailStatus' {} a -> s {name = a} :: GetTrailStatus)

instance Core.AWSRequest GetTrailStatus where
  type
    AWSResponse GetTrailStatus =
      GetTrailStatusResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTrailStatusResponse'
            Core.<$> (x Core..?> "LatestDigestDeliveryError")
            Core.<*> (x Core..?> "TimeLoggingStopped")
            Core.<*> (x Core..?> "LatestDeliveryAttemptTime")
            Core.<*> (x Core..?> "LatestDeliveryAttemptSucceeded")
            Core.<*> (x Core..?> "LatestNotificationError")
            Core.<*> (x Core..?> "LatestCloudWatchLogsDeliveryError")
            Core.<*> (x Core..?> "LatestNotificationAttemptSucceeded")
            Core.<*> (x Core..?> "LatestCloudWatchLogsDeliveryTime")
            Core.<*> (x Core..?> "LatestDigestDeliveryTime")
            Core.<*> (x Core..?> "LatestDeliveryError")
            Core.<*> (x Core..?> "TimeLoggingStarted")
            Core.<*> (x Core..?> "IsLogging")
            Core.<*> (x Core..?> "StopLoggingTime")
            Core.<*> (x Core..?> "LatestNotificationAttemptTime")
            Core.<*> (x Core..?> "StartLoggingTime")
            Core.<*> (x Core..?> "LatestNotificationTime")
            Core.<*> (x Core..?> "LatestDeliveryTime")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetTrailStatus

instance Core.NFData GetTrailStatus

instance Core.ToHeaders GetTrailStatus where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.GetTrailStatus" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetTrailStatus where
  toJSON GetTrailStatus' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.ToPath GetTrailStatus where
  toPath = Core.const "/"

instance Core.ToQuery GetTrailStatus where
  toQuery = Core.const Core.mempty

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
    latestDigestDeliveryError :: Core.Maybe Core.Text,
    -- | This field is no longer in use.
    timeLoggingStopped :: Core.Maybe Core.Text,
    -- | This field is no longer in use.
    latestDeliveryAttemptTime :: Core.Maybe Core.Text,
    -- | This field is no longer in use.
    latestDeliveryAttemptSucceeded :: Core.Maybe Core.Text,
    -- | Displays any Amazon SNS error that CloudTrail encountered when
    -- attempting to send a notification. For more information about Amazon SNS
    -- errors, see the
    -- <https://docs.aws.amazon.com/sns/latest/dg/welcome.html Amazon SNS Developer Guide>.
    latestNotificationError :: Core.Maybe Core.Text,
    -- | Displays any CloudWatch Logs error that CloudTrail encountered when
    -- attempting to deliver logs to CloudWatch Logs.
    latestCloudWatchLogsDeliveryError :: Core.Maybe Core.Text,
    -- | This field is no longer in use.
    latestNotificationAttemptSucceeded :: Core.Maybe Core.Text,
    -- | Displays the most recent date and time when CloudTrail delivered logs to
    -- CloudWatch Logs.
    latestCloudWatchLogsDeliveryTime :: Core.Maybe Core.POSIX,
    -- | Specifies the date and time that CloudTrail last delivered a digest file
    -- to an account\'s Amazon S3 bucket.
    latestDigestDeliveryTime :: Core.Maybe Core.POSIX,
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
    latestDeliveryError :: Core.Maybe Core.Text,
    -- | This field is no longer in use.
    timeLoggingStarted :: Core.Maybe Core.Text,
    -- | Whether the CloudTrail is currently logging AWS API calls.
    isLogging :: Core.Maybe Core.Bool,
    -- | Specifies the most recent date and time when CloudTrail stopped
    -- recording API calls for an AWS account.
    stopLoggingTime :: Core.Maybe Core.POSIX,
    -- | This field is no longer in use.
    latestNotificationAttemptTime :: Core.Maybe Core.Text,
    -- | Specifies the most recent date and time when CloudTrail started
    -- recording API calls for an AWS account.
    startLoggingTime :: Core.Maybe Core.POSIX,
    -- | Specifies the date and time of the most recent Amazon SNS notification
    -- that CloudTrail has written a new log file to an account\'s Amazon S3
    -- bucket.
    latestNotificationTime :: Core.Maybe Core.POSIX,
    -- | Specifies the date and time that CloudTrail last delivered log files to
    -- an account\'s Amazon S3 bucket.
    latestDeliveryTime :: Core.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetTrailStatusResponse
newGetTrailStatusResponse pHttpStatus_ =
  GetTrailStatusResponse'
    { latestDigestDeliveryError =
        Core.Nothing,
      timeLoggingStopped = Core.Nothing,
      latestDeliveryAttemptTime = Core.Nothing,
      latestDeliveryAttemptSucceeded = Core.Nothing,
      latestNotificationError = Core.Nothing,
      latestCloudWatchLogsDeliveryError = Core.Nothing,
      latestNotificationAttemptSucceeded = Core.Nothing,
      latestCloudWatchLogsDeliveryTime = Core.Nothing,
      latestDigestDeliveryTime = Core.Nothing,
      latestDeliveryError = Core.Nothing,
      timeLoggingStarted = Core.Nothing,
      isLogging = Core.Nothing,
      stopLoggingTime = Core.Nothing,
      latestNotificationAttemptTime = Core.Nothing,
      startLoggingTime = Core.Nothing,
      latestNotificationTime = Core.Nothing,
      latestDeliveryTime = Core.Nothing,
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
getTrailStatusResponse_latestDigestDeliveryError :: Lens.Lens' GetTrailStatusResponse (Core.Maybe Core.Text)
getTrailStatusResponse_latestDigestDeliveryError = Lens.lens (\GetTrailStatusResponse' {latestDigestDeliveryError} -> latestDigestDeliveryError) (\s@GetTrailStatusResponse' {} a -> s {latestDigestDeliveryError = a} :: GetTrailStatusResponse)

-- | This field is no longer in use.
getTrailStatusResponse_timeLoggingStopped :: Lens.Lens' GetTrailStatusResponse (Core.Maybe Core.Text)
getTrailStatusResponse_timeLoggingStopped = Lens.lens (\GetTrailStatusResponse' {timeLoggingStopped} -> timeLoggingStopped) (\s@GetTrailStatusResponse' {} a -> s {timeLoggingStopped = a} :: GetTrailStatusResponse)

-- | This field is no longer in use.
getTrailStatusResponse_latestDeliveryAttemptTime :: Lens.Lens' GetTrailStatusResponse (Core.Maybe Core.Text)
getTrailStatusResponse_latestDeliveryAttemptTime = Lens.lens (\GetTrailStatusResponse' {latestDeliveryAttemptTime} -> latestDeliveryAttemptTime) (\s@GetTrailStatusResponse' {} a -> s {latestDeliveryAttemptTime = a} :: GetTrailStatusResponse)

-- | This field is no longer in use.
getTrailStatusResponse_latestDeliveryAttemptSucceeded :: Lens.Lens' GetTrailStatusResponse (Core.Maybe Core.Text)
getTrailStatusResponse_latestDeliveryAttemptSucceeded = Lens.lens (\GetTrailStatusResponse' {latestDeliveryAttemptSucceeded} -> latestDeliveryAttemptSucceeded) (\s@GetTrailStatusResponse' {} a -> s {latestDeliveryAttemptSucceeded = a} :: GetTrailStatusResponse)

-- | Displays any Amazon SNS error that CloudTrail encountered when
-- attempting to send a notification. For more information about Amazon SNS
-- errors, see the
-- <https://docs.aws.amazon.com/sns/latest/dg/welcome.html Amazon SNS Developer Guide>.
getTrailStatusResponse_latestNotificationError :: Lens.Lens' GetTrailStatusResponse (Core.Maybe Core.Text)
getTrailStatusResponse_latestNotificationError = Lens.lens (\GetTrailStatusResponse' {latestNotificationError} -> latestNotificationError) (\s@GetTrailStatusResponse' {} a -> s {latestNotificationError = a} :: GetTrailStatusResponse)

-- | Displays any CloudWatch Logs error that CloudTrail encountered when
-- attempting to deliver logs to CloudWatch Logs.
getTrailStatusResponse_latestCloudWatchLogsDeliveryError :: Lens.Lens' GetTrailStatusResponse (Core.Maybe Core.Text)
getTrailStatusResponse_latestCloudWatchLogsDeliveryError = Lens.lens (\GetTrailStatusResponse' {latestCloudWatchLogsDeliveryError} -> latestCloudWatchLogsDeliveryError) (\s@GetTrailStatusResponse' {} a -> s {latestCloudWatchLogsDeliveryError = a} :: GetTrailStatusResponse)

-- | This field is no longer in use.
getTrailStatusResponse_latestNotificationAttemptSucceeded :: Lens.Lens' GetTrailStatusResponse (Core.Maybe Core.Text)
getTrailStatusResponse_latestNotificationAttemptSucceeded = Lens.lens (\GetTrailStatusResponse' {latestNotificationAttemptSucceeded} -> latestNotificationAttemptSucceeded) (\s@GetTrailStatusResponse' {} a -> s {latestNotificationAttemptSucceeded = a} :: GetTrailStatusResponse)

-- | Displays the most recent date and time when CloudTrail delivered logs to
-- CloudWatch Logs.
getTrailStatusResponse_latestCloudWatchLogsDeliveryTime :: Lens.Lens' GetTrailStatusResponse (Core.Maybe Core.UTCTime)
getTrailStatusResponse_latestCloudWatchLogsDeliveryTime = Lens.lens (\GetTrailStatusResponse' {latestCloudWatchLogsDeliveryTime} -> latestCloudWatchLogsDeliveryTime) (\s@GetTrailStatusResponse' {} a -> s {latestCloudWatchLogsDeliveryTime = a} :: GetTrailStatusResponse) Core.. Lens.mapping Core._Time

-- | Specifies the date and time that CloudTrail last delivered a digest file
-- to an account\'s Amazon S3 bucket.
getTrailStatusResponse_latestDigestDeliveryTime :: Lens.Lens' GetTrailStatusResponse (Core.Maybe Core.UTCTime)
getTrailStatusResponse_latestDigestDeliveryTime = Lens.lens (\GetTrailStatusResponse' {latestDigestDeliveryTime} -> latestDigestDeliveryTime) (\s@GetTrailStatusResponse' {} a -> s {latestDigestDeliveryTime = a} :: GetTrailStatusResponse) Core.. Lens.mapping Core._Time

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
getTrailStatusResponse_latestDeliveryError :: Lens.Lens' GetTrailStatusResponse (Core.Maybe Core.Text)
getTrailStatusResponse_latestDeliveryError = Lens.lens (\GetTrailStatusResponse' {latestDeliveryError} -> latestDeliveryError) (\s@GetTrailStatusResponse' {} a -> s {latestDeliveryError = a} :: GetTrailStatusResponse)

-- | This field is no longer in use.
getTrailStatusResponse_timeLoggingStarted :: Lens.Lens' GetTrailStatusResponse (Core.Maybe Core.Text)
getTrailStatusResponse_timeLoggingStarted = Lens.lens (\GetTrailStatusResponse' {timeLoggingStarted} -> timeLoggingStarted) (\s@GetTrailStatusResponse' {} a -> s {timeLoggingStarted = a} :: GetTrailStatusResponse)

-- | Whether the CloudTrail is currently logging AWS API calls.
getTrailStatusResponse_isLogging :: Lens.Lens' GetTrailStatusResponse (Core.Maybe Core.Bool)
getTrailStatusResponse_isLogging = Lens.lens (\GetTrailStatusResponse' {isLogging} -> isLogging) (\s@GetTrailStatusResponse' {} a -> s {isLogging = a} :: GetTrailStatusResponse)

-- | Specifies the most recent date and time when CloudTrail stopped
-- recording API calls for an AWS account.
getTrailStatusResponse_stopLoggingTime :: Lens.Lens' GetTrailStatusResponse (Core.Maybe Core.UTCTime)
getTrailStatusResponse_stopLoggingTime = Lens.lens (\GetTrailStatusResponse' {stopLoggingTime} -> stopLoggingTime) (\s@GetTrailStatusResponse' {} a -> s {stopLoggingTime = a} :: GetTrailStatusResponse) Core.. Lens.mapping Core._Time

-- | This field is no longer in use.
getTrailStatusResponse_latestNotificationAttemptTime :: Lens.Lens' GetTrailStatusResponse (Core.Maybe Core.Text)
getTrailStatusResponse_latestNotificationAttemptTime = Lens.lens (\GetTrailStatusResponse' {latestNotificationAttemptTime} -> latestNotificationAttemptTime) (\s@GetTrailStatusResponse' {} a -> s {latestNotificationAttemptTime = a} :: GetTrailStatusResponse)

-- | Specifies the most recent date and time when CloudTrail started
-- recording API calls for an AWS account.
getTrailStatusResponse_startLoggingTime :: Lens.Lens' GetTrailStatusResponse (Core.Maybe Core.UTCTime)
getTrailStatusResponse_startLoggingTime = Lens.lens (\GetTrailStatusResponse' {startLoggingTime} -> startLoggingTime) (\s@GetTrailStatusResponse' {} a -> s {startLoggingTime = a} :: GetTrailStatusResponse) Core.. Lens.mapping Core._Time

-- | Specifies the date and time of the most recent Amazon SNS notification
-- that CloudTrail has written a new log file to an account\'s Amazon S3
-- bucket.
getTrailStatusResponse_latestNotificationTime :: Lens.Lens' GetTrailStatusResponse (Core.Maybe Core.UTCTime)
getTrailStatusResponse_latestNotificationTime = Lens.lens (\GetTrailStatusResponse' {latestNotificationTime} -> latestNotificationTime) (\s@GetTrailStatusResponse' {} a -> s {latestNotificationTime = a} :: GetTrailStatusResponse) Core.. Lens.mapping Core._Time

-- | Specifies the date and time that CloudTrail last delivered log files to
-- an account\'s Amazon S3 bucket.
getTrailStatusResponse_latestDeliveryTime :: Lens.Lens' GetTrailStatusResponse (Core.Maybe Core.UTCTime)
getTrailStatusResponse_latestDeliveryTime = Lens.lens (\GetTrailStatusResponse' {latestDeliveryTime} -> latestDeliveryTime) (\s@GetTrailStatusResponse' {} a -> s {latestDeliveryTime = a} :: GetTrailStatusResponse) Core.. Lens.mapping Core._Time

-- | The response's http status code.
getTrailStatusResponse_httpStatus :: Lens.Lens' GetTrailStatusResponse Core.Int
getTrailStatusResponse_httpStatus = Lens.lens (\GetTrailStatusResponse' {httpStatus} -> httpStatus) (\s@GetTrailStatusResponse' {} a -> s {httpStatus = a} :: GetTrailStatusResponse)

instance Core.NFData GetTrailStatusResponse
