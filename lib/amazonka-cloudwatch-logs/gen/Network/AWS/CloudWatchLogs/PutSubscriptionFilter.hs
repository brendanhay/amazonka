{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.PutSubscriptionFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a subscription filter and associates it with the specified log group. Subscription filters allow you to subscribe to a real-time stream of log events ingested through <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_PutLogEvents.html PutLogEvents> and have them delivered to a specific destination. When log events are sent to the receiving service, they are Base64 encoded and compressed with the gzip format.
--
-- The following destinations are supported for subscription filters:
--
--     * An Amazon Kinesis stream belonging to the same account as the subscription filter, for same-account delivery.
--
--
--     * A logical destination that belongs to a different account, for cross-account delivery.
--
--
--     * An Amazon Kinesis Firehose delivery stream that belongs to the same account as the subscription filter, for same-account delivery.
--
--
--     * An AWS Lambda function that belongs to the same account as the subscription filter, for same-account delivery.
--
--
-- There can only be one subscription filter associated with a log group. If you are updating an existing filter, you must specify the correct name in @filterName@ . Otherwise, the call fails because you cannot associate a second filter with a log group.
-- To perform a @PutSubscriptionFilter@ operation, you must also have the @iam:PassRole@ permission.
module Network.AWS.CloudWatchLogs.PutSubscriptionFilter
  ( -- * Creating a request
    PutSubscriptionFilter (..),
    mkPutSubscriptionFilter,

    -- ** Request lenses
    psfDistribution,
    psfRoleARN,
    psfLogGroupName,
    psfFilterName,
    psfFilterPattern,
    psfDestinationARN,

    -- * Destructuring the response
    PutSubscriptionFilterResponse (..),
    mkPutSubscriptionFilterResponse,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutSubscriptionFilter' smart constructor.
data PutSubscriptionFilter = PutSubscriptionFilter'
  { distribution ::
      Lude.Maybe Distribution,
    roleARN :: Lude.Maybe Lude.Text,
    logGroupName :: Lude.Text,
    filterName :: Lude.Text,
    filterPattern :: Lude.Text,
    destinationARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutSubscriptionFilter' with the minimum fields required to make a request.
--
-- * 'destinationARN' - The ARN of the destination to deliver matching log events to. Currently, the supported destinations are:
--
--
--     * An Amazon Kinesis stream belonging to the same account as the subscription filter, for same-account delivery.
--
--
--     * A logical destination (specified using an ARN) belonging to a different account, for cross-account delivery.
--
--
--     * An Amazon Kinesis Firehose delivery stream belonging to the same account as the subscription filter, for same-account delivery.
--
--
--     * An AWS Lambda function belonging to the same account as the subscription filter, for same-account delivery.
--
--
-- * 'distribution' - The method used to distribute log data to the destination. By default, log data is grouped by log stream, but the grouping can be set to random for a more even distribution. This property is only applicable when the destination is an Amazon Kinesis stream.
-- * 'filterName' - A name for the subscription filter. If you are updating an existing filter, you must specify the correct name in @filterName@ . Otherwise, the call fails because you cannot associate a second filter with a log group. To find the name of the filter currently associated with a log group, use <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeSubscriptionFilters.html DescribeSubscriptionFilters> .
-- * 'filterPattern' - A filter pattern for subscribing to a filtered stream of log events.
-- * 'logGroupName' - The name of the log group.
-- * 'roleARN' - The ARN of an IAM role that grants CloudWatch Logs permissions to deliver ingested log events to the destination stream. You don't need to provide the ARN when you are working with a logical destination for cross-account delivery.
mkPutSubscriptionFilter ::
  -- | 'logGroupName'
  Lude.Text ->
  -- | 'filterName'
  Lude.Text ->
  -- | 'filterPattern'
  Lude.Text ->
  -- | 'destinationARN'
  Lude.Text ->
  PutSubscriptionFilter
mkPutSubscriptionFilter
  pLogGroupName_
  pFilterName_
  pFilterPattern_
  pDestinationARN_ =
    PutSubscriptionFilter'
      { distribution = Lude.Nothing,
        roleARN = Lude.Nothing,
        logGroupName = pLogGroupName_,
        filterName = pFilterName_,
        filterPattern = pFilterPattern_,
        destinationARN = pDestinationARN_
      }

-- | The method used to distribute log data to the destination. By default, log data is grouped by log stream, but the grouping can be set to random for a more even distribution. This property is only applicable when the destination is an Amazon Kinesis stream.
--
-- /Note:/ Consider using 'distribution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psfDistribution :: Lens.Lens' PutSubscriptionFilter (Lude.Maybe Distribution)
psfDistribution = Lens.lens (distribution :: PutSubscriptionFilter -> Lude.Maybe Distribution) (\s a -> s {distribution = a} :: PutSubscriptionFilter)
{-# DEPRECATED psfDistribution "Use generic-lens or generic-optics with 'distribution' instead." #-}

-- | The ARN of an IAM role that grants CloudWatch Logs permissions to deliver ingested log events to the destination stream. You don't need to provide the ARN when you are working with a logical destination for cross-account delivery.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psfRoleARN :: Lens.Lens' PutSubscriptionFilter (Lude.Maybe Lude.Text)
psfRoleARN = Lens.lens (roleARN :: PutSubscriptionFilter -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: PutSubscriptionFilter)
{-# DEPRECATED psfRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The name of the log group.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psfLogGroupName :: Lens.Lens' PutSubscriptionFilter Lude.Text
psfLogGroupName = Lens.lens (logGroupName :: PutSubscriptionFilter -> Lude.Text) (\s a -> s {logGroupName = a} :: PutSubscriptionFilter)
{-# DEPRECATED psfLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

-- | A name for the subscription filter. If you are updating an existing filter, you must specify the correct name in @filterName@ . Otherwise, the call fails because you cannot associate a second filter with a log group. To find the name of the filter currently associated with a log group, use <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeSubscriptionFilters.html DescribeSubscriptionFilters> .
--
-- /Note:/ Consider using 'filterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psfFilterName :: Lens.Lens' PutSubscriptionFilter Lude.Text
psfFilterName = Lens.lens (filterName :: PutSubscriptionFilter -> Lude.Text) (\s a -> s {filterName = a} :: PutSubscriptionFilter)
{-# DEPRECATED psfFilterName "Use generic-lens or generic-optics with 'filterName' instead." #-}

-- | A filter pattern for subscribing to a filtered stream of log events.
--
-- /Note:/ Consider using 'filterPattern' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psfFilterPattern :: Lens.Lens' PutSubscriptionFilter Lude.Text
psfFilterPattern = Lens.lens (filterPattern :: PutSubscriptionFilter -> Lude.Text) (\s a -> s {filterPattern = a} :: PutSubscriptionFilter)
{-# DEPRECATED psfFilterPattern "Use generic-lens or generic-optics with 'filterPattern' instead." #-}

-- | The ARN of the destination to deliver matching log events to. Currently, the supported destinations are:
--
--
--     * An Amazon Kinesis stream belonging to the same account as the subscription filter, for same-account delivery.
--
--
--     * A logical destination (specified using an ARN) belonging to a different account, for cross-account delivery.
--
--
--     * An Amazon Kinesis Firehose delivery stream belonging to the same account as the subscription filter, for same-account delivery.
--
--
--     * An AWS Lambda function belonging to the same account as the subscription filter, for same-account delivery.
--
--
--
-- /Note:/ Consider using 'destinationARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psfDestinationARN :: Lens.Lens' PutSubscriptionFilter Lude.Text
psfDestinationARN = Lens.lens (destinationARN :: PutSubscriptionFilter -> Lude.Text) (\s a -> s {destinationARN = a} :: PutSubscriptionFilter)
{-# DEPRECATED psfDestinationARN "Use generic-lens or generic-optics with 'destinationARN' instead." #-}

instance Lude.AWSRequest PutSubscriptionFilter where
  type Rs PutSubscriptionFilter = PutSubscriptionFilterResponse
  request = Req.postJSON cloudWatchLogsService
  response = Res.receiveNull PutSubscriptionFilterResponse'

instance Lude.ToHeaders PutSubscriptionFilter where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Logs_20140328.PutSubscriptionFilter" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutSubscriptionFilter where
  toJSON PutSubscriptionFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("distribution" Lude..=) Lude.<$> distribution,
            ("roleArn" Lude..=) Lude.<$> roleARN,
            Lude.Just ("logGroupName" Lude..= logGroupName),
            Lude.Just ("filterName" Lude..= filterName),
            Lude.Just ("filterPattern" Lude..= filterPattern),
            Lude.Just ("destinationArn" Lude..= destinationARN)
          ]
      )

instance Lude.ToPath PutSubscriptionFilter where
  toPath = Lude.const "/"

instance Lude.ToQuery PutSubscriptionFilter where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutSubscriptionFilterResponse' smart constructor.
data PutSubscriptionFilterResponse = PutSubscriptionFilterResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutSubscriptionFilterResponse' with the minimum fields required to make a request.
mkPutSubscriptionFilterResponse ::
  PutSubscriptionFilterResponse
mkPutSubscriptionFilterResponse = PutSubscriptionFilterResponse'
