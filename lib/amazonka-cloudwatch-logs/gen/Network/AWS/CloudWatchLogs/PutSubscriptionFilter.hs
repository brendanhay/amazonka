{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    psfLogGroupName,
    psfFilterName,
    psfFilterPattern,
    psfDestinationArn,
    psfDistribution,
    psfRoleArn,

    -- * Destructuring the response
    PutSubscriptionFilterResponse (..),
    mkPutSubscriptionFilterResponse,
  )
where

import qualified Network.AWS.CloudWatchLogs.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutSubscriptionFilter' smart constructor.
data PutSubscriptionFilter = PutSubscriptionFilter'
  { -- | The name of the log group.
    logGroupName :: Types.LogGroupName,
    -- | A name for the subscription filter. If you are updating an existing filter, you must specify the correct name in @filterName@ . Otherwise, the call fails because you cannot associate a second filter with a log group. To find the name of the filter currently associated with a log group, use <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeSubscriptionFilters.html DescribeSubscriptionFilters> .
    filterName :: Types.FilterName,
    -- | A filter pattern for subscribing to a filtered stream of log events.
    filterPattern :: Types.FilterPattern,
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
    destinationArn :: Types.DestinationArn,
    -- | The method used to distribute log data to the destination. By default, log data is grouped by log stream, but the grouping can be set to random for a more even distribution. This property is only applicable when the destination is an Amazon Kinesis stream.
    distribution :: Core.Maybe Types.Distribution,
    -- | The ARN of an IAM role that grants CloudWatch Logs permissions to deliver ingested log events to the destination stream. You don't need to provide the ARN when you are working with a logical destination for cross-account delivery.
    roleArn :: Core.Maybe Types.RoleArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutSubscriptionFilter' value with any optional fields omitted.
mkPutSubscriptionFilter ::
  -- | 'logGroupName'
  Types.LogGroupName ->
  -- | 'filterName'
  Types.FilterName ->
  -- | 'filterPattern'
  Types.FilterPattern ->
  -- | 'destinationArn'
  Types.DestinationArn ->
  PutSubscriptionFilter
mkPutSubscriptionFilter
  logGroupName
  filterName
  filterPattern
  destinationArn =
    PutSubscriptionFilter'
      { logGroupName,
        filterName,
        filterPattern,
        destinationArn,
        distribution = Core.Nothing,
        roleArn = Core.Nothing
      }

-- | The name of the log group.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psfLogGroupName :: Lens.Lens' PutSubscriptionFilter Types.LogGroupName
psfLogGroupName = Lens.field @"logGroupName"
{-# DEPRECATED psfLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

-- | A name for the subscription filter. If you are updating an existing filter, you must specify the correct name in @filterName@ . Otherwise, the call fails because you cannot associate a second filter with a log group. To find the name of the filter currently associated with a log group, use <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeSubscriptionFilters.html DescribeSubscriptionFilters> .
--
-- /Note:/ Consider using 'filterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psfFilterName :: Lens.Lens' PutSubscriptionFilter Types.FilterName
psfFilterName = Lens.field @"filterName"
{-# DEPRECATED psfFilterName "Use generic-lens or generic-optics with 'filterName' instead." #-}

-- | A filter pattern for subscribing to a filtered stream of log events.
--
-- /Note:/ Consider using 'filterPattern' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psfFilterPattern :: Lens.Lens' PutSubscriptionFilter Types.FilterPattern
psfFilterPattern = Lens.field @"filterPattern"
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
-- /Note:/ Consider using 'destinationArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psfDestinationArn :: Lens.Lens' PutSubscriptionFilter Types.DestinationArn
psfDestinationArn = Lens.field @"destinationArn"
{-# DEPRECATED psfDestinationArn "Use generic-lens or generic-optics with 'destinationArn' instead." #-}

-- | The method used to distribute log data to the destination. By default, log data is grouped by log stream, but the grouping can be set to random for a more even distribution. This property is only applicable when the destination is an Amazon Kinesis stream.
--
-- /Note:/ Consider using 'distribution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psfDistribution :: Lens.Lens' PutSubscriptionFilter (Core.Maybe Types.Distribution)
psfDistribution = Lens.field @"distribution"
{-# DEPRECATED psfDistribution "Use generic-lens or generic-optics with 'distribution' instead." #-}

-- | The ARN of an IAM role that grants CloudWatch Logs permissions to deliver ingested log events to the destination stream. You don't need to provide the ARN when you are working with a logical destination for cross-account delivery.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psfRoleArn :: Lens.Lens' PutSubscriptionFilter (Core.Maybe Types.RoleArn)
psfRoleArn = Lens.field @"roleArn"
{-# DEPRECATED psfRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

instance Core.FromJSON PutSubscriptionFilter where
  toJSON PutSubscriptionFilter {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("logGroupName" Core..= logGroupName),
            Core.Just ("filterName" Core..= filterName),
            Core.Just ("filterPattern" Core..= filterPattern),
            Core.Just ("destinationArn" Core..= destinationArn),
            ("distribution" Core..=) Core.<$> distribution,
            ("roleArn" Core..=) Core.<$> roleArn
          ]
      )

instance Core.AWSRequest PutSubscriptionFilter where
  type Rs PutSubscriptionFilter = PutSubscriptionFilterResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Logs_20140328.PutSubscriptionFilter")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull PutSubscriptionFilterResponse'

-- | /See:/ 'mkPutSubscriptionFilterResponse' smart constructor.
data PutSubscriptionFilterResponse = PutSubscriptionFilterResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutSubscriptionFilterResponse' value with any optional fields omitted.
mkPutSubscriptionFilterResponse ::
  PutSubscriptionFilterResponse
mkPutSubscriptionFilterResponse = PutSubscriptionFilterResponse'
