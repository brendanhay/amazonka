{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.PutDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a destination. This operation is used only to create destinations for cross-account subscriptions.
--
-- A destination encapsulates a physical resource (such as an Amazon Kinesis stream) and enables you to subscribe to a real-time stream of log events for a different account, ingested using <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_PutLogEvents.html PutLogEvents> .
-- Through an access policy, a destination controls what is written to it. By default, @PutDestination@ does not set any access policy with the destination, which means a cross-account user cannot call <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_PutSubscriptionFilter.html PutSubscriptionFilter> against this destination. To enable this, the destination owner must call <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_PutDestinationPolicy.html PutDestinationPolicy> after @PutDestination@ .
-- To perform a @PutDestination@ operation, you must also have the @iam:PassRole@ permission.
module Network.AWS.CloudWatchLogs.PutDestination
  ( -- * Creating a request
    PutDestination (..),
    mkPutDestination,

    -- ** Request lenses
    pdDestinationName,
    pdTargetArn,
    pdRoleArn,

    -- * Destructuring the response
    PutDestinationResponse (..),
    mkPutDestinationResponse,

    -- ** Response lenses
    pdrrsDestination,
    pdrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudWatchLogs.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutDestination' smart constructor.
data PutDestination = PutDestination'
  { -- | A name for the destination.
    destinationName :: Types.DestinationName,
    -- | The ARN of an Amazon Kinesis stream to which to deliver matching log events.
    targetArn :: Types.TargetArn,
    -- | The ARN of an IAM role that grants CloudWatch Logs permissions to call the Amazon Kinesis @PutRecord@ operation on the destination stream.
    roleArn :: Types.RoleArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutDestination' value with any optional fields omitted.
mkPutDestination ::
  -- | 'destinationName'
  Types.DestinationName ->
  -- | 'targetArn'
  Types.TargetArn ->
  -- | 'roleArn'
  Types.RoleArn ->
  PutDestination
mkPutDestination destinationName targetArn roleArn =
  PutDestination' {destinationName, targetArn, roleArn}

-- | A name for the destination.
--
-- /Note:/ Consider using 'destinationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdDestinationName :: Lens.Lens' PutDestination Types.DestinationName
pdDestinationName = Lens.field @"destinationName"
{-# DEPRECATED pdDestinationName "Use generic-lens or generic-optics with 'destinationName' instead." #-}

-- | The ARN of an Amazon Kinesis stream to which to deliver matching log events.
--
-- /Note:/ Consider using 'targetArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdTargetArn :: Lens.Lens' PutDestination Types.TargetArn
pdTargetArn = Lens.field @"targetArn"
{-# DEPRECATED pdTargetArn "Use generic-lens or generic-optics with 'targetArn' instead." #-}

-- | The ARN of an IAM role that grants CloudWatch Logs permissions to call the Amazon Kinesis @PutRecord@ operation on the destination stream.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdRoleArn :: Lens.Lens' PutDestination Types.RoleArn
pdRoleArn = Lens.field @"roleArn"
{-# DEPRECATED pdRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

instance Core.FromJSON PutDestination where
  toJSON PutDestination {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("destinationName" Core..= destinationName),
            Core.Just ("targetArn" Core..= targetArn),
            Core.Just ("roleArn" Core..= roleArn)
          ]
      )

instance Core.AWSRequest PutDestination where
  type Rs PutDestination = PutDestinationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Logs_20140328.PutDestination")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          PutDestinationResponse'
            Core.<$> (x Core..:? "destination") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkPutDestinationResponse' smart constructor.
data PutDestinationResponse = PutDestinationResponse'
  { -- | The destination.
    destination :: Core.Maybe Types.Destination,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutDestinationResponse' value with any optional fields omitted.
mkPutDestinationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PutDestinationResponse
mkPutDestinationResponse responseStatus =
  PutDestinationResponse'
    { destination = Core.Nothing,
      responseStatus
    }

-- | The destination.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdrrsDestination :: Lens.Lens' PutDestinationResponse (Core.Maybe Types.Destination)
pdrrsDestination = Lens.field @"destination"
{-# DEPRECATED pdrrsDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdrrsResponseStatus :: Lens.Lens' PutDestinationResponse Core.Int
pdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED pdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
