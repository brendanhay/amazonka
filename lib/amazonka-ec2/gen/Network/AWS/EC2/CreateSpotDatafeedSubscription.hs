{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateSpotDatafeedSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a data feed for Spot Instances, enabling you to view Spot Instance usage logs. You can create one data feed per AWS account. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-data-feeds.html Spot Instance data feed> in the /Amazon EC2 User Guide for Linux Instances/ .
module Network.AWS.EC2.CreateSpotDatafeedSubscription
  ( -- * Creating a request
    CreateSpotDatafeedSubscription (..),
    mkCreateSpotDatafeedSubscription,

    -- ** Request lenses
    csdsBucket,
    csdsDryRun,
    csdsPrefix,

    -- * Destructuring the response
    CreateSpotDatafeedSubscriptionResponse (..),
    mkCreateSpotDatafeedSubscriptionResponse,

    -- ** Response lenses
    csdsrrsSpotDatafeedSubscription,
    csdsrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for CreateSpotDatafeedSubscription.
--
-- /See:/ 'mkCreateSpotDatafeedSubscription' smart constructor.
data CreateSpotDatafeedSubscription = CreateSpotDatafeedSubscription'
  { -- | The name of the Amazon S3 bucket in which to store the Spot Instance data feed. For more information about bucket names, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/BucketRestrictions.html#bucketnamingrules Rules for bucket naming> in the /Amazon S3 Developer Guide/ .
    bucket :: Types.String,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The prefix for the data feed file names.
    prefix :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSpotDatafeedSubscription' value with any optional fields omitted.
mkCreateSpotDatafeedSubscription ::
  -- | 'bucket'
  Types.String ->
  CreateSpotDatafeedSubscription
mkCreateSpotDatafeedSubscription bucket =
  CreateSpotDatafeedSubscription'
    { bucket,
      dryRun = Core.Nothing,
      prefix = Core.Nothing
    }

-- | The name of the Amazon S3 bucket in which to store the Spot Instance data feed. For more information about bucket names, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/BucketRestrictions.html#bucketnamingrules Rules for bucket naming> in the /Amazon S3 Developer Guide/ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdsBucket :: Lens.Lens' CreateSpotDatafeedSubscription Types.String
csdsBucket = Lens.field @"bucket"
{-# DEPRECATED csdsBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdsDryRun :: Lens.Lens' CreateSpotDatafeedSubscription (Core.Maybe Core.Bool)
csdsDryRun = Lens.field @"dryRun"
{-# DEPRECATED csdsDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The prefix for the data feed file names.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdsPrefix :: Lens.Lens' CreateSpotDatafeedSubscription (Core.Maybe Types.String)
csdsPrefix = Lens.field @"prefix"
{-# DEPRECATED csdsPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

instance Core.AWSRequest CreateSpotDatafeedSubscription where
  type
    Rs CreateSpotDatafeedSubscription =
      CreateSpotDatafeedSubscriptionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "CreateSpotDatafeedSubscription")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "Bucket" bucket)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryValue "Prefix" Core.<$> prefix)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          CreateSpotDatafeedSubscriptionResponse'
            Core.<$> (x Core..@? "spotDatafeedSubscription")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the output of CreateSpotDatafeedSubscription.
--
-- /See:/ 'mkCreateSpotDatafeedSubscriptionResponse' smart constructor.
data CreateSpotDatafeedSubscriptionResponse = CreateSpotDatafeedSubscriptionResponse'
  { -- | The Spot Instance data feed subscription.
    spotDatafeedSubscription :: Core.Maybe Types.SpotDatafeedSubscription,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSpotDatafeedSubscriptionResponse' value with any optional fields omitted.
mkCreateSpotDatafeedSubscriptionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateSpotDatafeedSubscriptionResponse
mkCreateSpotDatafeedSubscriptionResponse responseStatus =
  CreateSpotDatafeedSubscriptionResponse'
    { spotDatafeedSubscription =
        Core.Nothing,
      responseStatus
    }

-- | The Spot Instance data feed subscription.
--
-- /Note:/ Consider using 'spotDatafeedSubscription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdsrrsSpotDatafeedSubscription :: Lens.Lens' CreateSpotDatafeedSubscriptionResponse (Core.Maybe Types.SpotDatafeedSubscription)
csdsrrsSpotDatafeedSubscription = Lens.field @"spotDatafeedSubscription"
{-# DEPRECATED csdsrrsSpotDatafeedSubscription "Use generic-lens or generic-optics with 'spotDatafeedSubscription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdsrrsResponseStatus :: Lens.Lens' CreateSpotDatafeedSubscriptionResponse Core.Int
csdsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED csdsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
