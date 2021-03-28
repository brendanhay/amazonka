{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateSpotDatafeedSubscription (..)
    , mkCreateSpotDatafeedSubscription
    -- ** Request lenses
    , csdsBucket
    , csdsDryRun
    , csdsPrefix

    -- * Destructuring the response
    , CreateSpotDatafeedSubscriptionResponse (..)
    , mkCreateSpotDatafeedSubscriptionResponse
    -- ** Response lenses
    , csdsrrsSpotDatafeedSubscription
    , csdsrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for CreateSpotDatafeedSubscription.
--
-- /See:/ 'mkCreateSpotDatafeedSubscription' smart constructor.
data CreateSpotDatafeedSubscription = CreateSpotDatafeedSubscription'
  { bucket :: Core.Text
    -- ^ The name of the Amazon S3 bucket in which to store the Spot Instance data feed. For more information about bucket names, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/BucketRestrictions.html#bucketnamingrules Rules for bucket naming> in the /Amazon S3 Developer Guide/ .
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , prefix :: Core.Maybe Core.Text
    -- ^ The prefix for the data feed file names.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSpotDatafeedSubscription' value with any optional fields omitted.
mkCreateSpotDatafeedSubscription
    :: Core.Text -- ^ 'bucket'
    -> CreateSpotDatafeedSubscription
mkCreateSpotDatafeedSubscription bucket
  = CreateSpotDatafeedSubscription'{bucket, dryRun = Core.Nothing,
                                    prefix = Core.Nothing}

-- | The name of the Amazon S3 bucket in which to store the Spot Instance data feed. For more information about bucket names, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/BucketRestrictions.html#bucketnamingrules Rules for bucket naming> in the /Amazon S3 Developer Guide/ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdsBucket :: Lens.Lens' CreateSpotDatafeedSubscription Core.Text
csdsBucket = Lens.field @"bucket"
{-# INLINEABLE csdsBucket #-}
{-# DEPRECATED bucket "Use generic-lens or generic-optics with 'bucket' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdsDryRun :: Lens.Lens' CreateSpotDatafeedSubscription (Core.Maybe Core.Bool)
csdsDryRun = Lens.field @"dryRun"
{-# INLINEABLE csdsDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The prefix for the data feed file names.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdsPrefix :: Lens.Lens' CreateSpotDatafeedSubscription (Core.Maybe Core.Text)
csdsPrefix = Lens.field @"prefix"
{-# INLINEABLE csdsPrefix #-}
{-# DEPRECATED prefix "Use generic-lens or generic-optics with 'prefix' instead"  #-}

instance Core.ToQuery CreateSpotDatafeedSubscription where
        toQuery CreateSpotDatafeedSubscription{..}
          = Core.toQueryPair "Action"
              ("CreateSpotDatafeedSubscription" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "Bucket" bucket
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Prefix") prefix

instance Core.ToHeaders CreateSpotDatafeedSubscription where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateSpotDatafeedSubscription where
        type Rs CreateSpotDatafeedSubscription =
             CreateSpotDatafeedSubscriptionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 CreateSpotDatafeedSubscriptionResponse' Core.<$>
                   (x Core..@? "spotDatafeedSubscription") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the output of CreateSpotDatafeedSubscription.
--
-- /See:/ 'mkCreateSpotDatafeedSubscriptionResponse' smart constructor.
data CreateSpotDatafeedSubscriptionResponse = CreateSpotDatafeedSubscriptionResponse'
  { spotDatafeedSubscription :: Core.Maybe Types.SpotDatafeedSubscription
    -- ^ The Spot Instance data feed subscription.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSpotDatafeedSubscriptionResponse' value with any optional fields omitted.
mkCreateSpotDatafeedSubscriptionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateSpotDatafeedSubscriptionResponse
mkCreateSpotDatafeedSubscriptionResponse responseStatus
  = CreateSpotDatafeedSubscriptionResponse'{spotDatafeedSubscription
                                              = Core.Nothing,
                                            responseStatus}

-- | The Spot Instance data feed subscription.
--
-- /Note:/ Consider using 'spotDatafeedSubscription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdsrrsSpotDatafeedSubscription :: Lens.Lens' CreateSpotDatafeedSubscriptionResponse (Core.Maybe Types.SpotDatafeedSubscription)
csdsrrsSpotDatafeedSubscription = Lens.field @"spotDatafeedSubscription"
{-# INLINEABLE csdsrrsSpotDatafeedSubscription #-}
{-# DEPRECATED spotDatafeedSubscription "Use generic-lens or generic-optics with 'spotDatafeedSubscription' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdsrrsResponseStatus :: Lens.Lens' CreateSpotDatafeedSubscriptionResponse Core.Int
csdsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE csdsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
