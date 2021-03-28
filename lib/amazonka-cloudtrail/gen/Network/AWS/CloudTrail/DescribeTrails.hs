{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.DescribeTrails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves settings for one or more trails associated with the current region for your account.
module Network.AWS.CloudTrail.DescribeTrails
    (
    -- * Creating a request
      DescribeTrails (..)
    , mkDescribeTrails
    -- ** Request lenses
    , dtIncludeShadowTrails
    , dtTrailNameList

    -- * Destructuring the response
    , DescribeTrailsResponse (..)
    , mkDescribeTrailsResponse
    -- ** Response lenses
    , dtrrsTrailList
    , dtrrsResponseStatus
    ) where

import qualified Network.AWS.CloudTrail.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Returns information about the trail.
--
-- /See:/ 'mkDescribeTrails' smart constructor.
data DescribeTrails = DescribeTrails'
  { includeShadowTrails :: Core.Maybe Core.Bool
    -- ^ Specifies whether to include shadow trails in the response. A shadow trail is the replication in a region of a trail that was created in a different region, or in the case of an organization trail, the replication of an organization trail in member accounts. If you do not include shadow trails, organization trails in a member account and region replication trails will not be returned. The default is true.
  , trailNameList :: Core.Maybe [Core.Text]
    -- ^ Specifies a list of trail names, trail ARNs, or both, of the trails to describe. The format of a trail ARN is:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@ 
-- If an empty list is specified, information for the trail in the current region is returned.
--
--     * If an empty list is specified and @IncludeShadowTrails@ is false, then information for all trails in the current region is returned.
--
--
--     * If an empty list is specified and IncludeShadowTrails is null or true, then information for all trails in the current region and any associated shadow trails in other regions is returned.
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTrails' value with any optional fields omitted.
mkDescribeTrails
    :: DescribeTrails
mkDescribeTrails
  = DescribeTrails'{includeShadowTrails = Core.Nothing,
                    trailNameList = Core.Nothing}

-- | Specifies whether to include shadow trails in the response. A shadow trail is the replication in a region of a trail that was created in a different region, or in the case of an organization trail, the replication of an organization trail in member accounts. If you do not include shadow trails, organization trails in a member account and region replication trails will not be returned. The default is true.
--
-- /Note:/ Consider using 'includeShadowTrails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtIncludeShadowTrails :: Lens.Lens' DescribeTrails (Core.Maybe Core.Bool)
dtIncludeShadowTrails = Lens.field @"includeShadowTrails"
{-# INLINEABLE dtIncludeShadowTrails #-}
{-# DEPRECATED includeShadowTrails "Use generic-lens or generic-optics with 'includeShadowTrails' instead"  #-}

-- | Specifies a list of trail names, trail ARNs, or both, of the trails to describe. The format of a trail ARN is:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@ 
-- If an empty list is specified, information for the trail in the current region is returned.
--
--     * If an empty list is specified and @IncludeShadowTrails@ is false, then information for all trails in the current region is returned.
--
--
--     * If an empty list is specified and IncludeShadowTrails is null or true, then information for all trails in the current region and any associated shadow trails in other regions is returned.
--
--
--
-- /Note:/ Consider using 'trailNameList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtTrailNameList :: Lens.Lens' DescribeTrails (Core.Maybe [Core.Text])
dtTrailNameList = Lens.field @"trailNameList"
{-# INLINEABLE dtTrailNameList #-}
{-# DEPRECATED trailNameList "Use generic-lens or generic-optics with 'trailNameList' instead"  #-}

instance Core.ToQuery DescribeTrails where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeTrails where
        toHeaders DescribeTrails{..}
          = Core.pure
              ("X-Amz-Target",
               "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.DescribeTrails")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeTrails where
        toJSON DescribeTrails{..}
          = Core.object
              (Core.catMaybes
                 [("includeShadowTrails" Core..=) Core.<$> includeShadowTrails,
                  ("trailNameList" Core..=) Core.<$> trailNameList])

instance Core.AWSRequest DescribeTrails where
        type Rs DescribeTrails = DescribeTrailsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeTrailsResponse' Core.<$>
                   (x Core..:? "trailList") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Returns the objects or data listed below if successful. Otherwise, returns an error.
--
-- /See:/ 'mkDescribeTrailsResponse' smart constructor.
data DescribeTrailsResponse = DescribeTrailsResponse'
  { trailList :: Core.Maybe [Types.Trail]
    -- ^ The list of trail objects. Trail objects with string values are only returned if values for the objects exist in a trail's configuration. For example, @SNSTopicName@ and @SNSTopicARN@ are only returned in results if a trail is configured to send SNS notifications. Similarly, @KMSKeyId@ only appears in results if a trail's log files are encrypted with AWS KMS-managed keys.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTrailsResponse' value with any optional fields omitted.
mkDescribeTrailsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeTrailsResponse
mkDescribeTrailsResponse responseStatus
  = DescribeTrailsResponse'{trailList = Core.Nothing, responseStatus}

-- | The list of trail objects. Trail objects with string values are only returned if values for the objects exist in a trail's configuration. For example, @SNSTopicName@ and @SNSTopicARN@ are only returned in results if a trail is configured to send SNS notifications. Similarly, @KMSKeyId@ only appears in results if a trail's log files are encrypted with AWS KMS-managed keys.
--
-- /Note:/ Consider using 'trailList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsTrailList :: Lens.Lens' DescribeTrailsResponse (Core.Maybe [Types.Trail])
dtrrsTrailList = Lens.field @"trailList"
{-# INLINEABLE dtrrsTrailList #-}
{-# DEPRECATED trailList "Use generic-lens or generic-optics with 'trailList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsResponseStatus :: Lens.Lens' DescribeTrailsResponse Core.Int
dtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
