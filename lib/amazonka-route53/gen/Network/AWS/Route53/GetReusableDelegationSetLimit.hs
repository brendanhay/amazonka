{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.GetReusableDelegationSetLimit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the maximum number of hosted zones that you can associate with the specified reusable delegation set.
--
-- For the default limit, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DNSLimitations.html Limits> in the /Amazon Route 53 Developer Guide/ . To request a higher limit, <https://console.aws.amazon.com/support/home#/case/create?issueType=service-limit-increase&limitType=service-code-route53 open a case> .
module Network.AWS.Route53.GetReusableDelegationSetLimit
    (
    -- * Creating a request
      GetReusableDelegationSetLimit (..)
    , mkGetReusableDelegationSetLimit
    -- ** Request lenses
    , grdslType
    , grdslDelegationSetId

    -- * Destructuring the response
    , GetReusableDelegationSetLimitResponse (..)
    , mkGetReusableDelegationSetLimitResponse
    -- ** Response lenses
    , grdslrrsLimit
    , grdslrrsCount
    , grdslrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53.Types as Types

-- | A complex type that contains information about the request to create a hosted zone.
--
-- /See:/ 'mkGetReusableDelegationSetLimit' smart constructor.
data GetReusableDelegationSetLimit = GetReusableDelegationSetLimit'
  { type' :: Types.ReusableDelegationSetLimitType
    -- ^ Specify @MAX_ZONES_BY_REUSABLE_DELEGATION_SET@ to get the maximum number of hosted zones that you can associate with the specified reusable delegation set.
  , delegationSetId :: Types.ResourceId
    -- ^ The ID of the delegation set that you want to get the limit for.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetReusableDelegationSetLimit' value with any optional fields omitted.
mkGetReusableDelegationSetLimit
    :: Types.ReusableDelegationSetLimitType -- ^ 'type\''
    -> Types.ResourceId -- ^ 'delegationSetId'
    -> GetReusableDelegationSetLimit
mkGetReusableDelegationSetLimit type' delegationSetId
  = GetReusableDelegationSetLimit'{type', delegationSetId}

-- | Specify @MAX_ZONES_BY_REUSABLE_DELEGATION_SET@ to get the maximum number of hosted zones that you can associate with the specified reusable delegation set.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdslType :: Lens.Lens' GetReusableDelegationSetLimit Types.ReusableDelegationSetLimitType
grdslType = Lens.field @"type'"
{-# INLINEABLE grdslType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The ID of the delegation set that you want to get the limit for.
--
-- /Note:/ Consider using 'delegationSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdslDelegationSetId :: Lens.Lens' GetReusableDelegationSetLimit Types.ResourceId
grdslDelegationSetId = Lens.field @"delegationSetId"
{-# INLINEABLE grdslDelegationSetId #-}
{-# DEPRECATED delegationSetId "Use generic-lens or generic-optics with 'delegationSetId' instead"  #-}

instance Core.ToQuery GetReusableDelegationSetLimit where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetReusableDelegationSetLimit where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetReusableDelegationSetLimit where
        type Rs GetReusableDelegationSetLimit =
             GetReusableDelegationSetLimitResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/2013-04-01/reusabledelegationsetlimit/" Core.<>
                             Core.toText delegationSetId
                             Core.<> "/"
                             Core.<> Core.toText type',
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 GetReusableDelegationSetLimitResponse' Core.<$>
                   (x Core..@ "Limit") Core.<*> x Core..@ "Count" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | A complex type that contains the requested limit. 
--
-- /See:/ 'mkGetReusableDelegationSetLimitResponse' smart constructor.
data GetReusableDelegationSetLimitResponse = GetReusableDelegationSetLimitResponse'
  { limit :: Types.ReusableDelegationSetLimit
    -- ^ The current setting for the limit on hosted zones that you can associate with the specified reusable delegation set.
  , count :: Core.Natural
    -- ^ The current number of hosted zones that you can associate with the specified reusable delegation set.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetReusableDelegationSetLimitResponse' value with any optional fields omitted.
mkGetReusableDelegationSetLimitResponse
    :: Types.ReusableDelegationSetLimit -- ^ 'limit'
    -> Core.Natural -- ^ 'count'
    -> Core.Int -- ^ 'responseStatus'
    -> GetReusableDelegationSetLimitResponse
mkGetReusableDelegationSetLimitResponse limit count responseStatus
  = GetReusableDelegationSetLimitResponse'{limit, count,
                                           responseStatus}

-- | The current setting for the limit on hosted zones that you can associate with the specified reusable delegation set.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdslrrsLimit :: Lens.Lens' GetReusableDelegationSetLimitResponse Types.ReusableDelegationSetLimit
grdslrrsLimit = Lens.field @"limit"
{-# INLINEABLE grdslrrsLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | The current number of hosted zones that you can associate with the specified reusable delegation set.
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdslrrsCount :: Lens.Lens' GetReusableDelegationSetLimitResponse Core.Natural
grdslrrsCount = Lens.field @"count"
{-# INLINEABLE grdslrrsCount #-}
{-# DEPRECATED count "Use generic-lens or generic-optics with 'count' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdslrrsResponseStatus :: Lens.Lens' GetReusableDelegationSetLimitResponse Core.Int
grdslrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE grdslrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
