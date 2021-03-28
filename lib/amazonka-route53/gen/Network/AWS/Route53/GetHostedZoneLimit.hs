{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.GetHostedZoneLimit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the specified limit for a specified hosted zone, for example, the maximum number of records that you can create in the hosted zone. 
--
-- For the default limit, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DNSLimitations.html Limits> in the /Amazon Route 53 Developer Guide/ . To request a higher limit, <https://console.aws.amazon.com/support/home#/case/create?issueType=service-limit-increase&limitType=service-code-route53 open a case> .
module Network.AWS.Route53.GetHostedZoneLimit
    (
    -- * Creating a request
      GetHostedZoneLimit (..)
    , mkGetHostedZoneLimit
    -- ** Request lenses
    , ghzlType
    , ghzlHostedZoneId

    -- * Destructuring the response
    , GetHostedZoneLimitResponse (..)
    , mkGetHostedZoneLimitResponse
    -- ** Response lenses
    , ghzlrrsLimit
    , ghzlrrsCount
    , ghzlrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53.Types as Types

-- | A complex type that contains information about the request to create a hosted zone.
--
-- /See:/ 'mkGetHostedZoneLimit' smart constructor.
data GetHostedZoneLimit = GetHostedZoneLimit'
  { type' :: Types.HostedZoneLimitType
    -- ^ The limit that you want to get. Valid values include the following:
--
--
--     * __MAX_RRSETS_BY_ZONE__ : The maximum number of records that you can create in the specified hosted zone.
--
--
--     * __MAX_VPCS_ASSOCIATED_BY_ZONE__ : The maximum number of Amazon VPCs that you can associate with the specified private hosted zone.
--
--
  , hostedZoneId :: Types.ResourceId
    -- ^ The ID of the hosted zone that you want to get a limit for.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetHostedZoneLimit' value with any optional fields omitted.
mkGetHostedZoneLimit
    :: Types.HostedZoneLimitType -- ^ 'type\''
    -> Types.ResourceId -- ^ 'hostedZoneId'
    -> GetHostedZoneLimit
mkGetHostedZoneLimit type' hostedZoneId
  = GetHostedZoneLimit'{type', hostedZoneId}

-- | The limit that you want to get. Valid values include the following:
--
--
--     * __MAX_RRSETS_BY_ZONE__ : The maximum number of records that you can create in the specified hosted zone.
--
--
--     * __MAX_VPCS_ASSOCIATED_BY_ZONE__ : The maximum number of Amazon VPCs that you can associate with the specified private hosted zone.
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghzlType :: Lens.Lens' GetHostedZoneLimit Types.HostedZoneLimitType
ghzlType = Lens.field @"type'"
{-# INLINEABLE ghzlType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The ID of the hosted zone that you want to get a limit for.
--
-- /Note:/ Consider using 'hostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghzlHostedZoneId :: Lens.Lens' GetHostedZoneLimit Types.ResourceId
ghzlHostedZoneId = Lens.field @"hostedZoneId"
{-# INLINEABLE ghzlHostedZoneId #-}
{-# DEPRECATED hostedZoneId "Use generic-lens or generic-optics with 'hostedZoneId' instead"  #-}

instance Core.ToQuery GetHostedZoneLimit where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetHostedZoneLimit where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetHostedZoneLimit where
        type Rs GetHostedZoneLimit = GetHostedZoneLimitResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/2013-04-01/hostedzonelimit/" Core.<> Core.toText hostedZoneId
                             Core.<> "/"
                             Core.<> Core.toText type',
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 GetHostedZoneLimitResponse' Core.<$>
                   (x Core..@ "Limit") Core.<*> x Core..@ "Count" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | A complex type that contains the requested limit. 
--
-- /See:/ 'mkGetHostedZoneLimitResponse' smart constructor.
data GetHostedZoneLimitResponse = GetHostedZoneLimitResponse'
  { limit :: Types.HostedZoneLimit
    -- ^ The current setting for the specified limit. For example, if you specified @MAX_RRSETS_BY_ZONE@ for the value of @Type@ in the request, the value of @Limit@ is the maximum number of records that you can create in the specified hosted zone.
  , count :: Core.Natural
    -- ^ The current number of entities that you have created of the specified type. For example, if you specified @MAX_RRSETS_BY_ZONE@ for the value of @Type@ in the request, the value of @Count@ is the current number of records that you have created in the specified hosted zone.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetHostedZoneLimitResponse' value with any optional fields omitted.
mkGetHostedZoneLimitResponse
    :: Types.HostedZoneLimit -- ^ 'limit'
    -> Core.Natural -- ^ 'count'
    -> Core.Int -- ^ 'responseStatus'
    -> GetHostedZoneLimitResponse
mkGetHostedZoneLimitResponse limit count responseStatus
  = GetHostedZoneLimitResponse'{limit, count, responseStatus}

-- | The current setting for the specified limit. For example, if you specified @MAX_RRSETS_BY_ZONE@ for the value of @Type@ in the request, the value of @Limit@ is the maximum number of records that you can create in the specified hosted zone.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghzlrrsLimit :: Lens.Lens' GetHostedZoneLimitResponse Types.HostedZoneLimit
ghzlrrsLimit = Lens.field @"limit"
{-# INLINEABLE ghzlrrsLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | The current number of entities that you have created of the specified type. For example, if you specified @MAX_RRSETS_BY_ZONE@ for the value of @Type@ in the request, the value of @Count@ is the current number of records that you have created in the specified hosted zone.
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghzlrrsCount :: Lens.Lens' GetHostedZoneLimitResponse Core.Natural
ghzlrrsCount = Lens.field @"count"
{-# INLINEABLE ghzlrrsCount #-}
{-# DEPRECATED count "Use generic-lens or generic-optics with 'count' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghzlrrsResponseStatus :: Lens.Lens' GetHostedZoneLimitResponse Core.Int
ghzlrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ghzlrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
