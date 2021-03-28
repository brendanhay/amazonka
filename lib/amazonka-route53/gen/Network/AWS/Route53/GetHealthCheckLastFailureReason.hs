{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.GetHealthCheckLastFailureReason
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the reason that a specified health check failed most recently.
module Network.AWS.Route53.GetHealthCheckLastFailureReason
    (
    -- * Creating a request
      GetHealthCheckLastFailureReason (..)
    , mkGetHealthCheckLastFailureReason
    -- ** Request lenses
    , ghclfrHealthCheckId

    -- * Destructuring the response
    , GetHealthCheckLastFailureReasonResponse (..)
    , mkGetHealthCheckLastFailureReasonResponse
    -- ** Response lenses
    , ghclfrrrsHealthCheckObservations
    , ghclfrrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53.Types as Types

-- | A request for the reason that a health check failed most recently.
--
-- /See:/ 'mkGetHealthCheckLastFailureReason' smart constructor.
newtype GetHealthCheckLastFailureReason = GetHealthCheckLastFailureReason'
  { healthCheckId :: Types.HealthCheckId
    -- ^ The ID for the health check for which you want the last failure reason. When you created the health check, @CreateHealthCheck@ returned the ID in the response, in the @HealthCheckId@ element.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetHealthCheckLastFailureReason' value with any optional fields omitted.
mkGetHealthCheckLastFailureReason
    :: Types.HealthCheckId -- ^ 'healthCheckId'
    -> GetHealthCheckLastFailureReason
mkGetHealthCheckLastFailureReason healthCheckId
  = GetHealthCheckLastFailureReason'{healthCheckId}

-- | The ID for the health check for which you want the last failure reason. When you created the health check, @CreateHealthCheck@ returned the ID in the response, in the @HealthCheckId@ element.
--
-- /Note:/ Consider using 'healthCheckId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghclfrHealthCheckId :: Lens.Lens' GetHealthCheckLastFailureReason Types.HealthCheckId
ghclfrHealthCheckId = Lens.field @"healthCheckId"
{-# INLINEABLE ghclfrHealthCheckId #-}
{-# DEPRECATED healthCheckId "Use generic-lens or generic-optics with 'healthCheckId' instead"  #-}

instance Core.ToQuery GetHealthCheckLastFailureReason where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetHealthCheckLastFailureReason where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetHealthCheckLastFailureReason where
        type Rs GetHealthCheckLastFailureReason =
             GetHealthCheckLastFailureReasonResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/2013-04-01/healthcheck/" Core.<> Core.toText healthCheckId
                             Core.<> "/lastfailurereason",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 GetHealthCheckLastFailureReasonResponse' Core.<$>
                   (x Core..@ "HealthCheckObservations" Core..@! Core.mempty Core..<@>
                      Core.parseXMLList "HealthCheckObservation")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | A complex type that contains the response to a @GetHealthCheckLastFailureReason@ request.
--
-- /See:/ 'mkGetHealthCheckLastFailureReasonResponse' smart constructor.
data GetHealthCheckLastFailureReasonResponse = GetHealthCheckLastFailureReasonResponse'
  { healthCheckObservations :: [Types.HealthCheckObservation]
    -- ^ A list that contains one @Observation@ element for each Amazon Route 53 health checker that is reporting a last failure reason. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetHealthCheckLastFailureReasonResponse' value with any optional fields omitted.
mkGetHealthCheckLastFailureReasonResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetHealthCheckLastFailureReasonResponse
mkGetHealthCheckLastFailureReasonResponse responseStatus
  = GetHealthCheckLastFailureReasonResponse'{healthCheckObservations
                                               = Core.mempty,
                                             responseStatus}

-- | A list that contains one @Observation@ element for each Amazon Route 53 health checker that is reporting a last failure reason. 
--
-- /Note:/ Consider using 'healthCheckObservations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghclfrrrsHealthCheckObservations :: Lens.Lens' GetHealthCheckLastFailureReasonResponse [Types.HealthCheckObservation]
ghclfrrrsHealthCheckObservations = Lens.field @"healthCheckObservations"
{-# INLINEABLE ghclfrrrsHealthCheckObservations #-}
{-# DEPRECATED healthCheckObservations "Use generic-lens or generic-optics with 'healthCheckObservations' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghclfrrrsResponseStatus :: Lens.Lens' GetHealthCheckLastFailureReasonResponse Core.Int
ghclfrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ghclfrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
