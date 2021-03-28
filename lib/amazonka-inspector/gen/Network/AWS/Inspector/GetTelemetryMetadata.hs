{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.GetTelemetryMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Information about the data that is collected for the specified assessment run.
module Network.AWS.Inspector.GetTelemetryMetadata
    (
    -- * Creating a request
      GetTelemetryMetadata (..)
    , mkGetTelemetryMetadata
    -- ** Request lenses
    , gtmAssessmentRunArn

    -- * Destructuring the response
    , GetTelemetryMetadataResponse (..)
    , mkGetTelemetryMetadataResponse
    -- ** Response lenses
    , gtmrrsTelemetryMetadata
    , gtmrrsResponseStatus
    ) where

import qualified Network.AWS.Inspector.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetTelemetryMetadata' smart constructor.
newtype GetTelemetryMetadata = GetTelemetryMetadata'
  { assessmentRunArn :: Types.Arn
    -- ^ The ARN that specifies the assessment run that has the telemetry data that you want to obtain.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetTelemetryMetadata' value with any optional fields omitted.
mkGetTelemetryMetadata
    :: Types.Arn -- ^ 'assessmentRunArn'
    -> GetTelemetryMetadata
mkGetTelemetryMetadata assessmentRunArn
  = GetTelemetryMetadata'{assessmentRunArn}

-- | The ARN that specifies the assessment run that has the telemetry data that you want to obtain.
--
-- /Note:/ Consider using 'assessmentRunArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtmAssessmentRunArn :: Lens.Lens' GetTelemetryMetadata Types.Arn
gtmAssessmentRunArn = Lens.field @"assessmentRunArn"
{-# INLINEABLE gtmAssessmentRunArn #-}
{-# DEPRECATED assessmentRunArn "Use generic-lens or generic-optics with 'assessmentRunArn' instead"  #-}

instance Core.ToQuery GetTelemetryMetadata where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetTelemetryMetadata where
        toHeaders GetTelemetryMetadata{..}
          = Core.pure
              ("X-Amz-Target", "InspectorService.GetTelemetryMetadata")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetTelemetryMetadata where
        toJSON GetTelemetryMetadata{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("assessmentRunArn" Core..= assessmentRunArn)])

instance Core.AWSRequest GetTelemetryMetadata where
        type Rs GetTelemetryMetadata = GetTelemetryMetadataResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetTelemetryMetadataResponse' Core.<$>
                   (x Core..:? "telemetryMetadata" Core..!= Core.mempty) Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetTelemetryMetadataResponse' smart constructor.
data GetTelemetryMetadataResponse = GetTelemetryMetadataResponse'
  { telemetryMetadata :: [Types.TelemetryMetadata]
    -- ^ Telemetry details.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTelemetryMetadataResponse' value with any optional fields omitted.
mkGetTelemetryMetadataResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetTelemetryMetadataResponse
mkGetTelemetryMetadataResponse responseStatus
  = GetTelemetryMetadataResponse'{telemetryMetadata = Core.mempty,
                                  responseStatus}

-- | Telemetry details.
--
-- /Note:/ Consider using 'telemetryMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtmrrsTelemetryMetadata :: Lens.Lens' GetTelemetryMetadataResponse [Types.TelemetryMetadata]
gtmrrsTelemetryMetadata = Lens.field @"telemetryMetadata"
{-# INLINEABLE gtmrrsTelemetryMetadata #-}
{-# DEPRECATED telemetryMetadata "Use generic-lens or generic-optics with 'telemetryMetadata' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtmrrsResponseStatus :: Lens.Lens' GetTelemetryMetadataResponse Core.Int
gtmrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gtmrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
