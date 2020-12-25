{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetTelemetryMetadata (..),
    mkGetTelemetryMetadata,

    -- ** Request lenses
    gtmAssessmentRunArn,

    -- * Destructuring the response
    GetTelemetryMetadataResponse (..),
    mkGetTelemetryMetadataResponse,

    -- ** Response lenses
    gtmrrsTelemetryMetadata,
    gtmrrsResponseStatus,
  )
where

import qualified Network.AWS.Inspector.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetTelemetryMetadata' smart constructor.
newtype GetTelemetryMetadata = GetTelemetryMetadata'
  { -- | The ARN that specifies the assessment run that has the telemetry data that you want to obtain.
    assessmentRunArn :: Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetTelemetryMetadata' value with any optional fields omitted.
mkGetTelemetryMetadata ::
  -- | 'assessmentRunArn'
  Types.Arn ->
  GetTelemetryMetadata
mkGetTelemetryMetadata assessmentRunArn =
  GetTelemetryMetadata' {assessmentRunArn}

-- | The ARN that specifies the assessment run that has the telemetry data that you want to obtain.
--
-- /Note:/ Consider using 'assessmentRunArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtmAssessmentRunArn :: Lens.Lens' GetTelemetryMetadata Types.Arn
gtmAssessmentRunArn = Lens.field @"assessmentRunArn"
{-# DEPRECATED gtmAssessmentRunArn "Use generic-lens or generic-optics with 'assessmentRunArn' instead." #-}

instance Core.FromJSON GetTelemetryMetadata where
  toJSON GetTelemetryMetadata {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("assessmentRunArn" Core..= assessmentRunArn)]
      )

instance Core.AWSRequest GetTelemetryMetadata where
  type Rs GetTelemetryMetadata = GetTelemetryMetadataResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "InspectorService.GetTelemetryMetadata")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTelemetryMetadataResponse'
            Core.<$> (x Core..:? "telemetryMetadata" Core..!= Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetTelemetryMetadataResponse' smart constructor.
data GetTelemetryMetadataResponse = GetTelemetryMetadataResponse'
  { -- | Telemetry details.
    telemetryMetadata :: [Types.TelemetryMetadata],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTelemetryMetadataResponse' value with any optional fields omitted.
mkGetTelemetryMetadataResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetTelemetryMetadataResponse
mkGetTelemetryMetadataResponse responseStatus =
  GetTelemetryMetadataResponse'
    { telemetryMetadata = Core.mempty,
      responseStatus
    }

-- | Telemetry details.
--
-- /Note:/ Consider using 'telemetryMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtmrrsTelemetryMetadata :: Lens.Lens' GetTelemetryMetadataResponse [Types.TelemetryMetadata]
gtmrrsTelemetryMetadata = Lens.field @"telemetryMetadata"
{-# DEPRECATED gtmrrsTelemetryMetadata "Use generic-lens or generic-optics with 'telemetryMetadata' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtmrrsResponseStatus :: Lens.Lens' GetTelemetryMetadataResponse Core.Int
gtmrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gtmrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
