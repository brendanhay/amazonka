{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostAndUsageReport.PutReportDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new report using the description that you provide.
module Network.AWS.CostAndUsageReport.PutReportDefinition
  ( -- * Creating a request
    PutReportDefinition (..),
    mkPutReportDefinition,

    -- ** Request lenses
    prdReportDefinition,

    -- * Destructuring the response
    PutReportDefinitionResponse (..),
    mkPutReportDefinitionResponse,

    -- ** Response lenses
    prdrrsResponseStatus,
  )
where

import qualified Network.AWS.CostAndUsageReport.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Creates a Cost and Usage Report.
--
-- /See:/ 'mkPutReportDefinition' smart constructor.
newtype PutReportDefinition = PutReportDefinition'
  { -- | Represents the output of the PutReportDefinition operation. The content consists of the detailed metadata and data file information.
    reportDefinition :: Types.ReportDefinition
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PutReportDefinition' value with any optional fields omitted.
mkPutReportDefinition ::
  -- | 'reportDefinition'
  Types.ReportDefinition ->
  PutReportDefinition
mkPutReportDefinition reportDefinition =
  PutReportDefinition' {reportDefinition}

-- | Represents the output of the PutReportDefinition operation. The content consists of the detailed metadata and data file information.
--
-- /Note:/ Consider using 'reportDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prdReportDefinition :: Lens.Lens' PutReportDefinition Types.ReportDefinition
prdReportDefinition = Lens.field @"reportDefinition"
{-# DEPRECATED prdReportDefinition "Use generic-lens or generic-optics with 'reportDefinition' instead." #-}

instance Core.FromJSON PutReportDefinition where
  toJSON PutReportDefinition {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("ReportDefinition" Core..= reportDefinition)]
      )

instance Core.AWSRequest PutReportDefinition where
  type Rs PutReportDefinition = PutReportDefinitionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSOrigamiServiceGatewayService.PutReportDefinition"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutReportDefinitionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | If the action is successful, the service sends back an HTTP 200 response with an empty HTTP body.
--
-- /See:/ 'mkPutReportDefinitionResponse' smart constructor.
newtype PutReportDefinitionResponse = PutReportDefinitionResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PutReportDefinitionResponse' value with any optional fields omitted.
mkPutReportDefinitionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PutReportDefinitionResponse
mkPutReportDefinitionResponse responseStatus =
  PutReportDefinitionResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prdrrsResponseStatus :: Lens.Lens' PutReportDefinitionResponse Core.Int
prdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED prdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
