{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostAndUsageReport.DeleteReportDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified report.
module Network.AWS.CostAndUsageReport.DeleteReportDefinition
  ( -- * Creating a request
    DeleteReportDefinition (..),
    mkDeleteReportDefinition,

    -- ** Request lenses
    drdReportName,

    -- * Destructuring the response
    DeleteReportDefinitionResponse (..),
    mkDeleteReportDefinitionResponse,

    -- ** Response lenses
    drsResponseMessage,
    drsResponseStatus,
  )
where

import qualified Network.AWS.CostAndUsageReport.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Deletes the specified report.
--
-- /See:/ 'mkDeleteReportDefinition' smart constructor.
newtype DeleteReportDefinition = DeleteReportDefinition'
  { -- | The name of the report that you want to delete. The name must be unique, is case sensitive, and can't include spaces.
    reportName :: Core.Maybe Types.ReportName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteReportDefinition' value with any optional fields omitted.
mkDeleteReportDefinition ::
  DeleteReportDefinition
mkDeleteReportDefinition =
  DeleteReportDefinition' {reportName = Core.Nothing}

-- | The name of the report that you want to delete. The name must be unique, is case sensitive, and can't include spaces.
--
-- /Note:/ Consider using 'reportName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdReportName :: Lens.Lens' DeleteReportDefinition (Core.Maybe Types.ReportName)
drdReportName = Lens.field @"reportName"
{-# DEPRECATED drdReportName "Use generic-lens or generic-optics with 'reportName' instead." #-}

instance Core.FromJSON DeleteReportDefinition where
  toJSON DeleteReportDefinition {..} =
    Core.object
      (Core.catMaybes [("ReportName" Core..=) Core.<$> reportName])

instance Core.AWSRequest DeleteReportDefinition where
  type Rs DeleteReportDefinition = DeleteReportDefinitionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSOrigamiServiceGatewayService.DeleteReportDefinition"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteReportDefinitionResponse'
            Core.<$> (x Core..:? "ResponseMessage")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | If the action is successful, the service sends back an HTTP 200 response.
--
-- /See:/ 'mkDeleteReportDefinitionResponse' smart constructor.
data DeleteReportDefinitionResponse = DeleteReportDefinitionResponse'
  { responseMessage :: Core.Maybe Types.ResponseMessage,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteReportDefinitionResponse' value with any optional fields omitted.
mkDeleteReportDefinitionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteReportDefinitionResponse
mkDeleteReportDefinitionResponse responseStatus =
  DeleteReportDefinitionResponse'
    { responseMessage = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'responseMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseMessage :: Lens.Lens' DeleteReportDefinitionResponse (Core.Maybe Types.ResponseMessage)
drsResponseMessage = Lens.field @"responseMessage"
{-# DEPRECATED drsResponseMessage "Use generic-lens or generic-optics with 'responseMessage' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteReportDefinitionResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
