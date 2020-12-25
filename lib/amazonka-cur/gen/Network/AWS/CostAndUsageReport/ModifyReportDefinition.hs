{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostAndUsageReport.ModifyReportDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows you to programatically update your report preferences.
module Network.AWS.CostAndUsageReport.ModifyReportDefinition
  ( -- * Creating a request
    ModifyReportDefinition (..),
    mkModifyReportDefinition,

    -- ** Request lenses
    mrdReportName,
    mrdReportDefinition,

    -- * Destructuring the response
    ModifyReportDefinitionResponse (..),
    mkModifyReportDefinitionResponse,

    -- ** Response lenses
    mrdrrsResponseStatus,
  )
where

import qualified Network.AWS.CostAndUsageReport.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyReportDefinition' smart constructor.
data ModifyReportDefinition = ModifyReportDefinition'
  { reportName :: Types.ReportName,
    reportDefinition :: Types.ReportDefinition
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyReportDefinition' value with any optional fields omitted.
mkModifyReportDefinition ::
  -- | 'reportName'
  Types.ReportName ->
  -- | 'reportDefinition'
  Types.ReportDefinition ->
  ModifyReportDefinition
mkModifyReportDefinition reportName reportDefinition =
  ModifyReportDefinition' {reportName, reportDefinition}

-- | Undocumented field.
--
-- /Note:/ Consider using 'reportName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrdReportName :: Lens.Lens' ModifyReportDefinition Types.ReportName
mrdReportName = Lens.field @"reportName"
{-# DEPRECATED mrdReportName "Use generic-lens or generic-optics with 'reportName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'reportDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrdReportDefinition :: Lens.Lens' ModifyReportDefinition Types.ReportDefinition
mrdReportDefinition = Lens.field @"reportDefinition"
{-# DEPRECATED mrdReportDefinition "Use generic-lens or generic-optics with 'reportDefinition' instead." #-}

instance Core.FromJSON ModifyReportDefinition where
  toJSON ModifyReportDefinition {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ReportName" Core..= reportName),
            Core.Just ("ReportDefinition" Core..= reportDefinition)
          ]
      )

instance Core.AWSRequest ModifyReportDefinition where
  type Rs ModifyReportDefinition = ModifyReportDefinitionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSOrigamiServiceGatewayService.ModifyReportDefinition"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          ModifyReportDefinitionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkModifyReportDefinitionResponse' smart constructor.
newtype ModifyReportDefinitionResponse = ModifyReportDefinitionResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyReportDefinitionResponse' value with any optional fields omitted.
mkModifyReportDefinitionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ModifyReportDefinitionResponse
mkModifyReportDefinitionResponse responseStatus =
  ModifyReportDefinitionResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrdrrsResponseStatus :: Lens.Lens' ModifyReportDefinitionResponse Core.Int
mrdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED mrdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
