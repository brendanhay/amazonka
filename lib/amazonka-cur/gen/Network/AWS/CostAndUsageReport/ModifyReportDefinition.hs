{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ModifyReportDefinition (..)
    , mkModifyReportDefinition
    -- ** Request lenses
    , mrdReportName
    , mrdReportDefinition

    -- * Destructuring the response
    , ModifyReportDefinitionResponse (..)
    , mkModifyReportDefinitionResponse
    -- ** Response lenses
    , mrdrrsResponseStatus
    ) where

import qualified Network.AWS.CostAndUsageReport.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyReportDefinition' smart constructor.
data ModifyReportDefinition = ModifyReportDefinition'
  { reportName :: Types.ReportName
  , reportDefinition :: Types.ReportDefinition
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyReportDefinition' value with any optional fields omitted.
mkModifyReportDefinition
    :: Types.ReportName -- ^ 'reportName'
    -> Types.ReportDefinition -- ^ 'reportDefinition'
    -> ModifyReportDefinition
mkModifyReportDefinition reportName reportDefinition
  = ModifyReportDefinition'{reportName, reportDefinition}

-- | Undocumented field.
--
-- /Note:/ Consider using 'reportName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrdReportName :: Lens.Lens' ModifyReportDefinition Types.ReportName
mrdReportName = Lens.field @"reportName"
{-# INLINEABLE mrdReportName #-}
{-# DEPRECATED reportName "Use generic-lens or generic-optics with 'reportName' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'reportDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrdReportDefinition :: Lens.Lens' ModifyReportDefinition Types.ReportDefinition
mrdReportDefinition = Lens.field @"reportDefinition"
{-# INLINEABLE mrdReportDefinition #-}
{-# DEPRECATED reportDefinition "Use generic-lens or generic-optics with 'reportDefinition' instead"  #-}

instance Core.ToQuery ModifyReportDefinition where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ModifyReportDefinition where
        toHeaders ModifyReportDefinition{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSOrigamiServiceGatewayService.ModifyReportDefinition")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ModifyReportDefinition where
        toJSON ModifyReportDefinition{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ReportName" Core..= reportName),
                  Core.Just ("ReportDefinition" Core..= reportDefinition)])

instance Core.AWSRequest ModifyReportDefinition where
        type Rs ModifyReportDefinition = ModifyReportDefinitionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 ModifyReportDefinitionResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyReportDefinitionResponse' smart constructor.
newtype ModifyReportDefinitionResponse = ModifyReportDefinitionResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyReportDefinitionResponse' value with any optional fields omitted.
mkModifyReportDefinitionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyReportDefinitionResponse
mkModifyReportDefinitionResponse responseStatus
  = ModifyReportDefinitionResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrdrrsResponseStatus :: Lens.Lens' ModifyReportDefinitionResponse Core.Int
mrdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mrdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
