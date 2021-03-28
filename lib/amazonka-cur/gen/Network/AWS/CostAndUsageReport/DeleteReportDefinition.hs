{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DeleteReportDefinition (..)
    , mkDeleteReportDefinition
    -- ** Request lenses
    , drdReportName

    -- * Destructuring the response
    , DeleteReportDefinitionResponse (..)
    , mkDeleteReportDefinitionResponse
    -- ** Response lenses
    , drsResponseMessage
    , drsResponseStatus
    ) where

import qualified Network.AWS.CostAndUsageReport.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Deletes the specified report.
--
-- /See:/ 'mkDeleteReportDefinition' smart constructor.
newtype DeleteReportDefinition = DeleteReportDefinition'
  { reportName :: Core.Maybe Types.ReportName
    -- ^ The name of the report that you want to delete. The name must be unique, is case sensitive, and can't include spaces.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteReportDefinition' value with any optional fields omitted.
mkDeleteReportDefinition
    :: DeleteReportDefinition
mkDeleteReportDefinition
  = DeleteReportDefinition'{reportName = Core.Nothing}

-- | The name of the report that you want to delete. The name must be unique, is case sensitive, and can't include spaces.
--
-- /Note:/ Consider using 'reportName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdReportName :: Lens.Lens' DeleteReportDefinition (Core.Maybe Types.ReportName)
drdReportName = Lens.field @"reportName"
{-# INLINEABLE drdReportName #-}
{-# DEPRECATED reportName "Use generic-lens or generic-optics with 'reportName' instead"  #-}

instance Core.ToQuery DeleteReportDefinition where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteReportDefinition where
        toHeaders DeleteReportDefinition{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSOrigamiServiceGatewayService.DeleteReportDefinition")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteReportDefinition where
        toJSON DeleteReportDefinition{..}
          = Core.object
              (Core.catMaybes [("ReportName" Core..=) Core.<$> reportName])

instance Core.AWSRequest DeleteReportDefinition where
        type Rs DeleteReportDefinition = DeleteReportDefinitionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteReportDefinitionResponse' Core.<$>
                   (x Core..:? "ResponseMessage") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | If the action is successful, the service sends back an HTTP 200 response.
--
-- /See:/ 'mkDeleteReportDefinitionResponse' smart constructor.
data DeleteReportDefinitionResponse = DeleteReportDefinitionResponse'
  { responseMessage :: Core.Maybe Types.ResponseMessage
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteReportDefinitionResponse' value with any optional fields omitted.
mkDeleteReportDefinitionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteReportDefinitionResponse
mkDeleteReportDefinitionResponse responseStatus
  = DeleteReportDefinitionResponse'{responseMessage = Core.Nothing,
                                    responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'responseMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseMessage :: Lens.Lens' DeleteReportDefinitionResponse (Core.Maybe Types.ResponseMessage)
drsResponseMessage = Lens.field @"responseMessage"
{-# INLINEABLE drsResponseMessage #-}
{-# DEPRECATED responseMessage "Use generic-lens or generic-optics with 'responseMessage' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteReportDefinitionResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
