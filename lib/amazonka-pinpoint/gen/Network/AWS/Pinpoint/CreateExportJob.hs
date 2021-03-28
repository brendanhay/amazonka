{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.CreateExportJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an export job for an application.
module Network.AWS.Pinpoint.CreateExportJob
    (
    -- * Creating a request
      CreateExportJob (..)
    , mkCreateExportJob
    -- ** Request lenses
    , cejApplicationId
    , cejExportJobRequest

    -- * Destructuring the response
    , CreateExportJobResponse (..)
    , mkCreateExportJobResponse
    -- ** Response lenses
    , cejrrsExportJobResponse
    , cejrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateExportJob' smart constructor.
data CreateExportJob = CreateExportJob'
  { applicationId :: Core.Text
    -- ^ The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
  , exportJobRequest :: Types.ExportJobRequest
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateExportJob' value with any optional fields omitted.
mkCreateExportJob
    :: Core.Text -- ^ 'applicationId'
    -> Types.ExportJobRequest -- ^ 'exportJobRequest'
    -> CreateExportJob
mkCreateExportJob applicationId exportJobRequest
  = CreateExportJob'{applicationId, exportJobRequest}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cejApplicationId :: Lens.Lens' CreateExportJob Core.Text
cejApplicationId = Lens.field @"applicationId"
{-# INLINEABLE cejApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'exportJobRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cejExportJobRequest :: Lens.Lens' CreateExportJob Types.ExportJobRequest
cejExportJobRequest = Lens.field @"exportJobRequest"
{-# INLINEABLE cejExportJobRequest #-}
{-# DEPRECATED exportJobRequest "Use generic-lens or generic-optics with 'exportJobRequest' instead"  #-}

instance Core.ToQuery CreateExportJob where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateExportJob where
        toHeaders CreateExportJob{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateExportJob where
        toJSON CreateExportJob{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ExportJobRequest" Core..= exportJobRequest)])

instance Core.AWSRequest CreateExportJob where
        type Rs CreateExportJob = CreateExportJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/v1/apps/" Core.<> Core.toText applicationId Core.<>
                             "/jobs/export",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateExportJobResponse' Core.<$>
                   (Core.eitherParseJSON x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateExportJobResponse' smart constructor.
data CreateExportJobResponse = CreateExportJobResponse'
  { exportJobResponse :: Types.ExportJobResponse
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateExportJobResponse' value with any optional fields omitted.
mkCreateExportJobResponse
    :: Types.ExportJobResponse -- ^ 'exportJobResponse'
    -> Core.Int -- ^ 'responseStatus'
    -> CreateExportJobResponse
mkCreateExportJobResponse exportJobResponse responseStatus
  = CreateExportJobResponse'{exportJobResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'exportJobResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cejrrsExportJobResponse :: Lens.Lens' CreateExportJobResponse Types.ExportJobResponse
cejrrsExportJobResponse = Lens.field @"exportJobResponse"
{-# INLINEABLE cejrrsExportJobResponse #-}
{-# DEPRECATED exportJobResponse "Use generic-lens or generic-optics with 'exportJobResponse' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cejrrsResponseStatus :: Lens.Lens' CreateExportJobResponse Core.Int
cejrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cejrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
