{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.DescribeExport
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an existing table export.
module Network.AWS.DynamoDB.DescribeExport
    (
    -- * Creating a request
      DescribeExport (..)
    , mkDescribeExport
    -- ** Request lenses
    , deExportArn

    -- * Destructuring the response
    , DescribeExportResponse (..)
    , mkDescribeExportResponse
    -- ** Response lenses
    , derfrsExportDescription
    , derfrsResponseStatus
    ) where

import qualified Network.AWS.DynamoDB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeExport' smart constructor.
newtype DescribeExport = DescribeExport'
  { exportArn :: Types.ExportArn
    -- ^ The Amazon Resource Name (ARN) associated with the export.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeExport' value with any optional fields omitted.
mkDescribeExport
    :: Types.ExportArn -- ^ 'exportArn'
    -> DescribeExport
mkDescribeExport exportArn = DescribeExport'{exportArn}

-- | The Amazon Resource Name (ARN) associated with the export.
--
-- /Note:/ Consider using 'exportArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deExportArn :: Lens.Lens' DescribeExport Types.ExportArn
deExportArn = Lens.field @"exportArn"
{-# INLINEABLE deExportArn #-}
{-# DEPRECATED exportArn "Use generic-lens or generic-optics with 'exportArn' instead"  #-}

instance Core.ToQuery DescribeExport where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeExport where
        toHeaders DescribeExport{..}
          = Core.pure ("X-Amz-Target", "DynamoDB_20120810.DescribeExport")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.0")

instance Core.FromJSON DescribeExport where
        toJSON DescribeExport{..}
          = Core.object
              (Core.catMaybes [Core.Just ("ExportArn" Core..= exportArn)])

instance Core.AWSRequest DescribeExport where
        type Rs DescribeExport = DescribeExportResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeExportResponse' Core.<$>
                   (x Core..:? "ExportDescription") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeExportResponse' smart constructor.
data DescribeExportResponse = DescribeExportResponse'
  { exportDescription :: Core.Maybe Types.ExportDescription
    -- ^ Represents the properties of the export.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeExportResponse' value with any optional fields omitted.
mkDescribeExportResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeExportResponse
mkDescribeExportResponse responseStatus
  = DescribeExportResponse'{exportDescription = Core.Nothing,
                            responseStatus}

-- | Represents the properties of the export.
--
-- /Note:/ Consider using 'exportDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derfrsExportDescription :: Lens.Lens' DescribeExportResponse (Core.Maybe Types.ExportDescription)
derfrsExportDescription = Lens.field @"exportDescription"
{-# INLINEABLE derfrsExportDescription #-}
{-# DEPRECATED exportDescription "Use generic-lens or generic-optics with 'exportDescription' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derfrsResponseStatus :: Lens.Lens' DescribeExportResponse Core.Int
derfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE derfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
