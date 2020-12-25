{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DescribeExport (..),
    mkDescribeExport,

    -- ** Request lenses
    deExportArn,

    -- * Destructuring the response
    DescribeExportResponse (..),
    mkDescribeExportResponse,

    -- ** Response lenses
    derfrsExportDescription,
    derfrsResponseStatus,
  )
where

import qualified Network.AWS.DynamoDB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeExport' smart constructor.
newtype DescribeExport = DescribeExport'
  { -- | The Amazon Resource Name (ARN) associated with the export.
    exportArn :: Types.ExportArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeExport' value with any optional fields omitted.
mkDescribeExport ::
  -- | 'exportArn'
  Types.ExportArn ->
  DescribeExport
mkDescribeExport exportArn = DescribeExport' {exportArn}

-- | The Amazon Resource Name (ARN) associated with the export.
--
-- /Note:/ Consider using 'exportArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deExportArn :: Lens.Lens' DescribeExport Types.ExportArn
deExportArn = Lens.field @"exportArn"
{-# DEPRECATED deExportArn "Use generic-lens or generic-optics with 'exportArn' instead." #-}

instance Core.FromJSON DescribeExport where
  toJSON DescribeExport {..} =
    Core.object
      (Core.catMaybes [Core.Just ("ExportArn" Core..= exportArn)])

instance Core.AWSRequest DescribeExport where
  type Rs DescribeExport = DescribeExportResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "DynamoDB_20120810.DescribeExport")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.0")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeExportResponse'
            Core.<$> (x Core..:? "ExportDescription")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeExportResponse' smart constructor.
data DescribeExportResponse = DescribeExportResponse'
  { -- | Represents the properties of the export.
    exportDescription :: Core.Maybe Types.ExportDescription,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeExportResponse' value with any optional fields omitted.
mkDescribeExportResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeExportResponse
mkDescribeExportResponse responseStatus =
  DescribeExportResponse'
    { exportDescription = Core.Nothing,
      responseStatus
    }

-- | Represents the properties of the export.
--
-- /Note:/ Consider using 'exportDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derfrsExportDescription :: Lens.Lens' DescribeExportResponse (Core.Maybe Types.ExportDescription)
derfrsExportDescription = Lens.field @"exportDescription"
{-# DEPRECATED derfrsExportDescription "Use generic-lens or generic-optics with 'exportDescription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derfrsResponseStatus :: Lens.Lens' DescribeExportResponse Core.Int
derfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED derfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
