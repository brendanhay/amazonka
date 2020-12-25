{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.ExportSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Exports an Amazon Lightsail instance or block storage disk snapshot to Amazon Elastic Compute Cloud (Amazon EC2). This operation results in an export snapshot record that can be used with the @create cloud formation stack@ operation to create new Amazon EC2 instances.
--
-- Exported instance snapshots appear in Amazon EC2 as Amazon Machine Images (AMIs), and the instance system disk appears as an Amazon Elastic Block Store (Amazon EBS) volume. Exported disk snapshots appear in Amazon EC2 as Amazon EBS volumes. Snapshots are exported to the same Amazon Web Services Region in Amazon EC2 as the source Lightsail snapshot.
--
-- The @export snapshot@ operation supports tag-based access control via resource tags applied to the resource identified by @source snapshot name@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.ExportSnapshot
  ( -- * Creating a request
    ExportSnapshot (..),
    mkExportSnapshot,

    -- ** Request lenses
    esSourceSnapshotName,

    -- * Destructuring the response
    ExportSnapshotResponse (..),
    mkExportSnapshotResponse,

    -- ** Response lenses
    esrrsOperations,
    esrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkExportSnapshot' smart constructor.
newtype ExportSnapshot = ExportSnapshot'
  { -- | The name of the instance or disk snapshot to be exported to Amazon EC2.
    sourceSnapshotName :: Types.ResourceName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ExportSnapshot' value with any optional fields omitted.
mkExportSnapshot ::
  -- | 'sourceSnapshotName'
  Types.ResourceName ->
  ExportSnapshot
mkExportSnapshot sourceSnapshotName =
  ExportSnapshot' {sourceSnapshotName}

-- | The name of the instance or disk snapshot to be exported to Amazon EC2.
--
-- /Note:/ Consider using 'sourceSnapshotName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esSourceSnapshotName :: Lens.Lens' ExportSnapshot Types.ResourceName
esSourceSnapshotName = Lens.field @"sourceSnapshotName"
{-# DEPRECATED esSourceSnapshotName "Use generic-lens or generic-optics with 'sourceSnapshotName' instead." #-}

instance Core.FromJSON ExportSnapshot where
  toJSON ExportSnapshot {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("sourceSnapshotName" Core..= sourceSnapshotName)]
      )

instance Core.AWSRequest ExportSnapshot where
  type Rs ExportSnapshot = ExportSnapshotResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Lightsail_20161128.ExportSnapshot")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ExportSnapshotResponse'
            Core.<$> (x Core..:? "operations") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkExportSnapshotResponse' smart constructor.
data ExportSnapshotResponse = ExportSnapshotResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operations :: Core.Maybe [Types.Operation],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ExportSnapshotResponse' value with any optional fields omitted.
mkExportSnapshotResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ExportSnapshotResponse
mkExportSnapshotResponse responseStatus =
  ExportSnapshotResponse'
    { operations = Core.Nothing,
      responseStatus
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esrrsOperations :: Lens.Lens' ExportSnapshotResponse (Core.Maybe [Types.Operation])
esrrsOperations = Lens.field @"operations"
{-# DEPRECATED esrrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esrrsResponseStatus :: Lens.Lens' ExportSnapshotResponse Core.Int
esrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED esrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
