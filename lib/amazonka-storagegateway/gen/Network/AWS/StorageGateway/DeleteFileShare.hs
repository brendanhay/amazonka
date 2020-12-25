{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DeleteFileShare
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a file share from a file gateway. This operation is only supported for file gateways.
module Network.AWS.StorageGateway.DeleteFileShare
  ( -- * Creating a request
    DeleteFileShare (..),
    mkDeleteFileShare,

    -- ** Request lenses
    dfsFileShareARN,
    dfsForceDelete,

    -- * Destructuring the response
    DeleteFileShareResponse (..),
    mkDeleteFileShareResponse,

    -- ** Response lenses
    dfsrrsFileShareARN,
    dfsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | DeleteFileShareInput
--
-- /See:/ 'mkDeleteFileShare' smart constructor.
data DeleteFileShare = DeleteFileShare'
  { -- | The Amazon Resource Name (ARN) of the file share to be deleted.
    fileShareARN :: Types.FileShareARN,
    -- | If this value is set to @true@ , the operation deletes a file share immediately and aborts all data uploads to AWS. Otherwise, the file share is not deleted until all data is uploaded to AWS. This process aborts the data upload process, and the file share enters the @FORCE_DELETING@ status.
    --
    -- Valid Values: @true@ | @false@
    forceDelete :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFileShare' value with any optional fields omitted.
mkDeleteFileShare ::
  -- | 'fileShareARN'
  Types.FileShareARN ->
  DeleteFileShare
mkDeleteFileShare fileShareARN =
  DeleteFileShare' {fileShareARN, forceDelete = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the file share to be deleted.
--
-- /Note:/ Consider using 'fileShareARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsFileShareARN :: Lens.Lens' DeleteFileShare Types.FileShareARN
dfsFileShareARN = Lens.field @"fileShareARN"
{-# DEPRECATED dfsFileShareARN "Use generic-lens or generic-optics with 'fileShareARN' instead." #-}

-- | If this value is set to @true@ , the operation deletes a file share immediately and aborts all data uploads to AWS. Otherwise, the file share is not deleted until all data is uploaded to AWS. This process aborts the data upload process, and the file share enters the @FORCE_DELETING@ status.
--
-- Valid Values: @true@ | @false@
--
-- /Note:/ Consider using 'forceDelete' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsForceDelete :: Lens.Lens' DeleteFileShare (Core.Maybe Core.Bool)
dfsForceDelete = Lens.field @"forceDelete"
{-# DEPRECATED dfsForceDelete "Use generic-lens or generic-optics with 'forceDelete' instead." #-}

instance Core.FromJSON DeleteFileShare where
  toJSON DeleteFileShare {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("FileShareARN" Core..= fileShareARN),
            ("ForceDelete" Core..=) Core.<$> forceDelete
          ]
      )

instance Core.AWSRequest DeleteFileShare where
  type Rs DeleteFileShare = DeleteFileShareResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "StorageGateway_20130630.DeleteFileShare")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteFileShareResponse'
            Core.<$> (x Core..:? "FileShareARN") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | DeleteFileShareOutput
--
-- /See:/ 'mkDeleteFileShareResponse' smart constructor.
data DeleteFileShareResponse = DeleteFileShareResponse'
  { -- | The Amazon Resource Name (ARN) of the deleted file share.
    fileShareARN :: Core.Maybe Types.FileShareARN,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFileShareResponse' value with any optional fields omitted.
mkDeleteFileShareResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteFileShareResponse
mkDeleteFileShareResponse responseStatus =
  DeleteFileShareResponse'
    { fileShareARN = Core.Nothing,
      responseStatus
    }

-- | The Amazon Resource Name (ARN) of the deleted file share.
--
-- /Note:/ Consider using 'fileShareARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrrsFileShareARN :: Lens.Lens' DeleteFileShareResponse (Core.Maybe Types.FileShareARN)
dfsrrsFileShareARN = Lens.field @"fileShareARN"
{-# DEPRECATED dfsrrsFileShareARN "Use generic-lens or generic-optics with 'fileShareARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrrsResponseStatus :: Lens.Lens' DeleteFileShareResponse Core.Int
dfsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dfsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
