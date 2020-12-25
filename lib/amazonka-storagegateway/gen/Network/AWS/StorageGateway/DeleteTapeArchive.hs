{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DeleteTapeArchive
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified virtual tape from the virtual tape shelf (VTS). This operation is only supported in the tape gateway type.
module Network.AWS.StorageGateway.DeleteTapeArchive
  ( -- * Creating a request
    DeleteTapeArchive (..),
    mkDeleteTapeArchive,

    -- ** Request lenses
    dtaTapeARN,
    dtaBypassGovernanceRetention,

    -- * Destructuring the response
    DeleteTapeArchiveResponse (..),
    mkDeleteTapeArchiveResponse,

    -- ** Response lenses
    dtarfrsTapeARN,
    dtarfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | DeleteTapeArchiveInput
--
-- /See:/ 'mkDeleteTapeArchive' smart constructor.
data DeleteTapeArchive = DeleteTapeArchive'
  { -- | The Amazon Resource Name (ARN) of the virtual tape to delete from the virtual tape shelf (VTS).
    tapeARN :: Types.TapeARN,
    -- | Set to @TRUE@ to delete an archived tape that belongs to a custom pool with tape retention lock. Only archived tapes with tape retention lock set to @governance@ can be deleted. Archived tapes with tape retention lock set to @compliance@ can't be deleted.
    bypassGovernanceRetention :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTapeArchive' value with any optional fields omitted.
mkDeleteTapeArchive ::
  -- | 'tapeARN'
  Types.TapeARN ->
  DeleteTapeArchive
mkDeleteTapeArchive tapeARN =
  DeleteTapeArchive'
    { tapeARN,
      bypassGovernanceRetention = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the virtual tape to delete from the virtual tape shelf (VTS).
--
-- /Note:/ Consider using 'tapeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtaTapeARN :: Lens.Lens' DeleteTapeArchive Types.TapeARN
dtaTapeARN = Lens.field @"tapeARN"
{-# DEPRECATED dtaTapeARN "Use generic-lens or generic-optics with 'tapeARN' instead." #-}

-- | Set to @TRUE@ to delete an archived tape that belongs to a custom pool with tape retention lock. Only archived tapes with tape retention lock set to @governance@ can be deleted. Archived tapes with tape retention lock set to @compliance@ can't be deleted.
--
-- /Note:/ Consider using 'bypassGovernanceRetention' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtaBypassGovernanceRetention :: Lens.Lens' DeleteTapeArchive (Core.Maybe Core.Bool)
dtaBypassGovernanceRetention = Lens.field @"bypassGovernanceRetention"
{-# DEPRECATED dtaBypassGovernanceRetention "Use generic-lens or generic-optics with 'bypassGovernanceRetention' instead." #-}

instance Core.FromJSON DeleteTapeArchive where
  toJSON DeleteTapeArchive {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TapeARN" Core..= tapeARN),
            ("BypassGovernanceRetention" Core..=)
              Core.<$> bypassGovernanceRetention
          ]
      )

instance Core.AWSRequest DeleteTapeArchive where
  type Rs DeleteTapeArchive = DeleteTapeArchiveResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "StorageGateway_20130630.DeleteTapeArchive")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteTapeArchiveResponse'
            Core.<$> (x Core..:? "TapeARN") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | DeleteTapeArchiveOutput
--
-- /See:/ 'mkDeleteTapeArchiveResponse' smart constructor.
data DeleteTapeArchiveResponse = DeleteTapeArchiveResponse'
  { -- | The Amazon Resource Name (ARN) of the virtual tape that was deleted from the virtual tape shelf (VTS).
    tapeARN :: Core.Maybe Types.TapeARN,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTapeArchiveResponse' value with any optional fields omitted.
mkDeleteTapeArchiveResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteTapeArchiveResponse
mkDeleteTapeArchiveResponse responseStatus =
  DeleteTapeArchiveResponse'
    { tapeARN = Core.Nothing,
      responseStatus
    }

-- | The Amazon Resource Name (ARN) of the virtual tape that was deleted from the virtual tape shelf (VTS).
--
-- /Note:/ Consider using 'tapeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtarfrsTapeARN :: Lens.Lens' DeleteTapeArchiveResponse (Core.Maybe Types.TapeARN)
dtarfrsTapeARN = Lens.field @"tapeARN"
{-# DEPRECATED dtarfrsTapeARN "Use generic-lens or generic-optics with 'tapeARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtarfrsResponseStatus :: Lens.Lens' DeleteTapeArchiveResponse Core.Int
dtarfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtarfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
