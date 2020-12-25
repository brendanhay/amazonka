{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.RejectSharedDirectory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rejects a directory sharing request that was sent from the directory owner account.
module Network.AWS.DirectoryService.RejectSharedDirectory
  ( -- * Creating a request
    RejectSharedDirectory (..),
    mkRejectSharedDirectory,

    -- ** Request lenses
    rsdSharedDirectoryId,

    -- * Destructuring the response
    RejectSharedDirectoryResponse (..),
    mkRejectSharedDirectoryResponse,

    -- ** Response lenses
    rsdrrsSharedDirectoryId,
    rsdrrsResponseStatus,
  )
where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRejectSharedDirectory' smart constructor.
newtype RejectSharedDirectory = RejectSharedDirectory'
  { -- | Identifier of the shared directory in the directory consumer account. This identifier is different for each directory owner account.
    sharedDirectoryId :: Types.DirectoryId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RejectSharedDirectory' value with any optional fields omitted.
mkRejectSharedDirectory ::
  -- | 'sharedDirectoryId'
  Types.DirectoryId ->
  RejectSharedDirectory
mkRejectSharedDirectory sharedDirectoryId =
  RejectSharedDirectory' {sharedDirectoryId}

-- | Identifier of the shared directory in the directory consumer account. This identifier is different for each directory owner account.
--
-- /Note:/ Consider using 'sharedDirectoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsdSharedDirectoryId :: Lens.Lens' RejectSharedDirectory Types.DirectoryId
rsdSharedDirectoryId = Lens.field @"sharedDirectoryId"
{-# DEPRECATED rsdSharedDirectoryId "Use generic-lens or generic-optics with 'sharedDirectoryId' instead." #-}

instance Core.FromJSON RejectSharedDirectory where
  toJSON RejectSharedDirectory {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("SharedDirectoryId" Core..= sharedDirectoryId)]
      )

instance Core.AWSRequest RejectSharedDirectory where
  type Rs RejectSharedDirectory = RejectSharedDirectoryResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "DirectoryService_20150416.RejectSharedDirectory")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          RejectSharedDirectoryResponse'
            Core.<$> (x Core..:? "SharedDirectoryId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRejectSharedDirectoryResponse' smart constructor.
data RejectSharedDirectoryResponse = RejectSharedDirectoryResponse'
  { -- | Identifier of the shared directory in the directory consumer account.
    sharedDirectoryId :: Core.Maybe Types.DirectoryId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RejectSharedDirectoryResponse' value with any optional fields omitted.
mkRejectSharedDirectoryResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RejectSharedDirectoryResponse
mkRejectSharedDirectoryResponse responseStatus =
  RejectSharedDirectoryResponse'
    { sharedDirectoryId = Core.Nothing,
      responseStatus
    }

-- | Identifier of the shared directory in the directory consumer account.
--
-- /Note:/ Consider using 'sharedDirectoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsdrrsSharedDirectoryId :: Lens.Lens' RejectSharedDirectoryResponse (Core.Maybe Types.DirectoryId)
rsdrrsSharedDirectoryId = Lens.field @"sharedDirectoryId"
{-# DEPRECATED rsdrrsSharedDirectoryId "Use generic-lens or generic-optics with 'sharedDirectoryId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsdrrsResponseStatus :: Lens.Lens' RejectSharedDirectoryResponse Core.Int
rsdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rsdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
