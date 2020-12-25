{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.AcceptSharedDirectory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts a directory sharing request that was sent from the directory owner account.
module Network.AWS.DirectoryService.AcceptSharedDirectory
  ( -- * Creating a request
    AcceptSharedDirectory (..),
    mkAcceptSharedDirectory,

    -- ** Request lenses
    asdSharedDirectoryId,

    -- * Destructuring the response
    AcceptSharedDirectoryResponse (..),
    mkAcceptSharedDirectoryResponse,

    -- ** Response lenses
    asdrrsSharedDirectory,
    asdrrsResponseStatus,
  )
where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAcceptSharedDirectory' smart constructor.
newtype AcceptSharedDirectory = AcceptSharedDirectory'
  { -- | Identifier of the shared directory in the directory consumer account. This identifier is different for each directory owner account.
    sharedDirectoryId :: Types.DirectoryId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AcceptSharedDirectory' value with any optional fields omitted.
mkAcceptSharedDirectory ::
  -- | 'sharedDirectoryId'
  Types.DirectoryId ->
  AcceptSharedDirectory
mkAcceptSharedDirectory sharedDirectoryId =
  AcceptSharedDirectory' {sharedDirectoryId}

-- | Identifier of the shared directory in the directory consumer account. This identifier is different for each directory owner account.
--
-- /Note:/ Consider using 'sharedDirectoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asdSharedDirectoryId :: Lens.Lens' AcceptSharedDirectory Types.DirectoryId
asdSharedDirectoryId = Lens.field @"sharedDirectoryId"
{-# DEPRECATED asdSharedDirectoryId "Use generic-lens or generic-optics with 'sharedDirectoryId' instead." #-}

instance Core.FromJSON AcceptSharedDirectory where
  toJSON AcceptSharedDirectory {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("SharedDirectoryId" Core..= sharedDirectoryId)]
      )

instance Core.AWSRequest AcceptSharedDirectory where
  type Rs AcceptSharedDirectory = AcceptSharedDirectoryResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "DirectoryService_20150416.AcceptSharedDirectory")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          AcceptSharedDirectoryResponse'
            Core.<$> (x Core..:? "SharedDirectory")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkAcceptSharedDirectoryResponse' smart constructor.
data AcceptSharedDirectoryResponse = AcceptSharedDirectoryResponse'
  { -- | The shared directory in the directory consumer account.
    sharedDirectory :: Core.Maybe Types.SharedDirectory,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'AcceptSharedDirectoryResponse' value with any optional fields omitted.
mkAcceptSharedDirectoryResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AcceptSharedDirectoryResponse
mkAcceptSharedDirectoryResponse responseStatus =
  AcceptSharedDirectoryResponse'
    { sharedDirectory = Core.Nothing,
      responseStatus
    }

-- | The shared directory in the directory consumer account.
--
-- /Note:/ Consider using 'sharedDirectory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asdrrsSharedDirectory :: Lens.Lens' AcceptSharedDirectoryResponse (Core.Maybe Types.SharedDirectory)
asdrrsSharedDirectory = Lens.field @"sharedDirectory"
{-# DEPRECATED asdrrsSharedDirectory "Use generic-lens or generic-optics with 'sharedDirectory' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asdrrsResponseStatus :: Lens.Lens' AcceptSharedDirectoryResponse Core.Int
asdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED asdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
