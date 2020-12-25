{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.UnshareDirectory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the directory sharing between the directory owner and consumer accounts.
module Network.AWS.DirectoryService.UnshareDirectory
  ( -- * Creating a request
    UnshareDirectory (..),
    mkUnshareDirectory,

    -- ** Request lenses
    udDirectoryId,
    udUnshareTarget,

    -- * Destructuring the response
    UnshareDirectoryResponse (..),
    mkUnshareDirectoryResponse,

    -- ** Response lenses
    udrrsSharedDirectoryId,
    udrrsResponseStatus,
  )
where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUnshareDirectory' smart constructor.
data UnshareDirectory = UnshareDirectory'
  { -- | The identifier of the AWS Managed Microsoft AD directory that you want to stop sharing.
    directoryId :: Types.DirectoryId,
    -- | Identifier for the directory consumer account with whom the directory has to be unshared.
    unshareTarget :: Types.UnshareTarget
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UnshareDirectory' value with any optional fields omitted.
mkUnshareDirectory ::
  -- | 'directoryId'
  Types.DirectoryId ->
  -- | 'unshareTarget'
  Types.UnshareTarget ->
  UnshareDirectory
mkUnshareDirectory directoryId unshareTarget =
  UnshareDirectory' {directoryId, unshareTarget}

-- | The identifier of the AWS Managed Microsoft AD directory that you want to stop sharing.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udDirectoryId :: Lens.Lens' UnshareDirectory Types.DirectoryId
udDirectoryId = Lens.field @"directoryId"
{-# DEPRECATED udDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | Identifier for the directory consumer account with whom the directory has to be unshared.
--
-- /Note:/ Consider using 'unshareTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udUnshareTarget :: Lens.Lens' UnshareDirectory Types.UnshareTarget
udUnshareTarget = Lens.field @"unshareTarget"
{-# DEPRECATED udUnshareTarget "Use generic-lens or generic-optics with 'unshareTarget' instead." #-}

instance Core.FromJSON UnshareDirectory where
  toJSON UnshareDirectory {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DirectoryId" Core..= directoryId),
            Core.Just ("UnshareTarget" Core..= unshareTarget)
          ]
      )

instance Core.AWSRequest UnshareDirectory where
  type Rs UnshareDirectory = UnshareDirectoryResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "DirectoryService_20150416.UnshareDirectory")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UnshareDirectoryResponse'
            Core.<$> (x Core..:? "SharedDirectoryId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUnshareDirectoryResponse' smart constructor.
data UnshareDirectoryResponse = UnshareDirectoryResponse'
  { -- | Identifier of the directory stored in the directory consumer account that is to be unshared from the specified directory (@DirectoryId@ ).
    sharedDirectoryId :: Core.Maybe Types.DirectoryId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UnshareDirectoryResponse' value with any optional fields omitted.
mkUnshareDirectoryResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UnshareDirectoryResponse
mkUnshareDirectoryResponse responseStatus =
  UnshareDirectoryResponse'
    { sharedDirectoryId = Core.Nothing,
      responseStatus
    }

-- | Identifier of the directory stored in the directory consumer account that is to be unshared from the specified directory (@DirectoryId@ ).
--
-- /Note:/ Consider using 'sharedDirectoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrrsSharedDirectoryId :: Lens.Lens' UnshareDirectoryResponse (Core.Maybe Types.DirectoryId)
udrrsSharedDirectoryId = Lens.field @"sharedDirectoryId"
{-# DEPRECATED udrrsSharedDirectoryId "Use generic-lens or generic-optics with 'sharedDirectoryId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrrsResponseStatus :: Lens.Lens' UnshareDirectoryResponse Core.Int
udrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED udrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
