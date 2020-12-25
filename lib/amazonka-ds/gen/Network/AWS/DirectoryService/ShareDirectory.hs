{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.ShareDirectory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Shares a specified directory (@DirectoryId@ ) in your AWS account (directory owner) with another AWS account (directory consumer). With this operation you can use your directory from any AWS account and from any Amazon VPC within an AWS Region.
--
-- When you share your AWS Managed Microsoft AD directory, AWS Directory Service creates a shared directory in the directory consumer account. This shared directory contains the metadata to provide access to the directory within the directory owner account. The shared directory is visible in all VPCs in the directory consumer account.
-- The @ShareMethod@ parameter determines whether the specified directory can be shared between AWS accounts inside the same AWS organization (@ORGANIZATIONS@ ). It also determines whether you can share the directory with any other AWS account either inside or outside of the organization (@HANDSHAKE@ ).
-- The @ShareNotes@ parameter is only used when @HANDSHAKE@ is called, which sends a directory sharing request to the directory consumer.
module Network.AWS.DirectoryService.ShareDirectory
  ( -- * Creating a request
    ShareDirectory (..),
    mkShareDirectory,

    -- ** Request lenses
    sdfDirectoryId,
    sdfShareTarget,
    sdfShareMethod,
    sdfShareNotes,

    -- * Destructuring the response
    ShareDirectoryResponse (..),
    mkShareDirectoryResponse,

    -- ** Response lenses
    sdrrsSharedDirectoryId,
    sdrrsResponseStatus,
  )
where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkShareDirectory' smart constructor.
data ShareDirectory = ShareDirectory'
  { -- | Identifier of the AWS Managed Microsoft AD directory that you want to share with other AWS accounts.
    directoryId :: Types.DirectoryId,
    -- | Identifier for the directory consumer account with whom the directory is to be shared.
    shareTarget :: Types.ShareTarget,
    -- | The method used when sharing a directory to determine whether the directory should be shared within your AWS organization (@ORGANIZATIONS@ ) or with any AWS account by sending a directory sharing request (@HANDSHAKE@ ).
    shareMethod :: Types.ShareMethod,
    -- | A directory share request that is sent by the directory owner to the directory consumer. The request includes a typed message to help the directory consumer administrator determine whether to approve or reject the share invitation.
    shareNotes :: Core.Maybe Types.ShareNotes
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ShareDirectory' value with any optional fields omitted.
mkShareDirectory ::
  -- | 'directoryId'
  Types.DirectoryId ->
  -- | 'shareTarget'
  Types.ShareTarget ->
  -- | 'shareMethod'
  Types.ShareMethod ->
  ShareDirectory
mkShareDirectory directoryId shareTarget shareMethod =
  ShareDirectory'
    { directoryId,
      shareTarget,
      shareMethod,
      shareNotes = Core.Nothing
    }

-- | Identifier of the AWS Managed Microsoft AD directory that you want to share with other AWS accounts.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdfDirectoryId :: Lens.Lens' ShareDirectory Types.DirectoryId
sdfDirectoryId = Lens.field @"directoryId"
{-# DEPRECATED sdfDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | Identifier for the directory consumer account with whom the directory is to be shared.
--
-- /Note:/ Consider using 'shareTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdfShareTarget :: Lens.Lens' ShareDirectory Types.ShareTarget
sdfShareTarget = Lens.field @"shareTarget"
{-# DEPRECATED sdfShareTarget "Use generic-lens or generic-optics with 'shareTarget' instead." #-}

-- | The method used when sharing a directory to determine whether the directory should be shared within your AWS organization (@ORGANIZATIONS@ ) or with any AWS account by sending a directory sharing request (@HANDSHAKE@ ).
--
-- /Note:/ Consider using 'shareMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdfShareMethod :: Lens.Lens' ShareDirectory Types.ShareMethod
sdfShareMethod = Lens.field @"shareMethod"
{-# DEPRECATED sdfShareMethod "Use generic-lens or generic-optics with 'shareMethod' instead." #-}

-- | A directory share request that is sent by the directory owner to the directory consumer. The request includes a typed message to help the directory consumer administrator determine whether to approve or reject the share invitation.
--
-- /Note:/ Consider using 'shareNotes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdfShareNotes :: Lens.Lens' ShareDirectory (Core.Maybe Types.ShareNotes)
sdfShareNotes = Lens.field @"shareNotes"
{-# DEPRECATED sdfShareNotes "Use generic-lens or generic-optics with 'shareNotes' instead." #-}

instance Core.FromJSON ShareDirectory where
  toJSON ShareDirectory {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DirectoryId" Core..= directoryId),
            Core.Just ("ShareTarget" Core..= shareTarget),
            Core.Just ("ShareMethod" Core..= shareMethod),
            ("ShareNotes" Core..=) Core.<$> shareNotes
          ]
      )

instance Core.AWSRequest ShareDirectory where
  type Rs ShareDirectory = ShareDirectoryResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "DirectoryService_20150416.ShareDirectory")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ShareDirectoryResponse'
            Core.<$> (x Core..:? "SharedDirectoryId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkShareDirectoryResponse' smart constructor.
data ShareDirectoryResponse = ShareDirectoryResponse'
  { -- | Identifier of the directory that is stored in the directory consumer account that is shared from the specified directory (@DirectoryId@ ).
    sharedDirectoryId :: Core.Maybe Types.DirectoryId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ShareDirectoryResponse' value with any optional fields omitted.
mkShareDirectoryResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ShareDirectoryResponse
mkShareDirectoryResponse responseStatus =
  ShareDirectoryResponse'
    { sharedDirectoryId = Core.Nothing,
      responseStatus
    }

-- | Identifier of the directory that is stored in the directory consumer account that is shared from the specified directory (@DirectoryId@ ).
--
-- /Note:/ Consider using 'sharedDirectoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdrrsSharedDirectoryId :: Lens.Lens' ShareDirectoryResponse (Core.Maybe Types.DirectoryId)
sdrrsSharedDirectoryId = Lens.field @"sharedDirectoryId"
{-# DEPRECATED sdrrsSharedDirectoryId "Use generic-lens or generic-optics with 'sharedDirectoryId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdrrsResponseStatus :: Lens.Lens' ShareDirectoryResponse Core.Int
sdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED sdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
