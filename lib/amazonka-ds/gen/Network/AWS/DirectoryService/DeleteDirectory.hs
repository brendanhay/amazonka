{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.DeleteDirectory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an AWS Directory Service directory.
--
-- Before you call @DeleteDirectory@ , ensure that all of the required permissions have been explicitly granted through a policy. For details about what permissions are required to run the @DeleteDirectory@ operation, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/UsingWithDS_IAM_ResourcePermissions.html AWS Directory Service API Permissions: Actions, Resources, and Conditions Reference> .
module Network.AWS.DirectoryService.DeleteDirectory
  ( -- * Creating a request
    DeleteDirectory (..),
    mkDeleteDirectory,

    -- ** Request lenses
    ddfDirectoryId,

    -- * Destructuring the response
    DeleteDirectoryResponse (..),
    mkDeleteDirectoryResponse,

    -- ** Response lenses
    ddrfrsDirectoryId,
    ddrfrsResponseStatus,
  )
where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the inputs for the 'DeleteDirectory' operation.
--
-- /See:/ 'mkDeleteDirectory' smart constructor.
newtype DeleteDirectory = DeleteDirectory'
  { -- | The identifier of the directory to delete.
    directoryId :: Types.DirectoryId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDirectory' value with any optional fields omitted.
mkDeleteDirectory ::
  -- | 'directoryId'
  Types.DirectoryId ->
  DeleteDirectory
mkDeleteDirectory directoryId = DeleteDirectory' {directoryId}

-- | The identifier of the directory to delete.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddfDirectoryId :: Lens.Lens' DeleteDirectory Types.DirectoryId
ddfDirectoryId = Lens.field @"directoryId"
{-# DEPRECATED ddfDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

instance Core.FromJSON DeleteDirectory where
  toJSON DeleteDirectory {..} =
    Core.object
      (Core.catMaybes [Core.Just ("DirectoryId" Core..= directoryId)])

instance Core.AWSRequest DeleteDirectory where
  type Rs DeleteDirectory = DeleteDirectoryResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "DirectoryService_20150416.DeleteDirectory")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteDirectoryResponse'
            Core.<$> (x Core..:? "DirectoryId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the results of the 'DeleteDirectory' operation.
--
-- /See:/ 'mkDeleteDirectoryResponse' smart constructor.
data DeleteDirectoryResponse = DeleteDirectoryResponse'
  { -- | The directory identifier.
    directoryId :: Core.Maybe Types.DirectoryId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDirectoryResponse' value with any optional fields omitted.
mkDeleteDirectoryResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteDirectoryResponse
mkDeleteDirectoryResponse responseStatus =
  DeleteDirectoryResponse'
    { directoryId = Core.Nothing,
      responseStatus
    }

-- | The directory identifier.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrfrsDirectoryId :: Lens.Lens' DeleteDirectoryResponse (Core.Maybe Types.DirectoryId)
ddrfrsDirectoryId = Lens.field @"directoryId"
{-# DEPRECATED ddrfrsDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrfrsResponseStatus :: Lens.Lens' DeleteDirectoryResponse Core.Int
ddrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ddrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
