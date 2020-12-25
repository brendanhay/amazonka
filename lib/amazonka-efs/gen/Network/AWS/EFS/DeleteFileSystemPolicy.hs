{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.DeleteFileSystemPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the @FileSystemPolicy@ for the specified file system. The default @FileSystemPolicy@ goes into effect once the existing policy is deleted. For more information about the default file system policy, see <https://docs.aws.amazon.com/efs/latest/ug/res-based-policies-efs.html Using Resource-based Policies with EFS> .
--
-- This operation requires permissions for the @elasticfilesystem:DeleteFileSystemPolicy@ action.
module Network.AWS.EFS.DeleteFileSystemPolicy
  ( -- * Creating a request
    DeleteFileSystemPolicy (..),
    mkDeleteFileSystemPolicy,

    -- ** Request lenses
    dfspFileSystemId,

    -- * Destructuring the response
    DeleteFileSystemPolicyResponse (..),
    mkDeleteFileSystemPolicyResponse,
  )
where

import qualified Network.AWS.EFS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteFileSystemPolicy' smart constructor.
newtype DeleteFileSystemPolicy = DeleteFileSystemPolicy'
  { -- | Specifies the EFS file system for which to delete the @FileSystemPolicy@ .
    fileSystemId :: Types.FileSystemId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFileSystemPolicy' value with any optional fields omitted.
mkDeleteFileSystemPolicy ::
  -- | 'fileSystemId'
  Types.FileSystemId ->
  DeleteFileSystemPolicy
mkDeleteFileSystemPolicy fileSystemId =
  DeleteFileSystemPolicy' {fileSystemId}

-- | Specifies the EFS file system for which to delete the @FileSystemPolicy@ .
--
-- /Note:/ Consider using 'fileSystemId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfspFileSystemId :: Lens.Lens' DeleteFileSystemPolicy Types.FileSystemId
dfspFileSystemId = Lens.field @"fileSystemId"
{-# DEPRECATED dfspFileSystemId "Use generic-lens or generic-optics with 'fileSystemId' instead." #-}

instance Core.AWSRequest DeleteFileSystemPolicy where
  type Rs DeleteFileSystemPolicy = DeleteFileSystemPolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ( "/2015-02-01/file-systems/" Core.<> (Core.toText fileSystemId)
                Core.<> ("/policy")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response = Response.receiveNull DeleteFileSystemPolicyResponse'

-- | /See:/ 'mkDeleteFileSystemPolicyResponse' smart constructor.
data DeleteFileSystemPolicyResponse = DeleteFileSystemPolicyResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFileSystemPolicyResponse' value with any optional fields omitted.
mkDeleteFileSystemPolicyResponse ::
  DeleteFileSystemPolicyResponse
mkDeleteFileSystemPolicyResponse = DeleteFileSystemPolicyResponse'
