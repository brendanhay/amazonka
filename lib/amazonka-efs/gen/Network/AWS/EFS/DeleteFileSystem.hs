{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.DeleteFileSystem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a file system, permanently severing access to its contents. Upon return, the file system no longer exists and you can't access any contents of the deleted file system.
--
-- You can't delete a file system that is in use. That is, if the file system has any mount targets, you must first delete them. For more information, see 'DescribeMountTargets' and 'DeleteMountTarget' .
-- This operation requires permissions for the @elasticfilesystem:DeleteFileSystem@ action.
module Network.AWS.EFS.DeleteFileSystem
  ( -- * Creating a request
    DeleteFileSystem (..),
    mkDeleteFileSystem,

    -- ** Request lenses
    dFileSystemId,

    -- * Destructuring the response
    DeleteFileSystemResponse (..),
    mkDeleteFileSystemResponse,
  )
where

import Network.AWS.EFS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDeleteFileSystem' smart constructor.
newtype DeleteFileSystem = DeleteFileSystem'
  { -- | The ID of the file system you want to delete.
    fileSystemId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteFileSystem' with the minimum fields required to make a request.
--
-- * 'fileSystemId' - The ID of the file system you want to delete.
mkDeleteFileSystem ::
  -- | 'fileSystemId'
  Lude.Text ->
  DeleteFileSystem
mkDeleteFileSystem pFileSystemId_ =
  DeleteFileSystem' {fileSystemId = pFileSystemId_}

-- | The ID of the file system you want to delete.
--
-- /Note:/ Consider using 'fileSystemId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dFileSystemId :: Lens.Lens' DeleteFileSystem Lude.Text
dFileSystemId = Lens.lens (fileSystemId :: DeleteFileSystem -> Lude.Text) (\s a -> s {fileSystemId = a} :: DeleteFileSystem)
{-# DEPRECATED dFileSystemId "Use generic-lens or generic-optics with 'fileSystemId' instead." #-}

instance Lude.AWSRequest DeleteFileSystem where
  type Rs DeleteFileSystem = DeleteFileSystemResponse
  request = Req.delete efsService
  response = Res.receiveNull DeleteFileSystemResponse'

instance Lude.ToHeaders DeleteFileSystem where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteFileSystem where
  toPath DeleteFileSystem' {..} =
    Lude.mconcat
      ["/2015-02-01/file-systems/", Lude.toBS fileSystemId]

instance Lude.ToQuery DeleteFileSystem where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteFileSystemResponse' smart constructor.
data DeleteFileSystemResponse = DeleteFileSystemResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteFileSystemResponse' with the minimum fields required to make a request.
mkDeleteFileSystemResponse ::
  DeleteFileSystemResponse
mkDeleteFileSystemResponse = DeleteFileSystemResponse'
