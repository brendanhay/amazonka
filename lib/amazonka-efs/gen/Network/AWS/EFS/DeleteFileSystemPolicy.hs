{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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

import Network.AWS.EFS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteFileSystemPolicy' smart constructor.
newtype DeleteFileSystemPolicy = DeleteFileSystemPolicy'
  { fileSystemId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteFileSystemPolicy' with the minimum fields required to make a request.
--
-- * 'fileSystemId' - Specifies the EFS file system for which to delete the @FileSystemPolicy@ .
mkDeleteFileSystemPolicy ::
  -- | 'fileSystemId'
  Lude.Text ->
  DeleteFileSystemPolicy
mkDeleteFileSystemPolicy pFileSystemId_ =
  DeleteFileSystemPolicy' {fileSystemId = pFileSystemId_}

-- | Specifies the EFS file system for which to delete the @FileSystemPolicy@ .
--
-- /Note:/ Consider using 'fileSystemId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfspFileSystemId :: Lens.Lens' DeleteFileSystemPolicy Lude.Text
dfspFileSystemId = Lens.lens (fileSystemId :: DeleteFileSystemPolicy -> Lude.Text) (\s a -> s {fileSystemId = a} :: DeleteFileSystemPolicy)
{-# DEPRECATED dfspFileSystemId "Use generic-lens or generic-optics with 'fileSystemId' instead." #-}

instance Lude.AWSRequest DeleteFileSystemPolicy where
  type Rs DeleteFileSystemPolicy = DeleteFileSystemPolicyResponse
  request = Req.delete efsService
  response = Res.receiveNull DeleteFileSystemPolicyResponse'

instance Lude.ToHeaders DeleteFileSystemPolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteFileSystemPolicy where
  toPath DeleteFileSystemPolicy' {..} =
    Lude.mconcat
      ["/2015-02-01/file-systems/", Lude.toBS fileSystemId, "/policy"]

instance Lude.ToQuery DeleteFileSystemPolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteFileSystemPolicyResponse' smart constructor.
data DeleteFileSystemPolicyResponse = DeleteFileSystemPolicyResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteFileSystemPolicyResponse' with the minimum fields required to make a request.
mkDeleteFileSystemPolicyResponse ::
  DeleteFileSystemPolicyResponse
mkDeleteFileSystemPolicyResponse = DeleteFileSystemPolicyResponse'
