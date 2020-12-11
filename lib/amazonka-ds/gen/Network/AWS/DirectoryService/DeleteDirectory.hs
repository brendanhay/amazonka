{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    dddDirectoryId,

    -- * Destructuring the response
    DeleteDirectoryResponse (..),
    mkDeleteDirectoryResponse,

    -- ** Response lenses
    delrsDirectoryId,
    delrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the inputs for the 'DeleteDirectory' operation.
--
-- /See:/ 'mkDeleteDirectory' smart constructor.
newtype DeleteDirectory = DeleteDirectory'
  { directoryId ::
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

-- | Creates a value of 'DeleteDirectory' with the minimum fields required to make a request.
--
-- * 'directoryId' - The identifier of the directory to delete.
mkDeleteDirectory ::
  -- | 'directoryId'
  Lude.Text ->
  DeleteDirectory
mkDeleteDirectory pDirectoryId_ =
  DeleteDirectory' {directoryId = pDirectoryId_}

-- | The identifier of the directory to delete.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dddDirectoryId :: Lens.Lens' DeleteDirectory Lude.Text
dddDirectoryId = Lens.lens (directoryId :: DeleteDirectory -> Lude.Text) (\s a -> s {directoryId = a} :: DeleteDirectory)
{-# DEPRECATED dddDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

instance Lude.AWSRequest DeleteDirectory where
  type Rs DeleteDirectory = DeleteDirectoryResponse
  request = Req.postJSON directoryServiceService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteDirectoryResponse'
            Lude.<$> (x Lude..?> "DirectoryId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteDirectory where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DirectoryService_20150416.DeleteDirectory" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteDirectory where
  toJSON DeleteDirectory' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("DirectoryId" Lude..= directoryId)])

instance Lude.ToPath DeleteDirectory where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteDirectory where
  toQuery = Lude.const Lude.mempty

-- | Contains the results of the 'DeleteDirectory' operation.
--
-- /See:/ 'mkDeleteDirectoryResponse' smart constructor.
data DeleteDirectoryResponse = DeleteDirectoryResponse'
  { directoryId ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDirectoryResponse' with the minimum fields required to make a request.
--
-- * 'directoryId' - The directory identifier.
-- * 'responseStatus' - The response status code.
mkDeleteDirectoryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteDirectoryResponse
mkDeleteDirectoryResponse pResponseStatus_ =
  DeleteDirectoryResponse'
    { directoryId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The directory identifier.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delrsDirectoryId :: Lens.Lens' DeleteDirectoryResponse (Lude.Maybe Lude.Text)
delrsDirectoryId = Lens.lens (directoryId :: DeleteDirectoryResponse -> Lude.Maybe Lude.Text) (\s a -> s {directoryId = a} :: DeleteDirectoryResponse)
{-# DEPRECATED delrsDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delrsResponseStatus :: Lens.Lens' DeleteDirectoryResponse Lude.Int
delrsResponseStatus = Lens.lens (responseStatus :: DeleteDirectoryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteDirectoryResponse)
{-# DEPRECATED delrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
