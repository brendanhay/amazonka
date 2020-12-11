{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.DeleteDirectory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a directory. Only disabled directories can be deleted. A deleted directory cannot be undone. Exercise extreme caution when deleting directories.
module Network.AWS.CloudDirectory.DeleteDirectory
  ( -- * Creating a request
    DeleteDirectory (..),
    mkDeleteDirectory,

    -- ** Request lenses
    delDirectoryARN,

    -- * Destructuring the response
    DeleteDirectoryResponse (..),
    mkDeleteDirectoryResponse,

    -- ** Response lenses
    ddrsResponseStatus,
    ddrsDirectoryARN,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteDirectory' smart constructor.
newtype DeleteDirectory = DeleteDirectory'
  { directoryARN ::
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
-- * 'directoryARN' - The ARN of the directory to delete.
mkDeleteDirectory ::
  -- | 'directoryARN'
  Lude.Text ->
  DeleteDirectory
mkDeleteDirectory pDirectoryARN_ =
  DeleteDirectory' {directoryARN = pDirectoryARN_}

-- | The ARN of the directory to delete.
--
-- /Note:/ Consider using 'directoryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delDirectoryARN :: Lens.Lens' DeleteDirectory Lude.Text
delDirectoryARN = Lens.lens (directoryARN :: DeleteDirectory -> Lude.Text) (\s a -> s {directoryARN = a} :: DeleteDirectory)
{-# DEPRECATED delDirectoryARN "Use generic-lens or generic-optics with 'directoryARN' instead." #-}

instance Lude.AWSRequest DeleteDirectory where
  type Rs DeleteDirectory = DeleteDirectoryResponse
  request = Req.putJSON cloudDirectoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteDirectoryResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (x Lude..:> "DirectoryArn")
      )

instance Lude.ToHeaders DeleteDirectory where
  toHeaders DeleteDirectory' {..} =
    Lude.mconcat ["x-amz-data-partition" Lude.=# directoryARN]

instance Lude.ToJSON DeleteDirectory where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath DeleteDirectory where
  toPath = Lude.const "/amazonclouddirectory/2017-01-11/directory"

instance Lude.ToQuery DeleteDirectory where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteDirectoryResponse' smart constructor.
data DeleteDirectoryResponse = DeleteDirectoryResponse'
  { responseStatus ::
      Lude.Int,
    directoryARN :: Lude.Text
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
-- * 'directoryARN' - The ARN of the deleted directory.
-- * 'responseStatus' - The response status code.
mkDeleteDirectoryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'directoryARN'
  Lude.Text ->
  DeleteDirectoryResponse
mkDeleteDirectoryResponse pResponseStatus_ pDirectoryARN_ =
  DeleteDirectoryResponse'
    { responseStatus = pResponseStatus_,
      directoryARN = pDirectoryARN_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrsResponseStatus :: Lens.Lens' DeleteDirectoryResponse Lude.Int
ddrsResponseStatus = Lens.lens (responseStatus :: DeleteDirectoryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteDirectoryResponse)
{-# DEPRECATED ddrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The ARN of the deleted directory.
--
-- /Note:/ Consider using 'directoryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrsDirectoryARN :: Lens.Lens' DeleteDirectoryResponse Lude.Text
ddrsDirectoryARN = Lens.lens (directoryARN :: DeleteDirectoryResponse -> Lude.Text) (\s a -> s {directoryARN = a} :: DeleteDirectoryResponse)
{-# DEPRECATED ddrsDirectoryARN "Use generic-lens or generic-optics with 'directoryARN' instead." #-}
