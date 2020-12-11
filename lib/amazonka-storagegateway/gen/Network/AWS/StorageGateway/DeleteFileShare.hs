{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DeleteFileShare
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a file share from a file gateway. This operation is only supported for file gateways.
module Network.AWS.StorageGateway.DeleteFileShare
  ( -- * Creating a request
    DeleteFileShare (..),
    mkDeleteFileShare,

    -- ** Request lenses
    dfsForceDelete,
    dfsFileShareARN,

    -- * Destructuring the response
    DeleteFileShareResponse (..),
    mkDeleteFileShareResponse,

    -- ** Response lenses
    dfsrsFileShareARN,
    dfsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | DeleteFileShareInput
--
-- /See:/ 'mkDeleteFileShare' smart constructor.
data DeleteFileShare = DeleteFileShare'
  { forceDelete ::
      Lude.Maybe Lude.Bool,
    fileShareARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteFileShare' with the minimum fields required to make a request.
--
-- * 'fileShareARN' - The Amazon Resource Name (ARN) of the file share to be deleted.
-- * 'forceDelete' - If this value is set to @true@ , the operation deletes a file share immediately and aborts all data uploads to AWS. Otherwise, the file share is not deleted until all data is uploaded to AWS. This process aborts the data upload process, and the file share enters the @FORCE_DELETING@ status.
--
-- Valid Values: @true@ | @false@
mkDeleteFileShare ::
  -- | 'fileShareARN'
  Lude.Text ->
  DeleteFileShare
mkDeleteFileShare pFileShareARN_ =
  DeleteFileShare'
    { forceDelete = Lude.Nothing,
      fileShareARN = pFileShareARN_
    }

-- | If this value is set to @true@ , the operation deletes a file share immediately and aborts all data uploads to AWS. Otherwise, the file share is not deleted until all data is uploaded to AWS. This process aborts the data upload process, and the file share enters the @FORCE_DELETING@ status.
--
-- Valid Values: @true@ | @false@
--
-- /Note:/ Consider using 'forceDelete' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsForceDelete :: Lens.Lens' DeleteFileShare (Lude.Maybe Lude.Bool)
dfsForceDelete = Lens.lens (forceDelete :: DeleteFileShare -> Lude.Maybe Lude.Bool) (\s a -> s {forceDelete = a} :: DeleteFileShare)
{-# DEPRECATED dfsForceDelete "Use generic-lens or generic-optics with 'forceDelete' instead." #-}

-- | The Amazon Resource Name (ARN) of the file share to be deleted.
--
-- /Note:/ Consider using 'fileShareARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsFileShareARN :: Lens.Lens' DeleteFileShare Lude.Text
dfsFileShareARN = Lens.lens (fileShareARN :: DeleteFileShare -> Lude.Text) (\s a -> s {fileShareARN = a} :: DeleteFileShare)
{-# DEPRECATED dfsFileShareARN "Use generic-lens or generic-optics with 'fileShareARN' instead." #-}

instance Lude.AWSRequest DeleteFileShare where
  type Rs DeleteFileShare = DeleteFileShareResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteFileShareResponse'
            Lude.<$> (x Lude..?> "FileShareARN") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteFileShare where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("StorageGateway_20130630.DeleteFileShare" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteFileShare where
  toJSON DeleteFileShare' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ForceDelete" Lude..=) Lude.<$> forceDelete,
            Lude.Just ("FileShareARN" Lude..= fileShareARN)
          ]
      )

instance Lude.ToPath DeleteFileShare where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteFileShare where
  toQuery = Lude.const Lude.mempty

-- | DeleteFileShareOutput
--
-- /See:/ 'mkDeleteFileShareResponse' smart constructor.
data DeleteFileShareResponse = DeleteFileShareResponse'
  { fileShareARN ::
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

-- | Creates a value of 'DeleteFileShareResponse' with the minimum fields required to make a request.
--
-- * 'fileShareARN' - The Amazon Resource Name (ARN) of the deleted file share.
-- * 'responseStatus' - The response status code.
mkDeleteFileShareResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteFileShareResponse
mkDeleteFileShareResponse pResponseStatus_ =
  DeleteFileShareResponse'
    { fileShareARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the deleted file share.
--
-- /Note:/ Consider using 'fileShareARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrsFileShareARN :: Lens.Lens' DeleteFileShareResponse (Lude.Maybe Lude.Text)
dfsrsFileShareARN = Lens.lens (fileShareARN :: DeleteFileShareResponse -> Lude.Maybe Lude.Text) (\s a -> s {fileShareARN = a} :: DeleteFileShareResponse)
{-# DEPRECATED dfsrsFileShareARN "Use generic-lens or generic-optics with 'fileShareARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrsResponseStatus :: Lens.Lens' DeleteFileShareResponse Lude.Int
dfsrsResponseStatus = Lens.lens (responseStatus :: DeleteFileShareResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteFileShareResponse)
{-# DEPRECATED dfsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
