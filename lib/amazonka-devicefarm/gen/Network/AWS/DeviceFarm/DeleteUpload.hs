{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.DeleteUpload
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an upload given the upload ARN.
module Network.AWS.DeviceFarm.DeleteUpload
  ( -- * Creating a request
    DeleteUpload (..),
    mkDeleteUpload,

    -- ** Request lenses
    duArn,

    -- * Destructuring the response
    DeleteUploadResponse (..),
    mkDeleteUploadResponse,

    -- ** Response lenses
    dursResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents a request to the delete upload operation.
--
-- /See:/ 'mkDeleteUpload' smart constructor.
newtype DeleteUpload = DeleteUpload' {arn :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteUpload' with the minimum fields required to make a request.
--
-- * 'arn' - Represents the Amazon Resource Name (ARN) of the Device Farm upload to delete.
mkDeleteUpload ::
  -- | 'arn'
  Lude.Text ->
  DeleteUpload
mkDeleteUpload pArn_ = DeleteUpload' {arn = pArn_}

-- | Represents the Amazon Resource Name (ARN) of the Device Farm upload to delete.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duArn :: Lens.Lens' DeleteUpload Lude.Text
duArn = Lens.lens (arn :: DeleteUpload -> Lude.Text) (\s a -> s {arn = a} :: DeleteUpload)
{-# DEPRECATED duArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Lude.AWSRequest DeleteUpload where
  type Rs DeleteUpload = DeleteUploadResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteUploadResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteUpload where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.DeleteUpload" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteUpload where
  toJSON DeleteUpload' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("arn" Lude..= arn)])

instance Lude.ToPath DeleteUpload where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteUpload where
  toQuery = Lude.const Lude.mempty

-- | Represents the result of a delete upload request.
--
-- /See:/ 'mkDeleteUploadResponse' smart constructor.
newtype DeleteUploadResponse = DeleteUploadResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteUploadResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteUploadResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteUploadResponse
mkDeleteUploadResponse pResponseStatus_ =
  DeleteUploadResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dursResponseStatus :: Lens.Lens' DeleteUploadResponse Lude.Int
dursResponseStatus = Lens.lens (responseStatus :: DeleteUploadResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteUploadResponse)
{-# DEPRECATED dursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
