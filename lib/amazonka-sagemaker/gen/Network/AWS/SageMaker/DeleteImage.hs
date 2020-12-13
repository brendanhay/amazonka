{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DeleteImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a SageMaker image and all versions of the image. The container images aren't deleted.
module Network.AWS.SageMaker.DeleteImage
  ( -- * Creating a request
    DeleteImage (..),
    mkDeleteImage,

    -- ** Request lenses
    diImageName,

    -- * Destructuring the response
    DeleteImageResponse (..),
    mkDeleteImageResponse,

    -- ** Response lenses
    dirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkDeleteImage' smart constructor.
newtype DeleteImage = DeleteImage'
  { -- | The name of the image to delete.
    imageName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteImage' with the minimum fields required to make a request.
--
-- * 'imageName' - The name of the image to delete.
mkDeleteImage ::
  -- | 'imageName'
  Lude.Text ->
  DeleteImage
mkDeleteImage pImageName_ = DeleteImage' {imageName = pImageName_}

-- | The name of the image to delete.
--
-- /Note:/ Consider using 'imageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diImageName :: Lens.Lens' DeleteImage Lude.Text
diImageName = Lens.lens (imageName :: DeleteImage -> Lude.Text) (\s a -> s {imageName = a} :: DeleteImage)
{-# DEPRECATED diImageName "Use generic-lens or generic-optics with 'imageName' instead." #-}

instance Lude.AWSRequest DeleteImage where
  type Rs DeleteImage = DeleteImageResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteImageResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteImage where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.DeleteImage" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteImage where
  toJSON DeleteImage' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("ImageName" Lude..= imageName)])

instance Lude.ToPath DeleteImage where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteImage where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteImageResponse' smart constructor.
newtype DeleteImageResponse = DeleteImageResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteImageResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteImageResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteImageResponse
mkDeleteImageResponse pResponseStatus_ =
  DeleteImageResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirsResponseStatus :: Lens.Lens' DeleteImageResponse Lude.Int
dirsResponseStatus = Lens.lens (responseStatus :: DeleteImageResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteImageResponse)
{-# DEPRECATED dirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
