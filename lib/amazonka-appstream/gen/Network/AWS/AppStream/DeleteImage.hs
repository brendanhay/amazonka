{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.DeleteImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified image. You cannot delete an image when it is in use. After you delete an image, you cannot provision new capacity using the image.
module Network.AWS.AppStream.DeleteImage
  ( -- * Creating a request
    DeleteImage (..),
    mkDeleteImage,

    -- ** Request lenses
    diName,

    -- * Destructuring the response
    DeleteImageResponse (..),
    mkDeleteImageResponse,

    -- ** Response lenses
    dirsImage,
    dirsResponseStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteImage' smart constructor.
newtype DeleteImage = DeleteImage'
  { -- | The name of the image.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteImage' with the minimum fields required to make a request.
--
-- * 'name' - The name of the image.
mkDeleteImage ::
  -- | 'name'
  Lude.Text ->
  DeleteImage
mkDeleteImage pName_ = DeleteImage' {name = pName_}

-- | The name of the image.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diName :: Lens.Lens' DeleteImage Lude.Text
diName = Lens.lens (name :: DeleteImage -> Lude.Text) (\s a -> s {name = a} :: DeleteImage)
{-# DEPRECATED diName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest DeleteImage where
  type Rs DeleteImage = DeleteImageResponse
  request = Req.postJSON appStreamService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteImageResponse'
            Lude.<$> (x Lude..?> "Image") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteImage where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("PhotonAdminProxyService.DeleteImage" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteImage where
  toJSON DeleteImage' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Name" Lude..= name)])

instance Lude.ToPath DeleteImage where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteImage where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteImageResponse' smart constructor.
data DeleteImageResponse = DeleteImageResponse'
  { -- | Information about the image.
    image :: Lude.Maybe Image,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteImageResponse' with the minimum fields required to make a request.
--
-- * 'image' - Information about the image.
-- * 'responseStatus' - The response status code.
mkDeleteImageResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteImageResponse
mkDeleteImageResponse pResponseStatus_ =
  DeleteImageResponse'
    { image = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the image.
--
-- /Note:/ Consider using 'image' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirsImage :: Lens.Lens' DeleteImageResponse (Lude.Maybe Image)
dirsImage = Lens.lens (image :: DeleteImageResponse -> Lude.Maybe Image) (\s a -> s {image = a} :: DeleteImageResponse)
{-# DEPRECATED dirsImage "Use generic-lens or generic-optics with 'image' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirsResponseStatus :: Lens.Lens' DeleteImageResponse Lude.Int
dirsResponseStatus = Lens.lens (responseStatus :: DeleteImageResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteImageResponse)
{-# DEPRECATED dirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
