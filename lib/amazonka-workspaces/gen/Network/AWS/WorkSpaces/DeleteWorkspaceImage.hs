{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.DeleteWorkspaceImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified image from your account. To delete an image, you must first delete any bundles that are associated with the image and unshare the image if it is shared with other accounts.
module Network.AWS.WorkSpaces.DeleteWorkspaceImage
  ( -- * Creating a request
    DeleteWorkspaceImage (..),
    mkDeleteWorkspaceImage,

    -- ** Request lenses
    dwiImageId,

    -- * Destructuring the response
    DeleteWorkspaceImageResponse (..),
    mkDeleteWorkspaceImageResponse,

    -- ** Response lenses
    dwirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'mkDeleteWorkspaceImage' smart constructor.
newtype DeleteWorkspaceImage = DeleteWorkspaceImage'
  { -- | The identifier of the image.
    imageId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteWorkspaceImage' with the minimum fields required to make a request.
--
-- * 'imageId' - The identifier of the image.
mkDeleteWorkspaceImage ::
  -- | 'imageId'
  Lude.Text ->
  DeleteWorkspaceImage
mkDeleteWorkspaceImage pImageId_ =
  DeleteWorkspaceImage' {imageId = pImageId_}

-- | The identifier of the image.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwiImageId :: Lens.Lens' DeleteWorkspaceImage Lude.Text
dwiImageId = Lens.lens (imageId :: DeleteWorkspaceImage -> Lude.Text) (\s a -> s {imageId = a} :: DeleteWorkspaceImage)
{-# DEPRECATED dwiImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

instance Lude.AWSRequest DeleteWorkspaceImage where
  type Rs DeleteWorkspaceImage = DeleteWorkspaceImageResponse
  request = Req.postJSON workSpacesService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteWorkspaceImageResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteWorkspaceImage where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkspacesService.DeleteWorkspaceImage" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteWorkspaceImage where
  toJSON DeleteWorkspaceImage' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("ImageId" Lude..= imageId)])

instance Lude.ToPath DeleteWorkspaceImage where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteWorkspaceImage where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteWorkspaceImageResponse' smart constructor.
newtype DeleteWorkspaceImageResponse = DeleteWorkspaceImageResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteWorkspaceImageResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteWorkspaceImageResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteWorkspaceImageResponse
mkDeleteWorkspaceImageResponse pResponseStatus_ =
  DeleteWorkspaceImageResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwirsResponseStatus :: Lens.Lens' DeleteWorkspaceImageResponse Lude.Int
dwirsResponseStatus = Lens.lens (responseStatus :: DeleteWorkspaceImageResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteWorkspaceImageResponse)
{-# DEPRECATED dwirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
