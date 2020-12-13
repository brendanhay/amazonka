{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.UpdateImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the properties of a SageMaker image. To change the image's tags, use the 'AddTags' and 'DeleteTags' APIs.
module Network.AWS.SageMaker.UpdateImage
  ( -- * Creating a request
    UpdateImage (..),
    mkUpdateImage,

    -- ** Request lenses
    uiDeleteProperties,
    uiDisplayName,
    uiImageName,
    uiDescription,
    uiRoleARN,

    -- * Destructuring the response
    UpdateImageResponse (..),
    mkUpdateImageResponse,

    -- ** Response lenses
    uirsImageARN,
    uirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkUpdateImage' smart constructor.
data UpdateImage = UpdateImage'
  { -- | A list of properties to delete. Only the @Description@ and @DisplayName@ properties can be deleted.
    deleteProperties :: Lude.Maybe [Lude.Text],
    -- | The new display name for the image.
    displayName :: Lude.Maybe Lude.Text,
    -- | The name of the image to update.
    imageName :: Lude.Text,
    -- | The new description for the image.
    description :: Lude.Maybe Lude.Text,
    -- | The new Amazon Resource Name (ARN) for the IAM role that enables Amazon SageMaker to perform tasks on your behalf.
    roleARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateImage' with the minimum fields required to make a request.
--
-- * 'deleteProperties' - A list of properties to delete. Only the @Description@ and @DisplayName@ properties can be deleted.
-- * 'displayName' - The new display name for the image.
-- * 'imageName' - The name of the image to update.
-- * 'description' - The new description for the image.
-- * 'roleARN' - The new Amazon Resource Name (ARN) for the IAM role that enables Amazon SageMaker to perform tasks on your behalf.
mkUpdateImage ::
  -- | 'imageName'
  Lude.Text ->
  UpdateImage
mkUpdateImage pImageName_ =
  UpdateImage'
    { deleteProperties = Lude.Nothing,
      displayName = Lude.Nothing,
      imageName = pImageName_,
      description = Lude.Nothing,
      roleARN = Lude.Nothing
    }

-- | A list of properties to delete. Only the @Description@ and @DisplayName@ properties can be deleted.
--
-- /Note:/ Consider using 'deleteProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiDeleteProperties :: Lens.Lens' UpdateImage (Lude.Maybe [Lude.Text])
uiDeleteProperties = Lens.lens (deleteProperties :: UpdateImage -> Lude.Maybe [Lude.Text]) (\s a -> s {deleteProperties = a} :: UpdateImage)
{-# DEPRECATED uiDeleteProperties "Use generic-lens or generic-optics with 'deleteProperties' instead." #-}

-- | The new display name for the image.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiDisplayName :: Lens.Lens' UpdateImage (Lude.Maybe Lude.Text)
uiDisplayName = Lens.lens (displayName :: UpdateImage -> Lude.Maybe Lude.Text) (\s a -> s {displayName = a} :: UpdateImage)
{-# DEPRECATED uiDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | The name of the image to update.
--
-- /Note:/ Consider using 'imageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiImageName :: Lens.Lens' UpdateImage Lude.Text
uiImageName = Lens.lens (imageName :: UpdateImage -> Lude.Text) (\s a -> s {imageName = a} :: UpdateImage)
{-# DEPRECATED uiImageName "Use generic-lens or generic-optics with 'imageName' instead." #-}

-- | The new description for the image.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiDescription :: Lens.Lens' UpdateImage (Lude.Maybe Lude.Text)
uiDescription = Lens.lens (description :: UpdateImage -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateImage)
{-# DEPRECATED uiDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The new Amazon Resource Name (ARN) for the IAM role that enables Amazon SageMaker to perform tasks on your behalf.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiRoleARN :: Lens.Lens' UpdateImage (Lude.Maybe Lude.Text)
uiRoleARN = Lens.lens (roleARN :: UpdateImage -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: UpdateImage)
{-# DEPRECATED uiRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.AWSRequest UpdateImage where
  type Rs UpdateImage = UpdateImageResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateImageResponse'
            Lude.<$> (x Lude..?> "ImageArn") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateImage where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.UpdateImage" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateImage where
  toJSON UpdateImage' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DeleteProperties" Lude..=) Lude.<$> deleteProperties,
            ("DisplayName" Lude..=) Lude.<$> displayName,
            Lude.Just ("ImageName" Lude..= imageName),
            ("Description" Lude..=) Lude.<$> description,
            ("RoleArn" Lude..=) Lude.<$> roleARN
          ]
      )

instance Lude.ToPath UpdateImage where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateImage where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateImageResponse' smart constructor.
data UpdateImageResponse = UpdateImageResponse'
  { -- | The Amazon Resource Name (ARN) of the image.
    imageARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateImageResponse' with the minimum fields required to make a request.
--
-- * 'imageARN' - The Amazon Resource Name (ARN) of the image.
-- * 'responseStatus' - The response status code.
mkUpdateImageResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateImageResponse
mkUpdateImageResponse pResponseStatus_ =
  UpdateImageResponse'
    { imageARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the image.
--
-- /Note:/ Consider using 'imageARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uirsImageARN :: Lens.Lens' UpdateImageResponse (Lude.Maybe Lude.Text)
uirsImageARN = Lens.lens (imageARN :: UpdateImageResponse -> Lude.Maybe Lude.Text) (\s a -> s {imageARN = a} :: UpdateImageResponse)
{-# DEPRECATED uirsImageARN "Use generic-lens or generic-optics with 'imageARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uirsResponseStatus :: Lens.Lens' UpdateImageResponse Lude.Int
uirsResponseStatus = Lens.lens (responseStatus :: UpdateImageResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateImageResponse)
{-# DEPRECATED uirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
