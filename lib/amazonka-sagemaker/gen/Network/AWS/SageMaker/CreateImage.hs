{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.CreateImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a custom SageMaker image. A SageMaker image is a set of image versions. Each image version represents a container image stored in Amazon Container Registry (ECR). For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/studio-byoi.html Bring your own SageMaker image> .
module Network.AWS.SageMaker.CreateImage
  ( -- * Creating a request
    CreateImage (..),
    mkCreateImage,

    -- ** Request lenses
    cifDisplayName,
    cifImageName,
    cifDescription,
    cifTags,
    cifRoleARN,

    -- * Destructuring the response
    CreateImageResponse (..),
    mkCreateImageResponse,

    -- ** Response lenses
    cirsImageARN,
    cirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkCreateImage' smart constructor.
data CreateImage = CreateImage'
  { -- | The display name of the image. If not provided, @ImageName@ is displayed.
    displayName :: Lude.Maybe Lude.Text,
    -- | The name of the image. Must be unique to your account.
    imageName :: Lude.Text,
    -- | The description of the image.
    description :: Lude.Maybe Lude.Text,
    -- | A list of tags to apply to the image.
    tags :: Lude.Maybe [Tag],
    -- | The Amazon Resource Name (ARN) of an IAM role that enables Amazon SageMaker to perform tasks on your behalf.
    roleARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateImage' with the minimum fields required to make a request.
--
-- * 'displayName' - The display name of the image. If not provided, @ImageName@ is displayed.
-- * 'imageName' - The name of the image. Must be unique to your account.
-- * 'description' - The description of the image.
-- * 'tags' - A list of tags to apply to the image.
-- * 'roleARN' - The Amazon Resource Name (ARN) of an IAM role that enables Amazon SageMaker to perform tasks on your behalf.
mkCreateImage ::
  -- | 'imageName'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  CreateImage
mkCreateImage pImageName_ pRoleARN_ =
  CreateImage'
    { displayName = Lude.Nothing,
      imageName = pImageName_,
      description = Lude.Nothing,
      tags = Lude.Nothing,
      roleARN = pRoleARN_
    }

-- | The display name of the image. If not provided, @ImageName@ is displayed.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifDisplayName :: Lens.Lens' CreateImage (Lude.Maybe Lude.Text)
cifDisplayName = Lens.lens (displayName :: CreateImage -> Lude.Maybe Lude.Text) (\s a -> s {displayName = a} :: CreateImage)
{-# DEPRECATED cifDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | The name of the image. Must be unique to your account.
--
-- /Note:/ Consider using 'imageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifImageName :: Lens.Lens' CreateImage Lude.Text
cifImageName = Lens.lens (imageName :: CreateImage -> Lude.Text) (\s a -> s {imageName = a} :: CreateImage)
{-# DEPRECATED cifImageName "Use generic-lens or generic-optics with 'imageName' instead." #-}

-- | The description of the image.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifDescription :: Lens.Lens' CreateImage (Lude.Maybe Lude.Text)
cifDescription = Lens.lens (description :: CreateImage -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateImage)
{-# DEPRECATED cifDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A list of tags to apply to the image.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifTags :: Lens.Lens' CreateImage (Lude.Maybe [Tag])
cifTags = Lens.lens (tags :: CreateImage -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateImage)
{-# DEPRECATED cifTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The Amazon Resource Name (ARN) of an IAM role that enables Amazon SageMaker to perform tasks on your behalf.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifRoleARN :: Lens.Lens' CreateImage Lude.Text
cifRoleARN = Lens.lens (roleARN :: CreateImage -> Lude.Text) (\s a -> s {roleARN = a} :: CreateImage)
{-# DEPRECATED cifRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.AWSRequest CreateImage where
  type Rs CreateImage = CreateImageResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateImageResponse'
            Lude.<$> (x Lude..?> "ImageArn") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateImage where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.CreateImage" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateImage where
  toJSON CreateImage' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DisplayName" Lude..=) Lude.<$> displayName,
            Lude.Just ("ImageName" Lude..= imageName),
            ("Description" Lude..=) Lude.<$> description,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("RoleArn" Lude..= roleARN)
          ]
      )

instance Lude.ToPath CreateImage where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateImage where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateImageResponse' smart constructor.
data CreateImageResponse = CreateImageResponse'
  { -- | The Amazon Resource Name (ARN) of the image.
    imageARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateImageResponse' with the minimum fields required to make a request.
--
-- * 'imageARN' - The Amazon Resource Name (ARN) of the image.
-- * 'responseStatus' - The response status code.
mkCreateImageResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateImageResponse
mkCreateImageResponse pResponseStatus_ =
  CreateImageResponse'
    { imageARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the image.
--
-- /Note:/ Consider using 'imageARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cirsImageARN :: Lens.Lens' CreateImageResponse (Lude.Maybe Lude.Text)
cirsImageARN = Lens.lens (imageARN :: CreateImageResponse -> Lude.Maybe Lude.Text) (\s a -> s {imageARN = a} :: CreateImageResponse)
{-# DEPRECATED cirsImageARN "Use generic-lens or generic-optics with 'imageARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cirsResponseStatus :: Lens.Lens' CreateImageResponse Lude.Int
cirsResponseStatus = Lens.lens (responseStatus :: CreateImageResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateImageResponse)
{-# DEPRECATED cirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
