{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DescribeImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a SageMaker image.
module Network.AWS.SageMaker.DescribeImage
  ( -- * Creating a request
    DescribeImage (..),
    mkDescribeImage,

    -- ** Request lenses
    diImageName,

    -- * Destructuring the response
    DescribeImageResponse (..),
    mkDescribeImageResponse,

    -- ** Response lenses
    desrsCreationTime,
    desrsFailureReason,
    desrsImageStatus,
    desrsLastModifiedTime,
    desrsImageARN,
    desrsDisplayName,
    desrsImageName,
    desrsDescription,
    desrsRoleARN,
    desrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkDescribeImage' smart constructor.
newtype DescribeImage = DescribeImage' {imageName :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeImage' with the minimum fields required to make a request.
--
-- * 'imageName' - The name of the image to describe.
mkDescribeImage ::
  -- | 'imageName'
  Lude.Text ->
  DescribeImage
mkDescribeImage pImageName_ =
  DescribeImage' {imageName = pImageName_}

-- | The name of the image to describe.
--
-- /Note:/ Consider using 'imageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diImageName :: Lens.Lens' DescribeImage Lude.Text
diImageName = Lens.lens (imageName :: DescribeImage -> Lude.Text) (\s a -> s {imageName = a} :: DescribeImage)
{-# DEPRECATED diImageName "Use generic-lens or generic-optics with 'imageName' instead." #-}

instance Lude.AWSRequest DescribeImage where
  type Rs DescribeImage = DescribeImageResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeImageResponse'
            Lude.<$> (x Lude..?> "CreationTime")
            Lude.<*> (x Lude..?> "FailureReason")
            Lude.<*> (x Lude..?> "ImageStatus")
            Lude.<*> (x Lude..?> "LastModifiedTime")
            Lude.<*> (x Lude..?> "ImageArn")
            Lude.<*> (x Lude..?> "DisplayName")
            Lude.<*> (x Lude..?> "ImageName")
            Lude.<*> (x Lude..?> "Description")
            Lude.<*> (x Lude..?> "RoleArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeImage where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.DescribeImage" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeImage where
  toJSON DescribeImage' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("ImageName" Lude..= imageName)])

instance Lude.ToPath DescribeImage where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeImage where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeImageResponse' smart constructor.
data DescribeImageResponse = DescribeImageResponse'
  { creationTime ::
      Lude.Maybe Lude.Timestamp,
    failureReason :: Lude.Maybe Lude.Text,
    imageStatus :: Lude.Maybe ImageStatus,
    lastModifiedTime :: Lude.Maybe Lude.Timestamp,
    imageARN :: Lude.Maybe Lude.Text,
    displayName :: Lude.Maybe Lude.Text,
    imageName :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    roleARN :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DescribeImageResponse' with the minimum fields required to make a request.
--
-- * 'creationTime' - When the image was created.
-- * 'description' - The description of the image.
-- * 'displayName' - The name of the image as displayed.
-- * 'failureReason' - When a create, update, or delete operation fails, the reason for the failure.
-- * 'imageARN' - The Amazon Resource Name (ARN) of the image.
-- * 'imageName' - The name of the image.
-- * 'imageStatus' - The status of the image.
-- * 'lastModifiedTime' - When the image was last modified.
-- * 'responseStatus' - The response status code.
-- * 'roleARN' - The Amazon Resource Name (ARN) of the IAM role that enables Amazon SageMaker to perform tasks on your behalf.
mkDescribeImageResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeImageResponse
mkDescribeImageResponse pResponseStatus_ =
  DescribeImageResponse'
    { creationTime = Lude.Nothing,
      failureReason = Lude.Nothing,
      imageStatus = Lude.Nothing,
      lastModifiedTime = Lude.Nothing,
      imageARN = Lude.Nothing,
      displayName = Lude.Nothing,
      imageName = Lude.Nothing,
      description = Lude.Nothing,
      roleARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | When the image was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsCreationTime :: Lens.Lens' DescribeImageResponse (Lude.Maybe Lude.Timestamp)
desrsCreationTime = Lens.lens (creationTime :: DescribeImageResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: DescribeImageResponse)
{-# DEPRECATED desrsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | When a create, update, or delete operation fails, the reason for the failure.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsFailureReason :: Lens.Lens' DescribeImageResponse (Lude.Maybe Lude.Text)
desrsFailureReason = Lens.lens (failureReason :: DescribeImageResponse -> Lude.Maybe Lude.Text) (\s a -> s {failureReason = a} :: DescribeImageResponse)
{-# DEPRECATED desrsFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | The status of the image.
--
-- /Note:/ Consider using 'imageStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsImageStatus :: Lens.Lens' DescribeImageResponse (Lude.Maybe ImageStatus)
desrsImageStatus = Lens.lens (imageStatus :: DescribeImageResponse -> Lude.Maybe ImageStatus) (\s a -> s {imageStatus = a} :: DescribeImageResponse)
{-# DEPRECATED desrsImageStatus "Use generic-lens or generic-optics with 'imageStatus' instead." #-}

-- | When the image was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsLastModifiedTime :: Lens.Lens' DescribeImageResponse (Lude.Maybe Lude.Timestamp)
desrsLastModifiedTime = Lens.lens (lastModifiedTime :: DescribeImageResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: DescribeImageResponse)
{-# DEPRECATED desrsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The Amazon Resource Name (ARN) of the image.
--
-- /Note:/ Consider using 'imageARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsImageARN :: Lens.Lens' DescribeImageResponse (Lude.Maybe Lude.Text)
desrsImageARN = Lens.lens (imageARN :: DescribeImageResponse -> Lude.Maybe Lude.Text) (\s a -> s {imageARN = a} :: DescribeImageResponse)
{-# DEPRECATED desrsImageARN "Use generic-lens or generic-optics with 'imageARN' instead." #-}

-- | The name of the image as displayed.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsDisplayName :: Lens.Lens' DescribeImageResponse (Lude.Maybe Lude.Text)
desrsDisplayName = Lens.lens (displayName :: DescribeImageResponse -> Lude.Maybe Lude.Text) (\s a -> s {displayName = a} :: DescribeImageResponse)
{-# DEPRECATED desrsDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | The name of the image.
--
-- /Note:/ Consider using 'imageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsImageName :: Lens.Lens' DescribeImageResponse (Lude.Maybe Lude.Text)
desrsImageName = Lens.lens (imageName :: DescribeImageResponse -> Lude.Maybe Lude.Text) (\s a -> s {imageName = a} :: DescribeImageResponse)
{-# DEPRECATED desrsImageName "Use generic-lens or generic-optics with 'imageName' instead." #-}

-- | The description of the image.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsDescription :: Lens.Lens' DescribeImageResponse (Lude.Maybe Lude.Text)
desrsDescription = Lens.lens (description :: DescribeImageResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: DescribeImageResponse)
{-# DEPRECATED desrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM role that enables Amazon SageMaker to perform tasks on your behalf.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsRoleARN :: Lens.Lens' DescribeImageResponse (Lude.Maybe Lude.Text)
desrsRoleARN = Lens.lens (roleARN :: DescribeImageResponse -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: DescribeImageResponse)
{-# DEPRECATED desrsRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsResponseStatus :: Lens.Lens' DescribeImageResponse Lude.Int
desrsResponseStatus = Lens.lens (responseStatus :: DescribeImageResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeImageResponse)
{-# DEPRECATED desrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
