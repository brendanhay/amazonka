{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    difImageName,

    -- * Destructuring the response
    DescribeImageResponse (..),
    mkDescribeImageResponse,

    -- ** Response lenses
    difrsCreationTime,
    difrsFailureReason,
    difrsImageStatus,
    difrsLastModifiedTime,
    difrsImageARN,
    difrsDisplayName,
    difrsImageName,
    difrsDescription,
    difrsRoleARN,
    difrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkDescribeImage' smart constructor.
newtype DescribeImage = DescribeImage'
  { -- | The name of the image to describe.
    imageName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
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
difImageName :: Lens.Lens' DescribeImage Lude.Text
difImageName = Lens.lens (imageName :: DescribeImage -> Lude.Text) (\s a -> s {imageName = a} :: DescribeImage)
{-# DEPRECATED difImageName "Use generic-lens or generic-optics with 'imageName' instead." #-}

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
  { -- | When the image was created.
    creationTime :: Lude.Maybe Lude.Timestamp,
    -- | When a create, update, or delete operation fails, the reason for the failure.
    failureReason :: Lude.Maybe Lude.Text,
    -- | The status of the image.
    imageStatus :: Lude.Maybe ImageStatus,
    -- | When the image was last modified.
    lastModifiedTime :: Lude.Maybe Lude.Timestamp,
    -- | The Amazon Resource Name (ARN) of the image.
    imageARN :: Lude.Maybe Lude.Text,
    -- | The name of the image as displayed.
    displayName :: Lude.Maybe Lude.Text,
    -- | The name of the image.
    imageName :: Lude.Maybe Lude.Text,
    -- | The description of the image.
    description :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role that enables Amazon SageMaker to perform tasks on your behalf.
    roleARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeImageResponse' with the minimum fields required to make a request.
--
-- * 'creationTime' - When the image was created.
-- * 'failureReason' - When a create, update, or delete operation fails, the reason for the failure.
-- * 'imageStatus' - The status of the image.
-- * 'lastModifiedTime' - When the image was last modified.
-- * 'imageARN' - The Amazon Resource Name (ARN) of the image.
-- * 'displayName' - The name of the image as displayed.
-- * 'imageName' - The name of the image.
-- * 'description' - The description of the image.
-- * 'roleARN' - The Amazon Resource Name (ARN) of the IAM role that enables Amazon SageMaker to perform tasks on your behalf.
-- * 'responseStatus' - The response status code.
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
difrsCreationTime :: Lens.Lens' DescribeImageResponse (Lude.Maybe Lude.Timestamp)
difrsCreationTime = Lens.lens (creationTime :: DescribeImageResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: DescribeImageResponse)
{-# DEPRECATED difrsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | When a create, update, or delete operation fails, the reason for the failure.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
difrsFailureReason :: Lens.Lens' DescribeImageResponse (Lude.Maybe Lude.Text)
difrsFailureReason = Lens.lens (failureReason :: DescribeImageResponse -> Lude.Maybe Lude.Text) (\s a -> s {failureReason = a} :: DescribeImageResponse)
{-# DEPRECATED difrsFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | The status of the image.
--
-- /Note:/ Consider using 'imageStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
difrsImageStatus :: Lens.Lens' DescribeImageResponse (Lude.Maybe ImageStatus)
difrsImageStatus = Lens.lens (imageStatus :: DescribeImageResponse -> Lude.Maybe ImageStatus) (\s a -> s {imageStatus = a} :: DescribeImageResponse)
{-# DEPRECATED difrsImageStatus "Use generic-lens or generic-optics with 'imageStatus' instead." #-}

-- | When the image was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
difrsLastModifiedTime :: Lens.Lens' DescribeImageResponse (Lude.Maybe Lude.Timestamp)
difrsLastModifiedTime = Lens.lens (lastModifiedTime :: DescribeImageResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: DescribeImageResponse)
{-# DEPRECATED difrsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The Amazon Resource Name (ARN) of the image.
--
-- /Note:/ Consider using 'imageARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
difrsImageARN :: Lens.Lens' DescribeImageResponse (Lude.Maybe Lude.Text)
difrsImageARN = Lens.lens (imageARN :: DescribeImageResponse -> Lude.Maybe Lude.Text) (\s a -> s {imageARN = a} :: DescribeImageResponse)
{-# DEPRECATED difrsImageARN "Use generic-lens or generic-optics with 'imageARN' instead." #-}

-- | The name of the image as displayed.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
difrsDisplayName :: Lens.Lens' DescribeImageResponse (Lude.Maybe Lude.Text)
difrsDisplayName = Lens.lens (displayName :: DescribeImageResponse -> Lude.Maybe Lude.Text) (\s a -> s {displayName = a} :: DescribeImageResponse)
{-# DEPRECATED difrsDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | The name of the image.
--
-- /Note:/ Consider using 'imageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
difrsImageName :: Lens.Lens' DescribeImageResponse (Lude.Maybe Lude.Text)
difrsImageName = Lens.lens (imageName :: DescribeImageResponse -> Lude.Maybe Lude.Text) (\s a -> s {imageName = a} :: DescribeImageResponse)
{-# DEPRECATED difrsImageName "Use generic-lens or generic-optics with 'imageName' instead." #-}

-- | The description of the image.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
difrsDescription :: Lens.Lens' DescribeImageResponse (Lude.Maybe Lude.Text)
difrsDescription = Lens.lens (description :: DescribeImageResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: DescribeImageResponse)
{-# DEPRECATED difrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM role that enables Amazon SageMaker to perform tasks on your behalf.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
difrsRoleARN :: Lens.Lens' DescribeImageResponse (Lude.Maybe Lude.Text)
difrsRoleARN = Lens.lens (roleARN :: DescribeImageResponse -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: DescribeImageResponse)
{-# DEPRECATED difrsRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
difrsResponseStatus :: Lens.Lens' DescribeImageResponse Lude.Int
difrsResponseStatus = Lens.lens (responseStatus :: DescribeImageResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeImageResponse)
{-# DEPRECATED difrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
