{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DescribeImageVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a version of a SageMaker image.
module Network.AWS.SageMaker.DescribeImageVersion
  ( -- * Creating a request
    DescribeImageVersion (..),
    mkDescribeImageVersion,

    -- ** Request lenses
    dVersion,
    dImageName,

    -- * Destructuring the response
    DescribeImageVersionResponse (..),
    mkDescribeImageVersionResponse,

    -- ** Response lenses
    divfrsCreationTime,
    divfrsFailureReason,
    divfrsContainerImage,
    divfrsLastModifiedTime,
    divfrsImageVersionStatus,
    divfrsVersion,
    divfrsBaseImage,
    divfrsImageARN,
    divfrsImageVersionARN,
    divfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkDescribeImageVersion' smart constructor.
data DescribeImageVersion = DescribeImageVersion'
  { -- | The version of the image. If not specified, the latest version is described.
    version :: Lude.Maybe Lude.Natural,
    -- | The name of the image.
    imageName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeImageVersion' with the minimum fields required to make a request.
--
-- * 'version' - The version of the image. If not specified, the latest version is described.
-- * 'imageName' - The name of the image.
mkDescribeImageVersion ::
  -- | 'imageName'
  Lude.Text ->
  DescribeImageVersion
mkDescribeImageVersion pImageName_ =
  DescribeImageVersion'
    { version = Lude.Nothing,
      imageName = pImageName_
    }

-- | The version of the image. If not specified, the latest version is described.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dVersion :: Lens.Lens' DescribeImageVersion (Lude.Maybe Lude.Natural)
dVersion = Lens.lens (version :: DescribeImageVersion -> Lude.Maybe Lude.Natural) (\s a -> s {version = a} :: DescribeImageVersion)
{-# DEPRECATED dVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The name of the image.
--
-- /Note:/ Consider using 'imageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dImageName :: Lens.Lens' DescribeImageVersion Lude.Text
dImageName = Lens.lens (imageName :: DescribeImageVersion -> Lude.Text) (\s a -> s {imageName = a} :: DescribeImageVersion)
{-# DEPRECATED dImageName "Use generic-lens or generic-optics with 'imageName' instead." #-}

instance Lude.AWSRequest DescribeImageVersion where
  type Rs DescribeImageVersion = DescribeImageVersionResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeImageVersionResponse'
            Lude.<$> (x Lude..?> "CreationTime")
            Lude.<*> (x Lude..?> "FailureReason")
            Lude.<*> (x Lude..?> "ContainerImage")
            Lude.<*> (x Lude..?> "LastModifiedTime")
            Lude.<*> (x Lude..?> "ImageVersionStatus")
            Lude.<*> (x Lude..?> "Version")
            Lude.<*> (x Lude..?> "BaseImage")
            Lude.<*> (x Lude..?> "ImageArn")
            Lude.<*> (x Lude..?> "ImageVersionArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeImageVersion where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.DescribeImageVersion" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeImageVersion where
  toJSON DescribeImageVersion' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Version" Lude..=) Lude.<$> version,
            Lude.Just ("ImageName" Lude..= imageName)
          ]
      )

instance Lude.ToPath DescribeImageVersion where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeImageVersion where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeImageVersionResponse' smart constructor.
data DescribeImageVersionResponse = DescribeImageVersionResponse'
  { -- | When the version was created.
    creationTime :: Lude.Maybe Lude.Timestamp,
    -- | When a create or delete operation fails, the reason for the failure.
    failureReason :: Lude.Maybe Lude.Text,
    -- | The registry path of the container image that contains this image version.
    containerImage :: Lude.Maybe Lude.Text,
    -- | When the version was last modified.
    lastModifiedTime :: Lude.Maybe Lude.Timestamp,
    -- | The status of the version.
    imageVersionStatus :: Lude.Maybe ImageVersionStatus,
    -- | The version number.
    version :: Lude.Maybe Lude.Natural,
    -- | The registry path of the container image on which this image version is based.
    baseImage :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the image the version is based on.
    imageARN :: Lude.Maybe Lude.Text,
    -- | The ARN of the version.
    imageVersionARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeImageVersionResponse' with the minimum fields required to make a request.
--
-- * 'creationTime' - When the version was created.
-- * 'failureReason' - When a create or delete operation fails, the reason for the failure.
-- * 'containerImage' - The registry path of the container image that contains this image version.
-- * 'lastModifiedTime' - When the version was last modified.
-- * 'imageVersionStatus' - The status of the version.
-- * 'version' - The version number.
-- * 'baseImage' - The registry path of the container image on which this image version is based.
-- * 'imageARN' - The Amazon Resource Name (ARN) of the image the version is based on.
-- * 'imageVersionARN' - The ARN of the version.
-- * 'responseStatus' - The response status code.
mkDescribeImageVersionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeImageVersionResponse
mkDescribeImageVersionResponse pResponseStatus_ =
  DescribeImageVersionResponse'
    { creationTime = Lude.Nothing,
      failureReason = Lude.Nothing,
      containerImage = Lude.Nothing,
      lastModifiedTime = Lude.Nothing,
      imageVersionStatus = Lude.Nothing,
      version = Lude.Nothing,
      baseImage = Lude.Nothing,
      imageARN = Lude.Nothing,
      imageVersionARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | When the version was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
divfrsCreationTime :: Lens.Lens' DescribeImageVersionResponse (Lude.Maybe Lude.Timestamp)
divfrsCreationTime = Lens.lens (creationTime :: DescribeImageVersionResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: DescribeImageVersionResponse)
{-# DEPRECATED divfrsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | When a create or delete operation fails, the reason for the failure.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
divfrsFailureReason :: Lens.Lens' DescribeImageVersionResponse (Lude.Maybe Lude.Text)
divfrsFailureReason = Lens.lens (failureReason :: DescribeImageVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {failureReason = a} :: DescribeImageVersionResponse)
{-# DEPRECATED divfrsFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | The registry path of the container image that contains this image version.
--
-- /Note:/ Consider using 'containerImage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
divfrsContainerImage :: Lens.Lens' DescribeImageVersionResponse (Lude.Maybe Lude.Text)
divfrsContainerImage = Lens.lens (containerImage :: DescribeImageVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {containerImage = a} :: DescribeImageVersionResponse)
{-# DEPRECATED divfrsContainerImage "Use generic-lens or generic-optics with 'containerImage' instead." #-}

-- | When the version was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
divfrsLastModifiedTime :: Lens.Lens' DescribeImageVersionResponse (Lude.Maybe Lude.Timestamp)
divfrsLastModifiedTime = Lens.lens (lastModifiedTime :: DescribeImageVersionResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: DescribeImageVersionResponse)
{-# DEPRECATED divfrsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The status of the version.
--
-- /Note:/ Consider using 'imageVersionStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
divfrsImageVersionStatus :: Lens.Lens' DescribeImageVersionResponse (Lude.Maybe ImageVersionStatus)
divfrsImageVersionStatus = Lens.lens (imageVersionStatus :: DescribeImageVersionResponse -> Lude.Maybe ImageVersionStatus) (\s a -> s {imageVersionStatus = a} :: DescribeImageVersionResponse)
{-# DEPRECATED divfrsImageVersionStatus "Use generic-lens or generic-optics with 'imageVersionStatus' instead." #-}

-- | The version number.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
divfrsVersion :: Lens.Lens' DescribeImageVersionResponse (Lude.Maybe Lude.Natural)
divfrsVersion = Lens.lens (version :: DescribeImageVersionResponse -> Lude.Maybe Lude.Natural) (\s a -> s {version = a} :: DescribeImageVersionResponse)
{-# DEPRECATED divfrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The registry path of the container image on which this image version is based.
--
-- /Note:/ Consider using 'baseImage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
divfrsBaseImage :: Lens.Lens' DescribeImageVersionResponse (Lude.Maybe Lude.Text)
divfrsBaseImage = Lens.lens (baseImage :: DescribeImageVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {baseImage = a} :: DescribeImageVersionResponse)
{-# DEPRECATED divfrsBaseImage "Use generic-lens or generic-optics with 'baseImage' instead." #-}

-- | The Amazon Resource Name (ARN) of the image the version is based on.
--
-- /Note:/ Consider using 'imageARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
divfrsImageARN :: Lens.Lens' DescribeImageVersionResponse (Lude.Maybe Lude.Text)
divfrsImageARN = Lens.lens (imageARN :: DescribeImageVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {imageARN = a} :: DescribeImageVersionResponse)
{-# DEPRECATED divfrsImageARN "Use generic-lens or generic-optics with 'imageARN' instead." #-}

-- | The ARN of the version.
--
-- /Note:/ Consider using 'imageVersionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
divfrsImageVersionARN :: Lens.Lens' DescribeImageVersionResponse (Lude.Maybe Lude.Text)
divfrsImageVersionARN = Lens.lens (imageVersionARN :: DescribeImageVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {imageVersionARN = a} :: DescribeImageVersionResponse)
{-# DEPRECATED divfrsImageVersionARN "Use generic-lens or generic-optics with 'imageVersionARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
divfrsResponseStatus :: Lens.Lens' DescribeImageVersionResponse Lude.Int
divfrsResponseStatus = Lens.lens (responseStatus :: DescribeImageVersionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeImageVersionResponse)
{-# DEPRECATED divfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
