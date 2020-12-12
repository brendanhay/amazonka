{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.Image
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.Image
  ( Image (..),

    -- * Smart constructor
    mkImage,

    -- * Lenses
    iFailureReason,
    iDisplayName,
    iDescription,
    iCreationTime,
    iImageARN,
    iImageName,
    iImageStatus,
    iLastModifiedTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.ImageStatus

-- | A SageMaker image. A SageMaker image represents a set of container images that are derived from a common base container image. Each of these container images is represented by a SageMaker @ImageVersion@ .
--
-- /See:/ 'mkImage' smart constructor.
data Image = Image'
  { failureReason :: Lude.Maybe Lude.Text,
    displayName :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    creationTime :: Lude.Timestamp,
    imageARN :: Lude.Text,
    imageName :: Lude.Text,
    imageStatus :: ImageStatus,
    lastModifiedTime :: Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Image' with the minimum fields required to make a request.
--
-- * 'creationTime' - When the image was created.
-- * 'description' - The description of the image.
-- * 'displayName' - The name of the image as displayed.
-- * 'failureReason' - When a create, update, or delete operation fails, the reason for the failure.
-- * 'imageARN' - The Amazon Resource Name (ARN) of the image.
-- * 'imageName' - The name of the image.
-- * 'imageStatus' - The status of the image.
-- * 'lastModifiedTime' - When the image was last modified.
mkImage ::
  -- | 'creationTime'
  Lude.Timestamp ->
  -- | 'imageARN'
  Lude.Text ->
  -- | 'imageName'
  Lude.Text ->
  -- | 'imageStatus'
  ImageStatus ->
  -- | 'lastModifiedTime'
  Lude.Timestamp ->
  Image
mkImage
  pCreationTime_
  pImageARN_
  pImageName_
  pImageStatus_
  pLastModifiedTime_ =
    Image'
      { failureReason = Lude.Nothing,
        displayName = Lude.Nothing,
        description = Lude.Nothing,
        creationTime = pCreationTime_,
        imageARN = pImageARN_,
        imageName = pImageName_,
        imageStatus = pImageStatus_,
        lastModifiedTime = pLastModifiedTime_
      }

-- | When a create, update, or delete operation fails, the reason for the failure.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iFailureReason :: Lens.Lens' Image (Lude.Maybe Lude.Text)
iFailureReason = Lens.lens (failureReason :: Image -> Lude.Maybe Lude.Text) (\s a -> s {failureReason = a} :: Image)
{-# DEPRECATED iFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | The name of the image as displayed.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iDisplayName :: Lens.Lens' Image (Lude.Maybe Lude.Text)
iDisplayName = Lens.lens (displayName :: Image -> Lude.Maybe Lude.Text) (\s a -> s {displayName = a} :: Image)
{-# DEPRECATED iDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | The description of the image.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iDescription :: Lens.Lens' Image (Lude.Maybe Lude.Text)
iDescription = Lens.lens (description :: Image -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: Image)
{-# DEPRECATED iDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | When the image was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iCreationTime :: Lens.Lens' Image Lude.Timestamp
iCreationTime = Lens.lens (creationTime :: Image -> Lude.Timestamp) (\s a -> s {creationTime = a} :: Image)
{-# DEPRECATED iCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The Amazon Resource Name (ARN) of the image.
--
-- /Note:/ Consider using 'imageARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iImageARN :: Lens.Lens' Image Lude.Text
iImageARN = Lens.lens (imageARN :: Image -> Lude.Text) (\s a -> s {imageARN = a} :: Image)
{-# DEPRECATED iImageARN "Use generic-lens or generic-optics with 'imageARN' instead." #-}

-- | The name of the image.
--
-- /Note:/ Consider using 'imageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iImageName :: Lens.Lens' Image Lude.Text
iImageName = Lens.lens (imageName :: Image -> Lude.Text) (\s a -> s {imageName = a} :: Image)
{-# DEPRECATED iImageName "Use generic-lens or generic-optics with 'imageName' instead." #-}

-- | The status of the image.
--
-- /Note:/ Consider using 'imageStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iImageStatus :: Lens.Lens' Image ImageStatus
iImageStatus = Lens.lens (imageStatus :: Image -> ImageStatus) (\s a -> s {imageStatus = a} :: Image)
{-# DEPRECATED iImageStatus "Use generic-lens or generic-optics with 'imageStatus' instead." #-}

-- | When the image was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iLastModifiedTime :: Lens.Lens' Image Lude.Timestamp
iLastModifiedTime = Lens.lens (lastModifiedTime :: Image -> Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: Image)
{-# DEPRECATED iLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

instance Lude.FromJSON Image where
  parseJSON =
    Lude.withObject
      "Image"
      ( \x ->
          Image'
            Lude.<$> (x Lude..:? "FailureReason")
            Lude.<*> (x Lude..:? "DisplayName")
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..: "CreationTime")
            Lude.<*> (x Lude..: "ImageArn")
            Lude.<*> (x Lude..: "ImageName")
            Lude.<*> (x Lude..: "ImageStatus")
            Lude.<*> (x Lude..: "LastModifiedTime")
      )
