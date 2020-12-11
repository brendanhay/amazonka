-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ImageVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ImageVersion
  ( ImageVersion (..),

    -- * Smart constructor
    mkImageVersion,

    -- * Lenses
    ivFailureReason,
    ivCreationTime,
    ivImageARN,
    ivImageVersionARN,
    ivImageVersionStatus,
    ivLastModifiedTime,
    ivVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.ImageVersionStatus

-- | A version of a SageMaker @Image@ . A version represents an existing container image.
--
-- /See:/ 'mkImageVersion' smart constructor.
data ImageVersion = ImageVersion'
  { failureReason ::
      Lude.Maybe Lude.Text,
    creationTime :: Lude.Timestamp,
    imageARN :: Lude.Text,
    imageVersionARN :: Lude.Text,
    imageVersionStatus :: ImageVersionStatus,
    lastModifiedTime :: Lude.Timestamp,
    version :: Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImageVersion' with the minimum fields required to make a request.
--
-- * 'creationTime' - When the version was created.
-- * 'failureReason' - When a create or delete operation fails, the reason for the failure.
-- * 'imageARN' - The Amazon Resource Name (ARN) of the image the version is based on.
-- * 'imageVersionARN' - The ARN of the version.
-- * 'imageVersionStatus' - The status of the version.
-- * 'lastModifiedTime' - When the version was last modified.
-- * 'version' - The version number.
mkImageVersion ::
  -- | 'creationTime'
  Lude.Timestamp ->
  -- | 'imageARN'
  Lude.Text ->
  -- | 'imageVersionARN'
  Lude.Text ->
  -- | 'imageVersionStatus'
  ImageVersionStatus ->
  -- | 'lastModifiedTime'
  Lude.Timestamp ->
  -- | 'version'
  Lude.Natural ->
  ImageVersion
mkImageVersion
  pCreationTime_
  pImageARN_
  pImageVersionARN_
  pImageVersionStatus_
  pLastModifiedTime_
  pVersion_ =
    ImageVersion'
      { failureReason = Lude.Nothing,
        creationTime = pCreationTime_,
        imageARN = pImageARN_,
        imageVersionARN = pImageVersionARN_,
        imageVersionStatus = pImageVersionStatus_,
        lastModifiedTime = pLastModifiedTime_,
        version = pVersion_
      }

-- | When a create or delete operation fails, the reason for the failure.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivFailureReason :: Lens.Lens' ImageVersion (Lude.Maybe Lude.Text)
ivFailureReason = Lens.lens (failureReason :: ImageVersion -> Lude.Maybe Lude.Text) (\s a -> s {failureReason = a} :: ImageVersion)
{-# DEPRECATED ivFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | When the version was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivCreationTime :: Lens.Lens' ImageVersion Lude.Timestamp
ivCreationTime = Lens.lens (creationTime :: ImageVersion -> Lude.Timestamp) (\s a -> s {creationTime = a} :: ImageVersion)
{-# DEPRECATED ivCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The Amazon Resource Name (ARN) of the image the version is based on.
--
-- /Note:/ Consider using 'imageARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivImageARN :: Lens.Lens' ImageVersion Lude.Text
ivImageARN = Lens.lens (imageARN :: ImageVersion -> Lude.Text) (\s a -> s {imageARN = a} :: ImageVersion)
{-# DEPRECATED ivImageARN "Use generic-lens or generic-optics with 'imageARN' instead." #-}

-- | The ARN of the version.
--
-- /Note:/ Consider using 'imageVersionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivImageVersionARN :: Lens.Lens' ImageVersion Lude.Text
ivImageVersionARN = Lens.lens (imageVersionARN :: ImageVersion -> Lude.Text) (\s a -> s {imageVersionARN = a} :: ImageVersion)
{-# DEPRECATED ivImageVersionARN "Use generic-lens or generic-optics with 'imageVersionARN' instead." #-}

-- | The status of the version.
--
-- /Note:/ Consider using 'imageVersionStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivImageVersionStatus :: Lens.Lens' ImageVersion ImageVersionStatus
ivImageVersionStatus = Lens.lens (imageVersionStatus :: ImageVersion -> ImageVersionStatus) (\s a -> s {imageVersionStatus = a} :: ImageVersion)
{-# DEPRECATED ivImageVersionStatus "Use generic-lens or generic-optics with 'imageVersionStatus' instead." #-}

-- | When the version was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivLastModifiedTime :: Lens.Lens' ImageVersion Lude.Timestamp
ivLastModifiedTime = Lens.lens (lastModifiedTime :: ImageVersion -> Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: ImageVersion)
{-# DEPRECATED ivLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The version number.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivVersion :: Lens.Lens' ImageVersion Lude.Natural
ivVersion = Lens.lens (version :: ImageVersion -> Lude.Natural) (\s a -> s {version = a} :: ImageVersion)
{-# DEPRECATED ivVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Lude.FromJSON ImageVersion where
  parseJSON =
    Lude.withObject
      "ImageVersion"
      ( \x ->
          ImageVersion'
            Lude.<$> (x Lude..:? "FailureReason")
            Lude.<*> (x Lude..: "CreationTime")
            Lude.<*> (x Lude..: "ImageArn")
            Lude.<*> (x Lude..: "ImageVersionArn")
            Lude.<*> (x Lude..: "ImageVersionStatus")
            Lude.<*> (x Lude..: "LastModifiedTime")
            Lude.<*> (x Lude..: "Version")
      )
