{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.Image
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.Image
  ( Image (..),

    -- * Smart constructor
    mkImage,

    -- * Lenses
    iState,
    iImagePermissions,
    iPlatform,
    iPublicBaseImageReleasedDate,
    iStateChangeReason,
    iARN,
    iCreatedTime,
    iImageBuilderSupported,
    iVisibility,
    iImageBuilderName,
    iBaseImageARN,
    iDisplayName,
    iDescription,
    iAppstreamAgentVersion,
    iApplications,
    iName,
  )
where

import Network.AWS.AppStream.Types.Application
import Network.AWS.AppStream.Types.ImagePermissions
import Network.AWS.AppStream.Types.ImageState
import Network.AWS.AppStream.Types.ImageStateChangeReason
import Network.AWS.AppStream.Types.PlatformType
import Network.AWS.AppStream.Types.VisibilityType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an image.
--
-- /See:/ 'mkImage' smart constructor.
data Image = Image'
  { state :: Lude.Maybe ImageState,
    imagePermissions :: Lude.Maybe ImagePermissions,
    platform :: Lude.Maybe PlatformType,
    publicBaseImageReleasedDate :: Lude.Maybe Lude.Timestamp,
    stateChangeReason :: Lude.Maybe ImageStateChangeReason,
    arn :: Lude.Maybe Lude.Text,
    createdTime :: Lude.Maybe Lude.Timestamp,
    imageBuilderSupported :: Lude.Maybe Lude.Bool,
    visibility :: Lude.Maybe VisibilityType,
    imageBuilderName :: Lude.Maybe Lude.Text,
    baseImageARN :: Lude.Maybe Lude.Text,
    displayName :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    appstreamAgentVersion :: Lude.Maybe Lude.Text,
    applications :: Lude.Maybe [Application],
    name :: Lude.Text
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
-- * 'applications' - The applications associated with the image.
-- * 'appstreamAgentVersion' - The version of the AppStream 2.0 agent to use for instances that are launched from this image.
-- * 'arn' - The ARN of the image.
-- * 'baseImageARN' - The ARN of the image from which this image was created.
-- * 'createdTime' - The time the image was created.
-- * 'description' - The description to display.
-- * 'displayName' - The image name to display.
-- * 'imageBuilderName' - The name of the image builder that was used to create the private image. If the image is shared, this value is null.
-- * 'imageBuilderSupported' - Indicates whether an image builder can be launched from this image.
-- * 'imagePermissions' - The permissions to provide to the destination AWS account for the specified image.
-- * 'name' - The name of the image.
-- * 'platform' - The operating system platform of the image.
-- * 'publicBaseImageReleasedDate' - The release date of the public base image. For private images, this date is the release date of the base image from which the image was created.
-- * 'state' - The image starts in the @PENDING@ state. If image creation succeeds, the state is @AVAILABLE@ . If image creation fails, the state is @FAILED@ .
-- * 'stateChangeReason' - The reason why the last state change occurred.
-- * 'visibility' - Indicates whether the image is public or private.
mkImage ::
  -- | 'name'
  Lude.Text ->
  Image
mkImage pName_ =
  Image'
    { state = Lude.Nothing,
      imagePermissions = Lude.Nothing,
      platform = Lude.Nothing,
      publicBaseImageReleasedDate = Lude.Nothing,
      stateChangeReason = Lude.Nothing,
      arn = Lude.Nothing,
      createdTime = Lude.Nothing,
      imageBuilderSupported = Lude.Nothing,
      visibility = Lude.Nothing,
      imageBuilderName = Lude.Nothing,
      baseImageARN = Lude.Nothing,
      displayName = Lude.Nothing,
      description = Lude.Nothing,
      appstreamAgentVersion = Lude.Nothing,
      applications = Lude.Nothing,
      name = pName_
    }

-- | The image starts in the @PENDING@ state. If image creation succeeds, the state is @AVAILABLE@ . If image creation fails, the state is @FAILED@ .
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iState :: Lens.Lens' Image (Lude.Maybe ImageState)
iState = Lens.lens (state :: Image -> Lude.Maybe ImageState) (\s a -> s {state = a} :: Image)
{-# DEPRECATED iState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The permissions to provide to the destination AWS account for the specified image.
--
-- /Note:/ Consider using 'imagePermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iImagePermissions :: Lens.Lens' Image (Lude.Maybe ImagePermissions)
iImagePermissions = Lens.lens (imagePermissions :: Image -> Lude.Maybe ImagePermissions) (\s a -> s {imagePermissions = a} :: Image)
{-# DEPRECATED iImagePermissions "Use generic-lens or generic-optics with 'imagePermissions' instead." #-}

-- | The operating system platform of the image.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iPlatform :: Lens.Lens' Image (Lude.Maybe PlatformType)
iPlatform = Lens.lens (platform :: Image -> Lude.Maybe PlatformType) (\s a -> s {platform = a} :: Image)
{-# DEPRECATED iPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

-- | The release date of the public base image. For private images, this date is the release date of the base image from which the image was created.
--
-- /Note:/ Consider using 'publicBaseImageReleasedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iPublicBaseImageReleasedDate :: Lens.Lens' Image (Lude.Maybe Lude.Timestamp)
iPublicBaseImageReleasedDate = Lens.lens (publicBaseImageReleasedDate :: Image -> Lude.Maybe Lude.Timestamp) (\s a -> s {publicBaseImageReleasedDate = a} :: Image)
{-# DEPRECATED iPublicBaseImageReleasedDate "Use generic-lens or generic-optics with 'publicBaseImageReleasedDate' instead." #-}

-- | The reason why the last state change occurred.
--
-- /Note:/ Consider using 'stateChangeReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iStateChangeReason :: Lens.Lens' Image (Lude.Maybe ImageStateChangeReason)
iStateChangeReason = Lens.lens (stateChangeReason :: Image -> Lude.Maybe ImageStateChangeReason) (\s a -> s {stateChangeReason = a} :: Image)
{-# DEPRECATED iStateChangeReason "Use generic-lens or generic-optics with 'stateChangeReason' instead." #-}

-- | The ARN of the image.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iARN :: Lens.Lens' Image (Lude.Maybe Lude.Text)
iARN = Lens.lens (arn :: Image -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Image)
{-# DEPRECATED iARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The time the image was created.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iCreatedTime :: Lens.Lens' Image (Lude.Maybe Lude.Timestamp)
iCreatedTime = Lens.lens (createdTime :: Image -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdTime = a} :: Image)
{-# DEPRECATED iCreatedTime "Use generic-lens or generic-optics with 'createdTime' instead." #-}

-- | Indicates whether an image builder can be launched from this image.
--
-- /Note:/ Consider using 'imageBuilderSupported' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iImageBuilderSupported :: Lens.Lens' Image (Lude.Maybe Lude.Bool)
iImageBuilderSupported = Lens.lens (imageBuilderSupported :: Image -> Lude.Maybe Lude.Bool) (\s a -> s {imageBuilderSupported = a} :: Image)
{-# DEPRECATED iImageBuilderSupported "Use generic-lens or generic-optics with 'imageBuilderSupported' instead." #-}

-- | Indicates whether the image is public or private.
--
-- /Note:/ Consider using 'visibility' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iVisibility :: Lens.Lens' Image (Lude.Maybe VisibilityType)
iVisibility = Lens.lens (visibility :: Image -> Lude.Maybe VisibilityType) (\s a -> s {visibility = a} :: Image)
{-# DEPRECATED iVisibility "Use generic-lens or generic-optics with 'visibility' instead." #-}

-- | The name of the image builder that was used to create the private image. If the image is shared, this value is null.
--
-- /Note:/ Consider using 'imageBuilderName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iImageBuilderName :: Lens.Lens' Image (Lude.Maybe Lude.Text)
iImageBuilderName = Lens.lens (imageBuilderName :: Image -> Lude.Maybe Lude.Text) (\s a -> s {imageBuilderName = a} :: Image)
{-# DEPRECATED iImageBuilderName "Use generic-lens or generic-optics with 'imageBuilderName' instead." #-}

-- | The ARN of the image from which this image was created.
--
-- /Note:/ Consider using 'baseImageARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iBaseImageARN :: Lens.Lens' Image (Lude.Maybe Lude.Text)
iBaseImageARN = Lens.lens (baseImageARN :: Image -> Lude.Maybe Lude.Text) (\s a -> s {baseImageARN = a} :: Image)
{-# DEPRECATED iBaseImageARN "Use generic-lens or generic-optics with 'baseImageARN' instead." #-}

-- | The image name to display.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iDisplayName :: Lens.Lens' Image (Lude.Maybe Lude.Text)
iDisplayName = Lens.lens (displayName :: Image -> Lude.Maybe Lude.Text) (\s a -> s {displayName = a} :: Image)
{-# DEPRECATED iDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | The description to display.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iDescription :: Lens.Lens' Image (Lude.Maybe Lude.Text)
iDescription = Lens.lens (description :: Image -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: Image)
{-# DEPRECATED iDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The version of the AppStream 2.0 agent to use for instances that are launched from this image.
--
-- /Note:/ Consider using 'appstreamAgentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iAppstreamAgentVersion :: Lens.Lens' Image (Lude.Maybe Lude.Text)
iAppstreamAgentVersion = Lens.lens (appstreamAgentVersion :: Image -> Lude.Maybe Lude.Text) (\s a -> s {appstreamAgentVersion = a} :: Image)
{-# DEPRECATED iAppstreamAgentVersion "Use generic-lens or generic-optics with 'appstreamAgentVersion' instead." #-}

-- | The applications associated with the image.
--
-- /Note:/ Consider using 'applications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iApplications :: Lens.Lens' Image (Lude.Maybe [Application])
iApplications = Lens.lens (applications :: Image -> Lude.Maybe [Application]) (\s a -> s {applications = a} :: Image)
{-# DEPRECATED iApplications "Use generic-lens or generic-optics with 'applications' instead." #-}

-- | The name of the image.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iName :: Lens.Lens' Image Lude.Text
iName = Lens.lens (name :: Image -> Lude.Text) (\s a -> s {name = a} :: Image)
{-# DEPRECATED iName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON Image where
  parseJSON =
    Lude.withObject
      "Image"
      ( \x ->
          Image'
            Lude.<$> (x Lude..:? "State")
            Lude.<*> (x Lude..:? "ImagePermissions")
            Lude.<*> (x Lude..:? "Platform")
            Lude.<*> (x Lude..:? "PublicBaseImageReleasedDate")
            Lude.<*> (x Lude..:? "StateChangeReason")
            Lude.<*> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "CreatedTime")
            Lude.<*> (x Lude..:? "ImageBuilderSupported")
            Lude.<*> (x Lude..:? "Visibility")
            Lude.<*> (x Lude..:? "ImageBuilderName")
            Lude.<*> (x Lude..:? "BaseImageArn")
            Lude.<*> (x Lude..:? "DisplayName")
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "AppstreamAgentVersion")
            Lude.<*> (x Lude..:? "Applications" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "Name")
      )
