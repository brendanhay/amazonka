{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.Image
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.Image where

import Network.AWS.AppStream.Types.Application
import Network.AWS.AppStream.Types.ImagePermissions
import Network.AWS.AppStream.Types.ImageState
import Network.AWS.AppStream.Types.ImageStateChangeReason
import Network.AWS.AppStream.Types.PlatformType
import Network.AWS.AppStream.Types.ResourceError
import Network.AWS.AppStream.Types.VisibilityType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an image.
--
-- /See:/ 'newImage' smart constructor.
data Image = Image'
  { -- | The image starts in the @PENDING@ state. If image creation succeeds, the
    -- state is @AVAILABLE@. If image creation fails, the state is @FAILED@.
    state :: Prelude.Maybe ImageState,
    -- | The permissions to provide to the destination AWS account for the
    -- specified image.
    imagePermissions :: Prelude.Maybe ImagePermissions,
    -- | The operating system platform of the image.
    platform :: Prelude.Maybe PlatformType,
    -- | The release date of the public base image. For private images, this date
    -- is the release date of the base image from which the image was created.
    publicBaseImageReleasedDate :: Prelude.Maybe Core.POSIX,
    -- | The reason why the last state change occurred.
    stateChangeReason :: Prelude.Maybe ImageStateChangeReason,
    -- | The ARN of the image.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time the image was created.
    createdTime :: Prelude.Maybe Core.POSIX,
    -- | Indicates whether an image builder can be launched from this image.
    imageBuilderSupported :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether the image is public or private.
    visibility :: Prelude.Maybe VisibilityType,
    -- | The name of the image builder that was used to create the private image.
    -- If the image is shared, this value is null.
    imageBuilderName :: Prelude.Maybe Prelude.Text,
    -- | Describes the errors that are returned when a new image can\'t be
    -- created.
    imageErrors :: Prelude.Maybe [ResourceError],
    -- | The ARN of the image from which this image was created.
    baseImageArn :: Prelude.Maybe Prelude.Text,
    -- | The image name to display.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The description to display.
    description :: Prelude.Maybe Prelude.Text,
    -- | The version of the AppStream 2.0 agent to use for instances that are
    -- launched from this image.
    appstreamAgentVersion :: Prelude.Maybe Prelude.Text,
    -- | The applications associated with the image.
    applications :: Prelude.Maybe [Application],
    -- | The name of the image.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Image' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'image_state' - The image starts in the @PENDING@ state. If image creation succeeds, the
-- state is @AVAILABLE@. If image creation fails, the state is @FAILED@.
--
-- 'imagePermissions', 'image_imagePermissions' - The permissions to provide to the destination AWS account for the
-- specified image.
--
-- 'platform', 'image_platform' - The operating system platform of the image.
--
-- 'publicBaseImageReleasedDate', 'image_publicBaseImageReleasedDate' - The release date of the public base image. For private images, this date
-- is the release date of the base image from which the image was created.
--
-- 'stateChangeReason', 'image_stateChangeReason' - The reason why the last state change occurred.
--
-- 'arn', 'image_arn' - The ARN of the image.
--
-- 'createdTime', 'image_createdTime' - The time the image was created.
--
-- 'imageBuilderSupported', 'image_imageBuilderSupported' - Indicates whether an image builder can be launched from this image.
--
-- 'visibility', 'image_visibility' - Indicates whether the image is public or private.
--
-- 'imageBuilderName', 'image_imageBuilderName' - The name of the image builder that was used to create the private image.
-- If the image is shared, this value is null.
--
-- 'imageErrors', 'image_imageErrors' - Describes the errors that are returned when a new image can\'t be
-- created.
--
-- 'baseImageArn', 'image_baseImageArn' - The ARN of the image from which this image was created.
--
-- 'displayName', 'image_displayName' - The image name to display.
--
-- 'description', 'image_description' - The description to display.
--
-- 'appstreamAgentVersion', 'image_appstreamAgentVersion' - The version of the AppStream 2.0 agent to use for instances that are
-- launched from this image.
--
-- 'applications', 'image_applications' - The applications associated with the image.
--
-- 'name', 'image_name' - The name of the image.
newImage ::
  -- | 'name'
  Prelude.Text ->
  Image
newImage pName_ =
  Image'
    { state = Prelude.Nothing,
      imagePermissions = Prelude.Nothing,
      platform = Prelude.Nothing,
      publicBaseImageReleasedDate = Prelude.Nothing,
      stateChangeReason = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      imageBuilderSupported = Prelude.Nothing,
      visibility = Prelude.Nothing,
      imageBuilderName = Prelude.Nothing,
      imageErrors = Prelude.Nothing,
      baseImageArn = Prelude.Nothing,
      displayName = Prelude.Nothing,
      description = Prelude.Nothing,
      appstreamAgentVersion = Prelude.Nothing,
      applications = Prelude.Nothing,
      name = pName_
    }

-- | The image starts in the @PENDING@ state. If image creation succeeds, the
-- state is @AVAILABLE@. If image creation fails, the state is @FAILED@.
image_state :: Lens.Lens' Image (Prelude.Maybe ImageState)
image_state = Lens.lens (\Image' {state} -> state) (\s@Image' {} a -> s {state = a} :: Image)

-- | The permissions to provide to the destination AWS account for the
-- specified image.
image_imagePermissions :: Lens.Lens' Image (Prelude.Maybe ImagePermissions)
image_imagePermissions = Lens.lens (\Image' {imagePermissions} -> imagePermissions) (\s@Image' {} a -> s {imagePermissions = a} :: Image)

-- | The operating system platform of the image.
image_platform :: Lens.Lens' Image (Prelude.Maybe PlatformType)
image_platform = Lens.lens (\Image' {platform} -> platform) (\s@Image' {} a -> s {platform = a} :: Image)

-- | The release date of the public base image. For private images, this date
-- is the release date of the base image from which the image was created.
image_publicBaseImageReleasedDate :: Lens.Lens' Image (Prelude.Maybe Prelude.UTCTime)
image_publicBaseImageReleasedDate = Lens.lens (\Image' {publicBaseImageReleasedDate} -> publicBaseImageReleasedDate) (\s@Image' {} a -> s {publicBaseImageReleasedDate = a} :: Image) Prelude.. Lens.mapping Core._Time

-- | The reason why the last state change occurred.
image_stateChangeReason :: Lens.Lens' Image (Prelude.Maybe ImageStateChangeReason)
image_stateChangeReason = Lens.lens (\Image' {stateChangeReason} -> stateChangeReason) (\s@Image' {} a -> s {stateChangeReason = a} :: Image)

-- | The ARN of the image.
image_arn :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_arn = Lens.lens (\Image' {arn} -> arn) (\s@Image' {} a -> s {arn = a} :: Image)

-- | The time the image was created.
image_createdTime :: Lens.Lens' Image (Prelude.Maybe Prelude.UTCTime)
image_createdTime = Lens.lens (\Image' {createdTime} -> createdTime) (\s@Image' {} a -> s {createdTime = a} :: Image) Prelude.. Lens.mapping Core._Time

-- | Indicates whether an image builder can be launched from this image.
image_imageBuilderSupported :: Lens.Lens' Image (Prelude.Maybe Prelude.Bool)
image_imageBuilderSupported = Lens.lens (\Image' {imageBuilderSupported} -> imageBuilderSupported) (\s@Image' {} a -> s {imageBuilderSupported = a} :: Image)

-- | Indicates whether the image is public or private.
image_visibility :: Lens.Lens' Image (Prelude.Maybe VisibilityType)
image_visibility = Lens.lens (\Image' {visibility} -> visibility) (\s@Image' {} a -> s {visibility = a} :: Image)

-- | The name of the image builder that was used to create the private image.
-- If the image is shared, this value is null.
image_imageBuilderName :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_imageBuilderName = Lens.lens (\Image' {imageBuilderName} -> imageBuilderName) (\s@Image' {} a -> s {imageBuilderName = a} :: Image)

-- | Describes the errors that are returned when a new image can\'t be
-- created.
image_imageErrors :: Lens.Lens' Image (Prelude.Maybe [ResourceError])
image_imageErrors = Lens.lens (\Image' {imageErrors} -> imageErrors) (\s@Image' {} a -> s {imageErrors = a} :: Image) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the image from which this image was created.
image_baseImageArn :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_baseImageArn = Lens.lens (\Image' {baseImageArn} -> baseImageArn) (\s@Image' {} a -> s {baseImageArn = a} :: Image)

-- | The image name to display.
image_displayName :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_displayName = Lens.lens (\Image' {displayName} -> displayName) (\s@Image' {} a -> s {displayName = a} :: Image)

-- | The description to display.
image_description :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_description = Lens.lens (\Image' {description} -> description) (\s@Image' {} a -> s {description = a} :: Image)

-- | The version of the AppStream 2.0 agent to use for instances that are
-- launched from this image.
image_appstreamAgentVersion :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_appstreamAgentVersion = Lens.lens (\Image' {appstreamAgentVersion} -> appstreamAgentVersion) (\s@Image' {} a -> s {appstreamAgentVersion = a} :: Image)

-- | The applications associated with the image.
image_applications :: Lens.Lens' Image (Prelude.Maybe [Application])
image_applications = Lens.lens (\Image' {applications} -> applications) (\s@Image' {} a -> s {applications = a} :: Image) Prelude.. Lens.mapping Lens.coerced

-- | The name of the image.
image_name :: Lens.Lens' Image Prelude.Text
image_name = Lens.lens (\Image' {name} -> name) (\s@Image' {} a -> s {name = a} :: Image)

instance Core.FromJSON Image where
  parseJSON =
    Core.withObject
      "Image"
      ( \x ->
          Image'
            Prelude.<$> (x Core..:? "State")
            Prelude.<*> (x Core..:? "ImagePermissions")
            Prelude.<*> (x Core..:? "Platform")
            Prelude.<*> (x Core..:? "PublicBaseImageReleasedDate")
            Prelude.<*> (x Core..:? "StateChangeReason")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "CreatedTime")
            Prelude.<*> (x Core..:? "ImageBuilderSupported")
            Prelude.<*> (x Core..:? "Visibility")
            Prelude.<*> (x Core..:? "ImageBuilderName")
            Prelude.<*> (x Core..:? "ImageErrors" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "BaseImageArn")
            Prelude.<*> (x Core..:? "DisplayName")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "AppstreamAgentVersion")
            Prelude.<*> (x Core..:? "Applications" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "Name")
      )

instance Prelude.Hashable Image

instance Prelude.NFData Image
