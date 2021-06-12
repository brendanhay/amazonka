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
import Network.AWS.AppStream.Types.VisibilityType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes an image.
--
-- /See:/ 'newImage' smart constructor.
data Image = Image'
  { -- | The permissions to provide to the destination AWS account for the
    -- specified image.
    imagePermissions :: Core.Maybe ImagePermissions,
    -- | The operating system platform of the image.
    platform :: Core.Maybe PlatformType,
    -- | The name of the image builder that was used to create the private image.
    -- If the image is shared, this value is null.
    imageBuilderName :: Core.Maybe Core.Text,
    -- | The ARN of the image.
    arn :: Core.Maybe Core.Text,
    -- | The reason why the last state change occurred.
    stateChangeReason :: Core.Maybe ImageStateChangeReason,
    -- | The time the image was created.
    createdTime :: Core.Maybe Core.POSIX,
    -- | The image starts in the @PENDING@ state. If image creation succeeds, the
    -- state is @AVAILABLE@. If image creation fails, the state is @FAILED@.
    state :: Core.Maybe ImageState,
    -- | The ARN of the image from which this image was created.
    baseImageArn :: Core.Maybe Core.Text,
    -- | The applications associated with the image.
    applications :: Core.Maybe [Application],
    -- | Indicates whether the image is public or private.
    visibility :: Core.Maybe VisibilityType,
    -- | The version of the AppStream 2.0 agent to use for instances that are
    -- launched from this image.
    appstreamAgentVersion :: Core.Maybe Core.Text,
    -- | The description to display.
    description :: Core.Maybe Core.Text,
    -- | Indicates whether an image builder can be launched from this image.
    imageBuilderSupported :: Core.Maybe Core.Bool,
    -- | The image name to display.
    displayName :: Core.Maybe Core.Text,
    -- | The release date of the public base image. For private images, this date
    -- is the release date of the base image from which the image was created.
    publicBaseImageReleasedDate :: Core.Maybe Core.POSIX,
    -- | The name of the image.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Image' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imagePermissions', 'image_imagePermissions' - The permissions to provide to the destination AWS account for the
-- specified image.
--
-- 'platform', 'image_platform' - The operating system platform of the image.
--
-- 'imageBuilderName', 'image_imageBuilderName' - The name of the image builder that was used to create the private image.
-- If the image is shared, this value is null.
--
-- 'arn', 'image_arn' - The ARN of the image.
--
-- 'stateChangeReason', 'image_stateChangeReason' - The reason why the last state change occurred.
--
-- 'createdTime', 'image_createdTime' - The time the image was created.
--
-- 'state', 'image_state' - The image starts in the @PENDING@ state. If image creation succeeds, the
-- state is @AVAILABLE@. If image creation fails, the state is @FAILED@.
--
-- 'baseImageArn', 'image_baseImageArn' - The ARN of the image from which this image was created.
--
-- 'applications', 'image_applications' - The applications associated with the image.
--
-- 'visibility', 'image_visibility' - Indicates whether the image is public or private.
--
-- 'appstreamAgentVersion', 'image_appstreamAgentVersion' - The version of the AppStream 2.0 agent to use for instances that are
-- launched from this image.
--
-- 'description', 'image_description' - The description to display.
--
-- 'imageBuilderSupported', 'image_imageBuilderSupported' - Indicates whether an image builder can be launched from this image.
--
-- 'displayName', 'image_displayName' - The image name to display.
--
-- 'publicBaseImageReleasedDate', 'image_publicBaseImageReleasedDate' - The release date of the public base image. For private images, this date
-- is the release date of the base image from which the image was created.
--
-- 'name', 'image_name' - The name of the image.
newImage ::
  -- | 'name'
  Core.Text ->
  Image
newImage pName_ =
  Image'
    { imagePermissions = Core.Nothing,
      platform = Core.Nothing,
      imageBuilderName = Core.Nothing,
      arn = Core.Nothing,
      stateChangeReason = Core.Nothing,
      createdTime = Core.Nothing,
      state = Core.Nothing,
      baseImageArn = Core.Nothing,
      applications = Core.Nothing,
      visibility = Core.Nothing,
      appstreamAgentVersion = Core.Nothing,
      description = Core.Nothing,
      imageBuilderSupported = Core.Nothing,
      displayName = Core.Nothing,
      publicBaseImageReleasedDate = Core.Nothing,
      name = pName_
    }

-- | The permissions to provide to the destination AWS account for the
-- specified image.
image_imagePermissions :: Lens.Lens' Image (Core.Maybe ImagePermissions)
image_imagePermissions = Lens.lens (\Image' {imagePermissions} -> imagePermissions) (\s@Image' {} a -> s {imagePermissions = a} :: Image)

-- | The operating system platform of the image.
image_platform :: Lens.Lens' Image (Core.Maybe PlatformType)
image_platform = Lens.lens (\Image' {platform} -> platform) (\s@Image' {} a -> s {platform = a} :: Image)

-- | The name of the image builder that was used to create the private image.
-- If the image is shared, this value is null.
image_imageBuilderName :: Lens.Lens' Image (Core.Maybe Core.Text)
image_imageBuilderName = Lens.lens (\Image' {imageBuilderName} -> imageBuilderName) (\s@Image' {} a -> s {imageBuilderName = a} :: Image)

-- | The ARN of the image.
image_arn :: Lens.Lens' Image (Core.Maybe Core.Text)
image_arn = Lens.lens (\Image' {arn} -> arn) (\s@Image' {} a -> s {arn = a} :: Image)

-- | The reason why the last state change occurred.
image_stateChangeReason :: Lens.Lens' Image (Core.Maybe ImageStateChangeReason)
image_stateChangeReason = Lens.lens (\Image' {stateChangeReason} -> stateChangeReason) (\s@Image' {} a -> s {stateChangeReason = a} :: Image)

-- | The time the image was created.
image_createdTime :: Lens.Lens' Image (Core.Maybe Core.UTCTime)
image_createdTime = Lens.lens (\Image' {createdTime} -> createdTime) (\s@Image' {} a -> s {createdTime = a} :: Image) Core.. Lens.mapping Core._Time

-- | The image starts in the @PENDING@ state. If image creation succeeds, the
-- state is @AVAILABLE@. If image creation fails, the state is @FAILED@.
image_state :: Lens.Lens' Image (Core.Maybe ImageState)
image_state = Lens.lens (\Image' {state} -> state) (\s@Image' {} a -> s {state = a} :: Image)

-- | The ARN of the image from which this image was created.
image_baseImageArn :: Lens.Lens' Image (Core.Maybe Core.Text)
image_baseImageArn = Lens.lens (\Image' {baseImageArn} -> baseImageArn) (\s@Image' {} a -> s {baseImageArn = a} :: Image)

-- | The applications associated with the image.
image_applications :: Lens.Lens' Image (Core.Maybe [Application])
image_applications = Lens.lens (\Image' {applications} -> applications) (\s@Image' {} a -> s {applications = a} :: Image) Core.. Lens.mapping Lens._Coerce

-- | Indicates whether the image is public or private.
image_visibility :: Lens.Lens' Image (Core.Maybe VisibilityType)
image_visibility = Lens.lens (\Image' {visibility} -> visibility) (\s@Image' {} a -> s {visibility = a} :: Image)

-- | The version of the AppStream 2.0 agent to use for instances that are
-- launched from this image.
image_appstreamAgentVersion :: Lens.Lens' Image (Core.Maybe Core.Text)
image_appstreamAgentVersion = Lens.lens (\Image' {appstreamAgentVersion} -> appstreamAgentVersion) (\s@Image' {} a -> s {appstreamAgentVersion = a} :: Image)

-- | The description to display.
image_description :: Lens.Lens' Image (Core.Maybe Core.Text)
image_description = Lens.lens (\Image' {description} -> description) (\s@Image' {} a -> s {description = a} :: Image)

-- | Indicates whether an image builder can be launched from this image.
image_imageBuilderSupported :: Lens.Lens' Image (Core.Maybe Core.Bool)
image_imageBuilderSupported = Lens.lens (\Image' {imageBuilderSupported} -> imageBuilderSupported) (\s@Image' {} a -> s {imageBuilderSupported = a} :: Image)

-- | The image name to display.
image_displayName :: Lens.Lens' Image (Core.Maybe Core.Text)
image_displayName = Lens.lens (\Image' {displayName} -> displayName) (\s@Image' {} a -> s {displayName = a} :: Image)

-- | The release date of the public base image. For private images, this date
-- is the release date of the base image from which the image was created.
image_publicBaseImageReleasedDate :: Lens.Lens' Image (Core.Maybe Core.UTCTime)
image_publicBaseImageReleasedDate = Lens.lens (\Image' {publicBaseImageReleasedDate} -> publicBaseImageReleasedDate) (\s@Image' {} a -> s {publicBaseImageReleasedDate = a} :: Image) Core.. Lens.mapping Core._Time

-- | The name of the image.
image_name :: Lens.Lens' Image Core.Text
image_name = Lens.lens (\Image' {name} -> name) (\s@Image' {} a -> s {name = a} :: Image)

instance Core.FromJSON Image where
  parseJSON =
    Core.withObject
      "Image"
      ( \x ->
          Image'
            Core.<$> (x Core..:? "ImagePermissions")
            Core.<*> (x Core..:? "Platform")
            Core.<*> (x Core..:? "ImageBuilderName")
            Core.<*> (x Core..:? "Arn")
            Core.<*> (x Core..:? "StateChangeReason")
            Core.<*> (x Core..:? "CreatedTime")
            Core.<*> (x Core..:? "State")
            Core.<*> (x Core..:? "BaseImageArn")
            Core.<*> (x Core..:? "Applications" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Visibility")
            Core.<*> (x Core..:? "AppstreamAgentVersion")
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "ImageBuilderSupported")
            Core.<*> (x Core..:? "DisplayName")
            Core.<*> (x Core..:? "PublicBaseImageReleasedDate")
            Core.<*> (x Core..: "Name")
      )

instance Core.Hashable Image

instance Core.NFData Image
