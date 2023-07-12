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
-- Module      : Amazonka.AppStream.Types.Image
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppStream.Types.Image where

import Amazonka.AppStream.Types.Application
import Amazonka.AppStream.Types.ImagePermissions
import Amazonka.AppStream.Types.ImageState
import Amazonka.AppStream.Types.ImageStateChangeReason
import Amazonka.AppStream.Types.PlatformType
import Amazonka.AppStream.Types.ResourceError
import Amazonka.AppStream.Types.VisibilityType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an image.
--
-- /See:/ 'newImage' smart constructor.
data Image = Image'
  { -- | The applications associated with the image.
    applications :: Prelude.Maybe [Application],
    -- | The version of the AppStream 2.0 agent to use for instances that are
    -- launched from this image.
    appstreamAgentVersion :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the image.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the image from which this image was created.
    baseImageArn :: Prelude.Maybe Prelude.Text,
    -- | The time the image was created.
    createdTime :: Prelude.Maybe Data.POSIX,
    -- | The description to display.
    description :: Prelude.Maybe Prelude.Text,
    -- | The image name to display.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The name of the image builder that was used to create the private image.
    -- If the image is shared, this value is null.
    imageBuilderName :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether an image builder can be launched from this image.
    imageBuilderSupported :: Prelude.Maybe Prelude.Bool,
    -- | Describes the errors that are returned when a new image can\'t be
    -- created.
    imageErrors :: Prelude.Maybe [ResourceError],
    -- | The permissions to provide to the destination AWS account for the
    -- specified image.
    imagePermissions :: Prelude.Maybe ImagePermissions,
    -- | The operating system platform of the image.
    platform :: Prelude.Maybe PlatformType,
    -- | The release date of the public base image. For private images, this date
    -- is the release date of the base image from which the image was created.
    publicBaseImageReleasedDate :: Prelude.Maybe Data.POSIX,
    -- | The image starts in the @PENDING@ state. If image creation succeeds, the
    -- state is @AVAILABLE@. If image creation fails, the state is @FAILED@.
    state :: Prelude.Maybe ImageState,
    -- | The reason why the last state change occurred.
    stateChangeReason :: Prelude.Maybe ImageStateChangeReason,
    -- | Indicates whether the image is public or private.
    visibility :: Prelude.Maybe VisibilityType,
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
-- 'applications', 'image_applications' - The applications associated with the image.
--
-- 'appstreamAgentVersion', 'image_appstreamAgentVersion' - The version of the AppStream 2.0 agent to use for instances that are
-- launched from this image.
--
-- 'arn', 'image_arn' - The ARN of the image.
--
-- 'baseImageArn', 'image_baseImageArn' - The ARN of the image from which this image was created.
--
-- 'createdTime', 'image_createdTime' - The time the image was created.
--
-- 'description', 'image_description' - The description to display.
--
-- 'displayName', 'image_displayName' - The image name to display.
--
-- 'imageBuilderName', 'image_imageBuilderName' - The name of the image builder that was used to create the private image.
-- If the image is shared, this value is null.
--
-- 'imageBuilderSupported', 'image_imageBuilderSupported' - Indicates whether an image builder can be launched from this image.
--
-- 'imageErrors', 'image_imageErrors' - Describes the errors that are returned when a new image can\'t be
-- created.
--
-- 'imagePermissions', 'image_imagePermissions' - The permissions to provide to the destination AWS account for the
-- specified image.
--
-- 'platform', 'image_platform' - The operating system platform of the image.
--
-- 'publicBaseImageReleasedDate', 'image_publicBaseImageReleasedDate' - The release date of the public base image. For private images, this date
-- is the release date of the base image from which the image was created.
--
-- 'state', 'image_state' - The image starts in the @PENDING@ state. If image creation succeeds, the
-- state is @AVAILABLE@. If image creation fails, the state is @FAILED@.
--
-- 'stateChangeReason', 'image_stateChangeReason' - The reason why the last state change occurred.
--
-- 'visibility', 'image_visibility' - Indicates whether the image is public or private.
--
-- 'name', 'image_name' - The name of the image.
newImage ::
  -- | 'name'
  Prelude.Text ->
  Image
newImage pName_ =
  Image'
    { applications = Prelude.Nothing,
      appstreamAgentVersion = Prelude.Nothing,
      arn = Prelude.Nothing,
      baseImageArn = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      description = Prelude.Nothing,
      displayName = Prelude.Nothing,
      imageBuilderName = Prelude.Nothing,
      imageBuilderSupported = Prelude.Nothing,
      imageErrors = Prelude.Nothing,
      imagePermissions = Prelude.Nothing,
      platform = Prelude.Nothing,
      publicBaseImageReleasedDate = Prelude.Nothing,
      state = Prelude.Nothing,
      stateChangeReason = Prelude.Nothing,
      visibility = Prelude.Nothing,
      name = pName_
    }

-- | The applications associated with the image.
image_applications :: Lens.Lens' Image (Prelude.Maybe [Application])
image_applications = Lens.lens (\Image' {applications} -> applications) (\s@Image' {} a -> s {applications = a} :: Image) Prelude.. Lens.mapping Lens.coerced

-- | The version of the AppStream 2.0 agent to use for instances that are
-- launched from this image.
image_appstreamAgentVersion :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_appstreamAgentVersion = Lens.lens (\Image' {appstreamAgentVersion} -> appstreamAgentVersion) (\s@Image' {} a -> s {appstreamAgentVersion = a} :: Image)

-- | The ARN of the image.
image_arn :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_arn = Lens.lens (\Image' {arn} -> arn) (\s@Image' {} a -> s {arn = a} :: Image)

-- | The ARN of the image from which this image was created.
image_baseImageArn :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_baseImageArn = Lens.lens (\Image' {baseImageArn} -> baseImageArn) (\s@Image' {} a -> s {baseImageArn = a} :: Image)

-- | The time the image was created.
image_createdTime :: Lens.Lens' Image (Prelude.Maybe Prelude.UTCTime)
image_createdTime = Lens.lens (\Image' {createdTime} -> createdTime) (\s@Image' {} a -> s {createdTime = a} :: Image) Prelude.. Lens.mapping Data._Time

-- | The description to display.
image_description :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_description = Lens.lens (\Image' {description} -> description) (\s@Image' {} a -> s {description = a} :: Image)

-- | The image name to display.
image_displayName :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_displayName = Lens.lens (\Image' {displayName} -> displayName) (\s@Image' {} a -> s {displayName = a} :: Image)

-- | The name of the image builder that was used to create the private image.
-- If the image is shared, this value is null.
image_imageBuilderName :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_imageBuilderName = Lens.lens (\Image' {imageBuilderName} -> imageBuilderName) (\s@Image' {} a -> s {imageBuilderName = a} :: Image)

-- | Indicates whether an image builder can be launched from this image.
image_imageBuilderSupported :: Lens.Lens' Image (Prelude.Maybe Prelude.Bool)
image_imageBuilderSupported = Lens.lens (\Image' {imageBuilderSupported} -> imageBuilderSupported) (\s@Image' {} a -> s {imageBuilderSupported = a} :: Image)

-- | Describes the errors that are returned when a new image can\'t be
-- created.
image_imageErrors :: Lens.Lens' Image (Prelude.Maybe [ResourceError])
image_imageErrors = Lens.lens (\Image' {imageErrors} -> imageErrors) (\s@Image' {} a -> s {imageErrors = a} :: Image) Prelude.. Lens.mapping Lens.coerced

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
image_publicBaseImageReleasedDate = Lens.lens (\Image' {publicBaseImageReleasedDate} -> publicBaseImageReleasedDate) (\s@Image' {} a -> s {publicBaseImageReleasedDate = a} :: Image) Prelude.. Lens.mapping Data._Time

-- | The image starts in the @PENDING@ state. If image creation succeeds, the
-- state is @AVAILABLE@. If image creation fails, the state is @FAILED@.
image_state :: Lens.Lens' Image (Prelude.Maybe ImageState)
image_state = Lens.lens (\Image' {state} -> state) (\s@Image' {} a -> s {state = a} :: Image)

-- | The reason why the last state change occurred.
image_stateChangeReason :: Lens.Lens' Image (Prelude.Maybe ImageStateChangeReason)
image_stateChangeReason = Lens.lens (\Image' {stateChangeReason} -> stateChangeReason) (\s@Image' {} a -> s {stateChangeReason = a} :: Image)

-- | Indicates whether the image is public or private.
image_visibility :: Lens.Lens' Image (Prelude.Maybe VisibilityType)
image_visibility = Lens.lens (\Image' {visibility} -> visibility) (\s@Image' {} a -> s {visibility = a} :: Image)

-- | The name of the image.
image_name :: Lens.Lens' Image Prelude.Text
image_name = Lens.lens (\Image' {name} -> name) (\s@Image' {} a -> s {name = a} :: Image)

instance Data.FromJSON Image where
  parseJSON =
    Data.withObject
      "Image"
      ( \x ->
          Image'
            Prelude.<$> (x Data..:? "Applications" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "AppstreamAgentVersion")
            Prelude.<*> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "BaseImageArn")
            Prelude.<*> (x Data..:? "CreatedTime")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "DisplayName")
            Prelude.<*> (x Data..:? "ImageBuilderName")
            Prelude.<*> (x Data..:? "ImageBuilderSupported")
            Prelude.<*> (x Data..:? "ImageErrors" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ImagePermissions")
            Prelude.<*> (x Data..:? "Platform")
            Prelude.<*> (x Data..:? "PublicBaseImageReleasedDate")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "StateChangeReason")
            Prelude.<*> (x Data..:? "Visibility")
            Prelude.<*> (x Data..: "Name")
      )

instance Prelude.Hashable Image where
  hashWithSalt _salt Image' {..} =
    _salt
      `Prelude.hashWithSalt` applications
      `Prelude.hashWithSalt` appstreamAgentVersion
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` baseImageArn
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` imageBuilderName
      `Prelude.hashWithSalt` imageBuilderSupported
      `Prelude.hashWithSalt` imageErrors
      `Prelude.hashWithSalt` imagePermissions
      `Prelude.hashWithSalt` platform
      `Prelude.hashWithSalt` publicBaseImageReleasedDate
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` stateChangeReason
      `Prelude.hashWithSalt` visibility
      `Prelude.hashWithSalt` name

instance Prelude.NFData Image where
  rnf Image' {..} =
    Prelude.rnf applications
      `Prelude.seq` Prelude.rnf appstreamAgentVersion
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf baseImageArn
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf imageBuilderName
      `Prelude.seq` Prelude.rnf imageBuilderSupported
      `Prelude.seq` Prelude.rnf imageErrors
      `Prelude.seq` Prelude.rnf imagePermissions
      `Prelude.seq` Prelude.rnf platform
      `Prelude.seq` Prelude.rnf publicBaseImageReleasedDate
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf stateChangeReason
      `Prelude.seq` Prelude.rnf visibility
      `Prelude.seq` Prelude.rnf name
