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
-- Module      : Amazonka.ImageBuilder.Types.ImageSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.ImageSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types.BuildType
import Amazonka.ImageBuilder.Types.ImageSource
import Amazonka.ImageBuilder.Types.ImageState
import Amazonka.ImageBuilder.Types.ImageType
import Amazonka.ImageBuilder.Types.OutputResources
import Amazonka.ImageBuilder.Types.Platform
import qualified Amazonka.Prelude as Prelude

-- | An image summary.
--
-- /See:/ 'newImageSummary' smart constructor.
data ImageSummary = ImageSummary'
  { -- | The Amazon Resource Name (ARN) of the image.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Indicates the type of build that created this image. The build can be
    -- initiated in the following ways:
    --
    -- -   __USER_INITIATED__ – A manual pipeline build request.
    --
    -- -   __SCHEDULED__ – A pipeline build initiated by a cron expression in
    --     the Image Builder pipeline, or from EventBridge.
    --
    -- -   __IMPORT__ – A VM import created the image to use as the base image
    --     for the recipe.
    buildType :: Prelude.Maybe BuildType,
    -- | The date on which Image Builder created this image.
    dateCreated :: Prelude.Maybe Prelude.Text,
    -- | The origin of the base image that Image Builder used to build this
    -- image.
    imageSource :: Prelude.Maybe ImageSource,
    -- | The name of the image.
    name :: Prelude.Maybe Prelude.Text,
    -- | The operating system version of the instances that launch from this
    -- image. For example, Amazon Linux 2, Ubuntu 18, or Microsoft Windows
    -- Server 2019.
    osVersion :: Prelude.Maybe Prelude.Text,
    -- | The output resources that Image Builder produced when it created this
    -- image.
    outputResources :: Prelude.Maybe OutputResources,
    -- | The owner of the image.
    owner :: Prelude.Maybe Prelude.Text,
    -- | The image operating system platform, such as Linux or Windows.
    platform :: Prelude.Maybe Platform,
    -- | The state of the image.
    state :: Prelude.Maybe ImageState,
    -- | The tags that apply to this image.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Specifies whether this image produces an AMI or a container image.
    type' :: Prelude.Maybe ImageType,
    -- | The version of the image.
    version :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImageSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'imageSummary_arn' - The Amazon Resource Name (ARN) of the image.
--
-- 'buildType', 'imageSummary_buildType' - Indicates the type of build that created this image. The build can be
-- initiated in the following ways:
--
-- -   __USER_INITIATED__ – A manual pipeline build request.
--
-- -   __SCHEDULED__ – A pipeline build initiated by a cron expression in
--     the Image Builder pipeline, or from EventBridge.
--
-- -   __IMPORT__ – A VM import created the image to use as the base image
--     for the recipe.
--
-- 'dateCreated', 'imageSummary_dateCreated' - The date on which Image Builder created this image.
--
-- 'imageSource', 'imageSummary_imageSource' - The origin of the base image that Image Builder used to build this
-- image.
--
-- 'name', 'imageSummary_name' - The name of the image.
--
-- 'osVersion', 'imageSummary_osVersion' - The operating system version of the instances that launch from this
-- image. For example, Amazon Linux 2, Ubuntu 18, or Microsoft Windows
-- Server 2019.
--
-- 'outputResources', 'imageSummary_outputResources' - The output resources that Image Builder produced when it created this
-- image.
--
-- 'owner', 'imageSummary_owner' - The owner of the image.
--
-- 'platform', 'imageSummary_platform' - The image operating system platform, such as Linux or Windows.
--
-- 'state', 'imageSummary_state' - The state of the image.
--
-- 'tags', 'imageSummary_tags' - The tags that apply to this image.
--
-- 'type'', 'imageSummary_type' - Specifies whether this image produces an AMI or a container image.
--
-- 'version', 'imageSummary_version' - The version of the image.
newImageSummary ::
  ImageSummary
newImageSummary =
  ImageSummary'
    { arn = Prelude.Nothing,
      buildType = Prelude.Nothing,
      dateCreated = Prelude.Nothing,
      imageSource = Prelude.Nothing,
      name = Prelude.Nothing,
      osVersion = Prelude.Nothing,
      outputResources = Prelude.Nothing,
      owner = Prelude.Nothing,
      platform = Prelude.Nothing,
      state = Prelude.Nothing,
      tags = Prelude.Nothing,
      type' = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the image.
imageSummary_arn :: Lens.Lens' ImageSummary (Prelude.Maybe Prelude.Text)
imageSummary_arn = Lens.lens (\ImageSummary' {arn} -> arn) (\s@ImageSummary' {} a -> s {arn = a} :: ImageSummary)

-- | Indicates the type of build that created this image. The build can be
-- initiated in the following ways:
--
-- -   __USER_INITIATED__ – A manual pipeline build request.
--
-- -   __SCHEDULED__ – A pipeline build initiated by a cron expression in
--     the Image Builder pipeline, or from EventBridge.
--
-- -   __IMPORT__ – A VM import created the image to use as the base image
--     for the recipe.
imageSummary_buildType :: Lens.Lens' ImageSummary (Prelude.Maybe BuildType)
imageSummary_buildType = Lens.lens (\ImageSummary' {buildType} -> buildType) (\s@ImageSummary' {} a -> s {buildType = a} :: ImageSummary)

-- | The date on which Image Builder created this image.
imageSummary_dateCreated :: Lens.Lens' ImageSummary (Prelude.Maybe Prelude.Text)
imageSummary_dateCreated = Lens.lens (\ImageSummary' {dateCreated} -> dateCreated) (\s@ImageSummary' {} a -> s {dateCreated = a} :: ImageSummary)

-- | The origin of the base image that Image Builder used to build this
-- image.
imageSummary_imageSource :: Lens.Lens' ImageSummary (Prelude.Maybe ImageSource)
imageSummary_imageSource = Lens.lens (\ImageSummary' {imageSource} -> imageSource) (\s@ImageSummary' {} a -> s {imageSource = a} :: ImageSummary)

-- | The name of the image.
imageSummary_name :: Lens.Lens' ImageSummary (Prelude.Maybe Prelude.Text)
imageSummary_name = Lens.lens (\ImageSummary' {name} -> name) (\s@ImageSummary' {} a -> s {name = a} :: ImageSummary)

-- | The operating system version of the instances that launch from this
-- image. For example, Amazon Linux 2, Ubuntu 18, or Microsoft Windows
-- Server 2019.
imageSummary_osVersion :: Lens.Lens' ImageSummary (Prelude.Maybe Prelude.Text)
imageSummary_osVersion = Lens.lens (\ImageSummary' {osVersion} -> osVersion) (\s@ImageSummary' {} a -> s {osVersion = a} :: ImageSummary)

-- | The output resources that Image Builder produced when it created this
-- image.
imageSummary_outputResources :: Lens.Lens' ImageSummary (Prelude.Maybe OutputResources)
imageSummary_outputResources = Lens.lens (\ImageSummary' {outputResources} -> outputResources) (\s@ImageSummary' {} a -> s {outputResources = a} :: ImageSummary)

-- | The owner of the image.
imageSummary_owner :: Lens.Lens' ImageSummary (Prelude.Maybe Prelude.Text)
imageSummary_owner = Lens.lens (\ImageSummary' {owner} -> owner) (\s@ImageSummary' {} a -> s {owner = a} :: ImageSummary)

-- | The image operating system platform, such as Linux or Windows.
imageSummary_platform :: Lens.Lens' ImageSummary (Prelude.Maybe Platform)
imageSummary_platform = Lens.lens (\ImageSummary' {platform} -> platform) (\s@ImageSummary' {} a -> s {platform = a} :: ImageSummary)

-- | The state of the image.
imageSummary_state :: Lens.Lens' ImageSummary (Prelude.Maybe ImageState)
imageSummary_state = Lens.lens (\ImageSummary' {state} -> state) (\s@ImageSummary' {} a -> s {state = a} :: ImageSummary)

-- | The tags that apply to this image.
imageSummary_tags :: Lens.Lens' ImageSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
imageSummary_tags = Lens.lens (\ImageSummary' {tags} -> tags) (\s@ImageSummary' {} a -> s {tags = a} :: ImageSummary) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether this image produces an AMI or a container image.
imageSummary_type :: Lens.Lens' ImageSummary (Prelude.Maybe ImageType)
imageSummary_type = Lens.lens (\ImageSummary' {type'} -> type') (\s@ImageSummary' {} a -> s {type' = a} :: ImageSummary)

-- | The version of the image.
imageSummary_version :: Lens.Lens' ImageSummary (Prelude.Maybe Prelude.Text)
imageSummary_version = Lens.lens (\ImageSummary' {version} -> version) (\s@ImageSummary' {} a -> s {version = a} :: ImageSummary)

instance Data.FromJSON ImageSummary where
  parseJSON =
    Data.withObject
      "ImageSummary"
      ( \x ->
          ImageSummary'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "buildType")
            Prelude.<*> (x Data..:? "dateCreated")
            Prelude.<*> (x Data..:? "imageSource")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "osVersion")
            Prelude.<*> (x Data..:? "outputResources")
            Prelude.<*> (x Data..:? "owner")
            Prelude.<*> (x Data..:? "platform")
            Prelude.<*> (x Data..:? "state")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "type")
            Prelude.<*> (x Data..:? "version")
      )

instance Prelude.Hashable ImageSummary where
  hashWithSalt _salt ImageSummary' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` buildType
      `Prelude.hashWithSalt` dateCreated
      `Prelude.hashWithSalt` imageSource
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` osVersion
      `Prelude.hashWithSalt` outputResources
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` platform
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` version

instance Prelude.NFData ImageSummary where
  rnf ImageSummary' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf buildType
      `Prelude.seq` Prelude.rnf dateCreated
      `Prelude.seq` Prelude.rnf imageSource
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf osVersion
      `Prelude.seq` Prelude.rnf outputResources
      `Prelude.seq` Prelude.rnf owner
      `Prelude.seq` Prelude.rnf platform
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf version
