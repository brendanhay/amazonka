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
-- Module      : Amazonka.ImageBuilder.Types.Image
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.Image where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types.BuildType
import Amazonka.ImageBuilder.Types.ContainerRecipe
import Amazonka.ImageBuilder.Types.DistributionConfiguration
import Amazonka.ImageBuilder.Types.ImageRecipe
import Amazonka.ImageBuilder.Types.ImageScanState
import Amazonka.ImageBuilder.Types.ImageScanningConfiguration
import Amazonka.ImageBuilder.Types.ImageSource
import Amazonka.ImageBuilder.Types.ImageState
import Amazonka.ImageBuilder.Types.ImageTestsConfiguration
import Amazonka.ImageBuilder.Types.ImageType
import Amazonka.ImageBuilder.Types.InfrastructureConfiguration
import Amazonka.ImageBuilder.Types.OutputResources
import Amazonka.ImageBuilder.Types.Platform
import qualified Amazonka.Prelude as Prelude

-- | An Image Builder image. You must specify exactly one recipe for the
-- image – either a container recipe (@containerRecipe@), which creates a
-- container image, or an image recipe (@imageRecipe@), which creates an
-- AMI.
--
-- /See:/ 'newImage' smart constructor.
data Image = Image'
  { -- | The Amazon Resource Name (ARN) of the image.
    --
    -- Semantic versioning is included in each object\'s Amazon Resource Name
    -- (ARN), at the level that applies to that object as follows:
    --
    -- 1.  Versionless ARNs and Name ARNs do not include specific values in any
    --     of the nodes. The nodes are either left off entirely, or they are
    --     specified as wildcards, for example: x.x.x.
    --
    -- 2.  Version ARNs have only the first three nodes:
    --     \<major>.\<minor>.\<patch>
    --
    -- 3.  Build version ARNs have all four nodes, and point to a specific
    --     build for a specific version of an object.
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
    -- | For container images, this is the container recipe that Image Builder
    -- used to create the image. For images that distribute an AMI, this is
    -- empty.
    containerRecipe :: Prelude.Maybe ContainerRecipe,
    -- | The date on which Image Builder created this image.
    dateCreated :: Prelude.Maybe Prelude.Text,
    -- | The distribution configuration that Image Builder used to create this
    -- image.
    distributionConfiguration :: Prelude.Maybe DistributionConfiguration,
    -- | Indicates whether Image Builder collects additional information about
    -- the image, such as the operating system (OS) version and package list.
    enhancedImageMetadataEnabled :: Prelude.Maybe Prelude.Bool,
    -- | For images that distribute an AMI, this is the image recipe that Image
    -- Builder used to create the image. For container images, this is empty.
    imageRecipe :: Prelude.Maybe ImageRecipe,
    -- | Contains settings for vulnerability scans.
    imageScanningConfiguration :: Prelude.Maybe ImageScanningConfiguration,
    -- | The origin of the base image that Image Builder used to build this
    -- image.
    imageSource :: Prelude.Maybe ImageSource,
    -- | The image tests that ran when that Image Builder created this image.
    imageTestsConfiguration :: Prelude.Maybe ImageTestsConfiguration,
    -- | The infrastructure that Image Builder used to create this image.
    infrastructureConfiguration :: Prelude.Maybe InfrastructureConfiguration,
    -- | The name of the image.
    name :: Prelude.Maybe Prelude.Text,
    -- | The operating system version for instances that launch from this image.
    -- For example, Amazon Linux 2, Ubuntu 18, or Microsoft Windows Server
    -- 2019.
    osVersion :: Prelude.Maybe Prelude.Text,
    -- | The output resources that Image Builder produces for this image.
    outputResources :: Prelude.Maybe OutputResources,
    -- | The image operating system platform, such as Linux or Windows.
    platform :: Prelude.Maybe Platform,
    -- | Contains information about the current state of scans for this image.
    scanState :: Prelude.Maybe ImageScanState,
    -- | The Amazon Resource Name (ARN) of the image pipeline that created this
    -- image.
    sourcePipelineArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the image pipeline that created this image.
    sourcePipelineName :: Prelude.Maybe Prelude.Text,
    -- | The state of the image.
    state :: Prelude.Maybe ImageState,
    -- | The tags that apply to this image.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Specifies whether this image produces an AMI or a container image.
    type' :: Prelude.Maybe ImageType,
    -- | The semantic version of the image.
    --
    -- The semantic version has four nodes:
    -- \<major>.\<minor>.\<patch>\/\<build>. You can assign values for the
    -- first three, and can filter on all of them.
    --
    -- __Assignment:__ For the first three nodes you can assign any positive
    -- integer value, including zero, with an upper limit of 2^30-1, or
    -- 1073741823 for each node. Image Builder automatically assigns the build
    -- number to the fourth node.
    --
    -- __Patterns:__ You can use any numeric pattern that adheres to the
    -- assignment requirements for the nodes that you can assign. For example,
    -- you might choose a software version pattern, such as 1.0.0, or a date,
    -- such as 2021.01.01.
    --
    -- __Filtering:__ With semantic versioning, you have the flexibility to use
    -- wildcards (x) to specify the most recent versions or nodes when
    -- selecting the base image or components for your recipe. When you use a
    -- wildcard in any node, all nodes to the right of the first wildcard must
    -- also be wildcards.
    version :: Prelude.Maybe Prelude.Text
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
-- 'arn', 'image_arn' - The Amazon Resource Name (ARN) of the image.
--
-- Semantic versioning is included in each object\'s Amazon Resource Name
-- (ARN), at the level that applies to that object as follows:
--
-- 1.  Versionless ARNs and Name ARNs do not include specific values in any
--     of the nodes. The nodes are either left off entirely, or they are
--     specified as wildcards, for example: x.x.x.
--
-- 2.  Version ARNs have only the first three nodes:
--     \<major>.\<minor>.\<patch>
--
-- 3.  Build version ARNs have all four nodes, and point to a specific
--     build for a specific version of an object.
--
-- 'buildType', 'image_buildType' - Indicates the type of build that created this image. The build can be
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
-- 'containerRecipe', 'image_containerRecipe' - For container images, this is the container recipe that Image Builder
-- used to create the image. For images that distribute an AMI, this is
-- empty.
--
-- 'dateCreated', 'image_dateCreated' - The date on which Image Builder created this image.
--
-- 'distributionConfiguration', 'image_distributionConfiguration' - The distribution configuration that Image Builder used to create this
-- image.
--
-- 'enhancedImageMetadataEnabled', 'image_enhancedImageMetadataEnabled' - Indicates whether Image Builder collects additional information about
-- the image, such as the operating system (OS) version and package list.
--
-- 'imageRecipe', 'image_imageRecipe' - For images that distribute an AMI, this is the image recipe that Image
-- Builder used to create the image. For container images, this is empty.
--
-- 'imageScanningConfiguration', 'image_imageScanningConfiguration' - Contains settings for vulnerability scans.
--
-- 'imageSource', 'image_imageSource' - The origin of the base image that Image Builder used to build this
-- image.
--
-- 'imageTestsConfiguration', 'image_imageTestsConfiguration' - The image tests that ran when that Image Builder created this image.
--
-- 'infrastructureConfiguration', 'image_infrastructureConfiguration' - The infrastructure that Image Builder used to create this image.
--
-- 'name', 'image_name' - The name of the image.
--
-- 'osVersion', 'image_osVersion' - The operating system version for instances that launch from this image.
-- For example, Amazon Linux 2, Ubuntu 18, or Microsoft Windows Server
-- 2019.
--
-- 'outputResources', 'image_outputResources' - The output resources that Image Builder produces for this image.
--
-- 'platform', 'image_platform' - The image operating system platform, such as Linux or Windows.
--
-- 'scanState', 'image_scanState' - Contains information about the current state of scans for this image.
--
-- 'sourcePipelineArn', 'image_sourcePipelineArn' - The Amazon Resource Name (ARN) of the image pipeline that created this
-- image.
--
-- 'sourcePipelineName', 'image_sourcePipelineName' - The name of the image pipeline that created this image.
--
-- 'state', 'image_state' - The state of the image.
--
-- 'tags', 'image_tags' - The tags that apply to this image.
--
-- 'type'', 'image_type' - Specifies whether this image produces an AMI or a container image.
--
-- 'version', 'image_version' - The semantic version of the image.
--
-- The semantic version has four nodes:
-- \<major>.\<minor>.\<patch>\/\<build>. You can assign values for the
-- first three, and can filter on all of them.
--
-- __Assignment:__ For the first three nodes you can assign any positive
-- integer value, including zero, with an upper limit of 2^30-1, or
-- 1073741823 for each node. Image Builder automatically assigns the build
-- number to the fourth node.
--
-- __Patterns:__ You can use any numeric pattern that adheres to the
-- assignment requirements for the nodes that you can assign. For example,
-- you might choose a software version pattern, such as 1.0.0, or a date,
-- such as 2021.01.01.
--
-- __Filtering:__ With semantic versioning, you have the flexibility to use
-- wildcards (x) to specify the most recent versions or nodes when
-- selecting the base image or components for your recipe. When you use a
-- wildcard in any node, all nodes to the right of the first wildcard must
-- also be wildcards.
newImage ::
  Image
newImage =
  Image'
    { arn = Prelude.Nothing,
      buildType = Prelude.Nothing,
      containerRecipe = Prelude.Nothing,
      dateCreated = Prelude.Nothing,
      distributionConfiguration = Prelude.Nothing,
      enhancedImageMetadataEnabled = Prelude.Nothing,
      imageRecipe = Prelude.Nothing,
      imageScanningConfiguration = Prelude.Nothing,
      imageSource = Prelude.Nothing,
      imageTestsConfiguration = Prelude.Nothing,
      infrastructureConfiguration = Prelude.Nothing,
      name = Prelude.Nothing,
      osVersion = Prelude.Nothing,
      outputResources = Prelude.Nothing,
      platform = Prelude.Nothing,
      scanState = Prelude.Nothing,
      sourcePipelineArn = Prelude.Nothing,
      sourcePipelineName = Prelude.Nothing,
      state = Prelude.Nothing,
      tags = Prelude.Nothing,
      type' = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the image.
--
-- Semantic versioning is included in each object\'s Amazon Resource Name
-- (ARN), at the level that applies to that object as follows:
--
-- 1.  Versionless ARNs and Name ARNs do not include specific values in any
--     of the nodes. The nodes are either left off entirely, or they are
--     specified as wildcards, for example: x.x.x.
--
-- 2.  Version ARNs have only the first three nodes:
--     \<major>.\<minor>.\<patch>
--
-- 3.  Build version ARNs have all four nodes, and point to a specific
--     build for a specific version of an object.
image_arn :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_arn = Lens.lens (\Image' {arn} -> arn) (\s@Image' {} a -> s {arn = a} :: Image)

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
image_buildType :: Lens.Lens' Image (Prelude.Maybe BuildType)
image_buildType = Lens.lens (\Image' {buildType} -> buildType) (\s@Image' {} a -> s {buildType = a} :: Image)

-- | For container images, this is the container recipe that Image Builder
-- used to create the image. For images that distribute an AMI, this is
-- empty.
image_containerRecipe :: Lens.Lens' Image (Prelude.Maybe ContainerRecipe)
image_containerRecipe = Lens.lens (\Image' {containerRecipe} -> containerRecipe) (\s@Image' {} a -> s {containerRecipe = a} :: Image)

-- | The date on which Image Builder created this image.
image_dateCreated :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_dateCreated = Lens.lens (\Image' {dateCreated} -> dateCreated) (\s@Image' {} a -> s {dateCreated = a} :: Image)

-- | The distribution configuration that Image Builder used to create this
-- image.
image_distributionConfiguration :: Lens.Lens' Image (Prelude.Maybe DistributionConfiguration)
image_distributionConfiguration = Lens.lens (\Image' {distributionConfiguration} -> distributionConfiguration) (\s@Image' {} a -> s {distributionConfiguration = a} :: Image)

-- | Indicates whether Image Builder collects additional information about
-- the image, such as the operating system (OS) version and package list.
image_enhancedImageMetadataEnabled :: Lens.Lens' Image (Prelude.Maybe Prelude.Bool)
image_enhancedImageMetadataEnabled = Lens.lens (\Image' {enhancedImageMetadataEnabled} -> enhancedImageMetadataEnabled) (\s@Image' {} a -> s {enhancedImageMetadataEnabled = a} :: Image)

-- | For images that distribute an AMI, this is the image recipe that Image
-- Builder used to create the image. For container images, this is empty.
image_imageRecipe :: Lens.Lens' Image (Prelude.Maybe ImageRecipe)
image_imageRecipe = Lens.lens (\Image' {imageRecipe} -> imageRecipe) (\s@Image' {} a -> s {imageRecipe = a} :: Image)

-- | Contains settings for vulnerability scans.
image_imageScanningConfiguration :: Lens.Lens' Image (Prelude.Maybe ImageScanningConfiguration)
image_imageScanningConfiguration = Lens.lens (\Image' {imageScanningConfiguration} -> imageScanningConfiguration) (\s@Image' {} a -> s {imageScanningConfiguration = a} :: Image)

-- | The origin of the base image that Image Builder used to build this
-- image.
image_imageSource :: Lens.Lens' Image (Prelude.Maybe ImageSource)
image_imageSource = Lens.lens (\Image' {imageSource} -> imageSource) (\s@Image' {} a -> s {imageSource = a} :: Image)

-- | The image tests that ran when that Image Builder created this image.
image_imageTestsConfiguration :: Lens.Lens' Image (Prelude.Maybe ImageTestsConfiguration)
image_imageTestsConfiguration = Lens.lens (\Image' {imageTestsConfiguration} -> imageTestsConfiguration) (\s@Image' {} a -> s {imageTestsConfiguration = a} :: Image)

-- | The infrastructure that Image Builder used to create this image.
image_infrastructureConfiguration :: Lens.Lens' Image (Prelude.Maybe InfrastructureConfiguration)
image_infrastructureConfiguration = Lens.lens (\Image' {infrastructureConfiguration} -> infrastructureConfiguration) (\s@Image' {} a -> s {infrastructureConfiguration = a} :: Image)

-- | The name of the image.
image_name :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_name = Lens.lens (\Image' {name} -> name) (\s@Image' {} a -> s {name = a} :: Image)

-- | The operating system version for instances that launch from this image.
-- For example, Amazon Linux 2, Ubuntu 18, or Microsoft Windows Server
-- 2019.
image_osVersion :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_osVersion = Lens.lens (\Image' {osVersion} -> osVersion) (\s@Image' {} a -> s {osVersion = a} :: Image)

-- | The output resources that Image Builder produces for this image.
image_outputResources :: Lens.Lens' Image (Prelude.Maybe OutputResources)
image_outputResources = Lens.lens (\Image' {outputResources} -> outputResources) (\s@Image' {} a -> s {outputResources = a} :: Image)

-- | The image operating system platform, such as Linux or Windows.
image_platform :: Lens.Lens' Image (Prelude.Maybe Platform)
image_platform = Lens.lens (\Image' {platform} -> platform) (\s@Image' {} a -> s {platform = a} :: Image)

-- | Contains information about the current state of scans for this image.
image_scanState :: Lens.Lens' Image (Prelude.Maybe ImageScanState)
image_scanState = Lens.lens (\Image' {scanState} -> scanState) (\s@Image' {} a -> s {scanState = a} :: Image)

-- | The Amazon Resource Name (ARN) of the image pipeline that created this
-- image.
image_sourcePipelineArn :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_sourcePipelineArn = Lens.lens (\Image' {sourcePipelineArn} -> sourcePipelineArn) (\s@Image' {} a -> s {sourcePipelineArn = a} :: Image)

-- | The name of the image pipeline that created this image.
image_sourcePipelineName :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_sourcePipelineName = Lens.lens (\Image' {sourcePipelineName} -> sourcePipelineName) (\s@Image' {} a -> s {sourcePipelineName = a} :: Image)

-- | The state of the image.
image_state :: Lens.Lens' Image (Prelude.Maybe ImageState)
image_state = Lens.lens (\Image' {state} -> state) (\s@Image' {} a -> s {state = a} :: Image)

-- | The tags that apply to this image.
image_tags :: Lens.Lens' Image (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
image_tags = Lens.lens (\Image' {tags} -> tags) (\s@Image' {} a -> s {tags = a} :: Image) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether this image produces an AMI or a container image.
image_type :: Lens.Lens' Image (Prelude.Maybe ImageType)
image_type = Lens.lens (\Image' {type'} -> type') (\s@Image' {} a -> s {type' = a} :: Image)

-- | The semantic version of the image.
--
-- The semantic version has four nodes:
-- \<major>.\<minor>.\<patch>\/\<build>. You can assign values for the
-- first three, and can filter on all of them.
--
-- __Assignment:__ For the first three nodes you can assign any positive
-- integer value, including zero, with an upper limit of 2^30-1, or
-- 1073741823 for each node. Image Builder automatically assigns the build
-- number to the fourth node.
--
-- __Patterns:__ You can use any numeric pattern that adheres to the
-- assignment requirements for the nodes that you can assign. For example,
-- you might choose a software version pattern, such as 1.0.0, or a date,
-- such as 2021.01.01.
--
-- __Filtering:__ With semantic versioning, you have the flexibility to use
-- wildcards (x) to specify the most recent versions or nodes when
-- selecting the base image or components for your recipe. When you use a
-- wildcard in any node, all nodes to the right of the first wildcard must
-- also be wildcards.
image_version :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_version = Lens.lens (\Image' {version} -> version) (\s@Image' {} a -> s {version = a} :: Image)

instance Data.FromJSON Image where
  parseJSON =
    Data.withObject
      "Image"
      ( \x ->
          Image'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "buildType")
            Prelude.<*> (x Data..:? "containerRecipe")
            Prelude.<*> (x Data..:? "dateCreated")
            Prelude.<*> (x Data..:? "distributionConfiguration")
            Prelude.<*> (x Data..:? "enhancedImageMetadataEnabled")
            Prelude.<*> (x Data..:? "imageRecipe")
            Prelude.<*> (x Data..:? "imageScanningConfiguration")
            Prelude.<*> (x Data..:? "imageSource")
            Prelude.<*> (x Data..:? "imageTestsConfiguration")
            Prelude.<*> (x Data..:? "infrastructureConfiguration")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "osVersion")
            Prelude.<*> (x Data..:? "outputResources")
            Prelude.<*> (x Data..:? "platform")
            Prelude.<*> (x Data..:? "scanState")
            Prelude.<*> (x Data..:? "sourcePipelineArn")
            Prelude.<*> (x Data..:? "sourcePipelineName")
            Prelude.<*> (x Data..:? "state")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "type")
            Prelude.<*> (x Data..:? "version")
      )

instance Prelude.Hashable Image where
  hashWithSalt _salt Image' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` buildType
      `Prelude.hashWithSalt` containerRecipe
      `Prelude.hashWithSalt` dateCreated
      `Prelude.hashWithSalt` distributionConfiguration
      `Prelude.hashWithSalt` enhancedImageMetadataEnabled
      `Prelude.hashWithSalt` imageRecipe
      `Prelude.hashWithSalt` imageScanningConfiguration
      `Prelude.hashWithSalt` imageSource
      `Prelude.hashWithSalt` imageTestsConfiguration
      `Prelude.hashWithSalt` infrastructureConfiguration
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` osVersion
      `Prelude.hashWithSalt` outputResources
      `Prelude.hashWithSalt` platform
      `Prelude.hashWithSalt` scanState
      `Prelude.hashWithSalt` sourcePipelineArn
      `Prelude.hashWithSalt` sourcePipelineName
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` version

instance Prelude.NFData Image where
  rnf Image' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf buildType
      `Prelude.seq` Prelude.rnf containerRecipe
      `Prelude.seq` Prelude.rnf dateCreated
      `Prelude.seq` Prelude.rnf distributionConfiguration
      `Prelude.seq` Prelude.rnf enhancedImageMetadataEnabled
      `Prelude.seq` Prelude.rnf imageRecipe
      `Prelude.seq` Prelude.rnf imageScanningConfiguration
      `Prelude.seq` Prelude.rnf imageSource
      `Prelude.seq` Prelude.rnf imageTestsConfiguration
      `Prelude.seq` Prelude.rnf infrastructureConfiguration
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf osVersion
      `Prelude.seq` Prelude.rnf outputResources
      `Prelude.seq` Prelude.rnf platform
      `Prelude.seq` Prelude.rnf scanState
      `Prelude.seq` Prelude.rnf sourcePipelineArn
      `Prelude.seq` Prelude.rnf sourcePipelineName
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf version
