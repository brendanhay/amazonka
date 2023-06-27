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
-- Module      : Amazonka.ImageBuilder.Types.ImagePipeline
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.ImagePipeline where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types.ImageScanningConfiguration
import Amazonka.ImageBuilder.Types.ImageTestsConfiguration
import Amazonka.ImageBuilder.Types.PipelineStatus
import Amazonka.ImageBuilder.Types.Platform
import Amazonka.ImageBuilder.Types.Schedule
import qualified Amazonka.Prelude as Prelude

-- | Details of an image pipeline.
--
-- /See:/ 'newImagePipeline' smart constructor.
data ImagePipeline = ImagePipeline'
  { -- | The Amazon Resource Name (ARN) of the image pipeline.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the container recipe that is used for
    -- this pipeline.
    containerRecipeArn :: Prelude.Maybe Prelude.Text,
    -- | The date on which this image pipeline was created.
    dateCreated :: Prelude.Maybe Prelude.Text,
    -- | This is no longer supported, and does not return a value.
    dateLastRun :: Prelude.Maybe Prelude.Text,
    -- | The next date when the pipeline is scheduled to run.
    dateNextRun :: Prelude.Maybe Prelude.Text,
    -- | The date on which this image pipeline was last updated.
    dateUpdated :: Prelude.Maybe Prelude.Text,
    -- | The description of the image pipeline.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the distribution configuration
    -- associated with this image pipeline.
    distributionConfigurationArn :: Prelude.Maybe Prelude.Text,
    -- | Collects additional information about the image being created, including
    -- the operating system (OS) version and package list. This information is
    -- used to enhance the overall experience of using EC2 Image Builder.
    -- Enabled by default.
    enhancedImageMetadataEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the image recipe associated with this
    -- image pipeline.
    imageRecipeArn :: Prelude.Maybe Prelude.Text,
    -- | Contains settings for vulnerability scans.
    imageScanningConfiguration :: Prelude.Maybe ImageScanningConfiguration,
    -- | The image tests configuration of the image pipeline.
    imageTestsConfiguration :: Prelude.Maybe ImageTestsConfiguration,
    -- | The Amazon Resource Name (ARN) of the infrastructure configuration
    -- associated with this image pipeline.
    infrastructureConfigurationArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the image pipeline.
    name :: Prelude.Maybe Prelude.Text,
    -- | The platform of the image pipeline.
    platform :: Prelude.Maybe Platform,
    -- | The schedule of the image pipeline.
    schedule :: Prelude.Maybe Schedule,
    -- | The status of the image pipeline.
    status :: Prelude.Maybe PipelineStatus,
    -- | The tags of this image pipeline.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImagePipeline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'imagePipeline_arn' - The Amazon Resource Name (ARN) of the image pipeline.
--
-- 'containerRecipeArn', 'imagePipeline_containerRecipeArn' - The Amazon Resource Name (ARN) of the container recipe that is used for
-- this pipeline.
--
-- 'dateCreated', 'imagePipeline_dateCreated' - The date on which this image pipeline was created.
--
-- 'dateLastRun', 'imagePipeline_dateLastRun' - This is no longer supported, and does not return a value.
--
-- 'dateNextRun', 'imagePipeline_dateNextRun' - The next date when the pipeline is scheduled to run.
--
-- 'dateUpdated', 'imagePipeline_dateUpdated' - The date on which this image pipeline was last updated.
--
-- 'description', 'imagePipeline_description' - The description of the image pipeline.
--
-- 'distributionConfigurationArn', 'imagePipeline_distributionConfigurationArn' - The Amazon Resource Name (ARN) of the distribution configuration
-- associated with this image pipeline.
--
-- 'enhancedImageMetadataEnabled', 'imagePipeline_enhancedImageMetadataEnabled' - Collects additional information about the image being created, including
-- the operating system (OS) version and package list. This information is
-- used to enhance the overall experience of using EC2 Image Builder.
-- Enabled by default.
--
-- 'imageRecipeArn', 'imagePipeline_imageRecipeArn' - The Amazon Resource Name (ARN) of the image recipe associated with this
-- image pipeline.
--
-- 'imageScanningConfiguration', 'imagePipeline_imageScanningConfiguration' - Contains settings for vulnerability scans.
--
-- 'imageTestsConfiguration', 'imagePipeline_imageTestsConfiguration' - The image tests configuration of the image pipeline.
--
-- 'infrastructureConfigurationArn', 'imagePipeline_infrastructureConfigurationArn' - The Amazon Resource Name (ARN) of the infrastructure configuration
-- associated with this image pipeline.
--
-- 'name', 'imagePipeline_name' - The name of the image pipeline.
--
-- 'platform', 'imagePipeline_platform' - The platform of the image pipeline.
--
-- 'schedule', 'imagePipeline_schedule' - The schedule of the image pipeline.
--
-- 'status', 'imagePipeline_status' - The status of the image pipeline.
--
-- 'tags', 'imagePipeline_tags' - The tags of this image pipeline.
newImagePipeline ::
  ImagePipeline
newImagePipeline =
  ImagePipeline'
    { arn = Prelude.Nothing,
      containerRecipeArn = Prelude.Nothing,
      dateCreated = Prelude.Nothing,
      dateLastRun = Prelude.Nothing,
      dateNextRun = Prelude.Nothing,
      dateUpdated = Prelude.Nothing,
      description = Prelude.Nothing,
      distributionConfigurationArn = Prelude.Nothing,
      enhancedImageMetadataEnabled = Prelude.Nothing,
      imageRecipeArn = Prelude.Nothing,
      imageScanningConfiguration = Prelude.Nothing,
      imageTestsConfiguration = Prelude.Nothing,
      infrastructureConfigurationArn = Prelude.Nothing,
      name = Prelude.Nothing,
      platform = Prelude.Nothing,
      schedule = Prelude.Nothing,
      status = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the image pipeline.
imagePipeline_arn :: Lens.Lens' ImagePipeline (Prelude.Maybe Prelude.Text)
imagePipeline_arn = Lens.lens (\ImagePipeline' {arn} -> arn) (\s@ImagePipeline' {} a -> s {arn = a} :: ImagePipeline)

-- | The Amazon Resource Name (ARN) of the container recipe that is used for
-- this pipeline.
imagePipeline_containerRecipeArn :: Lens.Lens' ImagePipeline (Prelude.Maybe Prelude.Text)
imagePipeline_containerRecipeArn = Lens.lens (\ImagePipeline' {containerRecipeArn} -> containerRecipeArn) (\s@ImagePipeline' {} a -> s {containerRecipeArn = a} :: ImagePipeline)

-- | The date on which this image pipeline was created.
imagePipeline_dateCreated :: Lens.Lens' ImagePipeline (Prelude.Maybe Prelude.Text)
imagePipeline_dateCreated = Lens.lens (\ImagePipeline' {dateCreated} -> dateCreated) (\s@ImagePipeline' {} a -> s {dateCreated = a} :: ImagePipeline)

-- | This is no longer supported, and does not return a value.
imagePipeline_dateLastRun :: Lens.Lens' ImagePipeline (Prelude.Maybe Prelude.Text)
imagePipeline_dateLastRun = Lens.lens (\ImagePipeline' {dateLastRun} -> dateLastRun) (\s@ImagePipeline' {} a -> s {dateLastRun = a} :: ImagePipeline)

-- | The next date when the pipeline is scheduled to run.
imagePipeline_dateNextRun :: Lens.Lens' ImagePipeline (Prelude.Maybe Prelude.Text)
imagePipeline_dateNextRun = Lens.lens (\ImagePipeline' {dateNextRun} -> dateNextRun) (\s@ImagePipeline' {} a -> s {dateNextRun = a} :: ImagePipeline)

-- | The date on which this image pipeline was last updated.
imagePipeline_dateUpdated :: Lens.Lens' ImagePipeline (Prelude.Maybe Prelude.Text)
imagePipeline_dateUpdated = Lens.lens (\ImagePipeline' {dateUpdated} -> dateUpdated) (\s@ImagePipeline' {} a -> s {dateUpdated = a} :: ImagePipeline)

-- | The description of the image pipeline.
imagePipeline_description :: Lens.Lens' ImagePipeline (Prelude.Maybe Prelude.Text)
imagePipeline_description = Lens.lens (\ImagePipeline' {description} -> description) (\s@ImagePipeline' {} a -> s {description = a} :: ImagePipeline)

-- | The Amazon Resource Name (ARN) of the distribution configuration
-- associated with this image pipeline.
imagePipeline_distributionConfigurationArn :: Lens.Lens' ImagePipeline (Prelude.Maybe Prelude.Text)
imagePipeline_distributionConfigurationArn = Lens.lens (\ImagePipeline' {distributionConfigurationArn} -> distributionConfigurationArn) (\s@ImagePipeline' {} a -> s {distributionConfigurationArn = a} :: ImagePipeline)

-- | Collects additional information about the image being created, including
-- the operating system (OS) version and package list. This information is
-- used to enhance the overall experience of using EC2 Image Builder.
-- Enabled by default.
imagePipeline_enhancedImageMetadataEnabled :: Lens.Lens' ImagePipeline (Prelude.Maybe Prelude.Bool)
imagePipeline_enhancedImageMetadataEnabled = Lens.lens (\ImagePipeline' {enhancedImageMetadataEnabled} -> enhancedImageMetadataEnabled) (\s@ImagePipeline' {} a -> s {enhancedImageMetadataEnabled = a} :: ImagePipeline)

-- | The Amazon Resource Name (ARN) of the image recipe associated with this
-- image pipeline.
imagePipeline_imageRecipeArn :: Lens.Lens' ImagePipeline (Prelude.Maybe Prelude.Text)
imagePipeline_imageRecipeArn = Lens.lens (\ImagePipeline' {imageRecipeArn} -> imageRecipeArn) (\s@ImagePipeline' {} a -> s {imageRecipeArn = a} :: ImagePipeline)

-- | Contains settings for vulnerability scans.
imagePipeline_imageScanningConfiguration :: Lens.Lens' ImagePipeline (Prelude.Maybe ImageScanningConfiguration)
imagePipeline_imageScanningConfiguration = Lens.lens (\ImagePipeline' {imageScanningConfiguration} -> imageScanningConfiguration) (\s@ImagePipeline' {} a -> s {imageScanningConfiguration = a} :: ImagePipeline)

-- | The image tests configuration of the image pipeline.
imagePipeline_imageTestsConfiguration :: Lens.Lens' ImagePipeline (Prelude.Maybe ImageTestsConfiguration)
imagePipeline_imageTestsConfiguration = Lens.lens (\ImagePipeline' {imageTestsConfiguration} -> imageTestsConfiguration) (\s@ImagePipeline' {} a -> s {imageTestsConfiguration = a} :: ImagePipeline)

-- | The Amazon Resource Name (ARN) of the infrastructure configuration
-- associated with this image pipeline.
imagePipeline_infrastructureConfigurationArn :: Lens.Lens' ImagePipeline (Prelude.Maybe Prelude.Text)
imagePipeline_infrastructureConfigurationArn = Lens.lens (\ImagePipeline' {infrastructureConfigurationArn} -> infrastructureConfigurationArn) (\s@ImagePipeline' {} a -> s {infrastructureConfigurationArn = a} :: ImagePipeline)

-- | The name of the image pipeline.
imagePipeline_name :: Lens.Lens' ImagePipeline (Prelude.Maybe Prelude.Text)
imagePipeline_name = Lens.lens (\ImagePipeline' {name} -> name) (\s@ImagePipeline' {} a -> s {name = a} :: ImagePipeline)

-- | The platform of the image pipeline.
imagePipeline_platform :: Lens.Lens' ImagePipeline (Prelude.Maybe Platform)
imagePipeline_platform = Lens.lens (\ImagePipeline' {platform} -> platform) (\s@ImagePipeline' {} a -> s {platform = a} :: ImagePipeline)

-- | The schedule of the image pipeline.
imagePipeline_schedule :: Lens.Lens' ImagePipeline (Prelude.Maybe Schedule)
imagePipeline_schedule = Lens.lens (\ImagePipeline' {schedule} -> schedule) (\s@ImagePipeline' {} a -> s {schedule = a} :: ImagePipeline)

-- | The status of the image pipeline.
imagePipeline_status :: Lens.Lens' ImagePipeline (Prelude.Maybe PipelineStatus)
imagePipeline_status = Lens.lens (\ImagePipeline' {status} -> status) (\s@ImagePipeline' {} a -> s {status = a} :: ImagePipeline)

-- | The tags of this image pipeline.
imagePipeline_tags :: Lens.Lens' ImagePipeline (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
imagePipeline_tags = Lens.lens (\ImagePipeline' {tags} -> tags) (\s@ImagePipeline' {} a -> s {tags = a} :: ImagePipeline) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ImagePipeline where
  parseJSON =
    Data.withObject
      "ImagePipeline"
      ( \x ->
          ImagePipeline'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "containerRecipeArn")
            Prelude.<*> (x Data..:? "dateCreated")
            Prelude.<*> (x Data..:? "dateLastRun")
            Prelude.<*> (x Data..:? "dateNextRun")
            Prelude.<*> (x Data..:? "dateUpdated")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "distributionConfigurationArn")
            Prelude.<*> (x Data..:? "enhancedImageMetadataEnabled")
            Prelude.<*> (x Data..:? "imageRecipeArn")
            Prelude.<*> (x Data..:? "imageScanningConfiguration")
            Prelude.<*> (x Data..:? "imageTestsConfiguration")
            Prelude.<*> (x Data..:? "infrastructureConfigurationArn")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "platform")
            Prelude.<*> (x Data..:? "schedule")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ImagePipeline where
  hashWithSalt _salt ImagePipeline' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` containerRecipeArn
      `Prelude.hashWithSalt` dateCreated
      `Prelude.hashWithSalt` dateLastRun
      `Prelude.hashWithSalt` dateNextRun
      `Prelude.hashWithSalt` dateUpdated
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` distributionConfigurationArn
      `Prelude.hashWithSalt` enhancedImageMetadataEnabled
      `Prelude.hashWithSalt` imageRecipeArn
      `Prelude.hashWithSalt` imageScanningConfiguration
      `Prelude.hashWithSalt` imageTestsConfiguration
      `Prelude.hashWithSalt` infrastructureConfigurationArn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` platform
      `Prelude.hashWithSalt` schedule
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` tags

instance Prelude.NFData ImagePipeline where
  rnf ImagePipeline' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf containerRecipeArn
      `Prelude.seq` Prelude.rnf dateCreated
      `Prelude.seq` Prelude.rnf dateLastRun
      `Prelude.seq` Prelude.rnf dateNextRun
      `Prelude.seq` Prelude.rnf dateUpdated
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf distributionConfigurationArn
      `Prelude.seq` Prelude.rnf enhancedImageMetadataEnabled
      `Prelude.seq` Prelude.rnf imageRecipeArn
      `Prelude.seq` Prelude.rnf imageScanningConfiguration
      `Prelude.seq` Prelude.rnf imageTestsConfiguration
      `Prelude.seq` Prelude.rnf infrastructureConfigurationArn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf platform
      `Prelude.seq` Prelude.rnf schedule
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf tags
