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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.ImagePipeline where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ImageBuilder.Types.ImageTestsConfiguration
import Amazonka.ImageBuilder.Types.PipelineStatus
import Amazonka.ImageBuilder.Types.Platform
import Amazonka.ImageBuilder.Types.Schedule
import qualified Amazonka.Prelude as Prelude

-- | Details of an image pipeline.
--
-- /See:/ 'newImagePipeline' smart constructor.
data ImagePipeline = ImagePipeline'
  { -- | Collects additional information about the image being created, including
    -- the operating system (OS) version and package list. This information is
    -- used to enhance the overall experience of using EC2 Image Builder.
    -- Enabled by default.
    enhancedImageMetadataEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The schedule of the image pipeline.
    schedule :: Prelude.Maybe Schedule,
    -- | The tags of this image pipeline.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the image pipeline.
    name :: Prelude.Maybe Prelude.Text,
    -- | The image tests configuration of the image pipeline.
    imageTestsConfiguration :: Prelude.Maybe ImageTestsConfiguration,
    -- | The Amazon Resource Name (ARN) of the image pipeline.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the image recipe associated with this
    -- image pipeline.
    imageRecipeArn :: Prelude.Maybe Prelude.Text,
    -- | The status of the image pipeline.
    status :: Prelude.Maybe PipelineStatus,
    -- | The description of the image pipeline.
    description :: Prelude.Maybe Prelude.Text,
    -- | The platform of the image pipeline.
    platform :: Prelude.Maybe Platform,
    -- | The Amazon Resource Name (ARN) of the container recipe that is used for
    -- this pipeline.
    containerRecipeArn :: Prelude.Maybe Prelude.Text,
    -- | The date on which this image pipeline was last run.
    dateLastRun :: Prelude.Maybe Prelude.Text,
    -- | The date on which this image pipeline will next be run.
    dateNextRun :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the infrastructure configuration
    -- associated with this image pipeline.
    infrastructureConfigurationArn :: Prelude.Maybe Prelude.Text,
    -- | The date on which this image pipeline was last updated.
    dateUpdated :: Prelude.Maybe Prelude.Text,
    -- | The date on which this image pipeline was created.
    dateCreated :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the distribution configuration
    -- associated with this image pipeline.
    distributionConfigurationArn :: Prelude.Maybe Prelude.Text
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
-- 'enhancedImageMetadataEnabled', 'imagePipeline_enhancedImageMetadataEnabled' - Collects additional information about the image being created, including
-- the operating system (OS) version and package list. This information is
-- used to enhance the overall experience of using EC2 Image Builder.
-- Enabled by default.
--
-- 'schedule', 'imagePipeline_schedule' - The schedule of the image pipeline.
--
-- 'tags', 'imagePipeline_tags' - The tags of this image pipeline.
--
-- 'name', 'imagePipeline_name' - The name of the image pipeline.
--
-- 'imageTestsConfiguration', 'imagePipeline_imageTestsConfiguration' - The image tests configuration of the image pipeline.
--
-- 'arn', 'imagePipeline_arn' - The Amazon Resource Name (ARN) of the image pipeline.
--
-- 'imageRecipeArn', 'imagePipeline_imageRecipeArn' - The Amazon Resource Name (ARN) of the image recipe associated with this
-- image pipeline.
--
-- 'status', 'imagePipeline_status' - The status of the image pipeline.
--
-- 'description', 'imagePipeline_description' - The description of the image pipeline.
--
-- 'platform', 'imagePipeline_platform' - The platform of the image pipeline.
--
-- 'containerRecipeArn', 'imagePipeline_containerRecipeArn' - The Amazon Resource Name (ARN) of the container recipe that is used for
-- this pipeline.
--
-- 'dateLastRun', 'imagePipeline_dateLastRun' - The date on which this image pipeline was last run.
--
-- 'dateNextRun', 'imagePipeline_dateNextRun' - The date on which this image pipeline will next be run.
--
-- 'infrastructureConfigurationArn', 'imagePipeline_infrastructureConfigurationArn' - The Amazon Resource Name (ARN) of the infrastructure configuration
-- associated with this image pipeline.
--
-- 'dateUpdated', 'imagePipeline_dateUpdated' - The date on which this image pipeline was last updated.
--
-- 'dateCreated', 'imagePipeline_dateCreated' - The date on which this image pipeline was created.
--
-- 'distributionConfigurationArn', 'imagePipeline_distributionConfigurationArn' - The Amazon Resource Name (ARN) of the distribution configuration
-- associated with this image pipeline.
newImagePipeline ::
  ImagePipeline
newImagePipeline =
  ImagePipeline'
    { enhancedImageMetadataEnabled =
        Prelude.Nothing,
      schedule = Prelude.Nothing,
      tags = Prelude.Nothing,
      name = Prelude.Nothing,
      imageTestsConfiguration = Prelude.Nothing,
      arn = Prelude.Nothing,
      imageRecipeArn = Prelude.Nothing,
      status = Prelude.Nothing,
      description = Prelude.Nothing,
      platform = Prelude.Nothing,
      containerRecipeArn = Prelude.Nothing,
      dateLastRun = Prelude.Nothing,
      dateNextRun = Prelude.Nothing,
      infrastructureConfigurationArn = Prelude.Nothing,
      dateUpdated = Prelude.Nothing,
      dateCreated = Prelude.Nothing,
      distributionConfigurationArn = Prelude.Nothing
    }

-- | Collects additional information about the image being created, including
-- the operating system (OS) version and package list. This information is
-- used to enhance the overall experience of using EC2 Image Builder.
-- Enabled by default.
imagePipeline_enhancedImageMetadataEnabled :: Lens.Lens' ImagePipeline (Prelude.Maybe Prelude.Bool)
imagePipeline_enhancedImageMetadataEnabled = Lens.lens (\ImagePipeline' {enhancedImageMetadataEnabled} -> enhancedImageMetadataEnabled) (\s@ImagePipeline' {} a -> s {enhancedImageMetadataEnabled = a} :: ImagePipeline)

-- | The schedule of the image pipeline.
imagePipeline_schedule :: Lens.Lens' ImagePipeline (Prelude.Maybe Schedule)
imagePipeline_schedule = Lens.lens (\ImagePipeline' {schedule} -> schedule) (\s@ImagePipeline' {} a -> s {schedule = a} :: ImagePipeline)

-- | The tags of this image pipeline.
imagePipeline_tags :: Lens.Lens' ImagePipeline (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
imagePipeline_tags = Lens.lens (\ImagePipeline' {tags} -> tags) (\s@ImagePipeline' {} a -> s {tags = a} :: ImagePipeline) Prelude.. Lens.mapping Lens.coerced

-- | The name of the image pipeline.
imagePipeline_name :: Lens.Lens' ImagePipeline (Prelude.Maybe Prelude.Text)
imagePipeline_name = Lens.lens (\ImagePipeline' {name} -> name) (\s@ImagePipeline' {} a -> s {name = a} :: ImagePipeline)

-- | The image tests configuration of the image pipeline.
imagePipeline_imageTestsConfiguration :: Lens.Lens' ImagePipeline (Prelude.Maybe ImageTestsConfiguration)
imagePipeline_imageTestsConfiguration = Lens.lens (\ImagePipeline' {imageTestsConfiguration} -> imageTestsConfiguration) (\s@ImagePipeline' {} a -> s {imageTestsConfiguration = a} :: ImagePipeline)

-- | The Amazon Resource Name (ARN) of the image pipeline.
imagePipeline_arn :: Lens.Lens' ImagePipeline (Prelude.Maybe Prelude.Text)
imagePipeline_arn = Lens.lens (\ImagePipeline' {arn} -> arn) (\s@ImagePipeline' {} a -> s {arn = a} :: ImagePipeline)

-- | The Amazon Resource Name (ARN) of the image recipe associated with this
-- image pipeline.
imagePipeline_imageRecipeArn :: Lens.Lens' ImagePipeline (Prelude.Maybe Prelude.Text)
imagePipeline_imageRecipeArn = Lens.lens (\ImagePipeline' {imageRecipeArn} -> imageRecipeArn) (\s@ImagePipeline' {} a -> s {imageRecipeArn = a} :: ImagePipeline)

-- | The status of the image pipeline.
imagePipeline_status :: Lens.Lens' ImagePipeline (Prelude.Maybe PipelineStatus)
imagePipeline_status = Lens.lens (\ImagePipeline' {status} -> status) (\s@ImagePipeline' {} a -> s {status = a} :: ImagePipeline)

-- | The description of the image pipeline.
imagePipeline_description :: Lens.Lens' ImagePipeline (Prelude.Maybe Prelude.Text)
imagePipeline_description = Lens.lens (\ImagePipeline' {description} -> description) (\s@ImagePipeline' {} a -> s {description = a} :: ImagePipeline)

-- | The platform of the image pipeline.
imagePipeline_platform :: Lens.Lens' ImagePipeline (Prelude.Maybe Platform)
imagePipeline_platform = Lens.lens (\ImagePipeline' {platform} -> platform) (\s@ImagePipeline' {} a -> s {platform = a} :: ImagePipeline)

-- | The Amazon Resource Name (ARN) of the container recipe that is used for
-- this pipeline.
imagePipeline_containerRecipeArn :: Lens.Lens' ImagePipeline (Prelude.Maybe Prelude.Text)
imagePipeline_containerRecipeArn = Lens.lens (\ImagePipeline' {containerRecipeArn} -> containerRecipeArn) (\s@ImagePipeline' {} a -> s {containerRecipeArn = a} :: ImagePipeline)

-- | The date on which this image pipeline was last run.
imagePipeline_dateLastRun :: Lens.Lens' ImagePipeline (Prelude.Maybe Prelude.Text)
imagePipeline_dateLastRun = Lens.lens (\ImagePipeline' {dateLastRun} -> dateLastRun) (\s@ImagePipeline' {} a -> s {dateLastRun = a} :: ImagePipeline)

-- | The date on which this image pipeline will next be run.
imagePipeline_dateNextRun :: Lens.Lens' ImagePipeline (Prelude.Maybe Prelude.Text)
imagePipeline_dateNextRun = Lens.lens (\ImagePipeline' {dateNextRun} -> dateNextRun) (\s@ImagePipeline' {} a -> s {dateNextRun = a} :: ImagePipeline)

-- | The Amazon Resource Name (ARN) of the infrastructure configuration
-- associated with this image pipeline.
imagePipeline_infrastructureConfigurationArn :: Lens.Lens' ImagePipeline (Prelude.Maybe Prelude.Text)
imagePipeline_infrastructureConfigurationArn = Lens.lens (\ImagePipeline' {infrastructureConfigurationArn} -> infrastructureConfigurationArn) (\s@ImagePipeline' {} a -> s {infrastructureConfigurationArn = a} :: ImagePipeline)

-- | The date on which this image pipeline was last updated.
imagePipeline_dateUpdated :: Lens.Lens' ImagePipeline (Prelude.Maybe Prelude.Text)
imagePipeline_dateUpdated = Lens.lens (\ImagePipeline' {dateUpdated} -> dateUpdated) (\s@ImagePipeline' {} a -> s {dateUpdated = a} :: ImagePipeline)

-- | The date on which this image pipeline was created.
imagePipeline_dateCreated :: Lens.Lens' ImagePipeline (Prelude.Maybe Prelude.Text)
imagePipeline_dateCreated = Lens.lens (\ImagePipeline' {dateCreated} -> dateCreated) (\s@ImagePipeline' {} a -> s {dateCreated = a} :: ImagePipeline)

-- | The Amazon Resource Name (ARN) of the distribution configuration
-- associated with this image pipeline.
imagePipeline_distributionConfigurationArn :: Lens.Lens' ImagePipeline (Prelude.Maybe Prelude.Text)
imagePipeline_distributionConfigurationArn = Lens.lens (\ImagePipeline' {distributionConfigurationArn} -> distributionConfigurationArn) (\s@ImagePipeline' {} a -> s {distributionConfigurationArn = a} :: ImagePipeline)

instance Core.FromJSON ImagePipeline where
  parseJSON =
    Core.withObject
      "ImagePipeline"
      ( \x ->
          ImagePipeline'
            Prelude.<$> (x Core..:? "enhancedImageMetadataEnabled")
            Prelude.<*> (x Core..:? "schedule")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "imageTestsConfiguration")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "imageRecipeArn")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "platform")
            Prelude.<*> (x Core..:? "containerRecipeArn")
            Prelude.<*> (x Core..:? "dateLastRun")
            Prelude.<*> (x Core..:? "dateNextRun")
            Prelude.<*> (x Core..:? "infrastructureConfigurationArn")
            Prelude.<*> (x Core..:? "dateUpdated")
            Prelude.<*> (x Core..:? "dateCreated")
            Prelude.<*> (x Core..:? "distributionConfigurationArn")
      )

instance Prelude.Hashable ImagePipeline where
  hashWithSalt _salt ImagePipeline' {..} =
    _salt
      `Prelude.hashWithSalt` enhancedImageMetadataEnabled
      `Prelude.hashWithSalt` schedule
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` imageTestsConfiguration
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` imageRecipeArn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` platform
      `Prelude.hashWithSalt` containerRecipeArn
      `Prelude.hashWithSalt` dateLastRun
      `Prelude.hashWithSalt` dateNextRun
      `Prelude.hashWithSalt` infrastructureConfigurationArn
      `Prelude.hashWithSalt` dateUpdated
      `Prelude.hashWithSalt` dateCreated
      `Prelude.hashWithSalt` distributionConfigurationArn

instance Prelude.NFData ImagePipeline where
  rnf ImagePipeline' {..} =
    Prelude.rnf enhancedImageMetadataEnabled
      `Prelude.seq` Prelude.rnf schedule
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf imageTestsConfiguration
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf imageRecipeArn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf platform
      `Prelude.seq` Prelude.rnf containerRecipeArn
      `Prelude.seq` Prelude.rnf dateLastRun
      `Prelude.seq` Prelude.rnf dateNextRun
      `Prelude.seq` Prelude.rnf infrastructureConfigurationArn
      `Prelude.seq` Prelude.rnf dateUpdated
      `Prelude.seq` Prelude.rnf dateCreated
      `Prelude.seq` Prelude.rnf
        distributionConfigurationArn
