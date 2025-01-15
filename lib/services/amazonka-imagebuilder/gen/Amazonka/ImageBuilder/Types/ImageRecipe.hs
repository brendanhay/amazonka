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
-- Module      : Amazonka.ImageBuilder.Types.ImageRecipe
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.ImageRecipe where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types.AdditionalInstanceConfiguration
import Amazonka.ImageBuilder.Types.ComponentConfiguration
import Amazonka.ImageBuilder.Types.ImageType
import Amazonka.ImageBuilder.Types.InstanceBlockDeviceMapping
import Amazonka.ImageBuilder.Types.Platform
import qualified Amazonka.Prelude as Prelude

-- | An image recipe.
--
-- /See:/ 'newImageRecipe' smart constructor.
data ImageRecipe = ImageRecipe'
  { -- | Before you create a new AMI, Image Builder launches temporary Amazon EC2
    -- instances to build and test your image configuration. Instance
    -- configuration adds a layer of control over those instances. You can
    -- define settings and add scripts to run when an instance is launched from
    -- your AMI.
    additionalInstanceConfiguration :: Prelude.Maybe AdditionalInstanceConfiguration,
    -- | The Amazon Resource Name (ARN) of the image recipe.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The block device mappings to apply when creating images from this
    -- recipe.
    blockDeviceMappings :: Prelude.Maybe [InstanceBlockDeviceMapping],
    -- | The components of the image recipe.
    components :: Prelude.Maybe (Prelude.NonEmpty ComponentConfiguration),
    -- | The date on which this image recipe was created.
    dateCreated :: Prelude.Maybe Prelude.Text,
    -- | The description of the image recipe.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the image recipe.
    name :: Prelude.Maybe Prelude.Text,
    -- | The owner of the image recipe.
    owner :: Prelude.Maybe Prelude.Text,
    -- | The base image of the image recipe.
    parentImage :: Prelude.Maybe Prelude.Text,
    -- | The platform of the image recipe.
    platform :: Prelude.Maybe Platform,
    -- | The tags of the image recipe.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Specifies which type of image is created by the recipe - an AMI or a
    -- container image.
    type' :: Prelude.Maybe ImageType,
    -- | The version of the image recipe.
    version :: Prelude.Maybe Prelude.Text,
    -- | The working directory to be used during build and test workflows.
    workingDirectory :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImageRecipe' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalInstanceConfiguration', 'imageRecipe_additionalInstanceConfiguration' - Before you create a new AMI, Image Builder launches temporary Amazon EC2
-- instances to build and test your image configuration. Instance
-- configuration adds a layer of control over those instances. You can
-- define settings and add scripts to run when an instance is launched from
-- your AMI.
--
-- 'arn', 'imageRecipe_arn' - The Amazon Resource Name (ARN) of the image recipe.
--
-- 'blockDeviceMappings', 'imageRecipe_blockDeviceMappings' - The block device mappings to apply when creating images from this
-- recipe.
--
-- 'components', 'imageRecipe_components' - The components of the image recipe.
--
-- 'dateCreated', 'imageRecipe_dateCreated' - The date on which this image recipe was created.
--
-- 'description', 'imageRecipe_description' - The description of the image recipe.
--
-- 'name', 'imageRecipe_name' - The name of the image recipe.
--
-- 'owner', 'imageRecipe_owner' - The owner of the image recipe.
--
-- 'parentImage', 'imageRecipe_parentImage' - The base image of the image recipe.
--
-- 'platform', 'imageRecipe_platform' - The platform of the image recipe.
--
-- 'tags', 'imageRecipe_tags' - The tags of the image recipe.
--
-- 'type'', 'imageRecipe_type' - Specifies which type of image is created by the recipe - an AMI or a
-- container image.
--
-- 'version', 'imageRecipe_version' - The version of the image recipe.
--
-- 'workingDirectory', 'imageRecipe_workingDirectory' - The working directory to be used during build and test workflows.
newImageRecipe ::
  ImageRecipe
newImageRecipe =
  ImageRecipe'
    { additionalInstanceConfiguration =
        Prelude.Nothing,
      arn = Prelude.Nothing,
      blockDeviceMappings = Prelude.Nothing,
      components = Prelude.Nothing,
      dateCreated = Prelude.Nothing,
      description = Prelude.Nothing,
      name = Prelude.Nothing,
      owner = Prelude.Nothing,
      parentImage = Prelude.Nothing,
      platform = Prelude.Nothing,
      tags = Prelude.Nothing,
      type' = Prelude.Nothing,
      version = Prelude.Nothing,
      workingDirectory = Prelude.Nothing
    }

-- | Before you create a new AMI, Image Builder launches temporary Amazon EC2
-- instances to build and test your image configuration. Instance
-- configuration adds a layer of control over those instances. You can
-- define settings and add scripts to run when an instance is launched from
-- your AMI.
imageRecipe_additionalInstanceConfiguration :: Lens.Lens' ImageRecipe (Prelude.Maybe AdditionalInstanceConfiguration)
imageRecipe_additionalInstanceConfiguration = Lens.lens (\ImageRecipe' {additionalInstanceConfiguration} -> additionalInstanceConfiguration) (\s@ImageRecipe' {} a -> s {additionalInstanceConfiguration = a} :: ImageRecipe)

-- | The Amazon Resource Name (ARN) of the image recipe.
imageRecipe_arn :: Lens.Lens' ImageRecipe (Prelude.Maybe Prelude.Text)
imageRecipe_arn = Lens.lens (\ImageRecipe' {arn} -> arn) (\s@ImageRecipe' {} a -> s {arn = a} :: ImageRecipe)

-- | The block device mappings to apply when creating images from this
-- recipe.
imageRecipe_blockDeviceMappings :: Lens.Lens' ImageRecipe (Prelude.Maybe [InstanceBlockDeviceMapping])
imageRecipe_blockDeviceMappings = Lens.lens (\ImageRecipe' {blockDeviceMappings} -> blockDeviceMappings) (\s@ImageRecipe' {} a -> s {blockDeviceMappings = a} :: ImageRecipe) Prelude.. Lens.mapping Lens.coerced

-- | The components of the image recipe.
imageRecipe_components :: Lens.Lens' ImageRecipe (Prelude.Maybe (Prelude.NonEmpty ComponentConfiguration))
imageRecipe_components = Lens.lens (\ImageRecipe' {components} -> components) (\s@ImageRecipe' {} a -> s {components = a} :: ImageRecipe) Prelude.. Lens.mapping Lens.coerced

-- | The date on which this image recipe was created.
imageRecipe_dateCreated :: Lens.Lens' ImageRecipe (Prelude.Maybe Prelude.Text)
imageRecipe_dateCreated = Lens.lens (\ImageRecipe' {dateCreated} -> dateCreated) (\s@ImageRecipe' {} a -> s {dateCreated = a} :: ImageRecipe)

-- | The description of the image recipe.
imageRecipe_description :: Lens.Lens' ImageRecipe (Prelude.Maybe Prelude.Text)
imageRecipe_description = Lens.lens (\ImageRecipe' {description} -> description) (\s@ImageRecipe' {} a -> s {description = a} :: ImageRecipe)

-- | The name of the image recipe.
imageRecipe_name :: Lens.Lens' ImageRecipe (Prelude.Maybe Prelude.Text)
imageRecipe_name = Lens.lens (\ImageRecipe' {name} -> name) (\s@ImageRecipe' {} a -> s {name = a} :: ImageRecipe)

-- | The owner of the image recipe.
imageRecipe_owner :: Lens.Lens' ImageRecipe (Prelude.Maybe Prelude.Text)
imageRecipe_owner = Lens.lens (\ImageRecipe' {owner} -> owner) (\s@ImageRecipe' {} a -> s {owner = a} :: ImageRecipe)

-- | The base image of the image recipe.
imageRecipe_parentImage :: Lens.Lens' ImageRecipe (Prelude.Maybe Prelude.Text)
imageRecipe_parentImage = Lens.lens (\ImageRecipe' {parentImage} -> parentImage) (\s@ImageRecipe' {} a -> s {parentImage = a} :: ImageRecipe)

-- | The platform of the image recipe.
imageRecipe_platform :: Lens.Lens' ImageRecipe (Prelude.Maybe Platform)
imageRecipe_platform = Lens.lens (\ImageRecipe' {platform} -> platform) (\s@ImageRecipe' {} a -> s {platform = a} :: ImageRecipe)

-- | The tags of the image recipe.
imageRecipe_tags :: Lens.Lens' ImageRecipe (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
imageRecipe_tags = Lens.lens (\ImageRecipe' {tags} -> tags) (\s@ImageRecipe' {} a -> s {tags = a} :: ImageRecipe) Prelude.. Lens.mapping Lens.coerced

-- | Specifies which type of image is created by the recipe - an AMI or a
-- container image.
imageRecipe_type :: Lens.Lens' ImageRecipe (Prelude.Maybe ImageType)
imageRecipe_type = Lens.lens (\ImageRecipe' {type'} -> type') (\s@ImageRecipe' {} a -> s {type' = a} :: ImageRecipe)

-- | The version of the image recipe.
imageRecipe_version :: Lens.Lens' ImageRecipe (Prelude.Maybe Prelude.Text)
imageRecipe_version = Lens.lens (\ImageRecipe' {version} -> version) (\s@ImageRecipe' {} a -> s {version = a} :: ImageRecipe)

-- | The working directory to be used during build and test workflows.
imageRecipe_workingDirectory :: Lens.Lens' ImageRecipe (Prelude.Maybe Prelude.Text)
imageRecipe_workingDirectory = Lens.lens (\ImageRecipe' {workingDirectory} -> workingDirectory) (\s@ImageRecipe' {} a -> s {workingDirectory = a} :: ImageRecipe)

instance Data.FromJSON ImageRecipe where
  parseJSON =
    Data.withObject
      "ImageRecipe"
      ( \x ->
          ImageRecipe'
            Prelude.<$> (x Data..:? "additionalInstanceConfiguration")
            Prelude.<*> (x Data..:? "arn")
            Prelude.<*> ( x
                            Data..:? "blockDeviceMappings"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "components")
            Prelude.<*> (x Data..:? "dateCreated")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "owner")
            Prelude.<*> (x Data..:? "parentImage")
            Prelude.<*> (x Data..:? "platform")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "type")
            Prelude.<*> (x Data..:? "version")
            Prelude.<*> (x Data..:? "workingDirectory")
      )

instance Prelude.Hashable ImageRecipe where
  hashWithSalt _salt ImageRecipe' {..} =
    _salt
      `Prelude.hashWithSalt` additionalInstanceConfiguration
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` blockDeviceMappings
      `Prelude.hashWithSalt` components
      `Prelude.hashWithSalt` dateCreated
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` parentImage
      `Prelude.hashWithSalt` platform
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` workingDirectory

instance Prelude.NFData ImageRecipe where
  rnf ImageRecipe' {..} =
    Prelude.rnf additionalInstanceConfiguration `Prelude.seq`
      Prelude.rnf arn `Prelude.seq`
        Prelude.rnf blockDeviceMappings `Prelude.seq`
          Prelude.rnf components `Prelude.seq`
            Prelude.rnf dateCreated `Prelude.seq`
              Prelude.rnf description `Prelude.seq`
                Prelude.rnf name `Prelude.seq`
                  Prelude.rnf owner `Prelude.seq`
                    Prelude.rnf parentImage `Prelude.seq`
                      Prelude.rnf platform `Prelude.seq`
                        Prelude.rnf tags `Prelude.seq`
                          Prelude.rnf type' `Prelude.seq`
                            Prelude.rnf version `Prelude.seq`
                              Prelude.rnf workingDirectory
