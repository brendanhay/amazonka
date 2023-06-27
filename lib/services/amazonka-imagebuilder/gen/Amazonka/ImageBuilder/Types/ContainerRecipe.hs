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
-- Module      : Amazonka.ImageBuilder.Types.ContainerRecipe
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.ContainerRecipe where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types.ComponentConfiguration
import Amazonka.ImageBuilder.Types.ContainerType
import Amazonka.ImageBuilder.Types.InstanceConfiguration
import Amazonka.ImageBuilder.Types.Platform
import Amazonka.ImageBuilder.Types.TargetContainerRepository
import qualified Amazonka.Prelude as Prelude

-- | A container recipe.
--
-- /See:/ 'newContainerRecipe' smart constructor.
data ContainerRecipe = ContainerRecipe'
  { -- | The Amazon Resource Name (ARN) of the container recipe.
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
    -- | Build and test components that are included in the container recipe.
    -- Recipes require a minimum of one build component, and can have a maximum
    -- of 20 build and test components in any combination.
    components :: Prelude.Maybe (Prelude.NonEmpty ComponentConfiguration),
    -- | Specifies the type of container, such as Docker.
    containerType :: Prelude.Maybe ContainerType,
    -- | The date when this container recipe was created.
    dateCreated :: Prelude.Maybe Prelude.Text,
    -- | The description of the container recipe.
    description :: Prelude.Maybe Prelude.Text,
    -- | Dockerfiles are text documents that are used to build Docker containers,
    -- and ensure that they contain all of the elements required by the
    -- application running inside. The template data consists of contextual
    -- variables where Image Builder places build information or scripts, based
    -- on your container image recipe.
    dockerfileTemplateData :: Prelude.Maybe Prelude.Text,
    -- | A flag that indicates if the target container is encrypted.
    encrypted :: Prelude.Maybe Prelude.Bool,
    -- | A group of options that can be used to configure an instance for
    -- building and testing container images.
    instanceConfiguration :: Prelude.Maybe InstanceConfiguration,
    -- | Identifies which KMS key is used to encrypt the container image for
    -- distribution to the target Region.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The name of the container recipe.
    name :: Prelude.Maybe Prelude.Text,
    -- | The owner of the container recipe.
    owner :: Prelude.Maybe Prelude.Text,
    -- | The base image for the container recipe.
    parentImage :: Prelude.Maybe Prelude.Text,
    -- | The system platform for the container, such as Windows or Linux.
    platform :: Prelude.Maybe Platform,
    -- | Tags that are attached to the container recipe.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The destination repository for the container image.
    targetRepository :: Prelude.Maybe TargetContainerRepository,
    -- | The semantic version of the container recipe.
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
    version :: Prelude.Maybe Prelude.Text,
    -- | The working directory for use during build and test workflows.
    workingDirectory :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContainerRecipe' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'containerRecipe_arn' - The Amazon Resource Name (ARN) of the container recipe.
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
-- 'components', 'containerRecipe_components' - Build and test components that are included in the container recipe.
-- Recipes require a minimum of one build component, and can have a maximum
-- of 20 build and test components in any combination.
--
-- 'containerType', 'containerRecipe_containerType' - Specifies the type of container, such as Docker.
--
-- 'dateCreated', 'containerRecipe_dateCreated' - The date when this container recipe was created.
--
-- 'description', 'containerRecipe_description' - The description of the container recipe.
--
-- 'dockerfileTemplateData', 'containerRecipe_dockerfileTemplateData' - Dockerfiles are text documents that are used to build Docker containers,
-- and ensure that they contain all of the elements required by the
-- application running inside. The template data consists of contextual
-- variables where Image Builder places build information or scripts, based
-- on your container image recipe.
--
-- 'encrypted', 'containerRecipe_encrypted' - A flag that indicates if the target container is encrypted.
--
-- 'instanceConfiguration', 'containerRecipe_instanceConfiguration' - A group of options that can be used to configure an instance for
-- building and testing container images.
--
-- 'kmsKeyId', 'containerRecipe_kmsKeyId' - Identifies which KMS key is used to encrypt the container image for
-- distribution to the target Region.
--
-- 'name', 'containerRecipe_name' - The name of the container recipe.
--
-- 'owner', 'containerRecipe_owner' - The owner of the container recipe.
--
-- 'parentImage', 'containerRecipe_parentImage' - The base image for the container recipe.
--
-- 'platform', 'containerRecipe_platform' - The system platform for the container, such as Windows or Linux.
--
-- 'tags', 'containerRecipe_tags' - Tags that are attached to the container recipe.
--
-- 'targetRepository', 'containerRecipe_targetRepository' - The destination repository for the container image.
--
-- 'version', 'containerRecipe_version' - The semantic version of the container recipe.
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
--
-- 'workingDirectory', 'containerRecipe_workingDirectory' - The working directory for use during build and test workflows.
newContainerRecipe ::
  ContainerRecipe
newContainerRecipe =
  ContainerRecipe'
    { arn = Prelude.Nothing,
      components = Prelude.Nothing,
      containerType = Prelude.Nothing,
      dateCreated = Prelude.Nothing,
      description = Prelude.Nothing,
      dockerfileTemplateData = Prelude.Nothing,
      encrypted = Prelude.Nothing,
      instanceConfiguration = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      name = Prelude.Nothing,
      owner = Prelude.Nothing,
      parentImage = Prelude.Nothing,
      platform = Prelude.Nothing,
      tags = Prelude.Nothing,
      targetRepository = Prelude.Nothing,
      version = Prelude.Nothing,
      workingDirectory = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the container recipe.
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
containerRecipe_arn :: Lens.Lens' ContainerRecipe (Prelude.Maybe Prelude.Text)
containerRecipe_arn = Lens.lens (\ContainerRecipe' {arn} -> arn) (\s@ContainerRecipe' {} a -> s {arn = a} :: ContainerRecipe)

-- | Build and test components that are included in the container recipe.
-- Recipes require a minimum of one build component, and can have a maximum
-- of 20 build and test components in any combination.
containerRecipe_components :: Lens.Lens' ContainerRecipe (Prelude.Maybe (Prelude.NonEmpty ComponentConfiguration))
containerRecipe_components = Lens.lens (\ContainerRecipe' {components} -> components) (\s@ContainerRecipe' {} a -> s {components = a} :: ContainerRecipe) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the type of container, such as Docker.
containerRecipe_containerType :: Lens.Lens' ContainerRecipe (Prelude.Maybe ContainerType)
containerRecipe_containerType = Lens.lens (\ContainerRecipe' {containerType} -> containerType) (\s@ContainerRecipe' {} a -> s {containerType = a} :: ContainerRecipe)

-- | The date when this container recipe was created.
containerRecipe_dateCreated :: Lens.Lens' ContainerRecipe (Prelude.Maybe Prelude.Text)
containerRecipe_dateCreated = Lens.lens (\ContainerRecipe' {dateCreated} -> dateCreated) (\s@ContainerRecipe' {} a -> s {dateCreated = a} :: ContainerRecipe)

-- | The description of the container recipe.
containerRecipe_description :: Lens.Lens' ContainerRecipe (Prelude.Maybe Prelude.Text)
containerRecipe_description = Lens.lens (\ContainerRecipe' {description} -> description) (\s@ContainerRecipe' {} a -> s {description = a} :: ContainerRecipe)

-- | Dockerfiles are text documents that are used to build Docker containers,
-- and ensure that they contain all of the elements required by the
-- application running inside. The template data consists of contextual
-- variables where Image Builder places build information or scripts, based
-- on your container image recipe.
containerRecipe_dockerfileTemplateData :: Lens.Lens' ContainerRecipe (Prelude.Maybe Prelude.Text)
containerRecipe_dockerfileTemplateData = Lens.lens (\ContainerRecipe' {dockerfileTemplateData} -> dockerfileTemplateData) (\s@ContainerRecipe' {} a -> s {dockerfileTemplateData = a} :: ContainerRecipe)

-- | A flag that indicates if the target container is encrypted.
containerRecipe_encrypted :: Lens.Lens' ContainerRecipe (Prelude.Maybe Prelude.Bool)
containerRecipe_encrypted = Lens.lens (\ContainerRecipe' {encrypted} -> encrypted) (\s@ContainerRecipe' {} a -> s {encrypted = a} :: ContainerRecipe)

-- | A group of options that can be used to configure an instance for
-- building and testing container images.
containerRecipe_instanceConfiguration :: Lens.Lens' ContainerRecipe (Prelude.Maybe InstanceConfiguration)
containerRecipe_instanceConfiguration = Lens.lens (\ContainerRecipe' {instanceConfiguration} -> instanceConfiguration) (\s@ContainerRecipe' {} a -> s {instanceConfiguration = a} :: ContainerRecipe)

-- | Identifies which KMS key is used to encrypt the container image for
-- distribution to the target Region.
containerRecipe_kmsKeyId :: Lens.Lens' ContainerRecipe (Prelude.Maybe Prelude.Text)
containerRecipe_kmsKeyId = Lens.lens (\ContainerRecipe' {kmsKeyId} -> kmsKeyId) (\s@ContainerRecipe' {} a -> s {kmsKeyId = a} :: ContainerRecipe)

-- | The name of the container recipe.
containerRecipe_name :: Lens.Lens' ContainerRecipe (Prelude.Maybe Prelude.Text)
containerRecipe_name = Lens.lens (\ContainerRecipe' {name} -> name) (\s@ContainerRecipe' {} a -> s {name = a} :: ContainerRecipe)

-- | The owner of the container recipe.
containerRecipe_owner :: Lens.Lens' ContainerRecipe (Prelude.Maybe Prelude.Text)
containerRecipe_owner = Lens.lens (\ContainerRecipe' {owner} -> owner) (\s@ContainerRecipe' {} a -> s {owner = a} :: ContainerRecipe)

-- | The base image for the container recipe.
containerRecipe_parentImage :: Lens.Lens' ContainerRecipe (Prelude.Maybe Prelude.Text)
containerRecipe_parentImage = Lens.lens (\ContainerRecipe' {parentImage} -> parentImage) (\s@ContainerRecipe' {} a -> s {parentImage = a} :: ContainerRecipe)

-- | The system platform for the container, such as Windows or Linux.
containerRecipe_platform :: Lens.Lens' ContainerRecipe (Prelude.Maybe Platform)
containerRecipe_platform = Lens.lens (\ContainerRecipe' {platform} -> platform) (\s@ContainerRecipe' {} a -> s {platform = a} :: ContainerRecipe)

-- | Tags that are attached to the container recipe.
containerRecipe_tags :: Lens.Lens' ContainerRecipe (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
containerRecipe_tags = Lens.lens (\ContainerRecipe' {tags} -> tags) (\s@ContainerRecipe' {} a -> s {tags = a} :: ContainerRecipe) Prelude.. Lens.mapping Lens.coerced

-- | The destination repository for the container image.
containerRecipe_targetRepository :: Lens.Lens' ContainerRecipe (Prelude.Maybe TargetContainerRepository)
containerRecipe_targetRepository = Lens.lens (\ContainerRecipe' {targetRepository} -> targetRepository) (\s@ContainerRecipe' {} a -> s {targetRepository = a} :: ContainerRecipe)

-- | The semantic version of the container recipe.
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
containerRecipe_version :: Lens.Lens' ContainerRecipe (Prelude.Maybe Prelude.Text)
containerRecipe_version = Lens.lens (\ContainerRecipe' {version} -> version) (\s@ContainerRecipe' {} a -> s {version = a} :: ContainerRecipe)

-- | The working directory for use during build and test workflows.
containerRecipe_workingDirectory :: Lens.Lens' ContainerRecipe (Prelude.Maybe Prelude.Text)
containerRecipe_workingDirectory = Lens.lens (\ContainerRecipe' {workingDirectory} -> workingDirectory) (\s@ContainerRecipe' {} a -> s {workingDirectory = a} :: ContainerRecipe)

instance Data.FromJSON ContainerRecipe where
  parseJSON =
    Data.withObject
      "ContainerRecipe"
      ( \x ->
          ContainerRecipe'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "components")
            Prelude.<*> (x Data..:? "containerType")
            Prelude.<*> (x Data..:? "dateCreated")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "dockerfileTemplateData")
            Prelude.<*> (x Data..:? "encrypted")
            Prelude.<*> (x Data..:? "instanceConfiguration")
            Prelude.<*> (x Data..:? "kmsKeyId")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "owner")
            Prelude.<*> (x Data..:? "parentImage")
            Prelude.<*> (x Data..:? "platform")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "targetRepository")
            Prelude.<*> (x Data..:? "version")
            Prelude.<*> (x Data..:? "workingDirectory")
      )

instance Prelude.Hashable ContainerRecipe where
  hashWithSalt _salt ContainerRecipe' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` components
      `Prelude.hashWithSalt` containerType
      `Prelude.hashWithSalt` dateCreated
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` dockerfileTemplateData
      `Prelude.hashWithSalt` encrypted
      `Prelude.hashWithSalt` instanceConfiguration
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` parentImage
      `Prelude.hashWithSalt` platform
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` targetRepository
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` workingDirectory

instance Prelude.NFData ContainerRecipe where
  rnf ContainerRecipe' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf components
      `Prelude.seq` Prelude.rnf containerType
      `Prelude.seq` Prelude.rnf dateCreated
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf dockerfileTemplateData
      `Prelude.seq` Prelude.rnf encrypted
      `Prelude.seq` Prelude.rnf instanceConfiguration
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf owner
      `Prelude.seq` Prelude.rnf parentImage
      `Prelude.seq` Prelude.rnf platform
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf targetRepository
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf workingDirectory
