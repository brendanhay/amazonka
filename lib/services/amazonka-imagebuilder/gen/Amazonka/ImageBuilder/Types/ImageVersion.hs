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
-- Module      : Amazonka.ImageBuilder.Types.ImageVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.ImageVersion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types.BuildType
import Amazonka.ImageBuilder.Types.ImageType
import Amazonka.ImageBuilder.Types.Platform
import qualified Amazonka.Prelude as Prelude

-- | The defining characteristics of a specific version of an Image Builder
-- image.
--
-- /See:/ 'newImageVersion' smart constructor.
data ImageVersion = ImageVersion'
  { -- | The Amazon Resource Name (ARN) of a specific version of an Image Builder
    -- image.
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
    -- | The date on which this specific version of the Image Builder image was
    -- created.
    dateCreated :: Prelude.Maybe Prelude.Text,
    -- | The name of this specific version of an Image Builder image.
    name :: Prelude.Maybe Prelude.Text,
    -- | The operating system version of the Amazon EC2 build instance. For
    -- example, Amazon Linux 2, Ubuntu 18, or Microsoft Windows Server 2019.
    osVersion :: Prelude.Maybe Prelude.Text,
    -- | The owner of the image version.
    owner :: Prelude.Maybe Prelude.Text,
    -- | The platform of the image version, for example \"Windows\" or \"Linux\".
    platform :: Prelude.Maybe Platform,
    -- | Specifies whether this image is an AMI or a container image.
    type' :: Prelude.Maybe ImageType,
    -- | Details for a specific version of an Image Builder image. This version
    -- follows the semantic version syntax.
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
-- Create a value of 'ImageVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'imageVersion_arn' - The Amazon Resource Name (ARN) of a specific version of an Image Builder
-- image.
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
-- 'buildType', 'imageVersion_buildType' - Indicates the type of build that created this image. The build can be
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
-- 'dateCreated', 'imageVersion_dateCreated' - The date on which this specific version of the Image Builder image was
-- created.
--
-- 'name', 'imageVersion_name' - The name of this specific version of an Image Builder image.
--
-- 'osVersion', 'imageVersion_osVersion' - The operating system version of the Amazon EC2 build instance. For
-- example, Amazon Linux 2, Ubuntu 18, or Microsoft Windows Server 2019.
--
-- 'owner', 'imageVersion_owner' - The owner of the image version.
--
-- 'platform', 'imageVersion_platform' - The platform of the image version, for example \"Windows\" or \"Linux\".
--
-- 'type'', 'imageVersion_type' - Specifies whether this image is an AMI or a container image.
--
-- 'version', 'imageVersion_version' - Details for a specific version of an Image Builder image. This version
-- follows the semantic version syntax.
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
newImageVersion ::
  ImageVersion
newImageVersion =
  ImageVersion'
    { arn = Prelude.Nothing,
      buildType = Prelude.Nothing,
      dateCreated = Prelude.Nothing,
      name = Prelude.Nothing,
      osVersion = Prelude.Nothing,
      owner = Prelude.Nothing,
      platform = Prelude.Nothing,
      type' = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of a specific version of an Image Builder
-- image.
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
imageVersion_arn :: Lens.Lens' ImageVersion (Prelude.Maybe Prelude.Text)
imageVersion_arn = Lens.lens (\ImageVersion' {arn} -> arn) (\s@ImageVersion' {} a -> s {arn = a} :: ImageVersion)

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
imageVersion_buildType :: Lens.Lens' ImageVersion (Prelude.Maybe BuildType)
imageVersion_buildType = Lens.lens (\ImageVersion' {buildType} -> buildType) (\s@ImageVersion' {} a -> s {buildType = a} :: ImageVersion)

-- | The date on which this specific version of the Image Builder image was
-- created.
imageVersion_dateCreated :: Lens.Lens' ImageVersion (Prelude.Maybe Prelude.Text)
imageVersion_dateCreated = Lens.lens (\ImageVersion' {dateCreated} -> dateCreated) (\s@ImageVersion' {} a -> s {dateCreated = a} :: ImageVersion)

-- | The name of this specific version of an Image Builder image.
imageVersion_name :: Lens.Lens' ImageVersion (Prelude.Maybe Prelude.Text)
imageVersion_name = Lens.lens (\ImageVersion' {name} -> name) (\s@ImageVersion' {} a -> s {name = a} :: ImageVersion)

-- | The operating system version of the Amazon EC2 build instance. For
-- example, Amazon Linux 2, Ubuntu 18, or Microsoft Windows Server 2019.
imageVersion_osVersion :: Lens.Lens' ImageVersion (Prelude.Maybe Prelude.Text)
imageVersion_osVersion = Lens.lens (\ImageVersion' {osVersion} -> osVersion) (\s@ImageVersion' {} a -> s {osVersion = a} :: ImageVersion)

-- | The owner of the image version.
imageVersion_owner :: Lens.Lens' ImageVersion (Prelude.Maybe Prelude.Text)
imageVersion_owner = Lens.lens (\ImageVersion' {owner} -> owner) (\s@ImageVersion' {} a -> s {owner = a} :: ImageVersion)

-- | The platform of the image version, for example \"Windows\" or \"Linux\".
imageVersion_platform :: Lens.Lens' ImageVersion (Prelude.Maybe Platform)
imageVersion_platform = Lens.lens (\ImageVersion' {platform} -> platform) (\s@ImageVersion' {} a -> s {platform = a} :: ImageVersion)

-- | Specifies whether this image is an AMI or a container image.
imageVersion_type :: Lens.Lens' ImageVersion (Prelude.Maybe ImageType)
imageVersion_type = Lens.lens (\ImageVersion' {type'} -> type') (\s@ImageVersion' {} a -> s {type' = a} :: ImageVersion)

-- | Details for a specific version of an Image Builder image. This version
-- follows the semantic version syntax.
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
imageVersion_version :: Lens.Lens' ImageVersion (Prelude.Maybe Prelude.Text)
imageVersion_version = Lens.lens (\ImageVersion' {version} -> version) (\s@ImageVersion' {} a -> s {version = a} :: ImageVersion)

instance Data.FromJSON ImageVersion where
  parseJSON =
    Data.withObject
      "ImageVersion"
      ( \x ->
          ImageVersion'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "buildType")
            Prelude.<*> (x Data..:? "dateCreated")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "osVersion")
            Prelude.<*> (x Data..:? "owner")
            Prelude.<*> (x Data..:? "platform")
            Prelude.<*> (x Data..:? "type")
            Prelude.<*> (x Data..:? "version")
      )

instance Prelude.Hashable ImageVersion where
  hashWithSalt _salt ImageVersion' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` buildType
      `Prelude.hashWithSalt` dateCreated
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` osVersion
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` platform
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` version

instance Prelude.NFData ImageVersion where
  rnf ImageVersion' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf buildType
      `Prelude.seq` Prelude.rnf dateCreated
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf osVersion
      `Prelude.seq` Prelude.rnf owner
      `Prelude.seq` Prelude.rnf platform
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf version
