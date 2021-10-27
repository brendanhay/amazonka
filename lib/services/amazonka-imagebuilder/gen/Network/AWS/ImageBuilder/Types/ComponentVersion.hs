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
-- Module      : Network.AWS.ImageBuilder.Types.ComponentVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ImageBuilder.Types.ComponentVersion where

import qualified Network.AWS.Core as Core
import Network.AWS.ImageBuilder.Types.ComponentType
import Network.AWS.ImageBuilder.Types.Platform
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The defining characteristics of a specific version of an Amazon Web
-- Services TOE component.
--
-- /See:/ 'newComponentVersion' smart constructor.
data ComponentVersion = ComponentVersion'
  { -- | The platform of the component.
    platform :: Prelude.Maybe Platform,
    -- | The Amazon Resource Name (ARN) of the component.
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
    -- | he operating system (OS) version supported by the component. If the OS
    -- information is available, a prefix match is performed against the base
    -- image OS version during image recipe creation.
    supportedOsVersions :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The owner of the component.
    owner :: Prelude.Maybe Prelude.Text,
    -- | The date that the component was created.
    dateCreated :: Prelude.Maybe Prelude.Text,
    -- | The name of the component.
    name :: Prelude.Maybe Prelude.Text,
    -- | The semantic version of the component.
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
    -- | The type of the component denotes whether the component is used to build
    -- the image or only to test it.
    type' :: Prelude.Maybe ComponentType,
    -- | The description of the component.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComponentVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'platform', 'componentVersion_platform' - The platform of the component.
--
-- 'arn', 'componentVersion_arn' - The Amazon Resource Name (ARN) of the component.
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
-- 'supportedOsVersions', 'componentVersion_supportedOsVersions' - he operating system (OS) version supported by the component. If the OS
-- information is available, a prefix match is performed against the base
-- image OS version during image recipe creation.
--
-- 'owner', 'componentVersion_owner' - The owner of the component.
--
-- 'dateCreated', 'componentVersion_dateCreated' - The date that the component was created.
--
-- 'name', 'componentVersion_name' - The name of the component.
--
-- 'version', 'componentVersion_version' - The semantic version of the component.
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
-- 'type'', 'componentVersion_type' - The type of the component denotes whether the component is used to build
-- the image or only to test it.
--
-- 'description', 'componentVersion_description' - The description of the component.
newComponentVersion ::
  ComponentVersion
newComponentVersion =
  ComponentVersion'
    { platform = Prelude.Nothing,
      arn = Prelude.Nothing,
      supportedOsVersions = Prelude.Nothing,
      owner = Prelude.Nothing,
      dateCreated = Prelude.Nothing,
      name = Prelude.Nothing,
      version = Prelude.Nothing,
      type' = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The platform of the component.
componentVersion_platform :: Lens.Lens' ComponentVersion (Prelude.Maybe Platform)
componentVersion_platform = Lens.lens (\ComponentVersion' {platform} -> platform) (\s@ComponentVersion' {} a -> s {platform = a} :: ComponentVersion)

-- | The Amazon Resource Name (ARN) of the component.
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
componentVersion_arn :: Lens.Lens' ComponentVersion (Prelude.Maybe Prelude.Text)
componentVersion_arn = Lens.lens (\ComponentVersion' {arn} -> arn) (\s@ComponentVersion' {} a -> s {arn = a} :: ComponentVersion)

-- | he operating system (OS) version supported by the component. If the OS
-- information is available, a prefix match is performed against the base
-- image OS version during image recipe creation.
componentVersion_supportedOsVersions :: Lens.Lens' ComponentVersion (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
componentVersion_supportedOsVersions = Lens.lens (\ComponentVersion' {supportedOsVersions} -> supportedOsVersions) (\s@ComponentVersion' {} a -> s {supportedOsVersions = a} :: ComponentVersion) Prelude.. Lens.mapping Lens.coerced

-- | The owner of the component.
componentVersion_owner :: Lens.Lens' ComponentVersion (Prelude.Maybe Prelude.Text)
componentVersion_owner = Lens.lens (\ComponentVersion' {owner} -> owner) (\s@ComponentVersion' {} a -> s {owner = a} :: ComponentVersion)

-- | The date that the component was created.
componentVersion_dateCreated :: Lens.Lens' ComponentVersion (Prelude.Maybe Prelude.Text)
componentVersion_dateCreated = Lens.lens (\ComponentVersion' {dateCreated} -> dateCreated) (\s@ComponentVersion' {} a -> s {dateCreated = a} :: ComponentVersion)

-- | The name of the component.
componentVersion_name :: Lens.Lens' ComponentVersion (Prelude.Maybe Prelude.Text)
componentVersion_name = Lens.lens (\ComponentVersion' {name} -> name) (\s@ComponentVersion' {} a -> s {name = a} :: ComponentVersion)

-- | The semantic version of the component.
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
componentVersion_version :: Lens.Lens' ComponentVersion (Prelude.Maybe Prelude.Text)
componentVersion_version = Lens.lens (\ComponentVersion' {version} -> version) (\s@ComponentVersion' {} a -> s {version = a} :: ComponentVersion)

-- | The type of the component denotes whether the component is used to build
-- the image or only to test it.
componentVersion_type :: Lens.Lens' ComponentVersion (Prelude.Maybe ComponentType)
componentVersion_type = Lens.lens (\ComponentVersion' {type'} -> type') (\s@ComponentVersion' {} a -> s {type' = a} :: ComponentVersion)

-- | The description of the component.
componentVersion_description :: Lens.Lens' ComponentVersion (Prelude.Maybe Prelude.Text)
componentVersion_description = Lens.lens (\ComponentVersion' {description} -> description) (\s@ComponentVersion' {} a -> s {description = a} :: ComponentVersion)

instance Core.FromJSON ComponentVersion where
  parseJSON =
    Core.withObject
      "ComponentVersion"
      ( \x ->
          ComponentVersion'
            Prelude.<$> (x Core..:? "platform")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "supportedOsVersions")
            Prelude.<*> (x Core..:? "owner")
            Prelude.<*> (x Core..:? "dateCreated")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "version")
            Prelude.<*> (x Core..:? "type")
            Prelude.<*> (x Core..:? "description")
      )

instance Prelude.Hashable ComponentVersion

instance Prelude.NFData ComponentVersion
