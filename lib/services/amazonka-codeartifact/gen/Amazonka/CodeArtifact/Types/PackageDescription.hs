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
-- Module      : Amazonka.CodeArtifact.Types.PackageDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeArtifact.Types.PackageDescription where

import Amazonka.CodeArtifact.Types.PackageFormat
import Amazonka.CodeArtifact.Types.PackageOriginConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Details about a package.
--
-- /See:/ 'newPackageDescription' smart constructor.
data PackageDescription = PackageDescription'
  { -- | The name of the package.
    name :: Prelude.Maybe Prelude.Text,
    -- | A format that specifies the type of the package.
    format :: Prelude.Maybe PackageFormat,
    -- | The package origin configuration for the package.
    originConfiguration :: Prelude.Maybe PackageOriginConfiguration,
    -- | The namespace of the package. The package component that specifies its
    -- namespace depends on its type. For example:
    --
    -- -   The namespace of a Maven package is its @groupId@.
    --
    -- -   The namespace of an npm package is its @scope@.
    --
    -- -   Python and NuGet packages do not contain a corresponding component,
    --     packages of those formats do not have a namespace.
    namespace :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PackageDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'packageDescription_name' - The name of the package.
--
-- 'format', 'packageDescription_format' - A format that specifies the type of the package.
--
-- 'originConfiguration', 'packageDescription_originConfiguration' - The package origin configuration for the package.
--
-- 'namespace', 'packageDescription_namespace' - The namespace of the package. The package component that specifies its
-- namespace depends on its type. For example:
--
-- -   The namespace of a Maven package is its @groupId@.
--
-- -   The namespace of an npm package is its @scope@.
--
-- -   Python and NuGet packages do not contain a corresponding component,
--     packages of those formats do not have a namespace.
newPackageDescription ::
  PackageDescription
newPackageDescription =
  PackageDescription'
    { name = Prelude.Nothing,
      format = Prelude.Nothing,
      originConfiguration = Prelude.Nothing,
      namespace = Prelude.Nothing
    }

-- | The name of the package.
packageDescription_name :: Lens.Lens' PackageDescription (Prelude.Maybe Prelude.Text)
packageDescription_name = Lens.lens (\PackageDescription' {name} -> name) (\s@PackageDescription' {} a -> s {name = a} :: PackageDescription)

-- | A format that specifies the type of the package.
packageDescription_format :: Lens.Lens' PackageDescription (Prelude.Maybe PackageFormat)
packageDescription_format = Lens.lens (\PackageDescription' {format} -> format) (\s@PackageDescription' {} a -> s {format = a} :: PackageDescription)

-- | The package origin configuration for the package.
packageDescription_originConfiguration :: Lens.Lens' PackageDescription (Prelude.Maybe PackageOriginConfiguration)
packageDescription_originConfiguration = Lens.lens (\PackageDescription' {originConfiguration} -> originConfiguration) (\s@PackageDescription' {} a -> s {originConfiguration = a} :: PackageDescription)

-- | The namespace of the package. The package component that specifies its
-- namespace depends on its type. For example:
--
-- -   The namespace of a Maven package is its @groupId@.
--
-- -   The namespace of an npm package is its @scope@.
--
-- -   Python and NuGet packages do not contain a corresponding component,
--     packages of those formats do not have a namespace.
packageDescription_namespace :: Lens.Lens' PackageDescription (Prelude.Maybe Prelude.Text)
packageDescription_namespace = Lens.lens (\PackageDescription' {namespace} -> namespace) (\s@PackageDescription' {} a -> s {namespace = a} :: PackageDescription)

instance Core.FromJSON PackageDescription where
  parseJSON =
    Core.withObject
      "PackageDescription"
      ( \x ->
          PackageDescription'
            Prelude.<$> (x Core..:? "name")
            Prelude.<*> (x Core..:? "format")
            Prelude.<*> (x Core..:? "originConfiguration")
            Prelude.<*> (x Core..:? "namespace")
      )

instance Prelude.Hashable PackageDescription where
  hashWithSalt _salt PackageDescription' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` format
      `Prelude.hashWithSalt` originConfiguration
      `Prelude.hashWithSalt` namespace

instance Prelude.NFData PackageDescription where
  rnf PackageDescription' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf format
      `Prelude.seq` Prelude.rnf originConfiguration
      `Prelude.seq` Prelude.rnf namespace
