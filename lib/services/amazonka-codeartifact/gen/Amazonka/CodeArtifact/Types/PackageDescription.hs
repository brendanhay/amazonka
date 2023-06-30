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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeArtifact.Types.PackageDescription where

import Amazonka.CodeArtifact.Types.PackageFormat
import Amazonka.CodeArtifact.Types.PackageOriginConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about a package.
--
-- /See:/ 'newPackageDescription' smart constructor.
data PackageDescription = PackageDescription'
  { -- | A format that specifies the type of the package.
    format :: Prelude.Maybe PackageFormat,
    -- | The name of the package.
    name :: Prelude.Maybe Prelude.Text,
    -- | The namespace of the package. The package component that specifies its
    -- namespace depends on its type. For example:
    --
    -- -   The namespace of a Maven package is its @groupId@.
    --
    -- -   The namespace of an npm package is its @scope@.
    --
    -- -   Python and NuGet packages do not contain a corresponding component,
    --     packages of those formats do not have a namespace.
    namespace :: Prelude.Maybe Prelude.Text,
    -- | The package origin configuration for the package.
    originConfiguration :: Prelude.Maybe PackageOriginConfiguration
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
-- 'format', 'packageDescription_format' - A format that specifies the type of the package.
--
-- 'name', 'packageDescription_name' - The name of the package.
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
--
-- 'originConfiguration', 'packageDescription_originConfiguration' - The package origin configuration for the package.
newPackageDescription ::
  PackageDescription
newPackageDescription =
  PackageDescription'
    { format = Prelude.Nothing,
      name = Prelude.Nothing,
      namespace = Prelude.Nothing,
      originConfiguration = Prelude.Nothing
    }

-- | A format that specifies the type of the package.
packageDescription_format :: Lens.Lens' PackageDescription (Prelude.Maybe PackageFormat)
packageDescription_format = Lens.lens (\PackageDescription' {format} -> format) (\s@PackageDescription' {} a -> s {format = a} :: PackageDescription)

-- | The name of the package.
packageDescription_name :: Lens.Lens' PackageDescription (Prelude.Maybe Prelude.Text)
packageDescription_name = Lens.lens (\PackageDescription' {name} -> name) (\s@PackageDescription' {} a -> s {name = a} :: PackageDescription)

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

-- | The package origin configuration for the package.
packageDescription_originConfiguration :: Lens.Lens' PackageDescription (Prelude.Maybe PackageOriginConfiguration)
packageDescription_originConfiguration = Lens.lens (\PackageDescription' {originConfiguration} -> originConfiguration) (\s@PackageDescription' {} a -> s {originConfiguration = a} :: PackageDescription)

instance Data.FromJSON PackageDescription where
  parseJSON =
    Data.withObject
      "PackageDescription"
      ( \x ->
          PackageDescription'
            Prelude.<$> (x Data..:? "format")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "namespace")
            Prelude.<*> (x Data..:? "originConfiguration")
      )

instance Prelude.Hashable PackageDescription where
  hashWithSalt _salt PackageDescription' {..} =
    _salt
      `Prelude.hashWithSalt` format
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` namespace
      `Prelude.hashWithSalt` originConfiguration

instance Prelude.NFData PackageDescription where
  rnf PackageDescription' {..} =
    Prelude.rnf format
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf namespace
      `Prelude.seq` Prelude.rnf originConfiguration
