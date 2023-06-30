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
-- Module      : Amazonka.CodeArtifact.Types.PackageSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeArtifact.Types.PackageSummary where

import Amazonka.CodeArtifact.Types.PackageFormat
import Amazonka.CodeArtifact.Types.PackageOriginConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about a package, including its format, namespace, and name. The
-- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_ListPackages.html ListPackages>
-- operation returns a list of @PackageSummary@ objects.
--
-- /See:/ 'newPackageSummary' smart constructor.
data PackageSummary = PackageSummary'
  { -- | The format of the package.
    format :: Prelude.Maybe PackageFormat,
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
    -- | A
    -- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_PackageOriginConfiguration.html PackageOriginConfiguration>
    -- object that contains a
    -- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_PackageOriginRestrictions.html PackageOriginRestrictions>
    -- object that contains information about the upstream and publish package
    -- origin restrictions.
    originConfiguration :: Prelude.Maybe PackageOriginConfiguration,
    -- | The name of the package.
    package :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PackageSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'format', 'packageSummary_format' - The format of the package.
--
-- 'namespace', 'packageSummary_namespace' - The namespace of the package. The package component that specifies its
-- namespace depends on its type. For example:
--
-- -   The namespace of a Maven package is its @groupId@.
--
-- -   The namespace of an npm package is its @scope@.
--
-- -   Python and NuGet packages do not contain a corresponding component,
--     packages of those formats do not have a namespace.
--
-- 'originConfiguration', 'packageSummary_originConfiguration' - A
-- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_PackageOriginConfiguration.html PackageOriginConfiguration>
-- object that contains a
-- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_PackageOriginRestrictions.html PackageOriginRestrictions>
-- object that contains information about the upstream and publish package
-- origin restrictions.
--
-- 'package', 'packageSummary_package' - The name of the package.
newPackageSummary ::
  PackageSummary
newPackageSummary =
  PackageSummary'
    { format = Prelude.Nothing,
      namespace = Prelude.Nothing,
      originConfiguration = Prelude.Nothing,
      package = Prelude.Nothing
    }

-- | The format of the package.
packageSummary_format :: Lens.Lens' PackageSummary (Prelude.Maybe PackageFormat)
packageSummary_format = Lens.lens (\PackageSummary' {format} -> format) (\s@PackageSummary' {} a -> s {format = a} :: PackageSummary)

-- | The namespace of the package. The package component that specifies its
-- namespace depends on its type. For example:
--
-- -   The namespace of a Maven package is its @groupId@.
--
-- -   The namespace of an npm package is its @scope@.
--
-- -   Python and NuGet packages do not contain a corresponding component,
--     packages of those formats do not have a namespace.
packageSummary_namespace :: Lens.Lens' PackageSummary (Prelude.Maybe Prelude.Text)
packageSummary_namespace = Lens.lens (\PackageSummary' {namespace} -> namespace) (\s@PackageSummary' {} a -> s {namespace = a} :: PackageSummary)

-- | A
-- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_PackageOriginConfiguration.html PackageOriginConfiguration>
-- object that contains a
-- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_PackageOriginRestrictions.html PackageOriginRestrictions>
-- object that contains information about the upstream and publish package
-- origin restrictions.
packageSummary_originConfiguration :: Lens.Lens' PackageSummary (Prelude.Maybe PackageOriginConfiguration)
packageSummary_originConfiguration = Lens.lens (\PackageSummary' {originConfiguration} -> originConfiguration) (\s@PackageSummary' {} a -> s {originConfiguration = a} :: PackageSummary)

-- | The name of the package.
packageSummary_package :: Lens.Lens' PackageSummary (Prelude.Maybe Prelude.Text)
packageSummary_package = Lens.lens (\PackageSummary' {package} -> package) (\s@PackageSummary' {} a -> s {package = a} :: PackageSummary)

instance Data.FromJSON PackageSummary where
  parseJSON =
    Data.withObject
      "PackageSummary"
      ( \x ->
          PackageSummary'
            Prelude.<$> (x Data..:? "format")
            Prelude.<*> (x Data..:? "namespace")
            Prelude.<*> (x Data..:? "originConfiguration")
            Prelude.<*> (x Data..:? "package")
      )

instance Prelude.Hashable PackageSummary where
  hashWithSalt _salt PackageSummary' {..} =
    _salt
      `Prelude.hashWithSalt` format
      `Prelude.hashWithSalt` namespace
      `Prelude.hashWithSalt` originConfiguration
      `Prelude.hashWithSalt` package

instance Prelude.NFData PackageSummary where
  rnf PackageSummary' {..} =
    Prelude.rnf format
      `Prelude.seq` Prelude.rnf namespace
      `Prelude.seq` Prelude.rnf originConfiguration
      `Prelude.seq` Prelude.rnf package
