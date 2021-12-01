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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeArtifact.Types.PackageSummary where

import Amazonka.CodeArtifact.Types.PackageFormat
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Details about a package, including its format, namespace, and name. The
-- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_ListPackages.html ListPackages>
-- operation returns a list of @PackageSummary@ objects.
--
-- /See:/ 'newPackageSummary' smart constructor.
data PackageSummary = PackageSummary'
  { -- | The format of the package. Valid values are:
    --
    -- -   @npm@
    --
    -- -   @pypi@
    --
    -- -   @maven@
    format :: Prelude.Maybe PackageFormat,
    -- | The namespace of the package. The package component that specifies its
    -- namespace depends on its type. For example:
    --
    -- -   The namespace of a Maven package is its @groupId@.
    --
    -- -   The namespace of an npm package is its @scope@.
    --
    -- -   A Python package does not contain a corresponding component, so
    --     Python packages do not have a namespace.
    namespace :: Prelude.Maybe Prelude.Text,
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
-- 'format', 'packageSummary_format' - The format of the package. Valid values are:
--
-- -   @npm@
--
-- -   @pypi@
--
-- -   @maven@
--
-- 'namespace', 'packageSummary_namespace' - The namespace of the package. The package component that specifies its
-- namespace depends on its type. For example:
--
-- -   The namespace of a Maven package is its @groupId@.
--
-- -   The namespace of an npm package is its @scope@.
--
-- -   A Python package does not contain a corresponding component, so
--     Python packages do not have a namespace.
--
-- 'package', 'packageSummary_package' - The name of the package.
newPackageSummary ::
  PackageSummary
newPackageSummary =
  PackageSummary'
    { format = Prelude.Nothing,
      namespace = Prelude.Nothing,
      package = Prelude.Nothing
    }

-- | The format of the package. Valid values are:
--
-- -   @npm@
--
-- -   @pypi@
--
-- -   @maven@
packageSummary_format :: Lens.Lens' PackageSummary (Prelude.Maybe PackageFormat)
packageSummary_format = Lens.lens (\PackageSummary' {format} -> format) (\s@PackageSummary' {} a -> s {format = a} :: PackageSummary)

-- | The namespace of the package. The package component that specifies its
-- namespace depends on its type. For example:
--
-- -   The namespace of a Maven package is its @groupId@.
--
-- -   The namespace of an npm package is its @scope@.
--
-- -   A Python package does not contain a corresponding component, so
--     Python packages do not have a namespace.
packageSummary_namespace :: Lens.Lens' PackageSummary (Prelude.Maybe Prelude.Text)
packageSummary_namespace = Lens.lens (\PackageSummary' {namespace} -> namespace) (\s@PackageSummary' {} a -> s {namespace = a} :: PackageSummary)

-- | The name of the package.
packageSummary_package :: Lens.Lens' PackageSummary (Prelude.Maybe Prelude.Text)
packageSummary_package = Lens.lens (\PackageSummary' {package} -> package) (\s@PackageSummary' {} a -> s {package = a} :: PackageSummary)

instance Core.FromJSON PackageSummary where
  parseJSON =
    Core.withObject
      "PackageSummary"
      ( \x ->
          PackageSummary'
            Prelude.<$> (x Core..:? "format")
            Prelude.<*> (x Core..:? "namespace")
            Prelude.<*> (x Core..:? "package")
      )

instance Prelude.Hashable PackageSummary where
  hashWithSalt salt' PackageSummary' {..} =
    salt' `Prelude.hashWithSalt` package
      `Prelude.hashWithSalt` namespace
      `Prelude.hashWithSalt` format

instance Prelude.NFData PackageSummary where
  rnf PackageSummary' {..} =
    Prelude.rnf format
      `Prelude.seq` Prelude.rnf package
      `Prelude.seq` Prelude.rnf namespace
