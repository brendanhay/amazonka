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
-- Module      : Amazonka.Inspector2.Types.PackageFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.PackageFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Inspector2.Types.NumberFilter
import Amazonka.Inspector2.Types.StringFilter
import qualified Amazonka.Prelude as Prelude

-- | Contains information on the details of a package filter.
--
-- /See:/ 'newPackageFilter' smart constructor.
data PackageFilter = PackageFilter'
  { -- | An object that contains details on the name of the package to filter on.
    name :: Prelude.Maybe StringFilter,
    -- | An object that contains details on the package epoch to filter on.
    epoch :: Prelude.Maybe NumberFilter,
    -- | An object that contains details on the package release to filter on.
    release :: Prelude.Maybe StringFilter,
    -- | An object that contains details on the source layer hash to filter on.
    sourceLayerHash :: Prelude.Maybe StringFilter,
    -- | An object that contains details on the package architecture type to
    -- filter on.
    architecture :: Prelude.Maybe StringFilter,
    -- | The package version to filter on.
    version :: Prelude.Maybe StringFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PackageFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'packageFilter_name' - An object that contains details on the name of the package to filter on.
--
-- 'epoch', 'packageFilter_epoch' - An object that contains details on the package epoch to filter on.
--
-- 'release', 'packageFilter_release' - An object that contains details on the package release to filter on.
--
-- 'sourceLayerHash', 'packageFilter_sourceLayerHash' - An object that contains details on the source layer hash to filter on.
--
-- 'architecture', 'packageFilter_architecture' - An object that contains details on the package architecture type to
-- filter on.
--
-- 'version', 'packageFilter_version' - The package version to filter on.
newPackageFilter ::
  PackageFilter
newPackageFilter =
  PackageFilter'
    { name = Prelude.Nothing,
      epoch = Prelude.Nothing,
      release = Prelude.Nothing,
      sourceLayerHash = Prelude.Nothing,
      architecture = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | An object that contains details on the name of the package to filter on.
packageFilter_name :: Lens.Lens' PackageFilter (Prelude.Maybe StringFilter)
packageFilter_name = Lens.lens (\PackageFilter' {name} -> name) (\s@PackageFilter' {} a -> s {name = a} :: PackageFilter)

-- | An object that contains details on the package epoch to filter on.
packageFilter_epoch :: Lens.Lens' PackageFilter (Prelude.Maybe NumberFilter)
packageFilter_epoch = Lens.lens (\PackageFilter' {epoch} -> epoch) (\s@PackageFilter' {} a -> s {epoch = a} :: PackageFilter)

-- | An object that contains details on the package release to filter on.
packageFilter_release :: Lens.Lens' PackageFilter (Prelude.Maybe StringFilter)
packageFilter_release = Lens.lens (\PackageFilter' {release} -> release) (\s@PackageFilter' {} a -> s {release = a} :: PackageFilter)

-- | An object that contains details on the source layer hash to filter on.
packageFilter_sourceLayerHash :: Lens.Lens' PackageFilter (Prelude.Maybe StringFilter)
packageFilter_sourceLayerHash = Lens.lens (\PackageFilter' {sourceLayerHash} -> sourceLayerHash) (\s@PackageFilter' {} a -> s {sourceLayerHash = a} :: PackageFilter)

-- | An object that contains details on the package architecture type to
-- filter on.
packageFilter_architecture :: Lens.Lens' PackageFilter (Prelude.Maybe StringFilter)
packageFilter_architecture = Lens.lens (\PackageFilter' {architecture} -> architecture) (\s@PackageFilter' {} a -> s {architecture = a} :: PackageFilter)

-- | The package version to filter on.
packageFilter_version :: Lens.Lens' PackageFilter (Prelude.Maybe StringFilter)
packageFilter_version = Lens.lens (\PackageFilter' {version} -> version) (\s@PackageFilter' {} a -> s {version = a} :: PackageFilter)

instance Core.FromJSON PackageFilter where
  parseJSON =
    Core.withObject
      "PackageFilter"
      ( \x ->
          PackageFilter'
            Prelude.<$> (x Core..:? "name")
            Prelude.<*> (x Core..:? "epoch")
            Prelude.<*> (x Core..:? "release")
            Prelude.<*> (x Core..:? "sourceLayerHash")
            Prelude.<*> (x Core..:? "architecture")
            Prelude.<*> (x Core..:? "version")
      )

instance Prelude.Hashable PackageFilter where
  hashWithSalt _salt PackageFilter' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` epoch
      `Prelude.hashWithSalt` release
      `Prelude.hashWithSalt` sourceLayerHash
      `Prelude.hashWithSalt` architecture
      `Prelude.hashWithSalt` version

instance Prelude.NFData PackageFilter where
  rnf PackageFilter' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf epoch
      `Prelude.seq` Prelude.rnf release
      `Prelude.seq` Prelude.rnf sourceLayerHash
      `Prelude.seq` Prelude.rnf architecture
      `Prelude.seq` Prelude.rnf version

instance Core.ToJSON PackageFilter where
  toJSON PackageFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("name" Core..=) Prelude.<$> name,
            ("epoch" Core..=) Prelude.<$> epoch,
            ("release" Core..=) Prelude.<$> release,
            ("sourceLayerHash" Core..=)
              Prelude.<$> sourceLayerHash,
            ("architecture" Core..=) Prelude.<$> architecture,
            ("version" Core..=) Prelude.<$> version
          ]
      )
