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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.PackageFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.NumberFilter
import Amazonka.Inspector2.Types.StringFilter
import qualified Amazonka.Prelude as Prelude

-- | Contains information on the details of a package filter.
--
-- /See:/ 'newPackageFilter' smart constructor.
data PackageFilter = PackageFilter'
  { -- | An object that contains details on the package architecture type to
    -- filter on.
    architecture :: Prelude.Maybe StringFilter,
    -- | An object that contains details on the package epoch to filter on.
    epoch :: Prelude.Maybe NumberFilter,
    -- | An object that contains details on the name of the package to filter on.
    name :: Prelude.Maybe StringFilter,
    -- | An object that contains details on the package release to filter on.
    release :: Prelude.Maybe StringFilter,
    sourceLambdaLayerArn :: Prelude.Maybe StringFilter,
    -- | An object that contains details on the source layer hash to filter on.
    sourceLayerHash :: Prelude.Maybe StringFilter,
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
-- 'architecture', 'packageFilter_architecture' - An object that contains details on the package architecture type to
-- filter on.
--
-- 'epoch', 'packageFilter_epoch' - An object that contains details on the package epoch to filter on.
--
-- 'name', 'packageFilter_name' - An object that contains details on the name of the package to filter on.
--
-- 'release', 'packageFilter_release' - An object that contains details on the package release to filter on.
--
-- 'sourceLambdaLayerArn', 'packageFilter_sourceLambdaLayerArn' - Undocumented member.
--
-- 'sourceLayerHash', 'packageFilter_sourceLayerHash' - An object that contains details on the source layer hash to filter on.
--
-- 'version', 'packageFilter_version' - The package version to filter on.
newPackageFilter ::
  PackageFilter
newPackageFilter =
  PackageFilter'
    { architecture = Prelude.Nothing,
      epoch = Prelude.Nothing,
      name = Prelude.Nothing,
      release = Prelude.Nothing,
      sourceLambdaLayerArn = Prelude.Nothing,
      sourceLayerHash = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | An object that contains details on the package architecture type to
-- filter on.
packageFilter_architecture :: Lens.Lens' PackageFilter (Prelude.Maybe StringFilter)
packageFilter_architecture = Lens.lens (\PackageFilter' {architecture} -> architecture) (\s@PackageFilter' {} a -> s {architecture = a} :: PackageFilter)

-- | An object that contains details on the package epoch to filter on.
packageFilter_epoch :: Lens.Lens' PackageFilter (Prelude.Maybe NumberFilter)
packageFilter_epoch = Lens.lens (\PackageFilter' {epoch} -> epoch) (\s@PackageFilter' {} a -> s {epoch = a} :: PackageFilter)

-- | An object that contains details on the name of the package to filter on.
packageFilter_name :: Lens.Lens' PackageFilter (Prelude.Maybe StringFilter)
packageFilter_name = Lens.lens (\PackageFilter' {name} -> name) (\s@PackageFilter' {} a -> s {name = a} :: PackageFilter)

-- | An object that contains details on the package release to filter on.
packageFilter_release :: Lens.Lens' PackageFilter (Prelude.Maybe StringFilter)
packageFilter_release = Lens.lens (\PackageFilter' {release} -> release) (\s@PackageFilter' {} a -> s {release = a} :: PackageFilter)

-- | Undocumented member.
packageFilter_sourceLambdaLayerArn :: Lens.Lens' PackageFilter (Prelude.Maybe StringFilter)
packageFilter_sourceLambdaLayerArn = Lens.lens (\PackageFilter' {sourceLambdaLayerArn} -> sourceLambdaLayerArn) (\s@PackageFilter' {} a -> s {sourceLambdaLayerArn = a} :: PackageFilter)

-- | An object that contains details on the source layer hash to filter on.
packageFilter_sourceLayerHash :: Lens.Lens' PackageFilter (Prelude.Maybe StringFilter)
packageFilter_sourceLayerHash = Lens.lens (\PackageFilter' {sourceLayerHash} -> sourceLayerHash) (\s@PackageFilter' {} a -> s {sourceLayerHash = a} :: PackageFilter)

-- | The package version to filter on.
packageFilter_version :: Lens.Lens' PackageFilter (Prelude.Maybe StringFilter)
packageFilter_version = Lens.lens (\PackageFilter' {version} -> version) (\s@PackageFilter' {} a -> s {version = a} :: PackageFilter)

instance Data.FromJSON PackageFilter where
  parseJSON =
    Data.withObject
      "PackageFilter"
      ( \x ->
          PackageFilter'
            Prelude.<$> (x Data..:? "architecture")
            Prelude.<*> (x Data..:? "epoch")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "release")
            Prelude.<*> (x Data..:? "sourceLambdaLayerArn")
            Prelude.<*> (x Data..:? "sourceLayerHash")
            Prelude.<*> (x Data..:? "version")
      )

instance Prelude.Hashable PackageFilter where
  hashWithSalt _salt PackageFilter' {..} =
    _salt
      `Prelude.hashWithSalt` architecture
      `Prelude.hashWithSalt` epoch
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` release
      `Prelude.hashWithSalt` sourceLambdaLayerArn
      `Prelude.hashWithSalt` sourceLayerHash
      `Prelude.hashWithSalt` version

instance Prelude.NFData PackageFilter where
  rnf PackageFilter' {..} =
    Prelude.rnf architecture
      `Prelude.seq` Prelude.rnf epoch
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf release
      `Prelude.seq` Prelude.rnf sourceLambdaLayerArn
      `Prelude.seq` Prelude.rnf sourceLayerHash
      `Prelude.seq` Prelude.rnf version

instance Data.ToJSON PackageFilter where
  toJSON PackageFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("architecture" Data..=) Prelude.<$> architecture,
            ("epoch" Data..=) Prelude.<$> epoch,
            ("name" Data..=) Prelude.<$> name,
            ("release" Data..=) Prelude.<$> release,
            ("sourceLambdaLayerArn" Data..=)
              Prelude.<$> sourceLambdaLayerArn,
            ("sourceLayerHash" Data..=)
              Prelude.<$> sourceLayerHash,
            ("version" Data..=) Prelude.<$> version
          ]
      )
