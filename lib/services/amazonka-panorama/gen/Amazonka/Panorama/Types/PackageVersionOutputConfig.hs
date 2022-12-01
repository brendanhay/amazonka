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
-- Module      : Amazonka.Panorama.Types.PackageVersionOutputConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Panorama.Types.PackageVersionOutputConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A package version output configuration.
--
-- /See:/ 'newPackageVersionOutputConfig' smart constructor.
data PackageVersionOutputConfig = PackageVersionOutputConfig'
  { -- | Indicates that the version is recommended for all users.
    markLatest :: Prelude.Maybe Prelude.Bool,
    -- | The output\'s package name.
    packageName :: Prelude.Text,
    -- | The output\'s package version.
    packageVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PackageVersionOutputConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'markLatest', 'packageVersionOutputConfig_markLatest' - Indicates that the version is recommended for all users.
--
-- 'packageName', 'packageVersionOutputConfig_packageName' - The output\'s package name.
--
-- 'packageVersion', 'packageVersionOutputConfig_packageVersion' - The output\'s package version.
newPackageVersionOutputConfig ::
  -- | 'packageName'
  Prelude.Text ->
  -- | 'packageVersion'
  Prelude.Text ->
  PackageVersionOutputConfig
newPackageVersionOutputConfig
  pPackageName_
  pPackageVersion_ =
    PackageVersionOutputConfig'
      { markLatest =
          Prelude.Nothing,
        packageName = pPackageName_,
        packageVersion = pPackageVersion_
      }

-- | Indicates that the version is recommended for all users.
packageVersionOutputConfig_markLatest :: Lens.Lens' PackageVersionOutputConfig (Prelude.Maybe Prelude.Bool)
packageVersionOutputConfig_markLatest = Lens.lens (\PackageVersionOutputConfig' {markLatest} -> markLatest) (\s@PackageVersionOutputConfig' {} a -> s {markLatest = a} :: PackageVersionOutputConfig)

-- | The output\'s package name.
packageVersionOutputConfig_packageName :: Lens.Lens' PackageVersionOutputConfig Prelude.Text
packageVersionOutputConfig_packageName = Lens.lens (\PackageVersionOutputConfig' {packageName} -> packageName) (\s@PackageVersionOutputConfig' {} a -> s {packageName = a} :: PackageVersionOutputConfig)

-- | The output\'s package version.
packageVersionOutputConfig_packageVersion :: Lens.Lens' PackageVersionOutputConfig Prelude.Text
packageVersionOutputConfig_packageVersion = Lens.lens (\PackageVersionOutputConfig' {packageVersion} -> packageVersion) (\s@PackageVersionOutputConfig' {} a -> s {packageVersion = a} :: PackageVersionOutputConfig)

instance Core.FromJSON PackageVersionOutputConfig where
  parseJSON =
    Core.withObject
      "PackageVersionOutputConfig"
      ( \x ->
          PackageVersionOutputConfig'
            Prelude.<$> (x Core..:? "MarkLatest")
            Prelude.<*> (x Core..: "PackageName")
            Prelude.<*> (x Core..: "PackageVersion")
      )

instance Prelude.Hashable PackageVersionOutputConfig where
  hashWithSalt _salt PackageVersionOutputConfig' {..} =
    _salt `Prelude.hashWithSalt` markLatest
      `Prelude.hashWithSalt` packageName
      `Prelude.hashWithSalt` packageVersion

instance Prelude.NFData PackageVersionOutputConfig where
  rnf PackageVersionOutputConfig' {..} =
    Prelude.rnf markLatest
      `Prelude.seq` Prelude.rnf packageName
      `Prelude.seq` Prelude.rnf packageVersion

instance Core.ToJSON PackageVersionOutputConfig where
  toJSON PackageVersionOutputConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("MarkLatest" Core..=) Prelude.<$> markLatest,
            Prelude.Just ("PackageName" Core..= packageName),
            Prelude.Just
              ("PackageVersion" Core..= packageVersion)
          ]
      )
