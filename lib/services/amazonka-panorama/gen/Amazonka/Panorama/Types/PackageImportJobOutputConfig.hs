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
-- Module      : Amazonka.Panorama.Types.PackageImportJobOutputConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Panorama.Types.PackageImportJobOutputConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Panorama.Types.PackageVersionOutputConfig
import qualified Amazonka.Prelude as Prelude

-- | An output configuration for a package import job.
--
-- /See:/ 'newPackageImportJobOutputConfig' smart constructor.
data PackageImportJobOutputConfig = PackageImportJobOutputConfig'
  { -- | The package version\'s output configuration.
    packageVersionOutputConfig :: Prelude.Maybe PackageVersionOutputConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PackageImportJobOutputConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'packageVersionOutputConfig', 'packageImportJobOutputConfig_packageVersionOutputConfig' - The package version\'s output configuration.
newPackageImportJobOutputConfig ::
  PackageImportJobOutputConfig
newPackageImportJobOutputConfig =
  PackageImportJobOutputConfig'
    { packageVersionOutputConfig =
        Prelude.Nothing
    }

-- | The package version\'s output configuration.
packageImportJobOutputConfig_packageVersionOutputConfig :: Lens.Lens' PackageImportJobOutputConfig (Prelude.Maybe PackageVersionOutputConfig)
packageImportJobOutputConfig_packageVersionOutputConfig = Lens.lens (\PackageImportJobOutputConfig' {packageVersionOutputConfig} -> packageVersionOutputConfig) (\s@PackageImportJobOutputConfig' {} a -> s {packageVersionOutputConfig = a} :: PackageImportJobOutputConfig)

instance Data.FromJSON PackageImportJobOutputConfig where
  parseJSON =
    Data.withObject
      "PackageImportJobOutputConfig"
      ( \x ->
          PackageImportJobOutputConfig'
            Prelude.<$> (x Data..:? "PackageVersionOutputConfig")
      )

instance
  Prelude.Hashable
    PackageImportJobOutputConfig
  where
  hashWithSalt _salt PackageImportJobOutputConfig' {..} =
    _salt
      `Prelude.hashWithSalt` packageVersionOutputConfig

instance Prelude.NFData PackageImportJobOutputConfig where
  rnf PackageImportJobOutputConfig' {..} =
    Prelude.rnf packageVersionOutputConfig

instance Data.ToJSON PackageImportJobOutputConfig where
  toJSON PackageImportJobOutputConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PackageVersionOutputConfig" Data..=)
              Prelude.<$> packageVersionOutputConfig
          ]
      )
