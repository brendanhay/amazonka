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
-- Module      : Amazonka.Panorama.Types.PackageImportJobInputConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Panorama.Types.PackageImportJobInputConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Panorama.Types.PackageVersionInputConfig
import qualified Amazonka.Prelude as Prelude

-- | A configuration for a package import job.
--
-- /See:/ 'newPackageImportJobInputConfig' smart constructor.
data PackageImportJobInputConfig = PackageImportJobInputConfig'
  { -- | The package version\'s input configuration.
    packageVersionInputConfig :: Prelude.Maybe PackageVersionInputConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PackageImportJobInputConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'packageVersionInputConfig', 'packageImportJobInputConfig_packageVersionInputConfig' - The package version\'s input configuration.
newPackageImportJobInputConfig ::
  PackageImportJobInputConfig
newPackageImportJobInputConfig =
  PackageImportJobInputConfig'
    { packageVersionInputConfig =
        Prelude.Nothing
    }

-- | The package version\'s input configuration.
packageImportJobInputConfig_packageVersionInputConfig :: Lens.Lens' PackageImportJobInputConfig (Prelude.Maybe PackageVersionInputConfig)
packageImportJobInputConfig_packageVersionInputConfig = Lens.lens (\PackageImportJobInputConfig' {packageVersionInputConfig} -> packageVersionInputConfig) (\s@PackageImportJobInputConfig' {} a -> s {packageVersionInputConfig = a} :: PackageImportJobInputConfig)

instance Data.FromJSON PackageImportJobInputConfig where
  parseJSON =
    Data.withObject
      "PackageImportJobInputConfig"
      ( \x ->
          PackageImportJobInputConfig'
            Prelude.<$> (x Data..:? "PackageVersionInputConfig")
      )

instance Prelude.Hashable PackageImportJobInputConfig where
  hashWithSalt _salt PackageImportJobInputConfig' {..} =
    _salt
      `Prelude.hashWithSalt` packageVersionInputConfig

instance Prelude.NFData PackageImportJobInputConfig where
  rnf PackageImportJobInputConfig' {..} =
    Prelude.rnf packageVersionInputConfig

instance Data.ToJSON PackageImportJobInputConfig where
  toJSON PackageImportJobInputConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PackageVersionInputConfig" Data..=)
              Prelude.<$> packageVersionInputConfig
          ]
      )
