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
-- Module      : Amazonka.SecurityLake.Types.AutoEnableNewRegionConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityLake.Types.AutoEnableNewRegionConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityLake.Types.AwsLogSourceType
import Amazonka.SecurityLake.Types.Region

-- | Automatically enable new organization accounts as member accounts from
-- an Amazon Security Lake administrator account.
--
-- /See:/ 'newAutoEnableNewRegionConfiguration' smart constructor.
data AutoEnableNewRegionConfiguration = AutoEnableNewRegionConfiguration'
  { -- | The Amazon Web Services Regions where Security Lake is automatically
    -- enabled.
    region :: Region,
    -- | The Amazon Web Services sources that are automatically enabled in
    -- Security Lake.
    sources :: [AwsLogSourceType]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoEnableNewRegionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'region', 'autoEnableNewRegionConfiguration_region' - The Amazon Web Services Regions where Security Lake is automatically
-- enabled.
--
-- 'sources', 'autoEnableNewRegionConfiguration_sources' - The Amazon Web Services sources that are automatically enabled in
-- Security Lake.
newAutoEnableNewRegionConfiguration ::
  -- | 'region'
  Region ->
  AutoEnableNewRegionConfiguration
newAutoEnableNewRegionConfiguration pRegion_ =
  AutoEnableNewRegionConfiguration'
    { region =
        pRegion_,
      sources = Prelude.mempty
    }

-- | The Amazon Web Services Regions where Security Lake is automatically
-- enabled.
autoEnableNewRegionConfiguration_region :: Lens.Lens' AutoEnableNewRegionConfiguration Region
autoEnableNewRegionConfiguration_region = Lens.lens (\AutoEnableNewRegionConfiguration' {region} -> region) (\s@AutoEnableNewRegionConfiguration' {} a -> s {region = a} :: AutoEnableNewRegionConfiguration)

-- | The Amazon Web Services sources that are automatically enabled in
-- Security Lake.
autoEnableNewRegionConfiguration_sources :: Lens.Lens' AutoEnableNewRegionConfiguration [AwsLogSourceType]
autoEnableNewRegionConfiguration_sources = Lens.lens (\AutoEnableNewRegionConfiguration' {sources} -> sources) (\s@AutoEnableNewRegionConfiguration' {} a -> s {sources = a} :: AutoEnableNewRegionConfiguration) Prelude.. Lens.coerced

instance
  Data.FromJSON
    AutoEnableNewRegionConfiguration
  where
  parseJSON =
    Data.withObject
      "AutoEnableNewRegionConfiguration"
      ( \x ->
          AutoEnableNewRegionConfiguration'
            Prelude.<$> (x Data..: "region")
            Prelude.<*> (x Data..:? "sources" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    AutoEnableNewRegionConfiguration
  where
  hashWithSalt
    _salt
    AutoEnableNewRegionConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` region
        `Prelude.hashWithSalt` sources

instance
  Prelude.NFData
    AutoEnableNewRegionConfiguration
  where
  rnf AutoEnableNewRegionConfiguration' {..} =
    Prelude.rnf region
      `Prelude.seq` Prelude.rnf sources

instance Data.ToJSON AutoEnableNewRegionConfiguration where
  toJSON AutoEnableNewRegionConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("region" Data..= region),
            Prelude.Just ("sources" Data..= sources)
          ]
      )
