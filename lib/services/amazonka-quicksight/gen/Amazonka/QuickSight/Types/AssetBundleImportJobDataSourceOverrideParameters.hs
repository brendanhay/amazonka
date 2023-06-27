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
-- Module      : Amazonka.QuickSight.Types.AssetBundleImportJobDataSourceOverrideParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AssetBundleImportJobDataSourceOverrideParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.AssetBundleImportJobDataSourceCredentials
import Amazonka.QuickSight.Types.DataSourceParameters
import Amazonka.QuickSight.Types.SslProperties
import Amazonka.QuickSight.Types.VpcConnectionProperties

-- | The override parameters for a single data source that is being imported.
--
-- /See:/ 'newAssetBundleImportJobDataSourceOverrideParameters' smart constructor.
data AssetBundleImportJobDataSourceOverrideParameters = AssetBundleImportJobDataSourceOverrideParameters'
  { -- | An optional structure that provides the credentials to be used to create
    -- the imported data source.
    credentials :: Prelude.Maybe AssetBundleImportJobDataSourceCredentials,
    dataSourceParameters :: Prelude.Maybe DataSourceParameters,
    -- | A new name for the data source.
    name :: Prelude.Maybe Prelude.Text,
    sslProperties :: Prelude.Maybe SslProperties,
    vpcConnectionProperties :: Prelude.Maybe VpcConnectionProperties,
    -- | The ID of the data source to apply overrides to.
    dataSourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssetBundleImportJobDataSourceOverrideParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'credentials', 'assetBundleImportJobDataSourceOverrideParameters_credentials' - An optional structure that provides the credentials to be used to create
-- the imported data source.
--
-- 'dataSourceParameters', 'assetBundleImportJobDataSourceOverrideParameters_dataSourceParameters' - Undocumented member.
--
-- 'name', 'assetBundleImportJobDataSourceOverrideParameters_name' - A new name for the data source.
--
-- 'sslProperties', 'assetBundleImportJobDataSourceOverrideParameters_sslProperties' - Undocumented member.
--
-- 'vpcConnectionProperties', 'assetBundleImportJobDataSourceOverrideParameters_vpcConnectionProperties' - Undocumented member.
--
-- 'dataSourceId', 'assetBundleImportJobDataSourceOverrideParameters_dataSourceId' - The ID of the data source to apply overrides to.
newAssetBundleImportJobDataSourceOverrideParameters ::
  -- | 'dataSourceId'
  Prelude.Text ->
  AssetBundleImportJobDataSourceOverrideParameters
newAssetBundleImportJobDataSourceOverrideParameters
  pDataSourceId_ =
    AssetBundleImportJobDataSourceOverrideParameters'
      { credentials =
          Prelude.Nothing,
        dataSourceParameters =
          Prelude.Nothing,
        name = Prelude.Nothing,
        sslProperties =
          Prelude.Nothing,
        vpcConnectionProperties =
          Prelude.Nothing,
        dataSourceId =
          pDataSourceId_
      }

-- | An optional structure that provides the credentials to be used to create
-- the imported data source.
assetBundleImportJobDataSourceOverrideParameters_credentials :: Lens.Lens' AssetBundleImportJobDataSourceOverrideParameters (Prelude.Maybe AssetBundleImportJobDataSourceCredentials)
assetBundleImportJobDataSourceOverrideParameters_credentials = Lens.lens (\AssetBundleImportJobDataSourceOverrideParameters' {credentials} -> credentials) (\s@AssetBundleImportJobDataSourceOverrideParameters' {} a -> s {credentials = a} :: AssetBundleImportJobDataSourceOverrideParameters)

-- | Undocumented member.
assetBundleImportJobDataSourceOverrideParameters_dataSourceParameters :: Lens.Lens' AssetBundleImportJobDataSourceOverrideParameters (Prelude.Maybe DataSourceParameters)
assetBundleImportJobDataSourceOverrideParameters_dataSourceParameters = Lens.lens (\AssetBundleImportJobDataSourceOverrideParameters' {dataSourceParameters} -> dataSourceParameters) (\s@AssetBundleImportJobDataSourceOverrideParameters' {} a -> s {dataSourceParameters = a} :: AssetBundleImportJobDataSourceOverrideParameters)

-- | A new name for the data source.
assetBundleImportJobDataSourceOverrideParameters_name :: Lens.Lens' AssetBundleImportJobDataSourceOverrideParameters (Prelude.Maybe Prelude.Text)
assetBundleImportJobDataSourceOverrideParameters_name = Lens.lens (\AssetBundleImportJobDataSourceOverrideParameters' {name} -> name) (\s@AssetBundleImportJobDataSourceOverrideParameters' {} a -> s {name = a} :: AssetBundleImportJobDataSourceOverrideParameters)

-- | Undocumented member.
assetBundleImportJobDataSourceOverrideParameters_sslProperties :: Lens.Lens' AssetBundleImportJobDataSourceOverrideParameters (Prelude.Maybe SslProperties)
assetBundleImportJobDataSourceOverrideParameters_sslProperties = Lens.lens (\AssetBundleImportJobDataSourceOverrideParameters' {sslProperties} -> sslProperties) (\s@AssetBundleImportJobDataSourceOverrideParameters' {} a -> s {sslProperties = a} :: AssetBundleImportJobDataSourceOverrideParameters)

-- | Undocumented member.
assetBundleImportJobDataSourceOverrideParameters_vpcConnectionProperties :: Lens.Lens' AssetBundleImportJobDataSourceOverrideParameters (Prelude.Maybe VpcConnectionProperties)
assetBundleImportJobDataSourceOverrideParameters_vpcConnectionProperties = Lens.lens (\AssetBundleImportJobDataSourceOverrideParameters' {vpcConnectionProperties} -> vpcConnectionProperties) (\s@AssetBundleImportJobDataSourceOverrideParameters' {} a -> s {vpcConnectionProperties = a} :: AssetBundleImportJobDataSourceOverrideParameters)

-- | The ID of the data source to apply overrides to.
assetBundleImportJobDataSourceOverrideParameters_dataSourceId :: Lens.Lens' AssetBundleImportJobDataSourceOverrideParameters Prelude.Text
assetBundleImportJobDataSourceOverrideParameters_dataSourceId = Lens.lens (\AssetBundleImportJobDataSourceOverrideParameters' {dataSourceId} -> dataSourceId) (\s@AssetBundleImportJobDataSourceOverrideParameters' {} a -> s {dataSourceId = a} :: AssetBundleImportJobDataSourceOverrideParameters)

instance
  Data.FromJSON
    AssetBundleImportJobDataSourceOverrideParameters
  where
  parseJSON =
    Data.withObject
      "AssetBundleImportJobDataSourceOverrideParameters"
      ( \x ->
          AssetBundleImportJobDataSourceOverrideParameters'
            Prelude.<$> (x Data..:? "Credentials")
            Prelude.<*> (x Data..:? "DataSourceParameters")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "SslProperties")
            Prelude.<*> (x Data..:? "VpcConnectionProperties")
            Prelude.<*> (x Data..: "DataSourceId")
      )

instance
  Prelude.Hashable
    AssetBundleImportJobDataSourceOverrideParameters
  where
  hashWithSalt
    _salt
    AssetBundleImportJobDataSourceOverrideParameters' {..} =
      _salt
        `Prelude.hashWithSalt` credentials
        `Prelude.hashWithSalt` dataSourceParameters
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` sslProperties
        `Prelude.hashWithSalt` vpcConnectionProperties
        `Prelude.hashWithSalt` dataSourceId

instance
  Prelude.NFData
    AssetBundleImportJobDataSourceOverrideParameters
  where
  rnf
    AssetBundleImportJobDataSourceOverrideParameters' {..} =
      Prelude.rnf credentials
        `Prelude.seq` Prelude.rnf dataSourceParameters
        `Prelude.seq` Prelude.rnf name
        `Prelude.seq` Prelude.rnf sslProperties
        `Prelude.seq` Prelude.rnf vpcConnectionProperties
        `Prelude.seq` Prelude.rnf dataSourceId

instance
  Data.ToJSON
    AssetBundleImportJobDataSourceOverrideParameters
  where
  toJSON
    AssetBundleImportJobDataSourceOverrideParameters' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Credentials" Data..=) Prelude.<$> credentials,
              ("DataSourceParameters" Data..=)
                Prelude.<$> dataSourceParameters,
              ("Name" Data..=) Prelude.<$> name,
              ("SslProperties" Data..=) Prelude.<$> sslProperties,
              ("VpcConnectionProperties" Data..=)
                Prelude.<$> vpcConnectionProperties,
              Prelude.Just ("DataSourceId" Data..= dataSourceId)
            ]
        )
