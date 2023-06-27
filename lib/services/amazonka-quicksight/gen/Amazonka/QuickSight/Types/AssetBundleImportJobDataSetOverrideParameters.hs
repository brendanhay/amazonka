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
-- Module      : Amazonka.QuickSight.Types.AssetBundleImportJobDataSetOverrideParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AssetBundleImportJobDataSetOverrideParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The override parameters for a single dataset that is being imported.
--
-- /See:/ 'newAssetBundleImportJobDataSetOverrideParameters' smart constructor.
data AssetBundleImportJobDataSetOverrideParameters = AssetBundleImportJobDataSetOverrideParameters'
  { -- | A new name for the dataset.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID of the dataset to apply overrides to.
    dataSetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssetBundleImportJobDataSetOverrideParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'assetBundleImportJobDataSetOverrideParameters_name' - A new name for the dataset.
--
-- 'dataSetId', 'assetBundleImportJobDataSetOverrideParameters_dataSetId' - The ID of the dataset to apply overrides to.
newAssetBundleImportJobDataSetOverrideParameters ::
  -- | 'dataSetId'
  Prelude.Text ->
  AssetBundleImportJobDataSetOverrideParameters
newAssetBundleImportJobDataSetOverrideParameters
  pDataSetId_ =
    AssetBundleImportJobDataSetOverrideParameters'
      { name =
          Prelude.Nothing,
        dataSetId = pDataSetId_
      }

-- | A new name for the dataset.
assetBundleImportJobDataSetOverrideParameters_name :: Lens.Lens' AssetBundleImportJobDataSetOverrideParameters (Prelude.Maybe Prelude.Text)
assetBundleImportJobDataSetOverrideParameters_name = Lens.lens (\AssetBundleImportJobDataSetOverrideParameters' {name} -> name) (\s@AssetBundleImportJobDataSetOverrideParameters' {} a -> s {name = a} :: AssetBundleImportJobDataSetOverrideParameters)

-- | The ID of the dataset to apply overrides to.
assetBundleImportJobDataSetOverrideParameters_dataSetId :: Lens.Lens' AssetBundleImportJobDataSetOverrideParameters Prelude.Text
assetBundleImportJobDataSetOverrideParameters_dataSetId = Lens.lens (\AssetBundleImportJobDataSetOverrideParameters' {dataSetId} -> dataSetId) (\s@AssetBundleImportJobDataSetOverrideParameters' {} a -> s {dataSetId = a} :: AssetBundleImportJobDataSetOverrideParameters)

instance
  Data.FromJSON
    AssetBundleImportJobDataSetOverrideParameters
  where
  parseJSON =
    Data.withObject
      "AssetBundleImportJobDataSetOverrideParameters"
      ( \x ->
          AssetBundleImportJobDataSetOverrideParameters'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..: "DataSetId")
      )

instance
  Prelude.Hashable
    AssetBundleImportJobDataSetOverrideParameters
  where
  hashWithSalt
    _salt
    AssetBundleImportJobDataSetOverrideParameters' {..} =
      _salt
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` dataSetId

instance
  Prelude.NFData
    AssetBundleImportJobDataSetOverrideParameters
  where
  rnf
    AssetBundleImportJobDataSetOverrideParameters' {..} =
      Prelude.rnf name
        `Prelude.seq` Prelude.rnf dataSetId

instance
  Data.ToJSON
    AssetBundleImportJobDataSetOverrideParameters
  where
  toJSON
    AssetBundleImportJobDataSetOverrideParameters' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Name" Data..=) Prelude.<$> name,
              Prelude.Just ("DataSetId" Data..= dataSetId)
            ]
        )
