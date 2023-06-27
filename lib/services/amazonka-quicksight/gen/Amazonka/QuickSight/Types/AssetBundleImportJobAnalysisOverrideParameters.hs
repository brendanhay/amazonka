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
-- Module      : Amazonka.QuickSight.Types.AssetBundleImportJobAnalysisOverrideParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AssetBundleImportJobAnalysisOverrideParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The override parameters for a single analysis that is being imported.
--
-- /See:/ 'newAssetBundleImportJobAnalysisOverrideParameters' smart constructor.
data AssetBundleImportJobAnalysisOverrideParameters = AssetBundleImportJobAnalysisOverrideParameters'
  { -- | A new name for the analysis.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID of the analysis that you ant to apply overrides to.
    analysisId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssetBundleImportJobAnalysisOverrideParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'assetBundleImportJobAnalysisOverrideParameters_name' - A new name for the analysis.
--
-- 'analysisId', 'assetBundleImportJobAnalysisOverrideParameters_analysisId' - The ID of the analysis that you ant to apply overrides to.
newAssetBundleImportJobAnalysisOverrideParameters ::
  -- | 'analysisId'
  Prelude.Text ->
  AssetBundleImportJobAnalysisOverrideParameters
newAssetBundleImportJobAnalysisOverrideParameters
  pAnalysisId_ =
    AssetBundleImportJobAnalysisOverrideParameters'
      { name =
          Prelude.Nothing,
        analysisId = pAnalysisId_
      }

-- | A new name for the analysis.
assetBundleImportJobAnalysisOverrideParameters_name :: Lens.Lens' AssetBundleImportJobAnalysisOverrideParameters (Prelude.Maybe Prelude.Text)
assetBundleImportJobAnalysisOverrideParameters_name = Lens.lens (\AssetBundleImportJobAnalysisOverrideParameters' {name} -> name) (\s@AssetBundleImportJobAnalysisOverrideParameters' {} a -> s {name = a} :: AssetBundleImportJobAnalysisOverrideParameters)

-- | The ID of the analysis that you ant to apply overrides to.
assetBundleImportJobAnalysisOverrideParameters_analysisId :: Lens.Lens' AssetBundleImportJobAnalysisOverrideParameters Prelude.Text
assetBundleImportJobAnalysisOverrideParameters_analysisId = Lens.lens (\AssetBundleImportJobAnalysisOverrideParameters' {analysisId} -> analysisId) (\s@AssetBundleImportJobAnalysisOverrideParameters' {} a -> s {analysisId = a} :: AssetBundleImportJobAnalysisOverrideParameters)

instance
  Data.FromJSON
    AssetBundleImportJobAnalysisOverrideParameters
  where
  parseJSON =
    Data.withObject
      "AssetBundleImportJobAnalysisOverrideParameters"
      ( \x ->
          AssetBundleImportJobAnalysisOverrideParameters'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..: "AnalysisId")
      )

instance
  Prelude.Hashable
    AssetBundleImportJobAnalysisOverrideParameters
  where
  hashWithSalt
    _salt
    AssetBundleImportJobAnalysisOverrideParameters' {..} =
      _salt
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` analysisId

instance
  Prelude.NFData
    AssetBundleImportJobAnalysisOverrideParameters
  where
  rnf
    AssetBundleImportJobAnalysisOverrideParameters' {..} =
      Prelude.rnf name
        `Prelude.seq` Prelude.rnf analysisId

instance
  Data.ToJSON
    AssetBundleImportJobAnalysisOverrideParameters
  where
  toJSON
    AssetBundleImportJobAnalysisOverrideParameters' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Name" Data..=) Prelude.<$> name,
              Prelude.Just ("AnalysisId" Data..= analysisId)
            ]
        )
