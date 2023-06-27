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
-- Module      : Amazonka.QuickSight.Types.AssetBundleImportJobResourceIdOverrideConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AssetBundleImportJobResourceIdOverrideConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An optional structure that configures resource ID overrides for the
-- import job.
--
-- /See:/ 'newAssetBundleImportJobResourceIdOverrideConfiguration' smart constructor.
data AssetBundleImportJobResourceIdOverrideConfiguration = AssetBundleImportJobResourceIdOverrideConfiguration'
  { -- | An option to request a CloudFormation variable for a prefix to be
    -- prepended to each resource\'s ID before import. The prefix is only added
    -- to the asset IDs and does not change the name of the asset.
    prefixForAllResources :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssetBundleImportJobResourceIdOverrideConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'prefixForAllResources', 'assetBundleImportJobResourceIdOverrideConfiguration_prefixForAllResources' - An option to request a CloudFormation variable for a prefix to be
-- prepended to each resource\'s ID before import. The prefix is only added
-- to the asset IDs and does not change the name of the asset.
newAssetBundleImportJobResourceIdOverrideConfiguration ::
  AssetBundleImportJobResourceIdOverrideConfiguration
newAssetBundleImportJobResourceIdOverrideConfiguration =
  AssetBundleImportJobResourceIdOverrideConfiguration'
    { prefixForAllResources =
        Prelude.Nothing
    }

-- | An option to request a CloudFormation variable for a prefix to be
-- prepended to each resource\'s ID before import. The prefix is only added
-- to the asset IDs and does not change the name of the asset.
assetBundleImportJobResourceIdOverrideConfiguration_prefixForAllResources :: Lens.Lens' AssetBundleImportJobResourceIdOverrideConfiguration (Prelude.Maybe Prelude.Text)
assetBundleImportJobResourceIdOverrideConfiguration_prefixForAllResources = Lens.lens (\AssetBundleImportJobResourceIdOverrideConfiguration' {prefixForAllResources} -> prefixForAllResources) (\s@AssetBundleImportJobResourceIdOverrideConfiguration' {} a -> s {prefixForAllResources = a} :: AssetBundleImportJobResourceIdOverrideConfiguration)

instance
  Data.FromJSON
    AssetBundleImportJobResourceIdOverrideConfiguration
  where
  parseJSON =
    Data.withObject
      "AssetBundleImportJobResourceIdOverrideConfiguration"
      ( \x ->
          AssetBundleImportJobResourceIdOverrideConfiguration'
            Prelude.<$> (x Data..:? "PrefixForAllResources")
      )

instance
  Prelude.Hashable
    AssetBundleImportJobResourceIdOverrideConfiguration
  where
  hashWithSalt
    _salt
    AssetBundleImportJobResourceIdOverrideConfiguration' {..} =
      _salt `Prelude.hashWithSalt` prefixForAllResources

instance
  Prelude.NFData
    AssetBundleImportJobResourceIdOverrideConfiguration
  where
  rnf
    AssetBundleImportJobResourceIdOverrideConfiguration' {..} =
      Prelude.rnf prefixForAllResources

instance
  Data.ToJSON
    AssetBundleImportJobResourceIdOverrideConfiguration
  where
  toJSON
    AssetBundleImportJobResourceIdOverrideConfiguration' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("PrefixForAllResources" Data..=)
                Prelude.<$> prefixForAllResources
            ]
        )
