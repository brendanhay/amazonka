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
-- Module      : Amazonka.QuickSight.Types.AssetBundleExportJobResourceIdOverrideConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AssetBundleExportJobResourceIdOverrideConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An optional structure that configures resource ID overrides for the
-- export job.
--
-- /See:/ 'newAssetBundleExportJobResourceIdOverrideConfiguration' smart constructor.
data AssetBundleExportJobResourceIdOverrideConfiguration = AssetBundleExportJobResourceIdOverrideConfiguration'
  { -- | An option to request a CloudFormation variable for a prefix to be
    -- prepended to each resource\'s ID before import. The prefix is only added
    -- to the asset IDs and does not change the name of the asset.
    prefixForAllResources :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssetBundleExportJobResourceIdOverrideConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'prefixForAllResources', 'assetBundleExportJobResourceIdOverrideConfiguration_prefixForAllResources' - An option to request a CloudFormation variable for a prefix to be
-- prepended to each resource\'s ID before import. The prefix is only added
-- to the asset IDs and does not change the name of the asset.
newAssetBundleExportJobResourceIdOverrideConfiguration ::
  AssetBundleExportJobResourceIdOverrideConfiguration
newAssetBundleExportJobResourceIdOverrideConfiguration =
  AssetBundleExportJobResourceIdOverrideConfiguration'
    { prefixForAllResources =
        Prelude.Nothing
    }

-- | An option to request a CloudFormation variable for a prefix to be
-- prepended to each resource\'s ID before import. The prefix is only added
-- to the asset IDs and does not change the name of the asset.
assetBundleExportJobResourceIdOverrideConfiguration_prefixForAllResources :: Lens.Lens' AssetBundleExportJobResourceIdOverrideConfiguration (Prelude.Maybe Prelude.Bool)
assetBundleExportJobResourceIdOverrideConfiguration_prefixForAllResources = Lens.lens (\AssetBundleExportJobResourceIdOverrideConfiguration' {prefixForAllResources} -> prefixForAllResources) (\s@AssetBundleExportJobResourceIdOverrideConfiguration' {} a -> s {prefixForAllResources = a} :: AssetBundleExportJobResourceIdOverrideConfiguration)

instance
  Data.FromJSON
    AssetBundleExportJobResourceIdOverrideConfiguration
  where
  parseJSON =
    Data.withObject
      "AssetBundleExportJobResourceIdOverrideConfiguration"
      ( \x ->
          AssetBundleExportJobResourceIdOverrideConfiguration'
            Prelude.<$> (x Data..:? "PrefixForAllResources")
      )

instance
  Prelude.Hashable
    AssetBundleExportJobResourceIdOverrideConfiguration
  where
  hashWithSalt
    _salt
    AssetBundleExportJobResourceIdOverrideConfiguration' {..} =
      _salt `Prelude.hashWithSalt` prefixForAllResources

instance
  Prelude.NFData
    AssetBundleExportJobResourceIdOverrideConfiguration
  where
  rnf
    AssetBundleExportJobResourceIdOverrideConfiguration' {..} =
      Prelude.rnf prefixForAllResources

instance
  Data.ToJSON
    AssetBundleExportJobResourceIdOverrideConfiguration
  where
  toJSON
    AssetBundleExportJobResourceIdOverrideConfiguration' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("PrefixForAllResources" Data..=)
                Prelude.<$> prefixForAllResources
            ]
        )
