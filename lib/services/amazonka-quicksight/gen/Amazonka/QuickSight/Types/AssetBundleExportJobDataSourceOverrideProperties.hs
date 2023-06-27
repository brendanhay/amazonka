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
-- Module      : Amazonka.QuickSight.Types.AssetBundleExportJobDataSourceOverrideProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AssetBundleExportJobDataSourceOverrideProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.AssetBundleExportJobDataSourcePropertyToOverride

-- | Controls how a specific @DataSource@ resource is parameterized in the
-- returned CloudFormation template.
--
-- /See:/ 'newAssetBundleExportJobDataSourceOverrideProperties' smart constructor.
data AssetBundleExportJobDataSourceOverrideProperties = AssetBundleExportJobDataSourceOverrideProperties'
  { -- | The ARN of the specific @DataSource@ resource whose override properties
    -- are configured in this structure.
    arn :: Prelude.Maybe Prelude.Text,
    -- | A list of @DataSource@ resource properties to generate variables for in
    -- the returned CloudFormation template.
    properties :: Prelude.NonEmpty AssetBundleExportJobDataSourcePropertyToOverride
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssetBundleExportJobDataSourceOverrideProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'assetBundleExportJobDataSourceOverrideProperties_arn' - The ARN of the specific @DataSource@ resource whose override properties
-- are configured in this structure.
--
-- 'properties', 'assetBundleExportJobDataSourceOverrideProperties_properties' - A list of @DataSource@ resource properties to generate variables for in
-- the returned CloudFormation template.
newAssetBundleExportJobDataSourceOverrideProperties ::
  -- | 'properties'
  Prelude.NonEmpty AssetBundleExportJobDataSourcePropertyToOverride ->
  AssetBundleExportJobDataSourceOverrideProperties
newAssetBundleExportJobDataSourceOverrideProperties
  pProperties_ =
    AssetBundleExportJobDataSourceOverrideProperties'
      { arn =
          Prelude.Nothing,
        properties =
          Lens.coerced
            Lens.# pProperties_
      }

-- | The ARN of the specific @DataSource@ resource whose override properties
-- are configured in this structure.
assetBundleExportJobDataSourceOverrideProperties_arn :: Lens.Lens' AssetBundleExportJobDataSourceOverrideProperties (Prelude.Maybe Prelude.Text)
assetBundleExportJobDataSourceOverrideProperties_arn = Lens.lens (\AssetBundleExportJobDataSourceOverrideProperties' {arn} -> arn) (\s@AssetBundleExportJobDataSourceOverrideProperties' {} a -> s {arn = a} :: AssetBundleExportJobDataSourceOverrideProperties)

-- | A list of @DataSource@ resource properties to generate variables for in
-- the returned CloudFormation template.
assetBundleExportJobDataSourceOverrideProperties_properties :: Lens.Lens' AssetBundleExportJobDataSourceOverrideProperties (Prelude.NonEmpty AssetBundleExportJobDataSourcePropertyToOverride)
assetBundleExportJobDataSourceOverrideProperties_properties = Lens.lens (\AssetBundleExportJobDataSourceOverrideProperties' {properties} -> properties) (\s@AssetBundleExportJobDataSourceOverrideProperties' {} a -> s {properties = a} :: AssetBundleExportJobDataSourceOverrideProperties) Prelude.. Lens.coerced

instance
  Data.FromJSON
    AssetBundleExportJobDataSourceOverrideProperties
  where
  parseJSON =
    Data.withObject
      "AssetBundleExportJobDataSourceOverrideProperties"
      ( \x ->
          AssetBundleExportJobDataSourceOverrideProperties'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..: "Properties")
      )

instance
  Prelude.Hashable
    AssetBundleExportJobDataSourceOverrideProperties
  where
  hashWithSalt
    _salt
    AssetBundleExportJobDataSourceOverrideProperties' {..} =
      _salt
        `Prelude.hashWithSalt` arn
        `Prelude.hashWithSalt` properties

instance
  Prelude.NFData
    AssetBundleExportJobDataSourceOverrideProperties
  where
  rnf
    AssetBundleExportJobDataSourceOverrideProperties' {..} =
      Prelude.rnf arn
        `Prelude.seq` Prelude.rnf properties

instance
  Data.ToJSON
    AssetBundleExportJobDataSourceOverrideProperties
  where
  toJSON
    AssetBundleExportJobDataSourceOverrideProperties' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Arn" Data..=) Prelude.<$> arn,
              Prelude.Just ("Properties" Data..= properties)
            ]
        )
