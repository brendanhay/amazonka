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
-- Module      : Amazonka.QuickSight.Types.AssetBundleExportJobDataSetOverrideProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AssetBundleExportJobDataSetOverrideProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.AssetBundleExportJobDataSetPropertyToOverride

-- | Controls how a specific @DataSet@ resource is parameterized in the
-- returned CloudFormation template.
--
-- /See:/ 'newAssetBundleExportJobDataSetOverrideProperties' smart constructor.
data AssetBundleExportJobDataSetOverrideProperties = AssetBundleExportJobDataSetOverrideProperties'
  { -- | The ARN of the specific @DataSet@ resource whose override properties are
    -- configured in this structure.
    arn :: Prelude.Maybe Prelude.Text,
    -- | A list of @DataSet@ resource properties to generate variables for in the
    -- returned CloudFormation template.
    properties :: Prelude.NonEmpty AssetBundleExportJobDataSetPropertyToOverride
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssetBundleExportJobDataSetOverrideProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'assetBundleExportJobDataSetOverrideProperties_arn' - The ARN of the specific @DataSet@ resource whose override properties are
-- configured in this structure.
--
-- 'properties', 'assetBundleExportJobDataSetOverrideProperties_properties' - A list of @DataSet@ resource properties to generate variables for in the
-- returned CloudFormation template.
newAssetBundleExportJobDataSetOverrideProperties ::
  -- | 'properties'
  Prelude.NonEmpty AssetBundleExportJobDataSetPropertyToOverride ->
  AssetBundleExportJobDataSetOverrideProperties
newAssetBundleExportJobDataSetOverrideProperties
  pProperties_ =
    AssetBundleExportJobDataSetOverrideProperties'
      { arn =
          Prelude.Nothing,
        properties =
          Lens.coerced
            Lens.# pProperties_
      }

-- | The ARN of the specific @DataSet@ resource whose override properties are
-- configured in this structure.
assetBundleExportJobDataSetOverrideProperties_arn :: Lens.Lens' AssetBundleExportJobDataSetOverrideProperties (Prelude.Maybe Prelude.Text)
assetBundleExportJobDataSetOverrideProperties_arn = Lens.lens (\AssetBundleExportJobDataSetOverrideProperties' {arn} -> arn) (\s@AssetBundleExportJobDataSetOverrideProperties' {} a -> s {arn = a} :: AssetBundleExportJobDataSetOverrideProperties)

-- | A list of @DataSet@ resource properties to generate variables for in the
-- returned CloudFormation template.
assetBundleExportJobDataSetOverrideProperties_properties :: Lens.Lens' AssetBundleExportJobDataSetOverrideProperties (Prelude.NonEmpty AssetBundleExportJobDataSetPropertyToOverride)
assetBundleExportJobDataSetOverrideProperties_properties = Lens.lens (\AssetBundleExportJobDataSetOverrideProperties' {properties} -> properties) (\s@AssetBundleExportJobDataSetOverrideProperties' {} a -> s {properties = a} :: AssetBundleExportJobDataSetOverrideProperties) Prelude.. Lens.coerced

instance
  Data.FromJSON
    AssetBundleExportJobDataSetOverrideProperties
  where
  parseJSON =
    Data.withObject
      "AssetBundleExportJobDataSetOverrideProperties"
      ( \x ->
          AssetBundleExportJobDataSetOverrideProperties'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..: "Properties")
      )

instance
  Prelude.Hashable
    AssetBundleExportJobDataSetOverrideProperties
  where
  hashWithSalt
    _salt
    AssetBundleExportJobDataSetOverrideProperties' {..} =
      _salt
        `Prelude.hashWithSalt` arn
        `Prelude.hashWithSalt` properties

instance
  Prelude.NFData
    AssetBundleExportJobDataSetOverrideProperties
  where
  rnf
    AssetBundleExportJobDataSetOverrideProperties' {..} =
      Prelude.rnf arn
        `Prelude.seq` Prelude.rnf properties

instance
  Data.ToJSON
    AssetBundleExportJobDataSetOverrideProperties
  where
  toJSON
    AssetBundleExportJobDataSetOverrideProperties' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Arn" Data..=) Prelude.<$> arn,
              Prelude.Just ("Properties" Data..= properties)
            ]
        )
