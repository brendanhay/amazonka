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
-- Module      : Amazonka.QuickSight.Types.AssetBundleExportJobThemeOverrideProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AssetBundleExportJobThemeOverrideProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.AssetBundleExportJobThemePropertyToOverride

-- | Controls how a specific @Theme@ resource is parameterized in the
-- returned CloudFormation template.
--
-- /See:/ 'newAssetBundleExportJobThemeOverrideProperties' smart constructor.
data AssetBundleExportJobThemeOverrideProperties = AssetBundleExportJobThemeOverrideProperties'
  { -- | The ARN of the specific @Theme@ resource whose override properties are
    -- configured in this structure.
    arn :: Prelude.Maybe Prelude.Text,
    -- | A list of @Theme@ resource properties to generate variables for in the
    -- returned CloudFormation template.
    properties :: Prelude.NonEmpty AssetBundleExportJobThemePropertyToOverride
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssetBundleExportJobThemeOverrideProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'assetBundleExportJobThemeOverrideProperties_arn' - The ARN of the specific @Theme@ resource whose override properties are
-- configured in this structure.
--
-- 'properties', 'assetBundleExportJobThemeOverrideProperties_properties' - A list of @Theme@ resource properties to generate variables for in the
-- returned CloudFormation template.
newAssetBundleExportJobThemeOverrideProperties ::
  -- | 'properties'
  Prelude.NonEmpty AssetBundleExportJobThemePropertyToOverride ->
  AssetBundleExportJobThemeOverrideProperties
newAssetBundleExportJobThemeOverrideProperties
  pProperties_ =
    AssetBundleExportJobThemeOverrideProperties'
      { arn =
          Prelude.Nothing,
        properties =
          Lens.coerced
            Lens.# pProperties_
      }

-- | The ARN of the specific @Theme@ resource whose override properties are
-- configured in this structure.
assetBundleExportJobThemeOverrideProperties_arn :: Lens.Lens' AssetBundleExportJobThemeOverrideProperties (Prelude.Maybe Prelude.Text)
assetBundleExportJobThemeOverrideProperties_arn = Lens.lens (\AssetBundleExportJobThemeOverrideProperties' {arn} -> arn) (\s@AssetBundleExportJobThemeOverrideProperties' {} a -> s {arn = a} :: AssetBundleExportJobThemeOverrideProperties)

-- | A list of @Theme@ resource properties to generate variables for in the
-- returned CloudFormation template.
assetBundleExportJobThemeOverrideProperties_properties :: Lens.Lens' AssetBundleExportJobThemeOverrideProperties (Prelude.NonEmpty AssetBundleExportJobThemePropertyToOverride)
assetBundleExportJobThemeOverrideProperties_properties = Lens.lens (\AssetBundleExportJobThemeOverrideProperties' {properties} -> properties) (\s@AssetBundleExportJobThemeOverrideProperties' {} a -> s {properties = a} :: AssetBundleExportJobThemeOverrideProperties) Prelude.. Lens.coerced

instance
  Data.FromJSON
    AssetBundleExportJobThemeOverrideProperties
  where
  parseJSON =
    Data.withObject
      "AssetBundleExportJobThemeOverrideProperties"
      ( \x ->
          AssetBundleExportJobThemeOverrideProperties'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..: "Properties")
      )

instance
  Prelude.Hashable
    AssetBundleExportJobThemeOverrideProperties
  where
  hashWithSalt
    _salt
    AssetBundleExportJobThemeOverrideProperties' {..} =
      _salt
        `Prelude.hashWithSalt` arn
        `Prelude.hashWithSalt` properties

instance
  Prelude.NFData
    AssetBundleExportJobThemeOverrideProperties
  where
  rnf AssetBundleExportJobThemeOverrideProperties' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf properties

instance
  Data.ToJSON
    AssetBundleExportJobThemeOverrideProperties
  where
  toJSON
    AssetBundleExportJobThemeOverrideProperties' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Arn" Data..=) Prelude.<$> arn,
              Prelude.Just ("Properties" Data..= properties)
            ]
        )
