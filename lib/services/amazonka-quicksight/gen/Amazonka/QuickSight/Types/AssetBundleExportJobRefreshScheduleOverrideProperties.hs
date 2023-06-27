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
-- Module      : Amazonka.QuickSight.Types.AssetBundleExportJobRefreshScheduleOverrideProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AssetBundleExportJobRefreshScheduleOverrideProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.AssetBundleExportJobRefreshSchedulePropertyToOverride

-- | Controls how a specific @RefreshSchedule@ resource is parameterized in
-- the returned CloudFormation template.
--
-- /See:/ 'newAssetBundleExportJobRefreshScheduleOverrideProperties' smart constructor.
data AssetBundleExportJobRefreshScheduleOverrideProperties = AssetBundleExportJobRefreshScheduleOverrideProperties'
  { -- | The ARN of the specific @RefreshSchedule@ resource whose override
    -- properties are configured in this structure.
    arn :: Prelude.Maybe Prelude.Text,
    -- | A list of @RefreshSchedule@ resource properties to generate variables
    -- for in the returned CloudFormation template.
    properties :: Prelude.NonEmpty AssetBundleExportJobRefreshSchedulePropertyToOverride
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssetBundleExportJobRefreshScheduleOverrideProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'assetBundleExportJobRefreshScheduleOverrideProperties_arn' - The ARN of the specific @RefreshSchedule@ resource whose override
-- properties are configured in this structure.
--
-- 'properties', 'assetBundleExportJobRefreshScheduleOverrideProperties_properties' - A list of @RefreshSchedule@ resource properties to generate variables
-- for in the returned CloudFormation template.
newAssetBundleExportJobRefreshScheduleOverrideProperties ::
  -- | 'properties'
  Prelude.NonEmpty AssetBundleExportJobRefreshSchedulePropertyToOverride ->
  AssetBundleExportJobRefreshScheduleOverrideProperties
newAssetBundleExportJobRefreshScheduleOverrideProperties
  pProperties_ =
    AssetBundleExportJobRefreshScheduleOverrideProperties'
      { arn =
          Prelude.Nothing,
        properties =
          Lens.coerced
            Lens.# pProperties_
      }

-- | The ARN of the specific @RefreshSchedule@ resource whose override
-- properties are configured in this structure.
assetBundleExportJobRefreshScheduleOverrideProperties_arn :: Lens.Lens' AssetBundleExportJobRefreshScheduleOverrideProperties (Prelude.Maybe Prelude.Text)
assetBundleExportJobRefreshScheduleOverrideProperties_arn = Lens.lens (\AssetBundleExportJobRefreshScheduleOverrideProperties' {arn} -> arn) (\s@AssetBundleExportJobRefreshScheduleOverrideProperties' {} a -> s {arn = a} :: AssetBundleExportJobRefreshScheduleOverrideProperties)

-- | A list of @RefreshSchedule@ resource properties to generate variables
-- for in the returned CloudFormation template.
assetBundleExportJobRefreshScheduleOverrideProperties_properties :: Lens.Lens' AssetBundleExportJobRefreshScheduleOverrideProperties (Prelude.NonEmpty AssetBundleExportJobRefreshSchedulePropertyToOverride)
assetBundleExportJobRefreshScheduleOverrideProperties_properties = Lens.lens (\AssetBundleExportJobRefreshScheduleOverrideProperties' {properties} -> properties) (\s@AssetBundleExportJobRefreshScheduleOverrideProperties' {} a -> s {properties = a} :: AssetBundleExportJobRefreshScheduleOverrideProperties) Prelude.. Lens.coerced

instance
  Data.FromJSON
    AssetBundleExportJobRefreshScheduleOverrideProperties
  where
  parseJSON =
    Data.withObject
      "AssetBundleExportJobRefreshScheduleOverrideProperties"
      ( \x ->
          AssetBundleExportJobRefreshScheduleOverrideProperties'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..: "Properties")
      )

instance
  Prelude.Hashable
    AssetBundleExportJobRefreshScheduleOverrideProperties
  where
  hashWithSalt
    _salt
    AssetBundleExportJobRefreshScheduleOverrideProperties' {..} =
      _salt
        `Prelude.hashWithSalt` arn
        `Prelude.hashWithSalt` properties

instance
  Prelude.NFData
    AssetBundleExportJobRefreshScheduleOverrideProperties
  where
  rnf
    AssetBundleExportJobRefreshScheduleOverrideProperties' {..} =
      Prelude.rnf arn
        `Prelude.seq` Prelude.rnf properties

instance
  Data.ToJSON
    AssetBundleExportJobRefreshScheduleOverrideProperties
  where
  toJSON
    AssetBundleExportJobRefreshScheduleOverrideProperties' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Arn" Data..=) Prelude.<$> arn,
              Prelude.Just ("Properties" Data..= properties)
            ]
        )
