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
-- Module      : Amazonka.QuickSight.Types.AssetBundleExportJobDashboardOverrideProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AssetBundleExportJobDashboardOverrideProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.AssetBundleExportJobDashboardPropertyToOverride

-- | Controls how a specific @Dashboard@ resource is parameterized in the
-- returned CloudFormation template.
--
-- /See:/ 'newAssetBundleExportJobDashboardOverrideProperties' smart constructor.
data AssetBundleExportJobDashboardOverrideProperties = AssetBundleExportJobDashboardOverrideProperties'
  { -- | The ARN of the specific @Dashboard@ resource whose override properties
    -- are configured in this structure.
    arn :: Prelude.Maybe Prelude.Text,
    -- | A list of @Dashboard@ resource properties to generate variables for in
    -- the returned CloudFormation template.
    properties :: Prelude.NonEmpty AssetBundleExportJobDashboardPropertyToOverride
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssetBundleExportJobDashboardOverrideProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'assetBundleExportJobDashboardOverrideProperties_arn' - The ARN of the specific @Dashboard@ resource whose override properties
-- are configured in this structure.
--
-- 'properties', 'assetBundleExportJobDashboardOverrideProperties_properties' - A list of @Dashboard@ resource properties to generate variables for in
-- the returned CloudFormation template.
newAssetBundleExportJobDashboardOverrideProperties ::
  -- | 'properties'
  Prelude.NonEmpty AssetBundleExportJobDashboardPropertyToOverride ->
  AssetBundleExportJobDashboardOverrideProperties
newAssetBundleExportJobDashboardOverrideProperties
  pProperties_ =
    AssetBundleExportJobDashboardOverrideProperties'
      { arn =
          Prelude.Nothing,
        properties =
          Lens.coerced
            Lens.# pProperties_
      }

-- | The ARN of the specific @Dashboard@ resource whose override properties
-- are configured in this structure.
assetBundleExportJobDashboardOverrideProperties_arn :: Lens.Lens' AssetBundleExportJobDashboardOverrideProperties (Prelude.Maybe Prelude.Text)
assetBundleExportJobDashboardOverrideProperties_arn = Lens.lens (\AssetBundleExportJobDashboardOverrideProperties' {arn} -> arn) (\s@AssetBundleExportJobDashboardOverrideProperties' {} a -> s {arn = a} :: AssetBundleExportJobDashboardOverrideProperties)

-- | A list of @Dashboard@ resource properties to generate variables for in
-- the returned CloudFormation template.
assetBundleExportJobDashboardOverrideProperties_properties :: Lens.Lens' AssetBundleExportJobDashboardOverrideProperties (Prelude.NonEmpty AssetBundleExportJobDashboardPropertyToOverride)
assetBundleExportJobDashboardOverrideProperties_properties = Lens.lens (\AssetBundleExportJobDashboardOverrideProperties' {properties} -> properties) (\s@AssetBundleExportJobDashboardOverrideProperties' {} a -> s {properties = a} :: AssetBundleExportJobDashboardOverrideProperties) Prelude.. Lens.coerced

instance
  Data.FromJSON
    AssetBundleExportJobDashboardOverrideProperties
  where
  parseJSON =
    Data.withObject
      "AssetBundleExportJobDashboardOverrideProperties"
      ( \x ->
          AssetBundleExportJobDashboardOverrideProperties'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..: "Properties")
      )

instance
  Prelude.Hashable
    AssetBundleExportJobDashboardOverrideProperties
  where
  hashWithSalt
    _salt
    AssetBundleExportJobDashboardOverrideProperties' {..} =
      _salt
        `Prelude.hashWithSalt` arn
        `Prelude.hashWithSalt` properties

instance
  Prelude.NFData
    AssetBundleExportJobDashboardOverrideProperties
  where
  rnf
    AssetBundleExportJobDashboardOverrideProperties' {..} =
      Prelude.rnf arn
        `Prelude.seq` Prelude.rnf properties

instance
  Data.ToJSON
    AssetBundleExportJobDashboardOverrideProperties
  where
  toJSON
    AssetBundleExportJobDashboardOverrideProperties' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Arn" Data..=) Prelude.<$> arn,
              Prelude.Just ("Properties" Data..= properties)
            ]
        )
