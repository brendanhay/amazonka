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
-- Module      : Amazonka.QuickSight.Types.AssetBundleExportJobAnalysisOverrideProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AssetBundleExportJobAnalysisOverrideProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.AssetBundleExportJobAnalysisPropertyToOverride

-- | Controls how a specific @Analysis@ resource is parameterized in the
-- returned CloudFormation template.
--
-- /See:/ 'newAssetBundleExportJobAnalysisOverrideProperties' smart constructor.
data AssetBundleExportJobAnalysisOverrideProperties = AssetBundleExportJobAnalysisOverrideProperties'
  { -- | The ARN of the specific @Analysis@ resource whose override properties
    -- are configured in this structure.
    arn :: Prelude.Maybe Prelude.Text,
    -- | A list of @Analysis@ resource properties to generate variables for in
    -- the returned CloudFormation template.
    properties :: Prelude.NonEmpty AssetBundleExportJobAnalysisPropertyToOverride
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssetBundleExportJobAnalysisOverrideProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'assetBundleExportJobAnalysisOverrideProperties_arn' - The ARN of the specific @Analysis@ resource whose override properties
-- are configured in this structure.
--
-- 'properties', 'assetBundleExportJobAnalysisOverrideProperties_properties' - A list of @Analysis@ resource properties to generate variables for in
-- the returned CloudFormation template.
newAssetBundleExportJobAnalysisOverrideProperties ::
  -- | 'properties'
  Prelude.NonEmpty AssetBundleExportJobAnalysisPropertyToOverride ->
  AssetBundleExportJobAnalysisOverrideProperties
newAssetBundleExportJobAnalysisOverrideProperties
  pProperties_ =
    AssetBundleExportJobAnalysisOverrideProperties'
      { arn =
          Prelude.Nothing,
        properties =
          Lens.coerced
            Lens.# pProperties_
      }

-- | The ARN of the specific @Analysis@ resource whose override properties
-- are configured in this structure.
assetBundleExportJobAnalysisOverrideProperties_arn :: Lens.Lens' AssetBundleExportJobAnalysisOverrideProperties (Prelude.Maybe Prelude.Text)
assetBundleExportJobAnalysisOverrideProperties_arn = Lens.lens (\AssetBundleExportJobAnalysisOverrideProperties' {arn} -> arn) (\s@AssetBundleExportJobAnalysisOverrideProperties' {} a -> s {arn = a} :: AssetBundleExportJobAnalysisOverrideProperties)

-- | A list of @Analysis@ resource properties to generate variables for in
-- the returned CloudFormation template.
assetBundleExportJobAnalysisOverrideProperties_properties :: Lens.Lens' AssetBundleExportJobAnalysisOverrideProperties (Prelude.NonEmpty AssetBundleExportJobAnalysisPropertyToOverride)
assetBundleExportJobAnalysisOverrideProperties_properties = Lens.lens (\AssetBundleExportJobAnalysisOverrideProperties' {properties} -> properties) (\s@AssetBundleExportJobAnalysisOverrideProperties' {} a -> s {properties = a} :: AssetBundleExportJobAnalysisOverrideProperties) Prelude.. Lens.coerced

instance
  Data.FromJSON
    AssetBundleExportJobAnalysisOverrideProperties
  where
  parseJSON =
    Data.withObject
      "AssetBundleExportJobAnalysisOverrideProperties"
      ( \x ->
          AssetBundleExportJobAnalysisOverrideProperties'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..: "Properties")
      )

instance
  Prelude.Hashable
    AssetBundleExportJobAnalysisOverrideProperties
  where
  hashWithSalt
    _salt
    AssetBundleExportJobAnalysisOverrideProperties' {..} =
      _salt
        `Prelude.hashWithSalt` arn
        `Prelude.hashWithSalt` properties

instance
  Prelude.NFData
    AssetBundleExportJobAnalysisOverrideProperties
  where
  rnf
    AssetBundleExportJobAnalysisOverrideProperties' {..} =
      Prelude.rnf arn
        `Prelude.seq` Prelude.rnf properties

instance
  Data.ToJSON
    AssetBundleExportJobAnalysisOverrideProperties
  where
  toJSON
    AssetBundleExportJobAnalysisOverrideProperties' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Arn" Data..=) Prelude.<$> arn,
              Prelude.Just ("Properties" Data..= properties)
            ]
        )
