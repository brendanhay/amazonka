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
-- Module      : Amazonka.QuickSight.Types.AssetBundleExportJobVPCConnectionOverrideProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AssetBundleExportJobVPCConnectionOverrideProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.AssetBundleExportJobVPCConnectionPropertyToOverride

-- | Controls how a specific @VPCConnection@ resource is parameterized in the
-- outputted CloudFormation template.
--
-- /See:/ 'newAssetBundleExportJobVPCConnectionOverrideProperties' smart constructor.
data AssetBundleExportJobVPCConnectionOverrideProperties = AssetBundleExportJobVPCConnectionOverrideProperties'
  { -- | The ARN of the specific @VPCConnection@ resource whose override
    -- properties are configured in this structure.
    arn :: Prelude.Maybe Prelude.Text,
    -- | A list of @VPCConnection@ resource properties to generate variables for
    -- in the returned CloudFormation template.
    properties :: Prelude.NonEmpty AssetBundleExportJobVPCConnectionPropertyToOverride
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssetBundleExportJobVPCConnectionOverrideProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'assetBundleExportJobVPCConnectionOverrideProperties_arn' - The ARN of the specific @VPCConnection@ resource whose override
-- properties are configured in this structure.
--
-- 'properties', 'assetBundleExportJobVPCConnectionOverrideProperties_properties' - A list of @VPCConnection@ resource properties to generate variables for
-- in the returned CloudFormation template.
newAssetBundleExportJobVPCConnectionOverrideProperties ::
  -- | 'properties'
  Prelude.NonEmpty AssetBundleExportJobVPCConnectionPropertyToOverride ->
  AssetBundleExportJobVPCConnectionOverrideProperties
newAssetBundleExportJobVPCConnectionOverrideProperties
  pProperties_ =
    AssetBundleExportJobVPCConnectionOverrideProperties'
      { arn =
          Prelude.Nothing,
        properties =
          Lens.coerced
            Lens.# pProperties_
      }

-- | The ARN of the specific @VPCConnection@ resource whose override
-- properties are configured in this structure.
assetBundleExportJobVPCConnectionOverrideProperties_arn :: Lens.Lens' AssetBundleExportJobVPCConnectionOverrideProperties (Prelude.Maybe Prelude.Text)
assetBundleExportJobVPCConnectionOverrideProperties_arn = Lens.lens (\AssetBundleExportJobVPCConnectionOverrideProperties' {arn} -> arn) (\s@AssetBundleExportJobVPCConnectionOverrideProperties' {} a -> s {arn = a} :: AssetBundleExportJobVPCConnectionOverrideProperties)

-- | A list of @VPCConnection@ resource properties to generate variables for
-- in the returned CloudFormation template.
assetBundleExportJobVPCConnectionOverrideProperties_properties :: Lens.Lens' AssetBundleExportJobVPCConnectionOverrideProperties (Prelude.NonEmpty AssetBundleExportJobVPCConnectionPropertyToOverride)
assetBundleExportJobVPCConnectionOverrideProperties_properties = Lens.lens (\AssetBundleExportJobVPCConnectionOverrideProperties' {properties} -> properties) (\s@AssetBundleExportJobVPCConnectionOverrideProperties' {} a -> s {properties = a} :: AssetBundleExportJobVPCConnectionOverrideProperties) Prelude.. Lens.coerced

instance
  Data.FromJSON
    AssetBundleExportJobVPCConnectionOverrideProperties
  where
  parseJSON =
    Data.withObject
      "AssetBundleExportJobVPCConnectionOverrideProperties"
      ( \x ->
          AssetBundleExportJobVPCConnectionOverrideProperties'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..: "Properties")
      )

instance
  Prelude.Hashable
    AssetBundleExportJobVPCConnectionOverrideProperties
  where
  hashWithSalt
    _salt
    AssetBundleExportJobVPCConnectionOverrideProperties' {..} =
      _salt
        `Prelude.hashWithSalt` arn
        `Prelude.hashWithSalt` properties

instance
  Prelude.NFData
    AssetBundleExportJobVPCConnectionOverrideProperties
  where
  rnf
    AssetBundleExportJobVPCConnectionOverrideProperties' {..} =
      Prelude.rnf arn
        `Prelude.seq` Prelude.rnf properties

instance
  Data.ToJSON
    AssetBundleExportJobVPCConnectionOverrideProperties
  where
  toJSON
    AssetBundleExportJobVPCConnectionOverrideProperties' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Arn" Data..=) Prelude.<$> arn,
              Prelude.Just ("Properties" Data..= properties)
            ]
        )
