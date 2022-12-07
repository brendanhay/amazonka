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
-- Module      : Amazonka.IoTFleetWise.Types.UpdateVehicleRequestItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTFleetWise.Types.UpdateVehicleRequestItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTFleetWise.Types.UpdateMode
import qualified Amazonka.Prelude as Prelude

-- | Information about the vehicle to update.
--
-- /See:/ 'newUpdateVehicleRequestItem' smart constructor.
data UpdateVehicleRequestItem = UpdateVehicleRequestItem'
  { -- | The ARN of the vehicle model (model manifest) associated with the
    -- vehicle to update.
    modelManifestArn :: Prelude.Maybe Prelude.Text,
    -- | Static information about a vehicle in a key-value pair. For example:
    --
    -- @\"engineType\"@ : @\"1.3 L R2\"@
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The ARN of the signal decoder manifest associated with the vehicle to
    -- update.
    decoderManifestArn :: Prelude.Maybe Prelude.Text,
    -- | The method the specified attributes will update the existing attributes
    -- on the vehicle. Use@Overwite@ to replace the vehicle attributes with the
    -- specified attributes. Or use @Merge@ to combine all attributes.
    --
    -- This is required if attributes are present in the input.
    attributeUpdateMode :: Prelude.Maybe UpdateMode,
    -- | The unique ID of the vehicle to update.
    vehicleName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateVehicleRequestItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelManifestArn', 'updateVehicleRequestItem_modelManifestArn' - The ARN of the vehicle model (model manifest) associated with the
-- vehicle to update.
--
-- 'attributes', 'updateVehicleRequestItem_attributes' - Static information about a vehicle in a key-value pair. For example:
--
-- @\"engineType\"@ : @\"1.3 L R2\"@
--
-- 'decoderManifestArn', 'updateVehicleRequestItem_decoderManifestArn' - The ARN of the signal decoder manifest associated with the vehicle to
-- update.
--
-- 'attributeUpdateMode', 'updateVehicleRequestItem_attributeUpdateMode' - The method the specified attributes will update the existing attributes
-- on the vehicle. Use@Overwite@ to replace the vehicle attributes with the
-- specified attributes. Or use @Merge@ to combine all attributes.
--
-- This is required if attributes are present in the input.
--
-- 'vehicleName', 'updateVehicleRequestItem_vehicleName' - The unique ID of the vehicle to update.
newUpdateVehicleRequestItem ::
  -- | 'vehicleName'
  Prelude.Text ->
  UpdateVehicleRequestItem
newUpdateVehicleRequestItem pVehicleName_ =
  UpdateVehicleRequestItem'
    { modelManifestArn =
        Prelude.Nothing,
      attributes = Prelude.Nothing,
      decoderManifestArn = Prelude.Nothing,
      attributeUpdateMode = Prelude.Nothing,
      vehicleName = pVehicleName_
    }

-- | The ARN of the vehicle model (model manifest) associated with the
-- vehicle to update.
updateVehicleRequestItem_modelManifestArn :: Lens.Lens' UpdateVehicleRequestItem (Prelude.Maybe Prelude.Text)
updateVehicleRequestItem_modelManifestArn = Lens.lens (\UpdateVehicleRequestItem' {modelManifestArn} -> modelManifestArn) (\s@UpdateVehicleRequestItem' {} a -> s {modelManifestArn = a} :: UpdateVehicleRequestItem)

-- | Static information about a vehicle in a key-value pair. For example:
--
-- @\"engineType\"@ : @\"1.3 L R2\"@
updateVehicleRequestItem_attributes :: Lens.Lens' UpdateVehicleRequestItem (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateVehicleRequestItem_attributes = Lens.lens (\UpdateVehicleRequestItem' {attributes} -> attributes) (\s@UpdateVehicleRequestItem' {} a -> s {attributes = a} :: UpdateVehicleRequestItem) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the signal decoder manifest associated with the vehicle to
-- update.
updateVehicleRequestItem_decoderManifestArn :: Lens.Lens' UpdateVehicleRequestItem (Prelude.Maybe Prelude.Text)
updateVehicleRequestItem_decoderManifestArn = Lens.lens (\UpdateVehicleRequestItem' {decoderManifestArn} -> decoderManifestArn) (\s@UpdateVehicleRequestItem' {} a -> s {decoderManifestArn = a} :: UpdateVehicleRequestItem)

-- | The method the specified attributes will update the existing attributes
-- on the vehicle. Use@Overwite@ to replace the vehicle attributes with the
-- specified attributes. Or use @Merge@ to combine all attributes.
--
-- This is required if attributes are present in the input.
updateVehicleRequestItem_attributeUpdateMode :: Lens.Lens' UpdateVehicleRequestItem (Prelude.Maybe UpdateMode)
updateVehicleRequestItem_attributeUpdateMode = Lens.lens (\UpdateVehicleRequestItem' {attributeUpdateMode} -> attributeUpdateMode) (\s@UpdateVehicleRequestItem' {} a -> s {attributeUpdateMode = a} :: UpdateVehicleRequestItem)

-- | The unique ID of the vehicle to update.
updateVehicleRequestItem_vehicleName :: Lens.Lens' UpdateVehicleRequestItem Prelude.Text
updateVehicleRequestItem_vehicleName = Lens.lens (\UpdateVehicleRequestItem' {vehicleName} -> vehicleName) (\s@UpdateVehicleRequestItem' {} a -> s {vehicleName = a} :: UpdateVehicleRequestItem)

instance Prelude.Hashable UpdateVehicleRequestItem where
  hashWithSalt _salt UpdateVehicleRequestItem' {..} =
    _salt `Prelude.hashWithSalt` modelManifestArn
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` decoderManifestArn
      `Prelude.hashWithSalt` attributeUpdateMode
      `Prelude.hashWithSalt` vehicleName

instance Prelude.NFData UpdateVehicleRequestItem where
  rnf UpdateVehicleRequestItem' {..} =
    Prelude.rnf modelManifestArn
      `Prelude.seq` Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf decoderManifestArn
      `Prelude.seq` Prelude.rnf attributeUpdateMode
      `Prelude.seq` Prelude.rnf vehicleName

instance Data.ToJSON UpdateVehicleRequestItem where
  toJSON UpdateVehicleRequestItem' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("modelManifestArn" Data..=)
              Prelude.<$> modelManifestArn,
            ("attributes" Data..=) Prelude.<$> attributes,
            ("decoderManifestArn" Data..=)
              Prelude.<$> decoderManifestArn,
            ("attributeUpdateMode" Data..=)
              Prelude.<$> attributeUpdateMode,
            Prelude.Just ("vehicleName" Data..= vehicleName)
          ]
      )
