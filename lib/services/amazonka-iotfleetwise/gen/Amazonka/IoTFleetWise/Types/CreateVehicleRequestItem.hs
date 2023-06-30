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
-- Module      : Amazonka.IoTFleetWise.Types.CreateVehicleRequestItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTFleetWise.Types.CreateVehicleRequestItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTFleetWise.Types.Tag
import Amazonka.IoTFleetWise.Types.VehicleAssociationBehavior
import qualified Amazonka.Prelude as Prelude

-- | Information about the vehicle to create.
--
-- /See:/ 'newCreateVehicleRequestItem' smart constructor.
data CreateVehicleRequestItem = CreateVehicleRequestItem'
  { -- | An option to create a new Amazon Web Services IoT thing when creating a
    -- vehicle, or to validate an existing thing as a vehicle.
    associationBehavior :: Prelude.Maybe VehicleAssociationBehavior,
    -- | Static information about a vehicle in a key-value pair. For example:
    -- @\"engine Type\"@ : @\"v6\"@
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Metadata which can be used to manage the vehicle.
    tags :: Prelude.Maybe [Tag],
    -- | The unique ID of the vehicle to create.
    vehicleName :: Prelude.Text,
    -- | The ARN of the vehicle model (model manifest) to create the vehicle
    -- from.
    modelManifestArn :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of a decoder manifest associated with the
    -- vehicle to create.
    decoderManifestArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVehicleRequestItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associationBehavior', 'createVehicleRequestItem_associationBehavior' - An option to create a new Amazon Web Services IoT thing when creating a
-- vehicle, or to validate an existing thing as a vehicle.
--
-- 'attributes', 'createVehicleRequestItem_attributes' - Static information about a vehicle in a key-value pair. For example:
-- @\"engine Type\"@ : @\"v6\"@
--
-- 'tags', 'createVehicleRequestItem_tags' - Metadata which can be used to manage the vehicle.
--
-- 'vehicleName', 'createVehicleRequestItem_vehicleName' - The unique ID of the vehicle to create.
--
-- 'modelManifestArn', 'createVehicleRequestItem_modelManifestArn' - The ARN of the vehicle model (model manifest) to create the vehicle
-- from.
--
-- 'decoderManifestArn', 'createVehicleRequestItem_decoderManifestArn' - The Amazon Resource Name (ARN) of a decoder manifest associated with the
-- vehicle to create.
newCreateVehicleRequestItem ::
  -- | 'vehicleName'
  Prelude.Text ->
  -- | 'modelManifestArn'
  Prelude.Text ->
  -- | 'decoderManifestArn'
  Prelude.Text ->
  CreateVehicleRequestItem
newCreateVehicleRequestItem
  pVehicleName_
  pModelManifestArn_
  pDecoderManifestArn_ =
    CreateVehicleRequestItem'
      { associationBehavior =
          Prelude.Nothing,
        attributes = Prelude.Nothing,
        tags = Prelude.Nothing,
        vehicleName = pVehicleName_,
        modelManifestArn = pModelManifestArn_,
        decoderManifestArn = pDecoderManifestArn_
      }

-- | An option to create a new Amazon Web Services IoT thing when creating a
-- vehicle, or to validate an existing thing as a vehicle.
createVehicleRequestItem_associationBehavior :: Lens.Lens' CreateVehicleRequestItem (Prelude.Maybe VehicleAssociationBehavior)
createVehicleRequestItem_associationBehavior = Lens.lens (\CreateVehicleRequestItem' {associationBehavior} -> associationBehavior) (\s@CreateVehicleRequestItem' {} a -> s {associationBehavior = a} :: CreateVehicleRequestItem)

-- | Static information about a vehicle in a key-value pair. For example:
-- @\"engine Type\"@ : @\"v6\"@
createVehicleRequestItem_attributes :: Lens.Lens' CreateVehicleRequestItem (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createVehicleRequestItem_attributes = Lens.lens (\CreateVehicleRequestItem' {attributes} -> attributes) (\s@CreateVehicleRequestItem' {} a -> s {attributes = a} :: CreateVehicleRequestItem) Prelude.. Lens.mapping Lens.coerced

-- | Metadata which can be used to manage the vehicle.
createVehicleRequestItem_tags :: Lens.Lens' CreateVehicleRequestItem (Prelude.Maybe [Tag])
createVehicleRequestItem_tags = Lens.lens (\CreateVehicleRequestItem' {tags} -> tags) (\s@CreateVehicleRequestItem' {} a -> s {tags = a} :: CreateVehicleRequestItem) Prelude.. Lens.mapping Lens.coerced

-- | The unique ID of the vehicle to create.
createVehicleRequestItem_vehicleName :: Lens.Lens' CreateVehicleRequestItem Prelude.Text
createVehicleRequestItem_vehicleName = Lens.lens (\CreateVehicleRequestItem' {vehicleName} -> vehicleName) (\s@CreateVehicleRequestItem' {} a -> s {vehicleName = a} :: CreateVehicleRequestItem)

-- | The ARN of the vehicle model (model manifest) to create the vehicle
-- from.
createVehicleRequestItem_modelManifestArn :: Lens.Lens' CreateVehicleRequestItem Prelude.Text
createVehicleRequestItem_modelManifestArn = Lens.lens (\CreateVehicleRequestItem' {modelManifestArn} -> modelManifestArn) (\s@CreateVehicleRequestItem' {} a -> s {modelManifestArn = a} :: CreateVehicleRequestItem)

-- | The Amazon Resource Name (ARN) of a decoder manifest associated with the
-- vehicle to create.
createVehicleRequestItem_decoderManifestArn :: Lens.Lens' CreateVehicleRequestItem Prelude.Text
createVehicleRequestItem_decoderManifestArn = Lens.lens (\CreateVehicleRequestItem' {decoderManifestArn} -> decoderManifestArn) (\s@CreateVehicleRequestItem' {} a -> s {decoderManifestArn = a} :: CreateVehicleRequestItem)

instance Prelude.Hashable CreateVehicleRequestItem where
  hashWithSalt _salt CreateVehicleRequestItem' {..} =
    _salt
      `Prelude.hashWithSalt` associationBehavior
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` vehicleName
      `Prelude.hashWithSalt` modelManifestArn
      `Prelude.hashWithSalt` decoderManifestArn

instance Prelude.NFData CreateVehicleRequestItem where
  rnf CreateVehicleRequestItem' {..} =
    Prelude.rnf associationBehavior
      `Prelude.seq` Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf vehicleName
      `Prelude.seq` Prelude.rnf modelManifestArn
      `Prelude.seq` Prelude.rnf decoderManifestArn

instance Data.ToJSON CreateVehicleRequestItem where
  toJSON CreateVehicleRequestItem' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("associationBehavior" Data..=)
              Prelude.<$> associationBehavior,
            ("attributes" Data..=) Prelude.<$> attributes,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("vehicleName" Data..= vehicleName),
            Prelude.Just
              ("modelManifestArn" Data..= modelManifestArn),
            Prelude.Just
              ("decoderManifestArn" Data..= decoderManifestArn)
          ]
      )
