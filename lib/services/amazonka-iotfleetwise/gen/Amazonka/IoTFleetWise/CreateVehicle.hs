{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IoTFleetWise.CreateVehicle
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a vehicle, which is an instance of a vehicle model (model
-- manifest). Vehicles created from the same vehicle model consist of the
-- same signals inherited from the vehicle model.
--
-- If you have an existing Amazon Web Services IoT thing, you can use
-- Amazon Web Services IoT FleetWise to create a vehicle and collect data
-- from your thing.
--
-- For more information, see
-- <https://docs.aws.amazon.com/iot-fleetwise/latest/developerguide/create-vehicle-cli.html Create a vehicle (AWS CLI)>
-- in the /Amazon Web Services IoT FleetWise Developer Guide/.
module Amazonka.IoTFleetWise.CreateVehicle
  ( -- * Creating a Request
    CreateVehicle (..),
    newCreateVehicle,

    -- * Request Lenses
    createVehicle_associationBehavior,
    createVehicle_attributes,
    createVehicle_tags,
    createVehicle_vehicleName,
    createVehicle_modelManifestArn,
    createVehicle_decoderManifestArn,

    -- * Destructuring the Response
    CreateVehicleResponse (..),
    newCreateVehicleResponse,

    -- * Response Lenses
    createVehicleResponse_arn,
    createVehicleResponse_thingArn,
    createVehicleResponse_vehicleName,
    createVehicleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTFleetWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateVehicle' smart constructor.
data CreateVehicle = CreateVehicle'
  { -- | An option to create a new Amazon Web Services IoT thing when creating a
    -- vehicle, or to validate an existing Amazon Web Services IoT thing as a
    -- vehicle.
    --
    -- Default:
    associationBehavior :: Prelude.Maybe VehicleAssociationBehavior,
    -- | Static information about a vehicle in a key-value pair. For example:
    -- @\"engineType\"@ : @\"1.3 L R2\"@
    --
    -- A campaign must include the keys (attribute names) in
    -- @dataExtraDimensions@ for them to display in Amazon Timestream.
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Metadata that can be used to manage the vehicle.
    tags :: Prelude.Maybe [Tag],
    -- | The unique ID of the vehicle to create.
    vehicleName :: Prelude.Text,
    -- | The Amazon Resource Name ARN of a vehicle model.
    modelManifestArn :: Prelude.Text,
    -- | The ARN of a decoder manifest.
    decoderManifestArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVehicle' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associationBehavior', 'createVehicle_associationBehavior' - An option to create a new Amazon Web Services IoT thing when creating a
-- vehicle, or to validate an existing Amazon Web Services IoT thing as a
-- vehicle.
--
-- Default:
--
-- 'attributes', 'createVehicle_attributes' - Static information about a vehicle in a key-value pair. For example:
-- @\"engineType\"@ : @\"1.3 L R2\"@
--
-- A campaign must include the keys (attribute names) in
-- @dataExtraDimensions@ for them to display in Amazon Timestream.
--
-- 'tags', 'createVehicle_tags' - Metadata that can be used to manage the vehicle.
--
-- 'vehicleName', 'createVehicle_vehicleName' - The unique ID of the vehicle to create.
--
-- 'modelManifestArn', 'createVehicle_modelManifestArn' - The Amazon Resource Name ARN of a vehicle model.
--
-- 'decoderManifestArn', 'createVehicle_decoderManifestArn' - The ARN of a decoder manifest.
newCreateVehicle ::
  -- | 'vehicleName'
  Prelude.Text ->
  -- | 'modelManifestArn'
  Prelude.Text ->
  -- | 'decoderManifestArn'
  Prelude.Text ->
  CreateVehicle
newCreateVehicle
  pVehicleName_
  pModelManifestArn_
  pDecoderManifestArn_ =
    CreateVehicle'
      { associationBehavior =
          Prelude.Nothing,
        attributes = Prelude.Nothing,
        tags = Prelude.Nothing,
        vehicleName = pVehicleName_,
        modelManifestArn = pModelManifestArn_,
        decoderManifestArn = pDecoderManifestArn_
      }

-- | An option to create a new Amazon Web Services IoT thing when creating a
-- vehicle, or to validate an existing Amazon Web Services IoT thing as a
-- vehicle.
--
-- Default:
createVehicle_associationBehavior :: Lens.Lens' CreateVehicle (Prelude.Maybe VehicleAssociationBehavior)
createVehicle_associationBehavior = Lens.lens (\CreateVehicle' {associationBehavior} -> associationBehavior) (\s@CreateVehicle' {} a -> s {associationBehavior = a} :: CreateVehicle)

-- | Static information about a vehicle in a key-value pair. For example:
-- @\"engineType\"@ : @\"1.3 L R2\"@
--
-- A campaign must include the keys (attribute names) in
-- @dataExtraDimensions@ for them to display in Amazon Timestream.
createVehicle_attributes :: Lens.Lens' CreateVehicle (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createVehicle_attributes = Lens.lens (\CreateVehicle' {attributes} -> attributes) (\s@CreateVehicle' {} a -> s {attributes = a} :: CreateVehicle) Prelude.. Lens.mapping Lens.coerced

-- | Metadata that can be used to manage the vehicle.
createVehicle_tags :: Lens.Lens' CreateVehicle (Prelude.Maybe [Tag])
createVehicle_tags = Lens.lens (\CreateVehicle' {tags} -> tags) (\s@CreateVehicle' {} a -> s {tags = a} :: CreateVehicle) Prelude.. Lens.mapping Lens.coerced

-- | The unique ID of the vehicle to create.
createVehicle_vehicleName :: Lens.Lens' CreateVehicle Prelude.Text
createVehicle_vehicleName = Lens.lens (\CreateVehicle' {vehicleName} -> vehicleName) (\s@CreateVehicle' {} a -> s {vehicleName = a} :: CreateVehicle)

-- | The Amazon Resource Name ARN of a vehicle model.
createVehicle_modelManifestArn :: Lens.Lens' CreateVehicle Prelude.Text
createVehicle_modelManifestArn = Lens.lens (\CreateVehicle' {modelManifestArn} -> modelManifestArn) (\s@CreateVehicle' {} a -> s {modelManifestArn = a} :: CreateVehicle)

-- | The ARN of a decoder manifest.
createVehicle_decoderManifestArn :: Lens.Lens' CreateVehicle Prelude.Text
createVehicle_decoderManifestArn = Lens.lens (\CreateVehicle' {decoderManifestArn} -> decoderManifestArn) (\s@CreateVehicle' {} a -> s {decoderManifestArn = a} :: CreateVehicle)

instance Core.AWSRequest CreateVehicle where
  type
    AWSResponse CreateVehicle =
      CreateVehicleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateVehicleResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "thingArn")
            Prelude.<*> (x Data..?> "vehicleName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateVehicle where
  hashWithSalt _salt CreateVehicle' {..} =
    _salt
      `Prelude.hashWithSalt` associationBehavior
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` vehicleName
      `Prelude.hashWithSalt` modelManifestArn
      `Prelude.hashWithSalt` decoderManifestArn

instance Prelude.NFData CreateVehicle where
  rnf CreateVehicle' {..} =
    Prelude.rnf associationBehavior
      `Prelude.seq` Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf vehicleName
      `Prelude.seq` Prelude.rnf modelManifestArn
      `Prelude.seq` Prelude.rnf decoderManifestArn

instance Data.ToHeaders CreateVehicle where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "IoTAutobahnControlPlane.CreateVehicle" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateVehicle where
  toJSON CreateVehicle' {..} =
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

instance Data.ToPath CreateVehicle where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateVehicle where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateVehicleResponse' smart constructor.
data CreateVehicleResponse = CreateVehicleResponse'
  { -- | The ARN of the created vehicle.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of a created or validated Amazon Web Services IoT thing.
    thingArn :: Prelude.Maybe Prelude.Text,
    -- | The unique ID of the created vehicle.
    vehicleName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVehicleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createVehicleResponse_arn' - The ARN of the created vehicle.
--
-- 'thingArn', 'createVehicleResponse_thingArn' - The ARN of a created or validated Amazon Web Services IoT thing.
--
-- 'vehicleName', 'createVehicleResponse_vehicleName' - The unique ID of the created vehicle.
--
-- 'httpStatus', 'createVehicleResponse_httpStatus' - The response's http status code.
newCreateVehicleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateVehicleResponse
newCreateVehicleResponse pHttpStatus_ =
  CreateVehicleResponse'
    { arn = Prelude.Nothing,
      thingArn = Prelude.Nothing,
      vehicleName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the created vehicle.
createVehicleResponse_arn :: Lens.Lens' CreateVehicleResponse (Prelude.Maybe Prelude.Text)
createVehicleResponse_arn = Lens.lens (\CreateVehicleResponse' {arn} -> arn) (\s@CreateVehicleResponse' {} a -> s {arn = a} :: CreateVehicleResponse)

-- | The ARN of a created or validated Amazon Web Services IoT thing.
createVehicleResponse_thingArn :: Lens.Lens' CreateVehicleResponse (Prelude.Maybe Prelude.Text)
createVehicleResponse_thingArn = Lens.lens (\CreateVehicleResponse' {thingArn} -> thingArn) (\s@CreateVehicleResponse' {} a -> s {thingArn = a} :: CreateVehicleResponse)

-- | The unique ID of the created vehicle.
createVehicleResponse_vehicleName :: Lens.Lens' CreateVehicleResponse (Prelude.Maybe Prelude.Text)
createVehicleResponse_vehicleName = Lens.lens (\CreateVehicleResponse' {vehicleName} -> vehicleName) (\s@CreateVehicleResponse' {} a -> s {vehicleName = a} :: CreateVehicleResponse)

-- | The response's http status code.
createVehicleResponse_httpStatus :: Lens.Lens' CreateVehicleResponse Prelude.Int
createVehicleResponse_httpStatus = Lens.lens (\CreateVehicleResponse' {httpStatus} -> httpStatus) (\s@CreateVehicleResponse' {} a -> s {httpStatus = a} :: CreateVehicleResponse)

instance Prelude.NFData CreateVehicleResponse where
  rnf CreateVehicleResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf thingArn
      `Prelude.seq` Prelude.rnf vehicleName
      `Prelude.seq` Prelude.rnf httpStatus
