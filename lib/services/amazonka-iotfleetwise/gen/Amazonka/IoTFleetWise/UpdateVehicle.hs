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
-- Module      : Amazonka.IoTFleetWise.UpdateVehicle
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a vehicle.
module Amazonka.IoTFleetWise.UpdateVehicle
  ( -- * Creating a Request
    UpdateVehicle (..),
    newUpdateVehicle,

    -- * Request Lenses
    updateVehicle_attributeUpdateMode,
    updateVehicle_attributes,
    updateVehicle_decoderManifestArn,
    updateVehicle_modelManifestArn,
    updateVehicle_vehicleName,

    -- * Destructuring the Response
    UpdateVehicleResponse (..),
    newUpdateVehicleResponse,

    -- * Response Lenses
    updateVehicleResponse_arn,
    updateVehicleResponse_vehicleName,
    updateVehicleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTFleetWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateVehicle' smart constructor.
data UpdateVehicle = UpdateVehicle'
  { -- | The method the specified attributes will update the existing attributes
    -- on the vehicle. Use@Overwite@ to replace the vehicle attributes with the
    -- specified attributes. Or use @Merge@ to combine all attributes.
    --
    -- This is required if attributes are present in the input.
    attributeUpdateMode :: Prelude.Maybe UpdateMode,
    -- | Static information about a vehicle in a key-value pair. For example:
    --
    -- @\"engineType\"@ : @\"1.3 L R2\"@
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The ARN of the decoder manifest associated with this vehicle.
    decoderManifestArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of a vehicle model (model manifest) associated with the vehicle.
    modelManifestArn :: Prelude.Maybe Prelude.Text,
    -- | The unique ID of the vehicle to update.
    vehicleName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateVehicle' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeUpdateMode', 'updateVehicle_attributeUpdateMode' - The method the specified attributes will update the existing attributes
-- on the vehicle. Use@Overwite@ to replace the vehicle attributes with the
-- specified attributes. Or use @Merge@ to combine all attributes.
--
-- This is required if attributes are present in the input.
--
-- 'attributes', 'updateVehicle_attributes' - Static information about a vehicle in a key-value pair. For example:
--
-- @\"engineType\"@ : @\"1.3 L R2\"@
--
-- 'decoderManifestArn', 'updateVehicle_decoderManifestArn' - The ARN of the decoder manifest associated with this vehicle.
--
-- 'modelManifestArn', 'updateVehicle_modelManifestArn' - The ARN of a vehicle model (model manifest) associated with the vehicle.
--
-- 'vehicleName', 'updateVehicle_vehicleName' - The unique ID of the vehicle to update.
newUpdateVehicle ::
  -- | 'vehicleName'
  Prelude.Text ->
  UpdateVehicle
newUpdateVehicle pVehicleName_ =
  UpdateVehicle'
    { attributeUpdateMode =
        Prelude.Nothing,
      attributes = Prelude.Nothing,
      decoderManifestArn = Prelude.Nothing,
      modelManifestArn = Prelude.Nothing,
      vehicleName = pVehicleName_
    }

-- | The method the specified attributes will update the existing attributes
-- on the vehicle. Use@Overwite@ to replace the vehicle attributes with the
-- specified attributes. Or use @Merge@ to combine all attributes.
--
-- This is required if attributes are present in the input.
updateVehicle_attributeUpdateMode :: Lens.Lens' UpdateVehicle (Prelude.Maybe UpdateMode)
updateVehicle_attributeUpdateMode = Lens.lens (\UpdateVehicle' {attributeUpdateMode} -> attributeUpdateMode) (\s@UpdateVehicle' {} a -> s {attributeUpdateMode = a} :: UpdateVehicle)

-- | Static information about a vehicle in a key-value pair. For example:
--
-- @\"engineType\"@ : @\"1.3 L R2\"@
updateVehicle_attributes :: Lens.Lens' UpdateVehicle (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateVehicle_attributes = Lens.lens (\UpdateVehicle' {attributes} -> attributes) (\s@UpdateVehicle' {} a -> s {attributes = a} :: UpdateVehicle) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the decoder manifest associated with this vehicle.
updateVehicle_decoderManifestArn :: Lens.Lens' UpdateVehicle (Prelude.Maybe Prelude.Text)
updateVehicle_decoderManifestArn = Lens.lens (\UpdateVehicle' {decoderManifestArn} -> decoderManifestArn) (\s@UpdateVehicle' {} a -> s {decoderManifestArn = a} :: UpdateVehicle)

-- | The ARN of a vehicle model (model manifest) associated with the vehicle.
updateVehicle_modelManifestArn :: Lens.Lens' UpdateVehicle (Prelude.Maybe Prelude.Text)
updateVehicle_modelManifestArn = Lens.lens (\UpdateVehicle' {modelManifestArn} -> modelManifestArn) (\s@UpdateVehicle' {} a -> s {modelManifestArn = a} :: UpdateVehicle)

-- | The unique ID of the vehicle to update.
updateVehicle_vehicleName :: Lens.Lens' UpdateVehicle Prelude.Text
updateVehicle_vehicleName = Lens.lens (\UpdateVehicle' {vehicleName} -> vehicleName) (\s@UpdateVehicle' {} a -> s {vehicleName = a} :: UpdateVehicle)

instance Core.AWSRequest UpdateVehicle where
  type
    AWSResponse UpdateVehicle =
      UpdateVehicleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateVehicleResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "vehicleName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateVehicle where
  hashWithSalt _salt UpdateVehicle' {..} =
    _salt
      `Prelude.hashWithSalt` attributeUpdateMode
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` decoderManifestArn
      `Prelude.hashWithSalt` modelManifestArn
      `Prelude.hashWithSalt` vehicleName

instance Prelude.NFData UpdateVehicle where
  rnf UpdateVehicle' {..} =
    Prelude.rnf attributeUpdateMode
      `Prelude.seq` Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf decoderManifestArn
      `Prelude.seq` Prelude.rnf modelManifestArn
      `Prelude.seq` Prelude.rnf vehicleName

instance Data.ToHeaders UpdateVehicle where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "IoTAutobahnControlPlane.UpdateVehicle" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateVehicle where
  toJSON UpdateVehicle' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("attributeUpdateMode" Data..=)
              Prelude.<$> attributeUpdateMode,
            ("attributes" Data..=) Prelude.<$> attributes,
            ("decoderManifestArn" Data..=)
              Prelude.<$> decoderManifestArn,
            ("modelManifestArn" Data..=)
              Prelude.<$> modelManifestArn,
            Prelude.Just ("vehicleName" Data..= vehicleName)
          ]
      )

instance Data.ToPath UpdateVehicle where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateVehicle where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateVehicleResponse' smart constructor.
data UpdateVehicleResponse = UpdateVehicleResponse'
  { -- | The ARN of the updated vehicle.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the updated vehicle.
    vehicleName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateVehicleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'updateVehicleResponse_arn' - The ARN of the updated vehicle.
--
-- 'vehicleName', 'updateVehicleResponse_vehicleName' - The ID of the updated vehicle.
--
-- 'httpStatus', 'updateVehicleResponse_httpStatus' - The response's http status code.
newUpdateVehicleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateVehicleResponse
newUpdateVehicleResponse pHttpStatus_ =
  UpdateVehicleResponse'
    { arn = Prelude.Nothing,
      vehicleName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the updated vehicle.
updateVehicleResponse_arn :: Lens.Lens' UpdateVehicleResponse (Prelude.Maybe Prelude.Text)
updateVehicleResponse_arn = Lens.lens (\UpdateVehicleResponse' {arn} -> arn) (\s@UpdateVehicleResponse' {} a -> s {arn = a} :: UpdateVehicleResponse)

-- | The ID of the updated vehicle.
updateVehicleResponse_vehicleName :: Lens.Lens' UpdateVehicleResponse (Prelude.Maybe Prelude.Text)
updateVehicleResponse_vehicleName = Lens.lens (\UpdateVehicleResponse' {vehicleName} -> vehicleName) (\s@UpdateVehicleResponse' {} a -> s {vehicleName = a} :: UpdateVehicleResponse)

-- | The response's http status code.
updateVehicleResponse_httpStatus :: Lens.Lens' UpdateVehicleResponse Prelude.Int
updateVehicleResponse_httpStatus = Lens.lens (\UpdateVehicleResponse' {httpStatus} -> httpStatus) (\s@UpdateVehicleResponse' {} a -> s {httpStatus = a} :: UpdateVehicleResponse)

instance Prelude.NFData UpdateVehicleResponse where
  rnf UpdateVehicleResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf vehicleName
      `Prelude.seq` Prelude.rnf httpStatus
