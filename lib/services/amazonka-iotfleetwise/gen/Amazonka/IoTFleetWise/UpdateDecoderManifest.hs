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
-- Module      : Amazonka.IoTFleetWise.UpdateDecoderManifest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a decoder manifest.
--
-- A decoder manifest can only be updated when the status is @DRAFT@. Only
-- @ACTIVE@ decoder manifests can be associated with vehicles.
module Amazonka.IoTFleetWise.UpdateDecoderManifest
  ( -- * Creating a Request
    UpdateDecoderManifest (..),
    newUpdateDecoderManifest,

    -- * Request Lenses
    updateDecoderManifest_description,
    updateDecoderManifest_networkInterfacesToAdd,
    updateDecoderManifest_networkInterfacesToRemove,
    updateDecoderManifest_networkInterfacesToUpdate,
    updateDecoderManifest_signalDecodersToAdd,
    updateDecoderManifest_signalDecodersToRemove,
    updateDecoderManifest_signalDecodersToUpdate,
    updateDecoderManifest_status,
    updateDecoderManifest_name,

    -- * Destructuring the Response
    UpdateDecoderManifestResponse (..),
    newUpdateDecoderManifestResponse,

    -- * Response Lenses
    updateDecoderManifestResponse_httpStatus,
    updateDecoderManifestResponse_name,
    updateDecoderManifestResponse_arn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTFleetWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateDecoderManifest' smart constructor.
data UpdateDecoderManifest = UpdateDecoderManifest'
  { -- | A brief description of the decoder manifest to update.
    description :: Prelude.Maybe Prelude.Text,
    -- | A list of information about the network interfaces to add to the decoder
    -- manifest.
    networkInterfacesToAdd :: Prelude.Maybe (Prelude.NonEmpty NetworkInterface),
    -- | A list of network interfaces to remove from the decoder manifest.
    networkInterfacesToRemove :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A list of information about the network interfaces to update in the
    -- decoder manifest.
    networkInterfacesToUpdate :: Prelude.Maybe (Prelude.NonEmpty NetworkInterface),
    -- | A list of information about decoding additional signals to add to the
    -- decoder manifest.
    signalDecodersToAdd :: Prelude.Maybe (Prelude.NonEmpty SignalDecoder),
    -- | A list of signal decoders to remove from the decoder manifest.
    signalDecodersToRemove :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A list of updated information about decoding signals to update in the
    -- decoder manifest.
    signalDecodersToUpdate :: Prelude.Maybe (Prelude.NonEmpty SignalDecoder),
    -- | The state of the decoder manifest. If the status is @ACTIVE@, the
    -- decoder manifest can\'t be edited. If the status is @DRAFT@, you can
    -- edit the decoder manifest.
    status :: Prelude.Maybe ManifestStatus,
    -- | The name of the decoder manifest to update.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDecoderManifest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateDecoderManifest_description' - A brief description of the decoder manifest to update.
--
-- 'networkInterfacesToAdd', 'updateDecoderManifest_networkInterfacesToAdd' - A list of information about the network interfaces to add to the decoder
-- manifest.
--
-- 'networkInterfacesToRemove', 'updateDecoderManifest_networkInterfacesToRemove' - A list of network interfaces to remove from the decoder manifest.
--
-- 'networkInterfacesToUpdate', 'updateDecoderManifest_networkInterfacesToUpdate' - A list of information about the network interfaces to update in the
-- decoder manifest.
--
-- 'signalDecodersToAdd', 'updateDecoderManifest_signalDecodersToAdd' - A list of information about decoding additional signals to add to the
-- decoder manifest.
--
-- 'signalDecodersToRemove', 'updateDecoderManifest_signalDecodersToRemove' - A list of signal decoders to remove from the decoder manifest.
--
-- 'signalDecodersToUpdate', 'updateDecoderManifest_signalDecodersToUpdate' - A list of updated information about decoding signals to update in the
-- decoder manifest.
--
-- 'status', 'updateDecoderManifest_status' - The state of the decoder manifest. If the status is @ACTIVE@, the
-- decoder manifest can\'t be edited. If the status is @DRAFT@, you can
-- edit the decoder manifest.
--
-- 'name', 'updateDecoderManifest_name' - The name of the decoder manifest to update.
newUpdateDecoderManifest ::
  -- | 'name'
  Prelude.Text ->
  UpdateDecoderManifest
newUpdateDecoderManifest pName_ =
  UpdateDecoderManifest'
    { description =
        Prelude.Nothing,
      networkInterfacesToAdd = Prelude.Nothing,
      networkInterfacesToRemove = Prelude.Nothing,
      networkInterfacesToUpdate = Prelude.Nothing,
      signalDecodersToAdd = Prelude.Nothing,
      signalDecodersToRemove = Prelude.Nothing,
      signalDecodersToUpdate = Prelude.Nothing,
      status = Prelude.Nothing,
      name = pName_
    }

-- | A brief description of the decoder manifest to update.
updateDecoderManifest_description :: Lens.Lens' UpdateDecoderManifest (Prelude.Maybe Prelude.Text)
updateDecoderManifest_description = Lens.lens (\UpdateDecoderManifest' {description} -> description) (\s@UpdateDecoderManifest' {} a -> s {description = a} :: UpdateDecoderManifest)

-- | A list of information about the network interfaces to add to the decoder
-- manifest.
updateDecoderManifest_networkInterfacesToAdd :: Lens.Lens' UpdateDecoderManifest (Prelude.Maybe (Prelude.NonEmpty NetworkInterface))
updateDecoderManifest_networkInterfacesToAdd = Lens.lens (\UpdateDecoderManifest' {networkInterfacesToAdd} -> networkInterfacesToAdd) (\s@UpdateDecoderManifest' {} a -> s {networkInterfacesToAdd = a} :: UpdateDecoderManifest) Prelude.. Lens.mapping Lens.coerced

-- | A list of network interfaces to remove from the decoder manifest.
updateDecoderManifest_networkInterfacesToRemove :: Lens.Lens' UpdateDecoderManifest (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
updateDecoderManifest_networkInterfacesToRemove = Lens.lens (\UpdateDecoderManifest' {networkInterfacesToRemove} -> networkInterfacesToRemove) (\s@UpdateDecoderManifest' {} a -> s {networkInterfacesToRemove = a} :: UpdateDecoderManifest) Prelude.. Lens.mapping Lens.coerced

-- | A list of information about the network interfaces to update in the
-- decoder manifest.
updateDecoderManifest_networkInterfacesToUpdate :: Lens.Lens' UpdateDecoderManifest (Prelude.Maybe (Prelude.NonEmpty NetworkInterface))
updateDecoderManifest_networkInterfacesToUpdate = Lens.lens (\UpdateDecoderManifest' {networkInterfacesToUpdate} -> networkInterfacesToUpdate) (\s@UpdateDecoderManifest' {} a -> s {networkInterfacesToUpdate = a} :: UpdateDecoderManifest) Prelude.. Lens.mapping Lens.coerced

-- | A list of information about decoding additional signals to add to the
-- decoder manifest.
updateDecoderManifest_signalDecodersToAdd :: Lens.Lens' UpdateDecoderManifest (Prelude.Maybe (Prelude.NonEmpty SignalDecoder))
updateDecoderManifest_signalDecodersToAdd = Lens.lens (\UpdateDecoderManifest' {signalDecodersToAdd} -> signalDecodersToAdd) (\s@UpdateDecoderManifest' {} a -> s {signalDecodersToAdd = a} :: UpdateDecoderManifest) Prelude.. Lens.mapping Lens.coerced

-- | A list of signal decoders to remove from the decoder manifest.
updateDecoderManifest_signalDecodersToRemove :: Lens.Lens' UpdateDecoderManifest (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
updateDecoderManifest_signalDecodersToRemove = Lens.lens (\UpdateDecoderManifest' {signalDecodersToRemove} -> signalDecodersToRemove) (\s@UpdateDecoderManifest' {} a -> s {signalDecodersToRemove = a} :: UpdateDecoderManifest) Prelude.. Lens.mapping Lens.coerced

-- | A list of updated information about decoding signals to update in the
-- decoder manifest.
updateDecoderManifest_signalDecodersToUpdate :: Lens.Lens' UpdateDecoderManifest (Prelude.Maybe (Prelude.NonEmpty SignalDecoder))
updateDecoderManifest_signalDecodersToUpdate = Lens.lens (\UpdateDecoderManifest' {signalDecodersToUpdate} -> signalDecodersToUpdate) (\s@UpdateDecoderManifest' {} a -> s {signalDecodersToUpdate = a} :: UpdateDecoderManifest) Prelude.. Lens.mapping Lens.coerced

-- | The state of the decoder manifest. If the status is @ACTIVE@, the
-- decoder manifest can\'t be edited. If the status is @DRAFT@, you can
-- edit the decoder manifest.
updateDecoderManifest_status :: Lens.Lens' UpdateDecoderManifest (Prelude.Maybe ManifestStatus)
updateDecoderManifest_status = Lens.lens (\UpdateDecoderManifest' {status} -> status) (\s@UpdateDecoderManifest' {} a -> s {status = a} :: UpdateDecoderManifest)

-- | The name of the decoder manifest to update.
updateDecoderManifest_name :: Lens.Lens' UpdateDecoderManifest Prelude.Text
updateDecoderManifest_name = Lens.lens (\UpdateDecoderManifest' {name} -> name) (\s@UpdateDecoderManifest' {} a -> s {name = a} :: UpdateDecoderManifest)

instance Core.AWSRequest UpdateDecoderManifest where
  type
    AWSResponse UpdateDecoderManifest =
      UpdateDecoderManifestResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDecoderManifestResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "name")
            Prelude.<*> (x Data..:> "arn")
      )

instance Prelude.Hashable UpdateDecoderManifest where
  hashWithSalt _salt UpdateDecoderManifest' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` networkInterfacesToAdd
      `Prelude.hashWithSalt` networkInterfacesToRemove
      `Prelude.hashWithSalt` networkInterfacesToUpdate
      `Prelude.hashWithSalt` signalDecodersToAdd
      `Prelude.hashWithSalt` signalDecodersToRemove
      `Prelude.hashWithSalt` signalDecodersToUpdate
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` name

instance Prelude.NFData UpdateDecoderManifest where
  rnf UpdateDecoderManifest' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf networkInterfacesToAdd
      `Prelude.seq` Prelude.rnf networkInterfacesToRemove
      `Prelude.seq` Prelude.rnf networkInterfacesToUpdate
      `Prelude.seq` Prelude.rnf signalDecodersToAdd
      `Prelude.seq` Prelude.rnf signalDecodersToRemove
      `Prelude.seq` Prelude.rnf signalDecodersToUpdate
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders UpdateDecoderManifest where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "IoTAutobahnControlPlane.UpdateDecoderManifest" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateDecoderManifest where
  toJSON UpdateDecoderManifest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("networkInterfacesToAdd" Data..=)
              Prelude.<$> networkInterfacesToAdd,
            ("networkInterfacesToRemove" Data..=)
              Prelude.<$> networkInterfacesToRemove,
            ("networkInterfacesToUpdate" Data..=)
              Prelude.<$> networkInterfacesToUpdate,
            ("signalDecodersToAdd" Data..=)
              Prelude.<$> signalDecodersToAdd,
            ("signalDecodersToRemove" Data..=)
              Prelude.<$> signalDecodersToRemove,
            ("signalDecodersToUpdate" Data..=)
              Prelude.<$> signalDecodersToUpdate,
            ("status" Data..=) Prelude.<$> status,
            Prelude.Just ("name" Data..= name)
          ]
      )

instance Data.ToPath UpdateDecoderManifest where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateDecoderManifest where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDecoderManifestResponse' smart constructor.
data UpdateDecoderManifestResponse = UpdateDecoderManifestResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the updated decoder manifest.
    name :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the updated decoder manifest.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDecoderManifestResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateDecoderManifestResponse_httpStatus' - The response's http status code.
--
-- 'name', 'updateDecoderManifestResponse_name' - The name of the updated decoder manifest.
--
-- 'arn', 'updateDecoderManifestResponse_arn' - The Amazon Resource Name (ARN) of the updated decoder manifest.
newUpdateDecoderManifestResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'name'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  UpdateDecoderManifestResponse
newUpdateDecoderManifestResponse
  pHttpStatus_
  pName_
  pArn_ =
    UpdateDecoderManifestResponse'
      { httpStatus =
          pHttpStatus_,
        name = pName_,
        arn = pArn_
      }

-- | The response's http status code.
updateDecoderManifestResponse_httpStatus :: Lens.Lens' UpdateDecoderManifestResponse Prelude.Int
updateDecoderManifestResponse_httpStatus = Lens.lens (\UpdateDecoderManifestResponse' {httpStatus} -> httpStatus) (\s@UpdateDecoderManifestResponse' {} a -> s {httpStatus = a} :: UpdateDecoderManifestResponse)

-- | The name of the updated decoder manifest.
updateDecoderManifestResponse_name :: Lens.Lens' UpdateDecoderManifestResponse Prelude.Text
updateDecoderManifestResponse_name = Lens.lens (\UpdateDecoderManifestResponse' {name} -> name) (\s@UpdateDecoderManifestResponse' {} a -> s {name = a} :: UpdateDecoderManifestResponse)

-- | The Amazon Resource Name (ARN) of the updated decoder manifest.
updateDecoderManifestResponse_arn :: Lens.Lens' UpdateDecoderManifestResponse Prelude.Text
updateDecoderManifestResponse_arn = Lens.lens (\UpdateDecoderManifestResponse' {arn} -> arn) (\s@UpdateDecoderManifestResponse' {} a -> s {arn = a} :: UpdateDecoderManifestResponse)

instance Prelude.NFData UpdateDecoderManifestResponse where
  rnf UpdateDecoderManifestResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf arn
