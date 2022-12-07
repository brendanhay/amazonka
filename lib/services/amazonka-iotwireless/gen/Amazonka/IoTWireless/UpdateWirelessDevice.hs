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
-- Module      : Amazonka.IoTWireless.UpdateWirelessDevice
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates properties of a wireless device.
module Amazonka.IoTWireless.UpdateWirelessDevice
  ( -- * Creating a Request
    UpdateWirelessDevice (..),
    newUpdateWirelessDevice,

    -- * Request Lenses
    updateWirelessDevice_name,
    updateWirelessDevice_loRaWAN,
    updateWirelessDevice_destinationName,
    updateWirelessDevice_description,
    updateWirelessDevice_id,

    -- * Destructuring the Response
    UpdateWirelessDeviceResponse (..),
    newUpdateWirelessDeviceResponse,

    -- * Response Lenses
    updateWirelessDeviceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateWirelessDevice' smart constructor.
data UpdateWirelessDevice = UpdateWirelessDevice'
  { -- | The new name of the resource.
    name :: Prelude.Maybe Prelude.Text,
    -- | The updated wireless device\'s configuration.
    loRaWAN :: Prelude.Maybe LoRaWANUpdateDevice,
    -- | The name of the new destination for the device.
    destinationName :: Prelude.Maybe Prelude.Text,
    -- | A new description of the resource.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the resource to update.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateWirelessDevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateWirelessDevice_name' - The new name of the resource.
--
-- 'loRaWAN', 'updateWirelessDevice_loRaWAN' - The updated wireless device\'s configuration.
--
-- 'destinationName', 'updateWirelessDevice_destinationName' - The name of the new destination for the device.
--
-- 'description', 'updateWirelessDevice_description' - A new description of the resource.
--
-- 'id', 'updateWirelessDevice_id' - The ID of the resource to update.
newUpdateWirelessDevice ::
  -- | 'id'
  Prelude.Text ->
  UpdateWirelessDevice
newUpdateWirelessDevice pId_ =
  UpdateWirelessDevice'
    { name = Prelude.Nothing,
      loRaWAN = Prelude.Nothing,
      destinationName = Prelude.Nothing,
      description = Prelude.Nothing,
      id = pId_
    }

-- | The new name of the resource.
updateWirelessDevice_name :: Lens.Lens' UpdateWirelessDevice (Prelude.Maybe Prelude.Text)
updateWirelessDevice_name = Lens.lens (\UpdateWirelessDevice' {name} -> name) (\s@UpdateWirelessDevice' {} a -> s {name = a} :: UpdateWirelessDevice)

-- | The updated wireless device\'s configuration.
updateWirelessDevice_loRaWAN :: Lens.Lens' UpdateWirelessDevice (Prelude.Maybe LoRaWANUpdateDevice)
updateWirelessDevice_loRaWAN = Lens.lens (\UpdateWirelessDevice' {loRaWAN} -> loRaWAN) (\s@UpdateWirelessDevice' {} a -> s {loRaWAN = a} :: UpdateWirelessDevice)

-- | The name of the new destination for the device.
updateWirelessDevice_destinationName :: Lens.Lens' UpdateWirelessDevice (Prelude.Maybe Prelude.Text)
updateWirelessDevice_destinationName = Lens.lens (\UpdateWirelessDevice' {destinationName} -> destinationName) (\s@UpdateWirelessDevice' {} a -> s {destinationName = a} :: UpdateWirelessDevice)

-- | A new description of the resource.
updateWirelessDevice_description :: Lens.Lens' UpdateWirelessDevice (Prelude.Maybe Prelude.Text)
updateWirelessDevice_description = Lens.lens (\UpdateWirelessDevice' {description} -> description) (\s@UpdateWirelessDevice' {} a -> s {description = a} :: UpdateWirelessDevice)

-- | The ID of the resource to update.
updateWirelessDevice_id :: Lens.Lens' UpdateWirelessDevice Prelude.Text
updateWirelessDevice_id = Lens.lens (\UpdateWirelessDevice' {id} -> id) (\s@UpdateWirelessDevice' {} a -> s {id = a} :: UpdateWirelessDevice)

instance Core.AWSRequest UpdateWirelessDevice where
  type
    AWSResponse UpdateWirelessDevice =
      UpdateWirelessDeviceResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateWirelessDeviceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateWirelessDevice where
  hashWithSalt _salt UpdateWirelessDevice' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` loRaWAN
      `Prelude.hashWithSalt` destinationName
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id

instance Prelude.NFData UpdateWirelessDevice where
  rnf UpdateWirelessDevice' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf loRaWAN
      `Prelude.seq` Prelude.rnf destinationName
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders UpdateWirelessDevice where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdateWirelessDevice where
  toJSON UpdateWirelessDevice' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            ("LoRaWAN" Data..=) Prelude.<$> loRaWAN,
            ("DestinationName" Data..=)
              Prelude.<$> destinationName,
            ("Description" Data..=) Prelude.<$> description
          ]
      )

instance Data.ToPath UpdateWirelessDevice where
  toPath UpdateWirelessDevice' {..} =
    Prelude.mconcat
      ["/wireless-devices/", Data.toBS id]

instance Data.ToQuery UpdateWirelessDevice where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateWirelessDeviceResponse' smart constructor.
data UpdateWirelessDeviceResponse = UpdateWirelessDeviceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateWirelessDeviceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateWirelessDeviceResponse_httpStatus' - The response's http status code.
newUpdateWirelessDeviceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateWirelessDeviceResponse
newUpdateWirelessDeviceResponse pHttpStatus_ =
  UpdateWirelessDeviceResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateWirelessDeviceResponse_httpStatus :: Lens.Lens' UpdateWirelessDeviceResponse Prelude.Int
updateWirelessDeviceResponse_httpStatus = Lens.lens (\UpdateWirelessDeviceResponse' {httpStatus} -> httpStatus) (\s@UpdateWirelessDeviceResponse' {} a -> s {httpStatus = a} :: UpdateWirelessDeviceResponse)

instance Prelude.NFData UpdateWirelessDeviceResponse where
  rnf UpdateWirelessDeviceResponse' {..} =
    Prelude.rnf httpStatus
