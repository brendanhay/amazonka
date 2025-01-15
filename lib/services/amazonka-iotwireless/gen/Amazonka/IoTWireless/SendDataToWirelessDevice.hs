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
-- Module      : Amazonka.IoTWireless.SendDataToWirelessDevice
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends a decrypted application data frame to a device.
module Amazonka.IoTWireless.SendDataToWirelessDevice
  ( -- * Creating a Request
    SendDataToWirelessDevice (..),
    newSendDataToWirelessDevice,

    -- * Request Lenses
    sendDataToWirelessDevice_wirelessMetadata,
    sendDataToWirelessDevice_id,
    sendDataToWirelessDevice_transmitMode,
    sendDataToWirelessDevice_payloadData,

    -- * Destructuring the Response
    SendDataToWirelessDeviceResponse (..),
    newSendDataToWirelessDeviceResponse,

    -- * Response Lenses
    sendDataToWirelessDeviceResponse_messageId,
    sendDataToWirelessDeviceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSendDataToWirelessDevice' smart constructor.
data SendDataToWirelessDevice = SendDataToWirelessDevice'
  { -- | Metadata about the message request.
    wirelessMetadata :: Prelude.Maybe WirelessMetadata,
    -- | The ID of the wireless device to receive the data.
    id :: Prelude.Text,
    -- | The transmit mode to use to send data to the wireless device. Can be:
    -- @0@ for UM (unacknowledge mode) or @1@ for AM (acknowledge mode).
    transmitMode :: Prelude.Natural,
    payloadData :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendDataToWirelessDevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'wirelessMetadata', 'sendDataToWirelessDevice_wirelessMetadata' - Metadata about the message request.
--
-- 'id', 'sendDataToWirelessDevice_id' - The ID of the wireless device to receive the data.
--
-- 'transmitMode', 'sendDataToWirelessDevice_transmitMode' - The transmit mode to use to send data to the wireless device. Can be:
-- @0@ for UM (unacknowledge mode) or @1@ for AM (acknowledge mode).
--
-- 'payloadData', 'sendDataToWirelessDevice_payloadData' - Undocumented member.
newSendDataToWirelessDevice ::
  -- | 'id'
  Prelude.Text ->
  -- | 'transmitMode'
  Prelude.Natural ->
  -- | 'payloadData'
  Prelude.Text ->
  SendDataToWirelessDevice
newSendDataToWirelessDevice
  pId_
  pTransmitMode_
  pPayloadData_ =
    SendDataToWirelessDevice'
      { wirelessMetadata =
          Prelude.Nothing,
        id = pId_,
        transmitMode = pTransmitMode_,
        payloadData = pPayloadData_
      }

-- | Metadata about the message request.
sendDataToWirelessDevice_wirelessMetadata :: Lens.Lens' SendDataToWirelessDevice (Prelude.Maybe WirelessMetadata)
sendDataToWirelessDevice_wirelessMetadata = Lens.lens (\SendDataToWirelessDevice' {wirelessMetadata} -> wirelessMetadata) (\s@SendDataToWirelessDevice' {} a -> s {wirelessMetadata = a} :: SendDataToWirelessDevice)

-- | The ID of the wireless device to receive the data.
sendDataToWirelessDevice_id :: Lens.Lens' SendDataToWirelessDevice Prelude.Text
sendDataToWirelessDevice_id = Lens.lens (\SendDataToWirelessDevice' {id} -> id) (\s@SendDataToWirelessDevice' {} a -> s {id = a} :: SendDataToWirelessDevice)

-- | The transmit mode to use to send data to the wireless device. Can be:
-- @0@ for UM (unacknowledge mode) or @1@ for AM (acknowledge mode).
sendDataToWirelessDevice_transmitMode :: Lens.Lens' SendDataToWirelessDevice Prelude.Natural
sendDataToWirelessDevice_transmitMode = Lens.lens (\SendDataToWirelessDevice' {transmitMode} -> transmitMode) (\s@SendDataToWirelessDevice' {} a -> s {transmitMode = a} :: SendDataToWirelessDevice)

-- | Undocumented member.
sendDataToWirelessDevice_payloadData :: Lens.Lens' SendDataToWirelessDevice Prelude.Text
sendDataToWirelessDevice_payloadData = Lens.lens (\SendDataToWirelessDevice' {payloadData} -> payloadData) (\s@SendDataToWirelessDevice' {} a -> s {payloadData = a} :: SendDataToWirelessDevice)

instance Core.AWSRequest SendDataToWirelessDevice where
  type
    AWSResponse SendDataToWirelessDevice =
      SendDataToWirelessDeviceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SendDataToWirelessDeviceResponse'
            Prelude.<$> (x Data..?> "MessageId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SendDataToWirelessDevice where
  hashWithSalt _salt SendDataToWirelessDevice' {..} =
    _salt
      `Prelude.hashWithSalt` wirelessMetadata
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` transmitMode
      `Prelude.hashWithSalt` payloadData

instance Prelude.NFData SendDataToWirelessDevice where
  rnf SendDataToWirelessDevice' {..} =
    Prelude.rnf wirelessMetadata `Prelude.seq`
      Prelude.rnf id `Prelude.seq`
        Prelude.rnf transmitMode `Prelude.seq`
          Prelude.rnf payloadData

instance Data.ToHeaders SendDataToWirelessDevice where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON SendDataToWirelessDevice where
  toJSON SendDataToWirelessDevice' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("WirelessMetadata" Data..=)
              Prelude.<$> wirelessMetadata,
            Prelude.Just ("TransmitMode" Data..= transmitMode),
            Prelude.Just ("PayloadData" Data..= payloadData)
          ]
      )

instance Data.ToPath SendDataToWirelessDevice where
  toPath SendDataToWirelessDevice' {..} =
    Prelude.mconcat
      ["/wireless-devices/", Data.toBS id, "/data"]

instance Data.ToQuery SendDataToWirelessDevice where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSendDataToWirelessDeviceResponse' smart constructor.
data SendDataToWirelessDeviceResponse = SendDataToWirelessDeviceResponse'
  { -- | The ID of the message sent to the wireless device.
    messageId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendDataToWirelessDeviceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'messageId', 'sendDataToWirelessDeviceResponse_messageId' - The ID of the message sent to the wireless device.
--
-- 'httpStatus', 'sendDataToWirelessDeviceResponse_httpStatus' - The response's http status code.
newSendDataToWirelessDeviceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SendDataToWirelessDeviceResponse
newSendDataToWirelessDeviceResponse pHttpStatus_ =
  SendDataToWirelessDeviceResponse'
    { messageId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the message sent to the wireless device.
sendDataToWirelessDeviceResponse_messageId :: Lens.Lens' SendDataToWirelessDeviceResponse (Prelude.Maybe Prelude.Text)
sendDataToWirelessDeviceResponse_messageId = Lens.lens (\SendDataToWirelessDeviceResponse' {messageId} -> messageId) (\s@SendDataToWirelessDeviceResponse' {} a -> s {messageId = a} :: SendDataToWirelessDeviceResponse)

-- | The response's http status code.
sendDataToWirelessDeviceResponse_httpStatus :: Lens.Lens' SendDataToWirelessDeviceResponse Prelude.Int
sendDataToWirelessDeviceResponse_httpStatus = Lens.lens (\SendDataToWirelessDeviceResponse' {httpStatus} -> httpStatus) (\s@SendDataToWirelessDeviceResponse' {} a -> s {httpStatus = a} :: SendDataToWirelessDeviceResponse)

instance
  Prelude.NFData
    SendDataToWirelessDeviceResponse
  where
  rnf SendDataToWirelessDeviceResponse' {..} =
    Prelude.rnf messageId `Prelude.seq`
      Prelude.rnf httpStatus
