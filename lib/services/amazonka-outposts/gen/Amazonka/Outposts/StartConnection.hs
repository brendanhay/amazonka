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
-- Module      : Amazonka.Outposts.StartConnection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon Web Services uses this action to install Outpost servers.
--
-- Starts the connection required for Outpost server installation.
--
-- Use CloudTrail to monitor this action or Amazon Web Services managed
-- policy for Amazon Web Services Outposts to secure it. For more
-- information, see
-- <https://docs.aws.amazon.com/outposts/latest/userguide/security-iam-awsmanpol.html Amazon Web Services managed policies for Amazon Web Services Outposts>
-- and
-- <https://docs.aws.amazon.com/outposts/latest/userguide/logging-using-cloudtrail.html Logging Amazon Web Services Outposts API calls with Amazon Web Services CloudTrail>
-- in the /Amazon Web Services Outposts User Guide/.
module Amazonka.Outposts.StartConnection
  ( -- * Creating a Request
    StartConnection (..),
    newStartConnection,

    -- * Request Lenses
    startConnection_deviceSerialNumber,
    startConnection_assetId,
    startConnection_clientPublicKey,
    startConnection_networkInterfaceDeviceIndex,

    -- * Destructuring the Response
    StartConnectionResponse (..),
    newStartConnectionResponse,

    -- * Response Lenses
    startConnectionResponse_connectionId,
    startConnectionResponse_underlayIpAddress,
    startConnectionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Outposts.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartConnection' smart constructor.
data StartConnection = StartConnection'
  { -- | The serial number of the dongle.
    deviceSerialNumber :: Prelude.Text,
    -- | The ID of the Outpost server.
    assetId :: Prelude.Text,
    -- | The public key of the client.
    clientPublicKey :: Prelude.Text,
    -- | The device index of the network interface on the Outpost server.
    networkInterfaceDeviceIndex :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceSerialNumber', 'startConnection_deviceSerialNumber' - The serial number of the dongle.
--
-- 'assetId', 'startConnection_assetId' - The ID of the Outpost server.
--
-- 'clientPublicKey', 'startConnection_clientPublicKey' - The public key of the client.
--
-- 'networkInterfaceDeviceIndex', 'startConnection_networkInterfaceDeviceIndex' - The device index of the network interface on the Outpost server.
newStartConnection ::
  -- | 'deviceSerialNumber'
  Prelude.Text ->
  -- | 'assetId'
  Prelude.Text ->
  -- | 'clientPublicKey'
  Prelude.Text ->
  -- | 'networkInterfaceDeviceIndex'
  Prelude.Natural ->
  StartConnection
newStartConnection
  pDeviceSerialNumber_
  pAssetId_
  pClientPublicKey_
  pNetworkInterfaceDeviceIndex_ =
    StartConnection'
      { deviceSerialNumber =
          pDeviceSerialNumber_,
        assetId = pAssetId_,
        clientPublicKey = pClientPublicKey_,
        networkInterfaceDeviceIndex =
          pNetworkInterfaceDeviceIndex_
      }

-- | The serial number of the dongle.
startConnection_deviceSerialNumber :: Lens.Lens' StartConnection Prelude.Text
startConnection_deviceSerialNumber = Lens.lens (\StartConnection' {deviceSerialNumber} -> deviceSerialNumber) (\s@StartConnection' {} a -> s {deviceSerialNumber = a} :: StartConnection)

-- | The ID of the Outpost server.
startConnection_assetId :: Lens.Lens' StartConnection Prelude.Text
startConnection_assetId = Lens.lens (\StartConnection' {assetId} -> assetId) (\s@StartConnection' {} a -> s {assetId = a} :: StartConnection)

-- | The public key of the client.
startConnection_clientPublicKey :: Lens.Lens' StartConnection Prelude.Text
startConnection_clientPublicKey = Lens.lens (\StartConnection' {clientPublicKey} -> clientPublicKey) (\s@StartConnection' {} a -> s {clientPublicKey = a} :: StartConnection)

-- | The device index of the network interface on the Outpost server.
startConnection_networkInterfaceDeviceIndex :: Lens.Lens' StartConnection Prelude.Natural
startConnection_networkInterfaceDeviceIndex = Lens.lens (\StartConnection' {networkInterfaceDeviceIndex} -> networkInterfaceDeviceIndex) (\s@StartConnection' {} a -> s {networkInterfaceDeviceIndex = a} :: StartConnection)

instance Core.AWSRequest StartConnection where
  type
    AWSResponse StartConnection =
      StartConnectionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartConnectionResponse'
            Prelude.<$> (x Data..?> "ConnectionId")
            Prelude.<*> (x Data..?> "UnderlayIpAddress")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartConnection where
  hashWithSalt _salt StartConnection' {..} =
    _salt `Prelude.hashWithSalt` deviceSerialNumber
      `Prelude.hashWithSalt` assetId
      `Prelude.hashWithSalt` clientPublicKey
      `Prelude.hashWithSalt` networkInterfaceDeviceIndex

instance Prelude.NFData StartConnection where
  rnf StartConnection' {..} =
    Prelude.rnf deviceSerialNumber
      `Prelude.seq` Prelude.rnf assetId
      `Prelude.seq` Prelude.rnf clientPublicKey
      `Prelude.seq` Prelude.rnf networkInterfaceDeviceIndex

instance Data.ToHeaders StartConnection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartConnection where
  toJSON StartConnection' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("DeviceSerialNumber" Data..= deviceSerialNumber),
            Prelude.Just ("AssetId" Data..= assetId),
            Prelude.Just
              ("ClientPublicKey" Data..= clientPublicKey),
            Prelude.Just
              ( "NetworkInterfaceDeviceIndex"
                  Data..= networkInterfaceDeviceIndex
              )
          ]
      )

instance Data.ToPath StartConnection where
  toPath = Prelude.const "/connections"

instance Data.ToQuery StartConnection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartConnectionResponse' smart constructor.
data StartConnectionResponse = StartConnectionResponse'
  { -- | The ID of the connection.
    connectionId :: Prelude.Maybe Prelude.Text,
    -- | The underlay IP address.
    underlayIpAddress :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionId', 'startConnectionResponse_connectionId' - The ID of the connection.
--
-- 'underlayIpAddress', 'startConnectionResponse_underlayIpAddress' - The underlay IP address.
--
-- 'httpStatus', 'startConnectionResponse_httpStatus' - The response's http status code.
newStartConnectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartConnectionResponse
newStartConnectionResponse pHttpStatus_ =
  StartConnectionResponse'
    { connectionId =
        Prelude.Nothing,
      underlayIpAddress = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the connection.
startConnectionResponse_connectionId :: Lens.Lens' StartConnectionResponse (Prelude.Maybe Prelude.Text)
startConnectionResponse_connectionId = Lens.lens (\StartConnectionResponse' {connectionId} -> connectionId) (\s@StartConnectionResponse' {} a -> s {connectionId = a} :: StartConnectionResponse)

-- | The underlay IP address.
startConnectionResponse_underlayIpAddress :: Lens.Lens' StartConnectionResponse (Prelude.Maybe Prelude.Text)
startConnectionResponse_underlayIpAddress = Lens.lens (\StartConnectionResponse' {underlayIpAddress} -> underlayIpAddress) (\s@StartConnectionResponse' {} a -> s {underlayIpAddress = a} :: StartConnectionResponse)

-- | The response's http status code.
startConnectionResponse_httpStatus :: Lens.Lens' StartConnectionResponse Prelude.Int
startConnectionResponse_httpStatus = Lens.lens (\StartConnectionResponse' {httpStatus} -> httpStatus) (\s@StartConnectionResponse' {} a -> s {httpStatus = a} :: StartConnectionResponse)

instance Prelude.NFData StartConnectionResponse where
  rnf StartConnectionResponse' {..} =
    Prelude.rnf connectionId
      `Prelude.seq` Prelude.rnf underlayIpAddress
      `Prelude.seq` Prelude.rnf httpStatus
