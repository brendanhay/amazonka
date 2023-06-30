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
-- Module      : Amazonka.EC2InstanceConnect.SendSerialConsoleSSHPublicKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Pushes an SSH public key to the specified EC2 instance. The key remains
-- for 60 seconds, which gives you 60 seconds to establish a serial console
-- connection to the instance using SSH. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-serial-console.html EC2 Serial Console>
-- in the /Amazon EC2 User Guide/.
module Amazonka.EC2InstanceConnect.SendSerialConsoleSSHPublicKey
  ( -- * Creating a Request
    SendSerialConsoleSSHPublicKey (..),
    newSendSerialConsoleSSHPublicKey,

    -- * Request Lenses
    sendSerialConsoleSSHPublicKey_serialPort,
    sendSerialConsoleSSHPublicKey_instanceId,
    sendSerialConsoleSSHPublicKey_sSHPublicKey,

    -- * Destructuring the Response
    SendSerialConsoleSSHPublicKeyResponse (..),
    newSendSerialConsoleSSHPublicKeyResponse,

    -- * Response Lenses
    sendSerialConsoleSSHPublicKeyResponse_requestId,
    sendSerialConsoleSSHPublicKeyResponse_success,
    sendSerialConsoleSSHPublicKeyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2InstanceConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSendSerialConsoleSSHPublicKey' smart constructor.
data SendSerialConsoleSSHPublicKey = SendSerialConsoleSSHPublicKey'
  { -- | The serial port of the EC2 instance. Currently only port 0 is supported.
    --
    -- Default: 0
    serialPort :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the EC2 instance.
    instanceId :: Prelude.Text,
    -- | The public key material. To use the public key, you must have the
    -- matching private key. For information about the supported key formats
    -- and lengths, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html#how-to-generate-your-own-key-and-import-it-to-aws Requirements for key pairs>
    -- in the /Amazon EC2 User Guide/.
    sSHPublicKey :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendSerialConsoleSSHPublicKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serialPort', 'sendSerialConsoleSSHPublicKey_serialPort' - The serial port of the EC2 instance. Currently only port 0 is supported.
--
-- Default: 0
--
-- 'instanceId', 'sendSerialConsoleSSHPublicKey_instanceId' - The ID of the EC2 instance.
--
-- 'sSHPublicKey', 'sendSerialConsoleSSHPublicKey_sSHPublicKey' - The public key material. To use the public key, you must have the
-- matching private key. For information about the supported key formats
-- and lengths, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html#how-to-generate-your-own-key-and-import-it-to-aws Requirements for key pairs>
-- in the /Amazon EC2 User Guide/.
newSendSerialConsoleSSHPublicKey ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'sSHPublicKey'
  Prelude.Text ->
  SendSerialConsoleSSHPublicKey
newSendSerialConsoleSSHPublicKey
  pInstanceId_
  pSSHPublicKey_ =
    SendSerialConsoleSSHPublicKey'
      { serialPort =
          Prelude.Nothing,
        instanceId = pInstanceId_,
        sSHPublicKey = pSSHPublicKey_
      }

-- | The serial port of the EC2 instance. Currently only port 0 is supported.
--
-- Default: 0
sendSerialConsoleSSHPublicKey_serialPort :: Lens.Lens' SendSerialConsoleSSHPublicKey (Prelude.Maybe Prelude.Natural)
sendSerialConsoleSSHPublicKey_serialPort = Lens.lens (\SendSerialConsoleSSHPublicKey' {serialPort} -> serialPort) (\s@SendSerialConsoleSSHPublicKey' {} a -> s {serialPort = a} :: SendSerialConsoleSSHPublicKey)

-- | The ID of the EC2 instance.
sendSerialConsoleSSHPublicKey_instanceId :: Lens.Lens' SendSerialConsoleSSHPublicKey Prelude.Text
sendSerialConsoleSSHPublicKey_instanceId = Lens.lens (\SendSerialConsoleSSHPublicKey' {instanceId} -> instanceId) (\s@SendSerialConsoleSSHPublicKey' {} a -> s {instanceId = a} :: SendSerialConsoleSSHPublicKey)

-- | The public key material. To use the public key, you must have the
-- matching private key. For information about the supported key formats
-- and lengths, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html#how-to-generate-your-own-key-and-import-it-to-aws Requirements for key pairs>
-- in the /Amazon EC2 User Guide/.
sendSerialConsoleSSHPublicKey_sSHPublicKey :: Lens.Lens' SendSerialConsoleSSHPublicKey Prelude.Text
sendSerialConsoleSSHPublicKey_sSHPublicKey = Lens.lens (\SendSerialConsoleSSHPublicKey' {sSHPublicKey} -> sSHPublicKey) (\s@SendSerialConsoleSSHPublicKey' {} a -> s {sSHPublicKey = a} :: SendSerialConsoleSSHPublicKey)

instance
  Core.AWSRequest
    SendSerialConsoleSSHPublicKey
  where
  type
    AWSResponse SendSerialConsoleSSHPublicKey =
      SendSerialConsoleSSHPublicKeyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SendSerialConsoleSSHPublicKeyResponse'
            Prelude.<$> (x Data..?> "RequestId")
            Prelude.<*> (x Data..?> "Success")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    SendSerialConsoleSSHPublicKey
  where
  hashWithSalt _salt SendSerialConsoleSSHPublicKey' {..} =
    _salt
      `Prelude.hashWithSalt` serialPort
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` sSHPublicKey

instance Prelude.NFData SendSerialConsoleSSHPublicKey where
  rnf SendSerialConsoleSSHPublicKey' {..} =
    Prelude.rnf serialPort
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf sSHPublicKey

instance Data.ToHeaders SendSerialConsoleSSHPublicKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSEC2InstanceConnectService.SendSerialConsoleSSHPublicKey" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SendSerialConsoleSSHPublicKey where
  toJSON SendSerialConsoleSSHPublicKey' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SerialPort" Data..=) Prelude.<$> serialPort,
            Prelude.Just ("InstanceId" Data..= instanceId),
            Prelude.Just ("SSHPublicKey" Data..= sSHPublicKey)
          ]
      )

instance Data.ToPath SendSerialConsoleSSHPublicKey where
  toPath = Prelude.const "/"

instance Data.ToQuery SendSerialConsoleSSHPublicKey where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSendSerialConsoleSSHPublicKeyResponse' smart constructor.
data SendSerialConsoleSSHPublicKeyResponse = SendSerialConsoleSSHPublicKeyResponse'
  { -- | The ID of the request. Please provide this ID when contacting AWS
    -- Support for assistance.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | Is true if the request succeeds and an error otherwise.
    success :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendSerialConsoleSSHPublicKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestId', 'sendSerialConsoleSSHPublicKeyResponse_requestId' - The ID of the request. Please provide this ID when contacting AWS
-- Support for assistance.
--
-- 'success', 'sendSerialConsoleSSHPublicKeyResponse_success' - Is true if the request succeeds and an error otherwise.
--
-- 'httpStatus', 'sendSerialConsoleSSHPublicKeyResponse_httpStatus' - The response's http status code.
newSendSerialConsoleSSHPublicKeyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SendSerialConsoleSSHPublicKeyResponse
newSendSerialConsoleSSHPublicKeyResponse pHttpStatus_ =
  SendSerialConsoleSSHPublicKeyResponse'
    { requestId =
        Prelude.Nothing,
      success = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the request. Please provide this ID when contacting AWS
-- Support for assistance.
sendSerialConsoleSSHPublicKeyResponse_requestId :: Lens.Lens' SendSerialConsoleSSHPublicKeyResponse (Prelude.Maybe Prelude.Text)
sendSerialConsoleSSHPublicKeyResponse_requestId = Lens.lens (\SendSerialConsoleSSHPublicKeyResponse' {requestId} -> requestId) (\s@SendSerialConsoleSSHPublicKeyResponse' {} a -> s {requestId = a} :: SendSerialConsoleSSHPublicKeyResponse)

-- | Is true if the request succeeds and an error otherwise.
sendSerialConsoleSSHPublicKeyResponse_success :: Lens.Lens' SendSerialConsoleSSHPublicKeyResponse (Prelude.Maybe Prelude.Bool)
sendSerialConsoleSSHPublicKeyResponse_success = Lens.lens (\SendSerialConsoleSSHPublicKeyResponse' {success} -> success) (\s@SendSerialConsoleSSHPublicKeyResponse' {} a -> s {success = a} :: SendSerialConsoleSSHPublicKeyResponse)

-- | The response's http status code.
sendSerialConsoleSSHPublicKeyResponse_httpStatus :: Lens.Lens' SendSerialConsoleSSHPublicKeyResponse Prelude.Int
sendSerialConsoleSSHPublicKeyResponse_httpStatus = Lens.lens (\SendSerialConsoleSSHPublicKeyResponse' {httpStatus} -> httpStatus) (\s@SendSerialConsoleSSHPublicKeyResponse' {} a -> s {httpStatus = a} :: SendSerialConsoleSSHPublicKeyResponse)

instance
  Prelude.NFData
    SendSerialConsoleSSHPublicKeyResponse
  where
  rnf SendSerialConsoleSSHPublicKeyResponse' {..} =
    Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf success
      `Prelude.seq` Prelude.rnf httpStatus
