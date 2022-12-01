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
-- Module      : Amazonka.EC2InstanceConnect.SendSSHPublicKey
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Pushes an SSH public key to the specified EC2 instance for use by the
-- specified user. The key remains for 60 seconds. For more information,
-- see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Connect-using-EC2-Instance-Connect.html Connect to your Linux instance using EC2 Instance Connect>
-- in the /Amazon EC2 User Guide/.
module Amazonka.EC2InstanceConnect.SendSSHPublicKey
  ( -- * Creating a Request
    SendSSHPublicKey (..),
    newSendSSHPublicKey,

    -- * Request Lenses
    sendSSHPublicKey_availabilityZone,
    sendSSHPublicKey_instanceId,
    sendSSHPublicKey_instanceOSUser,
    sendSSHPublicKey_sSHPublicKey,

    -- * Destructuring the Response
    SendSSHPublicKeyResponse (..),
    newSendSSHPublicKeyResponse,

    -- * Response Lenses
    sendSSHPublicKeyResponse_requestId,
    sendSSHPublicKeyResponse_success,
    sendSSHPublicKeyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2InstanceConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSendSSHPublicKey' smart constructor.
data SendSSHPublicKey = SendSSHPublicKey'
  { -- | The Availability Zone in which the EC2 instance was launched.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The ID of the EC2 instance.
    instanceId :: Prelude.Text,
    -- | The OS user on the EC2 instance for whom the key can be used to
    -- authenticate.
    instanceOSUser :: Prelude.Text,
    -- | The public key material. To use the public key, you must have the
    -- matching private key.
    sSHPublicKey :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendSSHPublicKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZone', 'sendSSHPublicKey_availabilityZone' - The Availability Zone in which the EC2 instance was launched.
--
-- 'instanceId', 'sendSSHPublicKey_instanceId' - The ID of the EC2 instance.
--
-- 'instanceOSUser', 'sendSSHPublicKey_instanceOSUser' - The OS user on the EC2 instance for whom the key can be used to
-- authenticate.
--
-- 'sSHPublicKey', 'sendSSHPublicKey_sSHPublicKey' - The public key material. To use the public key, you must have the
-- matching private key.
newSendSSHPublicKey ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'instanceOSUser'
  Prelude.Text ->
  -- | 'sSHPublicKey'
  Prelude.Text ->
  SendSSHPublicKey
newSendSSHPublicKey
  pInstanceId_
  pInstanceOSUser_
  pSSHPublicKey_ =
    SendSSHPublicKey'
      { availabilityZone =
          Prelude.Nothing,
        instanceId = pInstanceId_,
        instanceOSUser = pInstanceOSUser_,
        sSHPublicKey = pSSHPublicKey_
      }

-- | The Availability Zone in which the EC2 instance was launched.
sendSSHPublicKey_availabilityZone :: Lens.Lens' SendSSHPublicKey (Prelude.Maybe Prelude.Text)
sendSSHPublicKey_availabilityZone = Lens.lens (\SendSSHPublicKey' {availabilityZone} -> availabilityZone) (\s@SendSSHPublicKey' {} a -> s {availabilityZone = a} :: SendSSHPublicKey)

-- | The ID of the EC2 instance.
sendSSHPublicKey_instanceId :: Lens.Lens' SendSSHPublicKey Prelude.Text
sendSSHPublicKey_instanceId = Lens.lens (\SendSSHPublicKey' {instanceId} -> instanceId) (\s@SendSSHPublicKey' {} a -> s {instanceId = a} :: SendSSHPublicKey)

-- | The OS user on the EC2 instance for whom the key can be used to
-- authenticate.
sendSSHPublicKey_instanceOSUser :: Lens.Lens' SendSSHPublicKey Prelude.Text
sendSSHPublicKey_instanceOSUser = Lens.lens (\SendSSHPublicKey' {instanceOSUser} -> instanceOSUser) (\s@SendSSHPublicKey' {} a -> s {instanceOSUser = a} :: SendSSHPublicKey)

-- | The public key material. To use the public key, you must have the
-- matching private key.
sendSSHPublicKey_sSHPublicKey :: Lens.Lens' SendSSHPublicKey Prelude.Text
sendSSHPublicKey_sSHPublicKey = Lens.lens (\SendSSHPublicKey' {sSHPublicKey} -> sSHPublicKey) (\s@SendSSHPublicKey' {} a -> s {sSHPublicKey = a} :: SendSSHPublicKey)

instance Core.AWSRequest SendSSHPublicKey where
  type
    AWSResponse SendSSHPublicKey =
      SendSSHPublicKeyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SendSSHPublicKeyResponse'
            Prelude.<$> (x Core..?> "RequestId")
            Prelude.<*> (x Core..?> "Success")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SendSSHPublicKey where
  hashWithSalt _salt SendSSHPublicKey' {..} =
    _salt `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` instanceOSUser
      `Prelude.hashWithSalt` sSHPublicKey

instance Prelude.NFData SendSSHPublicKey where
  rnf SendSSHPublicKey' {..} =
    Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf instanceOSUser
      `Prelude.seq` Prelude.rnf sSHPublicKey

instance Core.ToHeaders SendSSHPublicKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSEC2InstanceConnectService.SendSSHPublicKey" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON SendSSHPublicKey where
  toJSON SendSSHPublicKey' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AvailabilityZone" Core..=)
              Prelude.<$> availabilityZone,
            Prelude.Just ("InstanceId" Core..= instanceId),
            Prelude.Just
              ("InstanceOSUser" Core..= instanceOSUser),
            Prelude.Just ("SSHPublicKey" Core..= sSHPublicKey)
          ]
      )

instance Core.ToPath SendSSHPublicKey where
  toPath = Prelude.const "/"

instance Core.ToQuery SendSSHPublicKey where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSendSSHPublicKeyResponse' smart constructor.
data SendSSHPublicKeyResponse = SendSSHPublicKeyResponse'
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
-- Create a value of 'SendSSHPublicKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestId', 'sendSSHPublicKeyResponse_requestId' - The ID of the request. Please provide this ID when contacting AWS
-- Support for assistance.
--
-- 'success', 'sendSSHPublicKeyResponse_success' - Is true if the request succeeds and an error otherwise.
--
-- 'httpStatus', 'sendSSHPublicKeyResponse_httpStatus' - The response's http status code.
newSendSSHPublicKeyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SendSSHPublicKeyResponse
newSendSSHPublicKeyResponse pHttpStatus_ =
  SendSSHPublicKeyResponse'
    { requestId =
        Prelude.Nothing,
      success = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the request. Please provide this ID when contacting AWS
-- Support for assistance.
sendSSHPublicKeyResponse_requestId :: Lens.Lens' SendSSHPublicKeyResponse (Prelude.Maybe Prelude.Text)
sendSSHPublicKeyResponse_requestId = Lens.lens (\SendSSHPublicKeyResponse' {requestId} -> requestId) (\s@SendSSHPublicKeyResponse' {} a -> s {requestId = a} :: SendSSHPublicKeyResponse)

-- | Is true if the request succeeds and an error otherwise.
sendSSHPublicKeyResponse_success :: Lens.Lens' SendSSHPublicKeyResponse (Prelude.Maybe Prelude.Bool)
sendSSHPublicKeyResponse_success = Lens.lens (\SendSSHPublicKeyResponse' {success} -> success) (\s@SendSSHPublicKeyResponse' {} a -> s {success = a} :: SendSSHPublicKeyResponse)

-- | The response's http status code.
sendSSHPublicKeyResponse_httpStatus :: Lens.Lens' SendSSHPublicKeyResponse Prelude.Int
sendSSHPublicKeyResponse_httpStatus = Lens.lens (\SendSSHPublicKeyResponse' {httpStatus} -> httpStatus) (\s@SendSSHPublicKeyResponse' {} a -> s {httpStatus = a} :: SendSSHPublicKeyResponse)

instance Prelude.NFData SendSSHPublicKeyResponse where
  rnf SendSSHPublicKeyResponse' {..} =
    Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf success
      `Prelude.seq` Prelude.rnf httpStatus
