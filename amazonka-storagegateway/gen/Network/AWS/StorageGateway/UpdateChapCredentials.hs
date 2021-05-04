{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.StorageGateway.UpdateChapCredentials
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the Challenge-Handshake Authentication Protocol (CHAP)
-- credentials for a specified iSCSI target. By default, a gateway does not
-- have CHAP enabled; however, for added security, you might use it. This
-- operation is supported in the volume and tape gateway types.
--
-- When you update CHAP credentials, all existing connections on the target
-- are closed and initiators must reconnect with the new credentials.
module Network.AWS.StorageGateway.UpdateChapCredentials
  ( -- * Creating a Request
    UpdateChapCredentials (..),
    newUpdateChapCredentials,

    -- * Request Lenses
    updateChapCredentials_secretToAuthenticateTarget,
    updateChapCredentials_targetARN,
    updateChapCredentials_secretToAuthenticateInitiator,
    updateChapCredentials_initiatorName,

    -- * Destructuring the Response
    UpdateChapCredentialsResponse (..),
    newUpdateChapCredentialsResponse,

    -- * Response Lenses
    updateChapCredentialsResponse_initiatorName,
    updateChapCredentialsResponse_targetARN,
    updateChapCredentialsResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | A JSON object containing one or more of the following fields:
--
-- -   UpdateChapCredentialsInput$InitiatorName
--
-- -   UpdateChapCredentialsInput$SecretToAuthenticateInitiator
--
-- -   UpdateChapCredentialsInput$SecretToAuthenticateTarget
--
-- -   UpdateChapCredentialsInput$TargetARN
--
-- /See:/ 'newUpdateChapCredentials' smart constructor.
data UpdateChapCredentials = UpdateChapCredentials'
  { -- | The secret key that the target must provide to participate in mutual
    -- CHAP with the initiator (e.g. Windows client).
    --
    -- Byte constraints: Minimum bytes of 12. Maximum bytes of 16.
    --
    -- The secret key must be between 12 and 16 bytes when encoded in UTF-8.
    secretToAuthenticateTarget :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the iSCSI volume target. Use the
    -- DescribeStorediSCSIVolumes operation to return the TargetARN for
    -- specified VolumeARN.
    targetARN :: Prelude.Text,
    -- | The secret key that the initiator (for example, the Windows client) must
    -- provide to participate in mutual CHAP with the target.
    --
    -- The secret key must be between 12 and 16 bytes when encoded in UTF-8.
    secretToAuthenticateInitiator :: Prelude.Sensitive Prelude.Text,
    -- | The iSCSI initiator that connects to the target.
    initiatorName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateChapCredentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'secretToAuthenticateTarget', 'updateChapCredentials_secretToAuthenticateTarget' - The secret key that the target must provide to participate in mutual
-- CHAP with the initiator (e.g. Windows client).
--
-- Byte constraints: Minimum bytes of 12. Maximum bytes of 16.
--
-- The secret key must be between 12 and 16 bytes when encoded in UTF-8.
--
-- 'targetARN', 'updateChapCredentials_targetARN' - The Amazon Resource Name (ARN) of the iSCSI volume target. Use the
-- DescribeStorediSCSIVolumes operation to return the TargetARN for
-- specified VolumeARN.
--
-- 'secretToAuthenticateInitiator', 'updateChapCredentials_secretToAuthenticateInitiator' - The secret key that the initiator (for example, the Windows client) must
-- provide to participate in mutual CHAP with the target.
--
-- The secret key must be between 12 and 16 bytes when encoded in UTF-8.
--
-- 'initiatorName', 'updateChapCredentials_initiatorName' - The iSCSI initiator that connects to the target.
newUpdateChapCredentials ::
  -- | 'targetARN'
  Prelude.Text ->
  -- | 'secretToAuthenticateInitiator'
  Prelude.Text ->
  -- | 'initiatorName'
  Prelude.Text ->
  UpdateChapCredentials
newUpdateChapCredentials
  pTargetARN_
  pSecretToAuthenticateInitiator_
  pInitiatorName_ =
    UpdateChapCredentials'
      { secretToAuthenticateTarget =
          Prelude.Nothing,
        targetARN = pTargetARN_,
        secretToAuthenticateInitiator =
          Prelude._Sensitive
            Lens.# pSecretToAuthenticateInitiator_,
        initiatorName = pInitiatorName_
      }

-- | The secret key that the target must provide to participate in mutual
-- CHAP with the initiator (e.g. Windows client).
--
-- Byte constraints: Minimum bytes of 12. Maximum bytes of 16.
--
-- The secret key must be between 12 and 16 bytes when encoded in UTF-8.
updateChapCredentials_secretToAuthenticateTarget :: Lens.Lens' UpdateChapCredentials (Prelude.Maybe Prelude.Text)
updateChapCredentials_secretToAuthenticateTarget = Lens.lens (\UpdateChapCredentials' {secretToAuthenticateTarget} -> secretToAuthenticateTarget) (\s@UpdateChapCredentials' {} a -> s {secretToAuthenticateTarget = a} :: UpdateChapCredentials) Prelude.. Lens.mapping Prelude._Sensitive

-- | The Amazon Resource Name (ARN) of the iSCSI volume target. Use the
-- DescribeStorediSCSIVolumes operation to return the TargetARN for
-- specified VolumeARN.
updateChapCredentials_targetARN :: Lens.Lens' UpdateChapCredentials Prelude.Text
updateChapCredentials_targetARN = Lens.lens (\UpdateChapCredentials' {targetARN} -> targetARN) (\s@UpdateChapCredentials' {} a -> s {targetARN = a} :: UpdateChapCredentials)

-- | The secret key that the initiator (for example, the Windows client) must
-- provide to participate in mutual CHAP with the target.
--
-- The secret key must be between 12 and 16 bytes when encoded in UTF-8.
updateChapCredentials_secretToAuthenticateInitiator :: Lens.Lens' UpdateChapCredentials Prelude.Text
updateChapCredentials_secretToAuthenticateInitiator = Lens.lens (\UpdateChapCredentials' {secretToAuthenticateInitiator} -> secretToAuthenticateInitiator) (\s@UpdateChapCredentials' {} a -> s {secretToAuthenticateInitiator = a} :: UpdateChapCredentials) Prelude.. Prelude._Sensitive

-- | The iSCSI initiator that connects to the target.
updateChapCredentials_initiatorName :: Lens.Lens' UpdateChapCredentials Prelude.Text
updateChapCredentials_initiatorName = Lens.lens (\UpdateChapCredentials' {initiatorName} -> initiatorName) (\s@UpdateChapCredentials' {} a -> s {initiatorName = a} :: UpdateChapCredentials)

instance Prelude.AWSRequest UpdateChapCredentials where
  type
    Rs UpdateChapCredentials =
      UpdateChapCredentialsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateChapCredentialsResponse'
            Prelude.<$> (x Prelude..?> "InitiatorName")
            Prelude.<*> (x Prelude..?> "TargetARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateChapCredentials

instance Prelude.NFData UpdateChapCredentials

instance Prelude.ToHeaders UpdateChapCredentials where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "StorageGateway_20130630.UpdateChapCredentials" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateChapCredentials where
  toJSON UpdateChapCredentials' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("SecretToAuthenticateTarget" Prelude..=)
              Prelude.<$> secretToAuthenticateTarget,
            Prelude.Just ("TargetARN" Prelude..= targetARN),
            Prelude.Just
              ( "SecretToAuthenticateInitiator"
                  Prelude..= secretToAuthenticateInitiator
              ),
            Prelude.Just
              ("InitiatorName" Prelude..= initiatorName)
          ]
      )

instance Prelude.ToPath UpdateChapCredentials where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateChapCredentials where
  toQuery = Prelude.const Prelude.mempty

-- | A JSON object containing the following fields:
--
-- /See:/ 'newUpdateChapCredentialsResponse' smart constructor.
data UpdateChapCredentialsResponse = UpdateChapCredentialsResponse'
  { -- | The iSCSI initiator that connects to the target. This is the same
    -- initiator name specified in the request.
    initiatorName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the target. This is the same target
    -- specified in the request.
    targetARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateChapCredentialsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'initiatorName', 'updateChapCredentialsResponse_initiatorName' - The iSCSI initiator that connects to the target. This is the same
-- initiator name specified in the request.
--
-- 'targetARN', 'updateChapCredentialsResponse_targetARN' - The Amazon Resource Name (ARN) of the target. This is the same target
-- specified in the request.
--
-- 'httpStatus', 'updateChapCredentialsResponse_httpStatus' - The response's http status code.
newUpdateChapCredentialsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateChapCredentialsResponse
newUpdateChapCredentialsResponse pHttpStatus_ =
  UpdateChapCredentialsResponse'
    { initiatorName =
        Prelude.Nothing,
      targetARN = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The iSCSI initiator that connects to the target. This is the same
-- initiator name specified in the request.
updateChapCredentialsResponse_initiatorName :: Lens.Lens' UpdateChapCredentialsResponse (Prelude.Maybe Prelude.Text)
updateChapCredentialsResponse_initiatorName = Lens.lens (\UpdateChapCredentialsResponse' {initiatorName} -> initiatorName) (\s@UpdateChapCredentialsResponse' {} a -> s {initiatorName = a} :: UpdateChapCredentialsResponse)

-- | The Amazon Resource Name (ARN) of the target. This is the same target
-- specified in the request.
updateChapCredentialsResponse_targetARN :: Lens.Lens' UpdateChapCredentialsResponse (Prelude.Maybe Prelude.Text)
updateChapCredentialsResponse_targetARN = Lens.lens (\UpdateChapCredentialsResponse' {targetARN} -> targetARN) (\s@UpdateChapCredentialsResponse' {} a -> s {targetARN = a} :: UpdateChapCredentialsResponse)

-- | The response's http status code.
updateChapCredentialsResponse_httpStatus :: Lens.Lens' UpdateChapCredentialsResponse Prelude.Int
updateChapCredentialsResponse_httpStatus = Lens.lens (\UpdateChapCredentialsResponse' {httpStatus} -> httpStatus) (\s@UpdateChapCredentialsResponse' {} a -> s {httpStatus = a} :: UpdateChapCredentialsResponse)

instance Prelude.NFData UpdateChapCredentialsResponse
