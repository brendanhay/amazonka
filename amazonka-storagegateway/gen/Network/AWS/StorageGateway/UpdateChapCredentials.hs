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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
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
    secretToAuthenticateTarget :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The Amazon Resource Name (ARN) of the iSCSI volume target. Use the
    -- DescribeStorediSCSIVolumes operation to return the TargetARN for
    -- specified VolumeARN.
    targetARN :: Core.Text,
    -- | The secret key that the initiator (for example, the Windows client) must
    -- provide to participate in mutual CHAP with the target.
    --
    -- The secret key must be between 12 and 16 bytes when encoded in UTF-8.
    secretToAuthenticateInitiator :: Core.Sensitive Core.Text,
    -- | The iSCSI initiator that connects to the target.
    initiatorName :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'secretToAuthenticateInitiator'
  Core.Text ->
  -- | 'initiatorName'
  Core.Text ->
  UpdateChapCredentials
newUpdateChapCredentials
  pTargetARN_
  pSecretToAuthenticateInitiator_
  pInitiatorName_ =
    UpdateChapCredentials'
      { secretToAuthenticateTarget =
          Core.Nothing,
        targetARN = pTargetARN_,
        secretToAuthenticateInitiator =
          Core._Sensitive
            Lens.# pSecretToAuthenticateInitiator_,
        initiatorName = pInitiatorName_
      }

-- | The secret key that the target must provide to participate in mutual
-- CHAP with the initiator (e.g. Windows client).
--
-- Byte constraints: Minimum bytes of 12. Maximum bytes of 16.
--
-- The secret key must be between 12 and 16 bytes when encoded in UTF-8.
updateChapCredentials_secretToAuthenticateTarget :: Lens.Lens' UpdateChapCredentials (Core.Maybe Core.Text)
updateChapCredentials_secretToAuthenticateTarget = Lens.lens (\UpdateChapCredentials' {secretToAuthenticateTarget} -> secretToAuthenticateTarget) (\s@UpdateChapCredentials' {} a -> s {secretToAuthenticateTarget = a} :: UpdateChapCredentials) Core.. Lens.mapping Core._Sensitive

-- | The Amazon Resource Name (ARN) of the iSCSI volume target. Use the
-- DescribeStorediSCSIVolumes operation to return the TargetARN for
-- specified VolumeARN.
updateChapCredentials_targetARN :: Lens.Lens' UpdateChapCredentials Core.Text
updateChapCredentials_targetARN = Lens.lens (\UpdateChapCredentials' {targetARN} -> targetARN) (\s@UpdateChapCredentials' {} a -> s {targetARN = a} :: UpdateChapCredentials)

-- | The secret key that the initiator (for example, the Windows client) must
-- provide to participate in mutual CHAP with the target.
--
-- The secret key must be between 12 and 16 bytes when encoded in UTF-8.
updateChapCredentials_secretToAuthenticateInitiator :: Lens.Lens' UpdateChapCredentials Core.Text
updateChapCredentials_secretToAuthenticateInitiator = Lens.lens (\UpdateChapCredentials' {secretToAuthenticateInitiator} -> secretToAuthenticateInitiator) (\s@UpdateChapCredentials' {} a -> s {secretToAuthenticateInitiator = a} :: UpdateChapCredentials) Core.. Core._Sensitive

-- | The iSCSI initiator that connects to the target.
updateChapCredentials_initiatorName :: Lens.Lens' UpdateChapCredentials Core.Text
updateChapCredentials_initiatorName = Lens.lens (\UpdateChapCredentials' {initiatorName} -> initiatorName) (\s@UpdateChapCredentials' {} a -> s {initiatorName = a} :: UpdateChapCredentials)

instance Core.AWSRequest UpdateChapCredentials where
  type
    AWSResponse UpdateChapCredentials =
      UpdateChapCredentialsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateChapCredentialsResponse'
            Core.<$> (x Core..?> "InitiatorName")
            Core.<*> (x Core..?> "TargetARN")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateChapCredentials

instance Core.NFData UpdateChapCredentials

instance Core.ToHeaders UpdateChapCredentials where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.UpdateChapCredentials" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateChapCredentials where
  toJSON UpdateChapCredentials' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SecretToAuthenticateTarget" Core..=)
              Core.<$> secretToAuthenticateTarget,
            Core.Just ("TargetARN" Core..= targetARN),
            Core.Just
              ( "SecretToAuthenticateInitiator"
                  Core..= secretToAuthenticateInitiator
              ),
            Core.Just ("InitiatorName" Core..= initiatorName)
          ]
      )

instance Core.ToPath UpdateChapCredentials where
  toPath = Core.const "/"

instance Core.ToQuery UpdateChapCredentials where
  toQuery = Core.const Core.mempty

-- | A JSON object containing the following fields:
--
-- /See:/ 'newUpdateChapCredentialsResponse' smart constructor.
data UpdateChapCredentialsResponse = UpdateChapCredentialsResponse'
  { -- | The iSCSI initiator that connects to the target. This is the same
    -- initiator name specified in the request.
    initiatorName :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the target. This is the same target
    -- specified in the request.
    targetARN :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  UpdateChapCredentialsResponse
newUpdateChapCredentialsResponse pHttpStatus_ =
  UpdateChapCredentialsResponse'
    { initiatorName =
        Core.Nothing,
      targetARN = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The iSCSI initiator that connects to the target. This is the same
-- initiator name specified in the request.
updateChapCredentialsResponse_initiatorName :: Lens.Lens' UpdateChapCredentialsResponse (Core.Maybe Core.Text)
updateChapCredentialsResponse_initiatorName = Lens.lens (\UpdateChapCredentialsResponse' {initiatorName} -> initiatorName) (\s@UpdateChapCredentialsResponse' {} a -> s {initiatorName = a} :: UpdateChapCredentialsResponse)

-- | The Amazon Resource Name (ARN) of the target. This is the same target
-- specified in the request.
updateChapCredentialsResponse_targetARN :: Lens.Lens' UpdateChapCredentialsResponse (Core.Maybe Core.Text)
updateChapCredentialsResponse_targetARN = Lens.lens (\UpdateChapCredentialsResponse' {targetARN} -> targetARN) (\s@UpdateChapCredentialsResponse' {} a -> s {targetARN = a} :: UpdateChapCredentialsResponse)

-- | The response's http status code.
updateChapCredentialsResponse_httpStatus :: Lens.Lens' UpdateChapCredentialsResponse Core.Int
updateChapCredentialsResponse_httpStatus = Lens.lens (\UpdateChapCredentialsResponse' {httpStatus} -> httpStatus) (\s@UpdateChapCredentialsResponse' {} a -> s {httpStatus = a} :: UpdateChapCredentialsResponse)

instance Core.NFData UpdateChapCredentialsResponse
