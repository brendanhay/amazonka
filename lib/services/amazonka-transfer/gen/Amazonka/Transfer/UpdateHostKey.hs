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
-- Module      : Amazonka.Transfer.UpdateHostKey
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the description for the host key that\'s specified by the
-- @ServerId@ and @HostKeyId@ parameters.
module Amazonka.Transfer.UpdateHostKey
  ( -- * Creating a Request
    UpdateHostKey (..),
    newUpdateHostKey,

    -- * Request Lenses
    updateHostKey_serverId,
    updateHostKey_hostKeyId,
    updateHostKey_description,

    -- * Destructuring the Response
    UpdateHostKeyResponse (..),
    newUpdateHostKeyResponse,

    -- * Response Lenses
    updateHostKeyResponse_httpStatus,
    updateHostKeyResponse_serverId,
    updateHostKeyResponse_hostKeyId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transfer.Types

-- | /See:/ 'newUpdateHostKey' smart constructor.
data UpdateHostKey = UpdateHostKey'
  { -- | The identifier of the server that contains the host key that you are
    -- updating.
    serverId :: Prelude.Text,
    -- | The identifier of the host key that you are updating.
    hostKeyId :: Prelude.Text,
    -- | An updated description for the host key.
    description :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateHostKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serverId', 'updateHostKey_serverId' - The identifier of the server that contains the host key that you are
-- updating.
--
-- 'hostKeyId', 'updateHostKey_hostKeyId' - The identifier of the host key that you are updating.
--
-- 'description', 'updateHostKey_description' - An updated description for the host key.
newUpdateHostKey ::
  -- | 'serverId'
  Prelude.Text ->
  -- | 'hostKeyId'
  Prelude.Text ->
  -- | 'description'
  Prelude.Text ->
  UpdateHostKey
newUpdateHostKey pServerId_ pHostKeyId_ pDescription_ =
  UpdateHostKey'
    { serverId = pServerId_,
      hostKeyId = pHostKeyId_,
      description = pDescription_
    }

-- | The identifier of the server that contains the host key that you are
-- updating.
updateHostKey_serverId :: Lens.Lens' UpdateHostKey Prelude.Text
updateHostKey_serverId = Lens.lens (\UpdateHostKey' {serverId} -> serverId) (\s@UpdateHostKey' {} a -> s {serverId = a} :: UpdateHostKey)

-- | The identifier of the host key that you are updating.
updateHostKey_hostKeyId :: Lens.Lens' UpdateHostKey Prelude.Text
updateHostKey_hostKeyId = Lens.lens (\UpdateHostKey' {hostKeyId} -> hostKeyId) (\s@UpdateHostKey' {} a -> s {hostKeyId = a} :: UpdateHostKey)

-- | An updated description for the host key.
updateHostKey_description :: Lens.Lens' UpdateHostKey Prelude.Text
updateHostKey_description = Lens.lens (\UpdateHostKey' {description} -> description) (\s@UpdateHostKey' {} a -> s {description = a} :: UpdateHostKey)

instance Core.AWSRequest UpdateHostKey where
  type
    AWSResponse UpdateHostKey =
      UpdateHostKeyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateHostKeyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "ServerId")
            Prelude.<*> (x Core..:> "HostKeyId")
      )

instance Prelude.Hashable UpdateHostKey where
  hashWithSalt _salt UpdateHostKey' {..} =
    _salt `Prelude.hashWithSalt` serverId
      `Prelude.hashWithSalt` hostKeyId
      `Prelude.hashWithSalt` description

instance Prelude.NFData UpdateHostKey where
  rnf UpdateHostKey' {..} =
    Prelude.rnf serverId
      `Prelude.seq` Prelude.rnf hostKeyId
      `Prelude.seq` Prelude.rnf description

instance Core.ToHeaders UpdateHostKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "TransferService.UpdateHostKey" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateHostKey where
  toJSON UpdateHostKey' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ServerId" Core..= serverId),
            Prelude.Just ("HostKeyId" Core..= hostKeyId),
            Prelude.Just ("Description" Core..= description)
          ]
      )

instance Core.ToPath UpdateHostKey where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateHostKey where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateHostKeyResponse' smart constructor.
data UpdateHostKeyResponse = UpdateHostKeyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Returns the server identifier for the server that contains the updated
    -- host key.
    serverId :: Prelude.Text,
    -- | Returns the host key identifier for the updated host key.
    hostKeyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateHostKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateHostKeyResponse_httpStatus' - The response's http status code.
--
-- 'serverId', 'updateHostKeyResponse_serverId' - Returns the server identifier for the server that contains the updated
-- host key.
--
-- 'hostKeyId', 'updateHostKeyResponse_hostKeyId' - Returns the host key identifier for the updated host key.
newUpdateHostKeyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'serverId'
  Prelude.Text ->
  -- | 'hostKeyId'
  Prelude.Text ->
  UpdateHostKeyResponse
newUpdateHostKeyResponse
  pHttpStatus_
  pServerId_
  pHostKeyId_ =
    UpdateHostKeyResponse'
      { httpStatus = pHttpStatus_,
        serverId = pServerId_,
        hostKeyId = pHostKeyId_
      }

-- | The response's http status code.
updateHostKeyResponse_httpStatus :: Lens.Lens' UpdateHostKeyResponse Prelude.Int
updateHostKeyResponse_httpStatus = Lens.lens (\UpdateHostKeyResponse' {httpStatus} -> httpStatus) (\s@UpdateHostKeyResponse' {} a -> s {httpStatus = a} :: UpdateHostKeyResponse)

-- | Returns the server identifier for the server that contains the updated
-- host key.
updateHostKeyResponse_serverId :: Lens.Lens' UpdateHostKeyResponse Prelude.Text
updateHostKeyResponse_serverId = Lens.lens (\UpdateHostKeyResponse' {serverId} -> serverId) (\s@UpdateHostKeyResponse' {} a -> s {serverId = a} :: UpdateHostKeyResponse)

-- | Returns the host key identifier for the updated host key.
updateHostKeyResponse_hostKeyId :: Lens.Lens' UpdateHostKeyResponse Prelude.Text
updateHostKeyResponse_hostKeyId = Lens.lens (\UpdateHostKeyResponse' {hostKeyId} -> hostKeyId) (\s@UpdateHostKeyResponse' {} a -> s {hostKeyId = a} :: UpdateHostKeyResponse)

instance Prelude.NFData UpdateHostKeyResponse where
  rnf UpdateHostKeyResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf serverId
      `Prelude.seq` Prelude.rnf hostKeyId
