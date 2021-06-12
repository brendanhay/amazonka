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
-- Module      : Network.AWS.KMS.DisconnectCustomKeyStore
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disconnects the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store>
-- from its associated AWS CloudHSM cluster. While a custom key store is
-- disconnected, you can manage the custom key store and its customer
-- master keys (CMKs), but you cannot create or use CMKs in the custom key
-- store. You can reconnect the custom key store at any time.
--
-- While a custom key store is disconnected, all attempts to create
-- customer master keys (CMKs) in the custom key store or to use existing
-- CMKs in
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operations>
-- will fail. This action can prevent users from storing and accessing
-- sensitive data.
--
-- To find the connection state of a custom key store, use the
-- DescribeCustomKeyStores operation. To reconnect a custom key store, use
-- the ConnectCustomKeyStore operation.
--
-- If the operation succeeds, it returns a JSON object with no properties.
--
-- This operation is part of the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html Custom Key Store feature>
-- feature in AWS KMS, which combines the convenience and extensive
-- integration of AWS KMS with the isolation and control of a single-tenant
-- key store.
--
-- __Cross-account use__: No. You cannot perform this operation on a custom
-- key store in a different AWS account.
--
-- __Required permissions__:
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:DisconnectCustomKeyStore>
-- (IAM policy)
--
-- __Related operations:__
--
-- -   ConnectCustomKeyStore
--
-- -   CreateCustomKeyStore
--
-- -   DeleteCustomKeyStore
--
-- -   DescribeCustomKeyStores
--
-- -   UpdateCustomKeyStore
module Network.AWS.KMS.DisconnectCustomKeyStore
  ( -- * Creating a Request
    DisconnectCustomKeyStore (..),
    newDisconnectCustomKeyStore,

    -- * Request Lenses
    disconnectCustomKeyStore_customKeyStoreId,

    -- * Destructuring the Response
    DisconnectCustomKeyStoreResponse (..),
    newDisconnectCustomKeyStoreResponse,

    -- * Response Lenses
    disconnectCustomKeyStoreResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisconnectCustomKeyStore' smart constructor.
data DisconnectCustomKeyStore = DisconnectCustomKeyStore'
  { -- | Enter the ID of the custom key store you want to disconnect. To find the
    -- ID of a custom key store, use the DescribeCustomKeyStores operation.
    customKeyStoreId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisconnectCustomKeyStore' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customKeyStoreId', 'disconnectCustomKeyStore_customKeyStoreId' - Enter the ID of the custom key store you want to disconnect. To find the
-- ID of a custom key store, use the DescribeCustomKeyStores operation.
newDisconnectCustomKeyStore ::
  -- | 'customKeyStoreId'
  Core.Text ->
  DisconnectCustomKeyStore
newDisconnectCustomKeyStore pCustomKeyStoreId_ =
  DisconnectCustomKeyStore'
    { customKeyStoreId =
        pCustomKeyStoreId_
    }

-- | Enter the ID of the custom key store you want to disconnect. To find the
-- ID of a custom key store, use the DescribeCustomKeyStores operation.
disconnectCustomKeyStore_customKeyStoreId :: Lens.Lens' DisconnectCustomKeyStore Core.Text
disconnectCustomKeyStore_customKeyStoreId = Lens.lens (\DisconnectCustomKeyStore' {customKeyStoreId} -> customKeyStoreId) (\s@DisconnectCustomKeyStore' {} a -> s {customKeyStoreId = a} :: DisconnectCustomKeyStore)

instance Core.AWSRequest DisconnectCustomKeyStore where
  type
    AWSResponse DisconnectCustomKeyStore =
      DisconnectCustomKeyStoreResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisconnectCustomKeyStoreResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DisconnectCustomKeyStore

instance Core.NFData DisconnectCustomKeyStore

instance Core.ToHeaders DisconnectCustomKeyStore where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "TrentService.DisconnectCustomKeyStore" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DisconnectCustomKeyStore where
  toJSON DisconnectCustomKeyStore' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("CustomKeyStoreId" Core..= customKeyStoreId)
          ]
      )

instance Core.ToPath DisconnectCustomKeyStore where
  toPath = Core.const "/"

instance Core.ToQuery DisconnectCustomKeyStore where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDisconnectCustomKeyStoreResponse' smart constructor.
data DisconnectCustomKeyStoreResponse = DisconnectCustomKeyStoreResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisconnectCustomKeyStoreResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disconnectCustomKeyStoreResponse_httpStatus' - The response's http status code.
newDisconnectCustomKeyStoreResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DisconnectCustomKeyStoreResponse
newDisconnectCustomKeyStoreResponse pHttpStatus_ =
  DisconnectCustomKeyStoreResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
disconnectCustomKeyStoreResponse_httpStatus :: Lens.Lens' DisconnectCustomKeyStoreResponse Core.Int
disconnectCustomKeyStoreResponse_httpStatus = Lens.lens (\DisconnectCustomKeyStoreResponse' {httpStatus} -> httpStatus) (\s@DisconnectCustomKeyStoreResponse' {} a -> s {httpStatus = a} :: DisconnectCustomKeyStoreResponse)

instance Core.NFData DisconnectCustomKeyStoreResponse
