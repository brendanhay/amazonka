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
-- Module      : Amazonka.KMS.ConnectCustomKeyStore
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Connects or reconnects a
-- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store>
-- to its backing key store. For an CloudHSM key store,
-- @ConnectCustomKeyStore@ connects the key store to its associated
-- CloudHSM cluster. For an external key store, @ConnectCustomKeyStore@
-- connects the key store to the external key store proxy that communicates
-- with your external key manager.
--
-- The custom key store must be connected before you can create KMS keys in
-- the key store or use the KMS keys it contains. You can disconnect and
-- reconnect a custom key store at any time.
--
-- The connection process for a custom key store can take an extended
-- amount of time to complete. This operation starts the connection
-- process, but it does not wait for it to complete. When it succeeds, this
-- operation quickly returns an HTTP 200 response and a JSON object with no
-- properties. However, this response does not indicate that the custom key
-- store is connected. To get the connection state of the custom key store,
-- use the DescribeCustomKeyStores operation.
--
-- This operation is part of the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key stores>
-- feature in KMS, which combines the convenience and extensive integration
-- of KMS with the isolation and control of a key store that you own and
-- manage.
--
-- The @ConnectCustomKeyStore@ operation might fail for various reasons. To
-- find the reason, use the DescribeCustomKeyStores operation and see the
-- @ConnectionErrorCode@ in the response. For help interpreting the
-- @ConnectionErrorCode@, see CustomKeyStoresListEntry.
--
-- To fix the failure, use the DisconnectCustomKeyStore operation to
-- disconnect the custom key store, correct the error, use the
-- UpdateCustomKeyStore operation if necessary, and then use
-- @ConnectCustomKeyStore@ again.
--
-- __CloudHSM key store__
--
-- During the connection process for an CloudHSM key store, KMS finds the
-- CloudHSM cluster that is associated with the custom key store, creates
-- the connection infrastructure, connects to the cluster, logs into the
-- CloudHSM client as the @kmsuser@ CU, and rotates its password.
--
-- To connect an CloudHSM key store, its associated CloudHSM cluster must
-- have at least one active HSM. To get the number of active HSMs in a
-- cluster, use the
-- <https://docs.aws.amazon.com/cloudhsm/latest/APIReference/API_DescribeClusters.html DescribeClusters>
-- operation. To add HSMs to the cluster, use the
-- <https://docs.aws.amazon.com/cloudhsm/latest/APIReference/API_CreateHsm.html CreateHsm>
-- operation. Also, the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-store-concepts.html#concept-kmsuser kmsuser crypto user>
-- (CU) must not be logged into the cluster. This prevents KMS from using
-- this account to log in.
--
-- If you are having trouble connecting or disconnecting a CloudHSM key
-- store, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html Troubleshooting an CloudHSM key store>
-- in the /Key Management Service Developer Guide/.
--
-- __External key store__
--
-- When you connect an external key store that uses public endpoint
-- connectivity, KMS tests its ability to communicate with your external
-- key manager by sending a request via the external key store proxy.
--
-- When you connect to an external key store that uses VPC endpoint service
-- connectivity, KMS establishes the networking elements that it needs to
-- communicate with your external key manager via the external key store
-- proxy. This includes creating an interface endpoint to the VPC endpoint
-- service and a private hosted zone for traffic between KMS and the VPC
-- endpoint service.
--
-- To connect an external key store, KMS must be able to connect to the
-- external key store proxy, the external key store proxy must be able to
-- communicate with your external key manager, and the external key manager
-- must be available for cryptographic operations.
--
-- If you are having trouble connecting or disconnecting an external key
-- store, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/xks-troubleshooting.html Troubleshooting an external key store>
-- in the /Key Management Service Developer Guide/.
--
-- __Cross-account use__: No. You cannot perform this operation on a custom
-- key store in a different Amazon Web Services account.
--
-- __Required permissions__:
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:ConnectCustomKeyStore>
-- (IAM policy)
--
-- __Related operations__
--
-- -   CreateCustomKeyStore
--
-- -   DeleteCustomKeyStore
--
-- -   DescribeCustomKeyStores
--
-- -   DisconnectCustomKeyStore
--
-- -   UpdateCustomKeyStore
module Amazonka.KMS.ConnectCustomKeyStore
  ( -- * Creating a Request
    ConnectCustomKeyStore (..),
    newConnectCustomKeyStore,

    -- * Request Lenses
    connectCustomKeyStore_customKeyStoreId,

    -- * Destructuring the Response
    ConnectCustomKeyStoreResponse (..),
    newConnectCustomKeyStoreResponse,

    -- * Response Lenses
    connectCustomKeyStoreResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newConnectCustomKeyStore' smart constructor.
data ConnectCustomKeyStore = ConnectCustomKeyStore'
  { -- | Enter the key store ID of the custom key store that you want to connect.
    -- To find the ID of a custom key store, use the DescribeCustomKeyStores
    -- operation.
    customKeyStoreId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConnectCustomKeyStore' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customKeyStoreId', 'connectCustomKeyStore_customKeyStoreId' - Enter the key store ID of the custom key store that you want to connect.
-- To find the ID of a custom key store, use the DescribeCustomKeyStores
-- operation.
newConnectCustomKeyStore ::
  -- | 'customKeyStoreId'
  Prelude.Text ->
  ConnectCustomKeyStore
newConnectCustomKeyStore pCustomKeyStoreId_ =
  ConnectCustomKeyStore'
    { customKeyStoreId =
        pCustomKeyStoreId_
    }

-- | Enter the key store ID of the custom key store that you want to connect.
-- To find the ID of a custom key store, use the DescribeCustomKeyStores
-- operation.
connectCustomKeyStore_customKeyStoreId :: Lens.Lens' ConnectCustomKeyStore Prelude.Text
connectCustomKeyStore_customKeyStoreId = Lens.lens (\ConnectCustomKeyStore' {customKeyStoreId} -> customKeyStoreId) (\s@ConnectCustomKeyStore' {} a -> s {customKeyStoreId = a} :: ConnectCustomKeyStore)

instance Core.AWSRequest ConnectCustomKeyStore where
  type
    AWSResponse ConnectCustomKeyStore =
      ConnectCustomKeyStoreResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          ConnectCustomKeyStoreResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ConnectCustomKeyStore where
  hashWithSalt _salt ConnectCustomKeyStore' {..} =
    _salt `Prelude.hashWithSalt` customKeyStoreId

instance Prelude.NFData ConnectCustomKeyStore where
  rnf ConnectCustomKeyStore' {..} =
    Prelude.rnf customKeyStoreId

instance Data.ToHeaders ConnectCustomKeyStore where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "TrentService.ConnectCustomKeyStore" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ConnectCustomKeyStore where
  toJSON ConnectCustomKeyStore' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("CustomKeyStoreId" Data..= customKeyStoreId)
          ]
      )

instance Data.ToPath ConnectCustomKeyStore where
  toPath = Prelude.const "/"

instance Data.ToQuery ConnectCustomKeyStore where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newConnectCustomKeyStoreResponse' smart constructor.
data ConnectCustomKeyStoreResponse = ConnectCustomKeyStoreResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConnectCustomKeyStoreResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'connectCustomKeyStoreResponse_httpStatus' - The response's http status code.
newConnectCustomKeyStoreResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ConnectCustomKeyStoreResponse
newConnectCustomKeyStoreResponse pHttpStatus_ =
  ConnectCustomKeyStoreResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
connectCustomKeyStoreResponse_httpStatus :: Lens.Lens' ConnectCustomKeyStoreResponse Prelude.Int
connectCustomKeyStoreResponse_httpStatus = Lens.lens (\ConnectCustomKeyStoreResponse' {httpStatus} -> httpStatus) (\s@ConnectCustomKeyStoreResponse' {} a -> s {httpStatus = a} :: ConnectCustomKeyStoreResponse)

instance Prelude.NFData ConnectCustomKeyStoreResponse where
  rnf ConnectCustomKeyStoreResponse' {..} =
    Prelude.rnf httpStatus
