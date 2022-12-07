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
-- Module      : Amazonka.KMS.DisconnectCustomKeyStore
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disconnects the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store>
-- from its associated CloudHSM cluster. While a custom key store is
-- disconnected, you can manage the custom key store and its KMS keys, but
-- you cannot create or use KMS keys in the custom key store. You can
-- reconnect the custom key store at any time.
--
-- While a custom key store is disconnected, all attempts to create KMS
-- keys in the custom key store or to use existing KMS keys in
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
-- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store feature>
-- feature in KMS, which combines the convenience and extensive integration
-- of KMS with the isolation and control of a single-tenant key store.
--
-- __Cross-account use__: No. You cannot perform this operation on a custom
-- key store in a different Amazon Web Services account.
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
module Amazonka.KMS.DisconnectCustomKeyStore
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisconnectCustomKeyStore' smart constructor.
data DisconnectCustomKeyStore = DisconnectCustomKeyStore'
  { -- | Enter the ID of the custom key store you want to disconnect. To find the
    -- ID of a custom key store, use the DescribeCustomKeyStores operation.
    customKeyStoreId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DisconnectCustomKeyStore
newDisconnectCustomKeyStore pCustomKeyStoreId_ =
  DisconnectCustomKeyStore'
    { customKeyStoreId =
        pCustomKeyStoreId_
    }

-- | Enter the ID of the custom key store you want to disconnect. To find the
-- ID of a custom key store, use the DescribeCustomKeyStores operation.
disconnectCustomKeyStore_customKeyStoreId :: Lens.Lens' DisconnectCustomKeyStore Prelude.Text
disconnectCustomKeyStore_customKeyStoreId = Lens.lens (\DisconnectCustomKeyStore' {customKeyStoreId} -> customKeyStoreId) (\s@DisconnectCustomKeyStore' {} a -> s {customKeyStoreId = a} :: DisconnectCustomKeyStore)

instance Core.AWSRequest DisconnectCustomKeyStore where
  type
    AWSResponse DisconnectCustomKeyStore =
      DisconnectCustomKeyStoreResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisconnectCustomKeyStoreResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisconnectCustomKeyStore where
  hashWithSalt _salt DisconnectCustomKeyStore' {..} =
    _salt `Prelude.hashWithSalt` customKeyStoreId

instance Prelude.NFData DisconnectCustomKeyStore where
  rnf DisconnectCustomKeyStore' {..} =
    Prelude.rnf customKeyStoreId

instance Data.ToHeaders DisconnectCustomKeyStore where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "TrentService.DisconnectCustomKeyStore" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisconnectCustomKeyStore where
  toJSON DisconnectCustomKeyStore' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("CustomKeyStoreId" Data..= customKeyStoreId)
          ]
      )

instance Data.ToPath DisconnectCustomKeyStore where
  toPath = Prelude.const "/"

instance Data.ToQuery DisconnectCustomKeyStore where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisconnectCustomKeyStoreResponse' smart constructor.
data DisconnectCustomKeyStoreResponse = DisconnectCustomKeyStoreResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DisconnectCustomKeyStoreResponse
newDisconnectCustomKeyStoreResponse pHttpStatus_ =
  DisconnectCustomKeyStoreResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
disconnectCustomKeyStoreResponse_httpStatus :: Lens.Lens' DisconnectCustomKeyStoreResponse Prelude.Int
disconnectCustomKeyStoreResponse_httpStatus = Lens.lens (\DisconnectCustomKeyStoreResponse' {httpStatus} -> httpStatus) (\s@DisconnectCustomKeyStoreResponse' {} a -> s {httpStatus = a} :: DisconnectCustomKeyStoreResponse)

instance
  Prelude.NFData
    DisconnectCustomKeyStoreResponse
  where
  rnf DisconnectCustomKeyStoreResponse' {..} =
    Prelude.rnf httpStatus
