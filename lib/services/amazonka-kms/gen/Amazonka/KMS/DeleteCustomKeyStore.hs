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
-- Module      : Amazonka.KMS.DeleteCustomKeyStore
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a
-- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store>.
-- This operation does not affect any backing elements of the custom key
-- store. It does not delete the CloudHSM cluster that is associated with
-- an CloudHSM key store, or affect any users or keys in the cluster. For
-- an external key store, it does not affect the external key store proxy,
-- external key manager, or any external keys.
--
-- This operation is part of the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key stores>
-- feature in KMS, which combines the convenience and extensive integration
-- of KMS with the isolation and control of a key store that you own and
-- manage.
--
-- The custom key store that you delete cannot contain any
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#kms_keys KMS keys>.
-- Before deleting the key store, verify that you will never need to use
-- any of the KMS keys in the key store for any
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operations>.
-- Then, use ScheduleKeyDeletion to delete the KMS keys from the key store.
-- After the required waiting period expires and all KMS keys are deleted
-- from the custom key store, use DisconnectCustomKeyStore to disconnect
-- the key store from KMS. Then, you can delete the custom key store.
--
-- For keys in an CloudHSM key store, the @ScheduleKeyDeletion@ operation
-- makes a best effort to delete the key material from the associated
-- cluster. However, you might need to manually
-- <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html#fix-keystore-orphaned-key delete the orphaned key material>
-- from the cluster and its backups. KMS never creates, manages, or deletes
-- cryptographic keys in the external key manager associated with an
-- external key store. You must manage them using your external key manager
-- tools.
--
-- Instead of deleting the custom key store, consider using the
-- DisconnectCustomKeyStore operation to disconnect the custom key store
-- from its backing key store. While the key store is disconnected, you
-- cannot create or use the KMS keys in the key store. But, you do not need
-- to delete KMS keys and you can reconnect a disconnected custom key store
-- at any time.
--
-- If the operation succeeds, it returns a JSON object with no properties.
--
-- __Cross-account use__: No. You cannot perform this operation on a custom
-- key store in a different Amazon Web Services account.
--
-- __Required permissions__:
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:DeleteCustomKeyStore>
-- (IAM policy)
--
-- __Related operations:__
--
-- -   ConnectCustomKeyStore
--
-- -   CreateCustomKeyStore
--
-- -   DescribeCustomKeyStores
--
-- -   DisconnectCustomKeyStore
--
-- -   UpdateCustomKeyStore
module Amazonka.KMS.DeleteCustomKeyStore
  ( -- * Creating a Request
    DeleteCustomKeyStore (..),
    newDeleteCustomKeyStore,

    -- * Request Lenses
    deleteCustomKeyStore_customKeyStoreId,

    -- * Destructuring the Response
    DeleteCustomKeyStoreResponse (..),
    newDeleteCustomKeyStoreResponse,

    -- * Response Lenses
    deleteCustomKeyStoreResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteCustomKeyStore' smart constructor.
data DeleteCustomKeyStore = DeleteCustomKeyStore'
  { -- | Enter the ID of the custom key store you want to delete. To find the ID
    -- of a custom key store, use the DescribeCustomKeyStores operation.
    customKeyStoreId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCustomKeyStore' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customKeyStoreId', 'deleteCustomKeyStore_customKeyStoreId' - Enter the ID of the custom key store you want to delete. To find the ID
-- of a custom key store, use the DescribeCustomKeyStores operation.
newDeleteCustomKeyStore ::
  -- | 'customKeyStoreId'
  Prelude.Text ->
  DeleteCustomKeyStore
newDeleteCustomKeyStore pCustomKeyStoreId_ =
  DeleteCustomKeyStore'
    { customKeyStoreId =
        pCustomKeyStoreId_
    }

-- | Enter the ID of the custom key store you want to delete. To find the ID
-- of a custom key store, use the DescribeCustomKeyStores operation.
deleteCustomKeyStore_customKeyStoreId :: Lens.Lens' DeleteCustomKeyStore Prelude.Text
deleteCustomKeyStore_customKeyStoreId = Lens.lens (\DeleteCustomKeyStore' {customKeyStoreId} -> customKeyStoreId) (\s@DeleteCustomKeyStore' {} a -> s {customKeyStoreId = a} :: DeleteCustomKeyStore)

instance Core.AWSRequest DeleteCustomKeyStore where
  type
    AWSResponse DeleteCustomKeyStore =
      DeleteCustomKeyStoreResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteCustomKeyStoreResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteCustomKeyStore where
  hashWithSalt _salt DeleteCustomKeyStore' {..} =
    _salt `Prelude.hashWithSalt` customKeyStoreId

instance Prelude.NFData DeleteCustomKeyStore where
  rnf DeleteCustomKeyStore' {..} =
    Prelude.rnf customKeyStoreId

instance Data.ToHeaders DeleteCustomKeyStore where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "TrentService.DeleteCustomKeyStore" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteCustomKeyStore where
  toJSON DeleteCustomKeyStore' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("CustomKeyStoreId" Data..= customKeyStoreId)
          ]
      )

instance Data.ToPath DeleteCustomKeyStore where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteCustomKeyStore where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteCustomKeyStoreResponse' smart constructor.
data DeleteCustomKeyStoreResponse = DeleteCustomKeyStoreResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCustomKeyStoreResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteCustomKeyStoreResponse_httpStatus' - The response's http status code.
newDeleteCustomKeyStoreResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteCustomKeyStoreResponse
newDeleteCustomKeyStoreResponse pHttpStatus_ =
  DeleteCustomKeyStoreResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteCustomKeyStoreResponse_httpStatus :: Lens.Lens' DeleteCustomKeyStoreResponse Prelude.Int
deleteCustomKeyStoreResponse_httpStatus = Lens.lens (\DeleteCustomKeyStoreResponse' {httpStatus} -> httpStatus) (\s@DeleteCustomKeyStoreResponse' {} a -> s {httpStatus = a} :: DeleteCustomKeyStoreResponse)

instance Prelude.NFData DeleteCustomKeyStoreResponse where
  rnf DeleteCustomKeyStoreResponse' {..} =
    Prelude.rnf httpStatus
