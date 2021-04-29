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
-- Module      : Network.AWS.KMS.DeleteCustomKeyStore
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a
-- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store>.
-- This operation does not delete the AWS CloudHSM cluster that is
-- associated with the custom key store, or affect any users or keys in the
-- cluster.
--
-- The custom key store that you delete cannot contain any AWS KMS
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#master_keys customer master keys (CMKs)>.
-- Before deleting the key store, verify that you will never need to use
-- any of the CMKs in the key store for any
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operations>.
-- Then, use ScheduleKeyDeletion to delete the AWS KMS customer master keys
-- (CMKs) from the key store. When the scheduled waiting period expires,
-- the @ScheduleKeyDeletion@ operation deletes the CMKs. Then it makes a
-- best effort to delete the key material from the associated cluster.
-- However, you might need to manually
-- <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html#fix-keystore-orphaned-key delete the orphaned key material>
-- from the cluster and its backups.
--
-- After all CMKs are deleted from AWS KMS, use DisconnectCustomKeyStore to
-- disconnect the key store from AWS KMS. Then, you can delete the custom
-- key store.
--
-- Instead of deleting the custom key store, consider using
-- DisconnectCustomKeyStore to disconnect it from AWS KMS. While the key
-- store is disconnected, you cannot create or use the CMKs in the key
-- store. But, you do not need to delete CMKs and you can reconnect a
-- disconnected custom key store at any time.
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
module Network.AWS.KMS.DeleteCustomKeyStore
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

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteCustomKeyStore' smart constructor.
data DeleteCustomKeyStore = DeleteCustomKeyStore'
  { -- | Enter the ID of the custom key store you want to delete. To find the ID
    -- of a custom key store, use the DescribeCustomKeyStores operation.
    customKeyStoreId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DeleteCustomKeyStore where
  type
    Rs DeleteCustomKeyStore =
      DeleteCustomKeyStoreResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteCustomKeyStoreResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteCustomKeyStore

instance Prelude.NFData DeleteCustomKeyStore

instance Prelude.ToHeaders DeleteCustomKeyStore where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "TrentService.DeleteCustomKeyStore" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteCustomKeyStore where
  toJSON DeleteCustomKeyStore' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("CustomKeyStoreId" Prelude..= customKeyStoreId)
          ]
      )

instance Prelude.ToPath DeleteCustomKeyStore where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteCustomKeyStore where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteCustomKeyStoreResponse' smart constructor.
data DeleteCustomKeyStoreResponse = DeleteCustomKeyStoreResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData DeleteCustomKeyStoreResponse
