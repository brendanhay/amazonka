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
-- Module      : Network.AWS.KMS.UpdateCustomKeyStore
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the properties of a custom key store. Use the @CustomKeyStoreId@
-- parameter to identify the custom key store you want to edit. Use the
-- remaining parameters to change the properties of the custom key store.
--
-- You can only update a custom key store that is disconnected. To
-- disconnect the custom key store, use DisconnectCustomKeyStore. To
-- reconnect the custom key store after the update completes, use
-- ConnectCustomKeyStore. To find the connection state of a custom key
-- store, use the DescribeCustomKeyStores operation.
--
-- Use the parameters of @UpdateCustomKeyStore@ to edit your keystore
-- settings.
--
-- -   Use the __NewCustomKeyStoreName__ parameter to change the friendly
--     name of the custom key store to the value that you specify.
--
-- -   Use the __KeyStorePassword__ parameter tell AWS KMS the current
--     password of the
--     <https://docs.aws.amazon.com/kms/latest/developerguide/key-store-concepts.html#concept-kmsuser kmsuser crypto user (CU)>
--     in the associated AWS CloudHSM cluster. You can use this parameter
--     to
--     <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html#fix-keystore-password fix connection failures>
--     that occur when AWS KMS cannot log into the associated cluster
--     because the @kmsuser@ password has changed. This value does not
--     change the password in the AWS CloudHSM cluster.
--
-- -   Use the __CloudHsmClusterId__ parameter to associate the custom key
--     store with a different, but related, AWS CloudHSM cluster. You can
--     use this parameter to repair a custom key store if its AWS CloudHSM
--     cluster becomes corrupted or is deleted, or when you need to create
--     or restore a cluster from a backup.
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
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:UpdateCustomKeyStore>
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
-- -   DisconnectCustomKeyStore
module Network.AWS.KMS.UpdateCustomKeyStore
  ( -- * Creating a Request
    UpdateCustomKeyStore (..),
    newUpdateCustomKeyStore,

    -- * Request Lenses
    updateCustomKeyStore_keyStorePassword,
    updateCustomKeyStore_newCustomKeyStoreName,
    updateCustomKeyStore_cloudHsmClusterId,
    updateCustomKeyStore_customKeyStoreId,

    -- * Destructuring the Response
    UpdateCustomKeyStoreResponse (..),
    newUpdateCustomKeyStoreResponse,

    -- * Response Lenses
    updateCustomKeyStoreResponse_httpStatus,
  )
where

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateCustomKeyStore' smart constructor.
data UpdateCustomKeyStore = UpdateCustomKeyStore'
  { -- | Enter the current password of the @kmsuser@ crypto user (CU) in the AWS
    -- CloudHSM cluster that is associated with the custom key store.
    --
    -- This parameter tells AWS KMS the current password of the @kmsuser@
    -- crypto user (CU). It does not set or change the password of any users in
    -- the AWS CloudHSM cluster.
    keyStorePassword :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | Changes the friendly name of the custom key store to the value that you
    -- specify. The custom key store name must be unique in the AWS account.
    newCustomKeyStoreName' :: Prelude.Maybe Prelude.Text,
    -- | Associates the custom key store with a related AWS CloudHSM cluster.
    --
    -- Enter the cluster ID of the cluster that you used to create the custom
    -- key store or a cluster that shares a backup history and has the same
    -- cluster certificate as the original cluster. You cannot use this
    -- parameter to associate a custom key store with an unrelated cluster. In
    -- addition, the replacement cluster must
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/create-keystore.html#before-keystore fulfill the requirements>
    -- for a cluster associated with a custom key store. To view the cluster
    -- certificate of a cluster, use the
    -- <https://docs.aws.amazon.com/cloudhsm/latest/APIReference/API_DescribeClusters.html DescribeClusters>
    -- operation.
    cloudHsmClusterId :: Prelude.Maybe Prelude.Text,
    -- | Identifies the custom key store that you want to update. Enter the ID of
    -- the custom key store. To find the ID of a custom key store, use the
    -- DescribeCustomKeyStores operation.
    customKeyStoreId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateCustomKeyStore' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyStorePassword', 'updateCustomKeyStore_keyStorePassword' - Enter the current password of the @kmsuser@ crypto user (CU) in the AWS
-- CloudHSM cluster that is associated with the custom key store.
--
-- This parameter tells AWS KMS the current password of the @kmsuser@
-- crypto user (CU). It does not set or change the password of any users in
-- the AWS CloudHSM cluster.
--
-- 'newCustomKeyStoreName'', 'updateCustomKeyStore_newCustomKeyStoreName' - Changes the friendly name of the custom key store to the value that you
-- specify. The custom key store name must be unique in the AWS account.
--
-- 'cloudHsmClusterId', 'updateCustomKeyStore_cloudHsmClusterId' - Associates the custom key store with a related AWS CloudHSM cluster.
--
-- Enter the cluster ID of the cluster that you used to create the custom
-- key store or a cluster that shares a backup history and has the same
-- cluster certificate as the original cluster. You cannot use this
-- parameter to associate a custom key store with an unrelated cluster. In
-- addition, the replacement cluster must
-- <https://docs.aws.amazon.com/kms/latest/developerguide/create-keystore.html#before-keystore fulfill the requirements>
-- for a cluster associated with a custom key store. To view the cluster
-- certificate of a cluster, use the
-- <https://docs.aws.amazon.com/cloudhsm/latest/APIReference/API_DescribeClusters.html DescribeClusters>
-- operation.
--
-- 'customKeyStoreId', 'updateCustomKeyStore_customKeyStoreId' - Identifies the custom key store that you want to update. Enter the ID of
-- the custom key store. To find the ID of a custom key store, use the
-- DescribeCustomKeyStores operation.
newUpdateCustomKeyStore ::
  -- | 'customKeyStoreId'
  Prelude.Text ->
  UpdateCustomKeyStore
newUpdateCustomKeyStore pCustomKeyStoreId_ =
  UpdateCustomKeyStore'
    { keyStorePassword =
        Prelude.Nothing,
      newCustomKeyStoreName' = Prelude.Nothing,
      cloudHsmClusterId = Prelude.Nothing,
      customKeyStoreId = pCustomKeyStoreId_
    }

-- | Enter the current password of the @kmsuser@ crypto user (CU) in the AWS
-- CloudHSM cluster that is associated with the custom key store.
--
-- This parameter tells AWS KMS the current password of the @kmsuser@
-- crypto user (CU). It does not set or change the password of any users in
-- the AWS CloudHSM cluster.
updateCustomKeyStore_keyStorePassword :: Lens.Lens' UpdateCustomKeyStore (Prelude.Maybe Prelude.Text)
updateCustomKeyStore_keyStorePassword = Lens.lens (\UpdateCustomKeyStore' {keyStorePassword} -> keyStorePassword) (\s@UpdateCustomKeyStore' {} a -> s {keyStorePassword = a} :: UpdateCustomKeyStore) Prelude.. Lens.mapping Prelude._Sensitive

-- | Changes the friendly name of the custom key store to the value that you
-- specify. The custom key store name must be unique in the AWS account.
updateCustomKeyStore_newCustomKeyStoreName :: Lens.Lens' UpdateCustomKeyStore (Prelude.Maybe Prelude.Text)
updateCustomKeyStore_newCustomKeyStoreName = Lens.lens (\UpdateCustomKeyStore' {newCustomKeyStoreName'} -> newCustomKeyStoreName') (\s@UpdateCustomKeyStore' {} a -> s {newCustomKeyStoreName' = a} :: UpdateCustomKeyStore)

-- | Associates the custom key store with a related AWS CloudHSM cluster.
--
-- Enter the cluster ID of the cluster that you used to create the custom
-- key store or a cluster that shares a backup history and has the same
-- cluster certificate as the original cluster. You cannot use this
-- parameter to associate a custom key store with an unrelated cluster. In
-- addition, the replacement cluster must
-- <https://docs.aws.amazon.com/kms/latest/developerguide/create-keystore.html#before-keystore fulfill the requirements>
-- for a cluster associated with a custom key store. To view the cluster
-- certificate of a cluster, use the
-- <https://docs.aws.amazon.com/cloudhsm/latest/APIReference/API_DescribeClusters.html DescribeClusters>
-- operation.
updateCustomKeyStore_cloudHsmClusterId :: Lens.Lens' UpdateCustomKeyStore (Prelude.Maybe Prelude.Text)
updateCustomKeyStore_cloudHsmClusterId = Lens.lens (\UpdateCustomKeyStore' {cloudHsmClusterId} -> cloudHsmClusterId) (\s@UpdateCustomKeyStore' {} a -> s {cloudHsmClusterId = a} :: UpdateCustomKeyStore)

-- | Identifies the custom key store that you want to update. Enter the ID of
-- the custom key store. To find the ID of a custom key store, use the
-- DescribeCustomKeyStores operation.
updateCustomKeyStore_customKeyStoreId :: Lens.Lens' UpdateCustomKeyStore Prelude.Text
updateCustomKeyStore_customKeyStoreId = Lens.lens (\UpdateCustomKeyStore' {customKeyStoreId} -> customKeyStoreId) (\s@UpdateCustomKeyStore' {} a -> s {customKeyStoreId = a} :: UpdateCustomKeyStore)

instance Prelude.AWSRequest UpdateCustomKeyStore where
  type
    Rs UpdateCustomKeyStore =
      UpdateCustomKeyStoreResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateCustomKeyStoreResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateCustomKeyStore

instance Prelude.NFData UpdateCustomKeyStore

instance Prelude.ToHeaders UpdateCustomKeyStore where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "TrentService.UpdateCustomKeyStore" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateCustomKeyStore where
  toJSON UpdateCustomKeyStore' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("KeyStorePassword" Prelude..=)
              Prelude.<$> keyStorePassword,
            ("NewCustomKeyStoreName" Prelude..=)
              Prelude.<$> newCustomKeyStoreName',
            ("CloudHsmClusterId" Prelude..=)
              Prelude.<$> cloudHsmClusterId,
            Prelude.Just
              ("CustomKeyStoreId" Prelude..= customKeyStoreId)
          ]
      )

instance Prelude.ToPath UpdateCustomKeyStore where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateCustomKeyStore where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateCustomKeyStoreResponse' smart constructor.
data UpdateCustomKeyStoreResponse = UpdateCustomKeyStoreResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateCustomKeyStoreResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateCustomKeyStoreResponse_httpStatus' - The response's http status code.
newUpdateCustomKeyStoreResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateCustomKeyStoreResponse
newUpdateCustomKeyStoreResponse pHttpStatus_ =
  UpdateCustomKeyStoreResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateCustomKeyStoreResponse_httpStatus :: Lens.Lens' UpdateCustomKeyStoreResponse Prelude.Int
updateCustomKeyStoreResponse_httpStatus = Lens.lens (\UpdateCustomKeyStoreResponse' {httpStatus} -> httpStatus) (\s@UpdateCustomKeyStoreResponse' {} a -> s {httpStatus = a} :: UpdateCustomKeyStoreResponse)

instance Prelude.NFData UpdateCustomKeyStoreResponse
