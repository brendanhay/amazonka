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
-- Module      : Network.AWS.KMS.CreateCustomKeyStore
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a
-- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store>
-- that is associated with an
-- <https://docs.aws.amazon.com/cloudhsm/latest/userguide/clusters.html AWS CloudHSM cluster>
-- that you own and manage.
--
-- This operation is part of the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html Custom Key Store feature>
-- feature in AWS KMS, which combines the convenience and extensive
-- integration of AWS KMS with the isolation and control of a single-tenant
-- key store.
--
-- Before you create the custom key store, you must assemble the required
-- elements, including an AWS CloudHSM cluster that fulfills the
-- requirements for a custom key store. For details about the required
-- elements, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/create-keystore.html#before-keystore Assemble the Prerequisites>
-- in the /AWS Key Management Service Developer Guide/.
--
-- When the operation completes successfully, it returns the ID of the new
-- custom key store. Before you can use your new custom key store, you need
-- to use the ConnectCustomKeyStore operation to connect the new key store
-- to its AWS CloudHSM cluster. Even if you are not going to use your
-- custom key store immediately, you might want to connect it to verify
-- that all settings are correct and then disconnect it until you are ready
-- to use it.
--
-- For help with failures, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html Troubleshooting a Custom Key Store>
-- in the /AWS Key Management Service Developer Guide/.
--
-- __Cross-account use__: No. You cannot perform this operation on a custom
-- key store in a different AWS account.
--
-- __Required permissions__:
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:CreateCustomKeyStore>
-- (IAM policy).
--
-- __Related operations:__
--
-- -   ConnectCustomKeyStore
--
-- -   DeleteCustomKeyStore
--
-- -   DescribeCustomKeyStores
--
-- -   DisconnectCustomKeyStore
--
-- -   UpdateCustomKeyStore
module Network.AWS.KMS.CreateCustomKeyStore
  ( -- * Creating a Request
    CreateCustomKeyStore (..),
    newCreateCustomKeyStore,

    -- * Request Lenses
    createCustomKeyStore_customKeyStoreName,
    createCustomKeyStore_cloudHsmClusterId,
    createCustomKeyStore_trustAnchorCertificate,
    createCustomKeyStore_keyStorePassword,

    -- * Destructuring the Response
    CreateCustomKeyStoreResponse (..),
    newCreateCustomKeyStoreResponse,

    -- * Response Lenses
    createCustomKeyStoreResponse_customKeyStoreId,
    createCustomKeyStoreResponse_httpStatus,
  )
where

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateCustomKeyStore' smart constructor.
data CreateCustomKeyStore = CreateCustomKeyStore'
  { -- | Specifies a friendly name for the custom key store. The name must be
    -- unique in your AWS account.
    customKeyStoreName :: Prelude.Text,
    -- | Identifies the AWS CloudHSM cluster for the custom key store. Enter the
    -- cluster ID of any active AWS CloudHSM cluster that is not already
    -- associated with a custom key store. To find the cluster ID, use the
    -- <https://docs.aws.amazon.com/cloudhsm/latest/APIReference/API_DescribeClusters.html DescribeClusters>
    -- operation.
    cloudHsmClusterId :: Prelude.Text,
    -- | Enter the content of the trust anchor certificate for the cluster. This
    -- is the content of the @customerCA.crt@ file that you created when you
    -- <https://docs.aws.amazon.com/cloudhsm/latest/userguide/initialize-cluster.html initialized the cluster>.
    trustAnchorCertificate :: Prelude.Text,
    -- | Enter the password of the
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/key-store-concepts.html#concept-kmsuser kmsuser crypto user (CU) account>
    -- in the specified AWS CloudHSM cluster. AWS KMS logs into the cluster as
    -- this user to manage key material on your behalf.
    --
    -- The password must be a string of 7 to 32 characters. Its value is case
    -- sensitive.
    --
    -- This parameter tells AWS KMS the @kmsuser@ account password; it does not
    -- change the password in the AWS CloudHSM cluster.
    keyStorePassword :: Prelude.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateCustomKeyStore' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customKeyStoreName', 'createCustomKeyStore_customKeyStoreName' - Specifies a friendly name for the custom key store. The name must be
-- unique in your AWS account.
--
-- 'cloudHsmClusterId', 'createCustomKeyStore_cloudHsmClusterId' - Identifies the AWS CloudHSM cluster for the custom key store. Enter the
-- cluster ID of any active AWS CloudHSM cluster that is not already
-- associated with a custom key store. To find the cluster ID, use the
-- <https://docs.aws.amazon.com/cloudhsm/latest/APIReference/API_DescribeClusters.html DescribeClusters>
-- operation.
--
-- 'trustAnchorCertificate', 'createCustomKeyStore_trustAnchorCertificate' - Enter the content of the trust anchor certificate for the cluster. This
-- is the content of the @customerCA.crt@ file that you created when you
-- <https://docs.aws.amazon.com/cloudhsm/latest/userguide/initialize-cluster.html initialized the cluster>.
--
-- 'keyStorePassword', 'createCustomKeyStore_keyStorePassword' - Enter the password of the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-store-concepts.html#concept-kmsuser kmsuser crypto user (CU) account>
-- in the specified AWS CloudHSM cluster. AWS KMS logs into the cluster as
-- this user to manage key material on your behalf.
--
-- The password must be a string of 7 to 32 characters. Its value is case
-- sensitive.
--
-- This parameter tells AWS KMS the @kmsuser@ account password; it does not
-- change the password in the AWS CloudHSM cluster.
newCreateCustomKeyStore ::
  -- | 'customKeyStoreName'
  Prelude.Text ->
  -- | 'cloudHsmClusterId'
  Prelude.Text ->
  -- | 'trustAnchorCertificate'
  Prelude.Text ->
  -- | 'keyStorePassword'
  Prelude.Text ->
  CreateCustomKeyStore
newCreateCustomKeyStore
  pCustomKeyStoreName_
  pCloudHsmClusterId_
  pTrustAnchorCertificate_
  pKeyStorePassword_ =
    CreateCustomKeyStore'
      { customKeyStoreName =
          pCustomKeyStoreName_,
        cloudHsmClusterId = pCloudHsmClusterId_,
        trustAnchorCertificate = pTrustAnchorCertificate_,
        keyStorePassword =
          Prelude._Sensitive Lens.# pKeyStorePassword_
      }

-- | Specifies a friendly name for the custom key store. The name must be
-- unique in your AWS account.
createCustomKeyStore_customKeyStoreName :: Lens.Lens' CreateCustomKeyStore Prelude.Text
createCustomKeyStore_customKeyStoreName = Lens.lens (\CreateCustomKeyStore' {customKeyStoreName} -> customKeyStoreName) (\s@CreateCustomKeyStore' {} a -> s {customKeyStoreName = a} :: CreateCustomKeyStore)

-- | Identifies the AWS CloudHSM cluster for the custom key store. Enter the
-- cluster ID of any active AWS CloudHSM cluster that is not already
-- associated with a custom key store. To find the cluster ID, use the
-- <https://docs.aws.amazon.com/cloudhsm/latest/APIReference/API_DescribeClusters.html DescribeClusters>
-- operation.
createCustomKeyStore_cloudHsmClusterId :: Lens.Lens' CreateCustomKeyStore Prelude.Text
createCustomKeyStore_cloudHsmClusterId = Lens.lens (\CreateCustomKeyStore' {cloudHsmClusterId} -> cloudHsmClusterId) (\s@CreateCustomKeyStore' {} a -> s {cloudHsmClusterId = a} :: CreateCustomKeyStore)

-- | Enter the content of the trust anchor certificate for the cluster. This
-- is the content of the @customerCA.crt@ file that you created when you
-- <https://docs.aws.amazon.com/cloudhsm/latest/userguide/initialize-cluster.html initialized the cluster>.
createCustomKeyStore_trustAnchorCertificate :: Lens.Lens' CreateCustomKeyStore Prelude.Text
createCustomKeyStore_trustAnchorCertificate = Lens.lens (\CreateCustomKeyStore' {trustAnchorCertificate} -> trustAnchorCertificate) (\s@CreateCustomKeyStore' {} a -> s {trustAnchorCertificate = a} :: CreateCustomKeyStore)

-- | Enter the password of the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-store-concepts.html#concept-kmsuser kmsuser crypto user (CU) account>
-- in the specified AWS CloudHSM cluster. AWS KMS logs into the cluster as
-- this user to manage key material on your behalf.
--
-- The password must be a string of 7 to 32 characters. Its value is case
-- sensitive.
--
-- This parameter tells AWS KMS the @kmsuser@ account password; it does not
-- change the password in the AWS CloudHSM cluster.
createCustomKeyStore_keyStorePassword :: Lens.Lens' CreateCustomKeyStore Prelude.Text
createCustomKeyStore_keyStorePassword = Lens.lens (\CreateCustomKeyStore' {keyStorePassword} -> keyStorePassword) (\s@CreateCustomKeyStore' {} a -> s {keyStorePassword = a} :: CreateCustomKeyStore) Prelude.. Prelude._Sensitive

instance Prelude.AWSRequest CreateCustomKeyStore where
  type
    Rs CreateCustomKeyStore =
      CreateCustomKeyStoreResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCustomKeyStoreResponse'
            Prelude.<$> (x Prelude..?> "CustomKeyStoreId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateCustomKeyStore

instance Prelude.NFData CreateCustomKeyStore

instance Prelude.ToHeaders CreateCustomKeyStore where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "TrentService.CreateCustomKeyStore" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateCustomKeyStore where
  toJSON CreateCustomKeyStore' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("CustomKeyStoreName" Prelude..= customKeyStoreName),
            Prelude.Just
              ("CloudHsmClusterId" Prelude..= cloudHsmClusterId),
            Prelude.Just
              ( "TrustAnchorCertificate"
                  Prelude..= trustAnchorCertificate
              ),
            Prelude.Just
              ("KeyStorePassword" Prelude..= keyStorePassword)
          ]
      )

instance Prelude.ToPath CreateCustomKeyStore where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateCustomKeyStore where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateCustomKeyStoreResponse' smart constructor.
data CreateCustomKeyStoreResponse = CreateCustomKeyStoreResponse'
  { -- | A unique identifier for the new custom key store.
    customKeyStoreId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateCustomKeyStoreResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customKeyStoreId', 'createCustomKeyStoreResponse_customKeyStoreId' - A unique identifier for the new custom key store.
--
-- 'httpStatus', 'createCustomKeyStoreResponse_httpStatus' - The response's http status code.
newCreateCustomKeyStoreResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateCustomKeyStoreResponse
newCreateCustomKeyStoreResponse pHttpStatus_ =
  CreateCustomKeyStoreResponse'
    { customKeyStoreId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A unique identifier for the new custom key store.
createCustomKeyStoreResponse_customKeyStoreId :: Lens.Lens' CreateCustomKeyStoreResponse (Prelude.Maybe Prelude.Text)
createCustomKeyStoreResponse_customKeyStoreId = Lens.lens (\CreateCustomKeyStoreResponse' {customKeyStoreId} -> customKeyStoreId) (\s@CreateCustomKeyStoreResponse' {} a -> s {customKeyStoreId = a} :: CreateCustomKeyStoreResponse)

-- | The response's http status code.
createCustomKeyStoreResponse_httpStatus :: Lens.Lens' CreateCustomKeyStoreResponse Prelude.Int
createCustomKeyStoreResponse_httpStatus = Lens.lens (\CreateCustomKeyStoreResponse' {httpStatus} -> httpStatus) (\s@CreateCustomKeyStoreResponse' {} a -> s {httpStatus = a} :: CreateCustomKeyStoreResponse)

instance Prelude.NFData CreateCustomKeyStoreResponse
