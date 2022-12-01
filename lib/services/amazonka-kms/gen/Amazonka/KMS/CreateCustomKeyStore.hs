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
-- Module      : Amazonka.KMS.CreateCustomKeyStore
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a
-- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store>
-- that is associated with an
-- <https://docs.aws.amazon.com/cloudhsm/latest/userguide/clusters.html CloudHSM cluster>
-- that you own and manage.
--
-- This operation is part of the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store feature>
-- feature in KMS, which combines the convenience and extensive integration
-- of KMS with the isolation and control of a single-tenant key store.
--
-- Before you create the custom key store, you must assemble the required
-- elements, including an CloudHSM cluster that fulfills the requirements
-- for a custom key store. For details about the required elements, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/create-keystore.html#before-keystore Assemble the Prerequisites>
-- in the /Key Management Service Developer Guide/.
--
-- When the operation completes successfully, it returns the ID of the new
-- custom key store. Before you can use your new custom key store, you need
-- to use the ConnectCustomKeyStore operation to connect the new key store
-- to its CloudHSM cluster. Even if you are not going to use your custom
-- key store immediately, you might want to connect it to verify that all
-- settings are correct and then disconnect it until you are ready to use
-- it.
--
-- For help with failures, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html Troubleshooting a Custom Key Store>
-- in the /Key Management Service Developer Guide/.
--
-- __Cross-account use__: No. You cannot perform this operation on a custom
-- key store in a different Amazon Web Services account.
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
module Amazonka.KMS.CreateCustomKeyStore
  ( -- * Creating a Request
    CreateCustomKeyStore (..),
    newCreateCustomKeyStore,

    -- * Request Lenses
    createCustomKeyStore_keyStorePassword,
    createCustomKeyStore_cloudHsmClusterId,
    createCustomKeyStore_trustAnchorCertificate,
    createCustomKeyStore_customKeyStoreName,

    -- * Destructuring the Response
    CreateCustomKeyStoreResponse (..),
    newCreateCustomKeyStoreResponse,

    -- * Response Lenses
    createCustomKeyStoreResponse_customKeyStoreId,
    createCustomKeyStoreResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.KMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateCustomKeyStore' smart constructor.
data CreateCustomKeyStore = CreateCustomKeyStore'
  { -- | Enter the password of the
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/key-store-concepts.html#concept-kmsuser kmsuser crypto user (CU) account>
    -- in the specified CloudHSM cluster. KMS logs into the cluster as this
    -- user to manage key material on your behalf.
    --
    -- The password must be a string of 7 to 32 characters. Its value is case
    -- sensitive.
    --
    -- This parameter tells KMS the @kmsuser@ account password; it does not
    -- change the password in the CloudHSM cluster.
    keyStorePassword :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | Identifies the CloudHSM cluster for the custom key store. Enter the
    -- cluster ID of any active CloudHSM cluster that is not already associated
    -- with a custom key store. To find the cluster ID, use the
    -- <https://docs.aws.amazon.com/cloudhsm/latest/APIReference/API_DescribeClusters.html DescribeClusters>
    -- operation.
    cloudHsmClusterId :: Prelude.Maybe Prelude.Text,
    -- | Enter the content of the trust anchor certificate for the cluster. This
    -- is the content of the @customerCA.crt@ file that you created when you
    -- <https://docs.aws.amazon.com/cloudhsm/latest/userguide/initialize-cluster.html initialized the cluster>.
    trustAnchorCertificate :: Prelude.Maybe Prelude.Text,
    -- | Specifies a friendly name for the custom key store. The name must be
    -- unique in your Amazon Web Services account.
    customKeyStoreName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCustomKeyStore' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyStorePassword', 'createCustomKeyStore_keyStorePassword' - Enter the password of the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-store-concepts.html#concept-kmsuser kmsuser crypto user (CU) account>
-- in the specified CloudHSM cluster. KMS logs into the cluster as this
-- user to manage key material on your behalf.
--
-- The password must be a string of 7 to 32 characters. Its value is case
-- sensitive.
--
-- This parameter tells KMS the @kmsuser@ account password; it does not
-- change the password in the CloudHSM cluster.
--
-- 'cloudHsmClusterId', 'createCustomKeyStore_cloudHsmClusterId' - Identifies the CloudHSM cluster for the custom key store. Enter the
-- cluster ID of any active CloudHSM cluster that is not already associated
-- with a custom key store. To find the cluster ID, use the
-- <https://docs.aws.amazon.com/cloudhsm/latest/APIReference/API_DescribeClusters.html DescribeClusters>
-- operation.
--
-- 'trustAnchorCertificate', 'createCustomKeyStore_trustAnchorCertificate' - Enter the content of the trust anchor certificate for the cluster. This
-- is the content of the @customerCA.crt@ file that you created when you
-- <https://docs.aws.amazon.com/cloudhsm/latest/userguide/initialize-cluster.html initialized the cluster>.
--
-- 'customKeyStoreName', 'createCustomKeyStore_customKeyStoreName' - Specifies a friendly name for the custom key store. The name must be
-- unique in your Amazon Web Services account.
newCreateCustomKeyStore ::
  -- | 'customKeyStoreName'
  Prelude.Text ->
  CreateCustomKeyStore
newCreateCustomKeyStore pCustomKeyStoreName_ =
  CreateCustomKeyStore'
    { keyStorePassword =
        Prelude.Nothing,
      cloudHsmClusterId = Prelude.Nothing,
      trustAnchorCertificate = Prelude.Nothing,
      customKeyStoreName = pCustomKeyStoreName_
    }

-- | Enter the password of the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-store-concepts.html#concept-kmsuser kmsuser crypto user (CU) account>
-- in the specified CloudHSM cluster. KMS logs into the cluster as this
-- user to manage key material on your behalf.
--
-- The password must be a string of 7 to 32 characters. Its value is case
-- sensitive.
--
-- This parameter tells KMS the @kmsuser@ account password; it does not
-- change the password in the CloudHSM cluster.
createCustomKeyStore_keyStorePassword :: Lens.Lens' CreateCustomKeyStore (Prelude.Maybe Prelude.Text)
createCustomKeyStore_keyStorePassword = Lens.lens (\CreateCustomKeyStore' {keyStorePassword} -> keyStorePassword) (\s@CreateCustomKeyStore' {} a -> s {keyStorePassword = a} :: CreateCustomKeyStore) Prelude.. Lens.mapping Core._Sensitive

-- | Identifies the CloudHSM cluster for the custom key store. Enter the
-- cluster ID of any active CloudHSM cluster that is not already associated
-- with a custom key store. To find the cluster ID, use the
-- <https://docs.aws.amazon.com/cloudhsm/latest/APIReference/API_DescribeClusters.html DescribeClusters>
-- operation.
createCustomKeyStore_cloudHsmClusterId :: Lens.Lens' CreateCustomKeyStore (Prelude.Maybe Prelude.Text)
createCustomKeyStore_cloudHsmClusterId = Lens.lens (\CreateCustomKeyStore' {cloudHsmClusterId} -> cloudHsmClusterId) (\s@CreateCustomKeyStore' {} a -> s {cloudHsmClusterId = a} :: CreateCustomKeyStore)

-- | Enter the content of the trust anchor certificate for the cluster. This
-- is the content of the @customerCA.crt@ file that you created when you
-- <https://docs.aws.amazon.com/cloudhsm/latest/userguide/initialize-cluster.html initialized the cluster>.
createCustomKeyStore_trustAnchorCertificate :: Lens.Lens' CreateCustomKeyStore (Prelude.Maybe Prelude.Text)
createCustomKeyStore_trustAnchorCertificate = Lens.lens (\CreateCustomKeyStore' {trustAnchorCertificate} -> trustAnchorCertificate) (\s@CreateCustomKeyStore' {} a -> s {trustAnchorCertificate = a} :: CreateCustomKeyStore)

-- | Specifies a friendly name for the custom key store. The name must be
-- unique in your Amazon Web Services account.
createCustomKeyStore_customKeyStoreName :: Lens.Lens' CreateCustomKeyStore Prelude.Text
createCustomKeyStore_customKeyStoreName = Lens.lens (\CreateCustomKeyStore' {customKeyStoreName} -> customKeyStoreName) (\s@CreateCustomKeyStore' {} a -> s {customKeyStoreName = a} :: CreateCustomKeyStore)

instance Core.AWSRequest CreateCustomKeyStore where
  type
    AWSResponse CreateCustomKeyStore =
      CreateCustomKeyStoreResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCustomKeyStoreResponse'
            Prelude.<$> (x Core..?> "CustomKeyStoreId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateCustomKeyStore where
  hashWithSalt _salt CreateCustomKeyStore' {..} =
    _salt `Prelude.hashWithSalt` keyStorePassword
      `Prelude.hashWithSalt` cloudHsmClusterId
      `Prelude.hashWithSalt` trustAnchorCertificate
      `Prelude.hashWithSalt` customKeyStoreName

instance Prelude.NFData CreateCustomKeyStore where
  rnf CreateCustomKeyStore' {..} =
    Prelude.rnf keyStorePassword
      `Prelude.seq` Prelude.rnf cloudHsmClusterId
      `Prelude.seq` Prelude.rnf trustAnchorCertificate
      `Prelude.seq` Prelude.rnf customKeyStoreName

instance Core.ToHeaders CreateCustomKeyStore where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "TrentService.CreateCustomKeyStore" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateCustomKeyStore where
  toJSON CreateCustomKeyStore' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("KeyStorePassword" Core..=)
              Prelude.<$> keyStorePassword,
            ("CloudHsmClusterId" Core..=)
              Prelude.<$> cloudHsmClusterId,
            ("TrustAnchorCertificate" Core..=)
              Prelude.<$> trustAnchorCertificate,
            Prelude.Just
              ("CustomKeyStoreName" Core..= customKeyStoreName)
          ]
      )

instance Core.ToPath CreateCustomKeyStore where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateCustomKeyStore where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateCustomKeyStoreResponse' smart constructor.
data CreateCustomKeyStoreResponse = CreateCustomKeyStoreResponse'
  { -- | A unique identifier for the new custom key store.
    customKeyStoreId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData CreateCustomKeyStoreResponse where
  rnf CreateCustomKeyStoreResponse' {..} =
    Prelude.rnf customKeyStoreId
      `Prelude.seq` Prelude.rnf httpStatus
