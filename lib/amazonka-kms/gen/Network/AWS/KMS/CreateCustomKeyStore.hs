{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.CreateCustomKeyStore
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store> that is associated with an <https://docs.aws.amazon.com/cloudhsm/latest/userguide/clusters.html AWS CloudHSM cluster> that you own and manage.
--
-- This operation is part of the <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html Custom Key Store feature> feature in AWS KMS, which combines the convenience and extensive integration of AWS KMS with the isolation and control of a single-tenant key store.
-- Before you create the custom key store, you must assemble the required elements, including an AWS CloudHSM cluster that fulfills the requirements for a custom key store. For details about the required elements, see <https://docs.aws.amazon.com/kms/latest/developerguide/create-keystore.html#before-keystore Assemble the Prerequisites> in the /AWS Key Management Service Developer Guide/ .
-- When the operation completes successfully, it returns the ID of the new custom key store. Before you can use your new custom key store, you need to use the 'ConnectCustomKeyStore' operation to connect the new key store to its AWS CloudHSM cluster. Even if you are not going to use your custom key store immediately, you might want to connect it to verify that all settings are correct and then disconnect it until you are ready to use it.
-- For help with failures, see <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html Troubleshooting a Custom Key Store> in the /AWS Key Management Service Developer Guide/ .
module Network.AWS.KMS.CreateCustomKeyStore
  ( -- * Creating a request
    CreateCustomKeyStore (..),
    mkCreateCustomKeyStore,

    -- ** Request lenses
    ccksCustomKeyStoreName,
    ccksCloudHSMClusterId,
    ccksTrustAnchorCertificate,
    ccksKeyStorePassword,

    -- * Destructuring the response
    CreateCustomKeyStoreResponse (..),
    mkCreateCustomKeyStoreResponse,

    -- ** Response lenses
    ccksrsCustomKeyStoreId,
    ccksrsResponseStatus,
  )
where

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateCustomKeyStore' smart constructor.
data CreateCustomKeyStore = CreateCustomKeyStore'
  { customKeyStoreName ::
      Lude.Text,
    cloudHSMClusterId :: Lude.Text,
    trustAnchorCertificate :: Lude.Text,
    keyStorePassword :: Lude.Sensitive Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCustomKeyStore' with the minimum fields required to make a request.
--
-- * 'cloudHSMClusterId' - Identifies the AWS CloudHSM cluster for the custom key store. Enter the cluster ID of any active AWS CloudHSM cluster that is not already associated with a custom key store. To find the cluster ID, use the <https://docs.aws.amazon.com/cloudhsm/latest/APIReference/API_DescribeClusters.html DescribeClusters> operation.
-- * 'customKeyStoreName' - Specifies a friendly name for the custom key store. The name must be unique in your AWS account.
-- * 'keyStorePassword' - Enter the password of the <https://docs.aws.amazon.com/kms/latest/developerguide/key-store-concepts.html#concept-kmsuser @kmsuser@ crypto user (CU) account> in the specified AWS CloudHSM cluster. AWS KMS logs into the cluster as this user to manage key material on your behalf.
--
-- The password must be a string of 7 to 32 characters. Its value is case sensitive.
-- This parameter tells AWS KMS the @kmsuser@ account password; it does not change the password in the AWS CloudHSM cluster.
-- * 'trustAnchorCertificate' - Enter the content of the trust anchor certificate for the cluster. This is the content of the @customerCA.crt@ file that you created when you <https://docs.aws.amazon.com/cloudhsm/latest/userguide/initialize-cluster.html initialized the cluster> .
mkCreateCustomKeyStore ::
  -- | 'customKeyStoreName'
  Lude.Text ->
  -- | 'cloudHSMClusterId'
  Lude.Text ->
  -- | 'trustAnchorCertificate'
  Lude.Text ->
  -- | 'keyStorePassword'
  Lude.Sensitive Lude.Text ->
  CreateCustomKeyStore
mkCreateCustomKeyStore
  pCustomKeyStoreName_
  pCloudHSMClusterId_
  pTrustAnchorCertificate_
  pKeyStorePassword_ =
    CreateCustomKeyStore'
      { customKeyStoreName = pCustomKeyStoreName_,
        cloudHSMClusterId = pCloudHSMClusterId_,
        trustAnchorCertificate = pTrustAnchorCertificate_,
        keyStorePassword = pKeyStorePassword_
      }

-- | Specifies a friendly name for the custom key store. The name must be unique in your AWS account.
--
-- /Note:/ Consider using 'customKeyStoreName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccksCustomKeyStoreName :: Lens.Lens' CreateCustomKeyStore Lude.Text
ccksCustomKeyStoreName = Lens.lens (customKeyStoreName :: CreateCustomKeyStore -> Lude.Text) (\s a -> s {customKeyStoreName = a} :: CreateCustomKeyStore)
{-# DEPRECATED ccksCustomKeyStoreName "Use generic-lens or generic-optics with 'customKeyStoreName' instead." #-}

-- | Identifies the AWS CloudHSM cluster for the custom key store. Enter the cluster ID of any active AWS CloudHSM cluster that is not already associated with a custom key store. To find the cluster ID, use the <https://docs.aws.amazon.com/cloudhsm/latest/APIReference/API_DescribeClusters.html DescribeClusters> operation.
--
-- /Note:/ Consider using 'cloudHSMClusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccksCloudHSMClusterId :: Lens.Lens' CreateCustomKeyStore Lude.Text
ccksCloudHSMClusterId = Lens.lens (cloudHSMClusterId :: CreateCustomKeyStore -> Lude.Text) (\s a -> s {cloudHSMClusterId = a} :: CreateCustomKeyStore)
{-# DEPRECATED ccksCloudHSMClusterId "Use generic-lens or generic-optics with 'cloudHSMClusterId' instead." #-}

-- | Enter the content of the trust anchor certificate for the cluster. This is the content of the @customerCA.crt@ file that you created when you <https://docs.aws.amazon.com/cloudhsm/latest/userguide/initialize-cluster.html initialized the cluster> .
--
-- /Note:/ Consider using 'trustAnchorCertificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccksTrustAnchorCertificate :: Lens.Lens' CreateCustomKeyStore Lude.Text
ccksTrustAnchorCertificate = Lens.lens (trustAnchorCertificate :: CreateCustomKeyStore -> Lude.Text) (\s a -> s {trustAnchorCertificate = a} :: CreateCustomKeyStore)
{-# DEPRECATED ccksTrustAnchorCertificate "Use generic-lens or generic-optics with 'trustAnchorCertificate' instead." #-}

-- | Enter the password of the <https://docs.aws.amazon.com/kms/latest/developerguide/key-store-concepts.html#concept-kmsuser @kmsuser@ crypto user (CU) account> in the specified AWS CloudHSM cluster. AWS KMS logs into the cluster as this user to manage key material on your behalf.
--
-- The password must be a string of 7 to 32 characters. Its value is case sensitive.
-- This parameter tells AWS KMS the @kmsuser@ account password; it does not change the password in the AWS CloudHSM cluster.
--
-- /Note:/ Consider using 'keyStorePassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccksKeyStorePassword :: Lens.Lens' CreateCustomKeyStore (Lude.Sensitive Lude.Text)
ccksKeyStorePassword = Lens.lens (keyStorePassword :: CreateCustomKeyStore -> Lude.Sensitive Lude.Text) (\s a -> s {keyStorePassword = a} :: CreateCustomKeyStore)
{-# DEPRECATED ccksKeyStorePassword "Use generic-lens or generic-optics with 'keyStorePassword' instead." #-}

instance Lude.AWSRequest CreateCustomKeyStore where
  type Rs CreateCustomKeyStore = CreateCustomKeyStoreResponse
  request = Req.postJSON kmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateCustomKeyStoreResponse'
            Lude.<$> (x Lude..?> "CustomKeyStoreId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateCustomKeyStore where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("TrentService.CreateCustomKeyStore" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateCustomKeyStore where
  toJSON CreateCustomKeyStore' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("CustomKeyStoreName" Lude..= customKeyStoreName),
            Lude.Just ("CloudHsmClusterId" Lude..= cloudHSMClusterId),
            Lude.Just
              ("TrustAnchorCertificate" Lude..= trustAnchorCertificate),
            Lude.Just ("KeyStorePassword" Lude..= keyStorePassword)
          ]
      )

instance Lude.ToPath CreateCustomKeyStore where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateCustomKeyStore where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateCustomKeyStoreResponse' smart constructor.
data CreateCustomKeyStoreResponse = CreateCustomKeyStoreResponse'
  { customKeyStoreId ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCustomKeyStoreResponse' with the minimum fields required to make a request.
--
-- * 'customKeyStoreId' - A unique identifier for the new custom key store.
-- * 'responseStatus' - The response status code.
mkCreateCustomKeyStoreResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateCustomKeyStoreResponse
mkCreateCustomKeyStoreResponse pResponseStatus_ =
  CreateCustomKeyStoreResponse'
    { customKeyStoreId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A unique identifier for the new custom key store.
--
-- /Note:/ Consider using 'customKeyStoreId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccksrsCustomKeyStoreId :: Lens.Lens' CreateCustomKeyStoreResponse (Lude.Maybe Lude.Text)
ccksrsCustomKeyStoreId = Lens.lens (customKeyStoreId :: CreateCustomKeyStoreResponse -> Lude.Maybe Lude.Text) (\s a -> s {customKeyStoreId = a} :: CreateCustomKeyStoreResponse)
{-# DEPRECATED ccksrsCustomKeyStoreId "Use generic-lens or generic-optics with 'customKeyStoreId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccksrsResponseStatus :: Lens.Lens' CreateCustomKeyStoreResponse Lude.Int
ccksrsResponseStatus = Lens.lens (responseStatus :: CreateCustomKeyStoreResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateCustomKeyStoreResponse)
{-# DEPRECATED ccksrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
