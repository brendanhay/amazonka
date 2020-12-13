{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.UpdateCustomKeyStore
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the properties of a custom key store. Use the @CustomKeyStoreId@ parameter to identify the custom key store you want to edit. Use the remaining parameters to change the properties of the custom key store.
--
-- You can only update a custom key store that is disconnected. To disconnect the custom key store, use 'DisconnectCustomKeyStore' . To reconnect the custom key store after the update completes, use 'ConnectCustomKeyStore' . To find the connection state of a custom key store, use the 'DescribeCustomKeyStores' operation.
-- Use the parameters of @UpdateCustomKeyStore@ to edit your keystore settings.
--
--     * Use the __NewCustomKeyStoreName__ parameter to change the friendly name of the custom key store to the value that you specify.
--
--
--
--     * Use the __KeyStorePassword__ parameter tell AWS KMS the current password of the <https://docs.aws.amazon.com/kms/latest/developerguide/key-store-concepts.html#concept-kmsuser @kmsuser@ crypto user (CU)> in the associated AWS CloudHSM cluster. You can use this parameter to <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html#fix-keystore-password fix connection failures> that occur when AWS KMS cannot log into the associated cluster because the @kmsuser@ password has changed. This value does not change the password in the AWS CloudHSM cluster.
--
--
--
--     * Use the __CloudHsmClusterId__ parameter to associate the custom key store with a different, but related, AWS CloudHSM cluster. You can use this parameter to repair a custom key store if its AWS CloudHSM cluster becomes corrupted or is deleted, or when you need to create or restore a cluster from a backup.
--
--
-- If the operation succeeds, it returns a JSON object with no properties.
-- This operation is part of the <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html Custom Key Store feature> feature in AWS KMS, which combines the convenience and extensive integration of AWS KMS with the isolation and control of a single-tenant key store.
module Network.AWS.KMS.UpdateCustomKeyStore
  ( -- * Creating a request
    UpdateCustomKeyStore (..),
    mkUpdateCustomKeyStore,

    -- ** Request lenses
    ucksKeyStorePassword,
    ucksCloudHSMClusterId,
    ucksNewCustomKeyStoreName,
    ucksCustomKeyStoreId,

    -- * Destructuring the response
    UpdateCustomKeyStoreResponse (..),
    mkUpdateCustomKeyStoreResponse,

    -- ** Response lenses
    ucksrsResponseStatus,
  )
where

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateCustomKeyStore' smart constructor.
data UpdateCustomKeyStore = UpdateCustomKeyStore'
  { -- | Enter the current password of the @kmsuser@ crypto user (CU) in the AWS CloudHSM cluster that is associated with the custom key store.
    --
    -- This parameter tells AWS KMS the current password of the @kmsuser@ crypto user (CU). It does not set or change the password of any users in the AWS CloudHSM cluster.
    keyStorePassword :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | Associates the custom key store with a related AWS CloudHSM cluster.
    --
    -- Enter the cluster ID of the cluster that you used to create the custom key store or a cluster that shares a backup history and has the same cluster certificate as the original cluster. You cannot use this parameter to associate a custom key store with an unrelated cluster. In addition, the replacement cluster must <https://docs.aws.amazon.com/kms/latest/developerguide/create-keystore.html#before-keystore fulfill the requirements> for a cluster associated with a custom key store. To view the cluster certificate of a cluster, use the <https://docs.aws.amazon.com/cloudhsm/latest/APIReference/API_DescribeClusters.html DescribeClusters> operation.
    cloudHSMClusterId :: Lude.Maybe Lude.Text,
    -- | Changes the friendly name of the custom key store to the value that you specify. The custom key store name must be unique in the AWS account.
    newCustomKeyStoreName :: Lude.Maybe Lude.Text,
    -- | Identifies the custom key store that you want to update. Enter the ID of the custom key store. To find the ID of a custom key store, use the 'DescribeCustomKeyStores' operation.
    customKeyStoreId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateCustomKeyStore' with the minimum fields required to make a request.
--
-- * 'keyStorePassword' - Enter the current password of the @kmsuser@ crypto user (CU) in the AWS CloudHSM cluster that is associated with the custom key store.
--
-- This parameter tells AWS KMS the current password of the @kmsuser@ crypto user (CU). It does not set or change the password of any users in the AWS CloudHSM cluster.
-- * 'cloudHSMClusterId' - Associates the custom key store with a related AWS CloudHSM cluster.
--
-- Enter the cluster ID of the cluster that you used to create the custom key store or a cluster that shares a backup history and has the same cluster certificate as the original cluster. You cannot use this parameter to associate a custom key store with an unrelated cluster. In addition, the replacement cluster must <https://docs.aws.amazon.com/kms/latest/developerguide/create-keystore.html#before-keystore fulfill the requirements> for a cluster associated with a custom key store. To view the cluster certificate of a cluster, use the <https://docs.aws.amazon.com/cloudhsm/latest/APIReference/API_DescribeClusters.html DescribeClusters> operation.
-- * 'newCustomKeyStoreName' - Changes the friendly name of the custom key store to the value that you specify. The custom key store name must be unique in the AWS account.
-- * 'customKeyStoreId' - Identifies the custom key store that you want to update. Enter the ID of the custom key store. To find the ID of a custom key store, use the 'DescribeCustomKeyStores' operation.
mkUpdateCustomKeyStore ::
  -- | 'customKeyStoreId'
  Lude.Text ->
  UpdateCustomKeyStore
mkUpdateCustomKeyStore pCustomKeyStoreId_ =
  UpdateCustomKeyStore'
    { keyStorePassword = Lude.Nothing,
      cloudHSMClusterId = Lude.Nothing,
      newCustomKeyStoreName = Lude.Nothing,
      customKeyStoreId = pCustomKeyStoreId_
    }

-- | Enter the current password of the @kmsuser@ crypto user (CU) in the AWS CloudHSM cluster that is associated with the custom key store.
--
-- This parameter tells AWS KMS the current password of the @kmsuser@ crypto user (CU). It does not set or change the password of any users in the AWS CloudHSM cluster.
--
-- /Note:/ Consider using 'keyStorePassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucksKeyStorePassword :: Lens.Lens' UpdateCustomKeyStore (Lude.Maybe (Lude.Sensitive Lude.Text))
ucksKeyStorePassword = Lens.lens (keyStorePassword :: UpdateCustomKeyStore -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {keyStorePassword = a} :: UpdateCustomKeyStore)
{-# DEPRECATED ucksKeyStorePassword "Use generic-lens or generic-optics with 'keyStorePassword' instead." #-}

-- | Associates the custom key store with a related AWS CloudHSM cluster.
--
-- Enter the cluster ID of the cluster that you used to create the custom key store or a cluster that shares a backup history and has the same cluster certificate as the original cluster. You cannot use this parameter to associate a custom key store with an unrelated cluster. In addition, the replacement cluster must <https://docs.aws.amazon.com/kms/latest/developerguide/create-keystore.html#before-keystore fulfill the requirements> for a cluster associated with a custom key store. To view the cluster certificate of a cluster, use the <https://docs.aws.amazon.com/cloudhsm/latest/APIReference/API_DescribeClusters.html DescribeClusters> operation.
--
-- /Note:/ Consider using 'cloudHSMClusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucksCloudHSMClusterId :: Lens.Lens' UpdateCustomKeyStore (Lude.Maybe Lude.Text)
ucksCloudHSMClusterId = Lens.lens (cloudHSMClusterId :: UpdateCustomKeyStore -> Lude.Maybe Lude.Text) (\s a -> s {cloudHSMClusterId = a} :: UpdateCustomKeyStore)
{-# DEPRECATED ucksCloudHSMClusterId "Use generic-lens or generic-optics with 'cloudHSMClusterId' instead." #-}

-- | Changes the friendly name of the custom key store to the value that you specify. The custom key store name must be unique in the AWS account.
--
-- /Note:/ Consider using 'newCustomKeyStoreName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucksNewCustomKeyStoreName :: Lens.Lens' UpdateCustomKeyStore (Lude.Maybe Lude.Text)
ucksNewCustomKeyStoreName = Lens.lens (newCustomKeyStoreName :: UpdateCustomKeyStore -> Lude.Maybe Lude.Text) (\s a -> s {newCustomKeyStoreName = a} :: UpdateCustomKeyStore)
{-# DEPRECATED ucksNewCustomKeyStoreName "Use generic-lens or generic-optics with 'newCustomKeyStoreName' instead." #-}

-- | Identifies the custom key store that you want to update. Enter the ID of the custom key store. To find the ID of a custom key store, use the 'DescribeCustomKeyStores' operation.
--
-- /Note:/ Consider using 'customKeyStoreId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucksCustomKeyStoreId :: Lens.Lens' UpdateCustomKeyStore Lude.Text
ucksCustomKeyStoreId = Lens.lens (customKeyStoreId :: UpdateCustomKeyStore -> Lude.Text) (\s a -> s {customKeyStoreId = a} :: UpdateCustomKeyStore)
{-# DEPRECATED ucksCustomKeyStoreId "Use generic-lens or generic-optics with 'customKeyStoreId' instead." #-}

instance Lude.AWSRequest UpdateCustomKeyStore where
  type Rs UpdateCustomKeyStore = UpdateCustomKeyStoreResponse
  request = Req.postJSON kmsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateCustomKeyStoreResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateCustomKeyStore where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("TrentService.UpdateCustomKeyStore" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateCustomKeyStore where
  toJSON UpdateCustomKeyStore' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("KeyStorePassword" Lude..=) Lude.<$> keyStorePassword,
            ("CloudHsmClusterId" Lude..=) Lude.<$> cloudHSMClusterId,
            ("NewCustomKeyStoreName" Lude..=) Lude.<$> newCustomKeyStoreName,
            Lude.Just ("CustomKeyStoreId" Lude..= customKeyStoreId)
          ]
      )

instance Lude.ToPath UpdateCustomKeyStore where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateCustomKeyStore where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateCustomKeyStoreResponse' smart constructor.
newtype UpdateCustomKeyStoreResponse = UpdateCustomKeyStoreResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateCustomKeyStoreResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateCustomKeyStoreResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateCustomKeyStoreResponse
mkUpdateCustomKeyStoreResponse pResponseStatus_ =
  UpdateCustomKeyStoreResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucksrsResponseStatus :: Lens.Lens' UpdateCustomKeyStoreResponse Lude.Int
ucksrsResponseStatus = Lens.lens (responseStatus :: UpdateCustomKeyStoreResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateCustomKeyStoreResponse)
{-# DEPRECATED ucksrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
