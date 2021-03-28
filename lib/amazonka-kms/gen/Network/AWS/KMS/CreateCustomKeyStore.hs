{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateCustomKeyStore (..)
    , mkCreateCustomKeyStore
    -- ** Request lenses
    , ccksCustomKeyStoreName
    , ccksCloudHsmClusterId
    , ccksTrustAnchorCertificate
    , ccksKeyStorePassword

    -- * Destructuring the response
    , CreateCustomKeyStoreResponse (..)
    , mkCreateCustomKeyStoreResponse
    -- ** Response lenses
    , ccksrrsCustomKeyStoreId
    , ccksrrsResponseStatus
    ) where

import qualified Network.AWS.KMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateCustomKeyStore' smart constructor.
data CreateCustomKeyStore = CreateCustomKeyStore'
  { customKeyStoreName :: Types.CustomKeyStoreNameType
    -- ^ Specifies a friendly name for the custom key store. The name must be unique in your AWS account.
  , cloudHsmClusterId :: Types.CloudHsmClusterIdType
    -- ^ Identifies the AWS CloudHSM cluster for the custom key store. Enter the cluster ID of any active AWS CloudHSM cluster that is not already associated with a custom key store. To find the cluster ID, use the <https://docs.aws.amazon.com/cloudhsm/latest/APIReference/API_DescribeClusters.html DescribeClusters> operation.
  , trustAnchorCertificate :: Types.TrustAnchorCertificateType
    -- ^ Enter the content of the trust anchor certificate for the cluster. This is the content of the @customerCA.crt@ file that you created when you <https://docs.aws.amazon.com/cloudhsm/latest/userguide/initialize-cluster.html initialized the cluster> .
  , keyStorePassword :: Types.KeyStorePasswordType
    -- ^ Enter the password of the <https://docs.aws.amazon.com/kms/latest/developerguide/key-store-concepts.html#concept-kmsuser @kmsuser@ crypto user (CU) account> in the specified AWS CloudHSM cluster. AWS KMS logs into the cluster as this user to manage key material on your behalf.
--
-- The password must be a string of 7 to 32 characters. Its value is case sensitive.
-- This parameter tells AWS KMS the @kmsuser@ account password; it does not change the password in the AWS CloudHSM cluster.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCustomKeyStore' value with any optional fields omitted.
mkCreateCustomKeyStore
    :: Types.CustomKeyStoreNameType -- ^ 'customKeyStoreName'
    -> Types.CloudHsmClusterIdType -- ^ 'cloudHsmClusterId'
    -> Types.TrustAnchorCertificateType -- ^ 'trustAnchorCertificate'
    -> Types.KeyStorePasswordType -- ^ 'keyStorePassword'
    -> CreateCustomKeyStore
mkCreateCustomKeyStore customKeyStoreName cloudHsmClusterId
  trustAnchorCertificate keyStorePassword
  = CreateCustomKeyStore'{customKeyStoreName, cloudHsmClusterId,
                          trustAnchorCertificate, keyStorePassword}

-- | Specifies a friendly name for the custom key store. The name must be unique in your AWS account.
--
-- /Note:/ Consider using 'customKeyStoreName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccksCustomKeyStoreName :: Lens.Lens' CreateCustomKeyStore Types.CustomKeyStoreNameType
ccksCustomKeyStoreName = Lens.field @"customKeyStoreName"
{-# INLINEABLE ccksCustomKeyStoreName #-}
{-# DEPRECATED customKeyStoreName "Use generic-lens or generic-optics with 'customKeyStoreName' instead"  #-}

-- | Identifies the AWS CloudHSM cluster for the custom key store. Enter the cluster ID of any active AWS CloudHSM cluster that is not already associated with a custom key store. To find the cluster ID, use the <https://docs.aws.amazon.com/cloudhsm/latest/APIReference/API_DescribeClusters.html DescribeClusters> operation.
--
-- /Note:/ Consider using 'cloudHsmClusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccksCloudHsmClusterId :: Lens.Lens' CreateCustomKeyStore Types.CloudHsmClusterIdType
ccksCloudHsmClusterId = Lens.field @"cloudHsmClusterId"
{-# INLINEABLE ccksCloudHsmClusterId #-}
{-# DEPRECATED cloudHsmClusterId "Use generic-lens or generic-optics with 'cloudHsmClusterId' instead"  #-}

-- | Enter the content of the trust anchor certificate for the cluster. This is the content of the @customerCA.crt@ file that you created when you <https://docs.aws.amazon.com/cloudhsm/latest/userguide/initialize-cluster.html initialized the cluster> .
--
-- /Note:/ Consider using 'trustAnchorCertificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccksTrustAnchorCertificate :: Lens.Lens' CreateCustomKeyStore Types.TrustAnchorCertificateType
ccksTrustAnchorCertificate = Lens.field @"trustAnchorCertificate"
{-# INLINEABLE ccksTrustAnchorCertificate #-}
{-# DEPRECATED trustAnchorCertificate "Use generic-lens or generic-optics with 'trustAnchorCertificate' instead"  #-}

-- | Enter the password of the <https://docs.aws.amazon.com/kms/latest/developerguide/key-store-concepts.html#concept-kmsuser @kmsuser@ crypto user (CU) account> in the specified AWS CloudHSM cluster. AWS KMS logs into the cluster as this user to manage key material on your behalf.
--
-- The password must be a string of 7 to 32 characters. Its value is case sensitive.
-- This parameter tells AWS KMS the @kmsuser@ account password; it does not change the password in the AWS CloudHSM cluster.
--
-- /Note:/ Consider using 'keyStorePassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccksKeyStorePassword :: Lens.Lens' CreateCustomKeyStore Types.KeyStorePasswordType
ccksKeyStorePassword = Lens.field @"keyStorePassword"
{-# INLINEABLE ccksKeyStorePassword #-}
{-# DEPRECATED keyStorePassword "Use generic-lens or generic-optics with 'keyStorePassword' instead"  #-}

instance Core.ToQuery CreateCustomKeyStore where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateCustomKeyStore where
        toHeaders CreateCustomKeyStore{..}
          = Core.pure ("X-Amz-Target", "TrentService.CreateCustomKeyStore")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateCustomKeyStore where
        toJSON CreateCustomKeyStore{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("CustomKeyStoreName" Core..= customKeyStoreName),
                  Core.Just ("CloudHsmClusterId" Core..= cloudHsmClusterId),
                  Core.Just
                    ("TrustAnchorCertificate" Core..= trustAnchorCertificate),
                  Core.Just ("KeyStorePassword" Core..= keyStorePassword)])

instance Core.AWSRequest CreateCustomKeyStore where
        type Rs CreateCustomKeyStore = CreateCustomKeyStoreResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateCustomKeyStoreResponse' Core.<$>
                   (x Core..:? "CustomKeyStoreId") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateCustomKeyStoreResponse' smart constructor.
data CreateCustomKeyStoreResponse = CreateCustomKeyStoreResponse'
  { customKeyStoreId :: Core.Maybe Types.CustomKeyStoreId
    -- ^ A unique identifier for the new custom key store.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCustomKeyStoreResponse' value with any optional fields omitted.
mkCreateCustomKeyStoreResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateCustomKeyStoreResponse
mkCreateCustomKeyStoreResponse responseStatus
  = CreateCustomKeyStoreResponse'{customKeyStoreId = Core.Nothing,
                                  responseStatus}

-- | A unique identifier for the new custom key store.
--
-- /Note:/ Consider using 'customKeyStoreId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccksrrsCustomKeyStoreId :: Lens.Lens' CreateCustomKeyStoreResponse (Core.Maybe Types.CustomKeyStoreId)
ccksrrsCustomKeyStoreId = Lens.field @"customKeyStoreId"
{-# INLINEABLE ccksrrsCustomKeyStoreId #-}
{-# DEPRECATED customKeyStoreId "Use generic-lens or generic-optics with 'customKeyStoreId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccksrrsResponseStatus :: Lens.Lens' CreateCustomKeyStoreResponse Core.Int
ccksrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ccksrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
