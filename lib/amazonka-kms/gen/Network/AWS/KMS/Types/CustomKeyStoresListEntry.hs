{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.CustomKeyStoresListEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.CustomKeyStoresListEntry
  ( CustomKeyStoresListEntry (..),

    -- * Smart constructor
    mkCustomKeyStoresListEntry,

    -- * Lenses
    cksleCloudHsmClusterId,
    cksleConnectionErrorCode,
    cksleConnectionState,
    cksleCreationDate,
    cksleCustomKeyStoreId,
    cksleCustomKeyStoreName,
    cksleTrustAnchorCertificate,
  )
where

import qualified Network.AWS.KMS.Types.CloudHsmClusterIdType as Types
import qualified Network.AWS.KMS.Types.ConnectionErrorCodeType as Types
import qualified Network.AWS.KMS.Types.ConnectionStateType as Types
import qualified Network.AWS.KMS.Types.CustomKeyStoreIdType as Types
import qualified Network.AWS.KMS.Types.CustomKeyStoreNameType as Types
import qualified Network.AWS.KMS.Types.TrustAnchorCertificateType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about each custom key store in the custom key store list.
--
-- /See:/ 'mkCustomKeyStoresListEntry' smart constructor.
data CustomKeyStoresListEntry = CustomKeyStoresListEntry'
  { -- | A unique identifier for the AWS CloudHSM cluster that is associated with the custom key store.
    cloudHsmClusterId :: Core.Maybe Types.CloudHsmClusterIdType,
    -- | Describes the connection error. This field appears in the response only when the @ConnectionState@ is @FAILED@ . For help resolving these errors, see <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html#fix-keystore-failed How to Fix a Connection Failure> in /AWS Key Management Service Developer Guide/ .
    --
    -- Valid values are:
    --
    --     * @CLUSTER_NOT_FOUND@ - AWS KMS cannot find the AWS CloudHSM cluster with the specified cluster ID.
    --
    --
    --     * @INSUFFICIENT_CLOUDHSM_HSMS@ - The associated AWS CloudHSM cluster does not contain any active HSMs. To connect a custom key store to its AWS CloudHSM cluster, the cluster must contain at least one active HSM.
    --
    --
    --     * @INTERNAL_ERROR@ - AWS KMS could not complete the request due to an internal error. Retry the request. For @ConnectCustomKeyStore@ requests, disconnect the custom key store before trying to connect again.
    --
    --
    --     * @INVALID_CREDENTIALS@ - AWS KMS does not have the correct password for the @kmsuser@ crypto user in the AWS CloudHSM cluster. Before you can connect your custom key store to its AWS CloudHSM cluster, you must change the @kmsuser@ account password and update the key store password value for the custom key store.
    --
    --
    --     * @NETWORK_ERRORS@ - Network errors are preventing AWS KMS from connecting to the custom key store.
    --
    --
    --     * @SUBNET_NOT_FOUND@ - A subnet in the AWS CloudHSM cluster configuration was deleted. If AWS KMS cannot find all of the subnets in the cluster configuration, attempts to connect the custom key store to the AWS CloudHSM cluster fail. To fix this error, create a cluster from a recent backup and associate it with your custom key store. (This process creates a new cluster configuration with a VPC and private subnets.) For details, see <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html#fix-keystore-failed How to Fix a Connection Failure> in the /AWS Key Management Service Developer Guide/ .
    --
    --
    --     * @USER_LOCKED_OUT@ - The @kmsuser@ CU account is locked out of the associated AWS CloudHSM cluster due to too many failed password attempts. Before you can connect your custom key store to its AWS CloudHSM cluster, you must change the @kmsuser@ account password and update the key store password value for the custom key store.
    --
    --
    --     * @USER_LOGGED_IN@ - The @kmsuser@ CU account is logged into the the associated AWS CloudHSM cluster. This prevents AWS KMS from rotating the @kmsuser@ account password and logging into the cluster. Before you can connect your custom key store to its AWS CloudHSM cluster, you must log the @kmsuser@ CU out of the cluster. If you changed the @kmsuser@ password to log into the cluster, you must also and update the key store password value for the custom key store. For help, see <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html#login-kmsuser-2 How to Log Out and Reconnect> in the /AWS Key Management Service Developer Guide/ .
    --
    --
    --     * @USER_NOT_FOUND@ - AWS KMS cannot find a @kmsuser@ CU account in the associated AWS CloudHSM cluster. Before you can connect your custom key store to its AWS CloudHSM cluster, you must create a @kmsuser@ CU account in the cluster, and then update the key store password value for the custom key store.
    connectionErrorCode :: Core.Maybe Types.ConnectionErrorCodeType,
    -- | Indicates whether the custom key store is connected to its AWS CloudHSM cluster.
    --
    -- You can create and use CMKs in your custom key stores only when its connection state is @CONNECTED@ .
    -- The value is @DISCONNECTED@ if the key store has never been connected or you use the 'DisconnectCustomKeyStore' operation to disconnect it. If the value is @CONNECTED@ but you are having trouble using the custom key store, make sure that its associated AWS CloudHSM cluster is active and contains at least one active HSM.
    -- A value of @FAILED@ indicates that an attempt to connect was unsuccessful. The @ConnectionErrorCode@ field in the response indicates the cause of the failure. For help resolving a connection failure, see <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html Troubleshooting a Custom Key Store> in the /AWS Key Management Service Developer Guide/ .
    connectionState :: Core.Maybe Types.ConnectionStateType,
    -- | The date and time when the custom key store was created.
    creationDate :: Core.Maybe Core.NominalDiffTime,
    -- | A unique identifier for the custom key store.
    customKeyStoreId :: Core.Maybe Types.CustomKeyStoreIdType,
    -- | The user-specified friendly name for the custom key store.
    customKeyStoreName :: Core.Maybe Types.CustomKeyStoreNameType,
    -- | The trust anchor certificate of the associated AWS CloudHSM cluster. When you <https://docs.aws.amazon.com/cloudhsm/latest/userguide/initialize-cluster.html#sign-csr initialize the cluster> , you create this certificate and save it in the @customerCA.crt@ file.
    trustAnchorCertificate :: Core.Maybe Types.TrustAnchorCertificateType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CustomKeyStoresListEntry' value with any optional fields omitted.
mkCustomKeyStoresListEntry ::
  CustomKeyStoresListEntry
mkCustomKeyStoresListEntry =
  CustomKeyStoresListEntry'
    { cloudHsmClusterId = Core.Nothing,
      connectionErrorCode = Core.Nothing,
      connectionState = Core.Nothing,
      creationDate = Core.Nothing,
      customKeyStoreId = Core.Nothing,
      customKeyStoreName = Core.Nothing,
      trustAnchorCertificate = Core.Nothing
    }

-- | A unique identifier for the AWS CloudHSM cluster that is associated with the custom key store.
--
-- /Note:/ Consider using 'cloudHsmClusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cksleCloudHsmClusterId :: Lens.Lens' CustomKeyStoresListEntry (Core.Maybe Types.CloudHsmClusterIdType)
cksleCloudHsmClusterId = Lens.field @"cloudHsmClusterId"
{-# DEPRECATED cksleCloudHsmClusterId "Use generic-lens or generic-optics with 'cloudHsmClusterId' instead." #-}

-- | Describes the connection error. This field appears in the response only when the @ConnectionState@ is @FAILED@ . For help resolving these errors, see <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html#fix-keystore-failed How to Fix a Connection Failure> in /AWS Key Management Service Developer Guide/ .
--
-- Valid values are:
--
--     * @CLUSTER_NOT_FOUND@ - AWS KMS cannot find the AWS CloudHSM cluster with the specified cluster ID.
--
--
--     * @INSUFFICIENT_CLOUDHSM_HSMS@ - The associated AWS CloudHSM cluster does not contain any active HSMs. To connect a custom key store to its AWS CloudHSM cluster, the cluster must contain at least one active HSM.
--
--
--     * @INTERNAL_ERROR@ - AWS KMS could not complete the request due to an internal error. Retry the request. For @ConnectCustomKeyStore@ requests, disconnect the custom key store before trying to connect again.
--
--
--     * @INVALID_CREDENTIALS@ - AWS KMS does not have the correct password for the @kmsuser@ crypto user in the AWS CloudHSM cluster. Before you can connect your custom key store to its AWS CloudHSM cluster, you must change the @kmsuser@ account password and update the key store password value for the custom key store.
--
--
--     * @NETWORK_ERRORS@ - Network errors are preventing AWS KMS from connecting to the custom key store.
--
--
--     * @SUBNET_NOT_FOUND@ - A subnet in the AWS CloudHSM cluster configuration was deleted. If AWS KMS cannot find all of the subnets in the cluster configuration, attempts to connect the custom key store to the AWS CloudHSM cluster fail. To fix this error, create a cluster from a recent backup and associate it with your custom key store. (This process creates a new cluster configuration with a VPC and private subnets.) For details, see <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html#fix-keystore-failed How to Fix a Connection Failure> in the /AWS Key Management Service Developer Guide/ .
--
--
--     * @USER_LOCKED_OUT@ - The @kmsuser@ CU account is locked out of the associated AWS CloudHSM cluster due to too many failed password attempts. Before you can connect your custom key store to its AWS CloudHSM cluster, you must change the @kmsuser@ account password and update the key store password value for the custom key store.
--
--
--     * @USER_LOGGED_IN@ - The @kmsuser@ CU account is logged into the the associated AWS CloudHSM cluster. This prevents AWS KMS from rotating the @kmsuser@ account password and logging into the cluster. Before you can connect your custom key store to its AWS CloudHSM cluster, you must log the @kmsuser@ CU out of the cluster. If you changed the @kmsuser@ password to log into the cluster, you must also and update the key store password value for the custom key store. For help, see <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html#login-kmsuser-2 How to Log Out and Reconnect> in the /AWS Key Management Service Developer Guide/ .
--
--
--     * @USER_NOT_FOUND@ - AWS KMS cannot find a @kmsuser@ CU account in the associated AWS CloudHSM cluster. Before you can connect your custom key store to its AWS CloudHSM cluster, you must create a @kmsuser@ CU account in the cluster, and then update the key store password value for the custom key store.
--
--
--
-- /Note:/ Consider using 'connectionErrorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cksleConnectionErrorCode :: Lens.Lens' CustomKeyStoresListEntry (Core.Maybe Types.ConnectionErrorCodeType)
cksleConnectionErrorCode = Lens.field @"connectionErrorCode"
{-# DEPRECATED cksleConnectionErrorCode "Use generic-lens or generic-optics with 'connectionErrorCode' instead." #-}

-- | Indicates whether the custom key store is connected to its AWS CloudHSM cluster.
--
-- You can create and use CMKs in your custom key stores only when its connection state is @CONNECTED@ .
-- The value is @DISCONNECTED@ if the key store has never been connected or you use the 'DisconnectCustomKeyStore' operation to disconnect it. If the value is @CONNECTED@ but you are having trouble using the custom key store, make sure that its associated AWS CloudHSM cluster is active and contains at least one active HSM.
-- A value of @FAILED@ indicates that an attempt to connect was unsuccessful. The @ConnectionErrorCode@ field in the response indicates the cause of the failure. For help resolving a connection failure, see <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html Troubleshooting a Custom Key Store> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'connectionState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cksleConnectionState :: Lens.Lens' CustomKeyStoresListEntry (Core.Maybe Types.ConnectionStateType)
cksleConnectionState = Lens.field @"connectionState"
{-# DEPRECATED cksleConnectionState "Use generic-lens or generic-optics with 'connectionState' instead." #-}

-- | The date and time when the custom key store was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cksleCreationDate :: Lens.Lens' CustomKeyStoresListEntry (Core.Maybe Core.NominalDiffTime)
cksleCreationDate = Lens.field @"creationDate"
{-# DEPRECATED cksleCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | A unique identifier for the custom key store.
--
-- /Note:/ Consider using 'customKeyStoreId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cksleCustomKeyStoreId :: Lens.Lens' CustomKeyStoresListEntry (Core.Maybe Types.CustomKeyStoreIdType)
cksleCustomKeyStoreId = Lens.field @"customKeyStoreId"
{-# DEPRECATED cksleCustomKeyStoreId "Use generic-lens or generic-optics with 'customKeyStoreId' instead." #-}

-- | The user-specified friendly name for the custom key store.
--
-- /Note:/ Consider using 'customKeyStoreName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cksleCustomKeyStoreName :: Lens.Lens' CustomKeyStoresListEntry (Core.Maybe Types.CustomKeyStoreNameType)
cksleCustomKeyStoreName = Lens.field @"customKeyStoreName"
{-# DEPRECATED cksleCustomKeyStoreName "Use generic-lens or generic-optics with 'customKeyStoreName' instead." #-}

-- | The trust anchor certificate of the associated AWS CloudHSM cluster. When you <https://docs.aws.amazon.com/cloudhsm/latest/userguide/initialize-cluster.html#sign-csr initialize the cluster> , you create this certificate and save it in the @customerCA.crt@ file.
--
-- /Note:/ Consider using 'trustAnchorCertificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cksleTrustAnchorCertificate :: Lens.Lens' CustomKeyStoresListEntry (Core.Maybe Types.TrustAnchorCertificateType)
cksleTrustAnchorCertificate = Lens.field @"trustAnchorCertificate"
{-# DEPRECATED cksleTrustAnchorCertificate "Use generic-lens or generic-optics with 'trustAnchorCertificate' instead." #-}

instance Core.FromJSON CustomKeyStoresListEntry where
  parseJSON =
    Core.withObject "CustomKeyStoresListEntry" Core.$
      \x ->
        CustomKeyStoresListEntry'
          Core.<$> (x Core..:? "CloudHsmClusterId")
          Core.<*> (x Core..:? "ConnectionErrorCode")
          Core.<*> (x Core..:? "ConnectionState")
          Core.<*> (x Core..:? "CreationDate")
          Core.<*> (x Core..:? "CustomKeyStoreId")
          Core.<*> (x Core..:? "CustomKeyStoreName")
          Core.<*> (x Core..:? "TrustAnchorCertificate")
