{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.CustomKeyStoresListEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.CustomKeyStoresListEntry where

import Network.AWS.KMS.Types.ConnectionErrorCodeType
import Network.AWS.KMS.Types.ConnectionStateType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about each custom key store in the custom key store list.
--
--
--
-- /See:/ 'customKeyStoresListEntry' smart constructor.
data CustomKeyStoresListEntry = CustomKeyStoresListEntry'
  { _cksleCustomKeyStoreName ::
      !(Maybe Text),
    _cksleTrustAnchorCertificate ::
      !(Maybe Text),
    _cksleConnectionErrorCode ::
      !(Maybe ConnectionErrorCodeType),
    _cksleCreationDate :: !(Maybe POSIX),
    _cksleCloudHSMClusterId :: !(Maybe Text),
    _cksleCustomKeyStoreId :: !(Maybe Text),
    _cksleConnectionState ::
      !(Maybe ConnectionStateType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CustomKeyStoresListEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cksleCustomKeyStoreName' - The user-specified friendly name for the custom key store.
--
-- * 'cksleTrustAnchorCertificate' - The trust anchor certificate of the associated AWS CloudHSM cluster. When you <https://docs.aws.amazon.com/cloudhsm/latest/userguide/initialize-cluster.html#sign-csr initialize the cluster> , you create this certificate and save it in the @customerCA.crt@ file.
--
-- * 'cksleConnectionErrorCode' - Describes the connection error. This field appears in the response only when the @ConnectionState@ is @FAILED@ . For help resolving these errors, see <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html#fix-keystore-failed How to Fix a Connection Failure> in /AWS Key Management Service Developer Guide/ . Valid values are:     * @CLUSTER_NOT_FOUND@ - AWS KMS cannot find the AWS CloudHSM cluster with the specified cluster ID.     * @INSUFFICIENT_CLOUDHSM_HSMS@ - The associated AWS CloudHSM cluster does not contain any active HSMs. To connect a custom key store to its AWS CloudHSM cluster, the cluster must contain at least one active HSM.     * @INTERNAL_ERROR@ - AWS KMS could not complete the request due to an internal error. Retry the request. For @ConnectCustomKeyStore@ requests, disconnect the custom key store before trying to connect again.     * @INVALID_CREDENTIALS@ - AWS KMS does not have the correct password for the @kmsuser@ crypto user in the AWS CloudHSM cluster. Before you can connect your custom key store to its AWS CloudHSM cluster, you must change the @kmsuser@ account password and update the key store password value for the custom key store.     * @NETWORK_ERRORS@ - Network errors are preventing AWS KMS from connecting to the custom key store.     * @SUBNET_NOT_FOUND@ - A subnet in the AWS CloudHSM cluster configuration was deleted. If AWS KMS cannot find all of the subnets in the cluster configuration, attempts to connect the custom key store to the AWS CloudHSM cluster fail. To fix this error, create a cluster from a recent backup and associate it with your custom key store. (This process creates a new cluster configuration with a VPC and private subnets.) For details, see <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html#fix-keystore-failed How to Fix a Connection Failure> in the /AWS Key Management Service Developer Guide/ .     * @USER_LOCKED_OUT@ - The @kmsuser@ CU account is locked out of the associated AWS CloudHSM cluster due to too many failed password attempts. Before you can connect your custom key store to its AWS CloudHSM cluster, you must change the @kmsuser@ account password and update the key store password value for the custom key store.     * @USER_LOGGED_IN@ - The @kmsuser@ CU account is logged into the the associated AWS CloudHSM cluster. This prevents AWS KMS from rotating the @kmsuser@ account password and logging into the cluster. Before you can connect your custom key store to its AWS CloudHSM cluster, you must log the @kmsuser@ CU out of the cluster. If you changed the @kmsuser@ password to log into the cluster, you must also and update the key store password value for the custom key store. For help, see <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html#login-kmsuser-2 How to Log Out and Reconnect> in the /AWS Key Management Service Developer Guide/ .     * @USER_NOT_FOUND@ - AWS KMS cannot find a @kmsuser@ CU account in the associated AWS CloudHSM cluster. Before you can connect your custom key store to its AWS CloudHSM cluster, you must create a @kmsuser@ CU account in the cluster, and then update the key store password value for the custom key store.
--
-- * 'cksleCreationDate' - The date and time when the custom key store was created.
--
-- * 'cksleCloudHSMClusterId' - A unique identifier for the AWS CloudHSM cluster that is associated with the custom key store.
--
-- * 'cksleCustomKeyStoreId' - A unique identifier for the custom key store.
--
-- * 'cksleConnectionState' - Indicates whether the custom key store is connected to its AWS CloudHSM cluster. You can create and use CMKs in your custom key stores only when its connection state is @CONNECTED@ . The value is @DISCONNECTED@ if the key store has never been connected or you use the 'DisconnectCustomKeyStore' operation to disconnect it. If the value is @CONNECTED@ but you are having trouble using the custom key store, make sure that its associated AWS CloudHSM cluster is active and contains at least one active HSM. A value of @FAILED@ indicates that an attempt to connect was unsuccessful. The @ConnectionErrorCode@ field in the response indicates the cause of the failure. For help resolving a connection failure, see <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html Troubleshooting a Custom Key Store> in the /AWS Key Management Service Developer Guide/ .
customKeyStoresListEntry ::
  CustomKeyStoresListEntry
customKeyStoresListEntry =
  CustomKeyStoresListEntry'
    { _cksleCustomKeyStoreName = Nothing,
      _cksleTrustAnchorCertificate = Nothing,
      _cksleConnectionErrorCode = Nothing,
      _cksleCreationDate = Nothing,
      _cksleCloudHSMClusterId = Nothing,
      _cksleCustomKeyStoreId = Nothing,
      _cksleConnectionState = Nothing
    }

-- | The user-specified friendly name for the custom key store.
cksleCustomKeyStoreName :: Lens' CustomKeyStoresListEntry (Maybe Text)
cksleCustomKeyStoreName = lens _cksleCustomKeyStoreName (\s a -> s {_cksleCustomKeyStoreName = a})

-- | The trust anchor certificate of the associated AWS CloudHSM cluster. When you <https://docs.aws.amazon.com/cloudhsm/latest/userguide/initialize-cluster.html#sign-csr initialize the cluster> , you create this certificate and save it in the @customerCA.crt@ file.
cksleTrustAnchorCertificate :: Lens' CustomKeyStoresListEntry (Maybe Text)
cksleTrustAnchorCertificate = lens _cksleTrustAnchorCertificate (\s a -> s {_cksleTrustAnchorCertificate = a})

-- | Describes the connection error. This field appears in the response only when the @ConnectionState@ is @FAILED@ . For help resolving these errors, see <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html#fix-keystore-failed How to Fix a Connection Failure> in /AWS Key Management Service Developer Guide/ . Valid values are:     * @CLUSTER_NOT_FOUND@ - AWS KMS cannot find the AWS CloudHSM cluster with the specified cluster ID.     * @INSUFFICIENT_CLOUDHSM_HSMS@ - The associated AWS CloudHSM cluster does not contain any active HSMs. To connect a custom key store to its AWS CloudHSM cluster, the cluster must contain at least one active HSM.     * @INTERNAL_ERROR@ - AWS KMS could not complete the request due to an internal error. Retry the request. For @ConnectCustomKeyStore@ requests, disconnect the custom key store before trying to connect again.     * @INVALID_CREDENTIALS@ - AWS KMS does not have the correct password for the @kmsuser@ crypto user in the AWS CloudHSM cluster. Before you can connect your custom key store to its AWS CloudHSM cluster, you must change the @kmsuser@ account password and update the key store password value for the custom key store.     * @NETWORK_ERRORS@ - Network errors are preventing AWS KMS from connecting to the custom key store.     * @SUBNET_NOT_FOUND@ - A subnet in the AWS CloudHSM cluster configuration was deleted. If AWS KMS cannot find all of the subnets in the cluster configuration, attempts to connect the custom key store to the AWS CloudHSM cluster fail. To fix this error, create a cluster from a recent backup and associate it with your custom key store. (This process creates a new cluster configuration with a VPC and private subnets.) For details, see <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html#fix-keystore-failed How to Fix a Connection Failure> in the /AWS Key Management Service Developer Guide/ .     * @USER_LOCKED_OUT@ - The @kmsuser@ CU account is locked out of the associated AWS CloudHSM cluster due to too many failed password attempts. Before you can connect your custom key store to its AWS CloudHSM cluster, you must change the @kmsuser@ account password and update the key store password value for the custom key store.     * @USER_LOGGED_IN@ - The @kmsuser@ CU account is logged into the the associated AWS CloudHSM cluster. This prevents AWS KMS from rotating the @kmsuser@ account password and logging into the cluster. Before you can connect your custom key store to its AWS CloudHSM cluster, you must log the @kmsuser@ CU out of the cluster. If you changed the @kmsuser@ password to log into the cluster, you must also and update the key store password value for the custom key store. For help, see <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html#login-kmsuser-2 How to Log Out and Reconnect> in the /AWS Key Management Service Developer Guide/ .     * @USER_NOT_FOUND@ - AWS KMS cannot find a @kmsuser@ CU account in the associated AWS CloudHSM cluster. Before you can connect your custom key store to its AWS CloudHSM cluster, you must create a @kmsuser@ CU account in the cluster, and then update the key store password value for the custom key store.
cksleConnectionErrorCode :: Lens' CustomKeyStoresListEntry (Maybe ConnectionErrorCodeType)
cksleConnectionErrorCode = lens _cksleConnectionErrorCode (\s a -> s {_cksleConnectionErrorCode = a})

-- | The date and time when the custom key store was created.
cksleCreationDate :: Lens' CustomKeyStoresListEntry (Maybe UTCTime)
cksleCreationDate = lens _cksleCreationDate (\s a -> s {_cksleCreationDate = a}) . mapping _Time

-- | A unique identifier for the AWS CloudHSM cluster that is associated with the custom key store.
cksleCloudHSMClusterId :: Lens' CustomKeyStoresListEntry (Maybe Text)
cksleCloudHSMClusterId = lens _cksleCloudHSMClusterId (\s a -> s {_cksleCloudHSMClusterId = a})

-- | A unique identifier for the custom key store.
cksleCustomKeyStoreId :: Lens' CustomKeyStoresListEntry (Maybe Text)
cksleCustomKeyStoreId = lens _cksleCustomKeyStoreId (\s a -> s {_cksleCustomKeyStoreId = a})

-- | Indicates whether the custom key store is connected to its AWS CloudHSM cluster. You can create and use CMKs in your custom key stores only when its connection state is @CONNECTED@ . The value is @DISCONNECTED@ if the key store has never been connected or you use the 'DisconnectCustomKeyStore' operation to disconnect it. If the value is @CONNECTED@ but you are having trouble using the custom key store, make sure that its associated AWS CloudHSM cluster is active and contains at least one active HSM. A value of @FAILED@ indicates that an attempt to connect was unsuccessful. The @ConnectionErrorCode@ field in the response indicates the cause of the failure. For help resolving a connection failure, see <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html Troubleshooting a Custom Key Store> in the /AWS Key Management Service Developer Guide/ .
cksleConnectionState :: Lens' CustomKeyStoresListEntry (Maybe ConnectionStateType)
cksleConnectionState = lens _cksleConnectionState (\s a -> s {_cksleConnectionState = a})

instance FromJSON CustomKeyStoresListEntry where
  parseJSON =
    withObject
      "CustomKeyStoresListEntry"
      ( \x ->
          CustomKeyStoresListEntry'
            <$> (x .:? "CustomKeyStoreName")
            <*> (x .:? "TrustAnchorCertificate")
            <*> (x .:? "ConnectionErrorCode")
            <*> (x .:? "CreationDate")
            <*> (x .:? "CloudHsmClusterId")
            <*> (x .:? "CustomKeyStoreId")
            <*> (x .:? "ConnectionState")
      )

instance Hashable CustomKeyStoresListEntry

instance NFData CustomKeyStoresListEntry
