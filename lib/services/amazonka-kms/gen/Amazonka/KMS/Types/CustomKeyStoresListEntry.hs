{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.KMS.Types.CustomKeyStoresListEntry
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KMS.Types.CustomKeyStoresListEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KMS.Types.ConnectionErrorCodeType
import Amazonka.KMS.Types.ConnectionStateType
import qualified Amazonka.Prelude as Prelude

-- | Contains information about each custom key store in the custom key store
-- list.
--
-- /See:/ 'newCustomKeyStoresListEntry' smart constructor.
data CustomKeyStoresListEntry = CustomKeyStoresListEntry'
  { -- | A unique identifier for the custom key store.
    customKeyStoreId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the custom key store is connected to its CloudHSM
    -- cluster.
    --
    -- You can create and use KMS keys in your custom key stores only when its
    -- connection state is @CONNECTED@.
    --
    -- The value is @DISCONNECTED@ if the key store has never been connected or
    -- you use the DisconnectCustomKeyStore operation to disconnect it. If the
    -- value is @CONNECTED@ but you are having trouble using the custom key
    -- store, make sure that its associated CloudHSM cluster is active and
    -- contains at least one active HSM.
    --
    -- A value of @FAILED@ indicates that an attempt to connect was
    -- unsuccessful. The @ConnectionErrorCode@ field in the response indicates
    -- the cause of the failure. For help resolving a connection failure, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html Troubleshooting a Custom Key Store>
    -- in the /Key Management Service Developer Guide/.
    connectionState :: Prelude.Maybe ConnectionStateType,
    -- | The date and time when the custom key store was created.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | A unique identifier for the CloudHSM cluster that is associated with the
    -- custom key store.
    cloudHsmClusterId :: Prelude.Maybe Prelude.Text,
    -- | The trust anchor certificate of the associated CloudHSM cluster. When
    -- you
    -- <https://docs.aws.amazon.com/cloudhsm/latest/userguide/initialize-cluster.html#sign-csr initialize the cluster>,
    -- you create this certificate and save it in the @customerCA.crt@ file.
    trustAnchorCertificate :: Prelude.Maybe Prelude.Text,
    -- | The user-specified friendly name for the custom key store.
    customKeyStoreName :: Prelude.Maybe Prelude.Text,
    -- | Describes the connection error. This field appears in the response only
    -- when the @ConnectionState@ is @FAILED@. For help resolving these errors,
    -- see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html#fix-keystore-failed How to Fix a Connection Failure>
    -- in /Key Management Service Developer Guide/.
    --
    -- Valid values are:
    --
    -- -   @CLUSTER_NOT_FOUND@ - KMS cannot find the CloudHSM cluster with the
    --     specified cluster ID.
    --
    -- -   @INSUFFICIENT_CLOUDHSM_HSMS@ - The associated CloudHSM cluster does
    --     not contain any active HSMs. To connect a custom key store to its
    --     CloudHSM cluster, the cluster must contain at least one active HSM.
    --
    -- -   @INTERNAL_ERROR@ - KMS could not complete the request due to an
    --     internal error. Retry the request. For @ConnectCustomKeyStore@
    --     requests, disconnect the custom key store before trying to connect
    --     again.
    --
    -- -   @INVALID_CREDENTIALS@ - KMS does not have the correct password for
    --     the @kmsuser@ crypto user in the CloudHSM cluster. Before you can
    --     connect your custom key store to its CloudHSM cluster, you must
    --     change the @kmsuser@ account password and update the key store
    --     password value for the custom key store.
    --
    -- -   @NETWORK_ERRORS@ - Network errors are preventing KMS from connecting
    --     to the custom key store.
    --
    -- -   @SUBNET_NOT_FOUND@ - A subnet in the CloudHSM cluster configuration
    --     was deleted. If KMS cannot find all of the subnets in the cluster
    --     configuration, attempts to connect the custom key store to the
    --     CloudHSM cluster fail. To fix this error, create a cluster from a
    --     recent backup and associate it with your custom key store. (This
    --     process creates a new cluster configuration with a VPC and private
    --     subnets.) For details, see
    --     <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html#fix-keystore-failed How to Fix a Connection Failure>
    --     in the /Key Management Service Developer Guide/.
    --
    -- -   @USER_LOCKED_OUT@ - The @kmsuser@ CU account is locked out of the
    --     associated CloudHSM cluster due to too many failed password
    --     attempts. Before you can connect your custom key store to its
    --     CloudHSM cluster, you must change the @kmsuser@ account password and
    --     update the key store password value for the custom key store.
    --
    -- -   @USER_LOGGED_IN@ - The @kmsuser@ CU account is logged into the the
    --     associated CloudHSM cluster. This prevents KMS from rotating the
    --     @kmsuser@ account password and logging into the cluster. Before you
    --     can connect your custom key store to its CloudHSM cluster, you must
    --     log the @kmsuser@ CU out of the cluster. If you changed the
    --     @kmsuser@ password to log into the cluster, you must also and update
    --     the key store password value for the custom key store. For help, see
    --     <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html#login-kmsuser-2 How to Log Out and Reconnect>
    --     in the /Key Management Service Developer Guide/.
    --
    -- -   @USER_NOT_FOUND@ - KMS cannot find a @kmsuser@ CU account in the
    --     associated CloudHSM cluster. Before you can connect your custom key
    --     store to its CloudHSM cluster, you must create a @kmsuser@ CU
    --     account in the cluster, and then update the key store password value
    --     for the custom key store.
    connectionErrorCode :: Prelude.Maybe ConnectionErrorCodeType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomKeyStoresListEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customKeyStoreId', 'customKeyStoresListEntry_customKeyStoreId' - A unique identifier for the custom key store.
--
-- 'connectionState', 'customKeyStoresListEntry_connectionState' - Indicates whether the custom key store is connected to its CloudHSM
-- cluster.
--
-- You can create and use KMS keys in your custom key stores only when its
-- connection state is @CONNECTED@.
--
-- The value is @DISCONNECTED@ if the key store has never been connected or
-- you use the DisconnectCustomKeyStore operation to disconnect it. If the
-- value is @CONNECTED@ but you are having trouble using the custom key
-- store, make sure that its associated CloudHSM cluster is active and
-- contains at least one active HSM.
--
-- A value of @FAILED@ indicates that an attempt to connect was
-- unsuccessful. The @ConnectionErrorCode@ field in the response indicates
-- the cause of the failure. For help resolving a connection failure, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html Troubleshooting a Custom Key Store>
-- in the /Key Management Service Developer Guide/.
--
-- 'creationDate', 'customKeyStoresListEntry_creationDate' - The date and time when the custom key store was created.
--
-- 'cloudHsmClusterId', 'customKeyStoresListEntry_cloudHsmClusterId' - A unique identifier for the CloudHSM cluster that is associated with the
-- custom key store.
--
-- 'trustAnchorCertificate', 'customKeyStoresListEntry_trustAnchorCertificate' - The trust anchor certificate of the associated CloudHSM cluster. When
-- you
-- <https://docs.aws.amazon.com/cloudhsm/latest/userguide/initialize-cluster.html#sign-csr initialize the cluster>,
-- you create this certificate and save it in the @customerCA.crt@ file.
--
-- 'customKeyStoreName', 'customKeyStoresListEntry_customKeyStoreName' - The user-specified friendly name for the custom key store.
--
-- 'connectionErrorCode', 'customKeyStoresListEntry_connectionErrorCode' - Describes the connection error. This field appears in the response only
-- when the @ConnectionState@ is @FAILED@. For help resolving these errors,
-- see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html#fix-keystore-failed How to Fix a Connection Failure>
-- in /Key Management Service Developer Guide/.
--
-- Valid values are:
--
-- -   @CLUSTER_NOT_FOUND@ - KMS cannot find the CloudHSM cluster with the
--     specified cluster ID.
--
-- -   @INSUFFICIENT_CLOUDHSM_HSMS@ - The associated CloudHSM cluster does
--     not contain any active HSMs. To connect a custom key store to its
--     CloudHSM cluster, the cluster must contain at least one active HSM.
--
-- -   @INTERNAL_ERROR@ - KMS could not complete the request due to an
--     internal error. Retry the request. For @ConnectCustomKeyStore@
--     requests, disconnect the custom key store before trying to connect
--     again.
--
-- -   @INVALID_CREDENTIALS@ - KMS does not have the correct password for
--     the @kmsuser@ crypto user in the CloudHSM cluster. Before you can
--     connect your custom key store to its CloudHSM cluster, you must
--     change the @kmsuser@ account password and update the key store
--     password value for the custom key store.
--
-- -   @NETWORK_ERRORS@ - Network errors are preventing KMS from connecting
--     to the custom key store.
--
-- -   @SUBNET_NOT_FOUND@ - A subnet in the CloudHSM cluster configuration
--     was deleted. If KMS cannot find all of the subnets in the cluster
--     configuration, attempts to connect the custom key store to the
--     CloudHSM cluster fail. To fix this error, create a cluster from a
--     recent backup and associate it with your custom key store. (This
--     process creates a new cluster configuration with a VPC and private
--     subnets.) For details, see
--     <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html#fix-keystore-failed How to Fix a Connection Failure>
--     in the /Key Management Service Developer Guide/.
--
-- -   @USER_LOCKED_OUT@ - The @kmsuser@ CU account is locked out of the
--     associated CloudHSM cluster due to too many failed password
--     attempts. Before you can connect your custom key store to its
--     CloudHSM cluster, you must change the @kmsuser@ account password and
--     update the key store password value for the custom key store.
--
-- -   @USER_LOGGED_IN@ - The @kmsuser@ CU account is logged into the the
--     associated CloudHSM cluster. This prevents KMS from rotating the
--     @kmsuser@ account password and logging into the cluster. Before you
--     can connect your custom key store to its CloudHSM cluster, you must
--     log the @kmsuser@ CU out of the cluster. If you changed the
--     @kmsuser@ password to log into the cluster, you must also and update
--     the key store password value for the custom key store. For help, see
--     <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html#login-kmsuser-2 How to Log Out and Reconnect>
--     in the /Key Management Service Developer Guide/.
--
-- -   @USER_NOT_FOUND@ - KMS cannot find a @kmsuser@ CU account in the
--     associated CloudHSM cluster. Before you can connect your custom key
--     store to its CloudHSM cluster, you must create a @kmsuser@ CU
--     account in the cluster, and then update the key store password value
--     for the custom key store.
newCustomKeyStoresListEntry ::
  CustomKeyStoresListEntry
newCustomKeyStoresListEntry =
  CustomKeyStoresListEntry'
    { customKeyStoreId =
        Prelude.Nothing,
      connectionState = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      cloudHsmClusterId = Prelude.Nothing,
      trustAnchorCertificate = Prelude.Nothing,
      customKeyStoreName = Prelude.Nothing,
      connectionErrorCode = Prelude.Nothing
    }

-- | A unique identifier for the custom key store.
customKeyStoresListEntry_customKeyStoreId :: Lens.Lens' CustomKeyStoresListEntry (Prelude.Maybe Prelude.Text)
customKeyStoresListEntry_customKeyStoreId = Lens.lens (\CustomKeyStoresListEntry' {customKeyStoreId} -> customKeyStoreId) (\s@CustomKeyStoresListEntry' {} a -> s {customKeyStoreId = a} :: CustomKeyStoresListEntry)

-- | Indicates whether the custom key store is connected to its CloudHSM
-- cluster.
--
-- You can create and use KMS keys in your custom key stores only when its
-- connection state is @CONNECTED@.
--
-- The value is @DISCONNECTED@ if the key store has never been connected or
-- you use the DisconnectCustomKeyStore operation to disconnect it. If the
-- value is @CONNECTED@ but you are having trouble using the custom key
-- store, make sure that its associated CloudHSM cluster is active and
-- contains at least one active HSM.
--
-- A value of @FAILED@ indicates that an attempt to connect was
-- unsuccessful. The @ConnectionErrorCode@ field in the response indicates
-- the cause of the failure. For help resolving a connection failure, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html Troubleshooting a Custom Key Store>
-- in the /Key Management Service Developer Guide/.
customKeyStoresListEntry_connectionState :: Lens.Lens' CustomKeyStoresListEntry (Prelude.Maybe ConnectionStateType)
customKeyStoresListEntry_connectionState = Lens.lens (\CustomKeyStoresListEntry' {connectionState} -> connectionState) (\s@CustomKeyStoresListEntry' {} a -> s {connectionState = a} :: CustomKeyStoresListEntry)

-- | The date and time when the custom key store was created.
customKeyStoresListEntry_creationDate :: Lens.Lens' CustomKeyStoresListEntry (Prelude.Maybe Prelude.UTCTime)
customKeyStoresListEntry_creationDate = Lens.lens (\CustomKeyStoresListEntry' {creationDate} -> creationDate) (\s@CustomKeyStoresListEntry' {} a -> s {creationDate = a} :: CustomKeyStoresListEntry) Prelude.. Lens.mapping Data._Time

-- | A unique identifier for the CloudHSM cluster that is associated with the
-- custom key store.
customKeyStoresListEntry_cloudHsmClusterId :: Lens.Lens' CustomKeyStoresListEntry (Prelude.Maybe Prelude.Text)
customKeyStoresListEntry_cloudHsmClusterId = Lens.lens (\CustomKeyStoresListEntry' {cloudHsmClusterId} -> cloudHsmClusterId) (\s@CustomKeyStoresListEntry' {} a -> s {cloudHsmClusterId = a} :: CustomKeyStoresListEntry)

-- | The trust anchor certificate of the associated CloudHSM cluster. When
-- you
-- <https://docs.aws.amazon.com/cloudhsm/latest/userguide/initialize-cluster.html#sign-csr initialize the cluster>,
-- you create this certificate and save it in the @customerCA.crt@ file.
customKeyStoresListEntry_trustAnchorCertificate :: Lens.Lens' CustomKeyStoresListEntry (Prelude.Maybe Prelude.Text)
customKeyStoresListEntry_trustAnchorCertificate = Lens.lens (\CustomKeyStoresListEntry' {trustAnchorCertificate} -> trustAnchorCertificate) (\s@CustomKeyStoresListEntry' {} a -> s {trustAnchorCertificate = a} :: CustomKeyStoresListEntry)

-- | The user-specified friendly name for the custom key store.
customKeyStoresListEntry_customKeyStoreName :: Lens.Lens' CustomKeyStoresListEntry (Prelude.Maybe Prelude.Text)
customKeyStoresListEntry_customKeyStoreName = Lens.lens (\CustomKeyStoresListEntry' {customKeyStoreName} -> customKeyStoreName) (\s@CustomKeyStoresListEntry' {} a -> s {customKeyStoreName = a} :: CustomKeyStoresListEntry)

-- | Describes the connection error. This field appears in the response only
-- when the @ConnectionState@ is @FAILED@. For help resolving these errors,
-- see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html#fix-keystore-failed How to Fix a Connection Failure>
-- in /Key Management Service Developer Guide/.
--
-- Valid values are:
--
-- -   @CLUSTER_NOT_FOUND@ - KMS cannot find the CloudHSM cluster with the
--     specified cluster ID.
--
-- -   @INSUFFICIENT_CLOUDHSM_HSMS@ - The associated CloudHSM cluster does
--     not contain any active HSMs. To connect a custom key store to its
--     CloudHSM cluster, the cluster must contain at least one active HSM.
--
-- -   @INTERNAL_ERROR@ - KMS could not complete the request due to an
--     internal error. Retry the request. For @ConnectCustomKeyStore@
--     requests, disconnect the custom key store before trying to connect
--     again.
--
-- -   @INVALID_CREDENTIALS@ - KMS does not have the correct password for
--     the @kmsuser@ crypto user in the CloudHSM cluster. Before you can
--     connect your custom key store to its CloudHSM cluster, you must
--     change the @kmsuser@ account password and update the key store
--     password value for the custom key store.
--
-- -   @NETWORK_ERRORS@ - Network errors are preventing KMS from connecting
--     to the custom key store.
--
-- -   @SUBNET_NOT_FOUND@ - A subnet in the CloudHSM cluster configuration
--     was deleted. If KMS cannot find all of the subnets in the cluster
--     configuration, attempts to connect the custom key store to the
--     CloudHSM cluster fail. To fix this error, create a cluster from a
--     recent backup and associate it with your custom key store. (This
--     process creates a new cluster configuration with a VPC and private
--     subnets.) For details, see
--     <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html#fix-keystore-failed How to Fix a Connection Failure>
--     in the /Key Management Service Developer Guide/.
--
-- -   @USER_LOCKED_OUT@ - The @kmsuser@ CU account is locked out of the
--     associated CloudHSM cluster due to too many failed password
--     attempts. Before you can connect your custom key store to its
--     CloudHSM cluster, you must change the @kmsuser@ account password and
--     update the key store password value for the custom key store.
--
-- -   @USER_LOGGED_IN@ - The @kmsuser@ CU account is logged into the the
--     associated CloudHSM cluster. This prevents KMS from rotating the
--     @kmsuser@ account password and logging into the cluster. Before you
--     can connect your custom key store to its CloudHSM cluster, you must
--     log the @kmsuser@ CU out of the cluster. If you changed the
--     @kmsuser@ password to log into the cluster, you must also and update
--     the key store password value for the custom key store. For help, see
--     <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html#login-kmsuser-2 How to Log Out and Reconnect>
--     in the /Key Management Service Developer Guide/.
--
-- -   @USER_NOT_FOUND@ - KMS cannot find a @kmsuser@ CU account in the
--     associated CloudHSM cluster. Before you can connect your custom key
--     store to its CloudHSM cluster, you must create a @kmsuser@ CU
--     account in the cluster, and then update the key store password value
--     for the custom key store.
customKeyStoresListEntry_connectionErrorCode :: Lens.Lens' CustomKeyStoresListEntry (Prelude.Maybe ConnectionErrorCodeType)
customKeyStoresListEntry_connectionErrorCode = Lens.lens (\CustomKeyStoresListEntry' {connectionErrorCode} -> connectionErrorCode) (\s@CustomKeyStoresListEntry' {} a -> s {connectionErrorCode = a} :: CustomKeyStoresListEntry)

instance Data.FromJSON CustomKeyStoresListEntry where
  parseJSON =
    Data.withObject
      "CustomKeyStoresListEntry"
      ( \x ->
          CustomKeyStoresListEntry'
            Prelude.<$> (x Data..:? "CustomKeyStoreId")
            Prelude.<*> (x Data..:? "ConnectionState")
            Prelude.<*> (x Data..:? "CreationDate")
            Prelude.<*> (x Data..:? "CloudHsmClusterId")
            Prelude.<*> (x Data..:? "TrustAnchorCertificate")
            Prelude.<*> (x Data..:? "CustomKeyStoreName")
            Prelude.<*> (x Data..:? "ConnectionErrorCode")
      )

instance Prelude.Hashable CustomKeyStoresListEntry where
  hashWithSalt _salt CustomKeyStoresListEntry' {..} =
    _salt `Prelude.hashWithSalt` customKeyStoreId
      `Prelude.hashWithSalt` connectionState
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` cloudHsmClusterId
      `Prelude.hashWithSalt` trustAnchorCertificate
      `Prelude.hashWithSalt` customKeyStoreName
      `Prelude.hashWithSalt` connectionErrorCode

instance Prelude.NFData CustomKeyStoresListEntry where
  rnf CustomKeyStoresListEntry' {..} =
    Prelude.rnf customKeyStoreId
      `Prelude.seq` Prelude.rnf connectionState
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf cloudHsmClusterId
      `Prelude.seq` Prelude.rnf trustAnchorCertificate
      `Prelude.seq` Prelude.rnf customKeyStoreName
      `Prelude.seq` Prelude.rnf connectionErrorCode
