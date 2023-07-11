{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.KMS.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KMS.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AlreadyExistsException,
    _CloudHsmClusterInUseException,
    _CloudHsmClusterInvalidConfigurationException,
    _CloudHsmClusterNotActiveException,
    _CloudHsmClusterNotFoundException,
    _CloudHsmClusterNotRelatedException,
    _CustomKeyStoreHasCMKsException,
    _CustomKeyStoreInvalidStateException,
    _CustomKeyStoreNameInUseException,
    _CustomKeyStoreNotFoundException,
    _DependencyTimeoutException,
    _DisabledException,
    _ExpiredImportTokenException,
    _IncorrectKeyException,
    _IncorrectKeyMaterialException,
    _IncorrectTrustAnchorException,
    _InvalidAliasNameException,
    _InvalidArnException,
    _InvalidCiphertextException,
    _InvalidGrantIdException,
    _InvalidGrantTokenException,
    _InvalidImportTokenException,
    _InvalidKeyUsageException,
    _InvalidMarkerException,
    _KMSInternalException,
    _KMSInvalidMacException,
    _KMSInvalidSignatureException,
    _KMSInvalidStateException,
    _KeyUnavailableException,
    _LimitExceededException,
    _MalformedPolicyDocumentException,
    _NotFoundException,
    _TagException,
    _UnsupportedOperationException,
    _XksKeyAlreadyInUseException,
    _XksKeyInvalidConfigurationException,
    _XksKeyNotFoundException,
    _XksProxyIncorrectAuthenticationCredentialException,
    _XksProxyInvalidConfigurationException,
    _XksProxyInvalidResponseException,
    _XksProxyUriEndpointInUseException,
    _XksProxyUriInUseException,
    _XksProxyUriUnreachableException,
    _XksProxyVpcEndpointServiceInUseException,
    _XksProxyVpcEndpointServiceInvalidConfigurationException,
    _XksProxyVpcEndpointServiceNotFoundException,

    -- * AlgorithmSpec
    AlgorithmSpec (..),

    -- * ConnectionErrorCodeType
    ConnectionErrorCodeType (..),

    -- * ConnectionStateType
    ConnectionStateType (..),

    -- * CustomKeyStoreType
    CustomKeyStoreType (..),

    -- * CustomerMasterKeySpec
    CustomerMasterKeySpec (..),

    -- * DataKeyPairSpec
    DataKeyPairSpec (..),

    -- * DataKeySpec
    DataKeySpec (..),

    -- * EncryptionAlgorithmSpec
    EncryptionAlgorithmSpec (..),

    -- * ExpirationModelType
    ExpirationModelType (..),

    -- * GrantOperation
    GrantOperation (..),

    -- * KeyManagerType
    KeyManagerType (..),

    -- * KeySpec
    KeySpec (..),

    -- * KeyState
    KeyState (..),

    -- * KeyUsageType
    KeyUsageType (..),

    -- * MacAlgorithmSpec
    MacAlgorithmSpec (..),

    -- * MessageType
    MessageType (..),

    -- * MultiRegionKeyType
    MultiRegionKeyType (..),

    -- * OriginType
    OriginType (..),

    -- * SigningAlgorithmSpec
    SigningAlgorithmSpec (..),

    -- * WrappingKeySpec
    WrappingKeySpec (..),

    -- * XksProxyConnectivityType
    XksProxyConnectivityType (..),

    -- * AliasListEntry
    AliasListEntry (..),
    newAliasListEntry,
    aliasListEntry_aliasArn,
    aliasListEntry_aliasName,
    aliasListEntry_creationDate,
    aliasListEntry_lastUpdatedDate,
    aliasListEntry_targetKeyId,

    -- * CustomKeyStoresListEntry
    CustomKeyStoresListEntry (..),
    newCustomKeyStoresListEntry,
    customKeyStoresListEntry_cloudHsmClusterId,
    customKeyStoresListEntry_connectionErrorCode,
    customKeyStoresListEntry_connectionState,
    customKeyStoresListEntry_creationDate,
    customKeyStoresListEntry_customKeyStoreId,
    customKeyStoresListEntry_customKeyStoreName,
    customKeyStoresListEntry_customKeyStoreType,
    customKeyStoresListEntry_trustAnchorCertificate,
    customKeyStoresListEntry_xksProxyConfiguration,

    -- * GrantConstraints
    GrantConstraints (..),
    newGrantConstraints,
    grantConstraints_encryptionContextEquals,
    grantConstraints_encryptionContextSubset,

    -- * GrantListEntry
    GrantListEntry (..),
    newGrantListEntry,
    grantListEntry_constraints,
    grantListEntry_creationDate,
    grantListEntry_grantId,
    grantListEntry_granteePrincipal,
    grantListEntry_issuingAccount,
    grantListEntry_keyId,
    grantListEntry_name,
    grantListEntry_operations,
    grantListEntry_retiringPrincipal,

    -- * KeyListEntry
    KeyListEntry (..),
    newKeyListEntry,
    keyListEntry_keyArn,
    keyListEntry_keyId,

    -- * KeyMetadata
    KeyMetadata (..),
    newKeyMetadata,
    keyMetadata_aWSAccountId,
    keyMetadata_arn,
    keyMetadata_cloudHsmClusterId,
    keyMetadata_creationDate,
    keyMetadata_customKeyStoreId,
    keyMetadata_customerMasterKeySpec,
    keyMetadata_deletionDate,
    keyMetadata_description,
    keyMetadata_enabled,
    keyMetadata_encryptionAlgorithms,
    keyMetadata_expirationModel,
    keyMetadata_keyManager,
    keyMetadata_keySpec,
    keyMetadata_keyState,
    keyMetadata_keyUsage,
    keyMetadata_macAlgorithms,
    keyMetadata_multiRegion,
    keyMetadata_multiRegionConfiguration,
    keyMetadata_origin,
    keyMetadata_pendingDeletionWindowInDays,
    keyMetadata_signingAlgorithms,
    keyMetadata_validTo,
    keyMetadata_xksKeyConfiguration,
    keyMetadata_keyId,

    -- * ListGrantsResponse
    ListGrantsResponse (..),
    newListGrantsResponse,
    listGrantsResponse_grants,
    listGrantsResponse_nextMarker,
    listGrantsResponse_truncated,

    -- * MultiRegionConfiguration
    MultiRegionConfiguration (..),
    newMultiRegionConfiguration,
    multiRegionConfiguration_multiRegionKeyType,
    multiRegionConfiguration_primaryKey,
    multiRegionConfiguration_replicaKeys,

    -- * MultiRegionKey
    MultiRegionKey (..),
    newMultiRegionKey,
    multiRegionKey_arn,
    multiRegionKey_region,

    -- * Tag
    Tag (..),
    newTag,
    tag_tagKey,
    tag_tagValue,

    -- * XksKeyConfigurationType
    XksKeyConfigurationType (..),
    newXksKeyConfigurationType,
    xksKeyConfigurationType_id,

    -- * XksProxyAuthenticationCredentialType
    XksProxyAuthenticationCredentialType (..),
    newXksProxyAuthenticationCredentialType,
    xksProxyAuthenticationCredentialType_accessKeyId,
    xksProxyAuthenticationCredentialType_rawSecretAccessKey,

    -- * XksProxyConfigurationType
    XksProxyConfigurationType (..),
    newXksProxyConfigurationType,
    xksProxyConfigurationType_accessKeyId,
    xksProxyConfigurationType_connectivity,
    xksProxyConfigurationType_uriEndpoint,
    xksProxyConfigurationType_uriPath,
    xksProxyConfigurationType_vpcEndpointServiceName,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.KMS.Types.AlgorithmSpec
import Amazonka.KMS.Types.AliasListEntry
import Amazonka.KMS.Types.ConnectionErrorCodeType
import Amazonka.KMS.Types.ConnectionStateType
import Amazonka.KMS.Types.CustomKeyStoreType
import Amazonka.KMS.Types.CustomKeyStoresListEntry
import Amazonka.KMS.Types.CustomerMasterKeySpec
import Amazonka.KMS.Types.DataKeyPairSpec
import Amazonka.KMS.Types.DataKeySpec
import Amazonka.KMS.Types.EncryptionAlgorithmSpec
import Amazonka.KMS.Types.ExpirationModelType
import Amazonka.KMS.Types.GrantConstraints
import Amazonka.KMS.Types.GrantListEntry
import Amazonka.KMS.Types.GrantOperation
import Amazonka.KMS.Types.KeyListEntry
import Amazonka.KMS.Types.KeyManagerType
import Amazonka.KMS.Types.KeyMetadata
import Amazonka.KMS.Types.KeySpec
import Amazonka.KMS.Types.KeyState
import Amazonka.KMS.Types.KeyUsageType
import Amazonka.KMS.Types.ListGrantsResponse
import Amazonka.KMS.Types.MacAlgorithmSpec
import Amazonka.KMS.Types.MessageType
import Amazonka.KMS.Types.MultiRegionConfiguration
import Amazonka.KMS.Types.MultiRegionKey
import Amazonka.KMS.Types.MultiRegionKeyType
import Amazonka.KMS.Types.OriginType
import Amazonka.KMS.Types.SigningAlgorithmSpec
import Amazonka.KMS.Types.Tag
import Amazonka.KMS.Types.WrappingKeySpec
import Amazonka.KMS.Types.XksKeyConfigurationType
import Amazonka.KMS.Types.XksProxyAuthenticationCredentialType
import Amazonka.KMS.Types.XksProxyConfigurationType
import Amazonka.KMS.Types.XksProxyConnectivityType
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2014-11-01@ of the Amazon Key Management Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "KMS",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "kms",
      Core.signingName = "kms",
      Core.version = "2014-11-01",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "KMS",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | The request was rejected because it attempted to create a resource that
-- already exists.
_AlreadyExistsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "AlreadyExistsException"

-- | The request was rejected because the specified CloudHSM cluster is
-- already associated with an CloudHSM key store in the account, or it
-- shares a backup history with an CloudHSM key store in the account. Each
-- CloudHSM key store in the account must be associated with a different
-- CloudHSM cluster.
--
-- CloudHSM clusters that share a backup history have the same cluster
-- certificate. To view the cluster certificate of an CloudHSM cluster, use
-- the
-- <https://docs.aws.amazon.com/cloudhsm/latest/APIReference/API_DescribeClusters.html DescribeClusters>
-- operation.
_CloudHsmClusterInUseException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CloudHsmClusterInUseException =
  Core._MatchServiceError
    defaultService
    "CloudHsmClusterInUseException"

-- | The request was rejected because the associated CloudHSM cluster did not
-- meet the configuration requirements for an CloudHSM key store.
--
-- -   The CloudHSM cluster must be configured with private subnets in at
--     least two different Availability Zones in the Region.
--
-- -   The
--     <https://docs.aws.amazon.com/cloudhsm/latest/userguide/configure-sg.html security group for the cluster>
--     (cloudhsm-cluster-/\<cluster-id>/-sg) must include inbound rules and
--     outbound rules that allow TCP traffic on ports 2223-2225. The
--     __Source__ in the inbound rules and the __Destination__ in the
--     outbound rules must match the security group ID. These rules are set
--     by default when you create the CloudHSM cluster. Do not delete or
--     change them. To get information about a particular security group,
--     use the
--     <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeSecurityGroups.html DescribeSecurityGroups>
--     operation.
--
-- -   The CloudHSM cluster must contain at least as many HSMs as the
--     operation requires. To add HSMs, use the CloudHSM
--     <https://docs.aws.amazon.com/cloudhsm/latest/APIReference/API_CreateHsm.html CreateHsm>
--     operation.
--
--     For the CreateCustomKeyStore, UpdateCustomKeyStore, and CreateKey
--     operations, the CloudHSM cluster must have at least two active HSMs,
--     each in a different Availability Zone. For the ConnectCustomKeyStore
--     operation, the CloudHSM must contain at least one active HSM.
--
-- For information about the requirements for an CloudHSM cluster that is
-- associated with an CloudHSM key store, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/create-keystore.html#before-keystore Assemble the Prerequisites>
-- in the /Key Management Service Developer Guide/. For information about
-- creating a private subnet for an CloudHSM cluster, see
-- <https://docs.aws.amazon.com/cloudhsm/latest/userguide/create-subnets.html Create a Private Subnet>
-- in the /CloudHSM User Guide/. For information about cluster security
-- groups, see
-- <https://docs.aws.amazon.com/cloudhsm/latest/userguide/configure-sg.html Configure a Default Security Group>
-- in the //CloudHSM User Guide// .
_CloudHsmClusterInvalidConfigurationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CloudHsmClusterInvalidConfigurationException =
  Core._MatchServiceError
    defaultService
    "CloudHsmClusterInvalidConfigurationException"

-- | The request was rejected because the CloudHSM cluster associated with
-- the CloudHSM key store is not active. Initialize and activate the
-- cluster and try the command again. For detailed instructions, see
-- <https://docs.aws.amazon.com/cloudhsm/latest/userguide/getting-started.html Getting Started>
-- in the /CloudHSM User Guide/.
_CloudHsmClusterNotActiveException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CloudHsmClusterNotActiveException =
  Core._MatchServiceError
    defaultService
    "CloudHsmClusterNotActiveException"

-- | The request was rejected because KMS cannot find the CloudHSM cluster
-- with the specified cluster ID. Retry the request with a different
-- cluster ID.
_CloudHsmClusterNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CloudHsmClusterNotFoundException =
  Core._MatchServiceError
    defaultService
    "CloudHsmClusterNotFoundException"

-- | The request was rejected because the specified CloudHSM cluster has a
-- different cluster certificate than the original cluster. You cannot use
-- the operation to specify an unrelated cluster for an CloudHSM key store.
--
-- Specify an CloudHSM cluster that shares a backup history with the
-- original cluster. This includes clusters that were created from a backup
-- of the current cluster, and clusters that were created from the same
-- backup that produced the current cluster.
--
-- CloudHSM clusters that share a backup history have the same cluster
-- certificate. To view the cluster certificate of an CloudHSM cluster, use
-- the
-- <https://docs.aws.amazon.com/cloudhsm/latest/APIReference/API_DescribeClusters.html DescribeClusters>
-- operation.
_CloudHsmClusterNotRelatedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CloudHsmClusterNotRelatedException =
  Core._MatchServiceError
    defaultService
    "CloudHsmClusterNotRelatedException"

-- | The request was rejected because the custom key store contains KMS keys.
-- After verifying that you do not need to use the KMS keys, use the
-- ScheduleKeyDeletion operation to delete the KMS keys. After they are
-- deleted, you can delete the custom key store.
_CustomKeyStoreHasCMKsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CustomKeyStoreHasCMKsException =
  Core._MatchServiceError
    defaultService
    "CustomKeyStoreHasCMKsException"

-- | The request was rejected because of the @ConnectionState@ of the custom
-- key store. To get the @ConnectionState@ of a custom key store, use the
-- DescribeCustomKeyStores operation.
--
-- This exception is thrown under the following conditions:
--
-- -   You requested the ConnectCustomKeyStore operation on a custom key
--     store with a @ConnectionState@ of @DISCONNECTING@ or @FAILED@. This
--     operation is valid for all other @ConnectionState@ values. To
--     reconnect a custom key store in a @FAILED@ state, disconnect it
--     (DisconnectCustomKeyStore), then connect it
--     (@ConnectCustomKeyStore@).
--
-- -   You requested the CreateKey operation in a custom key store that is
--     not connected. This operations is valid only when the custom key
--     store @ConnectionState@ is @CONNECTED@.
--
-- -   You requested the DisconnectCustomKeyStore operation on a custom key
--     store with a @ConnectionState@ of @DISCONNECTING@ or @DISCONNECTED@.
--     This operation is valid for all other @ConnectionState@ values.
--
-- -   You requested the UpdateCustomKeyStore or DeleteCustomKeyStore
--     operation on a custom key store that is not disconnected. This
--     operation is valid only when the custom key store @ConnectionState@
--     is @DISCONNECTED@.
--
-- -   You requested the GenerateRandom operation in an CloudHSM key store
--     that is not connected. This operation is valid only when the
--     CloudHSM key store @ConnectionState@ is @CONNECTED@.
_CustomKeyStoreInvalidStateException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CustomKeyStoreInvalidStateException =
  Core._MatchServiceError
    defaultService
    "CustomKeyStoreInvalidStateException"

-- | The request was rejected because the specified custom key store name is
-- already assigned to another custom key store in the account. Try again
-- with a custom key store name that is unique in the account.
_CustomKeyStoreNameInUseException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CustomKeyStoreNameInUseException =
  Core._MatchServiceError
    defaultService
    "CustomKeyStoreNameInUseException"

-- | The request was rejected because KMS cannot find a custom key store with
-- the specified key store name or ID.
_CustomKeyStoreNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CustomKeyStoreNotFoundException =
  Core._MatchServiceError
    defaultService
    "CustomKeyStoreNotFoundException"

-- | The system timed out while trying to fulfill the request. You can retry
-- the request.
_DependencyTimeoutException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DependencyTimeoutException =
  Core._MatchServiceError
    defaultService
    "DependencyTimeoutException"

-- | The request was rejected because the specified KMS key is not enabled.
_DisabledException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DisabledException =
  Core._MatchServiceError
    defaultService
    "DisabledException"

-- | The request was rejected because the specified import token is expired.
-- Use GetParametersForImport to get a new import token and public key, use
-- the new public key to encrypt the key material, and then try the request
-- again.
_ExpiredImportTokenException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ExpiredImportTokenException =
  Core._MatchServiceError
    defaultService
    "ExpiredImportTokenException"

-- | The request was rejected because the specified KMS key cannot decrypt
-- the data. The @KeyId@ in a Decrypt request and the @SourceKeyId@ in a
-- ReEncrypt request must identify the same KMS key that was used to
-- encrypt the ciphertext.
_IncorrectKeyException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_IncorrectKeyException =
  Core._MatchServiceError
    defaultService
    "IncorrectKeyException"

-- | The request was rejected because the key material in the request is,
-- expired, invalid, or is not the same key material that was previously
-- imported into this KMS key.
_IncorrectKeyMaterialException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_IncorrectKeyMaterialException =
  Core._MatchServiceError
    defaultService
    "IncorrectKeyMaterialException"

-- | The request was rejected because the trust anchor certificate in the
-- request to create an CloudHSM key store is not the trust anchor
-- certificate for the specified CloudHSM cluster.
--
-- When you
-- <https://docs.aws.amazon.com/cloudhsm/latest/userguide/initialize-cluster.html#sign-csr initialize the CloudHSM cluster>,
-- you create the trust anchor certificate and save it in the
-- @customerCA.crt@ file.
_IncorrectTrustAnchorException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_IncorrectTrustAnchorException =
  Core._MatchServiceError
    defaultService
    "IncorrectTrustAnchorException"

-- | The request was rejected because the specified alias name is not valid.
_InvalidAliasNameException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidAliasNameException =
  Core._MatchServiceError
    defaultService
    "InvalidAliasNameException"

-- | The request was rejected because a specified ARN, or an ARN in a key
-- policy, is not valid.
_InvalidArnException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidArnException =
  Core._MatchServiceError
    defaultService
    "InvalidArnException"

-- | From the Decrypt or ReEncrypt operation, the request was rejected
-- because the specified ciphertext, or additional authenticated data
-- incorporated into the ciphertext, such as the encryption context, is
-- corrupted, missing, or otherwise invalid.
--
-- From the ImportKeyMaterial operation, the request was rejected because
-- KMS could not decrypt the encrypted (wrapped) key material.
_InvalidCiphertextException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidCiphertextException =
  Core._MatchServiceError
    defaultService
    "InvalidCiphertextException"

-- | The request was rejected because the specified @GrantId@ is not valid.
_InvalidGrantIdException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidGrantIdException =
  Core._MatchServiceError
    defaultService
    "InvalidGrantIdException"

-- | The request was rejected because the specified grant token is not valid.
_InvalidGrantTokenException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidGrantTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidGrantTokenException"

-- | The request was rejected because the provided import token is invalid or
-- is associated with a different KMS key.
_InvalidImportTokenException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidImportTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidImportTokenException"

-- | The request was rejected for one of the following reasons:
--
-- -   The @KeyUsage@ value of the KMS key is incompatible with the API
--     operation.
--
-- -   The encryption algorithm or signing algorithm specified for the
--     operation is incompatible with the type of key material in the KMS
--     key @(KeySpec@).
--
-- For encrypting, decrypting, re-encrypting, and generating data keys, the
-- @KeyUsage@ must be @ENCRYPT_DECRYPT@. For signing and verifying
-- messages, the @KeyUsage@ must be @SIGN_VERIFY@. For generating and
-- verifying message authentication codes (MACs), the @KeyUsage@ must be
-- @GENERATE_VERIFY_MAC@. To find the @KeyUsage@ of a KMS key, use the
-- DescribeKey operation.
--
-- To find the encryption or signing algorithms supported for a particular
-- KMS key, use the DescribeKey operation.
_InvalidKeyUsageException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidKeyUsageException =
  Core._MatchServiceError
    defaultService
    "InvalidKeyUsageException"

-- | The request was rejected because the marker that specifies where
-- pagination should next begin is not valid.
_InvalidMarkerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidMarkerException =
  Core._MatchServiceError
    defaultService
    "InvalidMarkerException"

-- | The request was rejected because an internal exception occurred. The
-- request can be retried.
_KMSInternalException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_KMSInternalException =
  Core._MatchServiceError
    defaultService
    "KMSInternalException"

-- | The request was rejected because the HMAC verification failed. HMAC
-- verification fails when the HMAC computed by using the specified
-- message, HMAC KMS key, and MAC algorithm does not match the HMAC
-- specified in the request.
_KMSInvalidMacException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_KMSInvalidMacException =
  Core._MatchServiceError
    defaultService
    "KMSInvalidMacException"

-- | The request was rejected because the signature verification failed.
-- Signature verification fails when it cannot confirm that signature was
-- produced by signing the specified message with the specified KMS key and
-- signing algorithm.
_KMSInvalidSignatureException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_KMSInvalidSignatureException =
  Core._MatchServiceError
    defaultService
    "KMSInvalidSignatureException"

-- | The request was rejected because the state of the specified resource is
-- not valid for this request.
--
-- This exceptions means one of the following:
--
-- -   The key state of the KMS key is not compatible with the operation.
--
--     To find the key state, use the DescribeKey operation. For more
--     information about which key states are compatible with each KMS
--     operation, see
--     <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html Key states of KMS keys>
--     in the //Key Management Service Developer Guide// .
--
-- -   For cryptographic operations on KMS keys in custom key stores, this
--     exception represents a general failure with many possible causes. To
--     identify the cause, see the error message that accompanies the
--     exception.
_KMSInvalidStateException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_KMSInvalidStateException =
  Core._MatchServiceError
    defaultService
    "KMSInvalidStateException"

-- | The request was rejected because the specified KMS key was not
-- available. You can retry the request.
_KeyUnavailableException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_KeyUnavailableException =
  Core._MatchServiceError
    defaultService
    "KeyUnavailableException"

-- | The request was rejected because a quota was exceeded. For more
-- information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/limits.html Quotas>
-- in the /Key Management Service Developer Guide/.
_LimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The request was rejected because the specified policy is not
-- syntactically or semantically correct.
_MalformedPolicyDocumentException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_MalformedPolicyDocumentException =
  Core._MatchServiceError
    defaultService
    "MalformedPolicyDocumentException"

-- | The request was rejected because the specified entity or resource could
-- not be found.
_NotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"

-- | The request was rejected because one or more tags are not valid.
_TagException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TagException =
  Core._MatchServiceError
    defaultService
    "TagException"

-- | The request was rejected because a specified parameter is not supported
-- or a specified resource is not valid for this operation.
_UnsupportedOperationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UnsupportedOperationException =
  Core._MatchServiceError
    defaultService
    "UnsupportedOperationException"

-- | The request was rejected because the (@XksKeyId@) is already associated
-- with a KMS key in this external key store. Each KMS key in an external
-- key store must be associated with a different external key.
_XksKeyAlreadyInUseException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_XksKeyAlreadyInUseException =
  Core._MatchServiceError
    defaultService
    "XksKeyAlreadyInUseException"

-- | The request was rejected because the external key specified by the
-- @XksKeyId@ parameter did not meet the configuration requirements for an
-- external key store.
--
-- The external key must be an AES-256 symmetric key that is enabled and
-- performs encryption and decryption.
_XksKeyInvalidConfigurationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_XksKeyInvalidConfigurationException =
  Core._MatchServiceError
    defaultService
    "XksKeyInvalidConfigurationException"

-- | The request was rejected because the external key store proxy could not
-- find the external key. This exception is thrown when the value of the
-- @XksKeyId@ parameter doesn\'t identify a key in the external key manager
-- associated with the external key proxy.
--
-- Verify that the @XksKeyId@ represents an existing key in the external
-- key manager. Use the key identifier that the external key store proxy
-- uses to identify the key. For details, see the documentation provided
-- with your external key store proxy or key manager.
_XksKeyNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_XksKeyNotFoundException =
  Core._MatchServiceError
    defaultService
    "XksKeyNotFoundException"

-- | The request was rejected because the proxy credentials failed to
-- authenticate to the specified external key store proxy. The specified
-- external key store proxy rejected a status request from KMS due to
-- invalid credentials. This can indicate an error in the credentials or in
-- the identification of the external key store proxy.
_XksProxyIncorrectAuthenticationCredentialException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_XksProxyIncorrectAuthenticationCredentialException =
  Core._MatchServiceError
    defaultService
    "XksProxyIncorrectAuthenticationCredentialException"

-- | The request was rejected because the Amazon VPC endpoint service
-- configuration does not fulfill the requirements for an external key
-- store proxy. For details, see the exception message.
_XksProxyInvalidConfigurationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_XksProxyInvalidConfigurationException =
  Core._MatchServiceError
    defaultService
    "XksProxyInvalidConfigurationException"

-- | KMS cannot interpret the response it received from the external key
-- store proxy. The problem might be a poorly constructed response, but it
-- could also be a transient network issue. If you see this error
-- repeatedly, report it to the proxy vendor.
_XksProxyInvalidResponseException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_XksProxyInvalidResponseException =
  Core._MatchServiceError
    defaultService
    "XksProxyInvalidResponseException"

-- | The request was rejected because the concatenation of the
-- @XksProxyUriEndpoint@ is already associated with an external key store
-- in the Amazon Web Services account and Region. Each external key store
-- in an account and Region must use a unique external key store proxy
-- address.
_XksProxyUriEndpointInUseException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_XksProxyUriEndpointInUseException =
  Core._MatchServiceError
    defaultService
    "XksProxyUriEndpointInUseException"

-- | The request was rejected because the concatenation of the
-- @XksProxyUriEndpoint@ and @XksProxyUriPath@ is already associated with
-- an external key store in the Amazon Web Services account and Region.
-- Each external key store in an account and Region must use a unique
-- external key store proxy API address.
_XksProxyUriInUseException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_XksProxyUriInUseException =
  Core._MatchServiceError
    defaultService
    "XksProxyUriInUseException"

-- | KMS was unable to reach the specified @XksProxyUriPath@. The path must
-- be reachable before you create the external key store or update its
-- settings.
--
-- This exception is also thrown when the external key store proxy response
-- to a @GetHealthStatus@ request indicates that all external key manager
-- instances are unavailable.
_XksProxyUriUnreachableException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_XksProxyUriUnreachableException =
  Core._MatchServiceError
    defaultService
    "XksProxyUriUnreachableException"

-- | The request was rejected because the specified Amazon VPC endpoint
-- service is already associated with an external key store in the Amazon
-- Web Services account and Region. Each external key store in an Amazon
-- Web Services account and Region must use a different Amazon VPC endpoint
-- service.
_XksProxyVpcEndpointServiceInUseException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_XksProxyVpcEndpointServiceInUseException =
  Core._MatchServiceError
    defaultService
    "XksProxyVpcEndpointServiceInUseException"

-- | The request was rejected because the Amazon VPC endpoint service
-- configuration does not fulfill the requirements for an external key
-- store proxy. For details, see the exception message and
-- <kms/latest/developerguide/vpc-connectivity.html#xks-vpc-requirements review the requirements>
-- for Amazon VPC endpoint service connectivity for an external key store.
_XksProxyVpcEndpointServiceInvalidConfigurationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_XksProxyVpcEndpointServiceInvalidConfigurationException =
  Core._MatchServiceError
    defaultService
    "XksProxyVpcEndpointServiceInvalidConfigurationException"

-- | The request was rejected because KMS could not find the specified VPC
-- endpoint service. Use DescribeCustomKeyStores to verify the VPC endpoint
-- service name for the external key store. Also, confirm that the
-- @Allow principals@ list for the VPC endpoint service includes the KMS
-- service principal for the Region, such as
-- @cks.kms.us-east-1.amazonaws.com@.
_XksProxyVpcEndpointServiceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_XksProxyVpcEndpointServiceNotFoundException =
  Core._MatchServiceError
    defaultService
    "XksProxyVpcEndpointServiceNotFoundException"
