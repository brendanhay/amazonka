{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _CustomKeyStoreNameInUseException,
    _InvalidAliasNameException,
    _MalformedPolicyDocumentException,
    _CustomKeyStoreNotFoundException,
    _InvalidKeyUsageException,
    _NotFoundException,
    _KMSInvalidStateException,
    _InvalidMarkerException,
    _CloudHsmClusterNotFoundException,
    _IncorrectKeyException,
    _InvalidCiphertextException,
    _InvalidArnException,
    _CloudHsmClusterInvalidConfigurationException,
    _CustomKeyStoreHasCMKsException,
    _CloudHsmClusterNotRelatedException,
    _CustomKeyStoreInvalidStateException,
    _DisabledException,
    _UnsupportedOperationException,
    _KMSInvalidSignatureException,
    _LimitExceededException,
    _AlreadyExistsException,
    _ExpiredImportTokenException,
    _CloudHsmClusterInUseException,
    _CloudHsmClusterNotActiveException,
    _InvalidGrantTokenException,
    _DependencyTimeoutException,
    _IncorrectTrustAnchorException,
    _InvalidImportTokenException,
    _TagException,
    _KMSInternalException,
    _InvalidGrantIdException,
    _IncorrectKeyMaterialException,
    _KeyUnavailableException,

    -- * AlgorithmSpec
    AlgorithmSpec (..),

    -- * ConnectionErrorCodeType
    ConnectionErrorCodeType (..),

    -- * ConnectionStateType
    ConnectionStateType (..),

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

    -- * KeyState
    KeyState (..),

    -- * KeyUsageType
    KeyUsageType (..),

    -- * MessageType
    MessageType (..),

    -- * OriginType
    OriginType (..),

    -- * SigningAlgorithmSpec
    SigningAlgorithmSpec (..),

    -- * WrappingKeySpec
    WrappingKeySpec (..),

    -- * AliasListEntry
    AliasListEntry (..),
    newAliasListEntry,
    aliasListEntry_lastUpdatedDate,
    aliasListEntry_creationDate,
    aliasListEntry_aliasName,
    aliasListEntry_aliasArn,
    aliasListEntry_targetKeyId,

    -- * CustomKeyStoresListEntry
    CustomKeyStoresListEntry (..),
    newCustomKeyStoresListEntry,
    customKeyStoresListEntry_customKeyStoreName,
    customKeyStoresListEntry_connectionState,
    customKeyStoresListEntry_customKeyStoreId,
    customKeyStoresListEntry_cloudHsmClusterId,
    customKeyStoresListEntry_trustAnchorCertificate,
    customKeyStoresListEntry_creationDate,
    customKeyStoresListEntry_connectionErrorCode,

    -- * GrantConstraints
    GrantConstraints (..),
    newGrantConstraints,
    grantConstraints_encryptionContextEquals,
    grantConstraints_encryptionContextSubset,

    -- * GrantListEntry
    GrantListEntry (..),
    newGrantListEntry,
    grantListEntry_constraints,
    grantListEntry_operations,
    grantListEntry_creationDate,
    grantListEntry_name,
    grantListEntry_granteePrincipal,
    grantListEntry_grantId,
    grantListEntry_issuingAccount,
    grantListEntry_retiringPrincipal,
    grantListEntry_keyId,

    -- * KeyListEntry
    KeyListEntry (..),
    newKeyListEntry,
    keyListEntry_keyArn,
    keyListEntry_keyId,

    -- * KeyMetadata
    KeyMetadata (..),
    newKeyMetadata,
    keyMetadata_signingAlgorithms,
    keyMetadata_keyManager,
    keyMetadata_origin,
    keyMetadata_aWSAccountId,
    keyMetadata_customKeyStoreId,
    keyMetadata_encryptionAlgorithms,
    keyMetadata_cloudHsmClusterId,
    keyMetadata_keyState,
    keyMetadata_arn,
    keyMetadata_creationDate,
    keyMetadata_validTo,
    keyMetadata_enabled,
    keyMetadata_expirationModel,
    keyMetadata_description,
    keyMetadata_deletionDate,
    keyMetadata_keyUsage,
    keyMetadata_customerMasterKeySpec,
    keyMetadata_keyId,

    -- * ListGrantsResponse
    ListGrantsResponse (..),
    newListGrantsResponse,
    listGrantsResponse_nextMarker,
    listGrantsResponse_grants,
    listGrantsResponse_truncated,

    -- * Tag
    Tag (..),
    newTag,
    tag_tagKey,
    tag_tagValue,
  )
where

import Network.AWS.KMS.Types.AlgorithmSpec
import Network.AWS.KMS.Types.AliasListEntry
import Network.AWS.KMS.Types.ConnectionErrorCodeType
import Network.AWS.KMS.Types.ConnectionStateType
import Network.AWS.KMS.Types.CustomKeyStoresListEntry
import Network.AWS.KMS.Types.CustomerMasterKeySpec
import Network.AWS.KMS.Types.DataKeyPairSpec
import Network.AWS.KMS.Types.DataKeySpec
import Network.AWS.KMS.Types.EncryptionAlgorithmSpec
import Network.AWS.KMS.Types.ExpirationModelType
import Network.AWS.KMS.Types.GrantConstraints
import Network.AWS.KMS.Types.GrantListEntry
import Network.AWS.KMS.Types.GrantOperation
import Network.AWS.KMS.Types.KeyListEntry
import Network.AWS.KMS.Types.KeyManagerType
import Network.AWS.KMS.Types.KeyMetadata
import Network.AWS.KMS.Types.KeyState
import Network.AWS.KMS.Types.KeyUsageType
import Network.AWS.KMS.Types.ListGrantsResponse
import Network.AWS.KMS.Types.MessageType
import Network.AWS.KMS.Types.OriginType
import Network.AWS.KMS.Types.SigningAlgorithmSpec
import Network.AWS.KMS.Types.Tag
import Network.AWS.KMS.Types.WrappingKeySpec
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2014-11-01@ of the Amazon Key Management Service SDK configuration.
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev = "KMS",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcPrefix = "kms",
      Prelude._svcVersion = "2014-11-01",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError = Prelude.parseJSONError "KMS",
      Prelude._svcRetry = retry
    }
  where
    retry =
      Prelude.Exponential
        { Prelude._retryBase = 5.0e-2,
          Prelude._retryGrowth = 2,
          Prelude._retryAttempts = 5,
          Prelude._retryCheck = check
        }
    check e
      | Lens.has (Prelude.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Prelude.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Prelude.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Prelude.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Prelude.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Prelude.hasCode "RequestThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Prelude.hasCode "ThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Prelude.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Prelude.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Prelude.hasCode "ThrottlingException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Prelude.hasCode "Throttling"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | The request was rejected because the specified custom key store name is
-- already assigned to another custom key store in the account. Try again
-- with a custom key store name that is unique in the account.
_CustomKeyStoreNameInUseException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_CustomKeyStoreNameInUseException =
  Prelude._MatchServiceError
    defaultService
    "CustomKeyStoreNameInUseException"

-- | The request was rejected because the specified alias name is not valid.
_InvalidAliasNameException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidAliasNameException =
  Prelude._MatchServiceError
    defaultService
    "InvalidAliasNameException"

-- | The request was rejected because the specified policy is not
-- syntactically or semantically correct.
_MalformedPolicyDocumentException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_MalformedPolicyDocumentException =
  Prelude._MatchServiceError
    defaultService
    "MalformedPolicyDocumentException"

-- | The request was rejected because AWS KMS cannot find a custom key store
-- with the specified key store name or ID.
_CustomKeyStoreNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_CustomKeyStoreNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "CustomKeyStoreNotFoundException"

-- | The request was rejected for one of the following reasons:
--
-- -   The @KeyUsage@ value of the CMK is incompatible with the API
--     operation.
--
-- -   The encryption algorithm or signing algorithm specified for the
--     operation is incompatible with the type of key material in the CMK
--     @(CustomerMasterKeySpec@).
--
-- For encrypting, decrypting, re-encrypting, and generating data keys, the
-- @KeyUsage@ must be @ENCRYPT_DECRYPT@. For signing and verifying, the
-- @KeyUsage@ must be @SIGN_VERIFY@. To find the @KeyUsage@ of a CMK, use
-- the DescribeKey operation.
--
-- To find the encryption or signing algorithms supported for a particular
-- CMK, use the DescribeKey operation.
_InvalidKeyUsageException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidKeyUsageException =
  Prelude._MatchServiceError
    defaultService
    "InvalidKeyUsageException"

-- | The request was rejected because the specified entity or resource could
-- not be found.
_NotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NotFoundException =
  Prelude._MatchServiceError
    defaultService
    "NotFoundException"

-- | The request was rejected because the state of the specified resource is
-- not valid for this request.
--
-- For more information about how key state affects the use of a CMK, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key>
-- in the //AWS Key Management Service Developer Guide// .
_KMSInvalidStateException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_KMSInvalidStateException =
  Prelude._MatchServiceError
    defaultService
    "KMSInvalidStateException"

-- | The request was rejected because the marker that specifies where
-- pagination should next begin is not valid.
_InvalidMarkerException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidMarkerException =
  Prelude._MatchServiceError
    defaultService
    "InvalidMarkerException"

-- | The request was rejected because AWS KMS cannot find the AWS CloudHSM
-- cluster with the specified cluster ID. Retry the request with a
-- different cluster ID.
_CloudHsmClusterNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_CloudHsmClusterNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "CloudHsmClusterNotFoundException"

-- | The request was rejected because the specified CMK cannot decrypt the
-- data. The @KeyId@ in a Decrypt request and the @SourceKeyId@ in a
-- ReEncrypt request must identify the same CMK that was used to encrypt
-- the ciphertext.
_IncorrectKeyException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_IncorrectKeyException =
  Prelude._MatchServiceError
    defaultService
    "IncorrectKeyException"

-- | From the Decrypt or ReEncrypt operation, the request was rejected
-- because the specified ciphertext, or additional authenticated data
-- incorporated into the ciphertext, such as the encryption context, is
-- corrupted, missing, or otherwise invalid.
--
-- From the ImportKeyMaterial operation, the request was rejected because
-- AWS KMS could not decrypt the encrypted (wrapped) key material.
_InvalidCiphertextException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidCiphertextException =
  Prelude._MatchServiceError
    defaultService
    "InvalidCiphertextException"

-- | The request was rejected because a specified ARN, or an ARN in a key
-- policy, is not valid.
_InvalidArnException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidArnException =
  Prelude._MatchServiceError
    defaultService
    "InvalidArnException"

-- | The request was rejected because the associated AWS CloudHSM cluster did
-- not meet the configuration requirements for a custom key store.
--
-- -   The cluster must be configured with private subnets in at least two
--     different Availability Zones in the Region.
--
-- -   The
--     <https://docs.aws.amazon.com/cloudhsm/latest/userguide/configure-sg.html security group for the cluster>
--     (cloudhsm-cluster-/\<cluster-id>/-sg) must include inbound rules and
--     outbound rules that allow TCP traffic on ports 2223-2225. The
--     __Source__ in the inbound rules and the __Destination__ in the
--     outbound rules must match the security group ID. These rules are set
--     by default when you create the cluster. Do not delete or change
--     them. To get information about a particular security group, use the
--     <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeSecurityGroups.html DescribeSecurityGroups>
--     operation.
--
-- -   The cluster must contain at least as many HSMs as the operation
--     requires. To add HSMs, use the AWS CloudHSM
--     <https://docs.aws.amazon.com/cloudhsm/latest/APIReference/API_CreateHsm.html CreateHsm>
--     operation.
--
--     For the CreateCustomKeyStore, UpdateCustomKeyStore, and CreateKey
--     operations, the AWS CloudHSM cluster must have at least two active
--     HSMs, each in a different Availability Zone. For the
--     ConnectCustomKeyStore operation, the AWS CloudHSM must contain at
--     least one active HSM.
--
-- For information about the requirements for an AWS CloudHSM cluster that
-- is associated with a custom key store, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/create-keystore.html#before-keystore Assemble the Prerequisites>
-- in the /AWS Key Management Service Developer Guide/. For information
-- about creating a private subnet for an AWS CloudHSM cluster, see
-- <https://docs.aws.amazon.com/cloudhsm/latest/userguide/create-subnets.html Create a Private Subnet>
-- in the /AWS CloudHSM User Guide/. For information about cluster security
-- groups, see
-- <https://docs.aws.amazon.com/cloudhsm/latest/userguide/configure-sg.html Configure a Default Security Group>
-- in the //AWS CloudHSM User Guide// .
_CloudHsmClusterInvalidConfigurationException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_CloudHsmClusterInvalidConfigurationException =
  Prelude._MatchServiceError
    defaultService
    "CloudHsmClusterInvalidConfigurationException"

-- | The request was rejected because the custom key store contains AWS KMS
-- customer master keys (CMKs). After verifying that you do not need to use
-- the CMKs, use the ScheduleKeyDeletion operation to delete the CMKs.
-- After they are deleted, you can delete the custom key store.
_CustomKeyStoreHasCMKsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_CustomKeyStoreHasCMKsException =
  Prelude._MatchServiceError
    defaultService
    "CustomKeyStoreHasCMKsException"

-- | The request was rejected because the specified AWS CloudHSM cluster has
-- a different cluster certificate than the original cluster. You cannot
-- use the operation to specify an unrelated cluster.
--
-- Specify a cluster that shares a backup history with the original
-- cluster. This includes clusters that were created from a backup of the
-- current cluster, and clusters that were created from the same backup
-- that produced the current cluster.
--
-- Clusters that share a backup history have the same cluster certificate.
-- To view the cluster certificate of a cluster, use the
-- <https://docs.aws.amazon.com/cloudhsm/latest/APIReference/API_DescribeClusters.html DescribeClusters>
-- operation.
_CloudHsmClusterNotRelatedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_CloudHsmClusterNotRelatedException =
  Prelude._MatchServiceError
    defaultService
    "CloudHsmClusterNotRelatedException"

-- | The request was rejected because of the @ConnectionState@ of the custom
-- key store. To get the @ConnectionState@ of a custom key store, use the
-- DescribeCustomKeyStores operation.
--
-- This exception is thrown under the following conditions:
--
-- -   You requested the CreateKey or GenerateRandom operation in a custom
--     key store that is not connected. These operations are valid only
--     when the custom key store @ConnectionState@ is @CONNECTED@.
--
-- -   You requested the UpdateCustomKeyStore or DeleteCustomKeyStore
--     operation on a custom key store that is not disconnected. This
--     operation is valid only when the custom key store @ConnectionState@
--     is @DISCONNECTED@.
--
-- -   You requested the ConnectCustomKeyStore operation on a custom key
--     store with a @ConnectionState@ of @DISCONNECTING@ or @FAILED@. This
--     operation is valid for all other @ConnectionState@ values.
_CustomKeyStoreInvalidStateException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_CustomKeyStoreInvalidStateException =
  Prelude._MatchServiceError
    defaultService
    "CustomKeyStoreInvalidStateException"

-- | The request was rejected because the specified CMK is not enabled.
_DisabledException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DisabledException =
  Prelude._MatchServiceError
    defaultService
    "DisabledException"

-- | The request was rejected because a specified parameter is not supported
-- or a specified resource is not valid for this operation.
_UnsupportedOperationException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_UnsupportedOperationException =
  Prelude._MatchServiceError
    defaultService
    "UnsupportedOperationException"

-- | The request was rejected because the signature verification failed.
-- Signature verification fails when it cannot confirm that signature was
-- produced by signing the specified message with the specified CMK and
-- signing algorithm.
_KMSInvalidSignatureException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_KMSInvalidSignatureException =
  Prelude._MatchServiceError
    defaultService
    "KMSInvalidSignatureException"

-- | The request was rejected because a quota was exceeded. For more
-- information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/limits.html Quotas>
-- in the /AWS Key Management Service Developer Guide/.
_LimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_LimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The request was rejected because it attempted to create a resource that
-- already exists.
_AlreadyExistsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_AlreadyExistsException =
  Prelude._MatchServiceError
    defaultService
    "AlreadyExistsException"

-- | The request was rejected because the specified import token is expired.
-- Use GetParametersForImport to get a new import token and public key, use
-- the new public key to encrypt the key material, and then try the request
-- again.
_ExpiredImportTokenException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ExpiredImportTokenException =
  Prelude._MatchServiceError
    defaultService
    "ExpiredImportTokenException"

-- | The request was rejected because the specified AWS CloudHSM cluster is
-- already associated with a custom key store or it shares a backup history
-- with a cluster that is associated with a custom key store. Each custom
-- key store must be associated with a different AWS CloudHSM cluster.
--
-- Clusters that share a backup history have the same cluster certificate.
-- To view the cluster certificate of a cluster, use the
-- <https://docs.aws.amazon.com/cloudhsm/latest/APIReference/API_DescribeClusters.html DescribeClusters>
-- operation.
_CloudHsmClusterInUseException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_CloudHsmClusterInUseException =
  Prelude._MatchServiceError
    defaultService
    "CloudHsmClusterInUseException"

-- | The request was rejected because the AWS CloudHSM cluster that is
-- associated with the custom key store is not active. Initialize and
-- activate the cluster and try the command again. For detailed
-- instructions, see
-- <https://docs.aws.amazon.com/cloudhsm/latest/userguide/getting-started.html Getting Started>
-- in the /AWS CloudHSM User Guide/.
_CloudHsmClusterNotActiveException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_CloudHsmClusterNotActiveException =
  Prelude._MatchServiceError
    defaultService
    "CloudHsmClusterNotActiveException"

-- | The request was rejected because the specified grant token is not valid.
_InvalidGrantTokenException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidGrantTokenException =
  Prelude._MatchServiceError
    defaultService
    "InvalidGrantTokenException"

-- | The system timed out while trying to fulfill the request. The request
-- can be retried.
_DependencyTimeoutException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DependencyTimeoutException =
  Prelude._MatchServiceError
    defaultService
    "DependencyTimeoutException"

-- | The request was rejected because the trust anchor certificate in the
-- request is not the trust anchor certificate for the specified AWS
-- CloudHSM cluster.
--
-- When you
-- <https://docs.aws.amazon.com/cloudhsm/latest/userguide/initialize-cluster.html#sign-csr initialize the cluster>,
-- you create the trust anchor certificate and save it in the
-- @customerCA.crt@ file.
_IncorrectTrustAnchorException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_IncorrectTrustAnchorException =
  Prelude._MatchServiceError
    defaultService
    "IncorrectTrustAnchorException"

-- | The request was rejected because the provided import token is invalid or
-- is associated with a different customer master key (CMK).
_InvalidImportTokenException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidImportTokenException =
  Prelude._MatchServiceError
    defaultService
    "InvalidImportTokenException"

-- | The request was rejected because one or more tags are not valid.
_TagException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TagException =
  Prelude._MatchServiceError
    defaultService
    "TagException"

-- | The request was rejected because an internal exception occurred. The
-- request can be retried.
_KMSInternalException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_KMSInternalException =
  Prelude._MatchServiceError
    defaultService
    "KMSInternalException"

-- | The request was rejected because the specified @GrantId@ is not valid.
_InvalidGrantIdException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidGrantIdException =
  Prelude._MatchServiceError
    defaultService
    "InvalidGrantIdException"

-- | The request was rejected because the key material in the request is,
-- expired, invalid, or is not the same key material that was previously
-- imported into this customer master key (CMK).
_IncorrectKeyMaterialException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_IncorrectKeyMaterialException =
  Prelude._MatchServiceError
    defaultService
    "IncorrectKeyMaterialException"

-- | The request was rejected because the specified CMK was not available.
-- You can retry the request.
_KeyUnavailableException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_KeyUnavailableException =
  Prelude._MatchServiceError
    defaultService
    "KeyUnavailableException"
