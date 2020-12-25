-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    _InvalidMarkerException,
    _KMSInvalidStateException,
    _InvalidKeyUsageException,
    _MalformedPolicyDocumentException,
    _CustomKeyStoreNameInUseException,
    _UnsupportedOperationException,
    _DisabledException,
    _KeyUnavailableException,
    _IncorrectKeyMaterialException,
    _KMSInternalException,
    _TagException,
    _CustomKeyStoreHasCMKsException,
    _InvalidImportTokenException,
    _CloudHsmClusterNotRelatedException,
    _IncorrectTrustAnchorException,
    _CloudHsmClusterInvalidConfigurationException,
    _CloudHsmClusterNotActiveException,
    _CloudHsmClusterNotFoundException,
    _NotFoundException,
    _KMSInvalidSignatureException,
    _InvalidAliasNameException,
    _CustomKeyStoreNotFoundException,
    _CustomKeyStoreInvalidStateException,
    _InvalidGrantIdException,
    _InvalidGrantTokenException,
    _InvalidArnException,
    _DependencyTimeoutException,
    _ExpiredImportTokenException,
    _InvalidCiphertextException,
    _CloudHsmClusterInUseException,
    _IncorrectKeyException,
    _AlreadyExistsException,
    _LimitExceededException,

    -- * ListGrantsResponse
    ListGrantsResponse (..),
    mkListGrantsResponse,
    lgrGrants,
    lgrNextMarker,
    lgrTruncated,

    -- * KeyUsageType
    KeyUsageType (..),

    -- * Tag
    Tag (..),
    mkTag,
    tTagKey,
    tTagValue,

    -- * KeyMetadata
    KeyMetadata (..),
    mkKeyMetadata,
    kmKeyId,
    kmAWSAccountId,
    kmArn,
    kmCloudHsmClusterId,
    kmCreationDate,
    kmCustomKeyStoreId,
    kmCustomerMasterKeySpec,
    kmDeletionDate,
    kmDescription,
    kmEnabled,
    kmEncryptionAlgorithms,
    kmExpirationModel,
    kmKeyManager,
    kmKeyState,
    kmKeyUsage,
    kmOrigin,
    kmSigningAlgorithms,
    kmValidTo,

    -- * MarkerType
    MarkerType (..),

    -- * DataKeySpec
    DataKeySpec (..),

    -- * CustomerMasterKeySpec
    CustomerMasterKeySpec (..),

    -- * AlgorithmSpec
    AlgorithmSpec (..),

    -- * GrantTokenType
    GrantTokenType (..),

    -- * EncryptionAlgorithmSpec
    EncryptionAlgorithmSpec (..),

    -- * TagKeyType
    TagKeyType (..),

    -- * KeyState
    KeyState (..),

    -- * GrantConstraints
    GrantConstraints (..),
    mkGrantConstraints,
    gcEncryptionContextEquals,
    gcEncryptionContextSubset,

    -- * CloudHsmClusterIdType
    CloudHsmClusterIdType (..),

    -- * AliasListEntry
    AliasListEntry (..),
    mkAliasListEntry,
    aleAliasArn,
    aleAliasName,
    aleTargetKeyId,

    -- * GrantListEntry
    GrantListEntry (..),
    mkGrantListEntry,
    gleConstraints,
    gleCreationDate,
    gleGrantId,
    gleGranteePrincipal,
    gleIssuingAccount,
    gleKeyId,
    gleName,
    gleOperations,
    gleRetiringPrincipal,

    -- * PolicyType
    PolicyType (..),

    -- * EncryptionContextValue
    EncryptionContextValue (..),

    -- * MessageType
    MessageType (..),

    -- * DescriptionType
    DescriptionType (..),

    -- * CustomKeyStoreIdType
    CustomKeyStoreIdType (..),

    -- * WrappingKeySpec
    WrappingKeySpec (..),

    -- * ConnectionStateType
    ConnectionStateType (..),

    -- * CustomKeyStoreNameType
    CustomKeyStoreNameType (..),

    -- * ExpirationModelType
    ExpirationModelType (..),

    -- * KeyManagerType
    KeyManagerType (..),

    -- * OriginType
    OriginType (..),

    -- * EncryptionContextKey
    EncryptionContextKey (..),

    -- * PrincipalIdType
    PrincipalIdType (..),

    -- * KeyStorePasswordType
    KeyStorePasswordType (..),

    -- * PolicyNameType
    PolicyNameType (..),

    -- * KeyIdType
    KeyIdType (..),

    -- * ArnType
    ArnType (..),

    -- * TrustAnchorCertificateType
    TrustAnchorCertificateType (..),

    -- * DataKeyPairSpec
    DataKeyPairSpec (..),

    -- * GrantOperation
    GrantOperation (..),

    -- * ConnectionErrorCodeType
    ConnectionErrorCodeType (..),

    -- * CustomKeyStoresListEntry
    CustomKeyStoresListEntry (..),
    mkCustomKeyStoresListEntry,
    cksleCloudHsmClusterId,
    cksleConnectionErrorCode,
    cksleConnectionState,
    cksleCreationDate,
    cksleCustomKeyStoreId,
    cksleCustomKeyStoreName,
    cksleTrustAnchorCertificate,

    -- * SigningAlgorithmSpec
    SigningAlgorithmSpec (..),

    -- * KeyListEntry
    KeyListEntry (..),
    mkKeyListEntry,
    kleKeyArn,
    kleKeyId,

    -- * KeyId
    KeyId (..),

    -- * NextMarker
    NextMarker (..),

    -- * CustomKeyStoreId
    CustomKeyStoreId (..),

    -- * RetiringPrincipal
    RetiringPrincipal (..),

    -- * Marker
    Marker (..),

    -- * TagKey
    TagKey (..),

    -- * TagValue
    TagValue (..),

    -- * AWSAccountId
    AWSAccountId (..),

    -- * Arn
    Arn (..),

    -- * CloudHsmClusterId
    CloudHsmClusterId (..),

    -- * Description
    Description (..),

    -- * GrantId
    GrantId (..),

    -- * GrantToken
    GrantToken (..),

    -- * Policy
    Policy (..),

    -- * AliasArn
    AliasArn (..),

    -- * AliasName
    AliasName (..),

    -- * TargetKeyId
    TargetKeyId (..),

    -- * GranteePrincipal
    GranteePrincipal (..),

    -- * IssuingAccount
    IssuingAccount (..),

    -- * Name
    Name (..),

    -- * DestinationKeyId
    DestinationKeyId (..),

    -- * SourceKeyId
    SourceKeyId (..),

    -- * PolicyName
    PolicyName (..),
  )
where

import Network.AWS.KMS.Types.AWSAccountId
import Network.AWS.KMS.Types.AlgorithmSpec
import Network.AWS.KMS.Types.AliasArn
import Network.AWS.KMS.Types.AliasListEntry
import Network.AWS.KMS.Types.AliasName
import Network.AWS.KMS.Types.Arn
import Network.AWS.KMS.Types.ArnType
import Network.AWS.KMS.Types.CloudHsmClusterId
import Network.AWS.KMS.Types.CloudHsmClusterIdType
import Network.AWS.KMS.Types.ConnectionErrorCodeType
import Network.AWS.KMS.Types.ConnectionStateType
import Network.AWS.KMS.Types.CustomKeyStoreId
import Network.AWS.KMS.Types.CustomKeyStoreIdType
import Network.AWS.KMS.Types.CustomKeyStoreNameType
import Network.AWS.KMS.Types.CustomKeyStoresListEntry
import Network.AWS.KMS.Types.CustomerMasterKeySpec
import Network.AWS.KMS.Types.DataKeyPairSpec
import Network.AWS.KMS.Types.DataKeySpec
import Network.AWS.KMS.Types.Description
import Network.AWS.KMS.Types.DescriptionType
import Network.AWS.KMS.Types.DestinationKeyId
import Network.AWS.KMS.Types.EncryptionAlgorithmSpec
import Network.AWS.KMS.Types.EncryptionContextKey
import Network.AWS.KMS.Types.EncryptionContextValue
import Network.AWS.KMS.Types.ExpirationModelType
import Network.AWS.KMS.Types.GrantConstraints
import Network.AWS.KMS.Types.GrantId
import Network.AWS.KMS.Types.GrantListEntry
import Network.AWS.KMS.Types.GrantOperation
import Network.AWS.KMS.Types.GrantToken
import Network.AWS.KMS.Types.GrantTokenType
import Network.AWS.KMS.Types.GranteePrincipal
import Network.AWS.KMS.Types.IssuingAccount
import Network.AWS.KMS.Types.KeyId
import Network.AWS.KMS.Types.KeyIdType
import Network.AWS.KMS.Types.KeyListEntry
import Network.AWS.KMS.Types.KeyManagerType
import Network.AWS.KMS.Types.KeyMetadata
import Network.AWS.KMS.Types.KeyState
import Network.AWS.KMS.Types.KeyStorePasswordType
import Network.AWS.KMS.Types.KeyUsageType
import Network.AWS.KMS.Types.ListGrantsResponse
import Network.AWS.KMS.Types.Marker
import Network.AWS.KMS.Types.MarkerType
import Network.AWS.KMS.Types.MessageType
import Network.AWS.KMS.Types.Name
import Network.AWS.KMS.Types.NextMarker
import Network.AWS.KMS.Types.OriginType
import Network.AWS.KMS.Types.Policy
import Network.AWS.KMS.Types.PolicyName
import Network.AWS.KMS.Types.PolicyNameType
import Network.AWS.KMS.Types.PolicyType
import Network.AWS.KMS.Types.PrincipalIdType
import Network.AWS.KMS.Types.RetiringPrincipal
import Network.AWS.KMS.Types.SigningAlgorithmSpec
import Network.AWS.KMS.Types.SourceKeyId
import Network.AWS.KMS.Types.Tag
import Network.AWS.KMS.Types.TagKey
import Network.AWS.KMS.Types.TagKeyType
import Network.AWS.KMS.Types.TagValue
import Network.AWS.KMS.Types.TargetKeyId
import Network.AWS.KMS.Types.TrustAnchorCertificateType
import Network.AWS.KMS.Types.WrappingKeySpec
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2014-11-01@ of the Amazon Key Management Service SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "KMS",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "kms",
      Core._svcVersion = "2014-11-01",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "KMS",
      Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
      | Lens.has
          (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling_exception"
      | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e =
        Core.Just "throttling"
      | Lens.has
          ( Core.hasCode "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
      | Core.otherwise = Core.Nothing

-- | The request was rejected because the marker that specifies where pagination should next begin is not valid.
_InvalidMarkerException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidMarkerException =
  Core._MatchServiceError mkServiceConfig "InvalidMarkerException"
{-# DEPRECATED _InvalidMarkerException "Use generic-lens or generic-optics instead." #-}

-- | The request was rejected because the state of the specified resource is not valid for this request.
--
-- For more information about how key state affects the use of a CMK, see <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key> in the /\/AWS Key Management Service Developer Guide\/ / .
_KMSInvalidStateException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_KMSInvalidStateException =
  Core._MatchServiceError
    mkServiceConfig
    "KMSInvalidStateException"
{-# DEPRECATED _KMSInvalidStateException "Use generic-lens or generic-optics instead." #-}

-- | The request was rejected for one of the following reasons:
--
--
--     * The @KeyUsage@ value of the CMK is incompatible with the API operation.
--
--
--     * The encryption algorithm or signing algorithm specified for the operation is incompatible with the type of key material in the CMK @(CustomerMasterKeySpec@ ).
--
--
-- For encrypting, decrypting, re-encrypting, and generating data keys, the @KeyUsage@ must be @ENCRYPT_DECRYPT@ . For signing and verifying, the @KeyUsage@ must be @SIGN_VERIFY@ . To find the @KeyUsage@ of a CMK, use the 'DescribeKey' operation.
-- To find the encryption or signing algorithms supported for a particular CMK, use the 'DescribeKey' operation.
_InvalidKeyUsageException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidKeyUsageException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidKeyUsageException"
{-# DEPRECATED _InvalidKeyUsageException "Use generic-lens or generic-optics instead." #-}

-- | The request was rejected because the specified policy is not syntactically or semantically correct.
_MalformedPolicyDocumentException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MalformedPolicyDocumentException =
  Core._MatchServiceError
    mkServiceConfig
    "MalformedPolicyDocumentException"
{-# DEPRECATED _MalformedPolicyDocumentException "Use generic-lens or generic-optics instead." #-}

-- | The request was rejected because the specified custom key store name is already assigned to another custom key store in the account. Try again with a custom key store name that is unique in the account.
_CustomKeyStoreNameInUseException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CustomKeyStoreNameInUseException =
  Core._MatchServiceError
    mkServiceConfig
    "CustomKeyStoreNameInUseException"
{-# DEPRECATED _CustomKeyStoreNameInUseException "Use generic-lens or generic-optics instead." #-}

-- | The request was rejected because a specified parameter is not supported or a specified resource is not valid for this operation.
_UnsupportedOperationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnsupportedOperationException =
  Core._MatchServiceError
    mkServiceConfig
    "UnsupportedOperationException"
{-# DEPRECATED _UnsupportedOperationException "Use generic-lens or generic-optics instead." #-}

-- | The request was rejected because the specified CMK is not enabled.
_DisabledException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DisabledException =
  Core._MatchServiceError mkServiceConfig "DisabledException"
{-# DEPRECATED _DisabledException "Use generic-lens or generic-optics instead." #-}

-- | The request was rejected because the specified CMK was not available. You can retry the request.
_KeyUnavailableException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_KeyUnavailableException =
  Core._MatchServiceError mkServiceConfig "KeyUnavailableException"
{-# DEPRECATED _KeyUnavailableException "Use generic-lens or generic-optics instead." #-}

-- | The request was rejected because the key material in the request is, expired, invalid, or is not the same key material that was previously imported into this customer master key (CMK).
_IncorrectKeyMaterialException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_IncorrectKeyMaterialException =
  Core._MatchServiceError
    mkServiceConfig
    "IncorrectKeyMaterialException"
{-# DEPRECATED _IncorrectKeyMaterialException "Use generic-lens or generic-optics instead." #-}

-- | The request was rejected because an internal exception occurred. The request can be retried.
_KMSInternalException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_KMSInternalException =
  Core._MatchServiceError mkServiceConfig "KMSInternalException"
{-# DEPRECATED _KMSInternalException "Use generic-lens or generic-optics instead." #-}

-- | The request was rejected because one or more tags are not valid.
_TagException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TagException =
  Core._MatchServiceError mkServiceConfig "TagException"
{-# DEPRECATED _TagException "Use generic-lens or generic-optics instead." #-}

-- | The request was rejected because the custom key store contains AWS KMS customer master keys (CMKs). After verifying that you do not need to use the CMKs, use the 'ScheduleKeyDeletion' operation to delete the CMKs. After they are deleted, you can delete the custom key store.
_CustomKeyStoreHasCMKsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CustomKeyStoreHasCMKsException =
  Core._MatchServiceError
    mkServiceConfig
    "CustomKeyStoreHasCMKsException"
{-# DEPRECATED _CustomKeyStoreHasCMKsException "Use generic-lens or generic-optics instead." #-}

-- | The request was rejected because the provided import token is invalid or is associated with a different customer master key (CMK).
_InvalidImportTokenException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidImportTokenException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidImportTokenException"
{-# DEPRECATED _InvalidImportTokenException "Use generic-lens or generic-optics instead." #-}

-- | The request was rejected because the specified AWS CloudHSM cluster has a different cluster certificate than the original cluster. You cannot use the operation to specify an unrelated cluster.
--
-- Specify a cluster that shares a backup history with the original cluster. This includes clusters that were created from a backup of the current cluster, and clusters that were created from the same backup that produced the current cluster.
-- Clusters that share a backup history have the same cluster certificate. To view the cluster certificate of a cluster, use the <https://docs.aws.amazon.com/cloudhsm/latest/APIReference/API_DescribeClusters.html DescribeClusters> operation.
_CloudHsmClusterNotRelatedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CloudHsmClusterNotRelatedException =
  Core._MatchServiceError
    mkServiceConfig
    "CloudHsmClusterNotRelatedException"
{-# DEPRECATED _CloudHsmClusterNotRelatedException "Use generic-lens or generic-optics instead." #-}

-- | The request was rejected because the trust anchor certificate in the request is not the trust anchor certificate for the specified AWS CloudHSM cluster.
--
-- When you <https://docs.aws.amazon.com/cloudhsm/latest/userguide/initialize-cluster.html#sign-csr initialize the cluster> , you create the trust anchor certificate and save it in the @customerCA.crt@ file.
_IncorrectTrustAnchorException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_IncorrectTrustAnchorException =
  Core._MatchServiceError
    mkServiceConfig
    "IncorrectTrustAnchorException"
{-# DEPRECATED _IncorrectTrustAnchorException "Use generic-lens or generic-optics instead." #-}

-- | The request was rejected because the associated AWS CloudHSM cluster did not meet the configuration requirements for a custom key store.
--
--
--     * The cluster must be configured with private subnets in at least two different Availability Zones in the Region.
--
--
--     * The <https://docs.aws.amazon.com/cloudhsm/latest/userguide/configure-sg.html security group for the cluster> (cloudhsm-cluster-/<cluster-id>/ -sg) must include inbound rules and outbound rules that allow TCP traffic on ports 2223-2225. The __Source__ in the inbound rules and the __Destination__ in the outbound rules must match the security group ID. These rules are set by default when you create the cluster. Do not delete or change them. To get information about a particular security group, use the <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeSecurityGroups.html DescribeSecurityGroups> operation.
--
--
--     * The cluster must contain at least as many HSMs as the operation requires. To add HSMs, use the AWS CloudHSM <https://docs.aws.amazon.com/cloudhsm/latest/APIReference/API_CreateHsm.html CreateHsm> operation.
-- For the 'CreateCustomKeyStore' , 'UpdateCustomKeyStore' , and 'CreateKey' operations, the AWS CloudHSM cluster must have at least two active HSMs, each in a different Availability Zone. For the 'ConnectCustomKeyStore' operation, the AWS CloudHSM must contain at least one active HSM.
--
--
-- For information about the requirements for an AWS CloudHSM cluster that is associated with a custom key store, see <https://docs.aws.amazon.com/kms/latest/developerguide/create-keystore.html#before-keystore Assemble the Prerequisites> in the /AWS Key Management Service Developer Guide/ . For information about creating a private subnet for an AWS CloudHSM cluster, see <https://docs.aws.amazon.com/cloudhsm/latest/userguide/create-subnets.html Create a Private Subnet> in the /AWS CloudHSM User Guide/ . For information about cluster security groups, see <https://docs.aws.amazon.com/cloudhsm/latest/userguide/configure-sg.html Configure a Default Security Group> in the /\/AWS CloudHSM User Guide\/ / .
_CloudHsmClusterInvalidConfigurationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CloudHsmClusterInvalidConfigurationException =
  Core._MatchServiceError
    mkServiceConfig
    "CloudHsmClusterInvalidConfigurationException"
{-# DEPRECATED _CloudHsmClusterInvalidConfigurationException "Use generic-lens or generic-optics instead." #-}

-- | The request was rejected because the AWS CloudHSM cluster that is associated with the custom key store is not active. Initialize and activate the cluster and try the command again. For detailed instructions, see <https://docs.aws.amazon.com/cloudhsm/latest/userguide/getting-started.html Getting Started> in the /AWS CloudHSM User Guide/ .
_CloudHsmClusterNotActiveException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CloudHsmClusterNotActiveException =
  Core._MatchServiceError
    mkServiceConfig
    "CloudHsmClusterNotActiveException"
{-# DEPRECATED _CloudHsmClusterNotActiveException "Use generic-lens or generic-optics instead." #-}

-- | The request was rejected because AWS KMS cannot find the AWS CloudHSM cluster with the specified cluster ID. Retry the request with a different cluster ID.
_CloudHsmClusterNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CloudHsmClusterNotFoundException =
  Core._MatchServiceError
    mkServiceConfig
    "CloudHsmClusterNotFoundException"
{-# DEPRECATED _CloudHsmClusterNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | The request was rejected because the specified entity or resource could not be found.
_NotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError mkServiceConfig "NotFoundException"
{-# DEPRECATED _NotFoundException "Use generic-lens or generic-optics instead." #-}

-- | The request was rejected because the signature verification failed. Signature verification fails when it cannot confirm that signature was produced by signing the specified message with the specified CMK and signing algorithm.
_KMSInvalidSignatureException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_KMSInvalidSignatureException =
  Core._MatchServiceError
    mkServiceConfig
    "KMSInvalidSignatureException"
{-# DEPRECATED _KMSInvalidSignatureException "Use generic-lens or generic-optics instead." #-}

-- | The request was rejected because the specified alias name is not valid.
_InvalidAliasNameException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidAliasNameException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidAliasNameException"
{-# DEPRECATED _InvalidAliasNameException "Use generic-lens or generic-optics instead." #-}

-- | The request was rejected because AWS KMS cannot find a custom key store with the specified key store name or ID.
_CustomKeyStoreNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CustomKeyStoreNotFoundException =
  Core._MatchServiceError
    mkServiceConfig
    "CustomKeyStoreNotFoundException"
{-# DEPRECATED _CustomKeyStoreNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | The request was rejected because of the @ConnectionState@ of the custom key store. To get the @ConnectionState@ of a custom key store, use the 'DescribeCustomKeyStores' operation.
--
-- This exception is thrown under the following conditions:
--
--     * You requested the 'CreateKey' or 'GenerateRandom' operation in a custom key store that is not connected. These operations are valid only when the custom key store @ConnectionState@ is @CONNECTED@ .
--
--
--     * You requested the 'UpdateCustomKeyStore' or 'DeleteCustomKeyStore' operation on a custom key store that is not disconnected. This operation is valid only when the custom key store @ConnectionState@ is @DISCONNECTED@ .
--
--
--     * You requested the 'ConnectCustomKeyStore' operation on a custom key store with a @ConnectionState@ of @DISCONNECTING@ or @FAILED@ . This operation is valid for all other @ConnectionState@ values.
_CustomKeyStoreInvalidStateException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CustomKeyStoreInvalidStateException =
  Core._MatchServiceError
    mkServiceConfig
    "CustomKeyStoreInvalidStateException"
{-# DEPRECATED _CustomKeyStoreInvalidStateException "Use generic-lens or generic-optics instead." #-}

-- | The request was rejected because the specified @GrantId@ is not valid.
_InvalidGrantIdException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidGrantIdException =
  Core._MatchServiceError mkServiceConfig "InvalidGrantIdException"
{-# DEPRECATED _InvalidGrantIdException "Use generic-lens or generic-optics instead." #-}

-- | The request was rejected because the specified grant token is not valid.
_InvalidGrantTokenException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidGrantTokenException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidGrantTokenException"
{-# DEPRECATED _InvalidGrantTokenException "Use generic-lens or generic-optics instead." #-}

-- | The request was rejected because a specified ARN, or an ARN in a key policy, is not valid.
_InvalidArnException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidArnException =
  Core._MatchServiceError mkServiceConfig "InvalidArnException"
{-# DEPRECATED _InvalidArnException "Use generic-lens or generic-optics instead." #-}

-- | The system timed out while trying to fulfill the request. The request can be retried.
_DependencyTimeoutException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DependencyTimeoutException =
  Core._MatchServiceError
    mkServiceConfig
    "DependencyTimeoutException"
{-# DEPRECATED _DependencyTimeoutException "Use generic-lens or generic-optics instead." #-}

-- | The request was rejected because the specified import token is expired. Use 'GetParametersForImport' to get a new import token and public key, use the new public key to encrypt the key material, and then try the request again.
_ExpiredImportTokenException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ExpiredImportTokenException =
  Core._MatchServiceError
    mkServiceConfig
    "ExpiredImportTokenException"
{-# DEPRECATED _ExpiredImportTokenException "Use generic-lens or generic-optics instead." #-}

-- | From the 'Decrypt' or 'ReEncrypt' operation, the request was rejected because the specified ciphertext, or additional authenticated data incorporated into the ciphertext, such as the encryption context, is corrupted, missing, or otherwise invalid.
--
-- From the 'ImportKeyMaterial' operation, the request was rejected because AWS KMS could not decrypt the encrypted (wrapped) key material.
_InvalidCiphertextException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidCiphertextException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidCiphertextException"
{-# DEPRECATED _InvalidCiphertextException "Use generic-lens or generic-optics instead." #-}

-- | The request was rejected because the specified AWS CloudHSM cluster is already associated with a custom key store or it shares a backup history with a cluster that is associated with a custom key store. Each custom key store must be associated with a different AWS CloudHSM cluster.
--
-- Clusters that share a backup history have the same cluster certificate. To view the cluster certificate of a cluster, use the <https://docs.aws.amazon.com/cloudhsm/latest/APIReference/API_DescribeClusters.html DescribeClusters> operation.
_CloudHsmClusterInUseException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CloudHsmClusterInUseException =
  Core._MatchServiceError
    mkServiceConfig
    "CloudHsmClusterInUseException"
{-# DEPRECATED _CloudHsmClusterInUseException "Use generic-lens or generic-optics instead." #-}

-- | The request was rejected because the specified CMK cannot decrypt the data. The @KeyId@ in a 'Decrypt' request and the @SourceKeyId@ in a 'ReEncrypt' request must identify the same CMK that was used to encrypt the ciphertext.
_IncorrectKeyException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_IncorrectKeyException =
  Core._MatchServiceError mkServiceConfig "IncorrectKeyException"
{-# DEPRECATED _IncorrectKeyException "Use generic-lens or generic-optics instead." #-}

-- | The request was rejected because it attempted to create a resource that already exists.
_AlreadyExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AlreadyExistsException =
  Core._MatchServiceError mkServiceConfig "AlreadyExistsException"
{-# DEPRECATED _AlreadyExistsException "Use generic-lens or generic-optics instead." #-}

-- | The request was rejected because a quota was exceeded. For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/limits.html Quotas> in the /AWS Key Management Service Developer Guide/ .
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError mkServiceConfig "LimitExceededException"
{-# DEPRECATED _LimitExceededException "Use generic-lens or generic-optics instead." #-}
