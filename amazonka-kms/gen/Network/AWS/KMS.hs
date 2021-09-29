{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.KMS
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2014-11-01@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Key Management Service
--
-- Key Management Service (KMS) is an encryption and key management web
-- service. This guide describes the KMS operations that you can call
-- programmatically. For general information about KMS, see the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/ Key Management Service Developer Guide>
-- .
--
-- KMS is replacing the term /customer master key (CMK)/ with /KMS key/ and
-- /KMS key/. The concept has not changed. To prevent breaking changes, KMS
-- is keeping some variations of this term.
--
-- Amazon Web Services provides SDKs that consist of libraries and sample
-- code for various programming languages and platforms (Java, Ruby, .Net,
-- macOS, Android, etc.). The SDKs provide a convenient way to create
-- programmatic access to KMS and other Amazon Web Services services. For
-- example, the SDKs take care of tasks such as signing requests (see
-- below), managing errors, and retrying requests automatically. For more
-- information about the Amazon Web Services SDKs, including how to
-- download and install them, see
-- <http://aws.amazon.com/tools/ Tools for Amazon Web Services>.
--
-- We recommend that you use the Amazon Web Services SDKs to make
-- programmatic API calls to KMS.
--
-- Clients must support TLS (Transport Layer Security) 1.0. We recommend
-- TLS 1.2. Clients must also support cipher suites with Perfect Forward
-- Secrecy (PFS) such as Ephemeral Diffie-Hellman (DHE) or Elliptic Curve
-- Ephemeral Diffie-Hellman (ECDHE). Most modern systems such as Java 7 and
-- later support these modes.
--
-- __Signing Requests__
--
-- Requests must be signed by using an access key ID and a secret access
-- key. We strongly recommend that you /do not/ use your Amazon Web
-- Services account (root) access key ID and secret key for everyday work
-- with KMS. Instead, use the access key ID and secret access key for an
-- IAM user. You can also use the Amazon Web Services Security Token
-- Service to generate temporary security credentials that you can use to
-- sign requests.
--
-- All KMS operations require
-- <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4>.
--
-- __Logging API Requests__
--
-- KMS supports CloudTrail, a service that logs Amazon Web Services API
-- calls and related events for your Amazon Web Services account and
-- delivers them to an Amazon S3 bucket that you specify. By using the
-- information collected by CloudTrail, you can determine what requests
-- were made to KMS, who made the request, when it was made, and so on. To
-- learn more about CloudTrail, including how to turn it on and find your
-- log files, see the
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/ CloudTrail User Guide>.
--
-- __Additional Resources__
--
-- For more information about credentials and request signing, see the
-- following:
--
-- -   <https://docs.aws.amazon.com/general/latest/gr/aws-security-credentials.html Amazon Web Services Security Credentials>
--     - This topic provides general information about the types of
--     credentials used to access Amazon Web Services.
--
-- -   <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp.html Temporary Security Credentials>
--     - This section of the /IAM User Guide/ describes how to create and
--     use temporary security credentials.
--
-- -   <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process>
--     - This set of topics walks you through the process of signing a
--     request using an access key ID and a secret access key.
--
-- __Commonly Used API Operations__
--
-- Of the API operations discussed in this guide, the following will prove
-- the most useful for most applications. You will likely perform
-- operations other than these, such as creating keys and assigning
-- policies, by using the console.
--
-- -   Encrypt
--
-- -   Decrypt
--
-- -   GenerateDataKey
--
-- -   GenerateDataKeyWithoutPlaintext
module Network.AWS.KMS
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InvalidAliasNameException
    _InvalidAliasNameException,

    -- ** CustomKeyStoreNotFoundException
    _CustomKeyStoreNotFoundException,

    -- ** CustomKeyStoreNameInUseException
    _CustomKeyStoreNameInUseException,

    -- ** MalformedPolicyDocumentException
    _MalformedPolicyDocumentException,

    -- ** InvalidKeyUsageException
    _InvalidKeyUsageException,

    -- ** KMSInvalidStateException
    _KMSInvalidStateException,

    -- ** InvalidMarkerException
    _InvalidMarkerException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** CloudHsmClusterNotFoundException
    _CloudHsmClusterNotFoundException,

    -- ** IncorrectKeyException
    _IncorrectKeyException,

    -- ** InvalidCiphertextException
    _InvalidCiphertextException,

    -- ** CloudHsmClusterInvalidConfigurationException
    _CloudHsmClusterInvalidConfigurationException,

    -- ** InvalidArnException
    _InvalidArnException,

    -- ** CustomKeyStoreHasCMKsException
    _CustomKeyStoreHasCMKsException,

    -- ** CloudHsmClusterNotRelatedException
    _CloudHsmClusterNotRelatedException,

    -- ** CustomKeyStoreInvalidStateException
    _CustomKeyStoreInvalidStateException,

    -- ** UnsupportedOperationException
    _UnsupportedOperationException,

    -- ** KMSInvalidSignatureException
    _KMSInvalidSignatureException,

    -- ** DisabledException
    _DisabledException,

    -- ** AlreadyExistsException
    _AlreadyExistsException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ExpiredImportTokenException
    _ExpiredImportTokenException,

    -- ** CloudHsmClusterInUseException
    _CloudHsmClusterInUseException,

    -- ** DependencyTimeoutException
    _DependencyTimeoutException,

    -- ** IncorrectTrustAnchorException
    _IncorrectTrustAnchorException,

    -- ** CloudHsmClusterNotActiveException
    _CloudHsmClusterNotActiveException,

    -- ** InvalidGrantTokenException
    _InvalidGrantTokenException,

    -- ** InvalidImportTokenException
    _InvalidImportTokenException,

    -- ** KMSInternalException
    _KMSInternalException,

    -- ** InvalidGrantIdException
    _InvalidGrantIdException,

    -- ** TagException
    _TagException,

    -- ** IncorrectKeyMaterialException
    _IncorrectKeyMaterialException,

    -- ** KeyUnavailableException
    _KeyUnavailableException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** Verify
    Verify (Verify'),
    newVerify,
    VerifyResponse (VerifyResponse'),
    newVerifyResponse,

    -- ** DisableKeyRotation
    DisableKeyRotation (DisableKeyRotation'),
    newDisableKeyRotation,
    DisableKeyRotationResponse (DisableKeyRotationResponse'),
    newDisableKeyRotationResponse,

    -- ** ListGrants (Paginated)
    ListGrants (ListGrants'),
    newListGrants,
    ListGrantsResponse (ListGrantsResponse'),
    newListGrantsResponse,

    -- ** DeleteAlias
    DeleteAlias (DeleteAlias'),
    newDeleteAlias,
    DeleteAliasResponse (DeleteAliasResponse'),
    newDeleteAliasResponse,

    -- ** CreateCustomKeyStore
    CreateCustomKeyStore (CreateCustomKeyStore'),
    newCreateCustomKeyStore,
    CreateCustomKeyStoreResponse (CreateCustomKeyStoreResponse'),
    newCreateCustomKeyStoreResponse,

    -- ** UpdateAlias
    UpdateAlias (UpdateAlias'),
    newUpdateAlias,
    UpdateAliasResponse (UpdateAliasResponse'),
    newUpdateAliasResponse,

    -- ** GenerateDataKey
    GenerateDataKey (GenerateDataKey'),
    newGenerateDataKey,
    GenerateDataKeyResponse (GenerateDataKeyResponse'),
    newGenerateDataKeyResponse,

    -- ** DeleteImportedKeyMaterial
    DeleteImportedKeyMaterial (DeleteImportedKeyMaterial'),
    newDeleteImportedKeyMaterial,
    DeleteImportedKeyMaterialResponse (DeleteImportedKeyMaterialResponse'),
    newDeleteImportedKeyMaterialResponse,

    -- ** ImportKeyMaterial
    ImportKeyMaterial (ImportKeyMaterial'),
    newImportKeyMaterial,
    ImportKeyMaterialResponse (ImportKeyMaterialResponse'),
    newImportKeyMaterialResponse,

    -- ** GetKeyRotationStatus
    GetKeyRotationStatus (GetKeyRotationStatus'),
    newGetKeyRotationStatus,
    GetKeyRotationStatusResponse (GetKeyRotationStatusResponse'),
    newGetKeyRotationStatusResponse,

    -- ** ListResourceTags
    ListResourceTags (ListResourceTags'),
    newListResourceTags,
    ListResourceTagsResponse (ListResourceTagsResponse'),
    newListResourceTagsResponse,

    -- ** ReplicateKey
    ReplicateKey (ReplicateKey'),
    newReplicateKey,
    ReplicateKeyResponse (ReplicateKeyResponse'),
    newReplicateKeyResponse,

    -- ** PutKeyPolicy
    PutKeyPolicy (PutKeyPolicy'),
    newPutKeyPolicy,
    PutKeyPolicyResponse (PutKeyPolicyResponse'),
    newPutKeyPolicyResponse,

    -- ** DisableKey
    DisableKey (DisableKey'),
    newDisableKey,
    DisableKeyResponse (DisableKeyResponse'),
    newDisableKeyResponse,

    -- ** ListKeyPolicies (Paginated)
    ListKeyPolicies (ListKeyPolicies'),
    newListKeyPolicies,
    ListKeyPoliciesResponse (ListKeyPoliciesResponse'),
    newListKeyPoliciesResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** DisconnectCustomKeyStore
    DisconnectCustomKeyStore (DisconnectCustomKeyStore'),
    newDisconnectCustomKeyStore,
    DisconnectCustomKeyStoreResponse (DisconnectCustomKeyStoreResponse'),
    newDisconnectCustomKeyStoreResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** GetPublicKey
    GetPublicKey (GetPublicKey'),
    newGetPublicKey,
    GetPublicKeyResponse (GetPublicKeyResponse'),
    newGetPublicKeyResponse,

    -- ** GenerateRandom
    GenerateRandom (GenerateRandom'),
    newGenerateRandom,
    GenerateRandomResponse (GenerateRandomResponse'),
    newGenerateRandomResponse,

    -- ** ReEncrypt
    ReEncrypt (ReEncrypt'),
    newReEncrypt,
    ReEncryptResponse (ReEncryptResponse'),
    newReEncryptResponse,

    -- ** ListRetirableGrants
    ListRetirableGrants (ListRetirableGrants'),
    newListRetirableGrants,
    ListGrantsResponse (ListGrantsResponse'),
    newListGrantsResponse,

    -- ** ConnectCustomKeyStore
    ConnectCustomKeyStore (ConnectCustomKeyStore'),
    newConnectCustomKeyStore,
    ConnectCustomKeyStoreResponse (ConnectCustomKeyStoreResponse'),
    newConnectCustomKeyStoreResponse,

    -- ** UpdatePrimaryRegion
    UpdatePrimaryRegion (UpdatePrimaryRegion'),
    newUpdatePrimaryRegion,
    UpdatePrimaryRegionResponse (UpdatePrimaryRegionResponse'),
    newUpdatePrimaryRegionResponse,

    -- ** DescribeKey
    DescribeKey (DescribeKey'),
    newDescribeKey,
    DescribeKeyResponse (DescribeKeyResponse'),
    newDescribeKeyResponse,

    -- ** GetParametersForImport
    GetParametersForImport (GetParametersForImport'),
    newGetParametersForImport,
    GetParametersForImportResponse (GetParametersForImportResponse'),
    newGetParametersForImportResponse,

    -- ** UpdateCustomKeyStore
    UpdateCustomKeyStore (UpdateCustomKeyStore'),
    newUpdateCustomKeyStore,
    UpdateCustomKeyStoreResponse (UpdateCustomKeyStoreResponse'),
    newUpdateCustomKeyStoreResponse,

    -- ** DeleteCustomKeyStore
    DeleteCustomKeyStore (DeleteCustomKeyStore'),
    newDeleteCustomKeyStore,
    DeleteCustomKeyStoreResponse (DeleteCustomKeyStoreResponse'),
    newDeleteCustomKeyStoreResponse,

    -- ** Encrypt
    Encrypt (Encrypt'),
    newEncrypt,
    EncryptResponse (EncryptResponse'),
    newEncryptResponse,

    -- ** GenerateDataKeyWithoutPlaintext
    GenerateDataKeyWithoutPlaintext (GenerateDataKeyWithoutPlaintext'),
    newGenerateDataKeyWithoutPlaintext,
    GenerateDataKeyWithoutPlaintextResponse (GenerateDataKeyWithoutPlaintextResponse'),
    newGenerateDataKeyWithoutPlaintextResponse,

    -- ** GetKeyPolicy
    GetKeyPolicy (GetKeyPolicy'),
    newGetKeyPolicy,
    GetKeyPolicyResponse (GetKeyPolicyResponse'),
    newGetKeyPolicyResponse,

    -- ** EnableKey
    EnableKey (EnableKey'),
    newEnableKey,
    EnableKeyResponse (EnableKeyResponse'),
    newEnableKeyResponse,

    -- ** GenerateDataKeyPair
    GenerateDataKeyPair (GenerateDataKeyPair'),
    newGenerateDataKeyPair,
    GenerateDataKeyPairResponse (GenerateDataKeyPairResponse'),
    newGenerateDataKeyPairResponse,

    -- ** ListKeys (Paginated)
    ListKeys (ListKeys'),
    newListKeys,
    ListKeysResponse (ListKeysResponse'),
    newListKeysResponse,

    -- ** RevokeGrant
    RevokeGrant (RevokeGrant'),
    newRevokeGrant,
    RevokeGrantResponse (RevokeGrantResponse'),
    newRevokeGrantResponse,

    -- ** ScheduleKeyDeletion
    ScheduleKeyDeletion (ScheduleKeyDeletion'),
    newScheduleKeyDeletion,
    ScheduleKeyDeletionResponse (ScheduleKeyDeletionResponse'),
    newScheduleKeyDeletionResponse,

    -- ** RetireGrant
    RetireGrant (RetireGrant'),
    newRetireGrant,
    RetireGrantResponse (RetireGrantResponse'),
    newRetireGrantResponse,

    -- ** CreateKey
    CreateKey (CreateKey'),
    newCreateKey,
    CreateKeyResponse (CreateKeyResponse'),
    newCreateKeyResponse,

    -- ** Sign
    Sign (Sign'),
    newSign,
    SignResponse (SignResponse'),
    newSignResponse,

    -- ** UpdateKeyDescription
    UpdateKeyDescription (UpdateKeyDescription'),
    newUpdateKeyDescription,
    UpdateKeyDescriptionResponse (UpdateKeyDescriptionResponse'),
    newUpdateKeyDescriptionResponse,

    -- ** GenerateDataKeyPairWithoutPlaintext
    GenerateDataKeyPairWithoutPlaintext (GenerateDataKeyPairWithoutPlaintext'),
    newGenerateDataKeyPairWithoutPlaintext,
    GenerateDataKeyPairWithoutPlaintextResponse (GenerateDataKeyPairWithoutPlaintextResponse'),
    newGenerateDataKeyPairWithoutPlaintextResponse,

    -- ** CancelKeyDeletion
    CancelKeyDeletion (CancelKeyDeletion'),
    newCancelKeyDeletion,
    CancelKeyDeletionResponse (CancelKeyDeletionResponse'),
    newCancelKeyDeletionResponse,

    -- ** Decrypt
    Decrypt (Decrypt'),
    newDecrypt,
    DecryptResponse (DecryptResponse'),
    newDecryptResponse,

    -- ** DescribeCustomKeyStores
    DescribeCustomKeyStores (DescribeCustomKeyStores'),
    newDescribeCustomKeyStores,
    DescribeCustomKeyStoresResponse (DescribeCustomKeyStoresResponse'),
    newDescribeCustomKeyStoresResponse,

    -- ** CreateGrant
    CreateGrant (CreateGrant'),
    newCreateGrant,
    CreateGrantResponse (CreateGrantResponse'),
    newCreateGrantResponse,

    -- ** EnableKeyRotation
    EnableKeyRotation (EnableKeyRotation'),
    newEnableKeyRotation,
    EnableKeyRotationResponse (EnableKeyRotationResponse'),
    newEnableKeyRotationResponse,

    -- ** ListAliases (Paginated)
    ListAliases (ListAliases'),
    newListAliases,
    ListAliasesResponse (ListAliasesResponse'),
    newListAliasesResponse,

    -- ** CreateAlias
    CreateAlias (CreateAlias'),
    newCreateAlias,
    CreateAliasResponse (CreateAliasResponse'),
    newCreateAliasResponse,

    -- * Types

    -- ** AlgorithmSpec
    AlgorithmSpec (..),

    -- ** ConnectionErrorCodeType
    ConnectionErrorCodeType (..),

    -- ** ConnectionStateType
    ConnectionStateType (..),

    -- ** CustomerMasterKeySpec
    CustomerMasterKeySpec (..),

    -- ** DataKeyPairSpec
    DataKeyPairSpec (..),

    -- ** DataKeySpec
    DataKeySpec (..),

    -- ** EncryptionAlgorithmSpec
    EncryptionAlgorithmSpec (..),

    -- ** ExpirationModelType
    ExpirationModelType (..),

    -- ** GrantOperation
    GrantOperation (..),

    -- ** KeyManagerType
    KeyManagerType (..),

    -- ** KeySpec
    KeySpec (..),

    -- ** KeyState
    KeyState (..),

    -- ** KeyUsageType
    KeyUsageType (..),

    -- ** MessageType
    MessageType (..),

    -- ** MultiRegionKeyType
    MultiRegionKeyType (..),

    -- ** OriginType
    OriginType (..),

    -- ** SigningAlgorithmSpec
    SigningAlgorithmSpec (..),

    -- ** WrappingKeySpec
    WrappingKeySpec (..),

    -- ** AliasListEntry
    AliasListEntry (AliasListEntry'),
    newAliasListEntry,

    -- ** CustomKeyStoresListEntry
    CustomKeyStoresListEntry (CustomKeyStoresListEntry'),
    newCustomKeyStoresListEntry,

    -- ** GrantConstraints
    GrantConstraints (GrantConstraints'),
    newGrantConstraints,

    -- ** GrantListEntry
    GrantListEntry (GrantListEntry'),
    newGrantListEntry,

    -- ** KeyListEntry
    KeyListEntry (KeyListEntry'),
    newKeyListEntry,

    -- ** KeyMetadata
    KeyMetadata (KeyMetadata'),
    newKeyMetadata,

    -- ** ListGrantsResponse
    ListGrantsResponse (ListGrantsResponse'),
    newListGrantsResponse,

    -- ** MultiRegionConfiguration
    MultiRegionConfiguration (MultiRegionConfiguration'),
    newMultiRegionConfiguration,

    -- ** MultiRegionKey
    MultiRegionKey (MultiRegionKey'),
    newMultiRegionKey,

    -- ** Tag
    Tag (Tag'),
    newTag,
  )
where

import Network.AWS.KMS.CancelKeyDeletion
import Network.AWS.KMS.ConnectCustomKeyStore
import Network.AWS.KMS.CreateAlias
import Network.AWS.KMS.CreateCustomKeyStore
import Network.AWS.KMS.CreateGrant
import Network.AWS.KMS.CreateKey
import Network.AWS.KMS.Decrypt
import Network.AWS.KMS.DeleteAlias
import Network.AWS.KMS.DeleteCustomKeyStore
import Network.AWS.KMS.DeleteImportedKeyMaterial
import Network.AWS.KMS.DescribeCustomKeyStores
import Network.AWS.KMS.DescribeKey
import Network.AWS.KMS.DisableKey
import Network.AWS.KMS.DisableKeyRotation
import Network.AWS.KMS.DisconnectCustomKeyStore
import Network.AWS.KMS.EnableKey
import Network.AWS.KMS.EnableKeyRotation
import Network.AWS.KMS.Encrypt
import Network.AWS.KMS.GenerateDataKey
import Network.AWS.KMS.GenerateDataKeyPair
import Network.AWS.KMS.GenerateDataKeyPairWithoutPlaintext
import Network.AWS.KMS.GenerateDataKeyWithoutPlaintext
import Network.AWS.KMS.GenerateRandom
import Network.AWS.KMS.GetKeyPolicy
import Network.AWS.KMS.GetKeyRotationStatus
import Network.AWS.KMS.GetParametersForImport
import Network.AWS.KMS.GetPublicKey
import Network.AWS.KMS.ImportKeyMaterial
import Network.AWS.KMS.Lens
import Network.AWS.KMS.ListAliases
import Network.AWS.KMS.ListGrants
import Network.AWS.KMS.ListKeyPolicies
import Network.AWS.KMS.ListKeys
import Network.AWS.KMS.ListResourceTags
import Network.AWS.KMS.ListRetirableGrants
import Network.AWS.KMS.PutKeyPolicy
import Network.AWS.KMS.ReEncrypt
import Network.AWS.KMS.ReplicateKey
import Network.AWS.KMS.RetireGrant
import Network.AWS.KMS.RevokeGrant
import Network.AWS.KMS.ScheduleKeyDeletion
import Network.AWS.KMS.Sign
import Network.AWS.KMS.TagResource
import Network.AWS.KMS.Types
import Network.AWS.KMS.UntagResource
import Network.AWS.KMS.UpdateAlias
import Network.AWS.KMS.UpdateCustomKeyStore
import Network.AWS.KMS.UpdateKeyDescription
import Network.AWS.KMS.UpdatePrimaryRegion
import Network.AWS.KMS.Verify
import Network.AWS.KMS.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'KMS'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
