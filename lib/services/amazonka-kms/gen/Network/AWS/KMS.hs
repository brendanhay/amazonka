{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.KMS
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
module Amazonka.KMS
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InvalidMarkerException
    _InvalidMarkerException,

    -- ** KMSInvalidStateException
    _KMSInvalidStateException,

    -- ** InvalidKeyUsageException
    _InvalidKeyUsageException,

    -- ** MalformedPolicyDocumentException
    _MalformedPolicyDocumentException,

    -- ** CustomKeyStoreNameInUseException
    _CustomKeyStoreNameInUseException,

    -- ** UnsupportedOperationException
    _UnsupportedOperationException,

    -- ** DisabledException
    _DisabledException,

    -- ** KeyUnavailableException
    _KeyUnavailableException,

    -- ** IncorrectKeyMaterialException
    _IncorrectKeyMaterialException,

    -- ** KMSInternalException
    _KMSInternalException,

    -- ** TagException
    _TagException,

    -- ** CustomKeyStoreHasCMKsException
    _CustomKeyStoreHasCMKsException,

    -- ** InvalidImportTokenException
    _InvalidImportTokenException,

    -- ** CloudHsmClusterNotRelatedException
    _CloudHsmClusterNotRelatedException,

    -- ** IncorrectTrustAnchorException
    _IncorrectTrustAnchorException,

    -- ** CloudHsmClusterInvalidConfigurationException
    _CloudHsmClusterInvalidConfigurationException,

    -- ** CloudHsmClusterNotActiveException
    _CloudHsmClusterNotActiveException,

    -- ** CloudHsmClusterNotFoundException
    _CloudHsmClusterNotFoundException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** KMSInvalidSignatureException
    _KMSInvalidSignatureException,

    -- ** InvalidAliasNameException
    _InvalidAliasNameException,

    -- ** CustomKeyStoreNotFoundException
    _CustomKeyStoreNotFoundException,

    -- ** CustomKeyStoreInvalidStateException
    _CustomKeyStoreInvalidStateException,

    -- ** InvalidGrantIdException
    _InvalidGrantIdException,

    -- ** InvalidGrantTokenException
    _InvalidGrantTokenException,

    -- ** InvalidArnException
    _InvalidArnException,

    -- ** DependencyTimeoutException
    _DependencyTimeoutException,

    -- ** ExpiredImportTokenException
    _ExpiredImportTokenException,

    -- ** InvalidCiphertextException
    _InvalidCiphertextException,

    -- ** CloudHsmClusterInUseException
    _CloudHsmClusterInUseException,

    -- ** IncorrectKeyException
    _IncorrectKeyException,

    -- ** AlreadyExistsException
    _AlreadyExistsException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** Encrypt
    Encrypt (Encrypt'),
    newEncrypt,
    EncryptResponse (EncryptResponse'),
    newEncryptResponse,

    -- ** CreateCustomKeyStore
    CreateCustomKeyStore (CreateCustomKeyStore'),
    newCreateCustomKeyStore,
    CreateCustomKeyStoreResponse (CreateCustomKeyStoreResponse'),
    newCreateCustomKeyStoreResponse,

    -- ** ListGrants (Paginated)
    ListGrants (ListGrants'),
    newListGrants,
    ListGrantsResponse (ListGrantsResponse'),
    newListGrantsResponse,

    -- ** DisableKeyRotation
    DisableKeyRotation (DisableKeyRotation'),
    newDisableKeyRotation,
    DisableKeyRotationResponse (DisableKeyRotationResponse'),
    newDisableKeyRotationResponse,

    -- ** Verify
    Verify (Verify'),
    newVerify,
    VerifyResponse (VerifyResponse'),
    newVerifyResponse,

    -- ** GenerateDataKeyWithoutPlaintext
    GenerateDataKeyWithoutPlaintext (GenerateDataKeyWithoutPlaintext'),
    newGenerateDataKeyWithoutPlaintext,
    GenerateDataKeyWithoutPlaintextResponse (GenerateDataKeyWithoutPlaintextResponse'),
    newGenerateDataKeyWithoutPlaintextResponse,

    -- ** UpdateCustomKeyStore
    UpdateCustomKeyStore (UpdateCustomKeyStore'),
    newUpdateCustomKeyStore,
    UpdateCustomKeyStoreResponse (UpdateCustomKeyStoreResponse'),
    newUpdateCustomKeyStoreResponse,

    -- ** GetParametersForImport
    GetParametersForImport (GetParametersForImport'),
    newGetParametersForImport,
    GetParametersForImportResponse (GetParametersForImportResponse'),
    newGetParametersForImportResponse,

    -- ** EnableKeyRotation
    EnableKeyRotation (EnableKeyRotation'),
    newEnableKeyRotation,
    EnableKeyRotationResponse (EnableKeyRotationResponse'),
    newEnableKeyRotationResponse,

    -- ** DeleteCustomKeyStore
    DeleteCustomKeyStore (DeleteCustomKeyStore'),
    newDeleteCustomKeyStore,
    DeleteCustomKeyStoreResponse (DeleteCustomKeyStoreResponse'),
    newDeleteCustomKeyStoreResponse,

    -- ** CreateAlias
    CreateAlias (CreateAlias'),
    newCreateAlias,
    CreateAliasResponse (CreateAliasResponse'),
    newCreateAliasResponse,

    -- ** CreateGrant
    CreateGrant (CreateGrant'),
    newCreateGrant,
    CreateGrantResponse (CreateGrantResponse'),
    newCreateGrantResponse,

    -- ** ListAliases (Paginated)
    ListAliases (ListAliases'),
    newListAliases,
    ListAliasesResponse (ListAliasesResponse'),
    newListAliasesResponse,

    -- ** UpdatePrimaryRegion
    UpdatePrimaryRegion (UpdatePrimaryRegion'),
    newUpdatePrimaryRegion,
    UpdatePrimaryRegionResponse (UpdatePrimaryRegionResponse'),
    newUpdatePrimaryRegionResponse,

    -- ** ConnectCustomKeyStore
    ConnectCustomKeyStore (ConnectCustomKeyStore'),
    newConnectCustomKeyStore,
    ConnectCustomKeyStoreResponse (ConnectCustomKeyStoreResponse'),
    newConnectCustomKeyStoreResponse,

    -- ** ListRetirableGrants
    ListRetirableGrants (ListRetirableGrants'),
    newListRetirableGrants,
    ListGrantsResponse (ListGrantsResponse'),
    newListGrantsResponse,

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

    -- ** CreateKey
    CreateKey (CreateKey'),
    newCreateKey,
    CreateKeyResponse (CreateKeyResponse'),
    newCreateKeyResponse,

    -- ** DisableKey
    DisableKey (DisableKey'),
    newDisableKey,
    DisableKeyResponse (DisableKeyResponse'),
    newDisableKeyResponse,

    -- ** DisconnectCustomKeyStore
    DisconnectCustomKeyStore (DisconnectCustomKeyStore'),
    newDisconnectCustomKeyStore,
    DisconnectCustomKeyStoreResponse (DisconnectCustomKeyStoreResponse'),
    newDisconnectCustomKeyStoreResponse,

    -- ** RetireGrant
    RetireGrant (RetireGrant'),
    newRetireGrant,
    RetireGrantResponse (RetireGrantResponse'),
    newRetireGrantResponse,

    -- ** ListKeys (Paginated)
    ListKeys (ListKeys'),
    newListKeys,
    ListKeysResponse (ListKeysResponse'),
    newListKeysResponse,

    -- ** ListResourceTags
    ListResourceTags (ListResourceTags'),
    newListResourceTags,
    ListResourceTagsResponse (ListResourceTagsResponse'),
    newListResourceTagsResponse,

    -- ** GetKeyRotationStatus
    GetKeyRotationStatus (GetKeyRotationStatus'),
    newGetKeyRotationStatus,
    GetKeyRotationStatusResponse (GetKeyRotationStatusResponse'),
    newGetKeyRotationStatusResponse,

    -- ** GenerateDataKey
    GenerateDataKey (GenerateDataKey'),
    newGenerateDataKey,
    GenerateDataKeyResponse (GenerateDataKeyResponse'),
    newGenerateDataKeyResponse,

    -- ** DeleteAlias
    DeleteAlias (DeleteAlias'),
    newDeleteAlias,
    DeleteAliasResponse (DeleteAliasResponse'),
    newDeleteAliasResponse,

    -- ** UpdateAlias
    UpdateAlias (UpdateAlias'),
    newUpdateAlias,
    UpdateAliasResponse (UpdateAliasResponse'),
    newUpdateAliasResponse,

    -- ** DescribeKey
    DescribeKey (DescribeKey'),
    newDescribeKey,
    DescribeKeyResponse (DescribeKeyResponse'),
    newDescribeKeyResponse,

    -- ** DescribeCustomKeyStores
    DescribeCustomKeyStores (DescribeCustomKeyStores'),
    newDescribeCustomKeyStores,
    DescribeCustomKeyStoresResponse (DescribeCustomKeyStoresResponse'),
    newDescribeCustomKeyStoresResponse,

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

    -- ** GenerateDataKeyPairWithoutPlaintext
    GenerateDataKeyPairWithoutPlaintext (GenerateDataKeyPairWithoutPlaintext'),
    newGenerateDataKeyPairWithoutPlaintext,
    GenerateDataKeyPairWithoutPlaintextResponse (GenerateDataKeyPairWithoutPlaintextResponse'),
    newGenerateDataKeyPairWithoutPlaintextResponse,

    -- ** UpdateKeyDescription
    UpdateKeyDescription (UpdateKeyDescription'),
    newUpdateKeyDescription,
    UpdateKeyDescriptionResponse (UpdateKeyDescriptionResponse'),
    newUpdateKeyDescriptionResponse,

    -- ** ReEncrypt
    ReEncrypt (ReEncrypt'),
    newReEncrypt,
    ReEncryptResponse (ReEncryptResponse'),
    newReEncryptResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

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

    -- ** Sign
    Sign (Sign'),
    newSign,
    SignResponse (SignResponse'),
    newSignResponse,

    -- ** ScheduleKeyDeletion
    ScheduleKeyDeletion (ScheduleKeyDeletion'),
    newScheduleKeyDeletion,
    ScheduleKeyDeletionResponse (ScheduleKeyDeletionResponse'),
    newScheduleKeyDeletionResponse,

    -- ** GenerateDataKeyPair
    GenerateDataKeyPair (GenerateDataKeyPair'),
    newGenerateDataKeyPair,
    GenerateDataKeyPairResponse (GenerateDataKeyPairResponse'),
    newGenerateDataKeyPairResponse,

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

    -- ** EnableKey
    EnableKey (EnableKey'),
    newEnableKey,
    EnableKeyResponse (EnableKeyResponse'),
    newEnableKeyResponse,

    -- ** RevokeGrant
    RevokeGrant (RevokeGrant'),
    newRevokeGrant,
    RevokeGrantResponse (RevokeGrantResponse'),
    newRevokeGrantResponse,

    -- ** GetKeyPolicy
    GetKeyPolicy (GetKeyPolicy'),
    newGetKeyPolicy,
    GetKeyPolicyResponse (GetKeyPolicyResponse'),
    newGetKeyPolicyResponse,

    -- ** ImportKeyMaterial
    ImportKeyMaterial (ImportKeyMaterial'),
    newImportKeyMaterial,
    ImportKeyMaterialResponse (ImportKeyMaterialResponse'),
    newImportKeyMaterialResponse,

    -- ** DeleteImportedKeyMaterial
    DeleteImportedKeyMaterial (DeleteImportedKeyMaterial'),
    newDeleteImportedKeyMaterial,
    DeleteImportedKeyMaterialResponse (DeleteImportedKeyMaterialResponse'),
    newDeleteImportedKeyMaterialResponse,

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

import Amazonka.KMS.CancelKeyDeletion
import Amazonka.KMS.ConnectCustomKeyStore
import Amazonka.KMS.CreateAlias
import Amazonka.KMS.CreateCustomKeyStore
import Amazonka.KMS.CreateGrant
import Amazonka.KMS.CreateKey
import Amazonka.KMS.Decrypt
import Amazonka.KMS.DeleteAlias
import Amazonka.KMS.DeleteCustomKeyStore
import Amazonka.KMS.DeleteImportedKeyMaterial
import Amazonka.KMS.DescribeCustomKeyStores
import Amazonka.KMS.DescribeKey
import Amazonka.KMS.DisableKey
import Amazonka.KMS.DisableKeyRotation
import Amazonka.KMS.DisconnectCustomKeyStore
import Amazonka.KMS.EnableKey
import Amazonka.KMS.EnableKeyRotation
import Amazonka.KMS.Encrypt
import Amazonka.KMS.GenerateDataKey
import Amazonka.KMS.GenerateDataKeyPair
import Amazonka.KMS.GenerateDataKeyPairWithoutPlaintext
import Amazonka.KMS.GenerateDataKeyWithoutPlaintext
import Amazonka.KMS.GenerateRandom
import Amazonka.KMS.GetKeyPolicy
import Amazonka.KMS.GetKeyRotationStatus
import Amazonka.KMS.GetParametersForImport
import Amazonka.KMS.GetPublicKey
import Amazonka.KMS.ImportKeyMaterial
import Amazonka.KMS.Lens
import Amazonka.KMS.ListAliases
import Amazonka.KMS.ListGrants
import Amazonka.KMS.ListKeyPolicies
import Amazonka.KMS.ListKeys
import Amazonka.KMS.ListResourceTags
import Amazonka.KMS.ListRetirableGrants
import Amazonka.KMS.PutKeyPolicy
import Amazonka.KMS.ReEncrypt
import Amazonka.KMS.ReplicateKey
import Amazonka.KMS.RetireGrant
import Amazonka.KMS.RevokeGrant
import Amazonka.KMS.ScheduleKeyDeletion
import Amazonka.KMS.Sign
import Amazonka.KMS.TagResource
import Amazonka.KMS.Types
import Amazonka.KMS.UntagResource
import Amazonka.KMS.UpdateAlias
import Amazonka.KMS.UpdateCustomKeyStore
import Amazonka.KMS.UpdateKeyDescription
import Amazonka.KMS.UpdatePrimaryRegion
import Amazonka.KMS.Verify
import Amazonka.KMS.Waiters

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
