{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS Key Management Service
--
-- AWS Key Management Service (AWS KMS) is an encryption and key management
-- web service. This guide describes the AWS KMS operations that you can
-- call programmatically. For general information about AWS KMS, see the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/ AWS Key Management Service Developer Guide>
-- .
--
-- AWS provides SDKs that consist of libraries and sample code for various
-- programming languages and platforms (Java, Ruby, .Net, macOS, Android,
-- etc.). The SDKs provide a convenient way to create programmatic access
-- to AWS KMS and other AWS services. For example, the SDKs take care of
-- tasks such as signing requests (see below), managing errors, and
-- retrying requests automatically. For more information about the AWS
-- SDKs, including how to download and install them, see
-- <http://aws.amazon.com/tools/ Tools for Amazon Web Services>.
--
-- We recommend that you use the AWS SDKs to make programmatic API calls to
-- AWS KMS.
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
-- key. We strongly recommend that you /do not/ use your AWS account (root)
-- access key ID and secret key for everyday work with AWS KMS. Instead,
-- use the access key ID and secret access key for an IAM user. You can
-- also use the AWS Security Token Service to generate temporary security
-- credentials that you can use to sign requests.
--
-- All AWS KMS operations require
-- <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4>.
--
-- __Logging API Requests__
--
-- AWS KMS supports AWS CloudTrail, a service that logs AWS API calls and
-- related events for your AWS account and delivers them to an Amazon S3
-- bucket that you specify. By using the information collected by
-- CloudTrail, you can determine what requests were made to AWS KMS, who
-- made the request, when it was made, and so on. To learn more about
-- CloudTrail, including how to turn it on and find your log files, see the
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/ AWS CloudTrail User Guide>.
--
-- __Additional Resources__
--
-- For more information about credentials and request signing, see the
-- following:
--
-- -   <https://docs.aws.amazon.com/general/latest/gr/aws-security-credentials.html AWS Security Credentials>
--     - This topic provides general information about the types of
--     credentials used for accessing AWS.
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

    -- ** CustomKeyStoreNameInUseException
    _CustomKeyStoreNameInUseException,

    -- ** InvalidAliasNameException
    _InvalidAliasNameException,

    -- ** MalformedPolicyDocumentException
    _MalformedPolicyDocumentException,

    -- ** CustomKeyStoreNotFoundException
    _CustomKeyStoreNotFoundException,

    -- ** InvalidKeyUsageException
    _InvalidKeyUsageException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** KMSInvalidStateException
    _KMSInvalidStateException,

    -- ** InvalidMarkerException
    _InvalidMarkerException,

    -- ** CloudHsmClusterNotFoundException
    _CloudHsmClusterNotFoundException,

    -- ** IncorrectKeyException
    _IncorrectKeyException,

    -- ** InvalidCiphertextException
    _InvalidCiphertextException,

    -- ** InvalidArnException
    _InvalidArnException,

    -- ** CloudHsmClusterInvalidConfigurationException
    _CloudHsmClusterInvalidConfigurationException,

    -- ** CustomKeyStoreHasCMKsException
    _CustomKeyStoreHasCMKsException,

    -- ** CloudHsmClusterNotRelatedException
    _CloudHsmClusterNotRelatedException,

    -- ** CustomKeyStoreInvalidStateException
    _CustomKeyStoreInvalidStateException,

    -- ** DisabledException
    _DisabledException,

    -- ** UnsupportedOperationException
    _UnsupportedOperationException,

    -- ** KMSInvalidSignatureException
    _KMSInvalidSignatureException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** AlreadyExistsException
    _AlreadyExistsException,

    -- ** ExpiredImportTokenException
    _ExpiredImportTokenException,

    -- ** CloudHsmClusterInUseException
    _CloudHsmClusterInUseException,

    -- ** CloudHsmClusterNotActiveException
    _CloudHsmClusterNotActiveException,

    -- ** InvalidGrantTokenException
    _InvalidGrantTokenException,

    -- ** DependencyTimeoutException
    _DependencyTimeoutException,

    -- ** IncorrectTrustAnchorException
    _IncorrectTrustAnchorException,

    -- ** InvalidImportTokenException
    _InvalidImportTokenException,

    -- ** TagException
    _TagException,

    -- ** KMSInternalException
    _KMSInternalException,

    -- ** InvalidGrantIdException
    _InvalidGrantIdException,

    -- ** IncorrectKeyMaterialException
    _IncorrectKeyMaterialException,

    -- ** KeyUnavailableException
    _KeyUnavailableException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DisableKeyRotation
    DisableKeyRotation (DisableKeyRotation'),
    newDisableKeyRotation,
    DisableKeyRotationResponse (DisableKeyRotationResponse'),
    newDisableKeyRotationResponse,

    -- ** DeleteAlias
    DeleteAlias (DeleteAlias'),
    newDeleteAlias,
    DeleteAliasResponse (DeleteAliasResponse'),
    newDeleteAliasResponse,

    -- ** ListGrants (Paginated)
    ListGrants (ListGrants'),
    newListGrants,
    ListGrantsResponse (ListGrantsResponse'),
    newListGrantsResponse,

    -- ** Verify
    Verify (Verify'),
    newVerify,
    VerifyResponse (VerifyResponse'),
    newVerifyResponse,

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

    -- ** PutKeyPolicy
    PutKeyPolicy (PutKeyPolicy'),
    newPutKeyPolicy,
    PutKeyPolicyResponse (PutKeyPolicyResponse'),
    newPutKeyPolicyResponse,

    -- ** ListKeyPolicies (Paginated)
    ListKeyPolicies (ListKeyPolicies'),
    newListKeyPolicies,
    ListKeyPoliciesResponse (ListKeyPoliciesResponse'),
    newListKeyPoliciesResponse,

    -- ** DisableKey
    DisableKey (DisableKey'),
    newDisableKey,
    DisableKeyResponse (DisableKeyResponse'),
    newDisableKeyResponse,

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

    -- ** GenerateRandom
    GenerateRandom (GenerateRandom'),
    newGenerateRandom,
    GenerateRandomResponse (GenerateRandomResponse'),
    newGenerateRandomResponse,

    -- ** GetPublicKey
    GetPublicKey (GetPublicKey'),
    newGetPublicKey,
    GetPublicKeyResponse (GetPublicKeyResponse'),
    newGetPublicKeyResponse,

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

    -- ** GetParametersForImport
    GetParametersForImport (GetParametersForImport'),
    newGetParametersForImport,
    GetParametersForImportResponse (GetParametersForImportResponse'),
    newGetParametersForImportResponse,

    -- ** DescribeKey
    DescribeKey (DescribeKey'),
    newDescribeKey,
    DescribeKeyResponse (DescribeKeyResponse'),
    newDescribeKeyResponse,

    -- ** DeleteCustomKeyStore
    DeleteCustomKeyStore (DeleteCustomKeyStore'),
    newDeleteCustomKeyStore,
    DeleteCustomKeyStoreResponse (DeleteCustomKeyStoreResponse'),
    newDeleteCustomKeyStoreResponse,

    -- ** UpdateCustomKeyStore
    UpdateCustomKeyStore (UpdateCustomKeyStore'),
    newUpdateCustomKeyStore,
    UpdateCustomKeyStoreResponse (UpdateCustomKeyStoreResponse'),
    newUpdateCustomKeyStoreResponse,

    -- ** GenerateDataKeyWithoutPlaintext
    GenerateDataKeyWithoutPlaintext (GenerateDataKeyWithoutPlaintext'),
    newGenerateDataKeyWithoutPlaintext,
    GenerateDataKeyWithoutPlaintextResponse (GenerateDataKeyWithoutPlaintextResponse'),
    newGenerateDataKeyWithoutPlaintextResponse,

    -- ** Encrypt
    Encrypt (Encrypt'),
    newEncrypt,
    EncryptResponse (EncryptResponse'),
    newEncryptResponse,

    -- ** GetKeyPolicy
    GetKeyPolicy (GetKeyPolicy'),
    newGetKeyPolicy,
    GetKeyPolicyResponse (GetKeyPolicyResponse'),
    newGetKeyPolicyResponse,

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

    -- ** CancelKeyDeletion
    CancelKeyDeletion (CancelKeyDeletion'),
    newCancelKeyDeletion,
    CancelKeyDeletionResponse (CancelKeyDeletionResponse'),
    newCancelKeyDeletionResponse,

    -- ** GenerateDataKeyPairWithoutPlaintext
    GenerateDataKeyPairWithoutPlaintext (GenerateDataKeyPairWithoutPlaintext'),
    newGenerateDataKeyPairWithoutPlaintext,
    GenerateDataKeyPairWithoutPlaintextResponse (GenerateDataKeyPairWithoutPlaintextResponse'),
    newGenerateDataKeyPairWithoutPlaintextResponse,

    -- ** DescribeCustomKeyStores
    DescribeCustomKeyStores (DescribeCustomKeyStores'),
    newDescribeCustomKeyStores,
    DescribeCustomKeyStoresResponse (DescribeCustomKeyStoresResponse'),
    newDescribeCustomKeyStoresResponse,

    -- ** Decrypt
    Decrypt (Decrypt'),
    newDecrypt,
    DecryptResponse (DecryptResponse'),
    newDecryptResponse,

    -- ** CreateAlias
    CreateAlias (CreateAlias'),
    newCreateAlias,
    CreateAliasResponse (CreateAliasResponse'),
    newCreateAliasResponse,

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

    -- ** CreateGrant
    CreateGrant (CreateGrant'),
    newCreateGrant,
    CreateGrantResponse (CreateGrantResponse'),
    newCreateGrantResponse,

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

    -- ** KeyState
    KeyState (..),

    -- ** KeyUsageType
    KeyUsageType (..),

    -- ** MessageType
    MessageType (..),

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
