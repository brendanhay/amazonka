{-# OPTIONS_GHC -fno-warn-unused-imports    #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS Key Management Service__ 
--
-- AWS Key Management Service (AWS KMS) is an encryption and key management web service. This guide describes the AWS KMS operations that you can call programmatically. For general information about AWS KMS, see the <https://docs.aws.amazon.com/kms/latest/developerguide/ /AWS Key Management Service Developer Guide/ > .
-- We recommend that you use the AWS SDKs to make programmatic API calls to AWS KMS.
-- Clients must support TLS (Transport Layer Security) 1.0. We recommend TLS 1.2. Clients must also support cipher suites with Perfect Forward Secrecy (PFS) such as Ephemeral Diffie-Hellman (DHE) or Elliptic Curve Ephemeral Diffie-Hellman (ECDHE). Most modern systems such as Java 7 and later support these modes.
-- __Signing Requests__ 
-- Requests must be signed by using an access key ID and a secret access key. We strongly recommend that you /do not/ use your AWS account (root) access key ID and secret key for everyday work with AWS KMS. Instead, use the access key ID and secret access key for an IAM user. You can also use the AWS Security Token Service to generate temporary security credentials that you can use to sign requests.
-- All AWS KMS operations require <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4> .
-- __Logging API Requests__ 
-- AWS KMS supports AWS CloudTrail, a service that logs AWS API calls and related events for your AWS account and delivers them to an Amazon S3 bucket that you specify. By using the information collected by CloudTrail, you can determine what requests were made to AWS KMS, who made the request, when it was made, and so on. To learn more about CloudTrail, including how to turn it on and find your log files, see the <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/ AWS CloudTrail User Guide> .
-- __Additional Resources__ 
-- For more information about credentials and request signing, see the following:
--
--     * <https://docs.aws.amazon.com/general/latest/gr/aws-security-credentials.html AWS Security Credentials> - This topic provides general information about the types of credentials used for accessing AWS.
--
--
--     * <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp.html Temporary Security Credentials> - This section of the /IAM User Guide/ describes how to create and use temporary security credentials.
--
--
--     * <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process> - This set of topics walks you through the process of signing a request using an access key ID and a secret access key.
--
--
-- __Commonly Used API Operations__ 
-- Of the API operations discussed in this guide, the following will prove the most useful for most applications. You will likely perform operations other than these, such as creating keys and assigning policies, by using the console.
--
--     * 'Encrypt' 
--
--
--     * 'Decrypt' 
--
--
--     * 'GenerateDataKey' 
--
--
--     * 'GenerateDataKeyWithoutPlaintext' 
--
--
module Network.AWS.KMS
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    -- $errors

    -- ** InvalidMarkerException
    , _InvalidMarkerException

    -- ** KMSInvalidStateException
    , _KMSInvalidStateException

    -- ** InvalidKeyUsageException
    , _InvalidKeyUsageException

    -- ** MalformedPolicyDocumentException
    , _MalformedPolicyDocumentException

    -- ** CustomKeyStoreNameInUseException
    , _CustomKeyStoreNameInUseException

    -- ** UnsupportedOperationException
    , _UnsupportedOperationException

    -- ** DisabledException
    , _DisabledException

    -- ** KeyUnavailableException
    , _KeyUnavailableException

    -- ** IncorrectKeyMaterialException
    , _IncorrectKeyMaterialException

    -- ** KMSInternalException
    , _KMSInternalException

    -- ** TagException
    , _TagException

    -- ** CustomKeyStoreHasCMKsException
    , _CustomKeyStoreHasCMKsException

    -- ** InvalidImportTokenException
    , _InvalidImportTokenException

    -- ** CloudHsmClusterNotRelatedException
    , _CloudHsmClusterNotRelatedException

    -- ** IncorrectTrustAnchorException
    , _IncorrectTrustAnchorException

    -- ** CloudHsmClusterInvalidConfigurationException
    , _CloudHsmClusterInvalidConfigurationException

    -- ** CloudHsmClusterNotActiveException
    , _CloudHsmClusterNotActiveException

    -- ** CloudHsmClusterNotFoundException
    , _CloudHsmClusterNotFoundException

    -- ** NotFoundException
    , _NotFoundException

    -- ** KMSInvalidSignatureException
    , _KMSInvalidSignatureException

    -- ** InvalidAliasNameException
    , _InvalidAliasNameException

    -- ** CustomKeyStoreNotFoundException
    , _CustomKeyStoreNotFoundException

    -- ** CustomKeyStoreInvalidStateException
    , _CustomKeyStoreInvalidStateException

    -- ** InvalidGrantIdException
    , _InvalidGrantIdException

    -- ** InvalidGrantTokenException
    , _InvalidGrantTokenException

    -- ** InvalidArnException
    , _InvalidArnException

    -- ** DependencyTimeoutException
    , _DependencyTimeoutException

    -- ** ExpiredImportTokenException
    , _ExpiredImportTokenException

    -- ** InvalidCiphertextException
    , _InvalidCiphertextException

    -- ** CloudHsmClusterInUseException
    , _CloudHsmClusterInUseException

    -- ** IncorrectKeyException
    , _IncorrectKeyException

    -- ** AlreadyExistsException
    , _AlreadyExistsException

    -- ** LimitExceededException
    , _LimitExceededException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** Encrypt 
    , module Network.AWS.KMS.Encrypt

    -- ** CreateCustomKeyStore 
    , module Network.AWS.KMS.CreateCustomKeyStore

    -- ** ListGrants (Paginated)
    , module Network.AWS.KMS.ListGrants

    -- ** DisableKeyRotation 
    , module Network.AWS.KMS.DisableKeyRotation

    -- ** Verify 
    , module Network.AWS.KMS.Verify

    -- ** GenerateDataKeyWithoutPlaintext 
    , module Network.AWS.KMS.GenerateDataKeyWithoutPlaintext

    -- ** UpdateCustomKeyStore 
    , module Network.AWS.KMS.UpdateCustomKeyStore

    -- ** GetParametersForImport 
    , module Network.AWS.KMS.GetParametersForImport

    -- ** EnableKeyRotation 
    , module Network.AWS.KMS.EnableKeyRotation

    -- ** DeleteCustomKeyStore 
    , module Network.AWS.KMS.DeleteCustomKeyStore

    -- ** CreateAlias 
    , module Network.AWS.KMS.CreateAlias

    -- ** CreateGrant 
    , module Network.AWS.KMS.CreateGrant

    -- ** ListAliases (Paginated)
    , module Network.AWS.KMS.ListAliases

    -- ** ConnectCustomKeyStore 
    , module Network.AWS.KMS.ConnectCustomKeyStore

    -- ** ListRetirableGrants 
    , module Network.AWS.KMS.ListRetirableGrants

    -- ** GetPublicKey 
    , module Network.AWS.KMS.GetPublicKey

    -- ** GenerateRandom 
    , module Network.AWS.KMS.GenerateRandom

    -- ** CreateKey 
    , module Network.AWS.KMS.CreateKey

    -- ** DisableKey 
    , module Network.AWS.KMS.DisableKey

    -- ** DisconnectCustomKeyStore 
    , module Network.AWS.KMS.DisconnectCustomKeyStore

    -- ** RetireGrant 
    , module Network.AWS.KMS.RetireGrant

    -- ** ListKeys (Paginated)
    , module Network.AWS.KMS.ListKeys

    -- ** ListResourceTags 
    , module Network.AWS.KMS.ListResourceTags

    -- ** GetKeyRotationStatus 
    , module Network.AWS.KMS.GetKeyRotationStatus

    -- ** GenerateDataKey 
    , module Network.AWS.KMS.GenerateDataKey

    -- ** DeleteAlias 
    , module Network.AWS.KMS.DeleteAlias

    -- ** UpdateAlias 
    , module Network.AWS.KMS.UpdateAlias

    -- ** DescribeKey 
    , module Network.AWS.KMS.DescribeKey

    -- ** DescribeCustomKeyStores 
    , module Network.AWS.KMS.DescribeCustomKeyStores

    -- ** CancelKeyDeletion 
    , module Network.AWS.KMS.CancelKeyDeletion

    -- ** Decrypt 
    , module Network.AWS.KMS.Decrypt

    -- ** GenerateDataKeyPairWithoutPlaintext 
    , module Network.AWS.KMS.GenerateDataKeyPairWithoutPlaintext

    -- ** UpdateKeyDescription 
    , module Network.AWS.KMS.UpdateKeyDescription

    -- ** ReEncrypt 
    , module Network.AWS.KMS.ReEncrypt

    -- ** TagResource 
    , module Network.AWS.KMS.TagResource

    -- ** ListKeyPolicies (Paginated)
    , module Network.AWS.KMS.ListKeyPolicies

    -- ** UntagResource 
    , module Network.AWS.KMS.UntagResource

    -- ** Sign 
    , module Network.AWS.KMS.Sign

    -- ** ScheduleKeyDeletion 
    , module Network.AWS.KMS.ScheduleKeyDeletion

    -- ** GenerateDataKeyPair 
    , module Network.AWS.KMS.GenerateDataKeyPair

    -- ** PutKeyPolicy 
    , module Network.AWS.KMS.PutKeyPolicy

    -- ** EnableKey 
    , module Network.AWS.KMS.EnableKey

    -- ** RevokeGrant 
    , module Network.AWS.KMS.RevokeGrant

    -- ** GetKeyPolicy 
    , module Network.AWS.KMS.GetKeyPolicy

    -- ** ImportKeyMaterial 
    , module Network.AWS.KMS.ImportKeyMaterial

    -- ** DeleteImportedKeyMaterial 
    , module Network.AWS.KMS.DeleteImportedKeyMaterial

    -- * Types

    -- ** ListGrantsResponse
    , ListGrantsResponse (..)
    , mkListGrantsResponse
    , lgrGrants
    , lgrNextMarker
    , lgrTruncated

    -- ** KeyUsageType
    , KeyUsageType (..)

    -- ** Tag
    , Tag (..)
    , mkTag
    , tTagKey
    , tTagValue

    -- ** KeyMetadata
    , KeyMetadata (..)
    , mkKeyMetadata
    , kmKeyId
    , kmAWSAccountId
    , kmArn
    , kmCloudHsmClusterId
    , kmCreationDate
    , kmCustomKeyStoreId
    , kmCustomerMasterKeySpec
    , kmDeletionDate
    , kmDescription
    , kmEnabled
    , kmEncryptionAlgorithms
    , kmExpirationModel
    , kmKeyManager
    , kmKeyState
    , kmKeyUsage
    , kmOrigin
    , kmSigningAlgorithms
    , kmValidTo

    -- ** MarkerType
    , MarkerType (..)

    -- ** DataKeySpec
    , DataKeySpec (..)

    -- ** CustomerMasterKeySpec
    , CustomerMasterKeySpec (..)

    -- ** AlgorithmSpec
    , AlgorithmSpec (..)

    -- ** GrantTokenType
    , GrantTokenType (..)

    -- ** EncryptionAlgorithmSpec
    , EncryptionAlgorithmSpec (..)

    -- ** TagKeyType
    , TagKeyType (..)

    -- ** KeyState
    , KeyState (..)

    -- ** GrantConstraints
    , GrantConstraints (..)
    , mkGrantConstraints
    , gcEncryptionContextEquals
    , gcEncryptionContextSubset

    -- ** CloudHsmClusterIdType
    , CloudHsmClusterIdType (..)

    -- ** AliasListEntry
    , AliasListEntry (..)
    , mkAliasListEntry
    , aleAliasArn
    , aleAliasName
    , aleTargetKeyId

    -- ** GrantListEntry
    , GrantListEntry (..)
    , mkGrantListEntry
    , gleConstraints
    , gleCreationDate
    , gleGrantId
    , gleGranteePrincipal
    , gleIssuingAccount
    , gleKeyId
    , gleName
    , gleOperations
    , gleRetiringPrincipal

    -- ** PolicyType
    , PolicyType (..)

    -- ** EncryptionContextValue
    , EncryptionContextValue (..)

    -- ** MessageType
    , MessageType (..)

    -- ** DescriptionType
    , DescriptionType (..)

    -- ** CustomKeyStoreIdType
    , CustomKeyStoreIdType (..)

    -- ** WrappingKeySpec
    , WrappingKeySpec (..)

    -- ** ConnectionStateType
    , ConnectionStateType (..)

    -- ** CustomKeyStoreNameType
    , CustomKeyStoreNameType (..)

    -- ** ExpirationModelType
    , ExpirationModelType (..)

    -- ** KeyManagerType
    , KeyManagerType (..)

    -- ** OriginType
    , OriginType (..)

    -- ** EncryptionContextKey
    , EncryptionContextKey (..)

    -- ** PrincipalIdType
    , PrincipalIdType (..)

    -- ** KeyStorePasswordType
    , KeyStorePasswordType (..)

    -- ** PolicyNameType
    , PolicyNameType (..)

    -- ** KeyIdType
    , KeyIdType (..)

    -- ** ArnType
    , ArnType (..)

    -- ** TrustAnchorCertificateType
    , TrustAnchorCertificateType (..)

    -- ** DataKeyPairSpec
    , DataKeyPairSpec (..)

    -- ** GrantOperation
    , GrantOperation (..)

    -- ** ConnectionErrorCodeType
    , ConnectionErrorCodeType (..)

    -- ** CustomKeyStoresListEntry
    , CustomKeyStoresListEntry (..)
    , mkCustomKeyStoresListEntry
    , cksleCloudHsmClusterId
    , cksleConnectionErrorCode
    , cksleConnectionState
    , cksleCreationDate
    , cksleCustomKeyStoreId
    , cksleCustomKeyStoreName
    , cksleTrustAnchorCertificate

    -- ** SigningAlgorithmSpec
    , SigningAlgorithmSpec (..)

    -- ** KeyListEntry
    , KeyListEntry (..)
    , mkKeyListEntry
    , kleKeyArn
    , kleKeyId

    -- ** KeyId
    , KeyId (..)

    -- ** NextMarker
    , NextMarker (..)

    -- ** CustomKeyStoreId
    , CustomKeyStoreId (..)

    -- ** RetiringPrincipal
    , RetiringPrincipal (..)

    -- ** Marker
    , Marker (..)

    -- ** TagKey
    , TagKey (..)

    -- ** TagValue
    , TagValue (..)

    -- ** AWSAccountId
    , AWSAccountId (..)

    -- ** Arn
    , Arn (..)

    -- ** CloudHsmClusterId
    , CloudHsmClusterId (..)

    -- ** Description
    , Description (..)

    -- ** GrantId
    , GrantId (..)

    -- ** GrantToken
    , GrantToken (..)

    -- ** Policy
    , Policy (..)

    -- ** AliasArn
    , AliasArn (..)

    -- ** AliasName
    , AliasName (..)

    -- ** TargetKeyId
    , TargetKeyId (..)

    -- ** GranteePrincipal
    , GranteePrincipal (..)

    -- ** IssuingAccount
    , IssuingAccount (..)

    -- ** Name
    , Name (..)

    -- ** DestinationKeyId
    , DestinationKeyId (..)

    -- ** SourceKeyId
    , SourceKeyId (..)

    -- ** PolicyName
    , PolicyName (..)

    -- * Serialization types
    , Lude.Base64 (..)
    , Lude._Base64
    , Lude.Sensitive (..)
    , Lude._Sensitive
    , Lude.UTCTime
    , Lude.NominalDiffTime
    ) where

import Network.AWS.KMS.Types
import Network.AWS.KMS.Waiters
import Network.AWS.KMS.Encrypt
import Network.AWS.KMS.CreateCustomKeyStore
import Network.AWS.KMS.ListGrants
import Network.AWS.KMS.DisableKeyRotation
import Network.AWS.KMS.Verify
import Network.AWS.KMS.GenerateDataKeyWithoutPlaintext
import Network.AWS.KMS.UpdateCustomKeyStore
import Network.AWS.KMS.GetParametersForImport
import Network.AWS.KMS.EnableKeyRotation
import Network.AWS.KMS.DeleteCustomKeyStore
import Network.AWS.KMS.CreateAlias
import Network.AWS.KMS.CreateGrant
import Network.AWS.KMS.ListAliases
import Network.AWS.KMS.ConnectCustomKeyStore
import Network.AWS.KMS.ListRetirableGrants
import Network.AWS.KMS.GetPublicKey
import Network.AWS.KMS.GenerateRandom
import Network.AWS.KMS.CreateKey
import Network.AWS.KMS.DisableKey
import Network.AWS.KMS.DisconnectCustomKeyStore
import Network.AWS.KMS.RetireGrant
import Network.AWS.KMS.ListKeys
import Network.AWS.KMS.ListResourceTags
import Network.AWS.KMS.GetKeyRotationStatus
import Network.AWS.KMS.GenerateDataKey
import Network.AWS.KMS.DeleteAlias
import Network.AWS.KMS.UpdateAlias
import Network.AWS.KMS.DescribeKey
import Network.AWS.KMS.DescribeCustomKeyStores
import Network.AWS.KMS.CancelKeyDeletion
import Network.AWS.KMS.Decrypt
import Network.AWS.KMS.GenerateDataKeyPairWithoutPlaintext
import Network.AWS.KMS.UpdateKeyDescription
import Network.AWS.KMS.ReEncrypt
import Network.AWS.KMS.TagResource
import Network.AWS.KMS.ListKeyPolicies
import Network.AWS.KMS.UntagResource
import Network.AWS.KMS.Sign
import Network.AWS.KMS.ScheduleKeyDeletion
import Network.AWS.KMS.GenerateDataKeyPair
import Network.AWS.KMS.PutKeyPolicy
import Network.AWS.KMS.EnableKey
import Network.AWS.KMS.RevokeGrant
import Network.AWS.KMS.GetKeyPolicy
import Network.AWS.KMS.ImportKeyMaterial
import Network.AWS.KMS.DeleteImportedKeyMaterial
import qualified Network.AWS.Prelude as Lude

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'KMS'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
