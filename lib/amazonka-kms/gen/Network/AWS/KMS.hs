{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS Key Management Service__
--
-- AWS Key Management Service (AWS KMS) is an encryption and key management web service. This guide describes the AWS KMS operations that you can call programmatically. For general information about AWS KMS, see the <http://docs.aws.amazon.com/kms/latest/developerguide/ AWS Key Management Service Developer Guide> .
--
-- We recommend that you use the AWS SDKs to make programmatic API calls to AWS KMS.
--
-- Clients must support TLS (Transport Layer Security) 1.0. We recommend TLS 1.2. Clients must also support cipher suites with Perfect Forward Secrecy (PFS) such as Ephemeral Diffie-Hellman (DHE) or Elliptic Curve Ephemeral Diffie-Hellman (ECDHE). Most modern systems such as Java 7 and later support these modes.
--
-- __Signing Requests__
--
-- Requests must be signed by using an access key ID and a secret access key. We strongly recommend that you /do not/ use your AWS account (root) access key ID and secret key for everyday work with AWS KMS. Instead, use the access key ID and secret access key for an IAM user, or you can use the AWS Security Token Service to generate temporary security credentials that you can use to sign requests.
--
-- All AWS KMS operations require <http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4> .
--
-- __Logging API Requests__
--
-- AWS KMS supports AWS CloudTrail, a service that logs AWS API calls and related events for your AWS account and delivers them to an Amazon S3 bucket that you specify. By using the information collected by CloudTrail, you can determine what requests were made to AWS KMS, who made the request, when it was made, and so on. To learn more about CloudTrail, including how to turn it on and find your log files, see the <http://docs.aws.amazon.com/awscloudtrail/latest/userguide/ AWS CloudTrail User Guide> .
--
-- __Additional Resources__
--
-- For more information about credentials and request signing, see the following:
--
--     * <http://docs.aws.amazon.com/general/latest/gr/aws-security-credentials.html AWS Security Credentials> - This topic provides general information about the types of credentials used for accessing AWS.
--
--     * <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp.html Temporary Security Credentials> - This section of the /IAM User Guide/ describes how to create and use temporary security credentials.
--
--     * <http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process> - This set of topics walks you through the process of signing a request using an access key ID and a secret access key.
--
--
--
-- __Commonly Used APIs__
--
-- Of the APIs discussed in this guide, the following will prove the most useful for most applications. You will likely perform actions other than these, such as creating keys and assigning policies, by using the console.
--
--     * 'Encrypt'
--
--     * 'Decrypt'
--
--     * 'GenerateDataKey'
--
--     * 'GenerateDataKeyWithoutPlaintext'
--
--
--
module Network.AWS.KMS
    (
    -- * Service Configuration
      kms

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

    -- ** InvalidImportTokenException
    , _InvalidImportTokenException

    -- ** NotFoundException
    , _NotFoundException

    -- ** InvalidAliasNameException
    , _InvalidAliasNameException

    -- ** InvalidGrantIdException
    , _InvalidGrantIdException

    -- ** InvalidGrantTokenException
    , _InvalidGrantTokenException

    -- ** InvalidARNException
    , _InvalidARNException

    -- ** DependencyTimeoutException
    , _DependencyTimeoutException

    -- ** ExpiredImportTokenException
    , _ExpiredImportTokenException

    -- ** InvalidCiphertextException
    , _InvalidCiphertextException

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

    -- ** ListGrants (Paginated)
    , module Network.AWS.KMS.ListGrants

    -- ** DisableKeyRotation
    , module Network.AWS.KMS.DisableKeyRotation

    -- ** GenerateDataKeyWithoutPlaintext
    , module Network.AWS.KMS.GenerateDataKeyWithoutPlaintext

    -- ** GetParametersForImport
    , module Network.AWS.KMS.GetParametersForImport

    -- ** EnableKeyRotation
    , module Network.AWS.KMS.EnableKeyRotation

    -- ** CreateAlias
    , module Network.AWS.KMS.CreateAlias

    -- ** CreateGrant
    , module Network.AWS.KMS.CreateGrant

    -- ** ListAliases (Paginated)
    , module Network.AWS.KMS.ListAliases

    -- ** ListRetirableGrants
    , module Network.AWS.KMS.ListRetirableGrants

    -- ** GenerateRandom
    , module Network.AWS.KMS.GenerateRandom

    -- ** CreateKey
    , module Network.AWS.KMS.CreateKey

    -- ** DisableKey
    , module Network.AWS.KMS.DisableKey

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

    -- ** CancelKeyDeletion
    , module Network.AWS.KMS.CancelKeyDeletion

    -- ** Decrypt
    , module Network.AWS.KMS.Decrypt

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

    -- ** ScheduleKeyDeletion
    , module Network.AWS.KMS.ScheduleKeyDeletion

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

    -- ** AlgorithmSpec
    , AlgorithmSpec (..)

    -- ** DataKeySpec
    , DataKeySpec (..)

    -- ** ExpirationModelType
    , ExpirationModelType (..)

    -- ** GrantOperation
    , GrantOperation (..)

    -- ** KeyManagerType
    , KeyManagerType (..)

    -- ** KeyState
    , KeyState (..)

    -- ** KeyUsageType
    , KeyUsageType (..)

    -- ** OriginType
    , OriginType (..)

    -- ** WrappingKeySpec
    , WrappingKeySpec (..)

    -- ** AliasListEntry
    , AliasListEntry
    , aliasListEntry
    , aleTargetKeyId
    , aleAliasName
    , aleAliasARN

    -- ** GrantConstraints
    , GrantConstraints
    , grantConstraints
    , gcEncryptionContextEquals
    , gcEncryptionContextSubset

    -- ** GrantListEntry
    , GrantListEntry
    , grantListEntry
    , gleKeyId
    , gleRetiringPrincipal
    , gleIssuingAccount
    , gleGrantId
    , gleConstraints
    , gleGranteePrincipal
    , gleName
    , gleCreationDate
    , gleOperations

    -- ** KeyListEntry
    , KeyListEntry
    , keyListEntry
    , kleKeyId
    , kleKeyARN

    -- ** KeyMetadata
    , KeyMetadata
    , keyMetadata
    , kmOrigin
    , kmExpirationModel
    , kmKeyManager
    , kmEnabled
    , kmValidTo
    , kmARN
    , kmKeyState
    , kmAWSAccountId
    , kmKeyUsage
    , kmCreationDate
    , kmDeletionDate
    , kmDescription
    , kmKeyId

    -- ** ListGrantsResponse
    , ListGrantsResponse
    , listGrantsResponse
    , lgTruncated
    , lgGrants
    , lgNextMarker

    -- ** Tag
    , Tag
    , tag
    , tagTagKey
    , tagTagValue
    ) where

import Network.AWS.KMS.CancelKeyDeletion
import Network.AWS.KMS.CreateAlias
import Network.AWS.KMS.CreateGrant
import Network.AWS.KMS.CreateKey
import Network.AWS.KMS.Decrypt
import Network.AWS.KMS.DeleteAlias
import Network.AWS.KMS.DeleteImportedKeyMaterial
import Network.AWS.KMS.DescribeKey
import Network.AWS.KMS.DisableKey
import Network.AWS.KMS.DisableKeyRotation
import Network.AWS.KMS.EnableKey
import Network.AWS.KMS.EnableKeyRotation
import Network.AWS.KMS.Encrypt
import Network.AWS.KMS.GenerateDataKey
import Network.AWS.KMS.GenerateDataKeyWithoutPlaintext
import Network.AWS.KMS.GenerateRandom
import Network.AWS.KMS.GetKeyPolicy
import Network.AWS.KMS.GetKeyRotationStatus
import Network.AWS.KMS.GetParametersForImport
import Network.AWS.KMS.ImportKeyMaterial
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
import Network.AWS.KMS.TagResource
import Network.AWS.KMS.Types
import Network.AWS.KMS.UntagResource
import Network.AWS.KMS.UpdateAlias
import Network.AWS.KMS.UpdateKeyDescription
import Network.AWS.KMS.Waiters

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
