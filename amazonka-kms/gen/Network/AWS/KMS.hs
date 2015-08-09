{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS Key Management Service
--
-- AWS Key Management Service (KMS) is an encryption and key management web
-- service. This guide describes the KMS actions that you can call
-- programmatically. For general information about KMS, see the
-- <http://docs.aws.amazon.com/kms/latest/developerguide/overview.html AWS Key Management Service Developer Guide>
--
-- AWS provides SDKs that consist of libraries and sample code for various
-- programming languages and platforms (Java, Ruby, .Net, iOS, Android,
-- etc.). The SDKs provide a convenient way to create programmatic access
-- to KMS and AWS. For example, the SDKs take care of tasks such as signing
-- requests (see below), managing errors, and retrying requests
-- automatically. For more information about the AWS SDKs, including how to
-- download and install them, see
-- <http://aws.amazon.com/tools/ Tools for Amazon Web Services>.
--
-- We recommend that you use the AWS SDKs to make programmatic API calls to
-- KMS.
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
-- key. We strongly recommend that you do not use your AWS account access
-- key ID and secret key for everyday work with KMS. Instead, use the
-- access key ID and secret access key for an IAM user, or you can use the
-- AWS Security Token Service to generate temporary security credentials
-- that you can use to sign requests.
--
-- All KMS operations require
-- <http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4>.
--
-- __Recording API Requests__
--
-- KMS supports AWS CloudTrail, a service that records AWS API calls and
-- related events for your AWS account and delivers them to an Amazon S3
-- bucket that you specify. By using the information collected by
-- CloudTrail, you can determine what requests were made to KMS, who made
-- the request, when it was made, and so on. To learn more about
-- CloudTrail, including how to turn it on and find your log files, see the
-- <http://docs.aws.amazon.com/awscloudtrail/latest/userguide/whatiscloudtrail.html AWS CloudTrail User Guide>
--
-- __Additional Resources__
--
-- For more information about credentials and request signing, see the
-- following:
--
-- -   <http://docs.aws.amazon.com/general/latest/gr/aws-security-credentials.html AWS Security Credentials>.
--     This topic provides general information about the types of
--     credentials used for accessing AWS.
-- -   <http://docs.aws.amazon.com/STS/latest/UsingSTS/ AWS Security Token Service>.
--     This guide describes how to create and use temporary security
--     credentials.
-- -   <http://docs.aws.amazon.com/general/latest/gr/signing_aws_api_requests.html Signing AWS API Requests>.
--     This set of topics walks you through the process of signing a
--     request using an access key ID and a secret access key.
--
-- __Commonly Used APIs__
--
-- Of the APIs discussed in this guide, the following will prove the most
-- useful for most applications. You will likely perform actions other than
-- these, such as creating keys and assigning policies, by using the
-- console.
--
-- -   Encrypt
-- -   Decrypt
-- -   GenerateDataKey
-- -   GenerateDataKeyWithoutPlaintext
--
-- /See:/ <http://docs.aws.amazon.com/kms/latest/APIReference/Welcome.html AWS API Reference>
module Network.AWS.KMS
    (
    -- * Service Description
      KMS

    -- * Error Matchers
    -- $errors

    -- ** InvalidMarkerException
    , _InvalidMarkerException

    -- ** InvalidKeyUsageException
    , _InvalidKeyUsageException

    -- ** UnsupportedOperationException
    , _UnsupportedOperationException

    -- ** MalformedPolicyDocumentException
    , _MalformedPolicyDocumentException

    -- ** DisabledException
    , _DisabledException

    -- ** KeyUnavailableException
    , _KeyUnavailableException

    -- ** KMSInternalException
    , _KMSInternalException

    -- ** NotFoundException
    , _NotFoundException

    -- ** InvalidAliasNameException
    , _InvalidAliasNameException

    -- ** InvalidARNException
    , _InvalidARNException

    -- ** DependencyTimeoutException
    , _DependencyTimeoutException

    -- ** InvalidGrantTokenException
    , _InvalidGrantTokenException

    -- ** InvalidCiphertextException
    , _InvalidCiphertextException

    -- ** LimitExceededException
    , _LimitExceededException

    -- ** AlreadyExistsException
    , _AlreadyExistsException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DisableKeyRotation 
    , module Network.AWS.KMS.DisableKeyRotation

    -- ** GenerateDataKeyWithoutPlaintext 
    , module Network.AWS.KMS.GenerateDataKeyWithoutPlaintext

    -- ** ListGrants 
    , module Network.AWS.KMS.ListGrants

    -- ** Encrypt 
    , module Network.AWS.KMS.Encrypt

    -- ** EnableKeyRotation 
    , module Network.AWS.KMS.EnableKeyRotation

    -- ** CreateGrant 
    , module Network.AWS.KMS.CreateGrant

    -- ** CreateAlias 
    , module Network.AWS.KMS.CreateAlias

    -- ** ListAliases 
    , module Network.AWS.KMS.ListAliases

    -- ** GenerateRandom 
    , module Network.AWS.KMS.GenerateRandom

    -- ** DisableKey 
    , module Network.AWS.KMS.DisableKey

    -- ** CreateKey 
    , module Network.AWS.KMS.CreateKey

    -- ** RetireGrant 
    , module Network.AWS.KMS.RetireGrant

    -- ** ListKeys 
    , module Network.AWS.KMS.ListKeys

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

    -- ** Decrypt 
    , module Network.AWS.KMS.Decrypt

    -- ** UpdateKeyDescription 
    , module Network.AWS.KMS.UpdateKeyDescription

    -- ** ReEncrypt 
    , module Network.AWS.KMS.ReEncrypt

    -- ** ListKeyPolicies 
    , module Network.AWS.KMS.ListKeyPolicies

    -- ** EnableKey 
    , module Network.AWS.KMS.EnableKey

    -- ** PutKeyPolicy 
    , module Network.AWS.KMS.PutKeyPolicy

    -- ** RevokeGrant 
    , module Network.AWS.KMS.RevokeGrant

    -- ** GetKeyPolicy 
    , module Network.AWS.KMS.GetKeyPolicy

    -- * Types

    -- ** DataKeySpec
    , DataKeySpec (..)

    -- ** GrantOperation
    , GrantOperation (..)

    -- ** KeyUsageType
    , KeyUsageType (..)

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
    , gleRetiringPrincipal
    , gleIssuingAccount
    , gleGrantId
    , gleConstraints
    , gleGranteePrincipal
    , gleOperations

    -- ** KeyListEntry
    , KeyListEntry
    , keyListEntry
    , kleKeyARN
    , kleKeyId

    -- ** KeyMetadata
    , KeyMetadata
    , keyMetadata
    , kmARN
    , kmEnabled
    , kmAWSAccountId
    , kmKeyUsage
    , kmCreationDate
    , kmDescription
    , kmKeyId
    ) where

import Network.AWS.KMS.CreateAlias
import Network.AWS.KMS.CreateGrant
import Network.AWS.KMS.CreateKey
import Network.AWS.KMS.Decrypt
import Network.AWS.KMS.DeleteAlias
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
import Network.AWS.KMS.ListAliases
import Network.AWS.KMS.ListGrants
import Network.AWS.KMS.ListKeyPolicies
import Network.AWS.KMS.ListKeys
import Network.AWS.KMS.PutKeyPolicy
import Network.AWS.KMS.ReEncrypt
import Network.AWS.KMS.RetireGrant
import Network.AWS.KMS.RevokeGrant
import Network.AWS.KMS.Types
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

{- $pager
This operation can return paginated results.
-}
