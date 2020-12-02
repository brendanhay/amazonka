{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecretsManager
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS Secrets Manager API Reference__
--
-- AWS Secrets Manager is a web service that enables you to store, manage, and retrieve, secrets.
--
-- This guide provides descriptions of the Secrets Manager API. For more information about using this service, see the <http://docs.aws.amazon.com/secretsmanager/latest/userguide/introduction.html AWS Secrets Manager User Guide> .
--
-- __API Version__
--
-- This version of the Secrets Manager API Reference documents the Secrets Manager API version 2017-10-17.
--
-- We recommend that you use the AWS SDKs to make programmatic API calls to Secrets Manager. However, you also can use the Secrets Manager HTTP Query API to make direct calls to the Secrets Manager web service. To learn more about the Secrets Manager HTTP Query API, see <http://docs.aws.amazon.com/secretsmanager/latest/userguide/query-requests.html Making Query Requests> in the /AWS Secrets Manager User Guide/ .
--
-- Secrets Manager supports GET and POST requests for all actions. That is, the API doesn't require you to use GET for some actions and POST for others. However, GET requests are subject to the limitation size of a URL. Therefore, for operations that require larger sizes, use a POST request.
--
-- __Support and Feedback for AWS Secrets Manager__
--
-- We welcome your feedback. Send your comments to <mailto:awssecretsmanager-feedback@amazon.com awssecretsmanager-feedback@amazon.com> , or post your feedback and questions in the <http://forums.aws.amazon.com/forum.jspa?forumID=296 AWS Secrets Manager Discussion Forum> . For more information about the AWS Discussion Forums, see <http://forums.aws.amazon.com/help.jspa Forums Help> .
--
-- __How examples are presented__
--
-- The JSON that AWS Secrets Manager expects as your request parameters and that the service returns as a response to HTTP query requests are single, long strings without line breaks or white space formatting. The JSON shown in the examples is formatted with both line breaks and white space to improve readability. When example input parameters would also result in long strings that extend beyond the screen, we insert line breaks to enhance readability. You should always submit the input as a single JSON text string.
--
-- __Logging API Requests__
--
-- AWS Secrets Manager supports AWS CloudTrail, a service that records AWS API calls for your AWS account and delivers log files to an Amazon S3 bucket. By using information that's collected by AWS CloudTrail, you can determine which requests were successfully made to Secrets Manager, who made the request, when it was made, and so on. For more about AWS Secrets Manager and its support for AWS CloudTrail, see <http://docs.aws.amazon.com/secretsmanager/latest/userguide/monitoring.html#monitoring_cloudtrail Logging AWS Secrets Manager Events with AWS CloudTrail> in the /AWS Secrets Manager User Guide/ . To learn more about CloudTrail, including how to turn it on and find your log files, see the <http://docs.aws.amazon.com/awscloudtrail/latest/userguide/what_is_cloud_trail_top_level.html AWS CloudTrail User Guide> .
--
module Network.AWS.SecretsManager
    (
    -- * Service Configuration
      secretsManager

    -- * Errors
    -- $errors

    -- ** MalformedPolicyDocumentException
    , _MalformedPolicyDocumentException

    -- ** InvalidParameterException
    , _InvalidParameterException

    -- ** InvalidRequestException
    , _InvalidRequestException

    -- ** DecryptionFailure
    , _DecryptionFailure

    -- ** EncryptionFailure
    , _EncryptionFailure

    -- ** InvalidNextTokenException
    , _InvalidNextTokenException

    -- ** InternalServiceError
    , _InternalServiceError

    -- ** ResourceExistsException
    , _ResourceExistsException

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- ** LimitExceededException
    , _LimitExceededException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DeleteSecret
    , module Network.AWS.SecretsManager.DeleteSecret

    -- ** ListSecrets
    , module Network.AWS.SecretsManager.ListSecrets

    -- ** UpdateSecret
    , module Network.AWS.SecretsManager.UpdateSecret

    -- ** RotateSecret
    , module Network.AWS.SecretsManager.RotateSecret

    -- ** CreateSecret
    , module Network.AWS.SecretsManager.CreateSecret

    -- ** GetSecretValue
    , module Network.AWS.SecretsManager.GetSecretValue

    -- ** DescribeSecret
    , module Network.AWS.SecretsManager.DescribeSecret

    -- ** RestoreSecret
    , module Network.AWS.SecretsManager.RestoreSecret

    -- ** CancelRotateSecret
    , module Network.AWS.SecretsManager.CancelRotateSecret

    -- ** PutSecretValue
    , module Network.AWS.SecretsManager.PutSecretValue

    -- ** GetRandomPassword
    , module Network.AWS.SecretsManager.GetRandomPassword

    -- ** ListSecretVersionIds
    , module Network.AWS.SecretsManager.ListSecretVersionIds

    -- ** TagResource
    , module Network.AWS.SecretsManager.TagResource

    -- ** UntagResource
    , module Network.AWS.SecretsManager.UntagResource

    -- ** UpdateSecretVersionStage
    , module Network.AWS.SecretsManager.UpdateSecretVersionStage

    -- * Types

    -- ** RotationRulesType
    , RotationRulesType
    , rotationRulesType
    , rrtAutomaticallyAfterDays

    -- ** SecretListEntry
    , SecretListEntry
    , secretListEntry
    , sleLastChangedDate
    , sleARN
    , sleSecretVersionsToStages
    , sleRotationRules
    , sleDeletedDate
    , sleRotationEnabled
    , sleKMSKeyId
    , sleName
    , sleLastRotatedDate
    , sleLastAccessedDate
    , sleDescription
    , sleRotationLambdaARN
    , sleTags

    -- ** SecretVersionsListEntry
    , SecretVersionsListEntry
    , secretVersionsListEntry
    , svleVersionId
    , svleVersionStages
    , svleCreatedDate
    , svleLastAccessedDate

    -- ** Tag
    , Tag
    , tag
    , tagValue
    , tagKey
    ) where

import Network.AWS.SecretsManager.CancelRotateSecret
import Network.AWS.SecretsManager.CreateSecret
import Network.AWS.SecretsManager.DeleteSecret
import Network.AWS.SecretsManager.DescribeSecret
import Network.AWS.SecretsManager.GetRandomPassword
import Network.AWS.SecretsManager.GetSecretValue
import Network.AWS.SecretsManager.ListSecrets
import Network.AWS.SecretsManager.ListSecretVersionIds
import Network.AWS.SecretsManager.PutSecretValue
import Network.AWS.SecretsManager.RestoreSecret
import Network.AWS.SecretsManager.RotateSecret
import Network.AWS.SecretsManager.TagResource
import Network.AWS.SecretsManager.Types
import Network.AWS.SecretsManager.UntagResource
import Network.AWS.SecretsManager.UpdateSecret
import Network.AWS.SecretsManager.UpdateSecretVersionStage
import Network.AWS.SecretsManager.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'SecretsManager'.
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
