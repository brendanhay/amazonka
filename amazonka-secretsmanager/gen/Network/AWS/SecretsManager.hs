{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecretsManager
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS Secrets Manager API Reference
--
-- AWS Secrets Manager provides a service to enable you to store, manage,
-- and retrieve, secrets.
--
-- This guide provides descriptions of the Secrets Manager API. For more
-- information about using this service, see the
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/introduction.html AWS Secrets Manager User Guide>.
--
-- __API Version__
--
-- This version of the Secrets Manager API Reference documents the Secrets
-- Manager API version 2017-10-17.
--
-- As an alternative to using the API, you can use one of the AWS SDKs,
-- which consist of libraries and sample code for various programming
-- languages and platforms such as Java, Ruby, .NET, iOS, and Android. The
-- SDKs provide a convenient way to create programmatic access to AWS
-- Secrets Manager. For example, the SDKs provide cryptographically signing
-- requests, managing errors, and retrying requests automatically. For more
-- information about the AWS SDKs, including downloading and installing
-- them, see <http://aws.amazon.com/tools/ Tools for Amazon Web Services>.
--
-- We recommend you use the AWS SDKs to make programmatic API calls to
-- Secrets Manager. However, you also can use the Secrets Manager HTTP
-- Query API to make direct calls to the Secrets Manager web service. To
-- learn more about the Secrets Manager HTTP Query API, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/query-requests.html Making Query Requests>
-- in the /AWS Secrets Manager User Guide/.
--
-- Secrets Manager API supports GET and POST requests for all actions, and
-- doesn\'t require you to use GET for some actions and POST for others.
-- However, GET requests are subject to the limitation size of a URL.
-- Therefore, for operations that require larger sizes, use a POST request.
--
-- __Support and Feedback for AWS Secrets Manager__
--
-- We welcome your feedback. Send your comments to
-- <mailto:awssecretsmanager-feedback@amazon.com awssecretsmanager-feedback\@amazon.com>,
-- or post your feedback and questions in the
-- <http://forums.aws.amazon.com/forum.jspa?forumID=296 AWS Secrets Manager Discussion Forum>.
-- For more information about the AWS Discussion Forums, see
-- <http://forums.aws.amazon.com/help.jspa Forums Help>.
--
-- __How examples are presented__
--
-- The JSON that AWS Secrets Manager expects as your request parameters and
-- the service returns as a response to HTTP query requests contain single,
-- long strings without line breaks or white space formatting. The JSON
-- shown in the examples displays the code formatted with both line breaks
-- and white space to improve readability. When example input parameters
-- can also cause long strings extending beyond the screen, you can insert
-- line breaks to enhance readability. You should always submit the input
-- as a single JSON text string.
--
-- __Logging API Requests__
--
-- AWS Secrets Manager supports AWS CloudTrail, a service that records AWS
-- API calls for your AWS account and delivers log files to an Amazon S3
-- bucket. By using information that\'s collected by AWS CloudTrail, you
-- can determine the requests successfully made to Secrets Manager, who
-- made the request, when it was made, and so on. For more about AWS
-- Secrets Manager and support for AWS CloudTrail, see
-- <http://docs.aws.amazon.com/secretsmanager/latest/userguide/monitoring.html#monitoring_cloudtrail Logging AWS Secrets Manager Events with AWS CloudTrail>
-- in the /AWS Secrets Manager User Guide/. To learn more about CloudTrail,
-- including enabling it and find your log files, see the
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/what_is_cloud_trail_top_level.html AWS CloudTrail User Guide>.
module Network.AWS.SecretsManager
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** MalformedPolicyDocumentException
    _MalformedPolicyDocumentException,

    -- ** EncryptionFailure
    _EncryptionFailure,

    -- ** PublicPolicyException
    _PublicPolicyException,

    -- ** DecryptionFailure
    _DecryptionFailure,

    -- ** InvalidNextTokenException
    _InvalidNextTokenException,

    -- ** PreconditionNotMetException
    _PreconditionNotMetException,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ResourceExistsException
    _ResourceExistsException,

    -- ** InternalServiceError
    _InternalServiceError,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DeleteSecret
    DeleteSecret (DeleteSecret'),
    newDeleteSecret,
    DeleteSecretResponse (DeleteSecretResponse'),
    newDeleteSecretResponse,

    -- ** UpdateSecret
    UpdateSecret (UpdateSecret'),
    newUpdateSecret,
    UpdateSecretResponse (UpdateSecretResponse'),
    newUpdateSecretResponse,

    -- ** RotateSecret
    RotateSecret (RotateSecret'),
    newRotateSecret,
    RotateSecretResponse (RotateSecretResponse'),
    newRotateSecretResponse,

    -- ** RemoveRegionsFromReplication
    RemoveRegionsFromReplication (RemoveRegionsFromReplication'),
    newRemoveRegionsFromReplication,
    RemoveRegionsFromReplicationResponse (RemoveRegionsFromReplicationResponse'),
    newRemoveRegionsFromReplicationResponse,

    -- ** GetResourcePolicy
    GetResourcePolicy (GetResourcePolicy'),
    newGetResourcePolicy,
    GetResourcePolicyResponse (GetResourcePolicyResponse'),
    newGetResourcePolicyResponse,

    -- ** ValidateResourcePolicy
    ValidateResourcePolicy (ValidateResourcePolicy'),
    newValidateResourcePolicy,
    ValidateResourcePolicyResponse (ValidateResourcePolicyResponse'),
    newValidateResourcePolicyResponse,

    -- ** CancelRotateSecret
    CancelRotateSecret (CancelRotateSecret'),
    newCancelRotateSecret,
    CancelRotateSecretResponse (CancelRotateSecretResponse'),
    newCancelRotateSecretResponse,

    -- ** RestoreSecret
    RestoreSecret (RestoreSecret'),
    newRestoreSecret,
    RestoreSecretResponse (RestoreSecretResponse'),
    newRestoreSecretResponse,

    -- ** GetSecretValue
    GetSecretValue (GetSecretValue'),
    newGetSecretValue,
    GetSecretValueResponse (GetSecretValueResponse'),
    newGetSecretValueResponse,

    -- ** DescribeSecret
    DescribeSecret (DescribeSecret'),
    newDescribeSecret,
    DescribeSecretResponse (DescribeSecretResponse'),
    newDescribeSecretResponse,

    -- ** PutResourcePolicy
    PutResourcePolicy (PutResourcePolicy'),
    newPutResourcePolicy,
    PutResourcePolicyResponse (PutResourcePolicyResponse'),
    newPutResourcePolicyResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** GetRandomPassword
    GetRandomPassword (GetRandomPassword'),
    newGetRandomPassword,
    GetRandomPasswordResponse (GetRandomPasswordResponse'),
    newGetRandomPasswordResponse,

    -- ** CreateSecret
    CreateSecret (CreateSecret'),
    newCreateSecret,
    CreateSecretResponse (CreateSecretResponse'),
    newCreateSecretResponse,

    -- ** StopReplicationToReplica
    StopReplicationToReplica (StopReplicationToReplica'),
    newStopReplicationToReplica,
    StopReplicationToReplicaResponse (StopReplicationToReplicaResponse'),
    newStopReplicationToReplicaResponse,

    -- ** ListSecrets (Paginated)
    ListSecrets (ListSecrets'),
    newListSecrets,
    ListSecretsResponse (ListSecretsResponse'),
    newListSecretsResponse,

    -- ** ReplicateSecretToRegions
    ReplicateSecretToRegions (ReplicateSecretToRegions'),
    newReplicateSecretToRegions,
    ReplicateSecretToRegionsResponse (ReplicateSecretToRegionsResponse'),
    newReplicateSecretToRegionsResponse,

    -- ** PutSecretValue
    PutSecretValue (PutSecretValue'),
    newPutSecretValue,
    PutSecretValueResponse (PutSecretValueResponse'),
    newPutSecretValueResponse,

    -- ** UpdateSecretVersionStage
    UpdateSecretVersionStage (UpdateSecretVersionStage'),
    newUpdateSecretVersionStage,
    UpdateSecretVersionStageResponse (UpdateSecretVersionStageResponse'),
    newUpdateSecretVersionStageResponse,

    -- ** DeleteResourcePolicy
    DeleteResourcePolicy (DeleteResourcePolicy'),
    newDeleteResourcePolicy,
    DeleteResourcePolicyResponse (DeleteResourcePolicyResponse'),
    newDeleteResourcePolicyResponse,

    -- ** ListSecretVersionIds (Paginated)
    ListSecretVersionIds (ListSecretVersionIds'),
    newListSecretVersionIds,
    ListSecretVersionIdsResponse (ListSecretVersionIdsResponse'),
    newListSecretVersionIdsResponse,

    -- * Types

    -- ** FilterNameStringType
    FilterNameStringType (..),

    -- ** SortOrderType
    SortOrderType (..),

    -- ** StatusType
    StatusType (..),

    -- ** Filter
    Filter (Filter'),
    newFilter,

    -- ** ReplicaRegionType
    ReplicaRegionType (ReplicaRegionType'),
    newReplicaRegionType,

    -- ** ReplicationStatusType
    ReplicationStatusType (ReplicationStatusType'),
    newReplicationStatusType,

    -- ** RotationRulesType
    RotationRulesType (RotationRulesType'),
    newRotationRulesType,

    -- ** SecretListEntry
    SecretListEntry (SecretListEntry'),
    newSecretListEntry,

    -- ** SecretVersionsListEntry
    SecretVersionsListEntry (SecretVersionsListEntry'),
    newSecretVersionsListEntry,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** ValidationErrorsEntry
    ValidationErrorsEntry (ValidationErrorsEntry'),
    newValidationErrorsEntry,
  )
where

import Network.AWS.SecretsManager.CancelRotateSecret
import Network.AWS.SecretsManager.CreateSecret
import Network.AWS.SecretsManager.DeleteResourcePolicy
import Network.AWS.SecretsManager.DeleteSecret
import Network.AWS.SecretsManager.DescribeSecret
import Network.AWS.SecretsManager.GetRandomPassword
import Network.AWS.SecretsManager.GetResourcePolicy
import Network.AWS.SecretsManager.GetSecretValue
import Network.AWS.SecretsManager.Lens
import Network.AWS.SecretsManager.ListSecretVersionIds
import Network.AWS.SecretsManager.ListSecrets
import Network.AWS.SecretsManager.PutResourcePolicy
import Network.AWS.SecretsManager.PutSecretValue
import Network.AWS.SecretsManager.RemoveRegionsFromReplication
import Network.AWS.SecretsManager.ReplicateSecretToRegions
import Network.AWS.SecretsManager.RestoreSecret
import Network.AWS.SecretsManager.RotateSecret
import Network.AWS.SecretsManager.StopReplicationToReplica
import Network.AWS.SecretsManager.TagResource
import Network.AWS.SecretsManager.Types
import Network.AWS.SecretsManager.UntagResource
import Network.AWS.SecretsManager.UpdateSecret
import Network.AWS.SecretsManager.UpdateSecretVersionStage
import Network.AWS.SecretsManager.ValidateResourcePolicy
import Network.AWS.SecretsManager.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'SecretsManager'.

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
