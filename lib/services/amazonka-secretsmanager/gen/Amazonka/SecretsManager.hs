{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.SecretsManager
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2017-10-17@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Web Services Secrets Manager
--
-- Amazon Web Services Secrets Manager provides a service to enable you to
-- store, manage, and retrieve, secrets.
--
-- This guide provides descriptions of the Secrets Manager API. For more
-- information about using this service, see the
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/introduction.html Amazon Web Services Secrets Manager User Guide>.
--
-- __API Version__
--
-- This version of the Secrets Manager API Reference documents the Secrets
-- Manager API version 2017-10-17.
--
-- As an alternative to using the API, you can use one of the Amazon Web
-- Services SDKs, which consist of libraries and sample code for various
-- programming languages and platforms such as Java, Ruby, .NET, iOS, and
-- Android. The SDKs provide a convenient way to create programmatic access
-- to Amazon Web Services Secrets Manager. For example, the SDKs provide
-- cryptographically signing requests, managing errors, and retrying
-- requests automatically. For more information about the Amazon Web
-- Services SDKs, including downloading and installing them, see
-- <http://aws.amazon.com/tools/ Tools for Amazon Web Services>.
--
-- We recommend you use the Amazon Web Services SDKs to make programmatic
-- API calls to Secrets Manager. However, you also can use the Secrets
-- Manager HTTP Query API to make direct calls to the Secrets Manager web
-- service. To learn more about the Secrets Manager HTTP Query API, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/query-requests.html Making Query Requests>
-- in the /Amazon Web Services Secrets Manager User Guide/.
--
-- Secrets Manager API supports GET and POST requests for all actions, and
-- doesn\'t require you to use GET for some actions and POST for others.
-- However, GET requests are subject to the limitation size of a URL.
-- Therefore, for operations that require larger sizes, use a POST request.
--
-- __Support and Feedback for Amazon Web Services Secrets Manager__
--
-- We welcome your feedback. Send your comments to
-- <mailto:awssecretsmanager-feedback@amazon.com awssecretsmanager-feedback\@amazon.com>,
-- or post your feedback and questions in the
-- <http://forums.aws.amazon.com/forum.jspa?forumID=296 Amazon Web Services Secrets Manager Discussion Forum>.
-- For more information about the Amazon Web Services Discussion Forums,
-- see <http://forums.aws.amazon.com/help.jspa Forums Help>.
--
-- __How examples are presented__
--
-- The JSON that Amazon Web Services Secrets Manager expects as your
-- request parameters and the service returns as a response to HTTP query
-- requests contain single, long strings without line breaks or white space
-- formatting. The JSON shown in the examples displays the code formatted
-- with both line breaks and white space to improve readability. When
-- example input parameters can also cause long strings extending beyond
-- the screen, you can insert line breaks to enhance readability. You
-- should always submit the input as a single JSON text string.
--
-- __Logging API Requests__
--
-- Amazon Web Services Secrets Manager supports Amazon Web Services
-- CloudTrail, a service that records Amazon Web Services API calls for
-- your Amazon Web Services account and delivers log files to an Amazon S3
-- bucket. By using information that\'s collected by Amazon Web Services
-- CloudTrail, you can determine the requests successfully made to Secrets
-- Manager, who made the request, when it was made, and so on. For more
-- about Amazon Web Services Secrets Manager and support for Amazon Web
-- Services CloudTrail, see
-- <http://docs.aws.amazon.com/secretsmanager/latest/userguide/monitoring.html#monitoring_cloudtrail Logging Amazon Web Services Secrets Manager Events with Amazon Web Services CloudTrail>
-- in the /Amazon Web Services Secrets Manager User Guide/. To learn more
-- about CloudTrail, including enabling it and find your log files, see the
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/what_is_cloud_trail_top_level.html Amazon Web Services CloudTrail User Guide>.
module Amazonka.SecretsManager
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** MalformedPolicyDocumentException
    _MalformedPolicyDocumentException,

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** DecryptionFailure
    _DecryptionFailure,

    -- ** PublicPolicyException
    _PublicPolicyException,

    -- ** EncryptionFailure
    _EncryptionFailure,

    -- ** PreconditionNotMetException
    _PreconditionNotMetException,

    -- ** InvalidNextTokenException
    _InvalidNextTokenException,

    -- ** InternalServiceError
    _InternalServiceError,

    -- ** ResourceExistsException
    _ResourceExistsException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ValidateResourcePolicy
    ValidateResourcePolicy (ValidateResourcePolicy'),
    newValidateResourcePolicy,
    ValidateResourcePolicyResponse (ValidateResourcePolicyResponse'),
    newValidateResourcePolicyResponse,

    -- ** DeleteSecret
    DeleteSecret (DeleteSecret'),
    newDeleteSecret,
    DeleteSecretResponse (DeleteSecretResponse'),
    newDeleteSecretResponse,

    -- ** ListSecrets (Paginated)
    ListSecrets (ListSecrets'),
    newListSecrets,
    ListSecretsResponse (ListSecretsResponse'),
    newListSecretsResponse,

    -- ** UpdateSecret
    UpdateSecret (UpdateSecret'),
    newUpdateSecret,
    UpdateSecretResponse (UpdateSecretResponse'),
    newUpdateSecretResponse,

    -- ** RemoveRegionsFromReplication
    RemoveRegionsFromReplication (RemoveRegionsFromReplication'),
    newRemoveRegionsFromReplication,
    RemoveRegionsFromReplicationResponse (RemoveRegionsFromReplicationResponse'),
    newRemoveRegionsFromReplicationResponse,

    -- ** RotateSecret
    RotateSecret (RotateSecret'),
    newRotateSecret,
    RotateSecretResponse (RotateSecretResponse'),
    newRotateSecretResponse,

    -- ** CreateSecret
    CreateSecret (CreateSecret'),
    newCreateSecret,
    CreateSecretResponse (CreateSecretResponse'),
    newCreateSecretResponse,

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

    -- ** RestoreSecret
    RestoreSecret (RestoreSecret'),
    newRestoreSecret,
    RestoreSecretResponse (RestoreSecretResponse'),
    newRestoreSecretResponse,

    -- ** CancelRotateSecret
    CancelRotateSecret (CancelRotateSecret'),
    newCancelRotateSecret,
    CancelRotateSecretResponse (CancelRotateSecretResponse'),
    newCancelRotateSecretResponse,

    -- ** GetResourcePolicy
    GetResourcePolicy (GetResourcePolicy'),
    newGetResourcePolicy,
    GetResourcePolicyResponse (GetResourcePolicyResponse'),
    newGetResourcePolicyResponse,

    -- ** PutSecretValue
    PutSecretValue (PutSecretValue'),
    newPutSecretValue,
    PutSecretValueResponse (PutSecretValueResponse'),
    newPutSecretValueResponse,

    -- ** ReplicateSecretToRegions
    ReplicateSecretToRegions (ReplicateSecretToRegions'),
    newReplicateSecretToRegions,
    ReplicateSecretToRegionsResponse (ReplicateSecretToRegionsResponse'),
    newReplicateSecretToRegionsResponse,

    -- ** StopReplicationToReplica
    StopReplicationToReplica (StopReplicationToReplica'),
    newStopReplicationToReplica,
    StopReplicationToReplicaResponse (StopReplicationToReplicaResponse'),
    newStopReplicationToReplicaResponse,

    -- ** GetRandomPassword
    GetRandomPassword (GetRandomPassword'),
    newGetRandomPassword,
    GetRandomPasswordResponse (GetRandomPasswordResponse'),
    newGetRandomPasswordResponse,

    -- ** ListSecretVersionIds (Paginated)
    ListSecretVersionIds (ListSecretVersionIds'),
    newListSecretVersionIds,
    ListSecretVersionIdsResponse (ListSecretVersionIdsResponse'),
    newListSecretVersionIdsResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** PutResourcePolicy
    PutResourcePolicy (PutResourcePolicy'),
    newPutResourcePolicy,
    PutResourcePolicyResponse (PutResourcePolicyResponse'),
    newPutResourcePolicyResponse,

    -- ** DeleteResourcePolicy
    DeleteResourcePolicy (DeleteResourcePolicy'),
    newDeleteResourcePolicy,
    DeleteResourcePolicyResponse (DeleteResourcePolicyResponse'),
    newDeleteResourcePolicyResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateSecretVersionStage
    UpdateSecretVersionStage (UpdateSecretVersionStage'),
    newUpdateSecretVersionStage,
    UpdateSecretVersionStageResponse (UpdateSecretVersionStageResponse'),
    newUpdateSecretVersionStageResponse,

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

import Amazonka.SecretsManager.CancelRotateSecret
import Amazonka.SecretsManager.CreateSecret
import Amazonka.SecretsManager.DeleteResourcePolicy
import Amazonka.SecretsManager.DeleteSecret
import Amazonka.SecretsManager.DescribeSecret
import Amazonka.SecretsManager.GetRandomPassword
import Amazonka.SecretsManager.GetResourcePolicy
import Amazonka.SecretsManager.GetSecretValue
import Amazonka.SecretsManager.Lens
import Amazonka.SecretsManager.ListSecretVersionIds
import Amazonka.SecretsManager.ListSecrets
import Amazonka.SecretsManager.PutResourcePolicy
import Amazonka.SecretsManager.PutSecretValue
import Amazonka.SecretsManager.RemoveRegionsFromReplication
import Amazonka.SecretsManager.ReplicateSecretToRegions
import Amazonka.SecretsManager.RestoreSecret
import Amazonka.SecretsManager.RotateSecret
import Amazonka.SecretsManager.StopReplicationToReplica
import Amazonka.SecretsManager.TagResource
import Amazonka.SecretsManager.Types
import Amazonka.SecretsManager.UntagResource
import Amazonka.SecretsManager.UpdateSecret
import Amazonka.SecretsManager.UpdateSecretVersionStage
import Amazonka.SecretsManager.ValidateResourcePolicy
import Amazonka.SecretsManager.Waiters

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
