{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.SecretsManager
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
-- __Support and Feedback for Amazon Web Services Secrets Manager__
--
-- We welcome your feedback. Send your comments to
-- <mailto:awssecretsmanager-feedback@amazon.com awssecretsmanager-feedback\@amazon.com>,
-- or post your feedback and questions in the
-- <http://forums.aws.amazon.com/forum.jspa?forumID=296 Amazon Web Services Secrets Manager Discussion Forum>.
-- For more information about the Amazon Web Services Discussion Forums,
-- see <http://forums.aws.amazon.com/help.jspa Forums Help>.
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
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/monitoring.html#monitoring_cloudtrail Logging Amazon Web Services Secrets Manager Events with Amazon Web Services CloudTrail>
-- in the /Amazon Web Services Secrets Manager User Guide/. To learn more
-- about CloudTrail, including enabling it and find your log files, see the
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/what_is_cloud_trail_top_level.html Amazon Web Services CloudTrail User Guide>.
module Amazonka.SecretsManager
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** DecryptionFailure
    _DecryptionFailure,

    -- ** EncryptionFailure
    _EncryptionFailure,

    -- ** InternalServiceError
    _InternalServiceError,

    -- ** InvalidNextTokenException
    _InvalidNextTokenException,

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** MalformedPolicyDocumentException
    _MalformedPolicyDocumentException,

    -- ** PreconditionNotMetException
    _PreconditionNotMetException,

    -- ** PublicPolicyException
    _PublicPolicyException,

    -- ** ResourceExistsException
    _ResourceExistsException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CancelRotateSecret
    CancelRotateSecret (CancelRotateSecret'),
    newCancelRotateSecret,
    CancelRotateSecretResponse (CancelRotateSecretResponse'),
    newCancelRotateSecretResponse,

    -- ** CreateSecret
    CreateSecret (CreateSecret'),
    newCreateSecret,
    CreateSecretResponse (CreateSecretResponse'),
    newCreateSecretResponse,

    -- ** DeleteResourcePolicy
    DeleteResourcePolicy (DeleteResourcePolicy'),
    newDeleteResourcePolicy,
    DeleteResourcePolicyResponse (DeleteResourcePolicyResponse'),
    newDeleteResourcePolicyResponse,

    -- ** DeleteSecret
    DeleteSecret (DeleteSecret'),
    newDeleteSecret,
    DeleteSecretResponse (DeleteSecretResponse'),
    newDeleteSecretResponse,

    -- ** DescribeSecret
    DescribeSecret (DescribeSecret'),
    newDescribeSecret,
    DescribeSecretResponse (DescribeSecretResponse'),
    newDescribeSecretResponse,

    -- ** GetRandomPassword
    GetRandomPassword (GetRandomPassword'),
    newGetRandomPassword,
    GetRandomPasswordResponse (GetRandomPasswordResponse'),
    newGetRandomPasswordResponse,

    -- ** GetResourcePolicy
    GetResourcePolicy (GetResourcePolicy'),
    newGetResourcePolicy,
    GetResourcePolicyResponse (GetResourcePolicyResponse'),
    newGetResourcePolicyResponse,

    -- ** GetSecretValue
    GetSecretValue (GetSecretValue'),
    newGetSecretValue,
    GetSecretValueResponse (GetSecretValueResponse'),
    newGetSecretValueResponse,

    -- ** ListSecretVersionIds (Paginated)
    ListSecretVersionIds (ListSecretVersionIds'),
    newListSecretVersionIds,
    ListSecretVersionIdsResponse (ListSecretVersionIdsResponse'),
    newListSecretVersionIdsResponse,

    -- ** ListSecrets (Paginated)
    ListSecrets (ListSecrets'),
    newListSecrets,
    ListSecretsResponse (ListSecretsResponse'),
    newListSecretsResponse,

    -- ** PutResourcePolicy
    PutResourcePolicy (PutResourcePolicy'),
    newPutResourcePolicy,
    PutResourcePolicyResponse (PutResourcePolicyResponse'),
    newPutResourcePolicyResponse,

    -- ** PutSecretValue
    PutSecretValue (PutSecretValue'),
    newPutSecretValue,
    PutSecretValueResponse (PutSecretValueResponse'),
    newPutSecretValueResponse,

    -- ** RemoveRegionsFromReplication
    RemoveRegionsFromReplication (RemoveRegionsFromReplication'),
    newRemoveRegionsFromReplication,
    RemoveRegionsFromReplicationResponse (RemoveRegionsFromReplicationResponse'),
    newRemoveRegionsFromReplicationResponse,

    -- ** ReplicateSecretToRegions
    ReplicateSecretToRegions (ReplicateSecretToRegions'),
    newReplicateSecretToRegions,
    ReplicateSecretToRegionsResponse (ReplicateSecretToRegionsResponse'),
    newReplicateSecretToRegionsResponse,

    -- ** RestoreSecret
    RestoreSecret (RestoreSecret'),
    newRestoreSecret,
    RestoreSecretResponse (RestoreSecretResponse'),
    newRestoreSecretResponse,

    -- ** RotateSecret
    RotateSecret (RotateSecret'),
    newRotateSecret,
    RotateSecretResponse (RotateSecretResponse'),
    newRotateSecretResponse,

    -- ** StopReplicationToReplica
    StopReplicationToReplica (StopReplicationToReplica'),
    newStopReplicationToReplica,
    StopReplicationToReplicaResponse (StopReplicationToReplicaResponse'),
    newStopReplicationToReplicaResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateSecret
    UpdateSecret (UpdateSecret'),
    newUpdateSecret,
    UpdateSecretResponse (UpdateSecretResponse'),
    newUpdateSecretResponse,

    -- ** UpdateSecretVersionStage
    UpdateSecretVersionStage (UpdateSecretVersionStage'),
    newUpdateSecretVersionStage,
    UpdateSecretVersionStageResponse (UpdateSecretVersionStageResponse'),
    newUpdateSecretVersionStageResponse,

    -- ** ValidateResourcePolicy
    ValidateResourcePolicy (ValidateResourcePolicy'),
    newValidateResourcePolicy,
    ValidateResourcePolicyResponse (ValidateResourcePolicyResponse'),
    newValidateResourcePolicyResponse,

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
