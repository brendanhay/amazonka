{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.RAM.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RAM.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidClientTokenException,
    _TagPolicyViolationException,
    _InvalidParameterException,
    _InvalidResourceTypeException,
    _ResourceShareInvitationArnNotFoundException,
    _InvalidNextTokenException,
    _MalformedArnException,
    _ServerInternalException,
    _UnknownResourceException,
    _OperationNotPermittedException,
    _InvalidMaxResultsException,
    _ServiceUnavailableException,
    _ResourceArnNotFoundException,
    _IdempotentParameterMismatchException,
    _MissingRequiredParameterException,
    _TagLimitExceededException,
    _InvalidStateTransitionException,
    _ResourceShareInvitationAlreadyAcceptedException,
    _ResourceShareLimitExceededException,
    _ResourceShareInvitationExpiredException,
    _ResourceShareInvitationAlreadyRejectedException,

    -- * ResourceOwner
    ResourceOwner (..),

    -- * ResourceShareAssociationStatus
    ResourceShareAssociationStatus (..),

    -- * ResourceShareAssociationType
    ResourceShareAssociationType (..),

    -- * ResourceShareFeatureSet
    ResourceShareFeatureSet (..),

    -- * ResourceShareInvitationStatus
    ResourceShareInvitationStatus (..),

    -- * ResourceShareStatus
    ResourceShareStatus (..),

    -- * ResourceStatus
    ResourceStatus (..),

    -- * Principal
    Principal (..),
    newPrincipal,
    principal_creationTime,
    principal_resourceShareArn,
    principal_lastUpdatedTime,
    principal_external,
    principal_id,

    -- * Resource
    Resource (..),
    newResource,
    resource_creationTime,
    resource_status,
    resource_resourceShareArn,
    resource_lastUpdatedTime,
    resource_arn,
    resource_resourceGroupArn,
    resource_statusMessage,
    resource_type,

    -- * ResourceShare
    ResourceShare (..),
    newResourceShare,
    resourceShare_creationTime,
    resourceShare_status,
    resourceShare_resourceShareArn,
    resourceShare_owningAccountId,
    resourceShare_lastUpdatedTime,
    resourceShare_allowExternalPrincipals,
    resourceShare_name,
    resourceShare_statusMessage,
    resourceShare_featureSet,
    resourceShare_tags,

    -- * ResourceShareAssociation
    ResourceShareAssociation (..),
    newResourceShareAssociation,
    resourceShareAssociation_creationTime,
    resourceShareAssociation_status,
    resourceShareAssociation_resourceShareArn,
    resourceShareAssociation_lastUpdatedTime,
    resourceShareAssociation_external,
    resourceShareAssociation_resourceShareName,
    resourceShareAssociation_associatedEntity,
    resourceShareAssociation_associationType,
    resourceShareAssociation_statusMessage,

    -- * ResourceShareInvitation
    ResourceShareInvitation (..),
    newResourceShareInvitation,
    resourceShareInvitation_status,
    resourceShareInvitation_senderAccountId,
    resourceShareInvitation_resourceShareArn,
    resourceShareInvitation_receiverAccountId,
    resourceShareInvitation_resourceShareAssociations,
    resourceShareInvitation_resourceShareName,
    resourceShareInvitation_receiverArn,
    resourceShareInvitation_invitationTimestamp,
    resourceShareInvitation_resourceShareInvitationArn,

    -- * ResourceSharePermissionDetail
    ResourceSharePermissionDetail (..),
    newResourceSharePermissionDetail,
    resourceSharePermissionDetail_creationTime,
    resourceSharePermissionDetail_resourceType,
    resourceSharePermissionDetail_lastUpdatedTime,
    resourceSharePermissionDetail_arn,
    resourceSharePermissionDetail_defaultVersion,
    resourceSharePermissionDetail_name,
    resourceSharePermissionDetail_version,
    resourceSharePermissionDetail_isResourceTypeDefault,
    resourceSharePermissionDetail_permission,

    -- * ResourceSharePermissionSummary
    ResourceSharePermissionSummary (..),
    newResourceSharePermissionSummary,
    resourceSharePermissionSummary_creationTime,
    resourceSharePermissionSummary_status,
    resourceSharePermissionSummary_resourceType,
    resourceSharePermissionSummary_lastUpdatedTime,
    resourceSharePermissionSummary_arn,
    resourceSharePermissionSummary_defaultVersion,
    resourceSharePermissionSummary_name,
    resourceSharePermissionSummary_version,
    resourceSharePermissionSummary_isResourceTypeDefault,

    -- * ServiceNameAndResourceType
    ServiceNameAndResourceType (..),
    newServiceNameAndResourceType,
    serviceNameAndResourceType_resourceType,
    serviceNameAndResourceType_serviceName,

    -- * Tag
    Tag (..),
    newTag,
    tag_value,
    tag_key,

    -- * TagFilter
    TagFilter (..),
    newTagFilter,
    tagFilter_tagValues,
    tagFilter_tagKey,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RAM.Types.Principal
import Amazonka.RAM.Types.Resource
import Amazonka.RAM.Types.ResourceOwner
import Amazonka.RAM.Types.ResourceShare
import Amazonka.RAM.Types.ResourceShareAssociation
import Amazonka.RAM.Types.ResourceShareAssociationStatus
import Amazonka.RAM.Types.ResourceShareAssociationType
import Amazonka.RAM.Types.ResourceShareFeatureSet
import Amazonka.RAM.Types.ResourceShareInvitation
import Amazonka.RAM.Types.ResourceShareInvitationStatus
import Amazonka.RAM.Types.ResourceSharePermissionDetail
import Amazonka.RAM.Types.ResourceSharePermissionSummary
import Amazonka.RAM.Types.ResourceShareStatus
import Amazonka.RAM.Types.ResourceStatus
import Amazonka.RAM.Types.ServiceNameAndResourceType
import Amazonka.RAM.Types.Tag
import Amazonka.RAM.Types.TagFilter
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2018-01-04@ of the Amazon Resource Access Manager SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "RAM",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "ram",
      Core._serviceSigningName = "ram",
      Core._serviceVersion = "2018-01-04",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "RAM",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | A client token is not valid.
_InvalidClientTokenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidClientTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidClientTokenException"
    Prelude.. Core.hasStatus 400

-- | The specified tag is a reserved word and cannot be used.
_TagPolicyViolationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TagPolicyViolationException =
  Core._MatchServiceError
    defaultService
    "TagPolicyViolationException"
    Prelude.. Core.hasStatus 400

-- | A parameter is not valid.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterException"
    Prelude.. Core.hasStatus 400

-- | The specified resource type is not valid.
_InvalidResourceTypeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidResourceTypeException =
  Core._MatchServiceError
    defaultService
    "InvalidResourceTypeException"
    Prelude.. Core.hasStatus 400

-- | The Amazon Resource Name (ARN) for an invitation was not found.
_ResourceShareInvitationArnNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceShareInvitationArnNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceShareInvitationArnNotFoundException"
    Prelude.. Core.hasStatus 400

-- | The specified value for NextToken is not valid.
_InvalidNextTokenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidNextTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidNextTokenException"
    Prelude.. Core.hasStatus 400

-- | The format of an Amazon Resource Name (ARN) is not valid.
_MalformedArnException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MalformedArnException =
  Core._MatchServiceError
    defaultService
    "MalformedArnException"
    Prelude.. Core.hasStatus 400

-- | The service could not respond to the request due to an internal problem.
_ServerInternalException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServerInternalException =
  Core._MatchServiceError
    defaultService
    "ServerInternalException"
    Prelude.. Core.hasStatus 500

-- | A specified resource was not found.
_UnknownResourceException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnknownResourceException =
  Core._MatchServiceError
    defaultService
    "UnknownResourceException"
    Prelude.. Core.hasStatus 400

-- | The requested operation is not permitted.
_OperationNotPermittedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OperationNotPermittedException =
  Core._MatchServiceError
    defaultService
    "OperationNotPermittedException"
    Prelude.. Core.hasStatus 400

-- | The specified value for MaxResults is not valid.
_InvalidMaxResultsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidMaxResultsException =
  Core._MatchServiceError
    defaultService
    "InvalidMaxResultsException"
    Prelude.. Core.hasStatus 400

-- | The service is not available.
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"
    Prelude.. Core.hasStatus 503

-- | An Amazon Resource Name (ARN) was not found.
_ResourceArnNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceArnNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceArnNotFoundException"
    Prelude.. Core.hasStatus 400

-- | A client token input parameter was reused with an operation, but at
-- least one of the other input parameters is different from the previous
-- call to the operation.
_IdempotentParameterMismatchException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IdempotentParameterMismatchException =
  Core._MatchServiceError
    defaultService
    "IdempotentParameterMismatchException"
    Prelude.. Core.hasStatus 400

-- | A required input parameter is missing.
_MissingRequiredParameterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MissingRequiredParameterException =
  Core._MatchServiceError
    defaultService
    "MissingRequiredParameterException"
    Prelude.. Core.hasStatus 400

-- | The requested tags exceed the limit for your account.
_TagLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TagLimitExceededException =
  Core._MatchServiceError
    defaultService
    "TagLimitExceededException"
    Prelude.. Core.hasStatus 400

-- | The requested state transition is not valid.
_InvalidStateTransitionException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidStateTransitionException =
  Core._MatchServiceError
    defaultService
    "InvalidStateTransitionException"
    Prelude.. Core.hasStatus 400

-- | The invitation was already accepted.
_ResourceShareInvitationAlreadyAcceptedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceShareInvitationAlreadyAcceptedException =
  Core._MatchServiceError
    defaultService
    "ResourceShareInvitationAlreadyAcceptedException"
    Prelude.. Core.hasStatus 400

-- | The requested resource share exceeds the limit for your account.
_ResourceShareLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceShareLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ResourceShareLimitExceededException"
    Prelude.. Core.hasStatus 400

-- | The invitation is expired.
_ResourceShareInvitationExpiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceShareInvitationExpiredException =
  Core._MatchServiceError
    defaultService
    "ResourceShareInvitationExpiredException"
    Prelude.. Core.hasStatus 400

-- | The invitation was already rejected.
_ResourceShareInvitationAlreadyRejectedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceShareInvitationAlreadyRejectedException =
  Core._MatchServiceError
    defaultService
    "ResourceShareInvitationAlreadyRejectedException"
    Prelude.. Core.hasStatus 400
