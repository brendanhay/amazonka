{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.RAM.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RAM.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _IdempotentParameterMismatchException,
    _InvalidClientTokenException,
    _InvalidMaxResultsException,
    _InvalidNextTokenException,
    _InvalidParameterException,
    _InvalidPolicyException,
    _InvalidResourceTypeException,
    _InvalidStateTransitionException,
    _MalformedArnException,
    _MalformedPolicyTemplateException,
    _MissingRequiredParameterException,
    _OperationNotPermittedException,
    _PermissionAlreadyExistsException,
    _PermissionLimitExceededException,
    _PermissionVersionsLimitExceededException,
    _ResourceArnNotFoundException,
    _ResourceShareInvitationAlreadyAcceptedException,
    _ResourceShareInvitationAlreadyRejectedException,
    _ResourceShareInvitationArnNotFoundException,
    _ResourceShareInvitationExpiredException,
    _ResourceShareLimitExceededException,
    _ServerInternalException,
    _ServiceUnavailableException,
    _TagLimitExceededException,
    _TagPolicyViolationException,
    _ThrottlingException,
    _UnknownResourceException,
    _UnmatchedPolicyPermissionException,

    -- * PermissionFeatureSet
    PermissionFeatureSet (..),

    -- * PermissionStatus
    PermissionStatus (..),

    -- * PermissionType
    PermissionType (..),

    -- * PermissionTypeFilter
    PermissionTypeFilter (..),

    -- * ReplacePermissionAssociationsWorkStatus
    ReplacePermissionAssociationsWorkStatus (..),

    -- * ResourceOwner
    ResourceOwner (..),

    -- * ResourceRegionScope
    ResourceRegionScope (..),

    -- * ResourceRegionScopeFilter
    ResourceRegionScopeFilter (..),

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

    -- * AssociatedPermission
    AssociatedPermission (..),
    newAssociatedPermission,
    associatedPermission_arn,
    associatedPermission_defaultVersion,
    associatedPermission_featureSet,
    associatedPermission_lastUpdatedTime,
    associatedPermission_permissionVersion,
    associatedPermission_resourceShareArn,
    associatedPermission_resourceType,
    associatedPermission_status,

    -- * Principal
    Principal (..),
    newPrincipal,
    principal_creationTime,
    principal_external,
    principal_id,
    principal_lastUpdatedTime,
    principal_resourceShareArn,

    -- * ReplacePermissionAssociationsWork
    ReplacePermissionAssociationsWork (..),
    newReplacePermissionAssociationsWork,
    replacePermissionAssociationsWork_creationTime,
    replacePermissionAssociationsWork_fromPermissionArn,
    replacePermissionAssociationsWork_fromPermissionVersion,
    replacePermissionAssociationsWork_id,
    replacePermissionAssociationsWork_lastUpdatedTime,
    replacePermissionAssociationsWork_status,
    replacePermissionAssociationsWork_statusMessage,
    replacePermissionAssociationsWork_toPermissionArn,
    replacePermissionAssociationsWork_toPermissionVersion,

    -- * Resource
    Resource (..),
    newResource,
    resource_arn,
    resource_creationTime,
    resource_lastUpdatedTime,
    resource_resourceGroupArn,
    resource_resourceRegionScope,
    resource_resourceShareArn,
    resource_status,
    resource_statusMessage,
    resource_type,

    -- * ResourceShare
    ResourceShare (..),
    newResourceShare,
    resourceShare_allowExternalPrincipals,
    resourceShare_creationTime,
    resourceShare_featureSet,
    resourceShare_lastUpdatedTime,
    resourceShare_name,
    resourceShare_owningAccountId,
    resourceShare_resourceShareArn,
    resourceShare_status,
    resourceShare_statusMessage,
    resourceShare_tags,

    -- * ResourceShareAssociation
    ResourceShareAssociation (..),
    newResourceShareAssociation,
    resourceShareAssociation_associatedEntity,
    resourceShareAssociation_associationType,
    resourceShareAssociation_creationTime,
    resourceShareAssociation_external,
    resourceShareAssociation_lastUpdatedTime,
    resourceShareAssociation_resourceShareArn,
    resourceShareAssociation_resourceShareName,
    resourceShareAssociation_status,
    resourceShareAssociation_statusMessage,

    -- * ResourceShareInvitation
    ResourceShareInvitation (..),
    newResourceShareInvitation,
    resourceShareInvitation_invitationTimestamp,
    resourceShareInvitation_receiverAccountId,
    resourceShareInvitation_receiverArn,
    resourceShareInvitation_resourceShareArn,
    resourceShareInvitation_resourceShareAssociations,
    resourceShareInvitation_resourceShareInvitationArn,
    resourceShareInvitation_resourceShareName,
    resourceShareInvitation_senderAccountId,
    resourceShareInvitation_status,

    -- * ResourceSharePermissionDetail
    ResourceSharePermissionDetail (..),
    newResourceSharePermissionDetail,
    resourceSharePermissionDetail_arn,
    resourceSharePermissionDetail_creationTime,
    resourceSharePermissionDetail_defaultVersion,
    resourceSharePermissionDetail_featureSet,
    resourceSharePermissionDetail_isResourceTypeDefault,
    resourceSharePermissionDetail_lastUpdatedTime,
    resourceSharePermissionDetail_name,
    resourceSharePermissionDetail_permission,
    resourceSharePermissionDetail_permissionType,
    resourceSharePermissionDetail_resourceType,
    resourceSharePermissionDetail_status,
    resourceSharePermissionDetail_tags,
    resourceSharePermissionDetail_version,

    -- * ResourceSharePermissionSummary
    ResourceSharePermissionSummary (..),
    newResourceSharePermissionSummary,
    resourceSharePermissionSummary_arn,
    resourceSharePermissionSummary_creationTime,
    resourceSharePermissionSummary_defaultVersion,
    resourceSharePermissionSummary_featureSet,
    resourceSharePermissionSummary_isResourceTypeDefault,
    resourceSharePermissionSummary_lastUpdatedTime,
    resourceSharePermissionSummary_name,
    resourceSharePermissionSummary_permissionType,
    resourceSharePermissionSummary_resourceType,
    resourceSharePermissionSummary_status,
    resourceSharePermissionSummary_tags,
    resourceSharePermissionSummary_version,

    -- * ServiceNameAndResourceType
    ServiceNameAndResourceType (..),
    newServiceNameAndResourceType,
    serviceNameAndResourceType_resourceRegionScope,
    serviceNameAndResourceType_resourceType,
    serviceNameAndResourceType_serviceName,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * TagFilter
    TagFilter (..),
    newTagFilter,
    tagFilter_tagKey,
    tagFilter_tagValues,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RAM.Types.AssociatedPermission
import Amazonka.RAM.Types.PermissionFeatureSet
import Amazonka.RAM.Types.PermissionStatus
import Amazonka.RAM.Types.PermissionType
import Amazonka.RAM.Types.PermissionTypeFilter
import Amazonka.RAM.Types.Principal
import Amazonka.RAM.Types.ReplacePermissionAssociationsWork
import Amazonka.RAM.Types.ReplacePermissionAssociationsWorkStatus
import Amazonka.RAM.Types.Resource
import Amazonka.RAM.Types.ResourceOwner
import Amazonka.RAM.Types.ResourceRegionScope
import Amazonka.RAM.Types.ResourceRegionScopeFilter
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
    { Core.abbrev = "RAM",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "ram",
      Core.signingName = "ram",
      Core.version = "2018-01-04",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "RAM",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | The operation failed because the client token input parameter matched
-- one that was used with a previous call to the operation, but at least
-- one of the other input parameters is different from the previous call.
_IdempotentParameterMismatchException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_IdempotentParameterMismatchException =
  Core._MatchServiceError
    defaultService
    "IdempotentParameterMismatchException"
    Prelude.. Core.hasStatus 400

-- | The operation failed because the specified client token isn\'t valid.
_InvalidClientTokenException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidClientTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidClientTokenException"
    Prelude.. Core.hasStatus 400

-- | The operation failed because the specified value for @MaxResults@ isn\'t
-- valid.
_InvalidMaxResultsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidMaxResultsException =
  Core._MatchServiceError
    defaultService
    "InvalidMaxResultsException"
    Prelude.. Core.hasStatus 400

-- | The operation failed because the specified value for @NextToken@ isn\'t
-- valid. You must specify a value you received in the @NextToken@ response
-- of a previous call to this operation.
_InvalidNextTokenException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidNextTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidNextTokenException"
    Prelude.. Core.hasStatus 400

-- | The operation failed because a parameter you specified isn\'t valid.
_InvalidParameterException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterException"
    Prelude.. Core.hasStatus 400

-- | The operation failed because a policy you specified isn\'t valid.
_InvalidPolicyException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidPolicyException =
  Core._MatchServiceError
    defaultService
    "InvalidPolicyException"
    Prelude.. Core.hasStatus 400

-- | The operation failed because the specified resource type isn\'t valid.
_InvalidResourceTypeException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidResourceTypeException =
  Core._MatchServiceError
    defaultService
    "InvalidResourceTypeException"
    Prelude.. Core.hasStatus 400

-- | The operation failed because the requested operation isn\'t valid for
-- the resource share in its current state.
_InvalidStateTransitionException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidStateTransitionException =
  Core._MatchServiceError
    defaultService
    "InvalidStateTransitionException"
    Prelude.. Core.hasStatus 400

-- | The operation failed because the specified
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
-- has a format that isn\'t valid.
_MalformedArnException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_MalformedArnException =
  Core._MatchServiceError
    defaultService
    "MalformedArnException"
    Prelude.. Core.hasStatus 400

-- | The operation failed because the policy template that you provided
-- isn\'t valid.
_MalformedPolicyTemplateException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_MalformedPolicyTemplateException =
  Core._MatchServiceError
    defaultService
    "MalformedPolicyTemplateException"
    Prelude.. Core.hasStatus 400

-- | The operation failed because a required input parameter is missing.
_MissingRequiredParameterException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_MissingRequiredParameterException =
  Core._MatchServiceError
    defaultService
    "MissingRequiredParameterException"
    Prelude.. Core.hasStatus 400

-- | The operation failed because the requested operation isn\'t permitted.
_OperationNotPermittedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_OperationNotPermittedException =
  Core._MatchServiceError
    defaultService
    "OperationNotPermittedException"
    Prelude.. Core.hasStatus 400

-- | The operation failed because a permission with the specified name
-- already exists in the requested Amazon Web Services Region. Choose a
-- different name.
_PermissionAlreadyExistsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_PermissionAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "PermissionAlreadyExistsException"
    Prelude.. Core.hasStatus 409

-- | The operation failed because it would exceed the maximum number of
-- permissions you can create in each Amazon Web Services Region. To view
-- the limits for your Amazon Web Services account, see the
-- <https://console.aws.amazon.com/servicequotas/home/services/ram/quotas RAM page in the Service Quotas console>.
_PermissionLimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_PermissionLimitExceededException =
  Core._MatchServiceError
    defaultService
    "PermissionLimitExceededException"
    Prelude.. Core.hasStatus 400

-- | The operation failed because it would exceed the limit for the number of
-- versions you can have for a permission. To view the limits for your
-- Amazon Web Services account, see the
-- <https://console.aws.amazon.com/servicequotas/home/services/ram/quotas RAM page in the Service Quotas console>.
_PermissionVersionsLimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_PermissionVersionsLimitExceededException =
  Core._MatchServiceError
    defaultService
    "PermissionVersionsLimitExceededException"
    Prelude.. Core.hasStatus 400

-- | The operation failed because the specified
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
-- was not found.
_ResourceArnNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceArnNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceArnNotFoundException"
    Prelude.. Core.hasStatus 400

-- | The operation failed because the specified invitation was already
-- accepted.
_ResourceShareInvitationAlreadyAcceptedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceShareInvitationAlreadyAcceptedException =
  Core._MatchServiceError
    defaultService
    "ResourceShareInvitationAlreadyAcceptedException"
    Prelude.. Core.hasStatus 400

-- | The operation failed because the specified invitation was already
-- rejected.
_ResourceShareInvitationAlreadyRejectedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceShareInvitationAlreadyRejectedException =
  Core._MatchServiceError
    defaultService
    "ResourceShareInvitationAlreadyRejectedException"
    Prelude.. Core.hasStatus 400

-- | The operation failed because the specified
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
-- for an invitation was not found.
_ResourceShareInvitationArnNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceShareInvitationArnNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceShareInvitationArnNotFoundException"
    Prelude.. Core.hasStatus 400

-- | The operation failed because the specified invitation is past its
-- expiration date and time.
_ResourceShareInvitationExpiredException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceShareInvitationExpiredException =
  Core._MatchServiceError
    defaultService
    "ResourceShareInvitationExpiredException"
    Prelude.. Core.hasStatus 400

-- | The operation failed because it would exceed the limit for resource
-- shares for your account. To view the limits for your Amazon Web Services
-- account, see the
-- <https://console.aws.amazon.com/servicequotas/home/services/ram/quotas RAM page in the Service Quotas console>.
_ResourceShareLimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceShareLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ResourceShareLimitExceededException"
    Prelude.. Core.hasStatus 400

-- | The operation failed because the service could not respond to the
-- request due to an internal problem. Try again later.
_ServerInternalException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServerInternalException =
  Core._MatchServiceError
    defaultService
    "ServerInternalException"
    Prelude.. Core.hasStatus 500

-- | The operation failed because the service isn\'t available. Try again
-- later.
_ServiceUnavailableException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"
    Prelude.. Core.hasStatus 503

-- | The operation failed because it would exceed the limit for tags for your
-- Amazon Web Services account.
_TagLimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TagLimitExceededException =
  Core._MatchServiceError
    defaultService
    "TagLimitExceededException"
    Prelude.. Core.hasStatus 400

-- | The operation failed because the specified tag key is a reserved word
-- and can\'t be used.
_TagPolicyViolationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TagPolicyViolationException =
  Core._MatchServiceError
    defaultService
    "TagPolicyViolationException"
    Prelude.. Core.hasStatus 400

-- | The operation failed because it exceeded the rate at which you are
-- allowed to perform this operation. Please try again later.
_ThrottlingException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | The operation failed because a specified resource couldn\'t be found.
_UnknownResourceException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UnknownResourceException =
  Core._MatchServiceError
    defaultService
    "UnknownResourceException"
    Prelude.. Core.hasStatus 400

-- | There isn\'t an existing managed permission defined in RAM that has the
-- same IAM permissions as the resource-based policy attached to the
-- resource. You should first run PromotePermissionCreatedFromPolicy to
-- create that managed permission.
_UnmatchedPolicyPermissionException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UnmatchedPolicyPermissionException =
  Core._MatchServiceError
    defaultService
    "UnmatchedPolicyPermissionException"
    Prelude.. Core.hasStatus 400
