{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSOAdmin.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSOAdmin.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ValidationException,
    _AccessDeniedException,
    _ConflictException,
    _ServiceQuotaExceededException,
    _ThrottlingException,
    _InternalServerException,
    _ResourceNotFoundException,

    -- * InstanceAccessControlAttributeConfigurationStatus
    InstanceAccessControlAttributeConfigurationStatus (..),

    -- * PrincipalType
    PrincipalType (..),

    -- * ProvisionTargetType
    ProvisionTargetType (..),

    -- * ProvisioningStatus
    ProvisioningStatus (..),

    -- * StatusValues
    StatusValues (..),

    -- * TargetType
    TargetType (..),

    -- * AccessControlAttribute
    AccessControlAttribute (..),
    newAccessControlAttribute,
    accessControlAttribute_key,
    accessControlAttribute_value,

    -- * AccessControlAttributeValue
    AccessControlAttributeValue (..),
    newAccessControlAttributeValue,
    accessControlAttributeValue_source,

    -- * AccountAssignment
    AccountAssignment (..),
    newAccountAssignment,
    accountAssignment_principalId,
    accountAssignment_principalType,
    accountAssignment_accountId,
    accountAssignment_permissionSetArn,

    -- * AccountAssignmentOperationStatus
    AccountAssignmentOperationStatus (..),
    newAccountAssignmentOperationStatus,
    accountAssignmentOperationStatus_requestId,
    accountAssignmentOperationStatus_status,
    accountAssignmentOperationStatus_failureReason,
    accountAssignmentOperationStatus_targetId,
    accountAssignmentOperationStatus_principalId,
    accountAssignmentOperationStatus_principalType,
    accountAssignmentOperationStatus_targetType,
    accountAssignmentOperationStatus_createdDate,
    accountAssignmentOperationStatus_permissionSetArn,

    -- * AccountAssignmentOperationStatusMetadata
    AccountAssignmentOperationStatusMetadata (..),
    newAccountAssignmentOperationStatusMetadata,
    accountAssignmentOperationStatusMetadata_requestId,
    accountAssignmentOperationStatusMetadata_status,
    accountAssignmentOperationStatusMetadata_createdDate,

    -- * AttachedManagedPolicy
    AttachedManagedPolicy (..),
    newAttachedManagedPolicy,
    attachedManagedPolicy_arn,
    attachedManagedPolicy_name,

    -- * InstanceAccessControlAttributeConfiguration
    InstanceAccessControlAttributeConfiguration (..),
    newInstanceAccessControlAttributeConfiguration,
    instanceAccessControlAttributeConfiguration_accessControlAttributes,

    -- * InstanceMetadata
    InstanceMetadata (..),
    newInstanceMetadata,
    instanceMetadata_identityStoreId,
    instanceMetadata_instanceArn,

    -- * OperationStatusFilter
    OperationStatusFilter (..),
    newOperationStatusFilter,
    operationStatusFilter_status,

    -- * PermissionSet
    PermissionSet (..),
    newPermissionSet,
    permissionSet_relayState,
    permissionSet_sessionDuration,
    permissionSet_createdDate,
    permissionSet_permissionSetArn,
    permissionSet_name,
    permissionSet_description,

    -- * PermissionSetProvisioningStatus
    PermissionSetProvisioningStatus (..),
    newPermissionSetProvisioningStatus,
    permissionSetProvisioningStatus_requestId,
    permissionSetProvisioningStatus_status,
    permissionSetProvisioningStatus_failureReason,
    permissionSetProvisioningStatus_accountId,
    permissionSetProvisioningStatus_createdDate,
    permissionSetProvisioningStatus_permissionSetArn,

    -- * PermissionSetProvisioningStatusMetadata
    PermissionSetProvisioningStatusMetadata (..),
    newPermissionSetProvisioningStatusMetadata,
    permissionSetProvisioningStatusMetadata_requestId,
    permissionSetProvisioningStatusMetadata_status,
    permissionSetProvisioningStatusMetadata_createdDate,

    -- * Tag
    Tag (..),
    newTag,
    tag_value,
    tag_key,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSOAdmin.Types.AccessControlAttribute
import Network.AWS.SSOAdmin.Types.AccessControlAttributeValue
import Network.AWS.SSOAdmin.Types.AccountAssignment
import Network.AWS.SSOAdmin.Types.AccountAssignmentOperationStatus
import Network.AWS.SSOAdmin.Types.AccountAssignmentOperationStatusMetadata
import Network.AWS.SSOAdmin.Types.AttachedManagedPolicy
import Network.AWS.SSOAdmin.Types.InstanceAccessControlAttributeConfiguration
import Network.AWS.SSOAdmin.Types.InstanceAccessControlAttributeConfigurationStatus
import Network.AWS.SSOAdmin.Types.InstanceMetadata
import Network.AWS.SSOAdmin.Types.OperationStatusFilter
import Network.AWS.SSOAdmin.Types.PermissionSet
import Network.AWS.SSOAdmin.Types.PermissionSetProvisioningStatus
import Network.AWS.SSOAdmin.Types.PermissionSetProvisioningStatusMetadata
import Network.AWS.SSOAdmin.Types.PrincipalType
import Network.AWS.SSOAdmin.Types.ProvisionTargetType
import Network.AWS.SSOAdmin.Types.ProvisioningStatus
import Network.AWS.SSOAdmin.Types.StatusValues
import Network.AWS.SSOAdmin.Types.Tag
import Network.AWS.SSOAdmin.Types.TargetType
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2020-07-20@ of the Amazon Single Sign-On Admin SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "SSOAdmin",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "sso",
      Core._serviceSigningName = "sso",
      Core._serviceVersion = "2020-07-20",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "SSOAdmin",
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

-- | The request failed because it contains a syntax error.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"

-- | You do not have sufficient access to perform this action.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | Occurs when a conflict with a previous successful write is detected.
-- This generally occurs when the previous write did not have time to
-- propagate to the host serving the current request. A retry (with
-- appropriate backoff logic) is the recommended response to this
-- exception.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"

-- | Indicates that the principal has crossed the permitted number of
-- resources that can be created.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"

-- | Indicates that the principal has crossed the throttling limits of the
-- API operations.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"

-- | The request processing has failed because of an unknown error,
-- exception, or failure with an internal server.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"

-- | Indicates that a requested resource is not found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
