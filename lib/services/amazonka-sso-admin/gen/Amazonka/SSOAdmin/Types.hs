{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SSOAdmin.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSOAdmin.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _InternalServerException,
    _ServiceQuotaExceededException,
    _ResourceNotFoundException,
    _ConflictException,
    _ThrottlingException,
    _ValidationException,

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
    accountAssignment_accountId,
    accountAssignment_permissionSetArn,
    accountAssignment_principalType,

    -- * AccountAssignmentOperationStatus
    AccountAssignmentOperationStatus (..),
    newAccountAssignmentOperationStatus,
    accountAssignmentOperationStatus_principalId,
    accountAssignmentOperationStatus_targetId,
    accountAssignmentOperationStatus_requestId,
    accountAssignmentOperationStatus_status,
    accountAssignmentOperationStatus_targetType,
    accountAssignmentOperationStatus_permissionSetArn,
    accountAssignmentOperationStatus_principalType,
    accountAssignmentOperationStatus_createdDate,
    accountAssignmentOperationStatus_failureReason,

    -- * AccountAssignmentOperationStatusMetadata
    AccountAssignmentOperationStatusMetadata (..),
    newAccountAssignmentOperationStatusMetadata,
    accountAssignmentOperationStatusMetadata_requestId,
    accountAssignmentOperationStatusMetadata_status,
    accountAssignmentOperationStatusMetadata_createdDate,

    -- * AttachedManagedPolicy
    AttachedManagedPolicy (..),
    newAttachedManagedPolicy,
    attachedManagedPolicy_name,
    attachedManagedPolicy_arn,

    -- * CustomerManagedPolicyReference
    CustomerManagedPolicyReference (..),
    newCustomerManagedPolicyReference,
    customerManagedPolicyReference_path,
    customerManagedPolicyReference_name,

    -- * InstanceAccessControlAttributeConfiguration
    InstanceAccessControlAttributeConfiguration (..),
    newInstanceAccessControlAttributeConfiguration,
    instanceAccessControlAttributeConfiguration_accessControlAttributes,

    -- * InstanceMetadata
    InstanceMetadata (..),
    newInstanceMetadata,
    instanceMetadata_instanceArn,
    instanceMetadata_identityStoreId,

    -- * OperationStatusFilter
    OperationStatusFilter (..),
    newOperationStatusFilter,
    operationStatusFilter_status,

    -- * PermissionSet
    PermissionSet (..),
    newPermissionSet,
    permissionSet_name,
    permissionSet_description,
    permissionSet_sessionDuration,
    permissionSet_relayState,
    permissionSet_permissionSetArn,
    permissionSet_createdDate,

    -- * PermissionSetProvisioningStatus
    PermissionSetProvisioningStatus (..),
    newPermissionSetProvisioningStatus,
    permissionSetProvisioningStatus_requestId,
    permissionSetProvisioningStatus_status,
    permissionSetProvisioningStatus_accountId,
    permissionSetProvisioningStatus_permissionSetArn,
    permissionSetProvisioningStatus_createdDate,
    permissionSetProvisioningStatus_failureReason,

    -- * PermissionSetProvisioningStatusMetadata
    PermissionSetProvisioningStatusMetadata (..),
    newPermissionSetProvisioningStatusMetadata,
    permissionSetProvisioningStatusMetadata_requestId,
    permissionSetProvisioningStatusMetadata_status,
    permissionSetProvisioningStatusMetadata_createdDate,

    -- * PermissionsBoundary
    PermissionsBoundary (..),
    newPermissionsBoundary,
    permissionsBoundary_managedPolicyArn,
    permissionsBoundary_customerManagedPolicyReference,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSOAdmin.Types.AccessControlAttribute
import Amazonka.SSOAdmin.Types.AccessControlAttributeValue
import Amazonka.SSOAdmin.Types.AccountAssignment
import Amazonka.SSOAdmin.Types.AccountAssignmentOperationStatus
import Amazonka.SSOAdmin.Types.AccountAssignmentOperationStatusMetadata
import Amazonka.SSOAdmin.Types.AttachedManagedPolicy
import Amazonka.SSOAdmin.Types.CustomerManagedPolicyReference
import Amazonka.SSOAdmin.Types.InstanceAccessControlAttributeConfiguration
import Amazonka.SSOAdmin.Types.InstanceAccessControlAttributeConfigurationStatus
import Amazonka.SSOAdmin.Types.InstanceMetadata
import Amazonka.SSOAdmin.Types.OperationStatusFilter
import Amazonka.SSOAdmin.Types.PermissionSet
import Amazonka.SSOAdmin.Types.PermissionSetProvisioningStatus
import Amazonka.SSOAdmin.Types.PermissionSetProvisioningStatusMetadata
import Amazonka.SSOAdmin.Types.PermissionsBoundary
import Amazonka.SSOAdmin.Types.PrincipalType
import Amazonka.SSOAdmin.Types.ProvisionTargetType
import Amazonka.SSOAdmin.Types.ProvisioningStatus
import Amazonka.SSOAdmin.Types.StatusValues
import Amazonka.SSOAdmin.Types.Tag
import Amazonka.SSOAdmin.Types.TargetType
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2020-07-20@ of the Amazon Single Sign-On Admin SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "SSOAdmin",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "sso",
      Core.signingName = "sso",
      Core.version = "2020-07-20",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "SSOAdmin",
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
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | You do not have sufficient access to perform this action.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | The request processing has failed because of an unknown error,
-- exception, or failure with an internal server.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"

-- | Indicates that the principal has crossed the permitted number of
-- resources that can be created.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"

-- | Indicates that a requested resource is not found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

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

-- | Indicates that the principal has crossed the throttling limits of the
-- API operations.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"

-- | The request failed because it contains a syntax error.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
