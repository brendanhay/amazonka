{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53RecoveryReadiness.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53RecoveryReadiness.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ValidationException,
    _AccessDeniedException,
    _ConflictException,
    _ThrottlingException,
    _InternalServerException,
    _ResourceNotFoundException,

    -- * Readiness
    Readiness (..),

    -- * CellOutput
    CellOutput (..),
    newCellOutput,
    cellOutput_tags,
    cellOutput_parentReadinessScopes,
    cellOutput_cellArn,
    cellOutput_cellName,
    cellOutput_cells,

    -- * DNSTargetResource
    DNSTargetResource (..),
    newDNSTargetResource,
    dNSTargetResource_hostedZoneArn,
    dNSTargetResource_recordType,
    dNSTargetResource_targetResource,
    dNSTargetResource_domainName,
    dNSTargetResource_recordSetId,

    -- * ListRulesOutput
    ListRulesOutput (..),
    newListRulesOutput,
    listRulesOutput_ruleDescription,
    listRulesOutput_ruleId,
    listRulesOutput_resourceType,

    -- * Message
    Message (..),
    newMessage,
    message_messageText,

    -- * NLBResource
    NLBResource (..),
    newNLBResource,
    nLBResource_arn,

    -- * R53ResourceRecord
    R53ResourceRecord (..),
    newR53ResourceRecord,
    r53ResourceRecord_domainName,
    r53ResourceRecord_recordSetId,

    -- * ReadinessCheckOutput
    ReadinessCheckOutput (..),
    newReadinessCheckOutput,
    readinessCheckOutput_readinessCheckName,
    readinessCheckOutput_tags,
    readinessCheckOutput_readinessCheckArn,
    readinessCheckOutput_resourceSet,

    -- * ReadinessCheckSummary
    ReadinessCheckSummary (..),
    newReadinessCheckSummary,
    readinessCheckSummary_readiness,
    readinessCheckSummary_readinessCheckName,

    -- * Recommendation
    Recommendation (..),
    newRecommendation,
    recommendation_recommendationText,

    -- * RecoveryGroupOutput
    RecoveryGroupOutput (..),
    newRecoveryGroupOutput,
    recoveryGroupOutput_tags,
    recoveryGroupOutput_recoveryGroupArn,
    recoveryGroupOutput_recoveryGroupName,
    recoveryGroupOutput_cells,

    -- * Resource
    Resource (..),
    newResource,
    resource_readinessScopes,
    resource_resourceArn,
    resource_componentId,
    resource_dnsTargetResource,

    -- * ResourceResult
    ResourceResult (..),
    newResourceResult,
    resourceResult_resourceArn,
    resourceResult_componentId,
    resourceResult_readiness,
    resourceResult_lastCheckedTimestamp,

    -- * ResourceSetOutput
    ResourceSetOutput (..),
    newResourceSetOutput,
    resourceSetOutput_tags,
    resourceSetOutput_resourceSetType,
    resourceSetOutput_resourceSetName,
    resourceSetOutput_resourceSetArn,
    resourceSetOutput_resources,

    -- * RuleResult
    RuleResult (..),
    newRuleResult,
    ruleResult_messages,
    ruleResult_readiness,
    ruleResult_ruleId,
    ruleResult_lastCheckedTimestamp,

    -- * TargetResource
    TargetResource (..),
    newTargetResource,
    targetResource_r53Resource,
    targetResource_nLBResource,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Route53RecoveryReadiness.Types.CellOutput
import Network.AWS.Route53RecoveryReadiness.Types.DNSTargetResource
import Network.AWS.Route53RecoveryReadiness.Types.ListRulesOutput
import Network.AWS.Route53RecoveryReadiness.Types.Message
import Network.AWS.Route53RecoveryReadiness.Types.NLBResource
import Network.AWS.Route53RecoveryReadiness.Types.R53ResourceRecord
import Network.AWS.Route53RecoveryReadiness.Types.Readiness
import Network.AWS.Route53RecoveryReadiness.Types.ReadinessCheckOutput
import Network.AWS.Route53RecoveryReadiness.Types.ReadinessCheckSummary
import Network.AWS.Route53RecoveryReadiness.Types.Recommendation
import Network.AWS.Route53RecoveryReadiness.Types.RecoveryGroupOutput
import Network.AWS.Route53RecoveryReadiness.Types.Resource
import Network.AWS.Route53RecoveryReadiness.Types.ResourceResult
import Network.AWS.Route53RecoveryReadiness.Types.ResourceSetOutput
import Network.AWS.Route53RecoveryReadiness.Types.RuleResult
import Network.AWS.Route53RecoveryReadiness.Types.TargetResource
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2019-12-02@ of the Amazon Route53 Recovery Readiness SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev =
        "Route53RecoveryReadiness",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix =
        "route53-recovery-readiness",
      Core._serviceSigningName =
        "route53-recovery-readiness",
      Core._serviceVersion = "2019-12-02",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "Route53RecoveryReadiness",
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

-- | The input fails to satisfy the constraints specified by an AWS service.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400

-- | User does not have sufficient access to perform this action.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | Updating or deleting a resource can cause an inconsistent state.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | Request was denied due to request throttling.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | An unexpected error occurred.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The requested resource does not exist.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404
