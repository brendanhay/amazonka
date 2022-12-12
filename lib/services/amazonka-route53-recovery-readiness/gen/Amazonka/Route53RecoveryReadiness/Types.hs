{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Route53RecoveryReadiness.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53RecoveryReadiness.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ConflictException,
    _InternalServerException,
    _ResourceNotFoundException,
    _ThrottlingException,
    _ValidationException,

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
    dNSTargetResource_domainName,
    dNSTargetResource_hostedZoneArn,
    dNSTargetResource_recordSetId,
    dNSTargetResource_recordType,
    dNSTargetResource_targetResource,

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
    resource_componentId,
    resource_dnsTargetResource,
    resource_readinessScopes,
    resource_resourceArn,

    -- * ResourceResult
    ResourceResult (..),
    newResourceResult,
    resourceResult_componentId,
    resourceResult_resourceArn,
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
    targetResource_nLBResource,
    targetResource_r53Resource,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53RecoveryReadiness.Types.CellOutput
import Amazonka.Route53RecoveryReadiness.Types.DNSTargetResource
import Amazonka.Route53RecoveryReadiness.Types.ListRulesOutput
import Amazonka.Route53RecoveryReadiness.Types.Message
import Amazonka.Route53RecoveryReadiness.Types.NLBResource
import Amazonka.Route53RecoveryReadiness.Types.R53ResourceRecord
import Amazonka.Route53RecoveryReadiness.Types.Readiness
import Amazonka.Route53RecoveryReadiness.Types.ReadinessCheckOutput
import Amazonka.Route53RecoveryReadiness.Types.ReadinessCheckSummary
import Amazonka.Route53RecoveryReadiness.Types.Recommendation
import Amazonka.Route53RecoveryReadiness.Types.RecoveryGroupOutput
import Amazonka.Route53RecoveryReadiness.Types.Resource
import Amazonka.Route53RecoveryReadiness.Types.ResourceResult
import Amazonka.Route53RecoveryReadiness.Types.ResourceSetOutput
import Amazonka.Route53RecoveryReadiness.Types.RuleResult
import Amazonka.Route53RecoveryReadiness.Types.TargetResource
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2019-12-02@ of the Amazon Route53 Recovery Readiness SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev =
        "Route53RecoveryReadiness",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "route53-recovery-readiness",
      Core.signingName = "route53-recovery-readiness",
      Core.version = "2019-12-02",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error =
        Core.parseJSONError "Route53RecoveryReadiness",
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

-- | Request was denied due to request throttling.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | The input fails to satisfy the constraints specified by an AWS service.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
