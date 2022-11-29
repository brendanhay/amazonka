{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ConnectCampaigns.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectCampaigns.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidCampaignStateException,
    _InvalidStateException,
    _AccessDeniedException,
    _InternalServerException,
    _ServiceQuotaExceededException,
    _ResourceNotFoundException,
    _ConflictException,
    _ThrottlingException,
    _ValidationException,

    -- * CampaignState
    CampaignState (..),

    -- * EncryptionType
    EncryptionType (..),

    -- * FailureCode
    FailureCode (..),

    -- * GetCampaignStateBatchFailureCode
    GetCampaignStateBatchFailureCode (..),

    -- * InstanceIdFilterOperator
    InstanceIdFilterOperator (..),

    -- * InstanceOnboardingJobFailureCode
    InstanceOnboardingJobFailureCode (..),

    -- * InstanceOnboardingJobStatusCode
    InstanceOnboardingJobStatusCode (..),

    -- * AnswerMachineDetectionConfig
    AnswerMachineDetectionConfig (..),
    newAnswerMachineDetectionConfig,
    answerMachineDetectionConfig_enableAnswerMachineDetection,

    -- * Campaign
    Campaign (..),
    newCampaign,
    campaign_tags,
    campaign_arn,
    campaign_connectInstanceId,
    campaign_dialerConfig,
    campaign_id,
    campaign_name,
    campaign_outboundCallConfig,

    -- * CampaignFilters
    CampaignFilters (..),
    newCampaignFilters,
    campaignFilters_instanceIdFilter,

    -- * CampaignSummary
    CampaignSummary (..),
    newCampaignSummary,
    campaignSummary_arn,
    campaignSummary_connectInstanceId,
    campaignSummary_id,
    campaignSummary_name,

    -- * DialRequest
    DialRequest (..),
    newDialRequest,
    dialRequest_attributes,
    dialRequest_clientToken,
    dialRequest_expirationTime,
    dialRequest_phoneNumber,

    -- * DialerConfig
    DialerConfig (..),
    newDialerConfig,
    dialerConfig_predictiveDialerConfig,
    dialerConfig_progressiveDialerConfig,

    -- * EncryptionConfig
    EncryptionConfig (..),
    newEncryptionConfig,
    encryptionConfig_encryptionType,
    encryptionConfig_keyArn,
    encryptionConfig_enabled,

    -- * FailedCampaignStateResponse
    FailedCampaignStateResponse (..),
    newFailedCampaignStateResponse,
    failedCampaignStateResponse_failureCode,
    failedCampaignStateResponse_campaignId,

    -- * FailedRequest
    FailedRequest (..),
    newFailedRequest,
    failedRequest_clientToken,
    failedRequest_failureCode,
    failedRequest_id,

    -- * InstanceConfig
    InstanceConfig (..),
    newInstanceConfig,
    instanceConfig_connectInstanceId,
    instanceConfig_encryptionConfig,
    instanceConfig_serviceLinkedRoleArn,

    -- * InstanceIdFilter
    InstanceIdFilter (..),
    newInstanceIdFilter,
    instanceIdFilter_operator,
    instanceIdFilter_value,

    -- * InstanceOnboardingJobStatus
    InstanceOnboardingJobStatus (..),
    newInstanceOnboardingJobStatus,
    instanceOnboardingJobStatus_failureCode,
    instanceOnboardingJobStatus_connectInstanceId,
    instanceOnboardingJobStatus_status,

    -- * OutboundCallConfig
    OutboundCallConfig (..),
    newOutboundCallConfig,
    outboundCallConfig_answerMachineDetectionConfig,
    outboundCallConfig_connectSourcePhoneNumber,
    outboundCallConfig_connectContactFlowId,
    outboundCallConfig_connectQueueId,

    -- * PredictiveDialerConfig
    PredictiveDialerConfig (..),
    newPredictiveDialerConfig,
    predictiveDialerConfig_bandwidthAllocation,

    -- * ProgressiveDialerConfig
    ProgressiveDialerConfig (..),
    newProgressiveDialerConfig,
    progressiveDialerConfig_bandwidthAllocation,

    -- * SuccessfulCampaignStateResponse
    SuccessfulCampaignStateResponse (..),
    newSuccessfulCampaignStateResponse,
    successfulCampaignStateResponse_campaignId,
    successfulCampaignStateResponse_state,

    -- * SuccessfulRequest
    SuccessfulRequest (..),
    newSuccessfulRequest,
    successfulRequest_clientToken,
    successfulRequest_id,
  )
where

import Amazonka.ConnectCampaigns.Types.AnswerMachineDetectionConfig
import Amazonka.ConnectCampaigns.Types.Campaign
import Amazonka.ConnectCampaigns.Types.CampaignFilters
import Amazonka.ConnectCampaigns.Types.CampaignState
import Amazonka.ConnectCampaigns.Types.CampaignSummary
import Amazonka.ConnectCampaigns.Types.DialRequest
import Amazonka.ConnectCampaigns.Types.DialerConfig
import Amazonka.ConnectCampaigns.Types.EncryptionConfig
import Amazonka.ConnectCampaigns.Types.EncryptionType
import Amazonka.ConnectCampaigns.Types.FailedCampaignStateResponse
import Amazonka.ConnectCampaigns.Types.FailedRequest
import Amazonka.ConnectCampaigns.Types.FailureCode
import Amazonka.ConnectCampaigns.Types.GetCampaignStateBatchFailureCode
import Amazonka.ConnectCampaigns.Types.InstanceConfig
import Amazonka.ConnectCampaigns.Types.InstanceIdFilter
import Amazonka.ConnectCampaigns.Types.InstanceIdFilterOperator
import Amazonka.ConnectCampaigns.Types.InstanceOnboardingJobFailureCode
import Amazonka.ConnectCampaigns.Types.InstanceOnboardingJobStatus
import Amazonka.ConnectCampaigns.Types.InstanceOnboardingJobStatusCode
import Amazonka.ConnectCampaigns.Types.OutboundCallConfig
import Amazonka.ConnectCampaigns.Types.PredictiveDialerConfig
import Amazonka.ConnectCampaigns.Types.ProgressiveDialerConfig
import Amazonka.ConnectCampaigns.Types.SuccessfulCampaignStateResponse
import Amazonka.ConnectCampaigns.Types.SuccessfulRequest
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2021-01-30@ of the Amazon ConnectCampaignService SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "ConnectCampaigns",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "connect-campaigns",
      Core.signingName = "connect-campaigns",
      Core.version = "2021-01-30",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "ConnectCampaigns",
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

-- | The request could not be processed because of conflict in the current
-- state of the campaign.
_InvalidCampaignStateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidCampaignStateException =
  Core._MatchServiceError
    defaultService
    "InvalidCampaignStateException"
    Prelude.. Core.hasStatus 409

-- | The request could not be processed because of conflict in the current
-- state.
_InvalidStateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidStateException =
  Core._MatchServiceError
    defaultService
    "InvalidStateException"
    Prelude.. Core.hasStatus 409

-- | You do not have sufficient access to perform this action.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | Request processing failed because of an error or failure with the
-- service.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | Request would cause a service quota to be exceeded.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | The specified resource was not found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The request could not be processed because of conflict in the current
-- state of the resource.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | The request was denied due to request throttling.
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
