{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SSMContacts.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMContacts.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ConflictException,
    _DataEncryptionException,
    _InternalServerException,
    _ResourceNotFoundException,
    _ServiceQuotaExceededException,
    _ThrottlingException,
    _ValidationException,

    -- * AcceptCodeValidation
    AcceptCodeValidation (..),

    -- * AcceptType
    AcceptType (..),

    -- * ActivationStatus
    ActivationStatus (..),

    -- * ChannelType
    ChannelType (..),

    -- * ContactType
    ContactType (..),

    -- * ReceiptType
    ReceiptType (..),

    -- * ChannelTargetInfo
    ChannelTargetInfo (..),
    newChannelTargetInfo,
    channelTargetInfo_retryIntervalInMinutes,
    channelTargetInfo_contactChannelId,

    -- * Contact
    Contact (..),
    newContact,
    contact_displayName,
    contact_contactArn,
    contact_alias,
    contact_type,

    -- * ContactChannel
    ContactChannel (..),
    newContactChannel,
    contactChannel_type,
    contactChannel_contactChannelArn,
    contactChannel_contactArn,
    contactChannel_name,
    contactChannel_deliveryAddress,
    contactChannel_activationStatus,

    -- * ContactChannelAddress
    ContactChannelAddress (..),
    newContactChannelAddress,
    contactChannelAddress_simpleAddress,

    -- * ContactTargetInfo
    ContactTargetInfo (..),
    newContactTargetInfo,
    contactTargetInfo_contactId,
    contactTargetInfo_isEssential,

    -- * Engagement
    Engagement (..),
    newEngagement,
    engagement_incidentId,
    engagement_startTime,
    engagement_stopTime,
    engagement_engagementArn,
    engagement_contactArn,
    engagement_sender,

    -- * Page
    Page (..),
    newPage,
    page_deliveryTime,
    page_incidentId,
    page_readTime,
    page_sentTime,
    page_pageArn,
    page_engagementArn,
    page_contactArn,
    page_sender,

    -- * Plan
    Plan (..),
    newPlan,
    plan_stages,

    -- * Receipt
    Receipt (..),
    newReceipt,
    receipt_contactChannelArn,
    receipt_receiptInfo,
    receipt_receiptType,
    receipt_receiptTime,

    -- * Stage
    Stage (..),
    newStage,
    stage_durationInMinutes,
    stage_targets,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * Target
    Target (..),
    newTarget,
    target_channelTargetInfo,
    target_contactTargetInfo,

    -- * TimeRange
    TimeRange (..),
    newTimeRange,
    timeRange_endTime,
    timeRange_startTime,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSMContacts.Types.AcceptCodeValidation
import Amazonka.SSMContacts.Types.AcceptType
import Amazonka.SSMContacts.Types.ActivationStatus
import Amazonka.SSMContacts.Types.ChannelTargetInfo
import Amazonka.SSMContacts.Types.ChannelType
import Amazonka.SSMContacts.Types.Contact
import Amazonka.SSMContacts.Types.ContactChannel
import Amazonka.SSMContacts.Types.ContactChannelAddress
import Amazonka.SSMContacts.Types.ContactTargetInfo
import Amazonka.SSMContacts.Types.ContactType
import Amazonka.SSMContacts.Types.Engagement
import Amazonka.SSMContacts.Types.Page
import Amazonka.SSMContacts.Types.Plan
import Amazonka.SSMContacts.Types.Receipt
import Amazonka.SSMContacts.Types.ReceiptType
import Amazonka.SSMContacts.Types.Stage
import Amazonka.SSMContacts.Types.Tag
import Amazonka.SSMContacts.Types.Target
import Amazonka.SSMContacts.Types.TimeRange
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2021-05-03@ of the Amazon Systems Manager Incident Manager Contacts SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "SSMContacts",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "ssm-contacts",
      Core.signingName = "ssm-contacts",
      Core.version = "2021-05-03",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "SSMContacts",
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

-- | You don\'t have sufficient access to perform this operation.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | Updating or deleting a resource causes an inconsistent state.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"

-- | The operation failed to due an encryption key error.
_DataEncryptionException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DataEncryptionException =
  Core._MatchServiceError
    defaultService
    "DataEncryptionException"

-- | Unexpected error occurred while processing the request.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"

-- | Request references a resource that doesn\'t exist.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | Request would cause a service quota to be exceeded.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"

-- | The request was denied due to request throttling.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"

-- | The input fails to satisfy the constraints specified by an Amazon Web
-- Services service.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
