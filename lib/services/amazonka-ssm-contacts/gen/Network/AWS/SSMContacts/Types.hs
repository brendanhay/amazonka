{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSMContacts.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSMContacts.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ValidationException,
    _AccessDeniedException,
    _DataEncryptionException,
    _ConflictException,
    _ServiceQuotaExceededException,
    _ThrottlingException,
    _InternalServerException,
    _ResourceNotFoundException,

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
    engagement_startTime,
    engagement_stopTime,
    engagement_incidentId,
    engagement_engagementArn,
    engagement_contactArn,
    engagement_sender,

    -- * Page
    Page (..),
    newPage,
    page_readTime,
    page_deliveryTime,
    page_incidentId,
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
    receipt_receiptInfo,
    receipt_contactChannelArn,
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
    tag_value,
    tag_key,

    -- * Target
    Target (..),
    newTarget,
    target_channelTargetInfo,
    target_contactTargetInfo,

    -- * TimeRange
    TimeRange (..),
    newTimeRange,
    timeRange_startTime,
    timeRange_endTime,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSMContacts.Types.AcceptCodeValidation
import Network.AWS.SSMContacts.Types.AcceptType
import Network.AWS.SSMContacts.Types.ActivationStatus
import Network.AWS.SSMContacts.Types.ChannelTargetInfo
import Network.AWS.SSMContacts.Types.ChannelType
import Network.AWS.SSMContacts.Types.Contact
import Network.AWS.SSMContacts.Types.ContactChannel
import Network.AWS.SSMContacts.Types.ContactChannelAddress
import Network.AWS.SSMContacts.Types.ContactTargetInfo
import Network.AWS.SSMContacts.Types.ContactType
import Network.AWS.SSMContacts.Types.Engagement
import Network.AWS.SSMContacts.Types.Page
import Network.AWS.SSMContacts.Types.Plan
import Network.AWS.SSMContacts.Types.Receipt
import Network.AWS.SSMContacts.Types.ReceiptType
import Network.AWS.SSMContacts.Types.Stage
import Network.AWS.SSMContacts.Types.Tag
import Network.AWS.SSMContacts.Types.Target
import Network.AWS.SSMContacts.Types.TimeRange
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2021-05-03@ of the Amazon Systems Manager Incident Manager Contacts SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "SSMContacts",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "ssm-contacts",
      Core._serviceSigningName = "ssm-contacts",
      Core._serviceVersion = "2021-05-03",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "SSMContacts",
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

-- | The input fails to satisfy the constraints specified by an Amazon Web
-- Services service.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"

-- | You don\'t have sufficient access to perform this operation.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | The operation failed to due an encryption key error.
_DataEncryptionException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DataEncryptionException =
  Core._MatchServiceError
    defaultService
    "DataEncryptionException"

-- | Updating or deleting a resource causes an inconsistent state.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"

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
