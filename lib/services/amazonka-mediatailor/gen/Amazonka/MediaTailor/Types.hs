{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MediaTailor.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaTailor.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _BadRequestException,

    -- * AccessType
    AccessType (..),

    -- * ChannelState
    ChannelState (..),

    -- * MessageType
    MessageType (..),

    -- * Mode
    Mode (..),

    -- * Operator
    Operator (..),

    -- * OriginManifestType
    OriginManifestType (..),

    -- * PlaybackMode
    PlaybackMode (..),

    -- * RelativePosition
    RelativePosition (..),

    -- * ScheduleEntryType
    ScheduleEntryType (..),

    -- * Type
    Type (..),

    -- * AccessConfiguration
    AccessConfiguration (..),
    newAccessConfiguration,
    accessConfiguration_accessType,
    accessConfiguration_secretsManagerAccessTokenConfiguration,

    -- * AdBreak
    AdBreak (..),
    newAdBreak,
    adBreak_spliceInsertMessage,
    adBreak_messageType,
    adBreak_slate,
    adBreak_offsetMillis,

    -- * AdMarkerPassthrough
    AdMarkerPassthrough (..),
    newAdMarkerPassthrough,
    adMarkerPassthrough_enabled,

    -- * Alert
    Alert (..),
    newAlert,
    alert_resourceArn,
    alert_alertCode,
    alert_lastModifiedTime,
    alert_relatedResourceArns,
    alert_alertMessage,

    -- * AvailMatchingCriteria
    AvailMatchingCriteria (..),
    newAvailMatchingCriteria,
    availMatchingCriteria_operator,
    availMatchingCriteria_dynamicVariable,

    -- * AvailSuppression
    AvailSuppression (..),
    newAvailSuppression,
    availSuppression_value,
    availSuppression_mode,

    -- * Bumper
    Bumper (..),
    newBumper,
    bumper_endUrl,
    bumper_startUrl,

    -- * CdnConfiguration
    CdnConfiguration (..),
    newCdnConfiguration,
    cdnConfiguration_adSegmentUrlPrefix,
    cdnConfiguration_contentSegmentUrlPrefix,

    -- * Channel
    Channel (..),
    newChannel,
    channel_creationTime,
    channel_lastModifiedTime,
    channel_fillerSlate,
    channel_tags,
    channel_channelState,
    channel_channelName,
    channel_outputs,
    channel_arn,
    channel_playbackMode,

    -- * DashConfiguration
    DashConfiguration (..),
    newDashConfiguration,
    dashConfiguration_manifestEndpointPrefix,
    dashConfiguration_originManifestType,
    dashConfiguration_mpdLocation,

    -- * DashConfigurationForPut
    DashConfigurationForPut (..),
    newDashConfigurationForPut,
    dashConfigurationForPut_originManifestType,
    dashConfigurationForPut_mpdLocation,

    -- * DashPlaylistSettings
    DashPlaylistSettings (..),
    newDashPlaylistSettings,
    dashPlaylistSettings_minBufferTimeSeconds,
    dashPlaylistSettings_minUpdatePeriodSeconds,
    dashPlaylistSettings_suggestedPresentationDelaySeconds,
    dashPlaylistSettings_manifestWindowSeconds,

    -- * DefaultSegmentDeliveryConfiguration
    DefaultSegmentDeliveryConfiguration (..),
    newDefaultSegmentDeliveryConfiguration,
    defaultSegmentDeliveryConfiguration_baseUrl,

    -- * HlsConfiguration
    HlsConfiguration (..),
    newHlsConfiguration,
    hlsConfiguration_manifestEndpointPrefix,

    -- * HlsPlaylistSettings
    HlsPlaylistSettings (..),
    newHlsPlaylistSettings,
    hlsPlaylistSettings_manifestWindowSeconds,

    -- * HttpConfiguration
    HttpConfiguration (..),
    newHttpConfiguration,
    httpConfiguration_baseUrl,

    -- * HttpPackageConfiguration
    HttpPackageConfiguration (..),
    newHttpPackageConfiguration,
    httpPackageConfiguration_path,
    httpPackageConfiguration_type,
    httpPackageConfiguration_sourceGroup,

    -- * LivePreRollConfiguration
    LivePreRollConfiguration (..),
    newLivePreRollConfiguration,
    livePreRollConfiguration_adDecisionServerUrl,
    livePreRollConfiguration_maxDurationSeconds,

    -- * LogConfiguration
    LogConfiguration (..),
    newLogConfiguration,
    logConfiguration_percentEnabled,

    -- * ManifestProcessingRules
    ManifestProcessingRules (..),
    newManifestProcessingRules,
    manifestProcessingRules_adMarkerPassthrough,

    -- * PlaybackConfiguration
    PlaybackConfiguration (..),
    newPlaybackConfiguration,
    playbackConfiguration_playbackEndpointPrefix,
    playbackConfiguration_hlsConfiguration,
    playbackConfiguration_personalizationThresholdSeconds,
    playbackConfiguration_playbackConfigurationArn,
    playbackConfiguration_availSuppression,
    playbackConfiguration_bumper,
    playbackConfiguration_adDecisionServerUrl,
    playbackConfiguration_videoContentSourceUrl,
    playbackConfiguration_dashConfiguration,
    playbackConfiguration_manifestProcessingRules,
    playbackConfiguration_logConfiguration,
    playbackConfiguration_livePreRollConfiguration,
    playbackConfiguration_name,
    playbackConfiguration_sessionInitializationEndpointPrefix,
    playbackConfiguration_configurationAliases,
    playbackConfiguration_transcodeProfileName,
    playbackConfiguration_tags,
    playbackConfiguration_slateAdUrl,
    playbackConfiguration_cdnConfiguration,

    -- * PrefetchConsumption
    PrefetchConsumption (..),
    newPrefetchConsumption,
    prefetchConsumption_startTime,
    prefetchConsumption_availMatchingCriteria,
    prefetchConsumption_endTime,

    -- * PrefetchRetrieval
    PrefetchRetrieval (..),
    newPrefetchRetrieval,
    prefetchRetrieval_startTime,
    prefetchRetrieval_dynamicVariables,
    prefetchRetrieval_endTime,

    -- * PrefetchSchedule
    PrefetchSchedule (..),
    newPrefetchSchedule,
    prefetchSchedule_streamId,
    prefetchSchedule_retrieval,
    prefetchSchedule_consumption,
    prefetchSchedule_arn,
    prefetchSchedule_playbackConfigurationName,
    prefetchSchedule_name,

    -- * RequestOutputItem
    RequestOutputItem (..),
    newRequestOutputItem,
    requestOutputItem_hlsPlaylistSettings,
    requestOutputItem_dashPlaylistSettings,
    requestOutputItem_manifestName,
    requestOutputItem_sourceGroup,

    -- * ResponseOutputItem
    ResponseOutputItem (..),
    newResponseOutputItem,
    responseOutputItem_hlsPlaylistSettings,
    responseOutputItem_dashPlaylistSettings,
    responseOutputItem_manifestName,
    responseOutputItem_playbackUrl,
    responseOutputItem_sourceGroup,

    -- * ScheduleAdBreak
    ScheduleAdBreak (..),
    newScheduleAdBreak,
    scheduleAdBreak_sourceLocationName,
    scheduleAdBreak_approximateDurationSeconds,
    scheduleAdBreak_vodSourceName,
    scheduleAdBreak_approximateStartTime,

    -- * ScheduleConfiguration
    ScheduleConfiguration (..),
    newScheduleConfiguration,
    scheduleConfiguration_transition,

    -- * ScheduleEntry
    ScheduleEntry (..),
    newScheduleEntry,
    scheduleEntry_scheduleAdBreaks,
    scheduleEntry_approximateDurationSeconds,
    scheduleEntry_approximateStartTime,
    scheduleEntry_scheduleEntryType,
    scheduleEntry_vodSourceName,
    scheduleEntry_channelName,
    scheduleEntry_sourceLocationName,
    scheduleEntry_arn,
    scheduleEntry_programName,

    -- * SecretsManagerAccessTokenConfiguration
    SecretsManagerAccessTokenConfiguration (..),
    newSecretsManagerAccessTokenConfiguration,
    secretsManagerAccessTokenConfiguration_headerName,
    secretsManagerAccessTokenConfiguration_secretStringKey,
    secretsManagerAccessTokenConfiguration_secretArn,

    -- * SlateSource
    SlateSource (..),
    newSlateSource,
    slateSource_sourceLocationName,
    slateSource_vodSourceName,

    -- * SourceLocation
    SourceLocation (..),
    newSourceLocation,
    sourceLocation_creationTime,
    sourceLocation_lastModifiedTime,
    sourceLocation_accessConfiguration,
    sourceLocation_defaultSegmentDeliveryConfiguration,
    sourceLocation_tags,
    sourceLocation_sourceLocationName,
    sourceLocation_httpConfiguration,
    sourceLocation_arn,

    -- * SpliceInsertMessage
    SpliceInsertMessage (..),
    newSpliceInsertMessage,
    spliceInsertMessage_availNum,
    spliceInsertMessage_uniqueProgramId,
    spliceInsertMessage_availsExpected,
    spliceInsertMessage_spliceEventId,

    -- * Transition
    Transition (..),
    newTransition,
    transition_scheduledStartTimeMillis,
    transition_relativeProgram,
    transition_type,
    transition_relativePosition,

    -- * VodSource
    VodSource (..),
    newVodSource,
    vodSource_creationTime,
    vodSource_lastModifiedTime,
    vodSource_tags,
    vodSource_vodSourceName,
    vodSource_sourceLocationName,
    vodSource_httpPackageConfigurations,
    vodSource_arn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaTailor.Types.AccessConfiguration
import Amazonka.MediaTailor.Types.AccessType
import Amazonka.MediaTailor.Types.AdBreak
import Amazonka.MediaTailor.Types.AdMarkerPassthrough
import Amazonka.MediaTailor.Types.Alert
import Amazonka.MediaTailor.Types.AvailMatchingCriteria
import Amazonka.MediaTailor.Types.AvailSuppression
import Amazonka.MediaTailor.Types.Bumper
import Amazonka.MediaTailor.Types.CdnConfiguration
import Amazonka.MediaTailor.Types.Channel
import Amazonka.MediaTailor.Types.ChannelState
import Amazonka.MediaTailor.Types.DashConfiguration
import Amazonka.MediaTailor.Types.DashConfigurationForPut
import Amazonka.MediaTailor.Types.DashPlaylistSettings
import Amazonka.MediaTailor.Types.DefaultSegmentDeliveryConfiguration
import Amazonka.MediaTailor.Types.HlsConfiguration
import Amazonka.MediaTailor.Types.HlsPlaylistSettings
import Amazonka.MediaTailor.Types.HttpConfiguration
import Amazonka.MediaTailor.Types.HttpPackageConfiguration
import Amazonka.MediaTailor.Types.LivePreRollConfiguration
import Amazonka.MediaTailor.Types.LogConfiguration
import Amazonka.MediaTailor.Types.ManifestProcessingRules
import Amazonka.MediaTailor.Types.MessageType
import Amazonka.MediaTailor.Types.Mode
import Amazonka.MediaTailor.Types.Operator
import Amazonka.MediaTailor.Types.OriginManifestType
import Amazonka.MediaTailor.Types.PlaybackConfiguration
import Amazonka.MediaTailor.Types.PlaybackMode
import Amazonka.MediaTailor.Types.PrefetchConsumption
import Amazonka.MediaTailor.Types.PrefetchRetrieval
import Amazonka.MediaTailor.Types.PrefetchSchedule
import Amazonka.MediaTailor.Types.RelativePosition
import Amazonka.MediaTailor.Types.RequestOutputItem
import Amazonka.MediaTailor.Types.ResponseOutputItem
import Amazonka.MediaTailor.Types.ScheduleAdBreak
import Amazonka.MediaTailor.Types.ScheduleConfiguration
import Amazonka.MediaTailor.Types.ScheduleEntry
import Amazonka.MediaTailor.Types.ScheduleEntryType
import Amazonka.MediaTailor.Types.SecretsManagerAccessTokenConfiguration
import Amazonka.MediaTailor.Types.SlateSource
import Amazonka.MediaTailor.Types.SourceLocation
import Amazonka.MediaTailor.Types.SpliceInsertMessage
import Amazonka.MediaTailor.Types.Transition
import Amazonka.MediaTailor.Types.Type
import Amazonka.MediaTailor.Types.VodSource
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2018-04-23@ of the Amazon MediaTailor SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "MediaTailor",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "api.mediatailor",
      Core._serviceSigningName = "mediatailor",
      Core._serviceVersion = "2018-04-23",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "MediaTailor",
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

-- | Invalid request parameters.
_BadRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Core.hasStatus 400
