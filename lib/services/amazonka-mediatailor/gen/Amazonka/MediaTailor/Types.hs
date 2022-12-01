{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MediaTailor.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
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

    -- * Tier
    Tier (..),

    -- * Type
    Type (..),

    -- * AccessConfiguration
    AccessConfiguration (..),
    newAccessConfiguration,
    accessConfiguration_secretsManagerAccessTokenConfiguration,
    accessConfiguration_accessType,

    -- * AdBreak
    AdBreak (..),
    newAdBreak,
    adBreak_messageType,
    adBreak_offsetMillis,
    adBreak_spliceInsertMessage,
    adBreak_slate,
    adBreak_timeSignalMessage,

    -- * AdMarkerPassthrough
    AdMarkerPassthrough (..),
    newAdMarkerPassthrough,
    adMarkerPassthrough_enabled,

    -- * Alert
    Alert (..),
    newAlert,
    alert_alertCode,
    alert_alertMessage,
    alert_lastModifiedTime,
    alert_relatedResourceArns,
    alert_resourceArn,

    -- * AvailMatchingCriteria
    AvailMatchingCriteria (..),
    newAvailMatchingCriteria,
    availMatchingCriteria_dynamicVariable,
    availMatchingCriteria_operator,

    -- * AvailSuppression
    AvailSuppression (..),
    newAvailSuppression,
    availSuppression_mode,
    availSuppression_value,

    -- * Bumper
    Bumper (..),
    newBumper,
    bumper_startUrl,
    bumper_endUrl,

    -- * CdnConfiguration
    CdnConfiguration (..),
    newCdnConfiguration,
    cdnConfiguration_adSegmentUrlPrefix,
    cdnConfiguration_contentSegmentUrlPrefix,

    -- * Channel
    Channel (..),
    newChannel,
    channel_tags,
    channel_fillerSlate,
    channel_lastModifiedTime,
    channel_creationTime,
    channel_arn,
    channel_channelName,
    channel_channelState,
    channel_outputs,
    channel_playbackMode,
    channel_tier,

    -- * DashConfiguration
    DashConfiguration (..),
    newDashConfiguration,
    dashConfiguration_manifestEndpointPrefix,
    dashConfiguration_mpdLocation,
    dashConfiguration_originManifestType,

    -- * DashConfigurationForPut
    DashConfigurationForPut (..),
    newDashConfigurationForPut,
    dashConfigurationForPut_mpdLocation,
    dashConfigurationForPut_originManifestType,

    -- * DashPlaylistSettings
    DashPlaylistSettings (..),
    newDashPlaylistSettings,
    dashPlaylistSettings_suggestedPresentationDelaySeconds,
    dashPlaylistSettings_manifestWindowSeconds,
    dashPlaylistSettings_minUpdatePeriodSeconds,
    dashPlaylistSettings_minBufferTimeSeconds,

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
    httpPackageConfiguration_sourceGroup,
    httpPackageConfiguration_type,

    -- * LivePreRollConfiguration
    LivePreRollConfiguration (..),
    newLivePreRollConfiguration,
    livePreRollConfiguration_adDecisionServerUrl,
    livePreRollConfiguration_maxDurationSeconds,

    -- * LiveSource
    LiveSource (..),
    newLiveSource,
    liveSource_tags,
    liveSource_lastModifiedTime,
    liveSource_creationTime,
    liveSource_arn,
    liveSource_httpPackageConfigurations,
    liveSource_liveSourceName,
    liveSource_sourceLocationName,

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
    playbackConfiguration_tags,
    playbackConfiguration_name,
    playbackConfiguration_configurationAliases,
    playbackConfiguration_logConfiguration,
    playbackConfiguration_sessionInitializationEndpointPrefix,
    playbackConfiguration_cdnConfiguration,
    playbackConfiguration_bumper,
    playbackConfiguration_manifestProcessingRules,
    playbackConfiguration_livePreRollConfiguration,
    playbackConfiguration_dashConfiguration,
    playbackConfiguration_adDecisionServerUrl,
    playbackConfiguration_transcodeProfileName,
    playbackConfiguration_videoContentSourceUrl,
    playbackConfiguration_playbackConfigurationArn,
    playbackConfiguration_hlsConfiguration,
    playbackConfiguration_slateAdUrl,
    playbackConfiguration_availSuppression,
    playbackConfiguration_playbackEndpointPrefix,
    playbackConfiguration_personalizationThresholdSeconds,

    -- * PrefetchConsumption
    PrefetchConsumption (..),
    newPrefetchConsumption,
    prefetchConsumption_availMatchingCriteria,
    prefetchConsumption_startTime,
    prefetchConsumption_endTime,

    -- * PrefetchRetrieval
    PrefetchRetrieval (..),
    newPrefetchRetrieval,
    prefetchRetrieval_dynamicVariables,
    prefetchRetrieval_startTime,
    prefetchRetrieval_endTime,

    -- * PrefetchSchedule
    PrefetchSchedule (..),
    newPrefetchSchedule,
    prefetchSchedule_streamId,
    prefetchSchedule_arn,
    prefetchSchedule_consumption,
    prefetchSchedule_name,
    prefetchSchedule_playbackConfigurationName,
    prefetchSchedule_retrieval,

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
    scheduleAdBreak_approximateStartTime,
    scheduleAdBreak_vodSourceName,
    scheduleAdBreak_approximateDurationSeconds,
    scheduleAdBreak_sourceLocationName,

    -- * ScheduleConfiguration
    ScheduleConfiguration (..),
    newScheduleConfiguration,
    scheduleConfiguration_transition,

    -- * ScheduleEntry
    ScheduleEntry (..),
    newScheduleEntry,
    scheduleEntry_approximateStartTime,
    scheduleEntry_liveSourceName,
    scheduleEntry_vodSourceName,
    scheduleEntry_approximateDurationSeconds,
    scheduleEntry_scheduleEntryType,
    scheduleEntry_scheduleAdBreaks,
    scheduleEntry_arn,
    scheduleEntry_channelName,
    scheduleEntry_programName,
    scheduleEntry_sourceLocationName,

    -- * SecretsManagerAccessTokenConfiguration
    SecretsManagerAccessTokenConfiguration (..),
    newSecretsManagerAccessTokenConfiguration,
    secretsManagerAccessTokenConfiguration_headerName,
    secretsManagerAccessTokenConfiguration_secretArn,
    secretsManagerAccessTokenConfiguration_secretStringKey,

    -- * SegmentDeliveryConfiguration
    SegmentDeliveryConfiguration (..),
    newSegmentDeliveryConfiguration,
    segmentDeliveryConfiguration_baseUrl,
    segmentDeliveryConfiguration_name,

    -- * SegmentationDescriptor
    SegmentationDescriptor (..),
    newSegmentationDescriptor,
    segmentationDescriptor_subSegmentsExpected,
    segmentationDescriptor_segmentationTypeId,
    segmentationDescriptor_segmentationUpidType,
    segmentationDescriptor_segmentNum,
    segmentationDescriptor_segmentationUpid,
    segmentationDescriptor_subSegmentNum,
    segmentationDescriptor_segmentsExpected,
    segmentationDescriptor_segmentationEventId,

    -- * SlateSource
    SlateSource (..),
    newSlateSource,
    slateSource_vodSourceName,
    slateSource_sourceLocationName,

    -- * SourceLocation
    SourceLocation (..),
    newSourceLocation,
    sourceLocation_tags,
    sourceLocation_segmentDeliveryConfigurations,
    sourceLocation_accessConfiguration,
    sourceLocation_defaultSegmentDeliveryConfiguration,
    sourceLocation_lastModifiedTime,
    sourceLocation_creationTime,
    sourceLocation_arn,
    sourceLocation_httpConfiguration,
    sourceLocation_sourceLocationName,

    -- * SpliceInsertMessage
    SpliceInsertMessage (..),
    newSpliceInsertMessage,
    spliceInsertMessage_availsExpected,
    spliceInsertMessage_spliceEventId,
    spliceInsertMessage_availNum,
    spliceInsertMessage_uniqueProgramId,

    -- * TimeSignalMessage
    TimeSignalMessage (..),
    newTimeSignalMessage,
    timeSignalMessage_segmentationDescriptors,

    -- * Transition
    Transition (..),
    newTransition,
    transition_relativeProgram,
    transition_scheduledStartTimeMillis,
    transition_durationMillis,
    transition_relativePosition,
    transition_type,

    -- * VodSource
    VodSource (..),
    newVodSource,
    vodSource_tags,
    vodSource_lastModifiedTime,
    vodSource_creationTime,
    vodSource_arn,
    vodSource_httpPackageConfigurations,
    vodSource_sourceLocationName,
    vodSource_vodSourceName,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
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
import Amazonka.MediaTailor.Types.LiveSource
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
import Amazonka.MediaTailor.Types.SegmentDeliveryConfiguration
import Amazonka.MediaTailor.Types.SegmentationDescriptor
import Amazonka.MediaTailor.Types.SlateSource
import Amazonka.MediaTailor.Types.SourceLocation
import Amazonka.MediaTailor.Types.SpliceInsertMessage
import Amazonka.MediaTailor.Types.Tier
import Amazonka.MediaTailor.Types.TimeSignalMessage
import Amazonka.MediaTailor.Types.Transition
import Amazonka.MediaTailor.Types.Type
import Amazonka.MediaTailor.Types.VodSource
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2018-04-23@ of the Amazon MediaTailor SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "MediaTailor",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "api.mediatailor",
      Core.signingName = "mediatailor",
      Core.version = "2018-04-23",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "MediaTailor",
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

-- | A request contains unexpected data.
_BadRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Core.hasStatus 400
