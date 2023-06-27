{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MediaPackageV2.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackageV2.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ConflictException,
    _InternalServerException,
    _ResourceNotFoundException,
    _ServiceQuotaExceededException,
    _ThrottlingException,
    _ValidationException,

    -- * AdMarkerHls
    AdMarkerHls (..),

    -- * CmafEncryptionMethod
    CmafEncryptionMethod (..),

    -- * ContainerType
    ContainerType (..),

    -- * DrmSystem
    DrmSystem (..),

    -- * PresetSpeke20Audio
    PresetSpeke20Audio (..),

    -- * PresetSpeke20Video
    PresetSpeke20Video (..),

    -- * ScteFilter
    ScteFilter (..),

    -- * TsEncryptionMethod
    TsEncryptionMethod (..),

    -- * ChannelGroupListConfiguration
    ChannelGroupListConfiguration (..),
    newChannelGroupListConfiguration,
    channelGroupListConfiguration_description,
    channelGroupListConfiguration_channelGroupName,
    channelGroupListConfiguration_arn,
    channelGroupListConfiguration_createdAt,
    channelGroupListConfiguration_modifiedAt,

    -- * ChannelListConfiguration
    ChannelListConfiguration (..),
    newChannelListConfiguration,
    channelListConfiguration_description,
    channelListConfiguration_arn,
    channelListConfiguration_channelName,
    channelListConfiguration_channelGroupName,
    channelListConfiguration_createdAt,
    channelListConfiguration_modifiedAt,

    -- * CreateHlsManifestConfiguration
    CreateHlsManifestConfiguration (..),
    newCreateHlsManifestConfiguration,
    createHlsManifestConfiguration_childManifestName,
    createHlsManifestConfiguration_manifestWindowSeconds,
    createHlsManifestConfiguration_programDateTimeIntervalSeconds,
    createHlsManifestConfiguration_scteHls,
    createHlsManifestConfiguration_manifestName,

    -- * CreateLowLatencyHlsManifestConfiguration
    CreateLowLatencyHlsManifestConfiguration (..),
    newCreateLowLatencyHlsManifestConfiguration,
    createLowLatencyHlsManifestConfiguration_childManifestName,
    createLowLatencyHlsManifestConfiguration_manifestWindowSeconds,
    createLowLatencyHlsManifestConfiguration_programDateTimeIntervalSeconds,
    createLowLatencyHlsManifestConfiguration_scteHls,
    createLowLatencyHlsManifestConfiguration_manifestName,

    -- * Encryption
    Encryption (..),
    newEncryption,
    encryption_constantInitializationVector,
    encryption_keyRotationIntervalSeconds,
    encryption_encryptionMethod,
    encryption_spekeKeyProvider,

    -- * EncryptionContractConfiguration
    EncryptionContractConfiguration (..),
    newEncryptionContractConfiguration,
    encryptionContractConfiguration_presetSpeke20Audio,
    encryptionContractConfiguration_presetSpeke20Video,

    -- * EncryptionMethod
    EncryptionMethod (..),
    newEncryptionMethod,
    encryptionMethod_cmafEncryptionMethod,
    encryptionMethod_tsEncryptionMethod,

    -- * GetHlsManifestConfiguration
    GetHlsManifestConfiguration (..),
    newGetHlsManifestConfiguration,
    getHlsManifestConfiguration_childManifestName,
    getHlsManifestConfiguration_manifestWindowSeconds,
    getHlsManifestConfiguration_programDateTimeIntervalSeconds,
    getHlsManifestConfiguration_scteHls,
    getHlsManifestConfiguration_manifestName,
    getHlsManifestConfiguration_url,

    -- * GetLowLatencyHlsManifestConfiguration
    GetLowLatencyHlsManifestConfiguration (..),
    newGetLowLatencyHlsManifestConfiguration,
    getLowLatencyHlsManifestConfiguration_childManifestName,
    getLowLatencyHlsManifestConfiguration_manifestWindowSeconds,
    getLowLatencyHlsManifestConfiguration_programDateTimeIntervalSeconds,
    getLowLatencyHlsManifestConfiguration_scteHls,
    getLowLatencyHlsManifestConfiguration_manifestName,
    getLowLatencyHlsManifestConfiguration_url,

    -- * IngestEndpoint
    IngestEndpoint (..),
    newIngestEndpoint,
    ingestEndpoint_id,
    ingestEndpoint_url,

    -- * ListHlsManifestConfiguration
    ListHlsManifestConfiguration (..),
    newListHlsManifestConfiguration,
    listHlsManifestConfiguration_childManifestName,
    listHlsManifestConfiguration_url,
    listHlsManifestConfiguration_manifestName,

    -- * ListLowLatencyHlsManifestConfiguration
    ListLowLatencyHlsManifestConfiguration (..),
    newListLowLatencyHlsManifestConfiguration,
    listLowLatencyHlsManifestConfiguration_childManifestName,
    listLowLatencyHlsManifestConfiguration_url,
    listLowLatencyHlsManifestConfiguration_manifestName,

    -- * OriginEndpointListConfiguration
    OriginEndpointListConfiguration (..),
    newOriginEndpointListConfiguration,
    originEndpointListConfiguration_createdAt,
    originEndpointListConfiguration_description,
    originEndpointListConfiguration_hlsManifests,
    originEndpointListConfiguration_lowLatencyHlsManifests,
    originEndpointListConfiguration_modifiedAt,
    originEndpointListConfiguration_arn,
    originEndpointListConfiguration_channelGroupName,
    originEndpointListConfiguration_channelName,
    originEndpointListConfiguration_originEndpointName,
    originEndpointListConfiguration_containerType,

    -- * Scte
    Scte (..),
    newScte,
    scte_scteFilter,

    -- * ScteHls
    ScteHls (..),
    newScteHls,
    scteHls_adMarkerHls,

    -- * Segment
    Segment (..),
    newSegment,
    segment_encryption,
    segment_includeIframeOnlyStreams,
    segment_scte,
    segment_segmentDurationSeconds,
    segment_segmentName,
    segment_tsIncludeDvbSubtitles,
    segment_tsUseAudioRenditionGroup,

    -- * SpekeKeyProvider
    SpekeKeyProvider (..),
    newSpekeKeyProvider,
    spekeKeyProvider_encryptionContractConfiguration,
    spekeKeyProvider_resourceId,
    spekeKeyProvider_drmSystems,
    spekeKeyProvider_roleArn,
    spekeKeyProvider_url,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaPackageV2.Types.AdMarkerHls
import Amazonka.MediaPackageV2.Types.ChannelGroupListConfiguration
import Amazonka.MediaPackageV2.Types.ChannelListConfiguration
import Amazonka.MediaPackageV2.Types.CmafEncryptionMethod
import Amazonka.MediaPackageV2.Types.ContainerType
import Amazonka.MediaPackageV2.Types.CreateHlsManifestConfiguration
import Amazonka.MediaPackageV2.Types.CreateLowLatencyHlsManifestConfiguration
import Amazonka.MediaPackageV2.Types.DrmSystem
import Amazonka.MediaPackageV2.Types.Encryption
import Amazonka.MediaPackageV2.Types.EncryptionContractConfiguration
import Amazonka.MediaPackageV2.Types.EncryptionMethod
import Amazonka.MediaPackageV2.Types.GetHlsManifestConfiguration
import Amazonka.MediaPackageV2.Types.GetLowLatencyHlsManifestConfiguration
import Amazonka.MediaPackageV2.Types.IngestEndpoint
import Amazonka.MediaPackageV2.Types.ListHlsManifestConfiguration
import Amazonka.MediaPackageV2.Types.ListLowLatencyHlsManifestConfiguration
import Amazonka.MediaPackageV2.Types.OriginEndpointListConfiguration
import Amazonka.MediaPackageV2.Types.PresetSpeke20Audio
import Amazonka.MediaPackageV2.Types.PresetSpeke20Video
import Amazonka.MediaPackageV2.Types.Scte
import Amazonka.MediaPackageV2.Types.ScteFilter
import Amazonka.MediaPackageV2.Types.ScteHls
import Amazonka.MediaPackageV2.Types.Segment
import Amazonka.MediaPackageV2.Types.SpekeKeyProvider
import Amazonka.MediaPackageV2.Types.TsEncryptionMethod
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2022-12-25@ of the Amazon Elemental MediaPackage v2 SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "MediaPackageV2",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "mediapackagev2",
      Core.signingName = "mediapackagev2",
      Core.version = "2022-12-25",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "MediaPackageV2",
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

-- | You don\'t have permissions to perform the requested operation. The user
-- or role that is making the request must have at least one IAM
-- permissions policy attached that grants the required permissions. For
-- more information, see Access Management in the IAM User Guide.
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | Updating or deleting this resource can cause an inconsistent state.
_ConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | Indicates that an error from the service occurred while trying to
-- process a request.
_InternalServerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The specified resource doesn\'t exist.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The request would cause a service quota to be exceeded.
_ServiceQuotaExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | The request throughput limit was exceeded.
_ThrottlingException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | The input failed to meet the constraints specified by the AWS service.
_ValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
