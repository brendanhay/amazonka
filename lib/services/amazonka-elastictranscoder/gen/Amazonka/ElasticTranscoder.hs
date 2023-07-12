{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.ElasticTranscoder
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2012-09-25@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- AWS Elastic Transcoder Service
--
-- The AWS Elastic Transcoder Service.
module Amazonka.ElasticTranscoder
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** IncompatibleVersionException
    _IncompatibleVersionException,

    -- ** InternalServiceException
    _InternalServiceException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- ** JobComplete
    newJobComplete,

    -- * Operations
    -- $operations

    -- ** CancelJob
    CancelJob (CancelJob'),
    newCancelJob,
    CancelJobResponse (CancelJobResponse'),
    newCancelJobResponse,

    -- ** CreateJob
    CreateJob (CreateJob'),
    newCreateJob,
    CreateJobResponse (CreateJobResponse'),
    newCreateJobResponse,

    -- ** CreatePipeline
    CreatePipeline (CreatePipeline'),
    newCreatePipeline,
    CreatePipelineResponse (CreatePipelineResponse'),
    newCreatePipelineResponse,

    -- ** CreatePreset
    CreatePreset (CreatePreset'),
    newCreatePreset,
    CreatePresetResponse (CreatePresetResponse'),
    newCreatePresetResponse,

    -- ** DeletePipeline
    DeletePipeline (DeletePipeline'),
    newDeletePipeline,
    DeletePipelineResponse (DeletePipelineResponse'),
    newDeletePipelineResponse,

    -- ** DeletePreset
    DeletePreset (DeletePreset'),
    newDeletePreset,
    DeletePresetResponse (DeletePresetResponse'),
    newDeletePresetResponse,

    -- ** ListJobsByPipeline (Paginated)
    ListJobsByPipeline (ListJobsByPipeline'),
    newListJobsByPipeline,
    ListJobsByPipelineResponse (ListJobsByPipelineResponse'),
    newListJobsByPipelineResponse,

    -- ** ListJobsByStatus (Paginated)
    ListJobsByStatus (ListJobsByStatus'),
    newListJobsByStatus,
    ListJobsByStatusResponse (ListJobsByStatusResponse'),
    newListJobsByStatusResponse,

    -- ** ListPipelines (Paginated)
    ListPipelines (ListPipelines'),
    newListPipelines,
    ListPipelinesResponse (ListPipelinesResponse'),
    newListPipelinesResponse,

    -- ** ListPresets (Paginated)
    ListPresets (ListPresets'),
    newListPresets,
    ListPresetsResponse (ListPresetsResponse'),
    newListPresetsResponse,

    -- ** ReadJob
    ReadJob (ReadJob'),
    newReadJob,
    ReadJobResponse (ReadJobResponse'),
    newReadJobResponse,

    -- ** ReadPipeline
    ReadPipeline (ReadPipeline'),
    newReadPipeline,
    ReadPipelineResponse (ReadPipelineResponse'),
    newReadPipelineResponse,

    -- ** ReadPreset
    ReadPreset (ReadPreset'),
    newReadPreset,
    ReadPresetResponse (ReadPresetResponse'),
    newReadPresetResponse,

    -- ** UpdatePipeline
    UpdatePipeline (UpdatePipeline'),
    newUpdatePipeline,
    UpdatePipelineResponse (UpdatePipelineResponse'),
    newUpdatePipelineResponse,

    -- ** UpdatePipelineNotifications
    UpdatePipelineNotifications (UpdatePipelineNotifications'),
    newUpdatePipelineNotifications,
    UpdatePipelineNotificationsResponse (UpdatePipelineNotificationsResponse'),
    newUpdatePipelineNotificationsResponse,

    -- ** UpdatePipelineStatus
    UpdatePipelineStatus (UpdatePipelineStatus'),
    newUpdatePipelineStatus,
    UpdatePipelineStatusResponse (UpdatePipelineStatusResponse'),
    newUpdatePipelineStatusResponse,

    -- * Types

    -- ** Artwork
    Artwork (Artwork'),
    newArtwork,

    -- ** AudioCodecOptions
    AudioCodecOptions (AudioCodecOptions'),
    newAudioCodecOptions,

    -- ** AudioParameters
    AudioParameters (AudioParameters'),
    newAudioParameters,

    -- ** CaptionFormat
    CaptionFormat (CaptionFormat'),
    newCaptionFormat,

    -- ** CaptionSource
    CaptionSource (CaptionSource'),
    newCaptionSource,

    -- ** Captions
    Captions (Captions'),
    newCaptions,

    -- ** Clip
    Clip (Clip'),
    newClip,

    -- ** CreateJobOutput
    CreateJobOutput (CreateJobOutput'),
    newCreateJobOutput,

    -- ** CreateJobPlaylist
    CreateJobPlaylist (CreateJobPlaylist'),
    newCreateJobPlaylist,

    -- ** DetectedProperties
    DetectedProperties (DetectedProperties'),
    newDetectedProperties,

    -- ** Encryption
    Encryption (Encryption'),
    newEncryption,

    -- ** HlsContentProtection
    HlsContentProtection (HlsContentProtection'),
    newHlsContentProtection,

    -- ** InputCaptions
    InputCaptions (InputCaptions'),
    newInputCaptions,

    -- ** Job
    Job (Job'),
    newJob,

    -- ** JobAlbumArt
    JobAlbumArt (JobAlbumArt'),
    newJobAlbumArt,

    -- ** JobInput
    JobInput (JobInput'),
    newJobInput,

    -- ** JobOutput
    JobOutput (JobOutput'),
    newJobOutput,

    -- ** JobWatermark
    JobWatermark (JobWatermark'),
    newJobWatermark,

    -- ** Notifications
    Notifications (Notifications'),
    newNotifications,

    -- ** Permission
    Permission (Permission'),
    newPermission,

    -- ** Pipeline
    Pipeline (Pipeline'),
    newPipeline,

    -- ** PipelineOutputConfig
    PipelineOutputConfig (PipelineOutputConfig'),
    newPipelineOutputConfig,

    -- ** PlayReadyDrm
    PlayReadyDrm (PlayReadyDrm'),
    newPlayReadyDrm,

    -- ** Playlist
    Playlist (Playlist'),
    newPlaylist,

    -- ** Preset
    Preset (Preset'),
    newPreset,

    -- ** PresetWatermark
    PresetWatermark (PresetWatermark'),
    newPresetWatermark,

    -- ** Thumbnails
    Thumbnails (Thumbnails'),
    newThumbnails,

    -- ** TimeSpan
    TimeSpan (TimeSpan'),
    newTimeSpan,

    -- ** Timing
    Timing (Timing'),
    newTiming,

    -- ** VideoParameters
    VideoParameters (VideoParameters'),
    newVideoParameters,

    -- ** Warning
    Warning (Warning'),
    newWarning,
  )
where

import Amazonka.ElasticTranscoder.CancelJob
import Amazonka.ElasticTranscoder.CreateJob
import Amazonka.ElasticTranscoder.CreatePipeline
import Amazonka.ElasticTranscoder.CreatePreset
import Amazonka.ElasticTranscoder.DeletePipeline
import Amazonka.ElasticTranscoder.DeletePreset
import Amazonka.ElasticTranscoder.Lens
import Amazonka.ElasticTranscoder.ListJobsByPipeline
import Amazonka.ElasticTranscoder.ListJobsByStatus
import Amazonka.ElasticTranscoder.ListPipelines
import Amazonka.ElasticTranscoder.ListPresets
import Amazonka.ElasticTranscoder.ReadJob
import Amazonka.ElasticTranscoder.ReadPipeline
import Amazonka.ElasticTranscoder.ReadPreset
import Amazonka.ElasticTranscoder.Types
import Amazonka.ElasticTranscoder.UpdatePipeline
import Amazonka.ElasticTranscoder.UpdatePipelineNotifications
import Amazonka.ElasticTranscoder.UpdatePipelineStatus
import Amazonka.ElasticTranscoder.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'ElasticTranscoder'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
