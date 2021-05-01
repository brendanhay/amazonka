{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS Elastic Transcoder Service
--
-- The AWS Elastic Transcoder Service.
module Network.AWS.ElasticTranscoder
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** IncompatibleVersionException
    _IncompatibleVersionException,

    -- ** InternalServiceException
    _InternalServiceException,

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ValidationException
    _ValidationException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- * Waiters
    -- $waiters

    -- ** JobComplete
    newJobComplete,

    -- * Operations
    -- $operations

    -- ** ListJobsByPipeline (Paginated)
    ListJobsByPipeline (ListJobsByPipeline'),
    newListJobsByPipeline,
    ListJobsByPipelineResponse (ListJobsByPipelineResponse'),
    newListJobsByPipelineResponse,

    -- ** UpdatePipelineStatus
    UpdatePipelineStatus (UpdatePipelineStatus'),
    newUpdatePipelineStatus,
    UpdatePipelineStatusResponse (UpdatePipelineStatusResponse'),
    newUpdatePipelineStatusResponse,

    -- ** ListPresets (Paginated)
    ListPresets (ListPresets'),
    newListPresets,
    ListPresetsResponse (ListPresetsResponse'),
    newListPresetsResponse,

    -- ** DeletePreset
    DeletePreset (DeletePreset'),
    newDeletePreset,
    DeletePresetResponse (DeletePresetResponse'),
    newDeletePresetResponse,

    -- ** CancelJob
    CancelJob (CancelJob'),
    newCancelJob,
    CancelJobResponse (CancelJobResponse'),
    newCancelJobResponse,

    -- ** CreatePreset
    CreatePreset (CreatePreset'),
    newCreatePreset,
    CreatePresetResponse (CreatePresetResponse'),
    newCreatePresetResponse,

    -- ** CreatePipeline
    CreatePipeline (CreatePipeline'),
    newCreatePipeline,
    CreatePipelineResponse (CreatePipelineResponse'),
    newCreatePipelineResponse,

    -- ** UpdatePipeline
    UpdatePipeline (UpdatePipeline'),
    newUpdatePipeline,
    UpdatePipelineResponse (UpdatePipelineResponse'),
    newUpdatePipelineResponse,

    -- ** DeletePipeline
    DeletePipeline (DeletePipeline'),
    newDeletePipeline,
    DeletePipelineResponse (DeletePipelineResponse'),
    newDeletePipelineResponse,

    -- ** ReadPreset
    ReadPreset (ReadPreset'),
    newReadPreset,
    ReadPresetResponse (ReadPresetResponse'),
    newReadPresetResponse,

    -- ** ListJobsByStatus (Paginated)
    ListJobsByStatus (ListJobsByStatus'),
    newListJobsByStatus,
    ListJobsByStatusResponse (ListJobsByStatusResponse'),
    newListJobsByStatusResponse,

    -- ** CreateJob
    CreateJob (CreateJob'),
    newCreateJob,
    CreateJobResponse (CreateJobResponse'),
    newCreateJobResponse,

    -- ** ListPipelines (Paginated)
    ListPipelines (ListPipelines'),
    newListPipelines,
    ListPipelinesResponse (ListPipelinesResponse'),
    newListPipelinesResponse,

    -- ** ReadPipeline
    ReadPipeline (ReadPipeline'),
    newReadPipeline,
    ReadPipelineResponse (ReadPipelineResponse'),
    newReadPipelineResponse,

    -- ** UpdatePipelineNotifications
    UpdatePipelineNotifications (UpdatePipelineNotifications'),
    newUpdatePipelineNotifications,
    UpdatePipelineNotificationsResponse (UpdatePipelineNotificationsResponse'),
    newUpdatePipelineNotificationsResponse,

    -- ** ReadJob
    ReadJob (ReadJob'),
    newReadJob,
    ReadJobResponse (ReadJobResponse'),
    newReadJobResponse,

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

    -- ** Job'
    Job' (Job''),
    newJob',

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

import Network.AWS.ElasticTranscoder.CancelJob
import Network.AWS.ElasticTranscoder.CreateJob
import Network.AWS.ElasticTranscoder.CreatePipeline
import Network.AWS.ElasticTranscoder.CreatePreset
import Network.AWS.ElasticTranscoder.DeletePipeline
import Network.AWS.ElasticTranscoder.DeletePreset
import Network.AWS.ElasticTranscoder.Lens
import Network.AWS.ElasticTranscoder.ListJobsByPipeline
import Network.AWS.ElasticTranscoder.ListJobsByStatus
import Network.AWS.ElasticTranscoder.ListPipelines
import Network.AWS.ElasticTranscoder.ListPresets
import Network.AWS.ElasticTranscoder.ReadJob
import Network.AWS.ElasticTranscoder.ReadPipeline
import Network.AWS.ElasticTranscoder.ReadPreset
import Network.AWS.ElasticTranscoder.Types
import Network.AWS.ElasticTranscoder.UpdatePipeline
import Network.AWS.ElasticTranscoder.UpdatePipelineNotifications
import Network.AWS.ElasticTranscoder.UpdatePipelineStatus
import Network.AWS.ElasticTranscoder.Waiters

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
