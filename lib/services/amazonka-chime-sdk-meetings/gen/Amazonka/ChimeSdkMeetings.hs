{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.ChimeSdkMeetings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2021-07-15@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- The Amazon Chime SDK meetings APIs in this section allow software
-- developers to create Amazon Chime SDK meetings, set the AWS Regions for
-- meetings, create and manage users, and send and receive meeting
-- notifications. For more information about the meeting APIs, see
-- <https://docs.aws.amazon.com/chime/latest/APIReference/API_Operations_Amazon_Chime_SDK_Meetings.html Amazon Chime SDK meetings>.
module Amazonka.ChimeSdkMeetings
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** BadRequestException
    _BadRequestException,

    -- ** ConflictException
    _ConflictException,

    -- ** ForbiddenException
    _ForbiddenException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceFailureException
    _ServiceFailureException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** TooManyTagsException
    _TooManyTagsException,

    -- ** UnauthorizedException
    _UnauthorizedException,

    -- ** UnprocessableEntityException
    _UnprocessableEntityException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** BatchCreateAttendee
    BatchCreateAttendee (BatchCreateAttendee'),
    newBatchCreateAttendee,
    BatchCreateAttendeeResponse (BatchCreateAttendeeResponse'),
    newBatchCreateAttendeeResponse,

    -- ** BatchUpdateAttendeeCapabilitiesExcept
    BatchUpdateAttendeeCapabilitiesExcept (BatchUpdateAttendeeCapabilitiesExcept'),
    newBatchUpdateAttendeeCapabilitiesExcept,
    BatchUpdateAttendeeCapabilitiesExceptResponse (BatchUpdateAttendeeCapabilitiesExceptResponse'),
    newBatchUpdateAttendeeCapabilitiesExceptResponse,

    -- ** CreateAttendee
    CreateAttendee (CreateAttendee'),
    newCreateAttendee,
    CreateAttendeeResponse (CreateAttendeeResponse'),
    newCreateAttendeeResponse,

    -- ** CreateMeeting
    CreateMeeting (CreateMeeting'),
    newCreateMeeting,
    CreateMeetingResponse (CreateMeetingResponse'),
    newCreateMeetingResponse,

    -- ** CreateMeetingWithAttendees
    CreateMeetingWithAttendees (CreateMeetingWithAttendees'),
    newCreateMeetingWithAttendees,
    CreateMeetingWithAttendeesResponse (CreateMeetingWithAttendeesResponse'),
    newCreateMeetingWithAttendeesResponse,

    -- ** DeleteAttendee
    DeleteAttendee (DeleteAttendee'),
    newDeleteAttendee,
    DeleteAttendeeResponse (DeleteAttendeeResponse'),
    newDeleteAttendeeResponse,

    -- ** DeleteMeeting
    DeleteMeeting (DeleteMeeting'),
    newDeleteMeeting,
    DeleteMeetingResponse (DeleteMeetingResponse'),
    newDeleteMeetingResponse,

    -- ** GetAttendee
    GetAttendee (GetAttendee'),
    newGetAttendee,
    GetAttendeeResponse (GetAttendeeResponse'),
    newGetAttendeeResponse,

    -- ** GetMeeting
    GetMeeting (GetMeeting'),
    newGetMeeting,
    GetMeetingResponse (GetMeetingResponse'),
    newGetMeetingResponse,

    -- ** ListAttendees
    ListAttendees (ListAttendees'),
    newListAttendees,
    ListAttendeesResponse (ListAttendeesResponse'),
    newListAttendeesResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** StartMeetingTranscription
    StartMeetingTranscription (StartMeetingTranscription'),
    newStartMeetingTranscription,
    StartMeetingTranscriptionResponse (StartMeetingTranscriptionResponse'),
    newStartMeetingTranscriptionResponse,

    -- ** StopMeetingTranscription
    StopMeetingTranscription (StopMeetingTranscription'),
    newStopMeetingTranscription,
    StopMeetingTranscriptionResponse (StopMeetingTranscriptionResponse'),
    newStopMeetingTranscriptionResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateAttendeeCapabilities
    UpdateAttendeeCapabilities (UpdateAttendeeCapabilities'),
    newUpdateAttendeeCapabilities,
    UpdateAttendeeCapabilitiesResponse (UpdateAttendeeCapabilitiesResponse'),
    newUpdateAttendeeCapabilitiesResponse,

    -- * Types

    -- ** MediaCapabilities
    MediaCapabilities (..),

    -- ** MeetingFeatureStatus
    MeetingFeatureStatus (..),

    -- ** TranscribeContentIdentificationType
    TranscribeContentIdentificationType (..),

    -- ** TranscribeContentRedactionType
    TranscribeContentRedactionType (..),

    -- ** TranscribeLanguageCode
    TranscribeLanguageCode (..),

    -- ** TranscribeMedicalContentIdentificationType
    TranscribeMedicalContentIdentificationType (..),

    -- ** TranscribeMedicalLanguageCode
    TranscribeMedicalLanguageCode (..),

    -- ** TranscribeMedicalRegion
    TranscribeMedicalRegion (..),

    -- ** TranscribeMedicalSpecialty
    TranscribeMedicalSpecialty (..),

    -- ** TranscribeMedicalType
    TranscribeMedicalType (..),

    -- ** TranscribePartialResultsStability
    TranscribePartialResultsStability (..),

    -- ** TranscribeRegion
    TranscribeRegion (..),

    -- ** TranscribeVocabularyFilterMethod
    TranscribeVocabularyFilterMethod (..),

    -- ** Attendee
    Attendee (Attendee'),
    newAttendee,

    -- ** AttendeeCapabilities
    AttendeeCapabilities (AttendeeCapabilities'),
    newAttendeeCapabilities,

    -- ** AttendeeIdItem
    AttendeeIdItem (AttendeeIdItem'),
    newAttendeeIdItem,

    -- ** AudioFeatures
    AudioFeatures (AudioFeatures'),
    newAudioFeatures,

    -- ** CreateAttendeeError
    CreateAttendeeError (CreateAttendeeError'),
    newCreateAttendeeError,

    -- ** CreateAttendeeRequestItem
    CreateAttendeeRequestItem (CreateAttendeeRequestItem'),
    newCreateAttendeeRequestItem,

    -- ** EngineTranscribeMedicalSettings
    EngineTranscribeMedicalSettings (EngineTranscribeMedicalSettings'),
    newEngineTranscribeMedicalSettings,

    -- ** EngineTranscribeSettings
    EngineTranscribeSettings (EngineTranscribeSettings'),
    newEngineTranscribeSettings,

    -- ** MediaPlacement
    MediaPlacement (MediaPlacement'),
    newMediaPlacement,

    -- ** Meeting
    Meeting (Meeting'),
    newMeeting,

    -- ** MeetingFeaturesConfiguration
    MeetingFeaturesConfiguration (MeetingFeaturesConfiguration'),
    newMeetingFeaturesConfiguration,

    -- ** NotificationsConfiguration
    NotificationsConfiguration (NotificationsConfiguration'),
    newNotificationsConfiguration,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** TranscriptionConfiguration
    TranscriptionConfiguration (TranscriptionConfiguration'),
    newTranscriptionConfiguration,
  )
where

import Amazonka.ChimeSdkMeetings.BatchCreateAttendee
import Amazonka.ChimeSdkMeetings.BatchUpdateAttendeeCapabilitiesExcept
import Amazonka.ChimeSdkMeetings.CreateAttendee
import Amazonka.ChimeSdkMeetings.CreateMeeting
import Amazonka.ChimeSdkMeetings.CreateMeetingWithAttendees
import Amazonka.ChimeSdkMeetings.DeleteAttendee
import Amazonka.ChimeSdkMeetings.DeleteMeeting
import Amazonka.ChimeSdkMeetings.GetAttendee
import Amazonka.ChimeSdkMeetings.GetMeeting
import Amazonka.ChimeSdkMeetings.Lens
import Amazonka.ChimeSdkMeetings.ListAttendees
import Amazonka.ChimeSdkMeetings.ListTagsForResource
import Amazonka.ChimeSdkMeetings.StartMeetingTranscription
import Amazonka.ChimeSdkMeetings.StopMeetingTranscription
import Amazonka.ChimeSdkMeetings.TagResource
import Amazonka.ChimeSdkMeetings.Types
import Amazonka.ChimeSdkMeetings.UntagResource
import Amazonka.ChimeSdkMeetings.UpdateAttendeeCapabilities
import Amazonka.ChimeSdkMeetings.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'ChimeSdkMeetings'.

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
