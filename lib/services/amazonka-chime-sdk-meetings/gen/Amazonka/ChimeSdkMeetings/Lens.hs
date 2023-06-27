{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ChimeSdkMeetings.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMeetings.Lens
  ( -- * Operations

    -- ** BatchCreateAttendee
    batchCreateAttendee_meetingId,
    batchCreateAttendee_attendees,
    batchCreateAttendeeResponse_attendees,
    batchCreateAttendeeResponse_errors,
    batchCreateAttendeeResponse_httpStatus,

    -- ** BatchUpdateAttendeeCapabilitiesExcept
    batchUpdateAttendeeCapabilitiesExcept_meetingId,
    batchUpdateAttendeeCapabilitiesExcept_excludedAttendeeIds,
    batchUpdateAttendeeCapabilitiesExcept_capabilities,

    -- ** CreateAttendee
    createAttendee_capabilities,
    createAttendee_meetingId,
    createAttendee_externalUserId,
    createAttendeeResponse_attendee,
    createAttendeeResponse_httpStatus,

    -- ** CreateMeeting
    createMeeting_meetingFeatures,
    createMeeting_meetingHostId,
    createMeeting_notificationsConfiguration,
    createMeeting_primaryMeetingId,
    createMeeting_tags,
    createMeeting_tenantIds,
    createMeeting_clientRequestToken,
    createMeeting_mediaRegion,
    createMeeting_externalMeetingId,
    createMeetingResponse_meeting,
    createMeetingResponse_httpStatus,

    -- ** CreateMeetingWithAttendees
    createMeetingWithAttendees_meetingFeatures,
    createMeetingWithAttendees_meetingHostId,
    createMeetingWithAttendees_notificationsConfiguration,
    createMeetingWithAttendees_primaryMeetingId,
    createMeetingWithAttendees_tags,
    createMeetingWithAttendees_tenantIds,
    createMeetingWithAttendees_clientRequestToken,
    createMeetingWithAttendees_mediaRegion,
    createMeetingWithAttendees_externalMeetingId,
    createMeetingWithAttendees_attendees,
    createMeetingWithAttendeesResponse_attendees,
    createMeetingWithAttendeesResponse_errors,
    createMeetingWithAttendeesResponse_meeting,
    createMeetingWithAttendeesResponse_httpStatus,

    -- ** DeleteAttendee
    deleteAttendee_meetingId,
    deleteAttendee_attendeeId,

    -- ** DeleteMeeting
    deleteMeeting_meetingId,

    -- ** GetAttendee
    getAttendee_meetingId,
    getAttendee_attendeeId,
    getAttendeeResponse_attendee,
    getAttendeeResponse_httpStatus,

    -- ** GetMeeting
    getMeeting_meetingId,
    getMeetingResponse_meeting,
    getMeetingResponse_httpStatus,

    -- ** ListAttendees
    listAttendees_maxResults,
    listAttendees_nextToken,
    listAttendees_meetingId,
    listAttendeesResponse_attendees,
    listAttendeesResponse_nextToken,
    listAttendeesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** StartMeetingTranscription
    startMeetingTranscription_meetingId,
    startMeetingTranscription_transcriptionConfiguration,

    -- ** StopMeetingTranscription
    stopMeetingTranscription_meetingId,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateAttendeeCapabilities
    updateAttendeeCapabilities_meetingId,
    updateAttendeeCapabilities_attendeeId,
    updateAttendeeCapabilities_capabilities,
    updateAttendeeCapabilitiesResponse_attendee,
    updateAttendeeCapabilitiesResponse_httpStatus,

    -- * Types

    -- ** Attendee
    attendee_attendeeId,
    attendee_capabilities,
    attendee_externalUserId,
    attendee_joinToken,

    -- ** AttendeeCapabilities
    attendeeCapabilities_audio,
    attendeeCapabilities_video,
    attendeeCapabilities_content,

    -- ** AttendeeIdItem
    attendeeIdItem_attendeeId,

    -- ** AudioFeatures
    audioFeatures_echoReduction,

    -- ** CreateAttendeeError
    createAttendeeError_errorCode,
    createAttendeeError_errorMessage,
    createAttendeeError_externalUserId,

    -- ** CreateAttendeeRequestItem
    createAttendeeRequestItem_capabilities,
    createAttendeeRequestItem_externalUserId,

    -- ** EngineTranscribeMedicalSettings
    engineTranscribeMedicalSettings_contentIdentificationType,
    engineTranscribeMedicalSettings_region,
    engineTranscribeMedicalSettings_vocabularyName,
    engineTranscribeMedicalSettings_languageCode,
    engineTranscribeMedicalSettings_specialty,
    engineTranscribeMedicalSettings_type,

    -- ** EngineTranscribeSettings
    engineTranscribeSettings_contentIdentificationType,
    engineTranscribeSettings_contentRedactionType,
    engineTranscribeSettings_enablePartialResultsStabilization,
    engineTranscribeSettings_identifyLanguage,
    engineTranscribeSettings_languageCode,
    engineTranscribeSettings_languageModelName,
    engineTranscribeSettings_languageOptions,
    engineTranscribeSettings_partialResultsStability,
    engineTranscribeSettings_piiEntityTypes,
    engineTranscribeSettings_preferredLanguage,
    engineTranscribeSettings_region,
    engineTranscribeSettings_vocabularyFilterMethod,
    engineTranscribeSettings_vocabularyFilterName,
    engineTranscribeSettings_vocabularyFilterNames,
    engineTranscribeSettings_vocabularyName,
    engineTranscribeSettings_vocabularyNames,

    -- ** MediaPlacement
    mediaPlacement_audioFallbackUrl,
    mediaPlacement_audioHostUrl,
    mediaPlacement_eventIngestionUrl,
    mediaPlacement_screenDataUrl,
    mediaPlacement_screenSharingUrl,
    mediaPlacement_screenViewingUrl,
    mediaPlacement_signalingUrl,
    mediaPlacement_turnControlUrl,

    -- ** Meeting
    meeting_externalMeetingId,
    meeting_mediaPlacement,
    meeting_mediaRegion,
    meeting_meetingArn,
    meeting_meetingFeatures,
    meeting_meetingHostId,
    meeting_meetingId,
    meeting_primaryMeetingId,
    meeting_tenantIds,

    -- ** MeetingFeaturesConfiguration
    meetingFeaturesConfiguration_audio,

    -- ** NotificationsConfiguration
    notificationsConfiguration_lambdaFunctionArn,
    notificationsConfiguration_snsTopicArn,
    notificationsConfiguration_sqsQueueArn,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TranscriptionConfiguration
    transcriptionConfiguration_engineTranscribeMedicalSettings,
    transcriptionConfiguration_engineTranscribeSettings,
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
import Amazonka.ChimeSdkMeetings.ListAttendees
import Amazonka.ChimeSdkMeetings.ListTagsForResource
import Amazonka.ChimeSdkMeetings.StartMeetingTranscription
import Amazonka.ChimeSdkMeetings.StopMeetingTranscription
import Amazonka.ChimeSdkMeetings.TagResource
import Amazonka.ChimeSdkMeetings.Types.Attendee
import Amazonka.ChimeSdkMeetings.Types.AttendeeCapabilities
import Amazonka.ChimeSdkMeetings.Types.AttendeeIdItem
import Amazonka.ChimeSdkMeetings.Types.AudioFeatures
import Amazonka.ChimeSdkMeetings.Types.CreateAttendeeError
import Amazonka.ChimeSdkMeetings.Types.CreateAttendeeRequestItem
import Amazonka.ChimeSdkMeetings.Types.EngineTranscribeMedicalSettings
import Amazonka.ChimeSdkMeetings.Types.EngineTranscribeSettings
import Amazonka.ChimeSdkMeetings.Types.MediaPlacement
import Amazonka.ChimeSdkMeetings.Types.Meeting
import Amazonka.ChimeSdkMeetings.Types.MeetingFeaturesConfiguration
import Amazonka.ChimeSdkMeetings.Types.NotificationsConfiguration
import Amazonka.ChimeSdkMeetings.Types.Tag
import Amazonka.ChimeSdkMeetings.Types.TranscriptionConfiguration
import Amazonka.ChimeSdkMeetings.UntagResource
import Amazonka.ChimeSdkMeetings.UpdateAttendeeCapabilities
