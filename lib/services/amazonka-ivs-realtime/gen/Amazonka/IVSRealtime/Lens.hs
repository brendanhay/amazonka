{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IVSRealtime.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IVSRealtime.Lens
  ( -- * Operations

    -- ** CreateParticipantToken
    createParticipantToken_attributes,
    createParticipantToken_capabilities,
    createParticipantToken_duration,
    createParticipantToken_userId,
    createParticipantToken_stageArn,
    createParticipantTokenResponse_participantToken,
    createParticipantTokenResponse_httpStatus,

    -- ** CreateStage
    createStage_name,
    createStage_participantTokenConfigurations,
    createStage_tags,
    createStageResponse_participantTokens,
    createStageResponse_stage,
    createStageResponse_httpStatus,

    -- ** DeleteStage
    deleteStage_arn,
    deleteStageResponse_httpStatus,

    -- ** DisconnectParticipant
    disconnectParticipant_reason,
    disconnectParticipant_participantId,
    disconnectParticipant_stageArn,
    disconnectParticipantResponse_httpStatus,

    -- ** GetParticipant
    getParticipant_participantId,
    getParticipant_sessionId,
    getParticipant_stageArn,
    getParticipantResponse_participant,
    getParticipantResponse_httpStatus,

    -- ** GetStage
    getStage_arn,
    getStageResponse_stage,
    getStageResponse_httpStatus,

    -- ** GetStageSession
    getStageSession_sessionId,
    getStageSession_stageArn,
    getStageSessionResponse_stageSession,
    getStageSessionResponse_httpStatus,

    -- ** ListParticipantEvents
    listParticipantEvents_maxResults,
    listParticipantEvents_nextToken,
    listParticipantEvents_participantId,
    listParticipantEvents_sessionId,
    listParticipantEvents_stageArn,
    listParticipantEventsResponse_nextToken,
    listParticipantEventsResponse_httpStatus,
    listParticipantEventsResponse_events,

    -- ** ListParticipants
    listParticipants_filterByPublished,
    listParticipants_filterByState,
    listParticipants_filterByUserId,
    listParticipants_maxResults,
    listParticipants_nextToken,
    listParticipants_sessionId,
    listParticipants_stageArn,
    listParticipantsResponse_nextToken,
    listParticipantsResponse_httpStatus,
    listParticipantsResponse_participants,

    -- ** ListStageSessions
    listStageSessions_maxResults,
    listStageSessions_nextToken,
    listStageSessions_stageArn,
    listStageSessionsResponse_nextToken,
    listStageSessionsResponse_httpStatus,
    listStageSessionsResponse_stageSessions,

    -- ** ListStages
    listStages_maxResults,
    listStages_nextToken,
    listStagesResponse_nextToken,
    listStagesResponse_httpStatus,
    listStagesResponse_stages,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_httpStatus,
    listTagsForResourceResponse_tags,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateStage
    updateStage_name,
    updateStage_arn,
    updateStageResponse_stage,
    updateStageResponse_httpStatus,

    -- * Types

    -- ** Event
    event_errorCode,
    event_eventTime,
    event_name,
    event_participantId,
    event_remoteParticipantId,

    -- ** Participant
    participant_attributes,
    participant_firstJoinTime,
    participant_participantId,
    participant_published,
    participant_state,
    participant_userId,

    -- ** ParticipantSummary
    participantSummary_firstJoinTime,
    participantSummary_participantId,
    participantSummary_published,
    participantSummary_state,
    participantSummary_userId,

    -- ** ParticipantToken
    participantToken_attributes,
    participantToken_capabilities,
    participantToken_duration,
    participantToken_expirationTime,
    participantToken_participantId,
    participantToken_token,
    participantToken_userId,

    -- ** ParticipantTokenConfiguration
    participantTokenConfiguration_attributes,
    participantTokenConfiguration_capabilities,
    participantTokenConfiguration_duration,
    participantTokenConfiguration_userId,

    -- ** Stage
    stage_activeSessionId,
    stage_name,
    stage_tags,
    stage_arn,

    -- ** StageSession
    stageSession_endTime,
    stageSession_sessionId,
    stageSession_startTime,

    -- ** StageSessionSummary
    stageSessionSummary_endTime,
    stageSessionSummary_sessionId,
    stageSessionSummary_startTime,

    -- ** StageSummary
    stageSummary_activeSessionId,
    stageSummary_name,
    stageSummary_tags,
    stageSummary_arn,
  )
where

import Amazonka.IVSRealtime.CreateParticipantToken
import Amazonka.IVSRealtime.CreateStage
import Amazonka.IVSRealtime.DeleteStage
import Amazonka.IVSRealtime.DisconnectParticipant
import Amazonka.IVSRealtime.GetParticipant
import Amazonka.IVSRealtime.GetStage
import Amazonka.IVSRealtime.GetStageSession
import Amazonka.IVSRealtime.ListParticipantEvents
import Amazonka.IVSRealtime.ListParticipants
import Amazonka.IVSRealtime.ListStageSessions
import Amazonka.IVSRealtime.ListStages
import Amazonka.IVSRealtime.ListTagsForResource
import Amazonka.IVSRealtime.TagResource
import Amazonka.IVSRealtime.Types.Event
import Amazonka.IVSRealtime.Types.Participant
import Amazonka.IVSRealtime.Types.ParticipantSummary
import Amazonka.IVSRealtime.Types.ParticipantToken
import Amazonka.IVSRealtime.Types.ParticipantTokenConfiguration
import Amazonka.IVSRealtime.Types.Stage
import Amazonka.IVSRealtime.Types.StageSession
import Amazonka.IVSRealtime.Types.StageSessionSummary
import Amazonka.IVSRealtime.Types.StageSummary
import Amazonka.IVSRealtime.UntagResource
import Amazonka.IVSRealtime.UpdateStage
