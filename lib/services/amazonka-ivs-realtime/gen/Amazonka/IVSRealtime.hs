{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.IVSRealtime
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2020-07-14@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- __Introduction__
--
-- The Amazon Interactive Video Service (IVS) stage API is REST compatible,
-- using a standard HTTP API and an AWS EventBridge event stream for
-- responses. JSON is used for both requests and responses, including
-- errors.
--
-- Terminology:
--
-- -   The IVS stage API sometimes is referred to as the IVS /RealTime/
--     API.
--
-- -   A /participant token/ is an authorization token used to
--     publish\/subscribe to a stage.
--
-- -   A /participant object/ represents participants (people) in the stage
--     and contains information about them. When a token is created, it
--     includes a participant ID; when a participant uses that token to
--     join a stage, the participant is associated with that participant ID
--     There is a 1:1 mapping between participant tokens and participants.
--
-- __Resources__
--
-- The following resources contain information about your IVS live stream
-- (see
-- <https://docs.aws.amazon.com/ivs/latest/userguide/getting-started.html Getting Started with Amazon IVS>):
--
-- -   __Stage__ — A stage is a virtual space where multiple participants
--     can exchange audio and video in real time.
--
-- __Tagging__
--
-- A /tag/ is a metadata label that you assign to an AWS resource. A tag
-- comprises a /key/ and a /value/, both set by you. For example, you might
-- set a tag as @topic:nature@ to label a particular video category. See
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>
-- for more information, including restrictions that apply to tags and
-- \"Tag naming limits and requirements\"; Amazon IVS stages has no
-- service-specific constraints beyond what is documented there.
--
-- Tags can help you identify and organize your AWS resources. For example,
-- you can use the same tag for different resources to indicate that they
-- are related. You can also use tags to manage access (see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_tags.html Access Tags>).
--
-- The Amazon IVS stage API has these tag-related endpoints: TagResource,
-- UntagResource, and ListTagsForResource. The following resource supports
-- tagging: Stage.
--
-- At most 50 tags can be applied to a resource.
--
-- __Stages Endpoints__
--
-- -   CreateParticipantToken — Creates an additional token for a specified
--     stage. This can be done after stage creation or when tokens expire.
--
-- -   CreateStage — Creates a new stage (and optionally participant
--     tokens).
--
-- -   DeleteStage — Shuts down and deletes the specified stage
--     (disconnecting all participants).
--
-- -   DisconnectParticipant — Disconnects a specified participant and
--     revokes the participant permanently from a specified stage.
--
-- -   GetParticipant — Gets information about the specified participant
--     token.
--
-- -   GetStage — Gets information for the specified stage.
--
-- -   GetStageSession — Gets information for the specified stage session.
--
-- -   ListParticipantEvents — Lists events for a specified participant
--     that occurred during a specified stage session.
--
-- -   ListParticipants — Lists all participants in a specified stage
--     session.
--
-- -   ListStages — Gets summary information about all stages in your
--     account, in the AWS region where the API request is processed.
--
-- -   ListStageSessions — Gets all sessions for a specified stage.
--
-- -   UpdateStage — Updates a stage’s configuration.
--
-- __Tags Endpoints__
--
-- -   ListTagsForResource — Gets information about AWS tags for the
--     specified ARN.
--
-- -   TagResource — Adds or updates tags for the AWS resource with the
--     specified ARN.
--
-- -   UntagResource — Removes tags from the resource with the specified
--     ARN.
module Amazonka.IVSRealtime
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** PendingVerification
    _PendingVerification,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateParticipantToken
    CreateParticipantToken (CreateParticipantToken'),
    newCreateParticipantToken,
    CreateParticipantTokenResponse (CreateParticipantTokenResponse'),
    newCreateParticipantTokenResponse,

    -- ** CreateStage
    CreateStage (CreateStage'),
    newCreateStage,
    CreateStageResponse (CreateStageResponse'),
    newCreateStageResponse,

    -- ** DeleteStage
    DeleteStage (DeleteStage'),
    newDeleteStage,
    DeleteStageResponse (DeleteStageResponse'),
    newDeleteStageResponse,

    -- ** DisconnectParticipant
    DisconnectParticipant (DisconnectParticipant'),
    newDisconnectParticipant,
    DisconnectParticipantResponse (DisconnectParticipantResponse'),
    newDisconnectParticipantResponse,

    -- ** GetParticipant
    GetParticipant (GetParticipant'),
    newGetParticipant,
    GetParticipantResponse (GetParticipantResponse'),
    newGetParticipantResponse,

    -- ** GetStage
    GetStage (GetStage'),
    newGetStage,
    GetStageResponse (GetStageResponse'),
    newGetStageResponse,

    -- ** GetStageSession
    GetStageSession (GetStageSession'),
    newGetStageSession,
    GetStageSessionResponse (GetStageSessionResponse'),
    newGetStageSessionResponse,

    -- ** ListParticipantEvents
    ListParticipantEvents (ListParticipantEvents'),
    newListParticipantEvents,
    ListParticipantEventsResponse (ListParticipantEventsResponse'),
    newListParticipantEventsResponse,

    -- ** ListParticipants
    ListParticipants (ListParticipants'),
    newListParticipants,
    ListParticipantsResponse (ListParticipantsResponse'),
    newListParticipantsResponse,

    -- ** ListStageSessions
    ListStageSessions (ListStageSessions'),
    newListStageSessions,
    ListStageSessionsResponse (ListStageSessionsResponse'),
    newListStageSessionsResponse,

    -- ** ListStages
    ListStages (ListStages'),
    newListStages,
    ListStagesResponse (ListStagesResponse'),
    newListStagesResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

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

    -- ** UpdateStage
    UpdateStage (UpdateStage'),
    newUpdateStage,
    UpdateStageResponse (UpdateStageResponse'),
    newUpdateStageResponse,

    -- * Types

    -- ** EventErrorCode
    EventErrorCode (..),

    -- ** EventName
    EventName (..),

    -- ** ParticipantState
    ParticipantState (..),

    -- ** ParticipantTokenCapability
    ParticipantTokenCapability (..),

    -- ** Event
    Event (Event'),
    newEvent,

    -- ** Participant
    Participant (Participant'),
    newParticipant,

    -- ** ParticipantSummary
    ParticipantSummary (ParticipantSummary'),
    newParticipantSummary,

    -- ** ParticipantToken
    ParticipantToken (ParticipantToken'),
    newParticipantToken,

    -- ** ParticipantTokenConfiguration
    ParticipantTokenConfiguration (ParticipantTokenConfiguration'),
    newParticipantTokenConfiguration,

    -- ** Stage
    Stage (Stage'),
    newStage,

    -- ** StageSession
    StageSession (StageSession'),
    newStageSession,

    -- ** StageSessionSummary
    StageSessionSummary (StageSessionSummary'),
    newStageSessionSummary,

    -- ** StageSummary
    StageSummary (StageSummary'),
    newStageSummary,
  )
where

import Amazonka.IVSRealtime.CreateParticipantToken
import Amazonka.IVSRealtime.CreateStage
import Amazonka.IVSRealtime.DeleteStage
import Amazonka.IVSRealtime.DisconnectParticipant
import Amazonka.IVSRealtime.GetParticipant
import Amazonka.IVSRealtime.GetStage
import Amazonka.IVSRealtime.GetStageSession
import Amazonka.IVSRealtime.Lens
import Amazonka.IVSRealtime.ListParticipantEvents
import Amazonka.IVSRealtime.ListParticipants
import Amazonka.IVSRealtime.ListStageSessions
import Amazonka.IVSRealtime.ListStages
import Amazonka.IVSRealtime.ListTagsForResource
import Amazonka.IVSRealtime.TagResource
import Amazonka.IVSRealtime.Types
import Amazonka.IVSRealtime.UntagResource
import Amazonka.IVSRealtime.UpdateStage
import Amazonka.IVSRealtime.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'IVSRealtime'.

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
