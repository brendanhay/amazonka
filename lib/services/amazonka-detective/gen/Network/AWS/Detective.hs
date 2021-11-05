{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.Detective
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2018-10-26@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Detective uses machine learning and purpose-built visualizations to help
-- you analyze and investigate security issues across your Amazon Web
-- Services (AWS) workloads. Detective automatically extracts time-based
-- events such as login attempts, API calls, and network traffic from AWS
-- CloudTrail and Amazon Virtual Private Cloud (Amazon VPC) flow logs. It
-- also extracts findings detected by Amazon GuardDuty.
--
-- The Detective API primarily supports the creation and management of
-- behavior graphs. A behavior graph contains the extracted data from a set
-- of member accounts, and is created and managed by an administrator
-- account.
--
-- Every behavior graph is specific to a Region. You can only use the API
-- to manage graphs that belong to the Region that is associated with the
-- currently selected endpoint.
--
-- A Detective administrator account can use the Detective API to do the
-- following:
--
-- -   Enable and disable Detective. Enabling Detective creates a new
--     behavior graph.
--
-- -   View the list of member accounts in a behavior graph.
--
-- -   Add member accounts to a behavior graph.
--
-- -   Remove member accounts from a behavior graph.
--
-- A member account can use the Detective API to do the following:
--
-- -   View the list of behavior graphs that they are invited to.
--
-- -   Accept an invitation to contribute to a behavior graph.
--
-- -   Decline an invitation to contribute to a behavior graph.
--
-- -   Remove their account from a behavior graph.
--
-- All API actions are logged as CloudTrail events. See
-- <https://docs.aws.amazon.com/detective/latest/adminguide/logging-using-cloudtrail.html Logging Detective API Calls with CloudTrail>.
--
-- We replaced the term \"master account\" with the term \"administrator
-- account.\" An administrator account is used to centrally manage multiple
-- accounts. In the case of Detective, the administrator account manages
-- the accounts in their behavior graph.
module Network.AWS.Detective
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ValidationException
    _ValidationException,

    -- ** ConflictException
    _ConflictException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** StartMonitoringMember
    StartMonitoringMember (StartMonitoringMember'),
    newStartMonitoringMember,
    StartMonitoringMemberResponse (StartMonitoringMemberResponse'),
    newStartMonitoringMemberResponse,

    -- ** DeleteMembers
    DeleteMembers (DeleteMembers'),
    newDeleteMembers,
    DeleteMembersResponse (DeleteMembersResponse'),
    newDeleteMembersResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** DeleteGraph
    DeleteGraph (DeleteGraph'),
    newDeleteGraph,
    DeleteGraphResponse (DeleteGraphResponse'),
    newDeleteGraphResponse,

    -- ** ListInvitations
    ListInvitations (ListInvitations'),
    newListInvitations,
    ListInvitationsResponse (ListInvitationsResponse'),
    newListInvitationsResponse,

    -- ** DisassociateMembership
    DisassociateMembership (DisassociateMembership'),
    newDisassociateMembership,
    DisassociateMembershipResponse (DisassociateMembershipResponse'),
    newDisassociateMembershipResponse,

    -- ** AcceptInvitation
    AcceptInvitation (AcceptInvitation'),
    newAcceptInvitation,
    AcceptInvitationResponse (AcceptInvitationResponse'),
    newAcceptInvitationResponse,

    -- ** ListMembers
    ListMembers (ListMembers'),
    newListMembers,
    ListMembersResponse (ListMembersResponse'),
    newListMembersResponse,

    -- ** CreateMembers
    CreateMembers (CreateMembers'),
    newCreateMembers,
    CreateMembersResponse (CreateMembersResponse'),
    newCreateMembersResponse,

    -- ** GetMembers
    GetMembers (GetMembers'),
    newGetMembers,
    GetMembersResponse (GetMembersResponse'),
    newGetMembersResponse,

    -- ** ListGraphs
    ListGraphs (ListGraphs'),
    newListGraphs,
    ListGraphsResponse (ListGraphsResponse'),
    newListGraphsResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** CreateGraph
    CreateGraph (CreateGraph'),
    newCreateGraph,
    CreateGraphResponse (CreateGraphResponse'),
    newCreateGraphResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** RejectInvitation
    RejectInvitation (RejectInvitation'),
    newRejectInvitation,
    RejectInvitationResponse (RejectInvitationResponse'),
    newRejectInvitationResponse,

    -- * Types

    -- ** MemberDisabledReason
    MemberDisabledReason (..),

    -- ** MemberStatus
    MemberStatus (..),

    -- ** Account
    Account (Account'),
    newAccount,

    -- ** Graph
    Graph (Graph'),
    newGraph,

    -- ** MemberDetail
    MemberDetail (MemberDetail'),
    newMemberDetail,

    -- ** UnprocessedAccount
    UnprocessedAccount (UnprocessedAccount'),
    newUnprocessedAccount,
  )
where

import Network.AWS.Detective.AcceptInvitation
import Network.AWS.Detective.CreateGraph
import Network.AWS.Detective.CreateMembers
import Network.AWS.Detective.DeleteGraph
import Network.AWS.Detective.DeleteMembers
import Network.AWS.Detective.DisassociateMembership
import Network.AWS.Detective.GetMembers
import Network.AWS.Detective.Lens
import Network.AWS.Detective.ListGraphs
import Network.AWS.Detective.ListInvitations
import Network.AWS.Detective.ListMembers
import Network.AWS.Detective.ListTagsForResource
import Network.AWS.Detective.RejectInvitation
import Network.AWS.Detective.StartMonitoringMember
import Network.AWS.Detective.TagResource
import Network.AWS.Detective.Types
import Network.AWS.Detective.UntagResource
import Network.AWS.Detective.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Detective'.

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
