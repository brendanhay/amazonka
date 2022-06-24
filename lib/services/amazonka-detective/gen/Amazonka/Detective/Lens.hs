{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Detective.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Detective.Lens
  ( -- * Operations

    -- ** AcceptInvitation
    acceptInvitation_graphArn,

    -- ** CreateGraph
    createGraph_tags,
    createGraphResponse_graphArn,
    createGraphResponse_httpStatus,

    -- ** CreateMembers
    createMembers_message,
    createMembers_disableEmailNotification,
    createMembers_graphArn,
    createMembers_accounts,
    createMembersResponse_unprocessedAccounts,
    createMembersResponse_members,
    createMembersResponse_httpStatus,

    -- ** DeleteGraph
    deleteGraph_graphArn,

    -- ** DeleteMembers
    deleteMembers_graphArn,
    deleteMembers_accountIds,
    deleteMembersResponse_accountIds,
    deleteMembersResponse_unprocessedAccounts,
    deleteMembersResponse_httpStatus,

    -- ** DisassociateMembership
    disassociateMembership_graphArn,

    -- ** GetMembers
    getMembers_graphArn,
    getMembers_accountIds,
    getMembersResponse_unprocessedAccounts,
    getMembersResponse_memberDetails,
    getMembersResponse_httpStatus,

    -- ** ListGraphs
    listGraphs_nextToken,
    listGraphs_maxResults,
    listGraphsResponse_nextToken,
    listGraphsResponse_graphList,
    listGraphsResponse_httpStatus,

    -- ** ListInvitations
    listInvitations_nextToken,
    listInvitations_maxResults,
    listInvitationsResponse_invitations,
    listInvitationsResponse_nextToken,
    listInvitationsResponse_httpStatus,

    -- ** ListMembers
    listMembers_nextToken,
    listMembers_maxResults,
    listMembers_graphArn,
    listMembersResponse_nextToken,
    listMembersResponse_memberDetails,
    listMembersResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** RejectInvitation
    rejectInvitation_graphArn,

    -- ** StartMonitoringMember
    startMonitoringMember_graphArn,
    startMonitoringMember_accountId,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- * Types

    -- ** Account
    account_accountId,
    account_emailAddress,

    -- ** Graph
    graph_createdTime,
    graph_arn,

    -- ** MemberDetail
    memberDetail_percentOfGraphUtilization,
    memberDetail_volumeUsageInBytes,
    memberDetail_invitedTime,
    memberDetail_graphArn,
    memberDetail_administratorId,
    memberDetail_disabledReason,
    memberDetail_status,
    memberDetail_percentOfGraphUtilizationUpdatedTime,
    memberDetail_accountId,
    memberDetail_volumeUsageUpdatedTime,
    memberDetail_masterId,
    memberDetail_emailAddress,
    memberDetail_updatedTime,

    -- ** UnprocessedAccount
    unprocessedAccount_accountId,
    unprocessedAccount_reason,
  )
where

import Amazonka.Detective.AcceptInvitation
import Amazonka.Detective.CreateGraph
import Amazonka.Detective.CreateMembers
import Amazonka.Detective.DeleteGraph
import Amazonka.Detective.DeleteMembers
import Amazonka.Detective.DisassociateMembership
import Amazonka.Detective.GetMembers
import Amazonka.Detective.ListGraphs
import Amazonka.Detective.ListInvitations
import Amazonka.Detective.ListMembers
import Amazonka.Detective.ListTagsForResource
import Amazonka.Detective.RejectInvitation
import Amazonka.Detective.StartMonitoringMember
import Amazonka.Detective.TagResource
import Amazonka.Detective.Types.Account
import Amazonka.Detective.Types.Graph
import Amazonka.Detective.Types.MemberDetail
import Amazonka.Detective.Types.UnprocessedAccount
import Amazonka.Detective.UntagResource
