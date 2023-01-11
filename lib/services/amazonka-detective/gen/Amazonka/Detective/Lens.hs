{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Detective.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Detective.Lens
  ( -- * Operations

    -- ** AcceptInvitation
    acceptInvitation_graphArn,

    -- ** BatchGetGraphMemberDatasources
    batchGetGraphMemberDatasources_graphArn,
    batchGetGraphMemberDatasources_accountIds,
    batchGetGraphMemberDatasourcesResponse_memberDatasources,
    batchGetGraphMemberDatasourcesResponse_unprocessedAccounts,
    batchGetGraphMemberDatasourcesResponse_httpStatus,

    -- ** BatchGetMembershipDatasources
    batchGetMembershipDatasources_graphArns,
    batchGetMembershipDatasourcesResponse_membershipDatasources,
    batchGetMembershipDatasourcesResponse_unprocessedGraphs,
    batchGetMembershipDatasourcesResponse_httpStatus,

    -- ** CreateGraph
    createGraph_tags,
    createGraphResponse_graphArn,
    createGraphResponse_httpStatus,

    -- ** CreateMembers
    createMembers_disableEmailNotification,
    createMembers_message,
    createMembers_graphArn,
    createMembers_accounts,
    createMembersResponse_members,
    createMembersResponse_unprocessedAccounts,
    createMembersResponse_httpStatus,

    -- ** DeleteGraph
    deleteGraph_graphArn,

    -- ** DeleteMembers
    deleteMembers_graphArn,
    deleteMembers_accountIds,
    deleteMembersResponse_accountIds,
    deleteMembersResponse_unprocessedAccounts,
    deleteMembersResponse_httpStatus,

    -- ** DescribeOrganizationConfiguration
    describeOrganizationConfiguration_graphArn,
    describeOrganizationConfigurationResponse_autoEnable,
    describeOrganizationConfigurationResponse_httpStatus,

    -- ** DisableOrganizationAdminAccount

    -- ** DisassociateMembership
    disassociateMembership_graphArn,

    -- ** EnableOrganizationAdminAccount
    enableOrganizationAdminAccount_accountId,

    -- ** GetMembers
    getMembers_graphArn,
    getMembers_accountIds,
    getMembersResponse_memberDetails,
    getMembersResponse_unprocessedAccounts,
    getMembersResponse_httpStatus,

    -- ** ListDatasourcePackages
    listDatasourcePackages_maxResults,
    listDatasourcePackages_nextToken,
    listDatasourcePackages_graphArn,
    listDatasourcePackagesResponse_datasourcePackages,
    listDatasourcePackagesResponse_nextToken,
    listDatasourcePackagesResponse_httpStatus,

    -- ** ListGraphs
    listGraphs_maxResults,
    listGraphs_nextToken,
    listGraphsResponse_graphList,
    listGraphsResponse_nextToken,
    listGraphsResponse_httpStatus,

    -- ** ListInvitations
    listInvitations_maxResults,
    listInvitations_nextToken,
    listInvitationsResponse_invitations,
    listInvitationsResponse_nextToken,
    listInvitationsResponse_httpStatus,

    -- ** ListMembers
    listMembers_maxResults,
    listMembers_nextToken,
    listMembers_graphArn,
    listMembersResponse_memberDetails,
    listMembersResponse_nextToken,
    listMembersResponse_httpStatus,

    -- ** ListOrganizationAdminAccounts
    listOrganizationAdminAccounts_maxResults,
    listOrganizationAdminAccounts_nextToken,
    listOrganizationAdminAccountsResponse_administrators,
    listOrganizationAdminAccountsResponse_nextToken,
    listOrganizationAdminAccountsResponse_httpStatus,

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

    -- ** UpdateDatasourcePackages
    updateDatasourcePackages_graphArn,
    updateDatasourcePackages_datasourcePackages,

    -- ** UpdateOrganizationConfiguration
    updateOrganizationConfiguration_autoEnable,
    updateOrganizationConfiguration_graphArn,

    -- * Types

    -- ** Account
    account_accountId,
    account_emailAddress,

    -- ** Administrator
    administrator_accountId,
    administrator_delegationTime,
    administrator_graphArn,

    -- ** DatasourcePackageIngestDetail
    datasourcePackageIngestDetail_datasourcePackageIngestState,
    datasourcePackageIngestDetail_lastIngestStateChange,

    -- ** DatasourcePackageUsageInfo
    datasourcePackageUsageInfo_volumeUsageInBytes,
    datasourcePackageUsageInfo_volumeUsageUpdateTime,

    -- ** Graph
    graph_arn,
    graph_createdTime,

    -- ** MemberDetail
    memberDetail_accountId,
    memberDetail_administratorId,
    memberDetail_datasourcePackageIngestStates,
    memberDetail_disabledReason,
    memberDetail_emailAddress,
    memberDetail_graphArn,
    memberDetail_invitationType,
    memberDetail_invitedTime,
    memberDetail_masterId,
    memberDetail_percentOfGraphUtilization,
    memberDetail_percentOfGraphUtilizationUpdatedTime,
    memberDetail_status,
    memberDetail_updatedTime,
    memberDetail_volumeUsageByDatasourcePackage,
    memberDetail_volumeUsageInBytes,
    memberDetail_volumeUsageUpdatedTime,

    -- ** MembershipDatasources
    membershipDatasources_accountId,
    membershipDatasources_datasourcePackageIngestHistory,
    membershipDatasources_graphArn,

    -- ** TimestampForCollection
    timestampForCollection_timestamp,

    -- ** UnprocessedAccount
    unprocessedAccount_accountId,
    unprocessedAccount_reason,

    -- ** UnprocessedGraph
    unprocessedGraph_graphArn,
    unprocessedGraph_reason,
  )
where

import Amazonka.Detective.AcceptInvitation
import Amazonka.Detective.BatchGetGraphMemberDatasources
import Amazonka.Detective.BatchGetMembershipDatasources
import Amazonka.Detective.CreateGraph
import Amazonka.Detective.CreateMembers
import Amazonka.Detective.DeleteGraph
import Amazonka.Detective.DeleteMembers
import Amazonka.Detective.DescribeOrganizationConfiguration
import Amazonka.Detective.DisableOrganizationAdminAccount
import Amazonka.Detective.DisassociateMembership
import Amazonka.Detective.EnableOrganizationAdminAccount
import Amazonka.Detective.GetMembers
import Amazonka.Detective.ListDatasourcePackages
import Amazonka.Detective.ListGraphs
import Amazonka.Detective.ListInvitations
import Amazonka.Detective.ListMembers
import Amazonka.Detective.ListOrganizationAdminAccounts
import Amazonka.Detective.ListTagsForResource
import Amazonka.Detective.RejectInvitation
import Amazonka.Detective.StartMonitoringMember
import Amazonka.Detective.TagResource
import Amazonka.Detective.Types.Account
import Amazonka.Detective.Types.Administrator
import Amazonka.Detective.Types.DatasourcePackageIngestDetail
import Amazonka.Detective.Types.DatasourcePackageUsageInfo
import Amazonka.Detective.Types.Graph
import Amazonka.Detective.Types.MemberDetail
import Amazonka.Detective.Types.MembershipDatasources
import Amazonka.Detective.Types.TimestampForCollection
import Amazonka.Detective.Types.UnprocessedAccount
import Amazonka.Detective.Types.UnprocessedGraph
import Amazonka.Detective.UntagResource
import Amazonka.Detective.UpdateDatasourcePackages
import Amazonka.Detective.UpdateOrganizationConfiguration
