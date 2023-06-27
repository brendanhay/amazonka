{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.QuickSight
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.QuickSight where

import Amazonka.QuickSight
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.QuickSight.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCancelIngestion $
--             newCancelIngestion
--
--         , requestCreateAccountCustomization $
--             newCreateAccountCustomization
--
--         , requestCreateAccountSubscription $
--             newCreateAccountSubscription
--
--         , requestCreateAnalysis $
--             newCreateAnalysis
--
--         , requestCreateDashboard $
--             newCreateDashboard
--
--         , requestCreateDataSet $
--             newCreateDataSet
--
--         , requestCreateDataSource $
--             newCreateDataSource
--
--         , requestCreateFolder $
--             newCreateFolder
--
--         , requestCreateFolderMembership $
--             newCreateFolderMembership
--
--         , requestCreateGroup $
--             newCreateGroup
--
--         , requestCreateGroupMembership $
--             newCreateGroupMembership
--
--         , requestCreateIAMPolicyAssignment $
--             newCreateIAMPolicyAssignment
--
--         , requestCreateIngestion $
--             newCreateIngestion
--
--         , requestCreateNamespace $
--             newCreateNamespace
--
--         , requestCreateRefreshSchedule $
--             newCreateRefreshSchedule
--
--         , requestCreateTemplate $
--             newCreateTemplate
--
--         , requestCreateTemplateAlias $
--             newCreateTemplateAlias
--
--         , requestCreateTheme $
--             newCreateTheme
--
--         , requestCreateThemeAlias $
--             newCreateThemeAlias
--
--         , requestCreateTopic $
--             newCreateTopic
--
--         , requestCreateTopicRefreshSchedule $
--             newCreateTopicRefreshSchedule
--
--         , requestCreateVPCConnection $
--             newCreateVPCConnection
--
--         , requestDeleteAccountCustomization $
--             newDeleteAccountCustomization
--
--         , requestDeleteAccountSubscription $
--             newDeleteAccountSubscription
--
--         , requestDeleteAnalysis $
--             newDeleteAnalysis
--
--         , requestDeleteDashboard $
--             newDeleteDashboard
--
--         , requestDeleteDataSet $
--             newDeleteDataSet
--
--         , requestDeleteDataSetRefreshProperties $
--             newDeleteDataSetRefreshProperties
--
--         , requestDeleteDataSource $
--             newDeleteDataSource
--
--         , requestDeleteFolder $
--             newDeleteFolder
--
--         , requestDeleteFolderMembership $
--             newDeleteFolderMembership
--
--         , requestDeleteGroup $
--             newDeleteGroup
--
--         , requestDeleteGroupMembership $
--             newDeleteGroupMembership
--
--         , requestDeleteIAMPolicyAssignment $
--             newDeleteIAMPolicyAssignment
--
--         , requestDeleteNamespace $
--             newDeleteNamespace
--
--         , requestDeleteRefreshSchedule $
--             newDeleteRefreshSchedule
--
--         , requestDeleteTemplate $
--             newDeleteTemplate
--
--         , requestDeleteTemplateAlias $
--             newDeleteTemplateAlias
--
--         , requestDeleteTheme $
--             newDeleteTheme
--
--         , requestDeleteThemeAlias $
--             newDeleteThemeAlias
--
--         , requestDeleteTopic $
--             newDeleteTopic
--
--         , requestDeleteTopicRefreshSchedule $
--             newDeleteTopicRefreshSchedule
--
--         , requestDeleteUser $
--             newDeleteUser
--
--         , requestDeleteUserByPrincipalId $
--             newDeleteUserByPrincipalId
--
--         , requestDeleteVPCConnection $
--             newDeleteVPCConnection
--
--         , requestDescribeAccountCustomization $
--             newDescribeAccountCustomization
--
--         , requestDescribeAccountSettings $
--             newDescribeAccountSettings
--
--         , requestDescribeAccountSubscription $
--             newDescribeAccountSubscription
--
--         , requestDescribeAnalysis $
--             newDescribeAnalysis
--
--         , requestDescribeAnalysisDefinition $
--             newDescribeAnalysisDefinition
--
--         , requestDescribeAnalysisPermissions $
--             newDescribeAnalysisPermissions
--
--         , requestDescribeAssetBundleExportJob $
--             newDescribeAssetBundleExportJob
--
--         , requestDescribeAssetBundleImportJob $
--             newDescribeAssetBundleImportJob
--
--         , requestDescribeDashboard $
--             newDescribeDashboard
--
--         , requestDescribeDashboardDefinition $
--             newDescribeDashboardDefinition
--
--         , requestDescribeDashboardPermissions $
--             newDescribeDashboardPermissions
--
--         , requestDescribeDataSet $
--             newDescribeDataSet
--
--         , requestDescribeDataSetPermissions $
--             newDescribeDataSetPermissions
--
--         , requestDescribeDataSetRefreshProperties $
--             newDescribeDataSetRefreshProperties
--
--         , requestDescribeDataSource $
--             newDescribeDataSource
--
--         , requestDescribeDataSourcePermissions $
--             newDescribeDataSourcePermissions
--
--         , requestDescribeFolder $
--             newDescribeFolder
--
--         , requestDescribeFolderPermissions $
--             newDescribeFolderPermissions
--
--         , requestDescribeFolderResolvedPermissions $
--             newDescribeFolderResolvedPermissions
--
--         , requestDescribeGroup $
--             newDescribeGroup
--
--         , requestDescribeGroupMembership $
--             newDescribeGroupMembership
--
--         , requestDescribeIAMPolicyAssignment $
--             newDescribeIAMPolicyAssignment
--
--         , requestDescribeIngestion $
--             newDescribeIngestion
--
--         , requestDescribeIpRestriction $
--             newDescribeIpRestriction
--
--         , requestDescribeNamespace $
--             newDescribeNamespace
--
--         , requestDescribeRefreshSchedule $
--             newDescribeRefreshSchedule
--
--         , requestDescribeTemplate $
--             newDescribeTemplate
--
--         , requestDescribeTemplateAlias $
--             newDescribeTemplateAlias
--
--         , requestDescribeTemplateDefinition $
--             newDescribeTemplateDefinition
--
--         , requestDescribeTemplatePermissions $
--             newDescribeTemplatePermissions
--
--         , requestDescribeTheme $
--             newDescribeTheme
--
--         , requestDescribeThemeAlias $
--             newDescribeThemeAlias
--
--         , requestDescribeThemePermissions $
--             newDescribeThemePermissions
--
--         , requestDescribeTopic $
--             newDescribeTopic
--
--         , requestDescribeTopicPermissions $
--             newDescribeTopicPermissions
--
--         , requestDescribeTopicRefresh $
--             newDescribeTopicRefresh
--
--         , requestDescribeTopicRefreshSchedule $
--             newDescribeTopicRefreshSchedule
--
--         , requestDescribeUser $
--             newDescribeUser
--
--         , requestDescribeVPCConnection $
--             newDescribeVPCConnection
--
--         , requestGenerateEmbedUrlForAnonymousUser $
--             newGenerateEmbedUrlForAnonymousUser
--
--         , requestGenerateEmbedUrlForRegisteredUser $
--             newGenerateEmbedUrlForRegisteredUser
--
--         , requestGetDashboardEmbedUrl $
--             newGetDashboardEmbedUrl
--
--         , requestGetSessionEmbedUrl $
--             newGetSessionEmbedUrl
--
--         , requestListAnalyses $
--             newListAnalyses
--
--         , requestListAssetBundleExportJobs $
--             newListAssetBundleExportJobs
--
--         , requestListAssetBundleImportJobs $
--             newListAssetBundleImportJobs
--
--         , requestListDashboardVersions $
--             newListDashboardVersions
--
--         , requestListDashboards $
--             newListDashboards
--
--         , requestListDataSets $
--             newListDataSets
--
--         , requestListDataSources $
--             newListDataSources
--
--         , requestListFolderMembers $
--             newListFolderMembers
--
--         , requestListFolders $
--             newListFolders
--
--         , requestListGroupMemberships $
--             newListGroupMemberships
--
--         , requestListGroups $
--             newListGroups
--
--         , requestListIAMPolicyAssignments $
--             newListIAMPolicyAssignments
--
--         , requestListIAMPolicyAssignmentsForUser $
--             newListIAMPolicyAssignmentsForUser
--
--         , requestListIngestions $
--             newListIngestions
--
--         , requestListNamespaces $
--             newListNamespaces
--
--         , requestListRefreshSchedules $
--             newListRefreshSchedules
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListTemplateAliases $
--             newListTemplateAliases
--
--         , requestListTemplateVersions $
--             newListTemplateVersions
--
--         , requestListTemplates $
--             newListTemplates
--
--         , requestListThemeAliases $
--             newListThemeAliases
--
--         , requestListThemeVersions $
--             newListThemeVersions
--
--         , requestListThemes $
--             newListThemes
--
--         , requestListTopicRefreshSchedules $
--             newListTopicRefreshSchedules
--
--         , requestListTopics $
--             newListTopics
--
--         , requestListUserGroups $
--             newListUserGroups
--
--         , requestListUsers $
--             newListUsers
--
--         , requestListVPCConnections $
--             newListVPCConnections
--
--         , requestPutDataSetRefreshProperties $
--             newPutDataSetRefreshProperties
--
--         , requestRegisterUser $
--             newRegisterUser
--
--         , requestRestoreAnalysis $
--             newRestoreAnalysis
--
--         , requestSearchAnalyses $
--             newSearchAnalyses
--
--         , requestSearchDashboards $
--             newSearchDashboards
--
--         , requestSearchDataSets $
--             newSearchDataSets
--
--         , requestSearchDataSources $
--             newSearchDataSources
--
--         , requestSearchFolders $
--             newSearchFolders
--
--         , requestSearchGroups $
--             newSearchGroups
--
--         , requestStartAssetBundleExportJob $
--             newStartAssetBundleExportJob
--
--         , requestStartAssetBundleImportJob $
--             newStartAssetBundleImportJob
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateAccountCustomization $
--             newUpdateAccountCustomization
--
--         , requestUpdateAccountSettings $
--             newUpdateAccountSettings
--
--         , requestUpdateAnalysis $
--             newUpdateAnalysis
--
--         , requestUpdateAnalysisPermissions $
--             newUpdateAnalysisPermissions
--
--         , requestUpdateDashboard $
--             newUpdateDashboard
--
--         , requestUpdateDashboardPermissions $
--             newUpdateDashboardPermissions
--
--         , requestUpdateDashboardPublishedVersion $
--             newUpdateDashboardPublishedVersion
--
--         , requestUpdateDataSet $
--             newUpdateDataSet
--
--         , requestUpdateDataSetPermissions $
--             newUpdateDataSetPermissions
--
--         , requestUpdateDataSource $
--             newUpdateDataSource
--
--         , requestUpdateDataSourcePermissions $
--             newUpdateDataSourcePermissions
--
--         , requestUpdateFolder $
--             newUpdateFolder
--
--         , requestUpdateFolderPermissions $
--             newUpdateFolderPermissions
--
--         , requestUpdateGroup $
--             newUpdateGroup
--
--         , requestUpdateIAMPolicyAssignment $
--             newUpdateIAMPolicyAssignment
--
--         , requestUpdateIpRestriction $
--             newUpdateIpRestriction
--
--         , requestUpdatePublicSharingSettings $
--             newUpdatePublicSharingSettings
--
--         , requestUpdateRefreshSchedule $
--             newUpdateRefreshSchedule
--
--         , requestUpdateTemplate $
--             newUpdateTemplate
--
--         , requestUpdateTemplateAlias $
--             newUpdateTemplateAlias
--
--         , requestUpdateTemplatePermissions $
--             newUpdateTemplatePermissions
--
--         , requestUpdateTheme $
--             newUpdateTheme
--
--         , requestUpdateThemeAlias $
--             newUpdateThemeAlias
--
--         , requestUpdateThemePermissions $
--             newUpdateThemePermissions
--
--         , requestUpdateTopic $
--             newUpdateTopic
--
--         , requestUpdateTopicPermissions $
--             newUpdateTopicPermissions
--
--         , requestUpdateTopicRefreshSchedule $
--             newUpdateTopicRefreshSchedule
--
--         , requestUpdateUser $
--             newUpdateUser
--
--         , requestUpdateVPCConnection $
--             newUpdateVPCConnection
--
--           ]

--     , testGroup "response"
--         [ responseCancelIngestion $
--             newCancelIngestionResponse
--
--         , responseCreateAccountCustomization $
--             newCreateAccountCustomizationResponse
--
--         , responseCreateAccountSubscription $
--             newCreateAccountSubscriptionResponse
--
--         , responseCreateAnalysis $
--             newCreateAnalysisResponse
--
--         , responseCreateDashboard $
--             newCreateDashboardResponse
--
--         , responseCreateDataSet $
--             newCreateDataSetResponse
--
--         , responseCreateDataSource $
--             newCreateDataSourceResponse
--
--         , responseCreateFolder $
--             newCreateFolderResponse
--
--         , responseCreateFolderMembership $
--             newCreateFolderMembershipResponse
--
--         , responseCreateGroup $
--             newCreateGroupResponse
--
--         , responseCreateGroupMembership $
--             newCreateGroupMembershipResponse
--
--         , responseCreateIAMPolicyAssignment $
--             newCreateIAMPolicyAssignmentResponse
--
--         , responseCreateIngestion $
--             newCreateIngestionResponse
--
--         , responseCreateNamespace $
--             newCreateNamespaceResponse
--
--         , responseCreateRefreshSchedule $
--             newCreateRefreshScheduleResponse
--
--         , responseCreateTemplate $
--             newCreateTemplateResponse
--
--         , responseCreateTemplateAlias $
--             newCreateTemplateAliasResponse
--
--         , responseCreateTheme $
--             newCreateThemeResponse
--
--         , responseCreateThemeAlias $
--             newCreateThemeAliasResponse
--
--         , responseCreateTopic $
--             newCreateTopicResponse
--
--         , responseCreateTopicRefreshSchedule $
--             newCreateTopicRefreshScheduleResponse
--
--         , responseCreateVPCConnection $
--             newCreateVPCConnectionResponse
--
--         , responseDeleteAccountCustomization $
--             newDeleteAccountCustomizationResponse
--
--         , responseDeleteAccountSubscription $
--             newDeleteAccountSubscriptionResponse
--
--         , responseDeleteAnalysis $
--             newDeleteAnalysisResponse
--
--         , responseDeleteDashboard $
--             newDeleteDashboardResponse
--
--         , responseDeleteDataSet $
--             newDeleteDataSetResponse
--
--         , responseDeleteDataSetRefreshProperties $
--             newDeleteDataSetRefreshPropertiesResponse
--
--         , responseDeleteDataSource $
--             newDeleteDataSourceResponse
--
--         , responseDeleteFolder $
--             newDeleteFolderResponse
--
--         , responseDeleteFolderMembership $
--             newDeleteFolderMembershipResponse
--
--         , responseDeleteGroup $
--             newDeleteGroupResponse
--
--         , responseDeleteGroupMembership $
--             newDeleteGroupMembershipResponse
--
--         , responseDeleteIAMPolicyAssignment $
--             newDeleteIAMPolicyAssignmentResponse
--
--         , responseDeleteNamespace $
--             newDeleteNamespaceResponse
--
--         , responseDeleteRefreshSchedule $
--             newDeleteRefreshScheduleResponse
--
--         , responseDeleteTemplate $
--             newDeleteTemplateResponse
--
--         , responseDeleteTemplateAlias $
--             newDeleteTemplateAliasResponse
--
--         , responseDeleteTheme $
--             newDeleteThemeResponse
--
--         , responseDeleteThemeAlias $
--             newDeleteThemeAliasResponse
--
--         , responseDeleteTopic $
--             newDeleteTopicResponse
--
--         , responseDeleteTopicRefreshSchedule $
--             newDeleteTopicRefreshScheduleResponse
--
--         , responseDeleteUser $
--             newDeleteUserResponse
--
--         , responseDeleteUserByPrincipalId $
--             newDeleteUserByPrincipalIdResponse
--
--         , responseDeleteVPCConnection $
--             newDeleteVPCConnectionResponse
--
--         , responseDescribeAccountCustomization $
--             newDescribeAccountCustomizationResponse
--
--         , responseDescribeAccountSettings $
--             newDescribeAccountSettingsResponse
--
--         , responseDescribeAccountSubscription $
--             newDescribeAccountSubscriptionResponse
--
--         , responseDescribeAnalysis $
--             newDescribeAnalysisResponse
--
--         , responseDescribeAnalysisDefinition $
--             newDescribeAnalysisDefinitionResponse
--
--         , responseDescribeAnalysisPermissions $
--             newDescribeAnalysisPermissionsResponse
--
--         , responseDescribeAssetBundleExportJob $
--             newDescribeAssetBundleExportJobResponse
--
--         , responseDescribeAssetBundleImportJob $
--             newDescribeAssetBundleImportJobResponse
--
--         , responseDescribeDashboard $
--             newDescribeDashboardResponse
--
--         , responseDescribeDashboardDefinition $
--             newDescribeDashboardDefinitionResponse
--
--         , responseDescribeDashboardPermissions $
--             newDescribeDashboardPermissionsResponse
--
--         , responseDescribeDataSet $
--             newDescribeDataSetResponse
--
--         , responseDescribeDataSetPermissions $
--             newDescribeDataSetPermissionsResponse
--
--         , responseDescribeDataSetRefreshProperties $
--             newDescribeDataSetRefreshPropertiesResponse
--
--         , responseDescribeDataSource $
--             newDescribeDataSourceResponse
--
--         , responseDescribeDataSourcePermissions $
--             newDescribeDataSourcePermissionsResponse
--
--         , responseDescribeFolder $
--             newDescribeFolderResponse
--
--         , responseDescribeFolderPermissions $
--             newDescribeFolderPermissionsResponse
--
--         , responseDescribeFolderResolvedPermissions $
--             newDescribeFolderResolvedPermissionsResponse
--
--         , responseDescribeGroup $
--             newDescribeGroupResponse
--
--         , responseDescribeGroupMembership $
--             newDescribeGroupMembershipResponse
--
--         , responseDescribeIAMPolicyAssignment $
--             newDescribeIAMPolicyAssignmentResponse
--
--         , responseDescribeIngestion $
--             newDescribeIngestionResponse
--
--         , responseDescribeIpRestriction $
--             newDescribeIpRestrictionResponse
--
--         , responseDescribeNamespace $
--             newDescribeNamespaceResponse
--
--         , responseDescribeRefreshSchedule $
--             newDescribeRefreshScheduleResponse
--
--         , responseDescribeTemplate $
--             newDescribeTemplateResponse
--
--         , responseDescribeTemplateAlias $
--             newDescribeTemplateAliasResponse
--
--         , responseDescribeTemplateDefinition $
--             newDescribeTemplateDefinitionResponse
--
--         , responseDescribeTemplatePermissions $
--             newDescribeTemplatePermissionsResponse
--
--         , responseDescribeTheme $
--             newDescribeThemeResponse
--
--         , responseDescribeThemeAlias $
--             newDescribeThemeAliasResponse
--
--         , responseDescribeThemePermissions $
--             newDescribeThemePermissionsResponse
--
--         , responseDescribeTopic $
--             newDescribeTopicResponse
--
--         , responseDescribeTopicPermissions $
--             newDescribeTopicPermissionsResponse
--
--         , responseDescribeTopicRefresh $
--             newDescribeTopicRefreshResponse
--
--         , responseDescribeTopicRefreshSchedule $
--             newDescribeTopicRefreshScheduleResponse
--
--         , responseDescribeUser $
--             newDescribeUserResponse
--
--         , responseDescribeVPCConnection $
--             newDescribeVPCConnectionResponse
--
--         , responseGenerateEmbedUrlForAnonymousUser $
--             newGenerateEmbedUrlForAnonymousUserResponse
--
--         , responseGenerateEmbedUrlForRegisteredUser $
--             newGenerateEmbedUrlForRegisteredUserResponse
--
--         , responseGetDashboardEmbedUrl $
--             newGetDashboardEmbedUrlResponse
--
--         , responseGetSessionEmbedUrl $
--             newGetSessionEmbedUrlResponse
--
--         , responseListAnalyses $
--             newListAnalysesResponse
--
--         , responseListAssetBundleExportJobs $
--             newListAssetBundleExportJobsResponse
--
--         , responseListAssetBundleImportJobs $
--             newListAssetBundleImportJobsResponse
--
--         , responseListDashboardVersions $
--             newListDashboardVersionsResponse
--
--         , responseListDashboards $
--             newListDashboardsResponse
--
--         , responseListDataSets $
--             newListDataSetsResponse
--
--         , responseListDataSources $
--             newListDataSourcesResponse
--
--         , responseListFolderMembers $
--             newListFolderMembersResponse
--
--         , responseListFolders $
--             newListFoldersResponse
--
--         , responseListGroupMemberships $
--             newListGroupMembershipsResponse
--
--         , responseListGroups $
--             newListGroupsResponse
--
--         , responseListIAMPolicyAssignments $
--             newListIAMPolicyAssignmentsResponse
--
--         , responseListIAMPolicyAssignmentsForUser $
--             newListIAMPolicyAssignmentsForUserResponse
--
--         , responseListIngestions $
--             newListIngestionsResponse
--
--         , responseListNamespaces $
--             newListNamespacesResponse
--
--         , responseListRefreshSchedules $
--             newListRefreshSchedulesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListTemplateAliases $
--             newListTemplateAliasesResponse
--
--         , responseListTemplateVersions $
--             newListTemplateVersionsResponse
--
--         , responseListTemplates $
--             newListTemplatesResponse
--
--         , responseListThemeAliases $
--             newListThemeAliasesResponse
--
--         , responseListThemeVersions $
--             newListThemeVersionsResponse
--
--         , responseListThemes $
--             newListThemesResponse
--
--         , responseListTopicRefreshSchedules $
--             newListTopicRefreshSchedulesResponse
--
--         , responseListTopics $
--             newListTopicsResponse
--
--         , responseListUserGroups $
--             newListUserGroupsResponse
--
--         , responseListUsers $
--             newListUsersResponse
--
--         , responseListVPCConnections $
--             newListVPCConnectionsResponse
--
--         , responsePutDataSetRefreshProperties $
--             newPutDataSetRefreshPropertiesResponse
--
--         , responseRegisterUser $
--             newRegisterUserResponse
--
--         , responseRestoreAnalysis $
--             newRestoreAnalysisResponse
--
--         , responseSearchAnalyses $
--             newSearchAnalysesResponse
--
--         , responseSearchDashboards $
--             newSearchDashboardsResponse
--
--         , responseSearchDataSets $
--             newSearchDataSetsResponse
--
--         , responseSearchDataSources $
--             newSearchDataSourcesResponse
--
--         , responseSearchFolders $
--             newSearchFoldersResponse
--
--         , responseSearchGroups $
--             newSearchGroupsResponse
--
--         , responseStartAssetBundleExportJob $
--             newStartAssetBundleExportJobResponse
--
--         , responseStartAssetBundleImportJob $
--             newStartAssetBundleImportJobResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateAccountCustomization $
--             newUpdateAccountCustomizationResponse
--
--         , responseUpdateAccountSettings $
--             newUpdateAccountSettingsResponse
--
--         , responseUpdateAnalysis $
--             newUpdateAnalysisResponse
--
--         , responseUpdateAnalysisPermissions $
--             newUpdateAnalysisPermissionsResponse
--
--         , responseUpdateDashboard $
--             newUpdateDashboardResponse
--
--         , responseUpdateDashboardPermissions $
--             newUpdateDashboardPermissionsResponse
--
--         , responseUpdateDashboardPublishedVersion $
--             newUpdateDashboardPublishedVersionResponse
--
--         , responseUpdateDataSet $
--             newUpdateDataSetResponse
--
--         , responseUpdateDataSetPermissions $
--             newUpdateDataSetPermissionsResponse
--
--         , responseUpdateDataSource $
--             newUpdateDataSourceResponse
--
--         , responseUpdateDataSourcePermissions $
--             newUpdateDataSourcePermissionsResponse
--
--         , responseUpdateFolder $
--             newUpdateFolderResponse
--
--         , responseUpdateFolderPermissions $
--             newUpdateFolderPermissionsResponse
--
--         , responseUpdateGroup $
--             newUpdateGroupResponse
--
--         , responseUpdateIAMPolicyAssignment $
--             newUpdateIAMPolicyAssignmentResponse
--
--         , responseUpdateIpRestriction $
--             newUpdateIpRestrictionResponse
--
--         , responseUpdatePublicSharingSettings $
--             newUpdatePublicSharingSettingsResponse
--
--         , responseUpdateRefreshSchedule $
--             newUpdateRefreshScheduleResponse
--
--         , responseUpdateTemplate $
--             newUpdateTemplateResponse
--
--         , responseUpdateTemplateAlias $
--             newUpdateTemplateAliasResponse
--
--         , responseUpdateTemplatePermissions $
--             newUpdateTemplatePermissionsResponse
--
--         , responseUpdateTheme $
--             newUpdateThemeResponse
--
--         , responseUpdateThemeAlias $
--             newUpdateThemeAliasResponse
--
--         , responseUpdateThemePermissions $
--             newUpdateThemePermissionsResponse
--
--         , responseUpdateTopic $
--             newUpdateTopicResponse
--
--         , responseUpdateTopicPermissions $
--             newUpdateTopicPermissionsResponse
--
--         , responseUpdateTopicRefreshSchedule $
--             newUpdateTopicRefreshScheduleResponse
--
--         , responseUpdateUser $
--             newUpdateUserResponse
--
--         , responseUpdateVPCConnection $
--             newUpdateVPCConnectionResponse
--
--           ]
--     ]

-- Requests

requestCancelIngestion :: CancelIngestion -> TestTree
requestCancelIngestion =
  req
    "CancelIngestion"
    "fixture/CancelIngestion.yaml"

requestCreateAccountCustomization :: CreateAccountCustomization -> TestTree
requestCreateAccountCustomization =
  req
    "CreateAccountCustomization"
    "fixture/CreateAccountCustomization.yaml"

requestCreateAccountSubscription :: CreateAccountSubscription -> TestTree
requestCreateAccountSubscription =
  req
    "CreateAccountSubscription"
    "fixture/CreateAccountSubscription.yaml"

requestCreateAnalysis :: CreateAnalysis -> TestTree
requestCreateAnalysis =
  req
    "CreateAnalysis"
    "fixture/CreateAnalysis.yaml"

requestCreateDashboard :: CreateDashboard -> TestTree
requestCreateDashboard =
  req
    "CreateDashboard"
    "fixture/CreateDashboard.yaml"

requestCreateDataSet :: CreateDataSet -> TestTree
requestCreateDataSet =
  req
    "CreateDataSet"
    "fixture/CreateDataSet.yaml"

requestCreateDataSource :: CreateDataSource -> TestTree
requestCreateDataSource =
  req
    "CreateDataSource"
    "fixture/CreateDataSource.yaml"

requestCreateFolder :: CreateFolder -> TestTree
requestCreateFolder =
  req
    "CreateFolder"
    "fixture/CreateFolder.yaml"

requestCreateFolderMembership :: CreateFolderMembership -> TestTree
requestCreateFolderMembership =
  req
    "CreateFolderMembership"
    "fixture/CreateFolderMembership.yaml"

requestCreateGroup :: CreateGroup -> TestTree
requestCreateGroup =
  req
    "CreateGroup"
    "fixture/CreateGroup.yaml"

requestCreateGroupMembership :: CreateGroupMembership -> TestTree
requestCreateGroupMembership =
  req
    "CreateGroupMembership"
    "fixture/CreateGroupMembership.yaml"

requestCreateIAMPolicyAssignment :: CreateIAMPolicyAssignment -> TestTree
requestCreateIAMPolicyAssignment =
  req
    "CreateIAMPolicyAssignment"
    "fixture/CreateIAMPolicyAssignment.yaml"

requestCreateIngestion :: CreateIngestion -> TestTree
requestCreateIngestion =
  req
    "CreateIngestion"
    "fixture/CreateIngestion.yaml"

requestCreateNamespace :: CreateNamespace -> TestTree
requestCreateNamespace =
  req
    "CreateNamespace"
    "fixture/CreateNamespace.yaml"

requestCreateRefreshSchedule :: CreateRefreshSchedule -> TestTree
requestCreateRefreshSchedule =
  req
    "CreateRefreshSchedule"
    "fixture/CreateRefreshSchedule.yaml"

requestCreateTemplate :: CreateTemplate -> TestTree
requestCreateTemplate =
  req
    "CreateTemplate"
    "fixture/CreateTemplate.yaml"

requestCreateTemplateAlias :: CreateTemplateAlias -> TestTree
requestCreateTemplateAlias =
  req
    "CreateTemplateAlias"
    "fixture/CreateTemplateAlias.yaml"

requestCreateTheme :: CreateTheme -> TestTree
requestCreateTheme =
  req
    "CreateTheme"
    "fixture/CreateTheme.yaml"

requestCreateThemeAlias :: CreateThemeAlias -> TestTree
requestCreateThemeAlias =
  req
    "CreateThemeAlias"
    "fixture/CreateThemeAlias.yaml"

requestCreateTopic :: CreateTopic -> TestTree
requestCreateTopic =
  req
    "CreateTopic"
    "fixture/CreateTopic.yaml"

requestCreateTopicRefreshSchedule :: CreateTopicRefreshSchedule -> TestTree
requestCreateTopicRefreshSchedule =
  req
    "CreateTopicRefreshSchedule"
    "fixture/CreateTopicRefreshSchedule.yaml"

requestCreateVPCConnection :: CreateVPCConnection -> TestTree
requestCreateVPCConnection =
  req
    "CreateVPCConnection"
    "fixture/CreateVPCConnection.yaml"

requestDeleteAccountCustomization :: DeleteAccountCustomization -> TestTree
requestDeleteAccountCustomization =
  req
    "DeleteAccountCustomization"
    "fixture/DeleteAccountCustomization.yaml"

requestDeleteAccountSubscription :: DeleteAccountSubscription -> TestTree
requestDeleteAccountSubscription =
  req
    "DeleteAccountSubscription"
    "fixture/DeleteAccountSubscription.yaml"

requestDeleteAnalysis :: DeleteAnalysis -> TestTree
requestDeleteAnalysis =
  req
    "DeleteAnalysis"
    "fixture/DeleteAnalysis.yaml"

requestDeleteDashboard :: DeleteDashboard -> TestTree
requestDeleteDashboard =
  req
    "DeleteDashboard"
    "fixture/DeleteDashboard.yaml"

requestDeleteDataSet :: DeleteDataSet -> TestTree
requestDeleteDataSet =
  req
    "DeleteDataSet"
    "fixture/DeleteDataSet.yaml"

requestDeleteDataSetRefreshProperties :: DeleteDataSetRefreshProperties -> TestTree
requestDeleteDataSetRefreshProperties =
  req
    "DeleteDataSetRefreshProperties"
    "fixture/DeleteDataSetRefreshProperties.yaml"

requestDeleteDataSource :: DeleteDataSource -> TestTree
requestDeleteDataSource =
  req
    "DeleteDataSource"
    "fixture/DeleteDataSource.yaml"

requestDeleteFolder :: DeleteFolder -> TestTree
requestDeleteFolder =
  req
    "DeleteFolder"
    "fixture/DeleteFolder.yaml"

requestDeleteFolderMembership :: DeleteFolderMembership -> TestTree
requestDeleteFolderMembership =
  req
    "DeleteFolderMembership"
    "fixture/DeleteFolderMembership.yaml"

requestDeleteGroup :: DeleteGroup -> TestTree
requestDeleteGroup =
  req
    "DeleteGroup"
    "fixture/DeleteGroup.yaml"

requestDeleteGroupMembership :: DeleteGroupMembership -> TestTree
requestDeleteGroupMembership =
  req
    "DeleteGroupMembership"
    "fixture/DeleteGroupMembership.yaml"

requestDeleteIAMPolicyAssignment :: DeleteIAMPolicyAssignment -> TestTree
requestDeleteIAMPolicyAssignment =
  req
    "DeleteIAMPolicyAssignment"
    "fixture/DeleteIAMPolicyAssignment.yaml"

requestDeleteNamespace :: DeleteNamespace -> TestTree
requestDeleteNamespace =
  req
    "DeleteNamespace"
    "fixture/DeleteNamespace.yaml"

requestDeleteRefreshSchedule :: DeleteRefreshSchedule -> TestTree
requestDeleteRefreshSchedule =
  req
    "DeleteRefreshSchedule"
    "fixture/DeleteRefreshSchedule.yaml"

requestDeleteTemplate :: DeleteTemplate -> TestTree
requestDeleteTemplate =
  req
    "DeleteTemplate"
    "fixture/DeleteTemplate.yaml"

requestDeleteTemplateAlias :: DeleteTemplateAlias -> TestTree
requestDeleteTemplateAlias =
  req
    "DeleteTemplateAlias"
    "fixture/DeleteTemplateAlias.yaml"

requestDeleteTheme :: DeleteTheme -> TestTree
requestDeleteTheme =
  req
    "DeleteTheme"
    "fixture/DeleteTheme.yaml"

requestDeleteThemeAlias :: DeleteThemeAlias -> TestTree
requestDeleteThemeAlias =
  req
    "DeleteThemeAlias"
    "fixture/DeleteThemeAlias.yaml"

requestDeleteTopic :: DeleteTopic -> TestTree
requestDeleteTopic =
  req
    "DeleteTopic"
    "fixture/DeleteTopic.yaml"

requestDeleteTopicRefreshSchedule :: DeleteTopicRefreshSchedule -> TestTree
requestDeleteTopicRefreshSchedule =
  req
    "DeleteTopicRefreshSchedule"
    "fixture/DeleteTopicRefreshSchedule.yaml"

requestDeleteUser :: DeleteUser -> TestTree
requestDeleteUser =
  req
    "DeleteUser"
    "fixture/DeleteUser.yaml"

requestDeleteUserByPrincipalId :: DeleteUserByPrincipalId -> TestTree
requestDeleteUserByPrincipalId =
  req
    "DeleteUserByPrincipalId"
    "fixture/DeleteUserByPrincipalId.yaml"

requestDeleteVPCConnection :: DeleteVPCConnection -> TestTree
requestDeleteVPCConnection =
  req
    "DeleteVPCConnection"
    "fixture/DeleteVPCConnection.yaml"

requestDescribeAccountCustomization :: DescribeAccountCustomization -> TestTree
requestDescribeAccountCustomization =
  req
    "DescribeAccountCustomization"
    "fixture/DescribeAccountCustomization.yaml"

requestDescribeAccountSettings :: DescribeAccountSettings -> TestTree
requestDescribeAccountSettings =
  req
    "DescribeAccountSettings"
    "fixture/DescribeAccountSettings.yaml"

requestDescribeAccountSubscription :: DescribeAccountSubscription -> TestTree
requestDescribeAccountSubscription =
  req
    "DescribeAccountSubscription"
    "fixture/DescribeAccountSubscription.yaml"

requestDescribeAnalysis :: DescribeAnalysis -> TestTree
requestDescribeAnalysis =
  req
    "DescribeAnalysis"
    "fixture/DescribeAnalysis.yaml"

requestDescribeAnalysisDefinition :: DescribeAnalysisDefinition -> TestTree
requestDescribeAnalysisDefinition =
  req
    "DescribeAnalysisDefinition"
    "fixture/DescribeAnalysisDefinition.yaml"

requestDescribeAnalysisPermissions :: DescribeAnalysisPermissions -> TestTree
requestDescribeAnalysisPermissions =
  req
    "DescribeAnalysisPermissions"
    "fixture/DescribeAnalysisPermissions.yaml"

requestDescribeAssetBundleExportJob :: DescribeAssetBundleExportJob -> TestTree
requestDescribeAssetBundleExportJob =
  req
    "DescribeAssetBundleExportJob"
    "fixture/DescribeAssetBundleExportJob.yaml"

requestDescribeAssetBundleImportJob :: DescribeAssetBundleImportJob -> TestTree
requestDescribeAssetBundleImportJob =
  req
    "DescribeAssetBundleImportJob"
    "fixture/DescribeAssetBundleImportJob.yaml"

requestDescribeDashboard :: DescribeDashboard -> TestTree
requestDescribeDashboard =
  req
    "DescribeDashboard"
    "fixture/DescribeDashboard.yaml"

requestDescribeDashboardDefinition :: DescribeDashboardDefinition -> TestTree
requestDescribeDashboardDefinition =
  req
    "DescribeDashboardDefinition"
    "fixture/DescribeDashboardDefinition.yaml"

requestDescribeDashboardPermissions :: DescribeDashboardPermissions -> TestTree
requestDescribeDashboardPermissions =
  req
    "DescribeDashboardPermissions"
    "fixture/DescribeDashboardPermissions.yaml"

requestDescribeDataSet :: DescribeDataSet -> TestTree
requestDescribeDataSet =
  req
    "DescribeDataSet"
    "fixture/DescribeDataSet.yaml"

requestDescribeDataSetPermissions :: DescribeDataSetPermissions -> TestTree
requestDescribeDataSetPermissions =
  req
    "DescribeDataSetPermissions"
    "fixture/DescribeDataSetPermissions.yaml"

requestDescribeDataSetRefreshProperties :: DescribeDataSetRefreshProperties -> TestTree
requestDescribeDataSetRefreshProperties =
  req
    "DescribeDataSetRefreshProperties"
    "fixture/DescribeDataSetRefreshProperties.yaml"

requestDescribeDataSource :: DescribeDataSource -> TestTree
requestDescribeDataSource =
  req
    "DescribeDataSource"
    "fixture/DescribeDataSource.yaml"

requestDescribeDataSourcePermissions :: DescribeDataSourcePermissions -> TestTree
requestDescribeDataSourcePermissions =
  req
    "DescribeDataSourcePermissions"
    "fixture/DescribeDataSourcePermissions.yaml"

requestDescribeFolder :: DescribeFolder -> TestTree
requestDescribeFolder =
  req
    "DescribeFolder"
    "fixture/DescribeFolder.yaml"

requestDescribeFolderPermissions :: DescribeFolderPermissions -> TestTree
requestDescribeFolderPermissions =
  req
    "DescribeFolderPermissions"
    "fixture/DescribeFolderPermissions.yaml"

requestDescribeFolderResolvedPermissions :: DescribeFolderResolvedPermissions -> TestTree
requestDescribeFolderResolvedPermissions =
  req
    "DescribeFolderResolvedPermissions"
    "fixture/DescribeFolderResolvedPermissions.yaml"

requestDescribeGroup :: DescribeGroup -> TestTree
requestDescribeGroup =
  req
    "DescribeGroup"
    "fixture/DescribeGroup.yaml"

requestDescribeGroupMembership :: DescribeGroupMembership -> TestTree
requestDescribeGroupMembership =
  req
    "DescribeGroupMembership"
    "fixture/DescribeGroupMembership.yaml"

requestDescribeIAMPolicyAssignment :: DescribeIAMPolicyAssignment -> TestTree
requestDescribeIAMPolicyAssignment =
  req
    "DescribeIAMPolicyAssignment"
    "fixture/DescribeIAMPolicyAssignment.yaml"

requestDescribeIngestion :: DescribeIngestion -> TestTree
requestDescribeIngestion =
  req
    "DescribeIngestion"
    "fixture/DescribeIngestion.yaml"

requestDescribeIpRestriction :: DescribeIpRestriction -> TestTree
requestDescribeIpRestriction =
  req
    "DescribeIpRestriction"
    "fixture/DescribeIpRestriction.yaml"

requestDescribeNamespace :: DescribeNamespace -> TestTree
requestDescribeNamespace =
  req
    "DescribeNamespace"
    "fixture/DescribeNamespace.yaml"

requestDescribeRefreshSchedule :: DescribeRefreshSchedule -> TestTree
requestDescribeRefreshSchedule =
  req
    "DescribeRefreshSchedule"
    "fixture/DescribeRefreshSchedule.yaml"

requestDescribeTemplate :: DescribeTemplate -> TestTree
requestDescribeTemplate =
  req
    "DescribeTemplate"
    "fixture/DescribeTemplate.yaml"

requestDescribeTemplateAlias :: DescribeTemplateAlias -> TestTree
requestDescribeTemplateAlias =
  req
    "DescribeTemplateAlias"
    "fixture/DescribeTemplateAlias.yaml"

requestDescribeTemplateDefinition :: DescribeTemplateDefinition -> TestTree
requestDescribeTemplateDefinition =
  req
    "DescribeTemplateDefinition"
    "fixture/DescribeTemplateDefinition.yaml"

requestDescribeTemplatePermissions :: DescribeTemplatePermissions -> TestTree
requestDescribeTemplatePermissions =
  req
    "DescribeTemplatePermissions"
    "fixture/DescribeTemplatePermissions.yaml"

requestDescribeTheme :: DescribeTheme -> TestTree
requestDescribeTheme =
  req
    "DescribeTheme"
    "fixture/DescribeTheme.yaml"

requestDescribeThemeAlias :: DescribeThemeAlias -> TestTree
requestDescribeThemeAlias =
  req
    "DescribeThemeAlias"
    "fixture/DescribeThemeAlias.yaml"

requestDescribeThemePermissions :: DescribeThemePermissions -> TestTree
requestDescribeThemePermissions =
  req
    "DescribeThemePermissions"
    "fixture/DescribeThemePermissions.yaml"

requestDescribeTopic :: DescribeTopic -> TestTree
requestDescribeTopic =
  req
    "DescribeTopic"
    "fixture/DescribeTopic.yaml"

requestDescribeTopicPermissions :: DescribeTopicPermissions -> TestTree
requestDescribeTopicPermissions =
  req
    "DescribeTopicPermissions"
    "fixture/DescribeTopicPermissions.yaml"

requestDescribeTopicRefresh :: DescribeTopicRefresh -> TestTree
requestDescribeTopicRefresh =
  req
    "DescribeTopicRefresh"
    "fixture/DescribeTopicRefresh.yaml"

requestDescribeTopicRefreshSchedule :: DescribeTopicRefreshSchedule -> TestTree
requestDescribeTopicRefreshSchedule =
  req
    "DescribeTopicRefreshSchedule"
    "fixture/DescribeTopicRefreshSchedule.yaml"

requestDescribeUser :: DescribeUser -> TestTree
requestDescribeUser =
  req
    "DescribeUser"
    "fixture/DescribeUser.yaml"

requestDescribeVPCConnection :: DescribeVPCConnection -> TestTree
requestDescribeVPCConnection =
  req
    "DescribeVPCConnection"
    "fixture/DescribeVPCConnection.yaml"

requestGenerateEmbedUrlForAnonymousUser :: GenerateEmbedUrlForAnonymousUser -> TestTree
requestGenerateEmbedUrlForAnonymousUser =
  req
    "GenerateEmbedUrlForAnonymousUser"
    "fixture/GenerateEmbedUrlForAnonymousUser.yaml"

requestGenerateEmbedUrlForRegisteredUser :: GenerateEmbedUrlForRegisteredUser -> TestTree
requestGenerateEmbedUrlForRegisteredUser =
  req
    "GenerateEmbedUrlForRegisteredUser"
    "fixture/GenerateEmbedUrlForRegisteredUser.yaml"

requestGetDashboardEmbedUrl :: GetDashboardEmbedUrl -> TestTree
requestGetDashboardEmbedUrl =
  req
    "GetDashboardEmbedUrl"
    "fixture/GetDashboardEmbedUrl.yaml"

requestGetSessionEmbedUrl :: GetSessionEmbedUrl -> TestTree
requestGetSessionEmbedUrl =
  req
    "GetSessionEmbedUrl"
    "fixture/GetSessionEmbedUrl.yaml"

requestListAnalyses :: ListAnalyses -> TestTree
requestListAnalyses =
  req
    "ListAnalyses"
    "fixture/ListAnalyses.yaml"

requestListAssetBundleExportJobs :: ListAssetBundleExportJobs -> TestTree
requestListAssetBundleExportJobs =
  req
    "ListAssetBundleExportJobs"
    "fixture/ListAssetBundleExportJobs.yaml"

requestListAssetBundleImportJobs :: ListAssetBundleImportJobs -> TestTree
requestListAssetBundleImportJobs =
  req
    "ListAssetBundleImportJobs"
    "fixture/ListAssetBundleImportJobs.yaml"

requestListDashboardVersions :: ListDashboardVersions -> TestTree
requestListDashboardVersions =
  req
    "ListDashboardVersions"
    "fixture/ListDashboardVersions.yaml"

requestListDashboards :: ListDashboards -> TestTree
requestListDashboards =
  req
    "ListDashboards"
    "fixture/ListDashboards.yaml"

requestListDataSets :: ListDataSets -> TestTree
requestListDataSets =
  req
    "ListDataSets"
    "fixture/ListDataSets.yaml"

requestListDataSources :: ListDataSources -> TestTree
requestListDataSources =
  req
    "ListDataSources"
    "fixture/ListDataSources.yaml"

requestListFolderMembers :: ListFolderMembers -> TestTree
requestListFolderMembers =
  req
    "ListFolderMembers"
    "fixture/ListFolderMembers.yaml"

requestListFolders :: ListFolders -> TestTree
requestListFolders =
  req
    "ListFolders"
    "fixture/ListFolders.yaml"

requestListGroupMemberships :: ListGroupMemberships -> TestTree
requestListGroupMemberships =
  req
    "ListGroupMemberships"
    "fixture/ListGroupMemberships.yaml"

requestListGroups :: ListGroups -> TestTree
requestListGroups =
  req
    "ListGroups"
    "fixture/ListGroups.yaml"

requestListIAMPolicyAssignments :: ListIAMPolicyAssignments -> TestTree
requestListIAMPolicyAssignments =
  req
    "ListIAMPolicyAssignments"
    "fixture/ListIAMPolicyAssignments.yaml"

requestListIAMPolicyAssignmentsForUser :: ListIAMPolicyAssignmentsForUser -> TestTree
requestListIAMPolicyAssignmentsForUser =
  req
    "ListIAMPolicyAssignmentsForUser"
    "fixture/ListIAMPolicyAssignmentsForUser.yaml"

requestListIngestions :: ListIngestions -> TestTree
requestListIngestions =
  req
    "ListIngestions"
    "fixture/ListIngestions.yaml"

requestListNamespaces :: ListNamespaces -> TestTree
requestListNamespaces =
  req
    "ListNamespaces"
    "fixture/ListNamespaces.yaml"

requestListRefreshSchedules :: ListRefreshSchedules -> TestTree
requestListRefreshSchedules =
  req
    "ListRefreshSchedules"
    "fixture/ListRefreshSchedules.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListTemplateAliases :: ListTemplateAliases -> TestTree
requestListTemplateAliases =
  req
    "ListTemplateAliases"
    "fixture/ListTemplateAliases.yaml"

requestListTemplateVersions :: ListTemplateVersions -> TestTree
requestListTemplateVersions =
  req
    "ListTemplateVersions"
    "fixture/ListTemplateVersions.yaml"

requestListTemplates :: ListTemplates -> TestTree
requestListTemplates =
  req
    "ListTemplates"
    "fixture/ListTemplates.yaml"

requestListThemeAliases :: ListThemeAliases -> TestTree
requestListThemeAliases =
  req
    "ListThemeAliases"
    "fixture/ListThemeAliases.yaml"

requestListThemeVersions :: ListThemeVersions -> TestTree
requestListThemeVersions =
  req
    "ListThemeVersions"
    "fixture/ListThemeVersions.yaml"

requestListThemes :: ListThemes -> TestTree
requestListThemes =
  req
    "ListThemes"
    "fixture/ListThemes.yaml"

requestListTopicRefreshSchedules :: ListTopicRefreshSchedules -> TestTree
requestListTopicRefreshSchedules =
  req
    "ListTopicRefreshSchedules"
    "fixture/ListTopicRefreshSchedules.yaml"

requestListTopics :: ListTopics -> TestTree
requestListTopics =
  req
    "ListTopics"
    "fixture/ListTopics.yaml"

requestListUserGroups :: ListUserGroups -> TestTree
requestListUserGroups =
  req
    "ListUserGroups"
    "fixture/ListUserGroups.yaml"

requestListUsers :: ListUsers -> TestTree
requestListUsers =
  req
    "ListUsers"
    "fixture/ListUsers.yaml"

requestListVPCConnections :: ListVPCConnections -> TestTree
requestListVPCConnections =
  req
    "ListVPCConnections"
    "fixture/ListVPCConnections.yaml"

requestPutDataSetRefreshProperties :: PutDataSetRefreshProperties -> TestTree
requestPutDataSetRefreshProperties =
  req
    "PutDataSetRefreshProperties"
    "fixture/PutDataSetRefreshProperties.yaml"

requestRegisterUser :: RegisterUser -> TestTree
requestRegisterUser =
  req
    "RegisterUser"
    "fixture/RegisterUser.yaml"

requestRestoreAnalysis :: RestoreAnalysis -> TestTree
requestRestoreAnalysis =
  req
    "RestoreAnalysis"
    "fixture/RestoreAnalysis.yaml"

requestSearchAnalyses :: SearchAnalyses -> TestTree
requestSearchAnalyses =
  req
    "SearchAnalyses"
    "fixture/SearchAnalyses.yaml"

requestSearchDashboards :: SearchDashboards -> TestTree
requestSearchDashboards =
  req
    "SearchDashboards"
    "fixture/SearchDashboards.yaml"

requestSearchDataSets :: SearchDataSets -> TestTree
requestSearchDataSets =
  req
    "SearchDataSets"
    "fixture/SearchDataSets.yaml"

requestSearchDataSources :: SearchDataSources -> TestTree
requestSearchDataSources =
  req
    "SearchDataSources"
    "fixture/SearchDataSources.yaml"

requestSearchFolders :: SearchFolders -> TestTree
requestSearchFolders =
  req
    "SearchFolders"
    "fixture/SearchFolders.yaml"

requestSearchGroups :: SearchGroups -> TestTree
requestSearchGroups =
  req
    "SearchGroups"
    "fixture/SearchGroups.yaml"

requestStartAssetBundleExportJob :: StartAssetBundleExportJob -> TestTree
requestStartAssetBundleExportJob =
  req
    "StartAssetBundleExportJob"
    "fixture/StartAssetBundleExportJob.yaml"

requestStartAssetBundleImportJob :: StartAssetBundleImportJob -> TestTree
requestStartAssetBundleImportJob =
  req
    "StartAssetBundleImportJob"
    "fixture/StartAssetBundleImportJob.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateAccountCustomization :: UpdateAccountCustomization -> TestTree
requestUpdateAccountCustomization =
  req
    "UpdateAccountCustomization"
    "fixture/UpdateAccountCustomization.yaml"

requestUpdateAccountSettings :: UpdateAccountSettings -> TestTree
requestUpdateAccountSettings =
  req
    "UpdateAccountSettings"
    "fixture/UpdateAccountSettings.yaml"

requestUpdateAnalysis :: UpdateAnalysis -> TestTree
requestUpdateAnalysis =
  req
    "UpdateAnalysis"
    "fixture/UpdateAnalysis.yaml"

requestUpdateAnalysisPermissions :: UpdateAnalysisPermissions -> TestTree
requestUpdateAnalysisPermissions =
  req
    "UpdateAnalysisPermissions"
    "fixture/UpdateAnalysisPermissions.yaml"

requestUpdateDashboard :: UpdateDashboard -> TestTree
requestUpdateDashboard =
  req
    "UpdateDashboard"
    "fixture/UpdateDashboard.yaml"

requestUpdateDashboardPermissions :: UpdateDashboardPermissions -> TestTree
requestUpdateDashboardPermissions =
  req
    "UpdateDashboardPermissions"
    "fixture/UpdateDashboardPermissions.yaml"

requestUpdateDashboardPublishedVersion :: UpdateDashboardPublishedVersion -> TestTree
requestUpdateDashboardPublishedVersion =
  req
    "UpdateDashboardPublishedVersion"
    "fixture/UpdateDashboardPublishedVersion.yaml"

requestUpdateDataSet :: UpdateDataSet -> TestTree
requestUpdateDataSet =
  req
    "UpdateDataSet"
    "fixture/UpdateDataSet.yaml"

requestUpdateDataSetPermissions :: UpdateDataSetPermissions -> TestTree
requestUpdateDataSetPermissions =
  req
    "UpdateDataSetPermissions"
    "fixture/UpdateDataSetPermissions.yaml"

requestUpdateDataSource :: UpdateDataSource -> TestTree
requestUpdateDataSource =
  req
    "UpdateDataSource"
    "fixture/UpdateDataSource.yaml"

requestUpdateDataSourcePermissions :: UpdateDataSourcePermissions -> TestTree
requestUpdateDataSourcePermissions =
  req
    "UpdateDataSourcePermissions"
    "fixture/UpdateDataSourcePermissions.yaml"

requestUpdateFolder :: UpdateFolder -> TestTree
requestUpdateFolder =
  req
    "UpdateFolder"
    "fixture/UpdateFolder.yaml"

requestUpdateFolderPermissions :: UpdateFolderPermissions -> TestTree
requestUpdateFolderPermissions =
  req
    "UpdateFolderPermissions"
    "fixture/UpdateFolderPermissions.yaml"

requestUpdateGroup :: UpdateGroup -> TestTree
requestUpdateGroup =
  req
    "UpdateGroup"
    "fixture/UpdateGroup.yaml"

requestUpdateIAMPolicyAssignment :: UpdateIAMPolicyAssignment -> TestTree
requestUpdateIAMPolicyAssignment =
  req
    "UpdateIAMPolicyAssignment"
    "fixture/UpdateIAMPolicyAssignment.yaml"

requestUpdateIpRestriction :: UpdateIpRestriction -> TestTree
requestUpdateIpRestriction =
  req
    "UpdateIpRestriction"
    "fixture/UpdateIpRestriction.yaml"

requestUpdatePublicSharingSettings :: UpdatePublicSharingSettings -> TestTree
requestUpdatePublicSharingSettings =
  req
    "UpdatePublicSharingSettings"
    "fixture/UpdatePublicSharingSettings.yaml"

requestUpdateRefreshSchedule :: UpdateRefreshSchedule -> TestTree
requestUpdateRefreshSchedule =
  req
    "UpdateRefreshSchedule"
    "fixture/UpdateRefreshSchedule.yaml"

requestUpdateTemplate :: UpdateTemplate -> TestTree
requestUpdateTemplate =
  req
    "UpdateTemplate"
    "fixture/UpdateTemplate.yaml"

requestUpdateTemplateAlias :: UpdateTemplateAlias -> TestTree
requestUpdateTemplateAlias =
  req
    "UpdateTemplateAlias"
    "fixture/UpdateTemplateAlias.yaml"

requestUpdateTemplatePermissions :: UpdateTemplatePermissions -> TestTree
requestUpdateTemplatePermissions =
  req
    "UpdateTemplatePermissions"
    "fixture/UpdateTemplatePermissions.yaml"

requestUpdateTheme :: UpdateTheme -> TestTree
requestUpdateTheme =
  req
    "UpdateTheme"
    "fixture/UpdateTheme.yaml"

requestUpdateThemeAlias :: UpdateThemeAlias -> TestTree
requestUpdateThemeAlias =
  req
    "UpdateThemeAlias"
    "fixture/UpdateThemeAlias.yaml"

requestUpdateThemePermissions :: UpdateThemePermissions -> TestTree
requestUpdateThemePermissions =
  req
    "UpdateThemePermissions"
    "fixture/UpdateThemePermissions.yaml"

requestUpdateTopic :: UpdateTopic -> TestTree
requestUpdateTopic =
  req
    "UpdateTopic"
    "fixture/UpdateTopic.yaml"

requestUpdateTopicPermissions :: UpdateTopicPermissions -> TestTree
requestUpdateTopicPermissions =
  req
    "UpdateTopicPermissions"
    "fixture/UpdateTopicPermissions.yaml"

requestUpdateTopicRefreshSchedule :: UpdateTopicRefreshSchedule -> TestTree
requestUpdateTopicRefreshSchedule =
  req
    "UpdateTopicRefreshSchedule"
    "fixture/UpdateTopicRefreshSchedule.yaml"

requestUpdateUser :: UpdateUser -> TestTree
requestUpdateUser =
  req
    "UpdateUser"
    "fixture/UpdateUser.yaml"

requestUpdateVPCConnection :: UpdateVPCConnection -> TestTree
requestUpdateVPCConnection =
  req
    "UpdateVPCConnection"
    "fixture/UpdateVPCConnection.yaml"

-- Responses

responseCancelIngestion :: CancelIngestionResponse -> TestTree
responseCancelIngestion =
  res
    "CancelIngestionResponse"
    "fixture/CancelIngestionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelIngestion)

responseCreateAccountCustomization :: CreateAccountCustomizationResponse -> TestTree
responseCreateAccountCustomization =
  res
    "CreateAccountCustomizationResponse"
    "fixture/CreateAccountCustomizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAccountCustomization)

responseCreateAccountSubscription :: CreateAccountSubscriptionResponse -> TestTree
responseCreateAccountSubscription =
  res
    "CreateAccountSubscriptionResponse"
    "fixture/CreateAccountSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAccountSubscription)

responseCreateAnalysis :: CreateAnalysisResponse -> TestTree
responseCreateAnalysis =
  res
    "CreateAnalysisResponse"
    "fixture/CreateAnalysisResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAnalysis)

responseCreateDashboard :: CreateDashboardResponse -> TestTree
responseCreateDashboard =
  res
    "CreateDashboardResponse"
    "fixture/CreateDashboardResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDashboard)

responseCreateDataSet :: CreateDataSetResponse -> TestTree
responseCreateDataSet =
  res
    "CreateDataSetResponse"
    "fixture/CreateDataSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDataSet)

responseCreateDataSource :: CreateDataSourceResponse -> TestTree
responseCreateDataSource =
  res
    "CreateDataSourceResponse"
    "fixture/CreateDataSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDataSource)

responseCreateFolder :: CreateFolderResponse -> TestTree
responseCreateFolder =
  res
    "CreateFolderResponse"
    "fixture/CreateFolderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFolder)

responseCreateFolderMembership :: CreateFolderMembershipResponse -> TestTree
responseCreateFolderMembership =
  res
    "CreateFolderMembershipResponse"
    "fixture/CreateFolderMembershipResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFolderMembership)

responseCreateGroup :: CreateGroupResponse -> TestTree
responseCreateGroup =
  res
    "CreateGroupResponse"
    "fixture/CreateGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateGroup)

responseCreateGroupMembership :: CreateGroupMembershipResponse -> TestTree
responseCreateGroupMembership =
  res
    "CreateGroupMembershipResponse"
    "fixture/CreateGroupMembershipResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateGroupMembership)

responseCreateIAMPolicyAssignment :: CreateIAMPolicyAssignmentResponse -> TestTree
responseCreateIAMPolicyAssignment =
  res
    "CreateIAMPolicyAssignmentResponse"
    "fixture/CreateIAMPolicyAssignmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateIAMPolicyAssignment)

responseCreateIngestion :: CreateIngestionResponse -> TestTree
responseCreateIngestion =
  res
    "CreateIngestionResponse"
    "fixture/CreateIngestionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateIngestion)

responseCreateNamespace :: CreateNamespaceResponse -> TestTree
responseCreateNamespace =
  res
    "CreateNamespaceResponse"
    "fixture/CreateNamespaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateNamespace)

responseCreateRefreshSchedule :: CreateRefreshScheduleResponse -> TestTree
responseCreateRefreshSchedule =
  res
    "CreateRefreshScheduleResponse"
    "fixture/CreateRefreshScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRefreshSchedule)

responseCreateTemplate :: CreateTemplateResponse -> TestTree
responseCreateTemplate =
  res
    "CreateTemplateResponse"
    "fixture/CreateTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTemplate)

responseCreateTemplateAlias :: CreateTemplateAliasResponse -> TestTree
responseCreateTemplateAlias =
  res
    "CreateTemplateAliasResponse"
    "fixture/CreateTemplateAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTemplateAlias)

responseCreateTheme :: CreateThemeResponse -> TestTree
responseCreateTheme =
  res
    "CreateThemeResponse"
    "fixture/CreateThemeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTheme)

responseCreateThemeAlias :: CreateThemeAliasResponse -> TestTree
responseCreateThemeAlias =
  res
    "CreateThemeAliasResponse"
    "fixture/CreateThemeAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateThemeAlias)

responseCreateTopic :: CreateTopicResponse -> TestTree
responseCreateTopic =
  res
    "CreateTopicResponse"
    "fixture/CreateTopicResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTopic)

responseCreateTopicRefreshSchedule :: CreateTopicRefreshScheduleResponse -> TestTree
responseCreateTopicRefreshSchedule =
  res
    "CreateTopicRefreshScheduleResponse"
    "fixture/CreateTopicRefreshScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTopicRefreshSchedule)

responseCreateVPCConnection :: CreateVPCConnectionResponse -> TestTree
responseCreateVPCConnection =
  res
    "CreateVPCConnectionResponse"
    "fixture/CreateVPCConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVPCConnection)

responseDeleteAccountCustomization :: DeleteAccountCustomizationResponse -> TestTree
responseDeleteAccountCustomization =
  res
    "DeleteAccountCustomizationResponse"
    "fixture/DeleteAccountCustomizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAccountCustomization)

responseDeleteAccountSubscription :: DeleteAccountSubscriptionResponse -> TestTree
responseDeleteAccountSubscription =
  res
    "DeleteAccountSubscriptionResponse"
    "fixture/DeleteAccountSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAccountSubscription)

responseDeleteAnalysis :: DeleteAnalysisResponse -> TestTree
responseDeleteAnalysis =
  res
    "DeleteAnalysisResponse"
    "fixture/DeleteAnalysisResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAnalysis)

responseDeleteDashboard :: DeleteDashboardResponse -> TestTree
responseDeleteDashboard =
  res
    "DeleteDashboardResponse"
    "fixture/DeleteDashboardResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDashboard)

responseDeleteDataSet :: DeleteDataSetResponse -> TestTree
responseDeleteDataSet =
  res
    "DeleteDataSetResponse"
    "fixture/DeleteDataSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDataSet)

responseDeleteDataSetRefreshProperties :: DeleteDataSetRefreshPropertiesResponse -> TestTree
responseDeleteDataSetRefreshProperties =
  res
    "DeleteDataSetRefreshPropertiesResponse"
    "fixture/DeleteDataSetRefreshPropertiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDataSetRefreshProperties)

responseDeleteDataSource :: DeleteDataSourceResponse -> TestTree
responseDeleteDataSource =
  res
    "DeleteDataSourceResponse"
    "fixture/DeleteDataSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDataSource)

responseDeleteFolder :: DeleteFolderResponse -> TestTree
responseDeleteFolder =
  res
    "DeleteFolderResponse"
    "fixture/DeleteFolderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFolder)

responseDeleteFolderMembership :: DeleteFolderMembershipResponse -> TestTree
responseDeleteFolderMembership =
  res
    "DeleteFolderMembershipResponse"
    "fixture/DeleteFolderMembershipResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFolderMembership)

responseDeleteGroup :: DeleteGroupResponse -> TestTree
responseDeleteGroup =
  res
    "DeleteGroupResponse"
    "fixture/DeleteGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteGroup)

responseDeleteGroupMembership :: DeleteGroupMembershipResponse -> TestTree
responseDeleteGroupMembership =
  res
    "DeleteGroupMembershipResponse"
    "fixture/DeleteGroupMembershipResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteGroupMembership)

responseDeleteIAMPolicyAssignment :: DeleteIAMPolicyAssignmentResponse -> TestTree
responseDeleteIAMPolicyAssignment =
  res
    "DeleteIAMPolicyAssignmentResponse"
    "fixture/DeleteIAMPolicyAssignmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteIAMPolicyAssignment)

responseDeleteNamespace :: DeleteNamespaceResponse -> TestTree
responseDeleteNamespace =
  res
    "DeleteNamespaceResponse"
    "fixture/DeleteNamespaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteNamespace)

responseDeleteRefreshSchedule :: DeleteRefreshScheduleResponse -> TestTree
responseDeleteRefreshSchedule =
  res
    "DeleteRefreshScheduleResponse"
    "fixture/DeleteRefreshScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRefreshSchedule)

responseDeleteTemplate :: DeleteTemplateResponse -> TestTree
responseDeleteTemplate =
  res
    "DeleteTemplateResponse"
    "fixture/DeleteTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTemplate)

responseDeleteTemplateAlias :: DeleteTemplateAliasResponse -> TestTree
responseDeleteTemplateAlias =
  res
    "DeleteTemplateAliasResponse"
    "fixture/DeleteTemplateAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTemplateAlias)

responseDeleteTheme :: DeleteThemeResponse -> TestTree
responseDeleteTheme =
  res
    "DeleteThemeResponse"
    "fixture/DeleteThemeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTheme)

responseDeleteThemeAlias :: DeleteThemeAliasResponse -> TestTree
responseDeleteThemeAlias =
  res
    "DeleteThemeAliasResponse"
    "fixture/DeleteThemeAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteThemeAlias)

responseDeleteTopic :: DeleteTopicResponse -> TestTree
responseDeleteTopic =
  res
    "DeleteTopicResponse"
    "fixture/DeleteTopicResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTopic)

responseDeleteTopicRefreshSchedule :: DeleteTopicRefreshScheduleResponse -> TestTree
responseDeleteTopicRefreshSchedule =
  res
    "DeleteTopicRefreshScheduleResponse"
    "fixture/DeleteTopicRefreshScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTopicRefreshSchedule)

responseDeleteUser :: DeleteUserResponse -> TestTree
responseDeleteUser =
  res
    "DeleteUserResponse"
    "fixture/DeleteUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUser)

responseDeleteUserByPrincipalId :: DeleteUserByPrincipalIdResponse -> TestTree
responseDeleteUserByPrincipalId =
  res
    "DeleteUserByPrincipalIdResponse"
    "fixture/DeleteUserByPrincipalIdResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUserByPrincipalId)

responseDeleteVPCConnection :: DeleteVPCConnectionResponse -> TestTree
responseDeleteVPCConnection =
  res
    "DeleteVPCConnectionResponse"
    "fixture/DeleteVPCConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVPCConnection)

responseDescribeAccountCustomization :: DescribeAccountCustomizationResponse -> TestTree
responseDescribeAccountCustomization =
  res
    "DescribeAccountCustomizationResponse"
    "fixture/DescribeAccountCustomizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAccountCustomization)

responseDescribeAccountSettings :: DescribeAccountSettingsResponse -> TestTree
responseDescribeAccountSettings =
  res
    "DescribeAccountSettingsResponse"
    "fixture/DescribeAccountSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAccountSettings)

responseDescribeAccountSubscription :: DescribeAccountSubscriptionResponse -> TestTree
responseDescribeAccountSubscription =
  res
    "DescribeAccountSubscriptionResponse"
    "fixture/DescribeAccountSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAccountSubscription)

responseDescribeAnalysis :: DescribeAnalysisResponse -> TestTree
responseDescribeAnalysis =
  res
    "DescribeAnalysisResponse"
    "fixture/DescribeAnalysisResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAnalysis)

responseDescribeAnalysisDefinition :: DescribeAnalysisDefinitionResponse -> TestTree
responseDescribeAnalysisDefinition =
  res
    "DescribeAnalysisDefinitionResponse"
    "fixture/DescribeAnalysisDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAnalysisDefinition)

responseDescribeAnalysisPermissions :: DescribeAnalysisPermissionsResponse -> TestTree
responseDescribeAnalysisPermissions =
  res
    "DescribeAnalysisPermissionsResponse"
    "fixture/DescribeAnalysisPermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAnalysisPermissions)

responseDescribeAssetBundleExportJob :: DescribeAssetBundleExportJobResponse -> TestTree
responseDescribeAssetBundleExportJob =
  res
    "DescribeAssetBundleExportJobResponse"
    "fixture/DescribeAssetBundleExportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAssetBundleExportJob)

responseDescribeAssetBundleImportJob :: DescribeAssetBundleImportJobResponse -> TestTree
responseDescribeAssetBundleImportJob =
  res
    "DescribeAssetBundleImportJobResponse"
    "fixture/DescribeAssetBundleImportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAssetBundleImportJob)

responseDescribeDashboard :: DescribeDashboardResponse -> TestTree
responseDescribeDashboard =
  res
    "DescribeDashboardResponse"
    "fixture/DescribeDashboardResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDashboard)

responseDescribeDashboardDefinition :: DescribeDashboardDefinitionResponse -> TestTree
responseDescribeDashboardDefinition =
  res
    "DescribeDashboardDefinitionResponse"
    "fixture/DescribeDashboardDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDashboardDefinition)

responseDescribeDashboardPermissions :: DescribeDashboardPermissionsResponse -> TestTree
responseDescribeDashboardPermissions =
  res
    "DescribeDashboardPermissionsResponse"
    "fixture/DescribeDashboardPermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDashboardPermissions)

responseDescribeDataSet :: DescribeDataSetResponse -> TestTree
responseDescribeDataSet =
  res
    "DescribeDataSetResponse"
    "fixture/DescribeDataSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDataSet)

responseDescribeDataSetPermissions :: DescribeDataSetPermissionsResponse -> TestTree
responseDescribeDataSetPermissions =
  res
    "DescribeDataSetPermissionsResponse"
    "fixture/DescribeDataSetPermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDataSetPermissions)

responseDescribeDataSetRefreshProperties :: DescribeDataSetRefreshPropertiesResponse -> TestTree
responseDescribeDataSetRefreshProperties =
  res
    "DescribeDataSetRefreshPropertiesResponse"
    "fixture/DescribeDataSetRefreshPropertiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDataSetRefreshProperties)

responseDescribeDataSource :: DescribeDataSourceResponse -> TestTree
responseDescribeDataSource =
  res
    "DescribeDataSourceResponse"
    "fixture/DescribeDataSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDataSource)

responseDescribeDataSourcePermissions :: DescribeDataSourcePermissionsResponse -> TestTree
responseDescribeDataSourcePermissions =
  res
    "DescribeDataSourcePermissionsResponse"
    "fixture/DescribeDataSourcePermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDataSourcePermissions)

responseDescribeFolder :: DescribeFolderResponse -> TestTree
responseDescribeFolder =
  res
    "DescribeFolderResponse"
    "fixture/DescribeFolderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFolder)

responseDescribeFolderPermissions :: DescribeFolderPermissionsResponse -> TestTree
responseDescribeFolderPermissions =
  res
    "DescribeFolderPermissionsResponse"
    "fixture/DescribeFolderPermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFolderPermissions)

responseDescribeFolderResolvedPermissions :: DescribeFolderResolvedPermissionsResponse -> TestTree
responseDescribeFolderResolvedPermissions =
  res
    "DescribeFolderResolvedPermissionsResponse"
    "fixture/DescribeFolderResolvedPermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFolderResolvedPermissions)

responseDescribeGroup :: DescribeGroupResponse -> TestTree
responseDescribeGroup =
  res
    "DescribeGroupResponse"
    "fixture/DescribeGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeGroup)

responseDescribeGroupMembership :: DescribeGroupMembershipResponse -> TestTree
responseDescribeGroupMembership =
  res
    "DescribeGroupMembershipResponse"
    "fixture/DescribeGroupMembershipResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeGroupMembership)

responseDescribeIAMPolicyAssignment :: DescribeIAMPolicyAssignmentResponse -> TestTree
responseDescribeIAMPolicyAssignment =
  res
    "DescribeIAMPolicyAssignmentResponse"
    "fixture/DescribeIAMPolicyAssignmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeIAMPolicyAssignment)

responseDescribeIngestion :: DescribeIngestionResponse -> TestTree
responseDescribeIngestion =
  res
    "DescribeIngestionResponse"
    "fixture/DescribeIngestionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeIngestion)

responseDescribeIpRestriction :: DescribeIpRestrictionResponse -> TestTree
responseDescribeIpRestriction =
  res
    "DescribeIpRestrictionResponse"
    "fixture/DescribeIpRestrictionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeIpRestriction)

responseDescribeNamespace :: DescribeNamespaceResponse -> TestTree
responseDescribeNamespace =
  res
    "DescribeNamespaceResponse"
    "fixture/DescribeNamespaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeNamespace)

responseDescribeRefreshSchedule :: DescribeRefreshScheduleResponse -> TestTree
responseDescribeRefreshSchedule =
  res
    "DescribeRefreshScheduleResponse"
    "fixture/DescribeRefreshScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRefreshSchedule)

responseDescribeTemplate :: DescribeTemplateResponse -> TestTree
responseDescribeTemplate =
  res
    "DescribeTemplateResponse"
    "fixture/DescribeTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTemplate)

responseDescribeTemplateAlias :: DescribeTemplateAliasResponse -> TestTree
responseDescribeTemplateAlias =
  res
    "DescribeTemplateAliasResponse"
    "fixture/DescribeTemplateAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTemplateAlias)

responseDescribeTemplateDefinition :: DescribeTemplateDefinitionResponse -> TestTree
responseDescribeTemplateDefinition =
  res
    "DescribeTemplateDefinitionResponse"
    "fixture/DescribeTemplateDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTemplateDefinition)

responseDescribeTemplatePermissions :: DescribeTemplatePermissionsResponse -> TestTree
responseDescribeTemplatePermissions =
  res
    "DescribeTemplatePermissionsResponse"
    "fixture/DescribeTemplatePermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTemplatePermissions)

responseDescribeTheme :: DescribeThemeResponse -> TestTree
responseDescribeTheme =
  res
    "DescribeThemeResponse"
    "fixture/DescribeThemeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTheme)

responseDescribeThemeAlias :: DescribeThemeAliasResponse -> TestTree
responseDescribeThemeAlias =
  res
    "DescribeThemeAliasResponse"
    "fixture/DescribeThemeAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeThemeAlias)

responseDescribeThemePermissions :: DescribeThemePermissionsResponse -> TestTree
responseDescribeThemePermissions =
  res
    "DescribeThemePermissionsResponse"
    "fixture/DescribeThemePermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeThemePermissions)

responseDescribeTopic :: DescribeTopicResponse -> TestTree
responseDescribeTopic =
  res
    "DescribeTopicResponse"
    "fixture/DescribeTopicResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTopic)

responseDescribeTopicPermissions :: DescribeTopicPermissionsResponse -> TestTree
responseDescribeTopicPermissions =
  res
    "DescribeTopicPermissionsResponse"
    "fixture/DescribeTopicPermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTopicPermissions)

responseDescribeTopicRefresh :: DescribeTopicRefreshResponse -> TestTree
responseDescribeTopicRefresh =
  res
    "DescribeTopicRefreshResponse"
    "fixture/DescribeTopicRefreshResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTopicRefresh)

responseDescribeTopicRefreshSchedule :: DescribeTopicRefreshScheduleResponse -> TestTree
responseDescribeTopicRefreshSchedule =
  res
    "DescribeTopicRefreshScheduleResponse"
    "fixture/DescribeTopicRefreshScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTopicRefreshSchedule)

responseDescribeUser :: DescribeUserResponse -> TestTree
responseDescribeUser =
  res
    "DescribeUserResponse"
    "fixture/DescribeUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeUser)

responseDescribeVPCConnection :: DescribeVPCConnectionResponse -> TestTree
responseDescribeVPCConnection =
  res
    "DescribeVPCConnectionResponse"
    "fixture/DescribeVPCConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVPCConnection)

responseGenerateEmbedUrlForAnonymousUser :: GenerateEmbedUrlForAnonymousUserResponse -> TestTree
responseGenerateEmbedUrlForAnonymousUser =
  res
    "GenerateEmbedUrlForAnonymousUserResponse"
    "fixture/GenerateEmbedUrlForAnonymousUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GenerateEmbedUrlForAnonymousUser)

responseGenerateEmbedUrlForRegisteredUser :: GenerateEmbedUrlForRegisteredUserResponse -> TestTree
responseGenerateEmbedUrlForRegisteredUser =
  res
    "GenerateEmbedUrlForRegisteredUserResponse"
    "fixture/GenerateEmbedUrlForRegisteredUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GenerateEmbedUrlForRegisteredUser)

responseGetDashboardEmbedUrl :: GetDashboardEmbedUrlResponse -> TestTree
responseGetDashboardEmbedUrl =
  res
    "GetDashboardEmbedUrlResponse"
    "fixture/GetDashboardEmbedUrlResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDashboardEmbedUrl)

responseGetSessionEmbedUrl :: GetSessionEmbedUrlResponse -> TestTree
responseGetSessionEmbedUrl =
  res
    "GetSessionEmbedUrlResponse"
    "fixture/GetSessionEmbedUrlResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSessionEmbedUrl)

responseListAnalyses :: ListAnalysesResponse -> TestTree
responseListAnalyses =
  res
    "ListAnalysesResponse"
    "fixture/ListAnalysesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAnalyses)

responseListAssetBundleExportJobs :: ListAssetBundleExportJobsResponse -> TestTree
responseListAssetBundleExportJobs =
  res
    "ListAssetBundleExportJobsResponse"
    "fixture/ListAssetBundleExportJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAssetBundleExportJobs)

responseListAssetBundleImportJobs :: ListAssetBundleImportJobsResponse -> TestTree
responseListAssetBundleImportJobs =
  res
    "ListAssetBundleImportJobsResponse"
    "fixture/ListAssetBundleImportJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAssetBundleImportJobs)

responseListDashboardVersions :: ListDashboardVersionsResponse -> TestTree
responseListDashboardVersions =
  res
    "ListDashboardVersionsResponse"
    "fixture/ListDashboardVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDashboardVersions)

responseListDashboards :: ListDashboardsResponse -> TestTree
responseListDashboards =
  res
    "ListDashboardsResponse"
    "fixture/ListDashboardsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDashboards)

responseListDataSets :: ListDataSetsResponse -> TestTree
responseListDataSets =
  res
    "ListDataSetsResponse"
    "fixture/ListDataSetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDataSets)

responseListDataSources :: ListDataSourcesResponse -> TestTree
responseListDataSources =
  res
    "ListDataSourcesResponse"
    "fixture/ListDataSourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDataSources)

responseListFolderMembers :: ListFolderMembersResponse -> TestTree
responseListFolderMembers =
  res
    "ListFolderMembersResponse"
    "fixture/ListFolderMembersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFolderMembers)

responseListFolders :: ListFoldersResponse -> TestTree
responseListFolders =
  res
    "ListFoldersResponse"
    "fixture/ListFoldersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFolders)

responseListGroupMemberships :: ListGroupMembershipsResponse -> TestTree
responseListGroupMemberships =
  res
    "ListGroupMembershipsResponse"
    "fixture/ListGroupMembershipsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListGroupMemberships)

responseListGroups :: ListGroupsResponse -> TestTree
responseListGroups =
  res
    "ListGroupsResponse"
    "fixture/ListGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListGroups)

responseListIAMPolicyAssignments :: ListIAMPolicyAssignmentsResponse -> TestTree
responseListIAMPolicyAssignments =
  res
    "ListIAMPolicyAssignmentsResponse"
    "fixture/ListIAMPolicyAssignmentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListIAMPolicyAssignments)

responseListIAMPolicyAssignmentsForUser :: ListIAMPolicyAssignmentsForUserResponse -> TestTree
responseListIAMPolicyAssignmentsForUser =
  res
    "ListIAMPolicyAssignmentsForUserResponse"
    "fixture/ListIAMPolicyAssignmentsForUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListIAMPolicyAssignmentsForUser)

responseListIngestions :: ListIngestionsResponse -> TestTree
responseListIngestions =
  res
    "ListIngestionsResponse"
    "fixture/ListIngestionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListIngestions)

responseListNamespaces :: ListNamespacesResponse -> TestTree
responseListNamespaces =
  res
    "ListNamespacesResponse"
    "fixture/ListNamespacesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListNamespaces)

responseListRefreshSchedules :: ListRefreshSchedulesResponse -> TestTree
responseListRefreshSchedules =
  res
    "ListRefreshSchedulesResponse"
    "fixture/ListRefreshSchedulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRefreshSchedules)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListTemplateAliases :: ListTemplateAliasesResponse -> TestTree
responseListTemplateAliases =
  res
    "ListTemplateAliasesResponse"
    "fixture/ListTemplateAliasesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTemplateAliases)

responseListTemplateVersions :: ListTemplateVersionsResponse -> TestTree
responseListTemplateVersions =
  res
    "ListTemplateVersionsResponse"
    "fixture/ListTemplateVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTemplateVersions)

responseListTemplates :: ListTemplatesResponse -> TestTree
responseListTemplates =
  res
    "ListTemplatesResponse"
    "fixture/ListTemplatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTemplates)

responseListThemeAliases :: ListThemeAliasesResponse -> TestTree
responseListThemeAliases =
  res
    "ListThemeAliasesResponse"
    "fixture/ListThemeAliasesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListThemeAliases)

responseListThemeVersions :: ListThemeVersionsResponse -> TestTree
responseListThemeVersions =
  res
    "ListThemeVersionsResponse"
    "fixture/ListThemeVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListThemeVersions)

responseListThemes :: ListThemesResponse -> TestTree
responseListThemes =
  res
    "ListThemesResponse"
    "fixture/ListThemesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListThemes)

responseListTopicRefreshSchedules :: ListTopicRefreshSchedulesResponse -> TestTree
responseListTopicRefreshSchedules =
  res
    "ListTopicRefreshSchedulesResponse"
    "fixture/ListTopicRefreshSchedulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTopicRefreshSchedules)

responseListTopics :: ListTopicsResponse -> TestTree
responseListTopics =
  res
    "ListTopicsResponse"
    "fixture/ListTopicsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTopics)

responseListUserGroups :: ListUserGroupsResponse -> TestTree
responseListUserGroups =
  res
    "ListUserGroupsResponse"
    "fixture/ListUserGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListUserGroups)

responseListUsers :: ListUsersResponse -> TestTree
responseListUsers =
  res
    "ListUsersResponse"
    "fixture/ListUsersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListUsers)

responseListVPCConnections :: ListVPCConnectionsResponse -> TestTree
responseListVPCConnections =
  res
    "ListVPCConnectionsResponse"
    "fixture/ListVPCConnectionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListVPCConnections)

responsePutDataSetRefreshProperties :: PutDataSetRefreshPropertiesResponse -> TestTree
responsePutDataSetRefreshProperties =
  res
    "PutDataSetRefreshPropertiesResponse"
    "fixture/PutDataSetRefreshPropertiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutDataSetRefreshProperties)

responseRegisterUser :: RegisterUserResponse -> TestTree
responseRegisterUser =
  res
    "RegisterUserResponse"
    "fixture/RegisterUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterUser)

responseRestoreAnalysis :: RestoreAnalysisResponse -> TestTree
responseRestoreAnalysis =
  res
    "RestoreAnalysisResponse"
    "fixture/RestoreAnalysisResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RestoreAnalysis)

responseSearchAnalyses :: SearchAnalysesResponse -> TestTree
responseSearchAnalyses =
  res
    "SearchAnalysesResponse"
    "fixture/SearchAnalysesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchAnalyses)

responseSearchDashboards :: SearchDashboardsResponse -> TestTree
responseSearchDashboards =
  res
    "SearchDashboardsResponse"
    "fixture/SearchDashboardsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchDashboards)

responseSearchDataSets :: SearchDataSetsResponse -> TestTree
responseSearchDataSets =
  res
    "SearchDataSetsResponse"
    "fixture/SearchDataSetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchDataSets)

responseSearchDataSources :: SearchDataSourcesResponse -> TestTree
responseSearchDataSources =
  res
    "SearchDataSourcesResponse"
    "fixture/SearchDataSourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchDataSources)

responseSearchFolders :: SearchFoldersResponse -> TestTree
responseSearchFolders =
  res
    "SearchFoldersResponse"
    "fixture/SearchFoldersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchFolders)

responseSearchGroups :: SearchGroupsResponse -> TestTree
responseSearchGroups =
  res
    "SearchGroupsResponse"
    "fixture/SearchGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchGroups)

responseStartAssetBundleExportJob :: StartAssetBundleExportJobResponse -> TestTree
responseStartAssetBundleExportJob =
  res
    "StartAssetBundleExportJobResponse"
    "fixture/StartAssetBundleExportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartAssetBundleExportJob)

responseStartAssetBundleImportJob :: StartAssetBundleImportJobResponse -> TestTree
responseStartAssetBundleImportJob =
  res
    "StartAssetBundleImportJobResponse"
    "fixture/StartAssetBundleImportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartAssetBundleImportJob)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateAccountCustomization :: UpdateAccountCustomizationResponse -> TestTree
responseUpdateAccountCustomization =
  res
    "UpdateAccountCustomizationResponse"
    "fixture/UpdateAccountCustomizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAccountCustomization)

responseUpdateAccountSettings :: UpdateAccountSettingsResponse -> TestTree
responseUpdateAccountSettings =
  res
    "UpdateAccountSettingsResponse"
    "fixture/UpdateAccountSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAccountSettings)

responseUpdateAnalysis :: UpdateAnalysisResponse -> TestTree
responseUpdateAnalysis =
  res
    "UpdateAnalysisResponse"
    "fixture/UpdateAnalysisResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAnalysis)

responseUpdateAnalysisPermissions :: UpdateAnalysisPermissionsResponse -> TestTree
responseUpdateAnalysisPermissions =
  res
    "UpdateAnalysisPermissionsResponse"
    "fixture/UpdateAnalysisPermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAnalysisPermissions)

responseUpdateDashboard :: UpdateDashboardResponse -> TestTree
responseUpdateDashboard =
  res
    "UpdateDashboardResponse"
    "fixture/UpdateDashboardResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDashboard)

responseUpdateDashboardPermissions :: UpdateDashboardPermissionsResponse -> TestTree
responseUpdateDashboardPermissions =
  res
    "UpdateDashboardPermissionsResponse"
    "fixture/UpdateDashboardPermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDashboardPermissions)

responseUpdateDashboardPublishedVersion :: UpdateDashboardPublishedVersionResponse -> TestTree
responseUpdateDashboardPublishedVersion =
  res
    "UpdateDashboardPublishedVersionResponse"
    "fixture/UpdateDashboardPublishedVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDashboardPublishedVersion)

responseUpdateDataSet :: UpdateDataSetResponse -> TestTree
responseUpdateDataSet =
  res
    "UpdateDataSetResponse"
    "fixture/UpdateDataSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDataSet)

responseUpdateDataSetPermissions :: UpdateDataSetPermissionsResponse -> TestTree
responseUpdateDataSetPermissions =
  res
    "UpdateDataSetPermissionsResponse"
    "fixture/UpdateDataSetPermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDataSetPermissions)

responseUpdateDataSource :: UpdateDataSourceResponse -> TestTree
responseUpdateDataSource =
  res
    "UpdateDataSourceResponse"
    "fixture/UpdateDataSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDataSource)

responseUpdateDataSourcePermissions :: UpdateDataSourcePermissionsResponse -> TestTree
responseUpdateDataSourcePermissions =
  res
    "UpdateDataSourcePermissionsResponse"
    "fixture/UpdateDataSourcePermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDataSourcePermissions)

responseUpdateFolder :: UpdateFolderResponse -> TestTree
responseUpdateFolder =
  res
    "UpdateFolderResponse"
    "fixture/UpdateFolderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFolder)

responseUpdateFolderPermissions :: UpdateFolderPermissionsResponse -> TestTree
responseUpdateFolderPermissions =
  res
    "UpdateFolderPermissionsResponse"
    "fixture/UpdateFolderPermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFolderPermissions)

responseUpdateGroup :: UpdateGroupResponse -> TestTree
responseUpdateGroup =
  res
    "UpdateGroupResponse"
    "fixture/UpdateGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateGroup)

responseUpdateIAMPolicyAssignment :: UpdateIAMPolicyAssignmentResponse -> TestTree
responseUpdateIAMPolicyAssignment =
  res
    "UpdateIAMPolicyAssignmentResponse"
    "fixture/UpdateIAMPolicyAssignmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateIAMPolicyAssignment)

responseUpdateIpRestriction :: UpdateIpRestrictionResponse -> TestTree
responseUpdateIpRestriction =
  res
    "UpdateIpRestrictionResponse"
    "fixture/UpdateIpRestrictionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateIpRestriction)

responseUpdatePublicSharingSettings :: UpdatePublicSharingSettingsResponse -> TestTree
responseUpdatePublicSharingSettings =
  res
    "UpdatePublicSharingSettingsResponse"
    "fixture/UpdatePublicSharingSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePublicSharingSettings)

responseUpdateRefreshSchedule :: UpdateRefreshScheduleResponse -> TestTree
responseUpdateRefreshSchedule =
  res
    "UpdateRefreshScheduleResponse"
    "fixture/UpdateRefreshScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRefreshSchedule)

responseUpdateTemplate :: UpdateTemplateResponse -> TestTree
responseUpdateTemplate =
  res
    "UpdateTemplateResponse"
    "fixture/UpdateTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTemplate)

responseUpdateTemplateAlias :: UpdateTemplateAliasResponse -> TestTree
responseUpdateTemplateAlias =
  res
    "UpdateTemplateAliasResponse"
    "fixture/UpdateTemplateAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTemplateAlias)

responseUpdateTemplatePermissions :: UpdateTemplatePermissionsResponse -> TestTree
responseUpdateTemplatePermissions =
  res
    "UpdateTemplatePermissionsResponse"
    "fixture/UpdateTemplatePermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTemplatePermissions)

responseUpdateTheme :: UpdateThemeResponse -> TestTree
responseUpdateTheme =
  res
    "UpdateThemeResponse"
    "fixture/UpdateThemeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTheme)

responseUpdateThemeAlias :: UpdateThemeAliasResponse -> TestTree
responseUpdateThemeAlias =
  res
    "UpdateThemeAliasResponse"
    "fixture/UpdateThemeAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateThemeAlias)

responseUpdateThemePermissions :: UpdateThemePermissionsResponse -> TestTree
responseUpdateThemePermissions =
  res
    "UpdateThemePermissionsResponse"
    "fixture/UpdateThemePermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateThemePermissions)

responseUpdateTopic :: UpdateTopicResponse -> TestTree
responseUpdateTopic =
  res
    "UpdateTopicResponse"
    "fixture/UpdateTopicResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTopic)

responseUpdateTopicPermissions :: UpdateTopicPermissionsResponse -> TestTree
responseUpdateTopicPermissions =
  res
    "UpdateTopicPermissionsResponse"
    "fixture/UpdateTopicPermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTopicPermissions)

responseUpdateTopicRefreshSchedule :: UpdateTopicRefreshScheduleResponse -> TestTree
responseUpdateTopicRefreshSchedule =
  res
    "UpdateTopicRefreshScheduleResponse"
    "fixture/UpdateTopicRefreshScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTopicRefreshSchedule)

responseUpdateUser :: UpdateUserResponse -> TestTree
responseUpdateUser =
  res
    "UpdateUserResponse"
    "fixture/UpdateUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateUser)

responseUpdateVPCConnection :: UpdateVPCConnectionResponse -> TestTree
responseUpdateVPCConnection =
  res
    "UpdateVPCConnectionResponse"
    "fixture/UpdateVPCConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateVPCConnection)
