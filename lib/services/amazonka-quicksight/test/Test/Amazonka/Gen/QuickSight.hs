{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.QuickSight
-- Copyright   : (c) 2013-2022 Brendan Hay
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
--         , requestDeleteUser $
--             newDeleteUser
--
--         , requestDeleteUserByPrincipalId $
--             newDeleteUserByPrincipalId
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
--         , requestDescribeAnalysisPermissions $
--             newDescribeAnalysisPermissions
--
--         , requestDescribeDashboard $
--             newDescribeDashboard
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
--         , requestDescribeTemplate $
--             newDescribeTemplate
--
--         , requestDescribeTemplateAlias $
--             newDescribeTemplateAlias
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
--         , requestDescribeUser $
--             newDescribeUser
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
--         , requestListUserGroups $
--             newListUserGroups
--
--         , requestListUsers $
--             newListUsers
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
--         , requestUpdateUser $
--             newUpdateUser
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
--         , responseDeleteUser $
--             newDeleteUserResponse
--
--         , responseDeleteUserByPrincipalId $
--             newDeleteUserByPrincipalIdResponse
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
--         , responseDescribeAnalysisPermissions $
--             newDescribeAnalysisPermissionsResponse
--
--         , responseDescribeDashboard $
--             newDescribeDashboardResponse
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
--         , responseDescribeTemplate $
--             newDescribeTemplateResponse
--
--         , responseDescribeTemplateAlias $
--             newDescribeTemplateAliasResponse
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
--         , responseDescribeUser $
--             newDescribeUserResponse
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
--         , responseListUserGroups $
--             newListUserGroupsResponse
--
--         , responseListUsers $
--             newListUsersResponse
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
--         , responseUpdateUser $
--             newUpdateUserResponse
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

requestDescribeAnalysisPermissions :: DescribeAnalysisPermissions -> TestTree
requestDescribeAnalysisPermissions =
  req
    "DescribeAnalysisPermissions"
    "fixture/DescribeAnalysisPermissions.yaml"

requestDescribeDashboard :: DescribeDashboard -> TestTree
requestDescribeDashboard =
  req
    "DescribeDashboard"
    "fixture/DescribeDashboard.yaml"

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

requestDescribeUser :: DescribeUser -> TestTree
requestDescribeUser =
  req
    "DescribeUser"
    "fixture/DescribeUser.yaml"

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

requestUpdateUser :: UpdateUser -> TestTree
requestUpdateUser =
  req
    "UpdateUser"
    "fixture/UpdateUser.yaml"

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

responseDescribeAnalysisPermissions :: DescribeAnalysisPermissionsResponse -> TestTree
responseDescribeAnalysisPermissions =
  res
    "DescribeAnalysisPermissionsResponse"
    "fixture/DescribeAnalysisPermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAnalysisPermissions)

responseDescribeDashboard :: DescribeDashboardResponse -> TestTree
responseDescribeDashboard =
  res
    "DescribeDashboardResponse"
    "fixture/DescribeDashboardResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDashboard)

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

responseDescribeUser :: DescribeUserResponse -> TestTree
responseDescribeUser =
  res
    "DescribeUserResponse"
    "fixture/DescribeUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeUser)

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

responseUpdateUser :: UpdateUserResponse -> TestTree
responseUpdateUser =
  res
    "UpdateUserResponse"
    "fixture/UpdateUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateUser)
