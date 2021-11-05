{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.QuickSight
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.QuickSight where

import Amazonka.QuickSight
import qualified Data.Proxy as Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.QuickSight.Internal
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
--         , requestUpdateDataSource $
--             newUpdateDataSource
--
--         , requestDeleteUserByPrincipalId $
--             newDeleteUserByPrincipalId
--
--         , requestDeleteDataSource $
--             newDeleteDataSource
--
--         , requestCreateTemplate $
--             newCreateTemplate
--
--         , requestDeleteGroupMembership $
--             newDeleteGroupMembership
--
--         , requestDescribeThemePermissions $
--             newDescribeThemePermissions
--
--         , requestListGroupMemberships $
--             newListGroupMemberships
--
--         , requestListFolders $
--             newListFolders
--
--         , requestDescribeDataSetPermissions $
--             newDescribeDataSetPermissions
--
--         , requestDeleteIAMPolicyAssignment $
--             newDeleteIAMPolicyAssignment
--
--         , requestUpdateIAMPolicyAssignment $
--             newUpdateIAMPolicyAssignment
--
--         , requestDescribeIngestion $
--             newDescribeIngestion
--
--         , requestDeleteFolder $
--             newDeleteFolder
--
--         , requestUpdateFolder $
--             newUpdateFolder
--
--         , requestListUserGroups $
--             newListUserGroups
--
--         , requestUpdateDashboardPublishedVersion $
--             newUpdateDashboardPublishedVersion
--
--         , requestDescribeAnalysisPermissions $
--             newDescribeAnalysisPermissions
--
--         , requestDeleteTemplateAlias $
--             newDeleteTemplateAlias
--
--         , requestUpdateTemplateAlias $
--             newUpdateTemplateAlias
--
--         , requestDescribeAnalysis $
--             newDescribeAnalysis
--
--         , requestUpdateFolderPermissions $
--             newUpdateFolderPermissions
--
--         , requestDescribeDataSet $
--             newDescribeDataSet
--
--         , requestListNamespaces $
--             newListNamespaces
--
--         , requestDeleteNamespace $
--             newDeleteNamespace
--
--         , requestCreateFolder $
--             newCreateFolder
--
--         , requestDescribeGroup $
--             newDescribeGroup
--
--         , requestDescribeThemeAlias $
--             newDescribeThemeAlias
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestUpdateAccountSettings $
--             newUpdateAccountSettings
--
--         , requestDescribeTemplatePermissions $
--             newDescribeTemplatePermissions
--
--         , requestListDashboards $
--             newListDashboards
--
--         , requestDescribeTemplate $
--             newDescribeTemplate
--
--         , requestDeleteFolderMembership $
--             newDeleteFolderMembership
--
--         , requestCreateTheme $
--             newCreateTheme
--
--         , requestListUsers $
--             newListUsers
--
--         , requestCreateFolderMembership $
--             newCreateFolderMembership
--
--         , requestUpdateThemePermissions $
--             newUpdateThemePermissions
--
--         , requestGetSessionEmbedUrl $
--             newGetSessionEmbedUrl
--
--         , requestCreateDashboard $
--             newCreateDashboard
--
--         , requestRegisterUser $
--             newRegisterUser
--
--         , requestDescribeDataSource $
--             newDescribeDataSource
--
--         , requestDescribeFolderResolvedPermissions $
--             newDescribeFolderResolvedPermissions
--
--         , requestUpdateAnalysisPermissions $
--             newUpdateAnalysisPermissions
--
--         , requestDeleteDataSet $
--             newDeleteDataSet
--
--         , requestUpdateDataSet $
--             newUpdateDataSet
--
--         , requestListThemeAliases $
--             newListThemeAliases
--
--         , requestUpdateAnalysis $
--             newUpdateAnalysis
--
--         , requestDeleteAnalysis $
--             newDeleteAnalysis
--
--         , requestSearchFolders $
--             newSearchFolders
--
--         , requestDescribeFolderPermissions $
--             newDescribeFolderPermissions
--
--         , requestUpdateDataSetPermissions $
--             newUpdateDataSetPermissions
--
--         , requestCreateThemeAlias $
--             newCreateThemeAlias
--
--         , requestDescribeFolder $
--             newDescribeFolder
--
--         , requestDescribeTemplateAlias $
--             newDescribeTemplateAlias
--
--         , requestDescribeIAMPolicyAssignment $
--             newDescribeIAMPolicyAssignment
--
--         , requestCreateIngestion $
--             newCreateIngestion
--
--         , requestCreateDataSet $
--             newCreateDataSet
--
--         , requestCreateDataSource $
--             newCreateDataSource
--
--         , requestListTemplates $
--             newListTemplates
--
--         , requestDescribeAccountSettings $
--             newDescribeAccountSettings
--
--         , requestUpdateTemplate $
--             newUpdateTemplate
--
--         , requestDeleteTemplate $
--             newDeleteTemplate
--
--         , requestUpdateTemplatePermissions $
--             newUpdateTemplatePermissions
--
--         , requestSearchDashboards $
--             newSearchDashboards
--
--         , requestUpdateDataSourcePermissions $
--             newUpdateDataSourcePermissions
--
--         , requestUpdateIpRestriction $
--             newUpdateIpRestriction
--
--         , requestDescribeUser $
--             newDescribeUser
--
--         , requestDescribeTheme $
--             newDescribeTheme
--
--         , requestListAnalyses $
--             newListAnalyses
--
--         , requestListDataSources $
--             newListDataSources
--
--         , requestListFolderMembers $
--             newListFolderMembers
--
--         , requestListIAMPolicyAssignments $
--             newListIAMPolicyAssignments
--
--         , requestListDashboardVersions $
--             newListDashboardVersions
--
--         , requestCreateNamespace $
--             newCreateNamespace
--
--         , requestCreateIAMPolicyAssignment $
--             newCreateIAMPolicyAssignment
--
--         , requestRestoreAnalysis $
--             newRestoreAnalysis
--
--         , requestCreateTemplateAlias $
--             newCreateTemplateAlias
--
--         , requestListTemplateAliases $
--             newListTemplateAliases
--
--         , requestGetDashboardEmbedUrl $
--             newGetDashboardEmbedUrl
--
--         , requestGenerateEmbedUrlForAnonymousUser $
--             newGenerateEmbedUrlForAnonymousUser
--
--         , requestListThemeVersions $
--             newListThemeVersions
--
--         , requestUpdateDashboardPermissions $
--             newUpdateDashboardPermissions
--
--         , requestListIAMPolicyAssignmentsForUser $
--             newListIAMPolicyAssignmentsForUser
--
--         , requestDeleteAccountCustomization $
--             newDeleteAccountCustomization
--
--         , requestUpdateAccountCustomization $
--             newUpdateAccountCustomization
--
--         , requestDeleteDashboard $
--             newDeleteDashboard
--
--         , requestUpdateDashboard $
--             newUpdateDashboard
--
--         , requestDescribeDataSourcePermissions $
--             newDescribeDataSourcePermissions
--
--         , requestUpdateUser $
--             newUpdateUser
--
--         , requestDeleteUser $
--             newDeleteUser
--
--         , requestSearchAnalyses $
--             newSearchAnalyses
--
--         , requestCreateAccountCustomization $
--             newCreateAccountCustomization
--
--         , requestDescribeIpRestriction $
--             newDescribeIpRestriction
--
--         , requestListThemes $
--             newListThemes
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUpdateTheme $
--             newUpdateTheme
--
--         , requestDeleteTheme $
--             newDeleteTheme
--
--         , requestListIngestions $
--             newListIngestions
--
--         , requestCreateGroup $
--             newCreateGroup
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestListTemplateVersions $
--             newListTemplateVersions
--
--         , requestListDataSets $
--             newListDataSets
--
--         , requestDeleteThemeAlias $
--             newDeleteThemeAlias
--
--         , requestUpdateThemeAlias $
--             newUpdateThemeAlias
--
--         , requestDeleteGroup $
--             newDeleteGroup
--
--         , requestUpdateGroup $
--             newUpdateGroup
--
--         , requestDescribeNamespace $
--             newDescribeNamespace
--
--         , requestListGroups $
--             newListGroups
--
--         , requestCreateAnalysis $
--             newCreateAnalysis
--
--         , requestDescribeAccountCustomization $
--             newDescribeAccountCustomization
--
--         , requestGenerateEmbedUrlForRegisteredUser $
--             newGenerateEmbedUrlForRegisteredUser
--
--         , requestDescribeDashboard $
--             newDescribeDashboard
--
--         , requestCreateGroupMembership $
--             newCreateGroupMembership
--
--         , requestDescribeDashboardPermissions $
--             newDescribeDashboardPermissions
--
--           ]

--     , testGroup "response"
--         [ responseCancelIngestion $
--             newCancelIngestionResponse
--
--         , responseUpdateDataSource $
--             newUpdateDataSourceResponse
--
--         , responseDeleteUserByPrincipalId $
--             newDeleteUserByPrincipalIdResponse
--
--         , responseDeleteDataSource $
--             newDeleteDataSourceResponse
--
--         , responseCreateTemplate $
--             newCreateTemplateResponse
--
--         , responseDeleteGroupMembership $
--             newDeleteGroupMembershipResponse
--
--         , responseDescribeThemePermissions $
--             newDescribeThemePermissionsResponse
--
--         , responseListGroupMemberships $
--             newListGroupMembershipsResponse
--
--         , responseListFolders $
--             newListFoldersResponse
--
--         , responseDescribeDataSetPermissions $
--             newDescribeDataSetPermissionsResponse
--
--         , responseDeleteIAMPolicyAssignment $
--             newDeleteIAMPolicyAssignmentResponse
--
--         , responseUpdateIAMPolicyAssignment $
--             newUpdateIAMPolicyAssignmentResponse
--
--         , responseDescribeIngestion $
--             newDescribeIngestionResponse
--
--         , responseDeleteFolder $
--             newDeleteFolderResponse
--
--         , responseUpdateFolder $
--             newUpdateFolderResponse
--
--         , responseListUserGroups $
--             newListUserGroupsResponse
--
--         , responseUpdateDashboardPublishedVersion $
--             newUpdateDashboardPublishedVersionResponse
--
--         , responseDescribeAnalysisPermissions $
--             newDescribeAnalysisPermissionsResponse
--
--         , responseDeleteTemplateAlias $
--             newDeleteTemplateAliasResponse
--
--         , responseUpdateTemplateAlias $
--             newUpdateTemplateAliasResponse
--
--         , responseDescribeAnalysis $
--             newDescribeAnalysisResponse
--
--         , responseUpdateFolderPermissions $
--             newUpdateFolderPermissionsResponse
--
--         , responseDescribeDataSet $
--             newDescribeDataSetResponse
--
--         , responseListNamespaces $
--             newListNamespacesResponse
--
--         , responseDeleteNamespace $
--             newDeleteNamespaceResponse
--
--         , responseCreateFolder $
--             newCreateFolderResponse
--
--         , responseDescribeGroup $
--             newDescribeGroupResponse
--
--         , responseDescribeThemeAlias $
--             newDescribeThemeAliasResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseUpdateAccountSettings $
--             newUpdateAccountSettingsResponse
--
--         , responseDescribeTemplatePermissions $
--             newDescribeTemplatePermissionsResponse
--
--         , responseListDashboards $
--             newListDashboardsResponse
--
--         , responseDescribeTemplate $
--             newDescribeTemplateResponse
--
--         , responseDeleteFolderMembership $
--             newDeleteFolderMembershipResponse
--
--         , responseCreateTheme $
--             newCreateThemeResponse
--
--         , responseListUsers $
--             newListUsersResponse
--
--         , responseCreateFolderMembership $
--             newCreateFolderMembershipResponse
--
--         , responseUpdateThemePermissions $
--             newUpdateThemePermissionsResponse
--
--         , responseGetSessionEmbedUrl $
--             newGetSessionEmbedUrlResponse
--
--         , responseCreateDashboard $
--             newCreateDashboardResponse
--
--         , responseRegisterUser $
--             newRegisterUserResponse
--
--         , responseDescribeDataSource $
--             newDescribeDataSourceResponse
--
--         , responseDescribeFolderResolvedPermissions $
--             newDescribeFolderResolvedPermissionsResponse
--
--         , responseUpdateAnalysisPermissions $
--             newUpdateAnalysisPermissionsResponse
--
--         , responseDeleteDataSet $
--             newDeleteDataSetResponse
--
--         , responseUpdateDataSet $
--             newUpdateDataSetResponse
--
--         , responseListThemeAliases $
--             newListThemeAliasesResponse
--
--         , responseUpdateAnalysis $
--             newUpdateAnalysisResponse
--
--         , responseDeleteAnalysis $
--             newDeleteAnalysisResponse
--
--         , responseSearchFolders $
--             newSearchFoldersResponse
--
--         , responseDescribeFolderPermissions $
--             newDescribeFolderPermissionsResponse
--
--         , responseUpdateDataSetPermissions $
--             newUpdateDataSetPermissionsResponse
--
--         , responseCreateThemeAlias $
--             newCreateThemeAliasResponse
--
--         , responseDescribeFolder $
--             newDescribeFolderResponse
--
--         , responseDescribeTemplateAlias $
--             newDescribeTemplateAliasResponse
--
--         , responseDescribeIAMPolicyAssignment $
--             newDescribeIAMPolicyAssignmentResponse
--
--         , responseCreateIngestion $
--             newCreateIngestionResponse
--
--         , responseCreateDataSet $
--             newCreateDataSetResponse
--
--         , responseCreateDataSource $
--             newCreateDataSourceResponse
--
--         , responseListTemplates $
--             newListTemplatesResponse
--
--         , responseDescribeAccountSettings $
--             newDescribeAccountSettingsResponse
--
--         , responseUpdateTemplate $
--             newUpdateTemplateResponse
--
--         , responseDeleteTemplate $
--             newDeleteTemplateResponse
--
--         , responseUpdateTemplatePermissions $
--             newUpdateTemplatePermissionsResponse
--
--         , responseSearchDashboards $
--             newSearchDashboardsResponse
--
--         , responseUpdateDataSourcePermissions $
--             newUpdateDataSourcePermissionsResponse
--
--         , responseUpdateIpRestriction $
--             newUpdateIpRestrictionResponse
--
--         , responseDescribeUser $
--             newDescribeUserResponse
--
--         , responseDescribeTheme $
--             newDescribeThemeResponse
--
--         , responseListAnalyses $
--             newListAnalysesResponse
--
--         , responseListDataSources $
--             newListDataSourcesResponse
--
--         , responseListFolderMembers $
--             newListFolderMembersResponse
--
--         , responseListIAMPolicyAssignments $
--             newListIAMPolicyAssignmentsResponse
--
--         , responseListDashboardVersions $
--             newListDashboardVersionsResponse
--
--         , responseCreateNamespace $
--             newCreateNamespaceResponse
--
--         , responseCreateIAMPolicyAssignment $
--             newCreateIAMPolicyAssignmentResponse
--
--         , responseRestoreAnalysis $
--             newRestoreAnalysisResponse
--
--         , responseCreateTemplateAlias $
--             newCreateTemplateAliasResponse
--
--         , responseListTemplateAliases $
--             newListTemplateAliasesResponse
--
--         , responseGetDashboardEmbedUrl $
--             newGetDashboardEmbedUrlResponse
--
--         , responseGenerateEmbedUrlForAnonymousUser $
--             newGenerateEmbedUrlForAnonymousUserResponse
--
--         , responseListThemeVersions $
--             newListThemeVersionsResponse
--
--         , responseUpdateDashboardPermissions $
--             newUpdateDashboardPermissionsResponse
--
--         , responseListIAMPolicyAssignmentsForUser $
--             newListIAMPolicyAssignmentsForUserResponse
--
--         , responseDeleteAccountCustomization $
--             newDeleteAccountCustomizationResponse
--
--         , responseUpdateAccountCustomization $
--             newUpdateAccountCustomizationResponse
--
--         , responseDeleteDashboard $
--             newDeleteDashboardResponse
--
--         , responseUpdateDashboard $
--             newUpdateDashboardResponse
--
--         , responseDescribeDataSourcePermissions $
--             newDescribeDataSourcePermissionsResponse
--
--         , responseUpdateUser $
--             newUpdateUserResponse
--
--         , responseDeleteUser $
--             newDeleteUserResponse
--
--         , responseSearchAnalyses $
--             newSearchAnalysesResponse
--
--         , responseCreateAccountCustomization $
--             newCreateAccountCustomizationResponse
--
--         , responseDescribeIpRestriction $
--             newDescribeIpRestrictionResponse
--
--         , responseListThemes $
--             newListThemesResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUpdateTheme $
--             newUpdateThemeResponse
--
--         , responseDeleteTheme $
--             newDeleteThemeResponse
--
--         , responseListIngestions $
--             newListIngestionsResponse
--
--         , responseCreateGroup $
--             newCreateGroupResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseListTemplateVersions $
--             newListTemplateVersionsResponse
--
--         , responseListDataSets $
--             newListDataSetsResponse
--
--         , responseDeleteThemeAlias $
--             newDeleteThemeAliasResponse
--
--         , responseUpdateThemeAlias $
--             newUpdateThemeAliasResponse
--
--         , responseDeleteGroup $
--             newDeleteGroupResponse
--
--         , responseUpdateGroup $
--             newUpdateGroupResponse
--
--         , responseDescribeNamespace $
--             newDescribeNamespaceResponse
--
--         , responseListGroups $
--             newListGroupsResponse
--
--         , responseCreateAnalysis $
--             newCreateAnalysisResponse
--
--         , responseDescribeAccountCustomization $
--             newDescribeAccountCustomizationResponse
--
--         , responseGenerateEmbedUrlForRegisteredUser $
--             newGenerateEmbedUrlForRegisteredUserResponse
--
--         , responseDescribeDashboard $
--             newDescribeDashboardResponse
--
--         , responseCreateGroupMembership $
--             newCreateGroupMembershipResponse
--
--         , responseDescribeDashboardPermissions $
--             newDescribeDashboardPermissionsResponse
--
--           ]
--     ]

-- Requests

requestCancelIngestion :: CancelIngestion -> TestTree
requestCancelIngestion =
  req
    "CancelIngestion"
    "fixture/CancelIngestion.yaml"

requestUpdateDataSource :: UpdateDataSource -> TestTree
requestUpdateDataSource =
  req
    "UpdateDataSource"
    "fixture/UpdateDataSource.yaml"

requestDeleteUserByPrincipalId :: DeleteUserByPrincipalId -> TestTree
requestDeleteUserByPrincipalId =
  req
    "DeleteUserByPrincipalId"
    "fixture/DeleteUserByPrincipalId.yaml"

requestDeleteDataSource :: DeleteDataSource -> TestTree
requestDeleteDataSource =
  req
    "DeleteDataSource"
    "fixture/DeleteDataSource.yaml"

requestCreateTemplate :: CreateTemplate -> TestTree
requestCreateTemplate =
  req
    "CreateTemplate"
    "fixture/CreateTemplate.yaml"

requestDeleteGroupMembership :: DeleteGroupMembership -> TestTree
requestDeleteGroupMembership =
  req
    "DeleteGroupMembership"
    "fixture/DeleteGroupMembership.yaml"

requestDescribeThemePermissions :: DescribeThemePermissions -> TestTree
requestDescribeThemePermissions =
  req
    "DescribeThemePermissions"
    "fixture/DescribeThemePermissions.yaml"

requestListGroupMemberships :: ListGroupMemberships -> TestTree
requestListGroupMemberships =
  req
    "ListGroupMemberships"
    "fixture/ListGroupMemberships.yaml"

requestListFolders :: ListFolders -> TestTree
requestListFolders =
  req
    "ListFolders"
    "fixture/ListFolders.yaml"

requestDescribeDataSetPermissions :: DescribeDataSetPermissions -> TestTree
requestDescribeDataSetPermissions =
  req
    "DescribeDataSetPermissions"
    "fixture/DescribeDataSetPermissions.yaml"

requestDeleteIAMPolicyAssignment :: DeleteIAMPolicyAssignment -> TestTree
requestDeleteIAMPolicyAssignment =
  req
    "DeleteIAMPolicyAssignment"
    "fixture/DeleteIAMPolicyAssignment.yaml"

requestUpdateIAMPolicyAssignment :: UpdateIAMPolicyAssignment -> TestTree
requestUpdateIAMPolicyAssignment =
  req
    "UpdateIAMPolicyAssignment"
    "fixture/UpdateIAMPolicyAssignment.yaml"

requestDescribeIngestion :: DescribeIngestion -> TestTree
requestDescribeIngestion =
  req
    "DescribeIngestion"
    "fixture/DescribeIngestion.yaml"

requestDeleteFolder :: DeleteFolder -> TestTree
requestDeleteFolder =
  req
    "DeleteFolder"
    "fixture/DeleteFolder.yaml"

requestUpdateFolder :: UpdateFolder -> TestTree
requestUpdateFolder =
  req
    "UpdateFolder"
    "fixture/UpdateFolder.yaml"

requestListUserGroups :: ListUserGroups -> TestTree
requestListUserGroups =
  req
    "ListUserGroups"
    "fixture/ListUserGroups.yaml"

requestUpdateDashboardPublishedVersion :: UpdateDashboardPublishedVersion -> TestTree
requestUpdateDashboardPublishedVersion =
  req
    "UpdateDashboardPublishedVersion"
    "fixture/UpdateDashboardPublishedVersion.yaml"

requestDescribeAnalysisPermissions :: DescribeAnalysisPermissions -> TestTree
requestDescribeAnalysisPermissions =
  req
    "DescribeAnalysisPermissions"
    "fixture/DescribeAnalysisPermissions.yaml"

requestDeleteTemplateAlias :: DeleteTemplateAlias -> TestTree
requestDeleteTemplateAlias =
  req
    "DeleteTemplateAlias"
    "fixture/DeleteTemplateAlias.yaml"

requestUpdateTemplateAlias :: UpdateTemplateAlias -> TestTree
requestUpdateTemplateAlias =
  req
    "UpdateTemplateAlias"
    "fixture/UpdateTemplateAlias.yaml"

requestDescribeAnalysis :: DescribeAnalysis -> TestTree
requestDescribeAnalysis =
  req
    "DescribeAnalysis"
    "fixture/DescribeAnalysis.yaml"

requestUpdateFolderPermissions :: UpdateFolderPermissions -> TestTree
requestUpdateFolderPermissions =
  req
    "UpdateFolderPermissions"
    "fixture/UpdateFolderPermissions.yaml"

requestDescribeDataSet :: DescribeDataSet -> TestTree
requestDescribeDataSet =
  req
    "DescribeDataSet"
    "fixture/DescribeDataSet.yaml"

requestListNamespaces :: ListNamespaces -> TestTree
requestListNamespaces =
  req
    "ListNamespaces"
    "fixture/ListNamespaces.yaml"

requestDeleteNamespace :: DeleteNamespace -> TestTree
requestDeleteNamespace =
  req
    "DeleteNamespace"
    "fixture/DeleteNamespace.yaml"

requestCreateFolder :: CreateFolder -> TestTree
requestCreateFolder =
  req
    "CreateFolder"
    "fixture/CreateFolder.yaml"

requestDescribeGroup :: DescribeGroup -> TestTree
requestDescribeGroup =
  req
    "DescribeGroup"
    "fixture/DescribeGroup.yaml"

requestDescribeThemeAlias :: DescribeThemeAlias -> TestTree
requestDescribeThemeAlias =
  req
    "DescribeThemeAlias"
    "fixture/DescribeThemeAlias.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestUpdateAccountSettings :: UpdateAccountSettings -> TestTree
requestUpdateAccountSettings =
  req
    "UpdateAccountSettings"
    "fixture/UpdateAccountSettings.yaml"

requestDescribeTemplatePermissions :: DescribeTemplatePermissions -> TestTree
requestDescribeTemplatePermissions =
  req
    "DescribeTemplatePermissions"
    "fixture/DescribeTemplatePermissions.yaml"

requestListDashboards :: ListDashboards -> TestTree
requestListDashboards =
  req
    "ListDashboards"
    "fixture/ListDashboards.yaml"

requestDescribeTemplate :: DescribeTemplate -> TestTree
requestDescribeTemplate =
  req
    "DescribeTemplate"
    "fixture/DescribeTemplate.yaml"

requestDeleteFolderMembership :: DeleteFolderMembership -> TestTree
requestDeleteFolderMembership =
  req
    "DeleteFolderMembership"
    "fixture/DeleteFolderMembership.yaml"

requestCreateTheme :: CreateTheme -> TestTree
requestCreateTheme =
  req
    "CreateTheme"
    "fixture/CreateTheme.yaml"

requestListUsers :: ListUsers -> TestTree
requestListUsers =
  req
    "ListUsers"
    "fixture/ListUsers.yaml"

requestCreateFolderMembership :: CreateFolderMembership -> TestTree
requestCreateFolderMembership =
  req
    "CreateFolderMembership"
    "fixture/CreateFolderMembership.yaml"

requestUpdateThemePermissions :: UpdateThemePermissions -> TestTree
requestUpdateThemePermissions =
  req
    "UpdateThemePermissions"
    "fixture/UpdateThemePermissions.yaml"

requestGetSessionEmbedUrl :: GetSessionEmbedUrl -> TestTree
requestGetSessionEmbedUrl =
  req
    "GetSessionEmbedUrl"
    "fixture/GetSessionEmbedUrl.yaml"

requestCreateDashboard :: CreateDashboard -> TestTree
requestCreateDashboard =
  req
    "CreateDashboard"
    "fixture/CreateDashboard.yaml"

requestRegisterUser :: RegisterUser -> TestTree
requestRegisterUser =
  req
    "RegisterUser"
    "fixture/RegisterUser.yaml"

requestDescribeDataSource :: DescribeDataSource -> TestTree
requestDescribeDataSource =
  req
    "DescribeDataSource"
    "fixture/DescribeDataSource.yaml"

requestDescribeFolderResolvedPermissions :: DescribeFolderResolvedPermissions -> TestTree
requestDescribeFolderResolvedPermissions =
  req
    "DescribeFolderResolvedPermissions"
    "fixture/DescribeFolderResolvedPermissions.yaml"

requestUpdateAnalysisPermissions :: UpdateAnalysisPermissions -> TestTree
requestUpdateAnalysisPermissions =
  req
    "UpdateAnalysisPermissions"
    "fixture/UpdateAnalysisPermissions.yaml"

requestDeleteDataSet :: DeleteDataSet -> TestTree
requestDeleteDataSet =
  req
    "DeleteDataSet"
    "fixture/DeleteDataSet.yaml"

requestUpdateDataSet :: UpdateDataSet -> TestTree
requestUpdateDataSet =
  req
    "UpdateDataSet"
    "fixture/UpdateDataSet.yaml"

requestListThemeAliases :: ListThemeAliases -> TestTree
requestListThemeAliases =
  req
    "ListThemeAliases"
    "fixture/ListThemeAliases.yaml"

requestUpdateAnalysis :: UpdateAnalysis -> TestTree
requestUpdateAnalysis =
  req
    "UpdateAnalysis"
    "fixture/UpdateAnalysis.yaml"

requestDeleteAnalysis :: DeleteAnalysis -> TestTree
requestDeleteAnalysis =
  req
    "DeleteAnalysis"
    "fixture/DeleteAnalysis.yaml"

requestSearchFolders :: SearchFolders -> TestTree
requestSearchFolders =
  req
    "SearchFolders"
    "fixture/SearchFolders.yaml"

requestDescribeFolderPermissions :: DescribeFolderPermissions -> TestTree
requestDescribeFolderPermissions =
  req
    "DescribeFolderPermissions"
    "fixture/DescribeFolderPermissions.yaml"

requestUpdateDataSetPermissions :: UpdateDataSetPermissions -> TestTree
requestUpdateDataSetPermissions =
  req
    "UpdateDataSetPermissions"
    "fixture/UpdateDataSetPermissions.yaml"

requestCreateThemeAlias :: CreateThemeAlias -> TestTree
requestCreateThemeAlias =
  req
    "CreateThemeAlias"
    "fixture/CreateThemeAlias.yaml"

requestDescribeFolder :: DescribeFolder -> TestTree
requestDescribeFolder =
  req
    "DescribeFolder"
    "fixture/DescribeFolder.yaml"

requestDescribeTemplateAlias :: DescribeTemplateAlias -> TestTree
requestDescribeTemplateAlias =
  req
    "DescribeTemplateAlias"
    "fixture/DescribeTemplateAlias.yaml"

requestDescribeIAMPolicyAssignment :: DescribeIAMPolicyAssignment -> TestTree
requestDescribeIAMPolicyAssignment =
  req
    "DescribeIAMPolicyAssignment"
    "fixture/DescribeIAMPolicyAssignment.yaml"

requestCreateIngestion :: CreateIngestion -> TestTree
requestCreateIngestion =
  req
    "CreateIngestion"
    "fixture/CreateIngestion.yaml"

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

requestListTemplates :: ListTemplates -> TestTree
requestListTemplates =
  req
    "ListTemplates"
    "fixture/ListTemplates.yaml"

requestDescribeAccountSettings :: DescribeAccountSettings -> TestTree
requestDescribeAccountSettings =
  req
    "DescribeAccountSettings"
    "fixture/DescribeAccountSettings.yaml"

requestUpdateTemplate :: UpdateTemplate -> TestTree
requestUpdateTemplate =
  req
    "UpdateTemplate"
    "fixture/UpdateTemplate.yaml"

requestDeleteTemplate :: DeleteTemplate -> TestTree
requestDeleteTemplate =
  req
    "DeleteTemplate"
    "fixture/DeleteTemplate.yaml"

requestUpdateTemplatePermissions :: UpdateTemplatePermissions -> TestTree
requestUpdateTemplatePermissions =
  req
    "UpdateTemplatePermissions"
    "fixture/UpdateTemplatePermissions.yaml"

requestSearchDashboards :: SearchDashboards -> TestTree
requestSearchDashboards =
  req
    "SearchDashboards"
    "fixture/SearchDashboards.yaml"

requestUpdateDataSourcePermissions :: UpdateDataSourcePermissions -> TestTree
requestUpdateDataSourcePermissions =
  req
    "UpdateDataSourcePermissions"
    "fixture/UpdateDataSourcePermissions.yaml"

requestUpdateIpRestriction :: UpdateIpRestriction -> TestTree
requestUpdateIpRestriction =
  req
    "UpdateIpRestriction"
    "fixture/UpdateIpRestriction.yaml"

requestDescribeUser :: DescribeUser -> TestTree
requestDescribeUser =
  req
    "DescribeUser"
    "fixture/DescribeUser.yaml"

requestDescribeTheme :: DescribeTheme -> TestTree
requestDescribeTheme =
  req
    "DescribeTheme"
    "fixture/DescribeTheme.yaml"

requestListAnalyses :: ListAnalyses -> TestTree
requestListAnalyses =
  req
    "ListAnalyses"
    "fixture/ListAnalyses.yaml"

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

requestListIAMPolicyAssignments :: ListIAMPolicyAssignments -> TestTree
requestListIAMPolicyAssignments =
  req
    "ListIAMPolicyAssignments"
    "fixture/ListIAMPolicyAssignments.yaml"

requestListDashboardVersions :: ListDashboardVersions -> TestTree
requestListDashboardVersions =
  req
    "ListDashboardVersions"
    "fixture/ListDashboardVersions.yaml"

requestCreateNamespace :: CreateNamespace -> TestTree
requestCreateNamespace =
  req
    "CreateNamespace"
    "fixture/CreateNamespace.yaml"

requestCreateIAMPolicyAssignment :: CreateIAMPolicyAssignment -> TestTree
requestCreateIAMPolicyAssignment =
  req
    "CreateIAMPolicyAssignment"
    "fixture/CreateIAMPolicyAssignment.yaml"

requestRestoreAnalysis :: RestoreAnalysis -> TestTree
requestRestoreAnalysis =
  req
    "RestoreAnalysis"
    "fixture/RestoreAnalysis.yaml"

requestCreateTemplateAlias :: CreateTemplateAlias -> TestTree
requestCreateTemplateAlias =
  req
    "CreateTemplateAlias"
    "fixture/CreateTemplateAlias.yaml"

requestListTemplateAliases :: ListTemplateAliases -> TestTree
requestListTemplateAliases =
  req
    "ListTemplateAliases"
    "fixture/ListTemplateAliases.yaml"

requestGetDashboardEmbedUrl :: GetDashboardEmbedUrl -> TestTree
requestGetDashboardEmbedUrl =
  req
    "GetDashboardEmbedUrl"
    "fixture/GetDashboardEmbedUrl.yaml"

requestGenerateEmbedUrlForAnonymousUser :: GenerateEmbedUrlForAnonymousUser -> TestTree
requestGenerateEmbedUrlForAnonymousUser =
  req
    "GenerateEmbedUrlForAnonymousUser"
    "fixture/GenerateEmbedUrlForAnonymousUser.yaml"

requestListThemeVersions :: ListThemeVersions -> TestTree
requestListThemeVersions =
  req
    "ListThemeVersions"
    "fixture/ListThemeVersions.yaml"

requestUpdateDashboardPermissions :: UpdateDashboardPermissions -> TestTree
requestUpdateDashboardPermissions =
  req
    "UpdateDashboardPermissions"
    "fixture/UpdateDashboardPermissions.yaml"

requestListIAMPolicyAssignmentsForUser :: ListIAMPolicyAssignmentsForUser -> TestTree
requestListIAMPolicyAssignmentsForUser =
  req
    "ListIAMPolicyAssignmentsForUser"
    "fixture/ListIAMPolicyAssignmentsForUser.yaml"

requestDeleteAccountCustomization :: DeleteAccountCustomization -> TestTree
requestDeleteAccountCustomization =
  req
    "DeleteAccountCustomization"
    "fixture/DeleteAccountCustomization.yaml"

requestUpdateAccountCustomization :: UpdateAccountCustomization -> TestTree
requestUpdateAccountCustomization =
  req
    "UpdateAccountCustomization"
    "fixture/UpdateAccountCustomization.yaml"

requestDeleteDashboard :: DeleteDashboard -> TestTree
requestDeleteDashboard =
  req
    "DeleteDashboard"
    "fixture/DeleteDashboard.yaml"

requestUpdateDashboard :: UpdateDashboard -> TestTree
requestUpdateDashboard =
  req
    "UpdateDashboard"
    "fixture/UpdateDashboard.yaml"

requestDescribeDataSourcePermissions :: DescribeDataSourcePermissions -> TestTree
requestDescribeDataSourcePermissions =
  req
    "DescribeDataSourcePermissions"
    "fixture/DescribeDataSourcePermissions.yaml"

requestUpdateUser :: UpdateUser -> TestTree
requestUpdateUser =
  req
    "UpdateUser"
    "fixture/UpdateUser.yaml"

requestDeleteUser :: DeleteUser -> TestTree
requestDeleteUser =
  req
    "DeleteUser"
    "fixture/DeleteUser.yaml"

requestSearchAnalyses :: SearchAnalyses -> TestTree
requestSearchAnalyses =
  req
    "SearchAnalyses"
    "fixture/SearchAnalyses.yaml"

requestCreateAccountCustomization :: CreateAccountCustomization -> TestTree
requestCreateAccountCustomization =
  req
    "CreateAccountCustomization"
    "fixture/CreateAccountCustomization.yaml"

requestDescribeIpRestriction :: DescribeIpRestriction -> TestTree
requestDescribeIpRestriction =
  req
    "DescribeIpRestriction"
    "fixture/DescribeIpRestriction.yaml"

requestListThemes :: ListThemes -> TestTree
requestListThemes =
  req
    "ListThemes"
    "fixture/ListThemes.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUpdateTheme :: UpdateTheme -> TestTree
requestUpdateTheme =
  req
    "UpdateTheme"
    "fixture/UpdateTheme.yaml"

requestDeleteTheme :: DeleteTheme -> TestTree
requestDeleteTheme =
  req
    "DeleteTheme"
    "fixture/DeleteTheme.yaml"

requestListIngestions :: ListIngestions -> TestTree
requestListIngestions =
  req
    "ListIngestions"
    "fixture/ListIngestions.yaml"

requestCreateGroup :: CreateGroup -> TestTree
requestCreateGroup =
  req
    "CreateGroup"
    "fixture/CreateGroup.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestListTemplateVersions :: ListTemplateVersions -> TestTree
requestListTemplateVersions =
  req
    "ListTemplateVersions"
    "fixture/ListTemplateVersions.yaml"

requestListDataSets :: ListDataSets -> TestTree
requestListDataSets =
  req
    "ListDataSets"
    "fixture/ListDataSets.yaml"

requestDeleteThemeAlias :: DeleteThemeAlias -> TestTree
requestDeleteThemeAlias =
  req
    "DeleteThemeAlias"
    "fixture/DeleteThemeAlias.yaml"

requestUpdateThemeAlias :: UpdateThemeAlias -> TestTree
requestUpdateThemeAlias =
  req
    "UpdateThemeAlias"
    "fixture/UpdateThemeAlias.yaml"

requestDeleteGroup :: DeleteGroup -> TestTree
requestDeleteGroup =
  req
    "DeleteGroup"
    "fixture/DeleteGroup.yaml"

requestUpdateGroup :: UpdateGroup -> TestTree
requestUpdateGroup =
  req
    "UpdateGroup"
    "fixture/UpdateGroup.yaml"

requestDescribeNamespace :: DescribeNamespace -> TestTree
requestDescribeNamespace =
  req
    "DescribeNamespace"
    "fixture/DescribeNamespace.yaml"

requestListGroups :: ListGroups -> TestTree
requestListGroups =
  req
    "ListGroups"
    "fixture/ListGroups.yaml"

requestCreateAnalysis :: CreateAnalysis -> TestTree
requestCreateAnalysis =
  req
    "CreateAnalysis"
    "fixture/CreateAnalysis.yaml"

requestDescribeAccountCustomization :: DescribeAccountCustomization -> TestTree
requestDescribeAccountCustomization =
  req
    "DescribeAccountCustomization"
    "fixture/DescribeAccountCustomization.yaml"

requestGenerateEmbedUrlForRegisteredUser :: GenerateEmbedUrlForRegisteredUser -> TestTree
requestGenerateEmbedUrlForRegisteredUser =
  req
    "GenerateEmbedUrlForRegisteredUser"
    "fixture/GenerateEmbedUrlForRegisteredUser.yaml"

requestDescribeDashboard :: DescribeDashboard -> TestTree
requestDescribeDashboard =
  req
    "DescribeDashboard"
    "fixture/DescribeDashboard.yaml"

requestCreateGroupMembership :: CreateGroupMembership -> TestTree
requestCreateGroupMembership =
  req
    "CreateGroupMembership"
    "fixture/CreateGroupMembership.yaml"

requestDescribeDashboardPermissions :: DescribeDashboardPermissions -> TestTree
requestDescribeDashboardPermissions =
  req
    "DescribeDashboardPermissions"
    "fixture/DescribeDashboardPermissions.yaml"

-- Responses

responseCancelIngestion :: CancelIngestionResponse -> TestTree
responseCancelIngestion =
  res
    "CancelIngestionResponse"
    "fixture/CancelIngestionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelIngestion)

responseUpdateDataSource :: UpdateDataSourceResponse -> TestTree
responseUpdateDataSource =
  res
    "UpdateDataSourceResponse"
    "fixture/UpdateDataSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDataSource)

responseDeleteUserByPrincipalId :: DeleteUserByPrincipalIdResponse -> TestTree
responseDeleteUserByPrincipalId =
  res
    "DeleteUserByPrincipalIdResponse"
    "fixture/DeleteUserByPrincipalIdResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUserByPrincipalId)

responseDeleteDataSource :: DeleteDataSourceResponse -> TestTree
responseDeleteDataSource =
  res
    "DeleteDataSourceResponse"
    "fixture/DeleteDataSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDataSource)

responseCreateTemplate :: CreateTemplateResponse -> TestTree
responseCreateTemplate =
  res
    "CreateTemplateResponse"
    "fixture/CreateTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTemplate)

responseDeleteGroupMembership :: DeleteGroupMembershipResponse -> TestTree
responseDeleteGroupMembership =
  res
    "DeleteGroupMembershipResponse"
    "fixture/DeleteGroupMembershipResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteGroupMembership)

responseDescribeThemePermissions :: DescribeThemePermissionsResponse -> TestTree
responseDescribeThemePermissions =
  res
    "DescribeThemePermissionsResponse"
    "fixture/DescribeThemePermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeThemePermissions)

responseListGroupMemberships :: ListGroupMembershipsResponse -> TestTree
responseListGroupMemberships =
  res
    "ListGroupMembershipsResponse"
    "fixture/ListGroupMembershipsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListGroupMemberships)

responseListFolders :: ListFoldersResponse -> TestTree
responseListFolders =
  res
    "ListFoldersResponse"
    "fixture/ListFoldersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFolders)

responseDescribeDataSetPermissions :: DescribeDataSetPermissionsResponse -> TestTree
responseDescribeDataSetPermissions =
  res
    "DescribeDataSetPermissionsResponse"
    "fixture/DescribeDataSetPermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDataSetPermissions)

responseDeleteIAMPolicyAssignment :: DeleteIAMPolicyAssignmentResponse -> TestTree
responseDeleteIAMPolicyAssignment =
  res
    "DeleteIAMPolicyAssignmentResponse"
    "fixture/DeleteIAMPolicyAssignmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteIAMPolicyAssignment)

responseUpdateIAMPolicyAssignment :: UpdateIAMPolicyAssignmentResponse -> TestTree
responseUpdateIAMPolicyAssignment =
  res
    "UpdateIAMPolicyAssignmentResponse"
    "fixture/UpdateIAMPolicyAssignmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateIAMPolicyAssignment)

responseDescribeIngestion :: DescribeIngestionResponse -> TestTree
responseDescribeIngestion =
  res
    "DescribeIngestionResponse"
    "fixture/DescribeIngestionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeIngestion)

responseDeleteFolder :: DeleteFolderResponse -> TestTree
responseDeleteFolder =
  res
    "DeleteFolderResponse"
    "fixture/DeleteFolderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFolder)

responseUpdateFolder :: UpdateFolderResponse -> TestTree
responseUpdateFolder =
  res
    "UpdateFolderResponse"
    "fixture/UpdateFolderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFolder)

responseListUserGroups :: ListUserGroupsResponse -> TestTree
responseListUserGroups =
  res
    "ListUserGroupsResponse"
    "fixture/ListUserGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListUserGroups)

responseUpdateDashboardPublishedVersion :: UpdateDashboardPublishedVersionResponse -> TestTree
responseUpdateDashboardPublishedVersion =
  res
    "UpdateDashboardPublishedVersionResponse"
    "fixture/UpdateDashboardPublishedVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDashboardPublishedVersion)

responseDescribeAnalysisPermissions :: DescribeAnalysisPermissionsResponse -> TestTree
responseDescribeAnalysisPermissions =
  res
    "DescribeAnalysisPermissionsResponse"
    "fixture/DescribeAnalysisPermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAnalysisPermissions)

responseDeleteTemplateAlias :: DeleteTemplateAliasResponse -> TestTree
responseDeleteTemplateAlias =
  res
    "DeleteTemplateAliasResponse"
    "fixture/DeleteTemplateAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTemplateAlias)

responseUpdateTemplateAlias :: UpdateTemplateAliasResponse -> TestTree
responseUpdateTemplateAlias =
  res
    "UpdateTemplateAliasResponse"
    "fixture/UpdateTemplateAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTemplateAlias)

responseDescribeAnalysis :: DescribeAnalysisResponse -> TestTree
responseDescribeAnalysis =
  res
    "DescribeAnalysisResponse"
    "fixture/DescribeAnalysisResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAnalysis)

responseUpdateFolderPermissions :: UpdateFolderPermissionsResponse -> TestTree
responseUpdateFolderPermissions =
  res
    "UpdateFolderPermissionsResponse"
    "fixture/UpdateFolderPermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFolderPermissions)

responseDescribeDataSet :: DescribeDataSetResponse -> TestTree
responseDescribeDataSet =
  res
    "DescribeDataSetResponse"
    "fixture/DescribeDataSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDataSet)

responseListNamespaces :: ListNamespacesResponse -> TestTree
responseListNamespaces =
  res
    "ListNamespacesResponse"
    "fixture/ListNamespacesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListNamespaces)

responseDeleteNamespace :: DeleteNamespaceResponse -> TestTree
responseDeleteNamespace =
  res
    "DeleteNamespaceResponse"
    "fixture/DeleteNamespaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteNamespace)

responseCreateFolder :: CreateFolderResponse -> TestTree
responseCreateFolder =
  res
    "CreateFolderResponse"
    "fixture/CreateFolderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFolder)

responseDescribeGroup :: DescribeGroupResponse -> TestTree
responseDescribeGroup =
  res
    "DescribeGroupResponse"
    "fixture/DescribeGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeGroup)

responseDescribeThemeAlias :: DescribeThemeAliasResponse -> TestTree
responseDescribeThemeAlias =
  res
    "DescribeThemeAliasResponse"
    "fixture/DescribeThemeAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeThemeAlias)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseUpdateAccountSettings :: UpdateAccountSettingsResponse -> TestTree
responseUpdateAccountSettings =
  res
    "UpdateAccountSettingsResponse"
    "fixture/UpdateAccountSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAccountSettings)

responseDescribeTemplatePermissions :: DescribeTemplatePermissionsResponse -> TestTree
responseDescribeTemplatePermissions =
  res
    "DescribeTemplatePermissionsResponse"
    "fixture/DescribeTemplatePermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTemplatePermissions)

responseListDashboards :: ListDashboardsResponse -> TestTree
responseListDashboards =
  res
    "ListDashboardsResponse"
    "fixture/ListDashboardsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDashboards)

responseDescribeTemplate :: DescribeTemplateResponse -> TestTree
responseDescribeTemplate =
  res
    "DescribeTemplateResponse"
    "fixture/DescribeTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTemplate)

responseDeleteFolderMembership :: DeleteFolderMembershipResponse -> TestTree
responseDeleteFolderMembership =
  res
    "DeleteFolderMembershipResponse"
    "fixture/DeleteFolderMembershipResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFolderMembership)

responseCreateTheme :: CreateThemeResponse -> TestTree
responseCreateTheme =
  res
    "CreateThemeResponse"
    "fixture/CreateThemeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTheme)

responseListUsers :: ListUsersResponse -> TestTree
responseListUsers =
  res
    "ListUsersResponse"
    "fixture/ListUsersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListUsers)

responseCreateFolderMembership :: CreateFolderMembershipResponse -> TestTree
responseCreateFolderMembership =
  res
    "CreateFolderMembershipResponse"
    "fixture/CreateFolderMembershipResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFolderMembership)

responseUpdateThemePermissions :: UpdateThemePermissionsResponse -> TestTree
responseUpdateThemePermissions =
  res
    "UpdateThemePermissionsResponse"
    "fixture/UpdateThemePermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateThemePermissions)

responseGetSessionEmbedUrl :: GetSessionEmbedUrlResponse -> TestTree
responseGetSessionEmbedUrl =
  res
    "GetSessionEmbedUrlResponse"
    "fixture/GetSessionEmbedUrlResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSessionEmbedUrl)

responseCreateDashboard :: CreateDashboardResponse -> TestTree
responseCreateDashboard =
  res
    "CreateDashboardResponse"
    "fixture/CreateDashboardResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDashboard)

responseRegisterUser :: RegisterUserResponse -> TestTree
responseRegisterUser =
  res
    "RegisterUserResponse"
    "fixture/RegisterUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterUser)

responseDescribeDataSource :: DescribeDataSourceResponse -> TestTree
responseDescribeDataSource =
  res
    "DescribeDataSourceResponse"
    "fixture/DescribeDataSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDataSource)

responseDescribeFolderResolvedPermissions :: DescribeFolderResolvedPermissionsResponse -> TestTree
responseDescribeFolderResolvedPermissions =
  res
    "DescribeFolderResolvedPermissionsResponse"
    "fixture/DescribeFolderResolvedPermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFolderResolvedPermissions)

responseUpdateAnalysisPermissions :: UpdateAnalysisPermissionsResponse -> TestTree
responseUpdateAnalysisPermissions =
  res
    "UpdateAnalysisPermissionsResponse"
    "fixture/UpdateAnalysisPermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAnalysisPermissions)

responseDeleteDataSet :: DeleteDataSetResponse -> TestTree
responseDeleteDataSet =
  res
    "DeleteDataSetResponse"
    "fixture/DeleteDataSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDataSet)

responseUpdateDataSet :: UpdateDataSetResponse -> TestTree
responseUpdateDataSet =
  res
    "UpdateDataSetResponse"
    "fixture/UpdateDataSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDataSet)

responseListThemeAliases :: ListThemeAliasesResponse -> TestTree
responseListThemeAliases =
  res
    "ListThemeAliasesResponse"
    "fixture/ListThemeAliasesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListThemeAliases)

responseUpdateAnalysis :: UpdateAnalysisResponse -> TestTree
responseUpdateAnalysis =
  res
    "UpdateAnalysisResponse"
    "fixture/UpdateAnalysisResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAnalysis)

responseDeleteAnalysis :: DeleteAnalysisResponse -> TestTree
responseDeleteAnalysis =
  res
    "DeleteAnalysisResponse"
    "fixture/DeleteAnalysisResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAnalysis)

responseSearchFolders :: SearchFoldersResponse -> TestTree
responseSearchFolders =
  res
    "SearchFoldersResponse"
    "fixture/SearchFoldersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchFolders)

responseDescribeFolderPermissions :: DescribeFolderPermissionsResponse -> TestTree
responseDescribeFolderPermissions =
  res
    "DescribeFolderPermissionsResponse"
    "fixture/DescribeFolderPermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFolderPermissions)

responseUpdateDataSetPermissions :: UpdateDataSetPermissionsResponse -> TestTree
responseUpdateDataSetPermissions =
  res
    "UpdateDataSetPermissionsResponse"
    "fixture/UpdateDataSetPermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDataSetPermissions)

responseCreateThemeAlias :: CreateThemeAliasResponse -> TestTree
responseCreateThemeAlias =
  res
    "CreateThemeAliasResponse"
    "fixture/CreateThemeAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateThemeAlias)

responseDescribeFolder :: DescribeFolderResponse -> TestTree
responseDescribeFolder =
  res
    "DescribeFolderResponse"
    "fixture/DescribeFolderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFolder)

responseDescribeTemplateAlias :: DescribeTemplateAliasResponse -> TestTree
responseDescribeTemplateAlias =
  res
    "DescribeTemplateAliasResponse"
    "fixture/DescribeTemplateAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTemplateAlias)

responseDescribeIAMPolicyAssignment :: DescribeIAMPolicyAssignmentResponse -> TestTree
responseDescribeIAMPolicyAssignment =
  res
    "DescribeIAMPolicyAssignmentResponse"
    "fixture/DescribeIAMPolicyAssignmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeIAMPolicyAssignment)

responseCreateIngestion :: CreateIngestionResponse -> TestTree
responseCreateIngestion =
  res
    "CreateIngestionResponse"
    "fixture/CreateIngestionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateIngestion)

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

responseListTemplates :: ListTemplatesResponse -> TestTree
responseListTemplates =
  res
    "ListTemplatesResponse"
    "fixture/ListTemplatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTemplates)

responseDescribeAccountSettings :: DescribeAccountSettingsResponse -> TestTree
responseDescribeAccountSettings =
  res
    "DescribeAccountSettingsResponse"
    "fixture/DescribeAccountSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAccountSettings)

responseUpdateTemplate :: UpdateTemplateResponse -> TestTree
responseUpdateTemplate =
  res
    "UpdateTemplateResponse"
    "fixture/UpdateTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTemplate)

responseDeleteTemplate :: DeleteTemplateResponse -> TestTree
responseDeleteTemplate =
  res
    "DeleteTemplateResponse"
    "fixture/DeleteTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTemplate)

responseUpdateTemplatePermissions :: UpdateTemplatePermissionsResponse -> TestTree
responseUpdateTemplatePermissions =
  res
    "UpdateTemplatePermissionsResponse"
    "fixture/UpdateTemplatePermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTemplatePermissions)

responseSearchDashboards :: SearchDashboardsResponse -> TestTree
responseSearchDashboards =
  res
    "SearchDashboardsResponse"
    "fixture/SearchDashboardsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchDashboards)

responseUpdateDataSourcePermissions :: UpdateDataSourcePermissionsResponse -> TestTree
responseUpdateDataSourcePermissions =
  res
    "UpdateDataSourcePermissionsResponse"
    "fixture/UpdateDataSourcePermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDataSourcePermissions)

responseUpdateIpRestriction :: UpdateIpRestrictionResponse -> TestTree
responseUpdateIpRestriction =
  res
    "UpdateIpRestrictionResponse"
    "fixture/UpdateIpRestrictionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateIpRestriction)

responseDescribeUser :: DescribeUserResponse -> TestTree
responseDescribeUser =
  res
    "DescribeUserResponse"
    "fixture/DescribeUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeUser)

responseDescribeTheme :: DescribeThemeResponse -> TestTree
responseDescribeTheme =
  res
    "DescribeThemeResponse"
    "fixture/DescribeThemeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTheme)

responseListAnalyses :: ListAnalysesResponse -> TestTree
responseListAnalyses =
  res
    "ListAnalysesResponse"
    "fixture/ListAnalysesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAnalyses)

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

responseListIAMPolicyAssignments :: ListIAMPolicyAssignmentsResponse -> TestTree
responseListIAMPolicyAssignments =
  res
    "ListIAMPolicyAssignmentsResponse"
    "fixture/ListIAMPolicyAssignmentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListIAMPolicyAssignments)

responseListDashboardVersions :: ListDashboardVersionsResponse -> TestTree
responseListDashboardVersions =
  res
    "ListDashboardVersionsResponse"
    "fixture/ListDashboardVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDashboardVersions)

responseCreateNamespace :: CreateNamespaceResponse -> TestTree
responseCreateNamespace =
  res
    "CreateNamespaceResponse"
    "fixture/CreateNamespaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateNamespace)

responseCreateIAMPolicyAssignment :: CreateIAMPolicyAssignmentResponse -> TestTree
responseCreateIAMPolicyAssignment =
  res
    "CreateIAMPolicyAssignmentResponse"
    "fixture/CreateIAMPolicyAssignmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateIAMPolicyAssignment)

responseRestoreAnalysis :: RestoreAnalysisResponse -> TestTree
responseRestoreAnalysis =
  res
    "RestoreAnalysisResponse"
    "fixture/RestoreAnalysisResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RestoreAnalysis)

responseCreateTemplateAlias :: CreateTemplateAliasResponse -> TestTree
responseCreateTemplateAlias =
  res
    "CreateTemplateAliasResponse"
    "fixture/CreateTemplateAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTemplateAlias)

responseListTemplateAliases :: ListTemplateAliasesResponse -> TestTree
responseListTemplateAliases =
  res
    "ListTemplateAliasesResponse"
    "fixture/ListTemplateAliasesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTemplateAliases)

responseGetDashboardEmbedUrl :: GetDashboardEmbedUrlResponse -> TestTree
responseGetDashboardEmbedUrl =
  res
    "GetDashboardEmbedUrlResponse"
    "fixture/GetDashboardEmbedUrlResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDashboardEmbedUrl)

responseGenerateEmbedUrlForAnonymousUser :: GenerateEmbedUrlForAnonymousUserResponse -> TestTree
responseGenerateEmbedUrlForAnonymousUser =
  res
    "GenerateEmbedUrlForAnonymousUserResponse"
    "fixture/GenerateEmbedUrlForAnonymousUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GenerateEmbedUrlForAnonymousUser)

responseListThemeVersions :: ListThemeVersionsResponse -> TestTree
responseListThemeVersions =
  res
    "ListThemeVersionsResponse"
    "fixture/ListThemeVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListThemeVersions)

responseUpdateDashboardPermissions :: UpdateDashboardPermissionsResponse -> TestTree
responseUpdateDashboardPermissions =
  res
    "UpdateDashboardPermissionsResponse"
    "fixture/UpdateDashboardPermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDashboardPermissions)

responseListIAMPolicyAssignmentsForUser :: ListIAMPolicyAssignmentsForUserResponse -> TestTree
responseListIAMPolicyAssignmentsForUser =
  res
    "ListIAMPolicyAssignmentsForUserResponse"
    "fixture/ListIAMPolicyAssignmentsForUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListIAMPolicyAssignmentsForUser)

responseDeleteAccountCustomization :: DeleteAccountCustomizationResponse -> TestTree
responseDeleteAccountCustomization =
  res
    "DeleteAccountCustomizationResponse"
    "fixture/DeleteAccountCustomizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAccountCustomization)

responseUpdateAccountCustomization :: UpdateAccountCustomizationResponse -> TestTree
responseUpdateAccountCustomization =
  res
    "UpdateAccountCustomizationResponse"
    "fixture/UpdateAccountCustomizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAccountCustomization)

responseDeleteDashboard :: DeleteDashboardResponse -> TestTree
responseDeleteDashboard =
  res
    "DeleteDashboardResponse"
    "fixture/DeleteDashboardResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDashboard)

responseUpdateDashboard :: UpdateDashboardResponse -> TestTree
responseUpdateDashboard =
  res
    "UpdateDashboardResponse"
    "fixture/UpdateDashboardResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDashboard)

responseDescribeDataSourcePermissions :: DescribeDataSourcePermissionsResponse -> TestTree
responseDescribeDataSourcePermissions =
  res
    "DescribeDataSourcePermissionsResponse"
    "fixture/DescribeDataSourcePermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDataSourcePermissions)

responseUpdateUser :: UpdateUserResponse -> TestTree
responseUpdateUser =
  res
    "UpdateUserResponse"
    "fixture/UpdateUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateUser)

responseDeleteUser :: DeleteUserResponse -> TestTree
responseDeleteUser =
  res
    "DeleteUserResponse"
    "fixture/DeleteUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUser)

responseSearchAnalyses :: SearchAnalysesResponse -> TestTree
responseSearchAnalyses =
  res
    "SearchAnalysesResponse"
    "fixture/SearchAnalysesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchAnalyses)

responseCreateAccountCustomization :: CreateAccountCustomizationResponse -> TestTree
responseCreateAccountCustomization =
  res
    "CreateAccountCustomizationResponse"
    "fixture/CreateAccountCustomizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAccountCustomization)

responseDescribeIpRestriction :: DescribeIpRestrictionResponse -> TestTree
responseDescribeIpRestriction =
  res
    "DescribeIpRestrictionResponse"
    "fixture/DescribeIpRestrictionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeIpRestriction)

responseListThemes :: ListThemesResponse -> TestTree
responseListThemes =
  res
    "ListThemesResponse"
    "fixture/ListThemesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListThemes)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseUpdateTheme :: UpdateThemeResponse -> TestTree
responseUpdateTheme =
  res
    "UpdateThemeResponse"
    "fixture/UpdateThemeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTheme)

responseDeleteTheme :: DeleteThemeResponse -> TestTree
responseDeleteTheme =
  res
    "DeleteThemeResponse"
    "fixture/DeleteThemeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTheme)

responseListIngestions :: ListIngestionsResponse -> TestTree
responseListIngestions =
  res
    "ListIngestionsResponse"
    "fixture/ListIngestionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListIngestions)

responseCreateGroup :: CreateGroupResponse -> TestTree
responseCreateGroup =
  res
    "CreateGroupResponse"
    "fixture/CreateGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateGroup)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseListTemplateVersions :: ListTemplateVersionsResponse -> TestTree
responseListTemplateVersions =
  res
    "ListTemplateVersionsResponse"
    "fixture/ListTemplateVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTemplateVersions)

responseListDataSets :: ListDataSetsResponse -> TestTree
responseListDataSets =
  res
    "ListDataSetsResponse"
    "fixture/ListDataSetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDataSets)

responseDeleteThemeAlias :: DeleteThemeAliasResponse -> TestTree
responseDeleteThemeAlias =
  res
    "DeleteThemeAliasResponse"
    "fixture/DeleteThemeAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteThemeAlias)

responseUpdateThemeAlias :: UpdateThemeAliasResponse -> TestTree
responseUpdateThemeAlias =
  res
    "UpdateThemeAliasResponse"
    "fixture/UpdateThemeAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateThemeAlias)

responseDeleteGroup :: DeleteGroupResponse -> TestTree
responseDeleteGroup =
  res
    "DeleteGroupResponse"
    "fixture/DeleteGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteGroup)

responseUpdateGroup :: UpdateGroupResponse -> TestTree
responseUpdateGroup =
  res
    "UpdateGroupResponse"
    "fixture/UpdateGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateGroup)

responseDescribeNamespace :: DescribeNamespaceResponse -> TestTree
responseDescribeNamespace =
  res
    "DescribeNamespaceResponse"
    "fixture/DescribeNamespaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeNamespace)

responseListGroups :: ListGroupsResponse -> TestTree
responseListGroups =
  res
    "ListGroupsResponse"
    "fixture/ListGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListGroups)

responseCreateAnalysis :: CreateAnalysisResponse -> TestTree
responseCreateAnalysis =
  res
    "CreateAnalysisResponse"
    "fixture/CreateAnalysisResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAnalysis)

responseDescribeAccountCustomization :: DescribeAccountCustomizationResponse -> TestTree
responseDescribeAccountCustomization =
  res
    "DescribeAccountCustomizationResponse"
    "fixture/DescribeAccountCustomizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAccountCustomization)

responseGenerateEmbedUrlForRegisteredUser :: GenerateEmbedUrlForRegisteredUserResponse -> TestTree
responseGenerateEmbedUrlForRegisteredUser =
  res
    "GenerateEmbedUrlForRegisteredUserResponse"
    "fixture/GenerateEmbedUrlForRegisteredUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GenerateEmbedUrlForRegisteredUser)

responseDescribeDashboard :: DescribeDashboardResponse -> TestTree
responseDescribeDashboard =
  res
    "DescribeDashboardResponse"
    "fixture/DescribeDashboardResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDashboard)

responseCreateGroupMembership :: CreateGroupMembershipResponse -> TestTree
responseCreateGroupMembership =
  res
    "CreateGroupMembershipResponse"
    "fixture/CreateGroupMembershipResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateGroupMembership)

responseDescribeDashboardPermissions :: DescribeDashboardPermissionsResponse -> TestTree
responseDescribeDashboardPermissions =
  res
    "DescribeDashboardPermissionsResponse"
    "fixture/DescribeDashboardPermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDashboardPermissions)
