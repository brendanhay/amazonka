{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.QuickSight.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Lens
  ( -- * Operations

    -- ** CancelIngestion
    cancelIngestion_awsAccountId,
    cancelIngestion_dataSetId,
    cancelIngestion_ingestionId,
    cancelIngestionResponse_requestId,
    cancelIngestionResponse_arn,
    cancelIngestionResponse_ingestionId,
    cancelIngestionResponse_status,

    -- ** CreateAccountCustomization
    createAccountCustomization_tags,
    createAccountCustomization_namespace,
    createAccountCustomization_awsAccountId,
    createAccountCustomization_accountCustomization,
    createAccountCustomizationResponse_awsAccountId,
    createAccountCustomizationResponse_requestId,
    createAccountCustomizationResponse_arn,
    createAccountCustomizationResponse_namespace,
    createAccountCustomizationResponse_accountCustomization,
    createAccountCustomizationResponse_status,

    -- ** CreateAccountSubscription
    createAccountSubscription_directoryId,
    createAccountSubscription_activeDirectoryName,
    createAccountSubscription_readerGroup,
    createAccountSubscription_firstName,
    createAccountSubscription_adminGroup,
    createAccountSubscription_contactNumber,
    createAccountSubscription_lastName,
    createAccountSubscription_authorGroup,
    createAccountSubscription_realm,
    createAccountSubscription_emailAddress,
    createAccountSubscription_edition,
    createAccountSubscription_authenticationMethod,
    createAccountSubscription_awsAccountId,
    createAccountSubscription_accountName,
    createAccountSubscription_notificationEmail,
    createAccountSubscriptionResponse_requestId,
    createAccountSubscriptionResponse_signupResponse,
    createAccountSubscriptionResponse_status,

    -- ** CreateAnalysis
    createAnalysis_tags,
    createAnalysis_themeArn,
    createAnalysis_permissions,
    createAnalysis_parameters,
    createAnalysis_awsAccountId,
    createAnalysis_analysisId,
    createAnalysis_name,
    createAnalysis_sourceEntity,
    createAnalysisResponse_analysisId,
    createAnalysisResponse_creationStatus,
    createAnalysisResponse_requestId,
    createAnalysisResponse_arn,
    createAnalysisResponse_status,

    -- ** CreateDashboard
    createDashboard_tags,
    createDashboard_themeArn,
    createDashboard_permissions,
    createDashboard_versionDescription,
    createDashboard_dashboardPublishOptions,
    createDashboard_parameters,
    createDashboard_awsAccountId,
    createDashboard_dashboardId,
    createDashboard_name,
    createDashboard_sourceEntity,
    createDashboardResponse_creationStatus,
    createDashboardResponse_requestId,
    createDashboardResponse_arn,
    createDashboardResponse_dashboardId,
    createDashboardResponse_versionArn,
    createDashboardResponse_status,

    -- ** CreateDataSet
    createDataSet_tags,
    createDataSet_columnLevelPermissionRules,
    createDataSet_dataSetUsageConfiguration,
    createDataSet_permissions,
    createDataSet_rowLevelPermissionTagConfiguration,
    createDataSet_columnGroups,
    createDataSet_fieldFolders,
    createDataSet_rowLevelPermissionDataSet,
    createDataSet_logicalTableMap,
    createDataSet_awsAccountId,
    createDataSet_dataSetId,
    createDataSet_name,
    createDataSet_physicalTableMap,
    createDataSet_importMode,
    createDataSetResponse_ingestionArn,
    createDataSetResponse_requestId,
    createDataSetResponse_arn,
    createDataSetResponse_dataSetId,
    createDataSetResponse_ingestionId,
    createDataSetResponse_status,

    -- ** CreateDataSource
    createDataSource_tags,
    createDataSource_dataSourceParameters,
    createDataSource_permissions,
    createDataSource_vpcConnectionProperties,
    createDataSource_sslProperties,
    createDataSource_credentials,
    createDataSource_awsAccountId,
    createDataSource_dataSourceId,
    createDataSource_name,
    createDataSource_type,
    createDataSourceResponse_creationStatus,
    createDataSourceResponse_dataSourceId,
    createDataSourceResponse_requestId,
    createDataSourceResponse_arn,
    createDataSourceResponse_status,

    -- ** CreateFolder
    createFolder_tags,
    createFolder_name,
    createFolder_permissions,
    createFolder_folderType,
    createFolder_parentFolderArn,
    createFolder_awsAccountId,
    createFolder_folderId,
    createFolderResponse_requestId,
    createFolderResponse_arn,
    createFolderResponse_folderId,
    createFolderResponse_status,

    -- ** CreateFolderMembership
    createFolderMembership_awsAccountId,
    createFolderMembership_folderId,
    createFolderMembership_memberId,
    createFolderMembership_memberType,
    createFolderMembershipResponse_requestId,
    createFolderMembershipResponse_status,
    createFolderMembershipResponse_folderMember,
    createFolderMembershipResponse_httpStatus,

    -- ** CreateGroup
    createGroup_description,
    createGroup_groupName,
    createGroup_awsAccountId,
    createGroup_namespace,
    createGroupResponse_requestId,
    createGroupResponse_group,
    createGroupResponse_status,

    -- ** CreateGroupMembership
    createGroupMembership_memberName,
    createGroupMembership_groupName,
    createGroupMembership_awsAccountId,
    createGroupMembership_namespace,
    createGroupMembershipResponse_requestId,
    createGroupMembershipResponse_groupMember,
    createGroupMembershipResponse_status,

    -- ** CreateIAMPolicyAssignment
    createIAMPolicyAssignment_identities,
    createIAMPolicyAssignment_policyArn,
    createIAMPolicyAssignment_awsAccountId,
    createIAMPolicyAssignment_assignmentName,
    createIAMPolicyAssignment_assignmentStatus,
    createIAMPolicyAssignment_namespace,
    createIAMPolicyAssignmentResponse_requestId,
    createIAMPolicyAssignmentResponse_identities,
    createIAMPolicyAssignmentResponse_policyArn,
    createIAMPolicyAssignmentResponse_assignmentName,
    createIAMPolicyAssignmentResponse_assignmentId,
    createIAMPolicyAssignmentResponse_assignmentStatus,
    createIAMPolicyAssignmentResponse_status,

    -- ** CreateIngestion
    createIngestion_ingestionType,
    createIngestion_dataSetId,
    createIngestion_ingestionId,
    createIngestion_awsAccountId,
    createIngestionResponse_ingestionStatus,
    createIngestionResponse_requestId,
    createIngestionResponse_arn,
    createIngestionResponse_ingestionId,
    createIngestionResponse_status,

    -- ** CreateNamespace
    createNamespace_tags,
    createNamespace_awsAccountId,
    createNamespace_namespace,
    createNamespace_identityStore,
    createNamespaceResponse_creationStatus,
    createNamespaceResponse_name,
    createNamespaceResponse_requestId,
    createNamespaceResponse_arn,
    createNamespaceResponse_capacityRegion,
    createNamespaceResponse_identityStore,
    createNamespaceResponse_status,

    -- ** CreateTemplate
    createTemplate_tags,
    createTemplate_name,
    createTemplate_permissions,
    createTemplate_versionDescription,
    createTemplate_awsAccountId,
    createTemplate_templateId,
    createTemplate_sourceEntity,
    createTemplateResponse_creationStatus,
    createTemplateResponse_requestId,
    createTemplateResponse_arn,
    createTemplateResponse_templateId,
    createTemplateResponse_versionArn,
    createTemplateResponse_status,

    -- ** CreateTemplateAlias
    createTemplateAlias_awsAccountId,
    createTemplateAlias_templateId,
    createTemplateAlias_aliasName,
    createTemplateAlias_templateVersionNumber,
    createTemplateAliasResponse_requestId,
    createTemplateAliasResponse_templateAlias,
    createTemplateAliasResponse_status,

    -- ** CreateTheme
    createTheme_tags,
    createTheme_permissions,
    createTheme_versionDescription,
    createTheme_awsAccountId,
    createTheme_themeId,
    createTheme_name,
    createTheme_baseThemeId,
    createTheme_configuration,
    createThemeResponse_creationStatus,
    createThemeResponse_requestId,
    createThemeResponse_arn,
    createThemeResponse_versionArn,
    createThemeResponse_themeId,
    createThemeResponse_status,

    -- ** CreateThemeAlias
    createThemeAlias_awsAccountId,
    createThemeAlias_themeId,
    createThemeAlias_aliasName,
    createThemeAlias_themeVersionNumber,
    createThemeAliasResponse_requestId,
    createThemeAliasResponse_themeAlias,
    createThemeAliasResponse_status,

    -- ** DeleteAccountCustomization
    deleteAccountCustomization_namespace,
    deleteAccountCustomization_awsAccountId,
    deleteAccountCustomizationResponse_requestId,
    deleteAccountCustomizationResponse_status,

    -- ** DeleteAccountSubscription
    deleteAccountSubscription_awsAccountId,
    deleteAccountSubscriptionResponse_requestId,
    deleteAccountSubscriptionResponse_status,

    -- ** DeleteAnalysis
    deleteAnalysis_recoveryWindowInDays,
    deleteAnalysis_forceDeleteWithoutRecovery,
    deleteAnalysis_awsAccountId,
    deleteAnalysis_analysisId,
    deleteAnalysisResponse_deletionTime,
    deleteAnalysisResponse_analysisId,
    deleteAnalysisResponse_requestId,
    deleteAnalysisResponse_arn,
    deleteAnalysisResponse_status,

    -- ** DeleteDashboard
    deleteDashboard_versionNumber,
    deleteDashboard_awsAccountId,
    deleteDashboard_dashboardId,
    deleteDashboardResponse_requestId,
    deleteDashboardResponse_arn,
    deleteDashboardResponse_dashboardId,
    deleteDashboardResponse_status,

    -- ** DeleteDataSet
    deleteDataSet_awsAccountId,
    deleteDataSet_dataSetId,
    deleteDataSetResponse_requestId,
    deleteDataSetResponse_arn,
    deleteDataSetResponse_dataSetId,
    deleteDataSetResponse_status,

    -- ** DeleteDataSource
    deleteDataSource_awsAccountId,
    deleteDataSource_dataSourceId,
    deleteDataSourceResponse_dataSourceId,
    deleteDataSourceResponse_requestId,
    deleteDataSourceResponse_arn,
    deleteDataSourceResponse_status,

    -- ** DeleteFolder
    deleteFolder_awsAccountId,
    deleteFolder_folderId,
    deleteFolderResponse_requestId,
    deleteFolderResponse_arn,
    deleteFolderResponse_folderId,
    deleteFolderResponse_status,

    -- ** DeleteFolderMembership
    deleteFolderMembership_awsAccountId,
    deleteFolderMembership_folderId,
    deleteFolderMembership_memberId,
    deleteFolderMembership_memberType,
    deleteFolderMembershipResponse_requestId,
    deleteFolderMembershipResponse_status,
    deleteFolderMembershipResponse_httpStatus,

    -- ** DeleteGroup
    deleteGroup_groupName,
    deleteGroup_awsAccountId,
    deleteGroup_namespace,
    deleteGroupResponse_requestId,
    deleteGroupResponse_status,

    -- ** DeleteGroupMembership
    deleteGroupMembership_memberName,
    deleteGroupMembership_groupName,
    deleteGroupMembership_awsAccountId,
    deleteGroupMembership_namespace,
    deleteGroupMembershipResponse_requestId,
    deleteGroupMembershipResponse_status,

    -- ** DeleteIAMPolicyAssignment
    deleteIAMPolicyAssignment_awsAccountId,
    deleteIAMPolicyAssignment_assignmentName,
    deleteIAMPolicyAssignment_namespace,
    deleteIAMPolicyAssignmentResponse_requestId,
    deleteIAMPolicyAssignmentResponse_assignmentName,
    deleteIAMPolicyAssignmentResponse_status,

    -- ** DeleteNamespace
    deleteNamespace_awsAccountId,
    deleteNamespace_namespace,
    deleteNamespaceResponse_requestId,
    deleteNamespaceResponse_status,

    -- ** DeleteTemplate
    deleteTemplate_versionNumber,
    deleteTemplate_awsAccountId,
    deleteTemplate_templateId,
    deleteTemplateResponse_requestId,
    deleteTemplateResponse_arn,
    deleteTemplateResponse_templateId,
    deleteTemplateResponse_status,

    -- ** DeleteTemplateAlias
    deleteTemplateAlias_awsAccountId,
    deleteTemplateAlias_templateId,
    deleteTemplateAlias_aliasName,
    deleteTemplateAliasResponse_requestId,
    deleteTemplateAliasResponse_arn,
    deleteTemplateAliasResponse_templateId,
    deleteTemplateAliasResponse_aliasName,
    deleteTemplateAliasResponse_status,

    -- ** DeleteTheme
    deleteTheme_versionNumber,
    deleteTheme_awsAccountId,
    deleteTheme_themeId,
    deleteThemeResponse_requestId,
    deleteThemeResponse_arn,
    deleteThemeResponse_themeId,
    deleteThemeResponse_status,

    -- ** DeleteThemeAlias
    deleteThemeAlias_awsAccountId,
    deleteThemeAlias_themeId,
    deleteThemeAlias_aliasName,
    deleteThemeAliasResponse_requestId,
    deleteThemeAliasResponse_arn,
    deleteThemeAliasResponse_aliasName,
    deleteThemeAliasResponse_themeId,
    deleteThemeAliasResponse_status,

    -- ** DeleteUser
    deleteUser_userName,
    deleteUser_awsAccountId,
    deleteUser_namespace,
    deleteUserResponse_requestId,
    deleteUserResponse_status,

    -- ** DeleteUserByPrincipalId
    deleteUserByPrincipalId_principalId,
    deleteUserByPrincipalId_awsAccountId,
    deleteUserByPrincipalId_namespace,
    deleteUserByPrincipalIdResponse_requestId,
    deleteUserByPrincipalIdResponse_status,

    -- ** DescribeAccountCustomization
    describeAccountCustomization_resolved,
    describeAccountCustomization_namespace,
    describeAccountCustomization_awsAccountId,
    describeAccountCustomizationResponse_awsAccountId,
    describeAccountCustomizationResponse_requestId,
    describeAccountCustomizationResponse_arn,
    describeAccountCustomizationResponse_namespace,
    describeAccountCustomizationResponse_accountCustomization,
    describeAccountCustomizationResponse_status,

    -- ** DescribeAccountSettings
    describeAccountSettings_awsAccountId,
    describeAccountSettingsResponse_requestId,
    describeAccountSettingsResponse_accountSettings,
    describeAccountSettingsResponse_status,

    -- ** DescribeAccountSubscription
    describeAccountSubscription_awsAccountId,
    describeAccountSubscriptionResponse_accountInfo,
    describeAccountSubscriptionResponse_requestId,
    describeAccountSubscriptionResponse_status,

    -- ** DescribeAnalysis
    describeAnalysis_awsAccountId,
    describeAnalysis_analysisId,
    describeAnalysisResponse_requestId,
    describeAnalysisResponse_analysis,
    describeAnalysisResponse_status,

    -- ** DescribeAnalysisPermissions
    describeAnalysisPermissions_awsAccountId,
    describeAnalysisPermissions_analysisId,
    describeAnalysisPermissionsResponse_analysisId,
    describeAnalysisPermissionsResponse_requestId,
    describeAnalysisPermissionsResponse_permissions,
    describeAnalysisPermissionsResponse_analysisArn,
    describeAnalysisPermissionsResponse_status,

    -- ** DescribeDashboard
    describeDashboard_versionNumber,
    describeDashboard_aliasName,
    describeDashboard_awsAccountId,
    describeDashboard_dashboardId,
    describeDashboardResponse_dashboard,
    describeDashboardResponse_requestId,
    describeDashboardResponse_status,

    -- ** DescribeDashboardPermissions
    describeDashboardPermissions_awsAccountId,
    describeDashboardPermissions_dashboardId,
    describeDashboardPermissionsResponse_linkSharingConfiguration,
    describeDashboardPermissionsResponse_requestId,
    describeDashboardPermissionsResponse_permissions,
    describeDashboardPermissionsResponse_dashboardId,
    describeDashboardPermissionsResponse_dashboardArn,
    describeDashboardPermissionsResponse_status,

    -- ** DescribeDataSet
    describeDataSet_awsAccountId,
    describeDataSet_dataSetId,
    describeDataSetResponse_requestId,
    describeDataSetResponse_dataSet,
    describeDataSetResponse_status,

    -- ** DescribeDataSetPermissions
    describeDataSetPermissions_awsAccountId,
    describeDataSetPermissions_dataSetId,
    describeDataSetPermissionsResponse_requestId,
    describeDataSetPermissionsResponse_permissions,
    describeDataSetPermissionsResponse_dataSetArn,
    describeDataSetPermissionsResponse_dataSetId,
    describeDataSetPermissionsResponse_status,

    -- ** DescribeDataSource
    describeDataSource_awsAccountId,
    describeDataSource_dataSourceId,
    describeDataSourceResponse_requestId,
    describeDataSourceResponse_dataSource,
    describeDataSourceResponse_status,

    -- ** DescribeDataSourcePermissions
    describeDataSourcePermissions_awsAccountId,
    describeDataSourcePermissions_dataSourceId,
    describeDataSourcePermissionsResponse_dataSourceArn,
    describeDataSourcePermissionsResponse_dataSourceId,
    describeDataSourcePermissionsResponse_requestId,
    describeDataSourcePermissionsResponse_permissions,
    describeDataSourcePermissionsResponse_status,

    -- ** DescribeFolder
    describeFolder_awsAccountId,
    describeFolder_folderId,
    describeFolderResponse_folder,
    describeFolderResponse_requestId,
    describeFolderResponse_status,

    -- ** DescribeFolderPermissions
    describeFolderPermissions_awsAccountId,
    describeFolderPermissions_folderId,
    describeFolderPermissionsResponse_requestId,
    describeFolderPermissionsResponse_arn,
    describeFolderPermissionsResponse_permissions,
    describeFolderPermissionsResponse_folderId,
    describeFolderPermissionsResponse_status,

    -- ** DescribeFolderResolvedPermissions
    describeFolderResolvedPermissions_awsAccountId,
    describeFolderResolvedPermissions_folderId,
    describeFolderResolvedPermissionsResponse_requestId,
    describeFolderResolvedPermissionsResponse_arn,
    describeFolderResolvedPermissionsResponse_permissions,
    describeFolderResolvedPermissionsResponse_folderId,
    describeFolderResolvedPermissionsResponse_status,

    -- ** DescribeGroup
    describeGroup_groupName,
    describeGroup_awsAccountId,
    describeGroup_namespace,
    describeGroupResponse_requestId,
    describeGroupResponse_group,
    describeGroupResponse_status,

    -- ** DescribeGroupMembership
    describeGroupMembership_memberName,
    describeGroupMembership_groupName,
    describeGroupMembership_awsAccountId,
    describeGroupMembership_namespace,
    describeGroupMembershipResponse_requestId,
    describeGroupMembershipResponse_groupMember,
    describeGroupMembershipResponse_status,

    -- ** DescribeIAMPolicyAssignment
    describeIAMPolicyAssignment_awsAccountId,
    describeIAMPolicyAssignment_assignmentName,
    describeIAMPolicyAssignment_namespace,
    describeIAMPolicyAssignmentResponse_requestId,
    describeIAMPolicyAssignmentResponse_iAMPolicyAssignment,
    describeIAMPolicyAssignmentResponse_status,

    -- ** DescribeIngestion
    describeIngestion_awsAccountId,
    describeIngestion_dataSetId,
    describeIngestion_ingestionId,
    describeIngestionResponse_requestId,
    describeIngestionResponse_ingestion,
    describeIngestionResponse_status,

    -- ** DescribeIpRestriction
    describeIpRestriction_awsAccountId,
    describeIpRestrictionResponse_awsAccountId,
    describeIpRestrictionResponse_ipRestrictionRuleMap,
    describeIpRestrictionResponse_requestId,
    describeIpRestrictionResponse_enabled,
    describeIpRestrictionResponse_status,

    -- ** DescribeNamespace
    describeNamespace_awsAccountId,
    describeNamespace_namespace,
    describeNamespaceResponse_requestId,
    describeNamespaceResponse_namespace,
    describeNamespaceResponse_status,

    -- ** DescribeTemplate
    describeTemplate_versionNumber,
    describeTemplate_aliasName,
    describeTemplate_awsAccountId,
    describeTemplate_templateId,
    describeTemplateResponse_requestId,
    describeTemplateResponse_template,
    describeTemplateResponse_status,

    -- ** DescribeTemplateAlias
    describeTemplateAlias_awsAccountId,
    describeTemplateAlias_templateId,
    describeTemplateAlias_aliasName,
    describeTemplateAliasResponse_requestId,
    describeTemplateAliasResponse_templateAlias,
    describeTemplateAliasResponse_status,

    -- ** DescribeTemplatePermissions
    describeTemplatePermissions_awsAccountId,
    describeTemplatePermissions_templateId,
    describeTemplatePermissionsResponse_requestId,
    describeTemplatePermissionsResponse_permissions,
    describeTemplatePermissionsResponse_templateId,
    describeTemplatePermissionsResponse_templateArn,
    describeTemplatePermissionsResponse_status,

    -- ** DescribeTheme
    describeTheme_versionNumber,
    describeTheme_aliasName,
    describeTheme_awsAccountId,
    describeTheme_themeId,
    describeThemeResponse_requestId,
    describeThemeResponse_theme,
    describeThemeResponse_status,

    -- ** DescribeThemeAlias
    describeThemeAlias_awsAccountId,
    describeThemeAlias_themeId,
    describeThemeAlias_aliasName,
    describeThemeAliasResponse_requestId,
    describeThemeAliasResponse_themeAlias,
    describeThemeAliasResponse_status,

    -- ** DescribeThemePermissions
    describeThemePermissions_awsAccountId,
    describeThemePermissions_themeId,
    describeThemePermissionsResponse_themeArn,
    describeThemePermissionsResponse_requestId,
    describeThemePermissionsResponse_permissions,
    describeThemePermissionsResponse_themeId,
    describeThemePermissionsResponse_status,

    -- ** DescribeUser
    describeUser_userName,
    describeUser_awsAccountId,
    describeUser_namespace,
    describeUserResponse_user,
    describeUserResponse_requestId,
    describeUserResponse_status,

    -- ** GenerateEmbedUrlForAnonymousUser
    generateEmbedUrlForAnonymousUser_sessionTags,
    generateEmbedUrlForAnonymousUser_sessionLifetimeInMinutes,
    generateEmbedUrlForAnonymousUser_allowedDomains,
    generateEmbedUrlForAnonymousUser_awsAccountId,
    generateEmbedUrlForAnonymousUser_namespace,
    generateEmbedUrlForAnonymousUser_authorizedResourceArns,
    generateEmbedUrlForAnonymousUser_experienceConfiguration,
    generateEmbedUrlForAnonymousUserResponse_status,
    generateEmbedUrlForAnonymousUserResponse_embedUrl,
    generateEmbedUrlForAnonymousUserResponse_requestId,
    generateEmbedUrlForAnonymousUserResponse_anonymousUserArn,

    -- ** GenerateEmbedUrlForRegisteredUser
    generateEmbedUrlForRegisteredUser_sessionLifetimeInMinutes,
    generateEmbedUrlForRegisteredUser_allowedDomains,
    generateEmbedUrlForRegisteredUser_awsAccountId,
    generateEmbedUrlForRegisteredUser_userArn,
    generateEmbedUrlForRegisteredUser_experienceConfiguration,
    generateEmbedUrlForRegisteredUserResponse_status,
    generateEmbedUrlForRegisteredUserResponse_embedUrl,
    generateEmbedUrlForRegisteredUserResponse_requestId,

    -- ** GetDashboardEmbedUrl
    getDashboardEmbedUrl_undoRedoDisabled,
    getDashboardEmbedUrl_additionalDashboardIds,
    getDashboardEmbedUrl_resetDisabled,
    getDashboardEmbedUrl_sessionLifetimeInMinutes,
    getDashboardEmbedUrl_userArn,
    getDashboardEmbedUrl_namespace,
    getDashboardEmbedUrl_statePersistenceEnabled,
    getDashboardEmbedUrl_awsAccountId,
    getDashboardEmbedUrl_dashboardId,
    getDashboardEmbedUrl_identityType,
    getDashboardEmbedUrlResponse_requestId,
    getDashboardEmbedUrlResponse_embedUrl,
    getDashboardEmbedUrlResponse_status,

    -- ** GetSessionEmbedUrl
    getSessionEmbedUrl_sessionLifetimeInMinutes,
    getSessionEmbedUrl_userArn,
    getSessionEmbedUrl_entryPoint,
    getSessionEmbedUrl_awsAccountId,
    getSessionEmbedUrlResponse_requestId,
    getSessionEmbedUrlResponse_embedUrl,
    getSessionEmbedUrlResponse_status,

    -- ** ListAnalyses
    listAnalyses_nextToken,
    listAnalyses_maxResults,
    listAnalyses_awsAccountId,
    listAnalysesResponse_analysisSummaryList,
    listAnalysesResponse_nextToken,
    listAnalysesResponse_requestId,
    listAnalysesResponse_status,

    -- ** ListDashboardVersions
    listDashboardVersions_nextToken,
    listDashboardVersions_maxResults,
    listDashboardVersions_awsAccountId,
    listDashboardVersions_dashboardId,
    listDashboardVersionsResponse_nextToken,
    listDashboardVersionsResponse_requestId,
    listDashboardVersionsResponse_dashboardVersionSummaryList,
    listDashboardVersionsResponse_status,

    -- ** ListDashboards
    listDashboards_nextToken,
    listDashboards_maxResults,
    listDashboards_awsAccountId,
    listDashboardsResponse_dashboardSummaryList,
    listDashboardsResponse_nextToken,
    listDashboardsResponse_requestId,
    listDashboardsResponse_status,

    -- ** ListDataSets
    listDataSets_nextToken,
    listDataSets_maxResults,
    listDataSets_awsAccountId,
    listDataSetsResponse_nextToken,
    listDataSetsResponse_dataSetSummaries,
    listDataSetsResponse_requestId,
    listDataSetsResponse_status,

    -- ** ListDataSources
    listDataSources_nextToken,
    listDataSources_maxResults,
    listDataSources_awsAccountId,
    listDataSourcesResponse_nextToken,
    listDataSourcesResponse_dataSources,
    listDataSourcesResponse_requestId,
    listDataSourcesResponse_status,

    -- ** ListFolderMembers
    listFolderMembers_nextToken,
    listFolderMembers_maxResults,
    listFolderMembers_awsAccountId,
    listFolderMembers_folderId,
    listFolderMembersResponse_nextToken,
    listFolderMembersResponse_requestId,
    listFolderMembersResponse_folderMemberList,
    listFolderMembersResponse_status,

    -- ** ListFolders
    listFolders_nextToken,
    listFolders_maxResults,
    listFolders_awsAccountId,
    listFoldersResponse_nextToken,
    listFoldersResponse_folderSummaryList,
    listFoldersResponse_requestId,
    listFoldersResponse_status,

    -- ** ListGroupMemberships
    listGroupMemberships_nextToken,
    listGroupMemberships_maxResults,
    listGroupMemberships_groupName,
    listGroupMemberships_awsAccountId,
    listGroupMemberships_namespace,
    listGroupMembershipsResponse_nextToken,
    listGroupMembershipsResponse_requestId,
    listGroupMembershipsResponse_groupMemberList,
    listGroupMembershipsResponse_status,

    -- ** ListGroups
    listGroups_nextToken,
    listGroups_maxResults,
    listGroups_awsAccountId,
    listGroups_namespace,
    listGroupsResponse_nextToken,
    listGroupsResponse_requestId,
    listGroupsResponse_groupList,
    listGroupsResponse_status,

    -- ** ListIAMPolicyAssignments
    listIAMPolicyAssignments_nextToken,
    listIAMPolicyAssignments_maxResults,
    listIAMPolicyAssignments_assignmentStatus,
    listIAMPolicyAssignments_awsAccountId,
    listIAMPolicyAssignments_namespace,
    listIAMPolicyAssignmentsResponse_nextToken,
    listIAMPolicyAssignmentsResponse_iAMPolicyAssignments,
    listIAMPolicyAssignmentsResponse_requestId,
    listIAMPolicyAssignmentsResponse_status,

    -- ** ListIAMPolicyAssignmentsForUser
    listIAMPolicyAssignmentsForUser_nextToken,
    listIAMPolicyAssignmentsForUser_maxResults,
    listIAMPolicyAssignmentsForUser_awsAccountId,
    listIAMPolicyAssignmentsForUser_userName,
    listIAMPolicyAssignmentsForUser_namespace,
    listIAMPolicyAssignmentsForUserResponse_activeAssignments,
    listIAMPolicyAssignmentsForUserResponse_nextToken,
    listIAMPolicyAssignmentsForUserResponse_requestId,
    listIAMPolicyAssignmentsForUserResponse_status,

    -- ** ListIngestions
    listIngestions_nextToken,
    listIngestions_maxResults,
    listIngestions_dataSetId,
    listIngestions_awsAccountId,
    listIngestionsResponse_nextToken,
    listIngestionsResponse_requestId,
    listIngestionsResponse_ingestions,
    listIngestionsResponse_status,

    -- ** ListNamespaces
    listNamespaces_nextToken,
    listNamespaces_maxResults,
    listNamespaces_awsAccountId,
    listNamespacesResponse_nextToken,
    listNamespacesResponse_namespaces,
    listNamespacesResponse_requestId,
    listNamespacesResponse_status,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_requestId,
    listTagsForResourceResponse_status,

    -- ** ListTemplateAliases
    listTemplateAliases_nextToken,
    listTemplateAliases_maxResults,
    listTemplateAliases_awsAccountId,
    listTemplateAliases_templateId,
    listTemplateAliasesResponse_nextToken,
    listTemplateAliasesResponse_requestId,
    listTemplateAliasesResponse_templateAliasList,
    listTemplateAliasesResponse_status,

    -- ** ListTemplateVersions
    listTemplateVersions_nextToken,
    listTemplateVersions_maxResults,
    listTemplateVersions_awsAccountId,
    listTemplateVersions_templateId,
    listTemplateVersionsResponse_nextToken,
    listTemplateVersionsResponse_requestId,
    listTemplateVersionsResponse_templateVersionSummaryList,
    listTemplateVersionsResponse_status,

    -- ** ListTemplates
    listTemplates_nextToken,
    listTemplates_maxResults,
    listTemplates_awsAccountId,
    listTemplatesResponse_nextToken,
    listTemplatesResponse_requestId,
    listTemplatesResponse_templateSummaryList,
    listTemplatesResponse_status,

    -- ** ListThemeAliases
    listThemeAliases_nextToken,
    listThemeAliases_maxResults,
    listThemeAliases_awsAccountId,
    listThemeAliases_themeId,
    listThemeAliasesResponse_nextToken,
    listThemeAliasesResponse_requestId,
    listThemeAliasesResponse_themeAliasList,
    listThemeAliasesResponse_status,

    -- ** ListThemeVersions
    listThemeVersions_nextToken,
    listThemeVersions_maxResults,
    listThemeVersions_awsAccountId,
    listThemeVersions_themeId,
    listThemeVersionsResponse_nextToken,
    listThemeVersionsResponse_requestId,
    listThemeVersionsResponse_themeVersionSummaryList,
    listThemeVersionsResponse_status,

    -- ** ListThemes
    listThemes_nextToken,
    listThemes_type,
    listThemes_maxResults,
    listThemes_awsAccountId,
    listThemesResponse_nextToken,
    listThemesResponse_requestId,
    listThemesResponse_themeSummaryList,
    listThemesResponse_status,

    -- ** ListUserGroups
    listUserGroups_nextToken,
    listUserGroups_maxResults,
    listUserGroups_userName,
    listUserGroups_awsAccountId,
    listUserGroups_namespace,
    listUserGroupsResponse_nextToken,
    listUserGroupsResponse_requestId,
    listUserGroupsResponse_groupList,
    listUserGroupsResponse_status,

    -- ** ListUsers
    listUsers_nextToken,
    listUsers_maxResults,
    listUsers_awsAccountId,
    listUsers_namespace,
    listUsersResponse_nextToken,
    listUsersResponse_requestId,
    listUsersResponse_userList,
    listUsersResponse_status,

    -- ** RegisterUser
    registerUser_iamArn,
    registerUser_externalLoginFederationProviderType,
    registerUser_customFederationProviderUrl,
    registerUser_userName,
    registerUser_externalLoginId,
    registerUser_customPermissionsName,
    registerUser_sessionName,
    registerUser_identityType,
    registerUser_email,
    registerUser_userRole,
    registerUser_awsAccountId,
    registerUser_namespace,
    registerUserResponse_userInvitationUrl,
    registerUserResponse_user,
    registerUserResponse_requestId,
    registerUserResponse_status,

    -- ** RestoreAnalysis
    restoreAnalysis_awsAccountId,
    restoreAnalysis_analysisId,
    restoreAnalysisResponse_analysisId,
    restoreAnalysisResponse_requestId,
    restoreAnalysisResponse_arn,
    restoreAnalysisResponse_status,

    -- ** SearchAnalyses
    searchAnalyses_nextToken,
    searchAnalyses_maxResults,
    searchAnalyses_awsAccountId,
    searchAnalyses_filters,
    searchAnalysesResponse_analysisSummaryList,
    searchAnalysesResponse_nextToken,
    searchAnalysesResponse_requestId,
    searchAnalysesResponse_status,

    -- ** SearchDashboards
    searchDashboards_nextToken,
    searchDashboards_maxResults,
    searchDashboards_awsAccountId,
    searchDashboards_filters,
    searchDashboardsResponse_dashboardSummaryList,
    searchDashboardsResponse_nextToken,
    searchDashboardsResponse_requestId,
    searchDashboardsResponse_status,

    -- ** SearchDataSets
    searchDataSets_nextToken,
    searchDataSets_maxResults,
    searchDataSets_awsAccountId,
    searchDataSets_filters,
    searchDataSetsResponse_nextToken,
    searchDataSetsResponse_dataSetSummaries,
    searchDataSetsResponse_requestId,
    searchDataSetsResponse_status,

    -- ** SearchDataSources
    searchDataSources_nextToken,
    searchDataSources_maxResults,
    searchDataSources_awsAccountId,
    searchDataSources_filters,
    searchDataSourcesResponse_nextToken,
    searchDataSourcesResponse_requestId,
    searchDataSourcesResponse_dataSourceSummaries,
    searchDataSourcesResponse_status,

    -- ** SearchFolders
    searchFolders_nextToken,
    searchFolders_maxResults,
    searchFolders_awsAccountId,
    searchFolders_filters,
    searchFoldersResponse_nextToken,
    searchFoldersResponse_folderSummaryList,
    searchFoldersResponse_requestId,
    searchFoldersResponse_status,

    -- ** SearchGroups
    searchGroups_nextToken,
    searchGroups_maxResults,
    searchGroups_awsAccountId,
    searchGroups_namespace,
    searchGroups_filters,
    searchGroupsResponse_nextToken,
    searchGroupsResponse_requestId,
    searchGroupsResponse_groupList,
    searchGroupsResponse_status,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_requestId,
    tagResourceResponse_status,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_requestId,
    untagResourceResponse_status,

    -- ** UpdateAccountCustomization
    updateAccountCustomization_namespace,
    updateAccountCustomization_awsAccountId,
    updateAccountCustomization_accountCustomization,
    updateAccountCustomizationResponse_awsAccountId,
    updateAccountCustomizationResponse_requestId,
    updateAccountCustomizationResponse_arn,
    updateAccountCustomizationResponse_namespace,
    updateAccountCustomizationResponse_accountCustomization,
    updateAccountCustomizationResponse_status,

    -- ** UpdateAccountSettings
    updateAccountSettings_notificationEmail,
    updateAccountSettings_terminationProtectionEnabled,
    updateAccountSettings_awsAccountId,
    updateAccountSettings_defaultNamespace,
    updateAccountSettingsResponse_requestId,
    updateAccountSettingsResponse_status,

    -- ** UpdateAnalysis
    updateAnalysis_themeArn,
    updateAnalysis_parameters,
    updateAnalysis_awsAccountId,
    updateAnalysis_analysisId,
    updateAnalysis_name,
    updateAnalysis_sourceEntity,
    updateAnalysisResponse_analysisId,
    updateAnalysisResponse_requestId,
    updateAnalysisResponse_arn,
    updateAnalysisResponse_updateStatus,
    updateAnalysisResponse_status,

    -- ** UpdateAnalysisPermissions
    updateAnalysisPermissions_grantPermissions,
    updateAnalysisPermissions_revokePermissions,
    updateAnalysisPermissions_awsAccountId,
    updateAnalysisPermissions_analysisId,
    updateAnalysisPermissionsResponse_analysisId,
    updateAnalysisPermissionsResponse_requestId,
    updateAnalysisPermissionsResponse_permissions,
    updateAnalysisPermissionsResponse_analysisArn,
    updateAnalysisPermissionsResponse_status,

    -- ** UpdateDashboard
    updateDashboard_themeArn,
    updateDashboard_versionDescription,
    updateDashboard_dashboardPublishOptions,
    updateDashboard_parameters,
    updateDashboard_awsAccountId,
    updateDashboard_dashboardId,
    updateDashboard_name,
    updateDashboard_sourceEntity,
    updateDashboardResponse_creationStatus,
    updateDashboardResponse_requestId,
    updateDashboardResponse_arn,
    updateDashboardResponse_status,
    updateDashboardResponse_dashboardId,
    updateDashboardResponse_versionArn,
    updateDashboardResponse_httpStatus,

    -- ** UpdateDashboardPermissions
    updateDashboardPermissions_grantPermissions,
    updateDashboardPermissions_revokeLinkPermissions,
    updateDashboardPermissions_grantLinkPermissions,
    updateDashboardPermissions_revokePermissions,
    updateDashboardPermissions_awsAccountId,
    updateDashboardPermissions_dashboardId,
    updateDashboardPermissionsResponse_linkSharingConfiguration,
    updateDashboardPermissionsResponse_requestId,
    updateDashboardPermissionsResponse_permissions,
    updateDashboardPermissionsResponse_dashboardId,
    updateDashboardPermissionsResponse_dashboardArn,
    updateDashboardPermissionsResponse_status,

    -- ** UpdateDashboardPublishedVersion
    updateDashboardPublishedVersion_awsAccountId,
    updateDashboardPublishedVersion_dashboardId,
    updateDashboardPublishedVersion_versionNumber,
    updateDashboardPublishedVersionResponse_requestId,
    updateDashboardPublishedVersionResponse_dashboardId,
    updateDashboardPublishedVersionResponse_dashboardArn,
    updateDashboardPublishedVersionResponse_status,

    -- ** UpdateDataSet
    updateDataSet_columnLevelPermissionRules,
    updateDataSet_dataSetUsageConfiguration,
    updateDataSet_rowLevelPermissionTagConfiguration,
    updateDataSet_columnGroups,
    updateDataSet_fieldFolders,
    updateDataSet_rowLevelPermissionDataSet,
    updateDataSet_logicalTableMap,
    updateDataSet_awsAccountId,
    updateDataSet_dataSetId,
    updateDataSet_name,
    updateDataSet_physicalTableMap,
    updateDataSet_importMode,
    updateDataSetResponse_ingestionArn,
    updateDataSetResponse_requestId,
    updateDataSetResponse_arn,
    updateDataSetResponse_dataSetId,
    updateDataSetResponse_ingestionId,
    updateDataSetResponse_status,

    -- ** UpdateDataSetPermissions
    updateDataSetPermissions_grantPermissions,
    updateDataSetPermissions_revokePermissions,
    updateDataSetPermissions_awsAccountId,
    updateDataSetPermissions_dataSetId,
    updateDataSetPermissionsResponse_requestId,
    updateDataSetPermissionsResponse_dataSetArn,
    updateDataSetPermissionsResponse_dataSetId,
    updateDataSetPermissionsResponse_status,

    -- ** UpdateDataSource
    updateDataSource_dataSourceParameters,
    updateDataSource_vpcConnectionProperties,
    updateDataSource_sslProperties,
    updateDataSource_credentials,
    updateDataSource_awsAccountId,
    updateDataSource_dataSourceId,
    updateDataSource_name,
    updateDataSourceResponse_dataSourceId,
    updateDataSourceResponse_requestId,
    updateDataSourceResponse_arn,
    updateDataSourceResponse_updateStatus,
    updateDataSourceResponse_status,

    -- ** UpdateDataSourcePermissions
    updateDataSourcePermissions_grantPermissions,
    updateDataSourcePermissions_revokePermissions,
    updateDataSourcePermissions_awsAccountId,
    updateDataSourcePermissions_dataSourceId,
    updateDataSourcePermissionsResponse_dataSourceArn,
    updateDataSourcePermissionsResponse_dataSourceId,
    updateDataSourcePermissionsResponse_requestId,
    updateDataSourcePermissionsResponse_status,

    -- ** UpdateFolder
    updateFolder_awsAccountId,
    updateFolder_folderId,
    updateFolder_name,
    updateFolderResponse_requestId,
    updateFolderResponse_arn,
    updateFolderResponse_folderId,
    updateFolderResponse_status,

    -- ** UpdateFolderPermissions
    updateFolderPermissions_grantPermissions,
    updateFolderPermissions_revokePermissions,
    updateFolderPermissions_awsAccountId,
    updateFolderPermissions_folderId,
    updateFolderPermissionsResponse_requestId,
    updateFolderPermissionsResponse_arn,
    updateFolderPermissionsResponse_permissions,
    updateFolderPermissionsResponse_status,
    updateFolderPermissionsResponse_folderId,
    updateFolderPermissionsResponse_httpStatus,

    -- ** UpdateGroup
    updateGroup_description,
    updateGroup_groupName,
    updateGroup_awsAccountId,
    updateGroup_namespace,
    updateGroupResponse_requestId,
    updateGroupResponse_group,
    updateGroupResponse_status,

    -- ** UpdateIAMPolicyAssignment
    updateIAMPolicyAssignment_identities,
    updateIAMPolicyAssignment_policyArn,
    updateIAMPolicyAssignment_assignmentStatus,
    updateIAMPolicyAssignment_awsAccountId,
    updateIAMPolicyAssignment_assignmentName,
    updateIAMPolicyAssignment_namespace,
    updateIAMPolicyAssignmentResponse_requestId,
    updateIAMPolicyAssignmentResponse_identities,
    updateIAMPolicyAssignmentResponse_policyArn,
    updateIAMPolicyAssignmentResponse_assignmentName,
    updateIAMPolicyAssignmentResponse_assignmentId,
    updateIAMPolicyAssignmentResponse_assignmentStatus,
    updateIAMPolicyAssignmentResponse_status,

    -- ** UpdateIpRestriction
    updateIpRestriction_ipRestrictionRuleMap,
    updateIpRestriction_enabled,
    updateIpRestriction_awsAccountId,
    updateIpRestrictionResponse_awsAccountId,
    updateIpRestrictionResponse_requestId,
    updateIpRestrictionResponse_status,

    -- ** UpdatePublicSharingSettings
    updatePublicSharingSettings_publicSharingEnabled,
    updatePublicSharingSettings_awsAccountId,
    updatePublicSharingSettingsResponse_requestId,
    updatePublicSharingSettingsResponse_status,

    -- ** UpdateTemplate
    updateTemplate_name,
    updateTemplate_versionDescription,
    updateTemplate_awsAccountId,
    updateTemplate_templateId,
    updateTemplate_sourceEntity,
    updateTemplateResponse_creationStatus,
    updateTemplateResponse_requestId,
    updateTemplateResponse_arn,
    updateTemplateResponse_templateId,
    updateTemplateResponse_versionArn,
    updateTemplateResponse_status,

    -- ** UpdateTemplateAlias
    updateTemplateAlias_awsAccountId,
    updateTemplateAlias_templateId,
    updateTemplateAlias_aliasName,
    updateTemplateAlias_templateVersionNumber,
    updateTemplateAliasResponse_requestId,
    updateTemplateAliasResponse_templateAlias,
    updateTemplateAliasResponse_status,

    -- ** UpdateTemplatePermissions
    updateTemplatePermissions_grantPermissions,
    updateTemplatePermissions_revokePermissions,
    updateTemplatePermissions_awsAccountId,
    updateTemplatePermissions_templateId,
    updateTemplatePermissionsResponse_requestId,
    updateTemplatePermissionsResponse_permissions,
    updateTemplatePermissionsResponse_templateId,
    updateTemplatePermissionsResponse_templateArn,
    updateTemplatePermissionsResponse_status,

    -- ** UpdateTheme
    updateTheme_name,
    updateTheme_configuration,
    updateTheme_versionDescription,
    updateTheme_awsAccountId,
    updateTheme_themeId,
    updateTheme_baseThemeId,
    updateThemeResponse_creationStatus,
    updateThemeResponse_requestId,
    updateThemeResponse_arn,
    updateThemeResponse_versionArn,
    updateThemeResponse_themeId,
    updateThemeResponse_status,

    -- ** UpdateThemeAlias
    updateThemeAlias_awsAccountId,
    updateThemeAlias_themeId,
    updateThemeAlias_aliasName,
    updateThemeAlias_themeVersionNumber,
    updateThemeAliasResponse_requestId,
    updateThemeAliasResponse_themeAlias,
    updateThemeAliasResponse_status,

    -- ** UpdateThemePermissions
    updateThemePermissions_grantPermissions,
    updateThemePermissions_revokePermissions,
    updateThemePermissions_awsAccountId,
    updateThemePermissions_themeId,
    updateThemePermissionsResponse_themeArn,
    updateThemePermissionsResponse_requestId,
    updateThemePermissionsResponse_permissions,
    updateThemePermissionsResponse_themeId,
    updateThemePermissionsResponse_status,

    -- ** UpdateUser
    updateUser_externalLoginFederationProviderType,
    updateUser_unapplyCustomPermissions,
    updateUser_customFederationProviderUrl,
    updateUser_externalLoginId,
    updateUser_customPermissionsName,
    updateUser_userName,
    updateUser_awsAccountId,
    updateUser_namespace,
    updateUser_email,
    updateUser_role,
    updateUserResponse_user,
    updateUserResponse_requestId,
    updateUserResponse_status,

    -- * Types

    -- ** AccountCustomization
    accountCustomization_defaultEmailCustomizationTemplate,
    accountCustomization_defaultTheme,

    -- ** AccountInfo
    accountInfo_authenticationType,
    accountInfo_notificationEmail,
    accountInfo_edition,
    accountInfo_accountSubscriptionStatus,
    accountInfo_accountName,

    -- ** AccountSettings
    accountSettings_notificationEmail,
    accountSettings_edition,
    accountSettings_accountName,
    accountSettings_defaultNamespace,
    accountSettings_terminationProtectionEnabled,
    accountSettings_publicSharingEnabled,

    -- ** ActiveIAMPolicyAssignment
    activeIAMPolicyAssignment_policyArn,
    activeIAMPolicyAssignment_assignmentName,

    -- ** AdHocFilteringOption
    adHocFilteringOption_availabilityStatus,

    -- ** AmazonElasticsearchParameters
    amazonElasticsearchParameters_domain,

    -- ** AmazonOpenSearchParameters
    amazonOpenSearchParameters_domain,

    -- ** Analysis
    analysis_analysisId,
    analysis_name,
    analysis_themeArn,
    analysis_createdTime,
    analysis_arn,
    analysis_status,
    analysis_lastUpdatedTime,
    analysis_errors,
    analysis_dataSetArns,
    analysis_sheets,

    -- ** AnalysisError
    analysisError_message,
    analysisError_type,

    -- ** AnalysisSearchFilter
    analysisSearchFilter_name,
    analysisSearchFilter_operator,
    analysisSearchFilter_value,

    -- ** AnalysisSourceEntity
    analysisSourceEntity_sourceTemplate,

    -- ** AnalysisSourceTemplate
    analysisSourceTemplate_dataSetReferences,
    analysisSourceTemplate_arn,

    -- ** AnalysisSummary
    analysisSummary_analysisId,
    analysisSummary_name,
    analysisSummary_createdTime,
    analysisSummary_arn,
    analysisSummary_status,
    analysisSummary_lastUpdatedTime,

    -- ** AnonymousUserDashboardEmbeddingConfiguration
    anonymousUserDashboardEmbeddingConfiguration_initialDashboardId,

    -- ** AnonymousUserDashboardVisualEmbeddingConfiguration
    anonymousUserDashboardVisualEmbeddingConfiguration_initialDashboardVisualId,

    -- ** AnonymousUserEmbeddingExperienceConfiguration
    anonymousUserEmbeddingExperienceConfiguration_dashboardVisual,
    anonymousUserEmbeddingExperienceConfiguration_dashboard,
    anonymousUserEmbeddingExperienceConfiguration_qSearchBar,

    -- ** AnonymousUserQSearchBarEmbeddingConfiguration
    anonymousUserQSearchBarEmbeddingConfiguration_initialTopicId,

    -- ** AthenaParameters
    athenaParameters_roleArn,
    athenaParameters_workGroup,

    -- ** AuroraParameters
    auroraParameters_host,
    auroraParameters_port,
    auroraParameters_database,

    -- ** AuroraPostgreSqlParameters
    auroraPostgreSqlParameters_host,
    auroraPostgreSqlParameters_port,
    auroraPostgreSqlParameters_database,

    -- ** AwsIotAnalyticsParameters
    awsIotAnalyticsParameters_dataSetName,

    -- ** BorderStyle
    borderStyle_show,

    -- ** CalculatedColumn
    calculatedColumn_columnName,
    calculatedColumn_columnId,
    calculatedColumn_expression,

    -- ** CastColumnTypeOperation
    castColumnTypeOperation_format,
    castColumnTypeOperation_columnName,
    castColumnTypeOperation_newColumnType,

    -- ** ColumnDescription
    columnDescription_text,

    -- ** ColumnGroup
    columnGroup_geoSpatialColumnGroup,

    -- ** ColumnGroupColumnSchema
    columnGroupColumnSchema_name,

    -- ** ColumnGroupSchema
    columnGroupSchema_columnGroupColumnSchemaList,
    columnGroupSchema_name,

    -- ** ColumnLevelPermissionRule
    columnLevelPermissionRule_columnNames,
    columnLevelPermissionRule_principals,

    -- ** ColumnSchema
    columnSchema_name,
    columnSchema_geographicRole,
    columnSchema_dataType,

    -- ** ColumnTag
    columnTag_columnGeographicRole,
    columnTag_columnDescription,

    -- ** CreateColumnsOperation
    createColumnsOperation_columns,

    -- ** CredentialPair
    credentialPair_alternateDataSourceParameters,
    credentialPair_username,
    credentialPair_password,

    -- ** CustomSql
    customSql_columns,
    customSql_dataSourceArn,
    customSql_name,
    customSql_sqlQuery,

    -- ** Dashboard
    dashboard_name,
    dashboard_createdTime,
    dashboard_arn,
    dashboard_lastUpdatedTime,
    dashboard_dashboardId,
    dashboard_version,
    dashboard_lastPublishedTime,

    -- ** DashboardError
    dashboardError_message,
    dashboardError_type,

    -- ** DashboardPublishOptions
    dashboardPublishOptions_adHocFilteringOption,
    dashboardPublishOptions_sheetControlsOption,
    dashboardPublishOptions_exportToCSVOption,

    -- ** DashboardSearchFilter
    dashboardSearchFilter_name,
    dashboardSearchFilter_value,
    dashboardSearchFilter_operator,

    -- ** DashboardSourceEntity
    dashboardSourceEntity_sourceTemplate,

    -- ** DashboardSourceTemplate
    dashboardSourceTemplate_dataSetReferences,
    dashboardSourceTemplate_arn,

    -- ** DashboardSummary
    dashboardSummary_name,
    dashboardSummary_createdTime,
    dashboardSummary_arn,
    dashboardSummary_lastUpdatedTime,
    dashboardSummary_publishedVersionNumber,
    dashboardSummary_dashboardId,
    dashboardSummary_lastPublishedTime,

    -- ** DashboardVersion
    dashboardVersion_sourceEntityArn,
    dashboardVersion_themeArn,
    dashboardVersion_createdTime,
    dashboardVersion_arn,
    dashboardVersion_status,
    dashboardVersion_description,
    dashboardVersion_errors,
    dashboardVersion_dataSetArns,
    dashboardVersion_versionNumber,
    dashboardVersion_sheets,

    -- ** DashboardVersionSummary
    dashboardVersionSummary_sourceEntityArn,
    dashboardVersionSummary_createdTime,
    dashboardVersionSummary_arn,
    dashboardVersionSummary_status,
    dashboardVersionSummary_description,
    dashboardVersionSummary_versionNumber,

    -- ** DashboardVisualId
    dashboardVisualId_dashboardId,
    dashboardVisualId_sheetId,
    dashboardVisualId_visualId,

    -- ** DataColorPalette
    dataColorPalette_minMaxGradient,
    dataColorPalette_emptyFillColor,
    dataColorPalette_colors,

    -- ** DataSet
    dataSet_name,
    dataSet_columnLevelPermissionRules,
    dataSet_createdTime,
    dataSet_dataSetUsageConfiguration,
    dataSet_arn,
    dataSet_outputColumns,
    dataSet_consumedSpiceCapacityInBytes,
    dataSet_rowLevelPermissionTagConfiguration,
    dataSet_lastUpdatedTime,
    dataSet_columnGroups,
    dataSet_fieldFolders,
    dataSet_rowLevelPermissionDataSet,
    dataSet_dataSetId,
    dataSet_logicalTableMap,
    dataSet_importMode,
    dataSet_physicalTableMap,

    -- ** DataSetConfiguration
    dataSetConfiguration_dataSetSchema,
    dataSetConfiguration_columnGroupSchemaList,
    dataSetConfiguration_placeholder,

    -- ** DataSetReference
    dataSetReference_dataSetPlaceholder,
    dataSetReference_dataSetArn,

    -- ** DataSetSchema
    dataSetSchema_columnSchemaList,

    -- ** DataSetSearchFilter
    dataSetSearchFilter_operator,
    dataSetSearchFilter_name,
    dataSetSearchFilter_value,

    -- ** DataSetSummary
    dataSetSummary_name,
    dataSetSummary_columnLevelPermissionRulesApplied,
    dataSetSummary_createdTime,
    dataSetSummary_arn,
    dataSetSummary_lastUpdatedTime,
    dataSetSummary_rowLevelPermissionDataSet,
    dataSetSummary_rowLevelPermissionTagConfigurationApplied,
    dataSetSummary_dataSetId,
    dataSetSummary_importMode,

    -- ** DataSetUsageConfiguration
    dataSetUsageConfiguration_disableUseAsDirectQuerySource,
    dataSetUsageConfiguration_disableUseAsImportedSource,

    -- ** DataSource
    dataSource_name,
    dataSource_type,
    dataSource_dataSourceId,
    dataSource_createdTime,
    dataSource_dataSourceParameters,
    dataSource_arn,
    dataSource_status,
    dataSource_lastUpdatedTime,
    dataSource_vpcConnectionProperties,
    dataSource_sslProperties,
    dataSource_secretArn,
    dataSource_alternateDataSourceParameters,
    dataSource_errorInfo,

    -- ** DataSourceCredentials
    dataSourceCredentials_secretArn,
    dataSourceCredentials_credentialPair,
    dataSourceCredentials_copySourceArn,

    -- ** DataSourceErrorInfo
    dataSourceErrorInfo_message,
    dataSourceErrorInfo_type,

    -- ** DataSourceParameters
    dataSourceParameters_serviceNowParameters,
    dataSourceParameters_s3Parameters,
    dataSourceParameters_postgreSqlParameters,
    dataSourceParameters_mySqlParameters,
    dataSourceParameters_exasolParameters,
    dataSourceParameters_redshiftParameters,
    dataSourceParameters_awsIotAnalyticsParameters,
    dataSourceParameters_sparkParameters,
    dataSourceParameters_teradataParameters,
    dataSourceParameters_twitterParameters,
    dataSourceParameters_prestoParameters,
    dataSourceParameters_snowflakeParameters,
    dataSourceParameters_rdsParameters,
    dataSourceParameters_oracleParameters,
    dataSourceParameters_auroraPostgreSqlParameters,
    dataSourceParameters_mariaDbParameters,
    dataSourceParameters_athenaParameters,
    dataSourceParameters_amazonOpenSearchParameters,
    dataSourceParameters_databricksParameters,
    dataSourceParameters_jiraParameters,
    dataSourceParameters_amazonElasticsearchParameters,
    dataSourceParameters_sqlServerParameters,
    dataSourceParameters_auroraParameters,

    -- ** DataSourceSearchFilter
    dataSourceSearchFilter_operator,
    dataSourceSearchFilter_name,
    dataSourceSearchFilter_value,

    -- ** DataSourceSummary
    dataSourceSummary_name,
    dataSourceSummary_type,
    dataSourceSummary_dataSourceId,
    dataSourceSummary_createdTime,
    dataSourceSummary_arn,
    dataSourceSummary_lastUpdatedTime,

    -- ** DatabricksParameters
    databricksParameters_host,
    databricksParameters_port,
    databricksParameters_sqlEndpointPath,

    -- ** DateTimeParameter
    dateTimeParameter_name,
    dateTimeParameter_values,

    -- ** DecimalParameter
    decimalParameter_name,
    decimalParameter_values,

    -- ** ErrorInfo
    errorInfo_message,
    errorInfo_type,

    -- ** ExasolParameters
    exasolParameters_host,
    exasolParameters_port,

    -- ** ExportToCSVOption
    exportToCSVOption_availabilityStatus,

    -- ** FieldFolder
    fieldFolder_columns,
    fieldFolder_description,

    -- ** FilterOperation
    filterOperation_conditionExpression,

    -- ** Folder
    folder_name,
    folder_createdTime,
    folder_arn,
    folder_lastUpdatedTime,
    folder_folderId,
    folder_folderType,
    folder_folderPath,

    -- ** FolderMember
    folderMember_memberId,
    folderMember_memberType,

    -- ** FolderSearchFilter
    folderSearchFilter_name,
    folderSearchFilter_operator,
    folderSearchFilter_value,

    -- ** FolderSummary
    folderSummary_name,
    folderSummary_createdTime,
    folderSummary_arn,
    folderSummary_lastUpdatedTime,
    folderSummary_folderId,
    folderSummary_folderType,

    -- ** GeoSpatialColumnGroup
    geoSpatialColumnGroup_countryCode,
    geoSpatialColumnGroup_name,
    geoSpatialColumnGroup_columns,

    -- ** Group
    group_principalId,
    group_arn,
    group_groupName,
    group_description,

    -- ** GroupMember
    groupMember_memberName,
    groupMember_arn,

    -- ** GroupSearchFilter
    groupSearchFilter_operator,
    groupSearchFilter_name,
    groupSearchFilter_value,

    -- ** GutterStyle
    gutterStyle_show,

    -- ** IAMPolicyAssignment
    iAMPolicyAssignment_awsAccountId,
    iAMPolicyAssignment_identities,
    iAMPolicyAssignment_policyArn,
    iAMPolicyAssignment_assignmentName,
    iAMPolicyAssignment_assignmentId,
    iAMPolicyAssignment_assignmentStatus,

    -- ** IAMPolicyAssignmentSummary
    iAMPolicyAssignmentSummary_assignmentName,
    iAMPolicyAssignmentSummary_assignmentStatus,

    -- ** Ingestion
    ingestion_requestType,
    ingestion_ingestionSizeInBytes,
    ingestion_ingestionTimeInSeconds,
    ingestion_requestSource,
    ingestion_queueInfo,
    ingestion_rowInfo,
    ingestion_ingestionId,
    ingestion_errorInfo,
    ingestion_arn,
    ingestion_ingestionStatus,
    ingestion_createdTime,

    -- ** InputColumn
    inputColumn_name,
    inputColumn_type,

    -- ** IntegerParameter
    integerParameter_name,
    integerParameter_values,

    -- ** JiraParameters
    jiraParameters_siteBaseUrl,

    -- ** JoinInstruction
    joinInstruction_leftJoinKeyProperties,
    joinInstruction_rightJoinKeyProperties,
    joinInstruction_leftOperand,
    joinInstruction_rightOperand,
    joinInstruction_type,
    joinInstruction_onClause,

    -- ** JoinKeyProperties
    joinKeyProperties_uniqueKey,

    -- ** LinkSharingConfiguration
    linkSharingConfiguration_permissions,

    -- ** LogicalTable
    logicalTable_dataTransforms,
    logicalTable_alias,
    logicalTable_source,

    -- ** LogicalTableSource
    logicalTableSource_physicalTableId,
    logicalTableSource_joinInstruction,
    logicalTableSource_dataSetArn,

    -- ** ManifestFileLocation
    manifestFileLocation_bucket,
    manifestFileLocation_key,

    -- ** MarginStyle
    marginStyle_show,

    -- ** MariaDbParameters
    mariaDbParameters_host,
    mariaDbParameters_port,
    mariaDbParameters_database,

    -- ** MemberIdArnPair
    memberIdArnPair_memberId,
    memberIdArnPair_memberArn,

    -- ** MySqlParameters
    mySqlParameters_host,
    mySqlParameters_port,
    mySqlParameters_database,

    -- ** NamespaceError
    namespaceError_message,
    namespaceError_type,

    -- ** NamespaceInfoV2
    namespaceInfoV2_creationStatus,
    namespaceInfoV2_name,
    namespaceInfoV2_namespaceError,
    namespaceInfoV2_arn,
    namespaceInfoV2_capacityRegion,
    namespaceInfoV2_identityStore,

    -- ** OracleParameters
    oracleParameters_host,
    oracleParameters_port,
    oracleParameters_database,

    -- ** OutputColumn
    outputColumn_name,
    outputColumn_type,
    outputColumn_description,

    -- ** Parameters
    parameters_decimalParameters,
    parameters_dateTimeParameters,
    parameters_integerParameters,
    parameters_stringParameters,

    -- ** PhysicalTable
    physicalTable_s3Source,
    physicalTable_relationalTable,
    physicalTable_customSql,

    -- ** PostgreSqlParameters
    postgreSqlParameters_host,
    postgreSqlParameters_port,
    postgreSqlParameters_database,

    -- ** PrestoParameters
    prestoParameters_host,
    prestoParameters_port,
    prestoParameters_catalog,

    -- ** ProjectOperation
    projectOperation_projectedColumns,

    -- ** QueueInfo
    queueInfo_waitingOnIngestion,
    queueInfo_queuedIngestion,

    -- ** RdsParameters
    rdsParameters_instanceId,
    rdsParameters_database,

    -- ** RedshiftParameters
    redshiftParameters_port,
    redshiftParameters_host,
    redshiftParameters_clusterId,
    redshiftParameters_database,

    -- ** RegisteredUserDashboardEmbeddingConfiguration
    registeredUserDashboardEmbeddingConfiguration_initialDashboardId,

    -- ** RegisteredUserDashboardVisualEmbeddingConfiguration
    registeredUserDashboardVisualEmbeddingConfiguration_initialDashboardVisualId,

    -- ** RegisteredUserEmbeddingExperienceConfiguration
    registeredUserEmbeddingExperienceConfiguration_dashboardVisual,
    registeredUserEmbeddingExperienceConfiguration_dashboard,
    registeredUserEmbeddingExperienceConfiguration_qSearchBar,
    registeredUserEmbeddingExperienceConfiguration_quickSightConsole,

    -- ** RegisteredUserQSearchBarEmbeddingConfiguration
    registeredUserQSearchBarEmbeddingConfiguration_initialTopicId,

    -- ** RegisteredUserQuickSightConsoleEmbeddingConfiguration
    registeredUserQuickSightConsoleEmbeddingConfiguration_initialPath,

    -- ** RelationalTable
    relationalTable_catalog,
    relationalTable_schema,
    relationalTable_dataSourceArn,
    relationalTable_name,
    relationalTable_inputColumns,

    -- ** RenameColumnOperation
    renameColumnOperation_columnName,
    renameColumnOperation_newColumnName,

    -- ** ResourcePermission
    resourcePermission_principal,
    resourcePermission_actions,

    -- ** RowInfo
    rowInfo_totalRowsInDataset,
    rowInfo_rowsDropped,
    rowInfo_rowsIngested,

    -- ** RowLevelPermissionDataSet
    rowLevelPermissionDataSet_formatVersion,
    rowLevelPermissionDataSet_status,
    rowLevelPermissionDataSet_namespace,
    rowLevelPermissionDataSet_arn,
    rowLevelPermissionDataSet_permissionPolicy,

    -- ** RowLevelPermissionTagConfiguration
    rowLevelPermissionTagConfiguration_status,
    rowLevelPermissionTagConfiguration_tagRules,

    -- ** RowLevelPermissionTagRule
    rowLevelPermissionTagRule_matchAllValue,
    rowLevelPermissionTagRule_tagMultiValueDelimiter,
    rowLevelPermissionTagRule_tagKey,
    rowLevelPermissionTagRule_columnName,

    -- ** S3Parameters
    s3Parameters_manifestFileLocation,

    -- ** S3Source
    s3Source_uploadSettings,
    s3Source_dataSourceArn,
    s3Source_inputColumns,

    -- ** ServiceNowParameters
    serviceNowParameters_siteBaseUrl,

    -- ** SessionTag
    sessionTag_key,
    sessionTag_value,

    -- ** Sheet
    sheet_name,
    sheet_sheetId,

    -- ** SheetControlsOption
    sheetControlsOption_visibilityState,

    -- ** SheetStyle
    sheetStyle_tile,
    sheetStyle_tileLayout,

    -- ** SignupResponse
    signupResponse_directoryType,
    signupResponse_userLoginName,
    signupResponse_iAMUser,
    signupResponse_accountName,

    -- ** SnowflakeParameters
    snowflakeParameters_host,
    snowflakeParameters_database,
    snowflakeParameters_warehouse,

    -- ** SparkParameters
    sparkParameters_host,
    sparkParameters_port,

    -- ** SqlServerParameters
    sqlServerParameters_host,
    sqlServerParameters_port,
    sqlServerParameters_database,

    -- ** SslProperties
    sslProperties_disableSsl,

    -- ** StringParameter
    stringParameter_name,
    stringParameter_values,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TagColumnOperation
    tagColumnOperation_columnName,
    tagColumnOperation_tags,

    -- ** Template
    template_name,
    template_createdTime,
    template_arn,
    template_lastUpdatedTime,
    template_templateId,
    template_version,

    -- ** TemplateAlias
    templateAlias_templateVersionNumber,
    templateAlias_arn,
    templateAlias_aliasName,

    -- ** TemplateError
    templateError_message,
    templateError_type,

    -- ** TemplateSourceAnalysis
    templateSourceAnalysis_arn,
    templateSourceAnalysis_dataSetReferences,

    -- ** TemplateSourceEntity
    templateSourceEntity_sourceAnalysis,
    templateSourceEntity_sourceTemplate,

    -- ** TemplateSourceTemplate
    templateSourceTemplate_arn,

    -- ** TemplateSummary
    templateSummary_name,
    templateSummary_createdTime,
    templateSummary_latestVersionNumber,
    templateSummary_arn,
    templateSummary_lastUpdatedTime,
    templateSummary_templateId,

    -- ** TemplateVersion
    templateVersion_sourceEntityArn,
    templateVersion_themeArn,
    templateVersion_createdTime,
    templateVersion_status,
    templateVersion_description,
    templateVersion_errors,
    templateVersion_versionNumber,
    templateVersion_sheets,
    templateVersion_dataSetConfigurations,

    -- ** TemplateVersionSummary
    templateVersionSummary_createdTime,
    templateVersionSummary_arn,
    templateVersionSummary_status,
    templateVersionSummary_description,
    templateVersionSummary_versionNumber,

    -- ** TeradataParameters
    teradataParameters_host,
    teradataParameters_port,
    teradataParameters_database,

    -- ** Theme
    theme_name,
    theme_type,
    theme_createdTime,
    theme_arn,
    theme_lastUpdatedTime,
    theme_themeId,
    theme_version,

    -- ** ThemeAlias
    themeAlias_arn,
    themeAlias_aliasName,
    themeAlias_themeVersionNumber,

    -- ** ThemeConfiguration
    themeConfiguration_dataColorPalette,
    themeConfiguration_sheet,
    themeConfiguration_uIColorPalette,

    -- ** ThemeError
    themeError_message,
    themeError_type,

    -- ** ThemeSummary
    themeSummary_name,
    themeSummary_createdTime,
    themeSummary_latestVersionNumber,
    themeSummary_arn,
    themeSummary_lastUpdatedTime,
    themeSummary_themeId,

    -- ** ThemeVersion
    themeVersion_createdTime,
    themeVersion_configuration,
    themeVersion_arn,
    themeVersion_status,
    themeVersion_description,
    themeVersion_errors,
    themeVersion_versionNumber,
    themeVersion_baseThemeId,

    -- ** ThemeVersionSummary
    themeVersionSummary_createdTime,
    themeVersionSummary_arn,
    themeVersionSummary_status,
    themeVersionSummary_description,
    themeVersionSummary_versionNumber,

    -- ** TileLayoutStyle
    tileLayoutStyle_gutter,
    tileLayoutStyle_margin,

    -- ** TileStyle
    tileStyle_border,

    -- ** TransformOperation
    transformOperation_untagColumnOperation,
    transformOperation_createColumnsOperation,
    transformOperation_renameColumnOperation,
    transformOperation_projectOperation,
    transformOperation_tagColumnOperation,
    transformOperation_filterOperation,
    transformOperation_castColumnTypeOperation,

    -- ** TwitterParameters
    twitterParameters_query,
    twitterParameters_maxRows,

    -- ** UIColorPalette
    uIColorPalette_accentForeground,
    uIColorPalette_danger,
    uIColorPalette_dangerForeground,
    uIColorPalette_secondaryBackground,
    uIColorPalette_primaryBackground,
    uIColorPalette_warningForeground,
    uIColorPalette_dimensionForeground,
    uIColorPalette_warning,
    uIColorPalette_successForeground,
    uIColorPalette_primaryForeground,
    uIColorPalette_secondaryForeground,
    uIColorPalette_measure,
    uIColorPalette_dimension,
    uIColorPalette_accent,
    uIColorPalette_success,
    uIColorPalette_measureForeground,

    -- ** UntagColumnOperation
    untagColumnOperation_columnName,
    untagColumnOperation_tagNames,

    -- ** UploadSettings
    uploadSettings_containsHeader,
    uploadSettings_format,
    uploadSettings_textQualifier,
    uploadSettings_delimiter,
    uploadSettings_startFromRow,

    -- ** User
    user_principalId,
    user_externalLoginFederationProviderType,
    user_active,
    user_email,
    user_userName,
    user_externalLoginFederationProviderUrl,
    user_arn,
    user_role,
    user_externalLoginId,
    user_identityType,
    user_customPermissionsName,

    -- ** VpcConnectionProperties
    vpcConnectionProperties_vpcConnectionArn,
  )
where

import Amazonka.QuickSight.CancelIngestion
import Amazonka.QuickSight.CreateAccountCustomization
import Amazonka.QuickSight.CreateAccountSubscription
import Amazonka.QuickSight.CreateAnalysis
import Amazonka.QuickSight.CreateDashboard
import Amazonka.QuickSight.CreateDataSet
import Amazonka.QuickSight.CreateDataSource
import Amazonka.QuickSight.CreateFolder
import Amazonka.QuickSight.CreateFolderMembership
import Amazonka.QuickSight.CreateGroup
import Amazonka.QuickSight.CreateGroupMembership
import Amazonka.QuickSight.CreateIAMPolicyAssignment
import Amazonka.QuickSight.CreateIngestion
import Amazonka.QuickSight.CreateNamespace
import Amazonka.QuickSight.CreateTemplate
import Amazonka.QuickSight.CreateTemplateAlias
import Amazonka.QuickSight.CreateTheme
import Amazonka.QuickSight.CreateThemeAlias
import Amazonka.QuickSight.DeleteAccountCustomization
import Amazonka.QuickSight.DeleteAccountSubscription
import Amazonka.QuickSight.DeleteAnalysis
import Amazonka.QuickSight.DeleteDashboard
import Amazonka.QuickSight.DeleteDataSet
import Amazonka.QuickSight.DeleteDataSource
import Amazonka.QuickSight.DeleteFolder
import Amazonka.QuickSight.DeleteFolderMembership
import Amazonka.QuickSight.DeleteGroup
import Amazonka.QuickSight.DeleteGroupMembership
import Amazonka.QuickSight.DeleteIAMPolicyAssignment
import Amazonka.QuickSight.DeleteNamespace
import Amazonka.QuickSight.DeleteTemplate
import Amazonka.QuickSight.DeleteTemplateAlias
import Amazonka.QuickSight.DeleteTheme
import Amazonka.QuickSight.DeleteThemeAlias
import Amazonka.QuickSight.DeleteUser
import Amazonka.QuickSight.DeleteUserByPrincipalId
import Amazonka.QuickSight.DescribeAccountCustomization
import Amazonka.QuickSight.DescribeAccountSettings
import Amazonka.QuickSight.DescribeAccountSubscription
import Amazonka.QuickSight.DescribeAnalysis
import Amazonka.QuickSight.DescribeAnalysisPermissions
import Amazonka.QuickSight.DescribeDashboard
import Amazonka.QuickSight.DescribeDashboardPermissions
import Amazonka.QuickSight.DescribeDataSet
import Amazonka.QuickSight.DescribeDataSetPermissions
import Amazonka.QuickSight.DescribeDataSource
import Amazonka.QuickSight.DescribeDataSourcePermissions
import Amazonka.QuickSight.DescribeFolder
import Amazonka.QuickSight.DescribeFolderPermissions
import Amazonka.QuickSight.DescribeFolderResolvedPermissions
import Amazonka.QuickSight.DescribeGroup
import Amazonka.QuickSight.DescribeGroupMembership
import Amazonka.QuickSight.DescribeIAMPolicyAssignment
import Amazonka.QuickSight.DescribeIngestion
import Amazonka.QuickSight.DescribeIpRestriction
import Amazonka.QuickSight.DescribeNamespace
import Amazonka.QuickSight.DescribeTemplate
import Amazonka.QuickSight.DescribeTemplateAlias
import Amazonka.QuickSight.DescribeTemplatePermissions
import Amazonka.QuickSight.DescribeTheme
import Amazonka.QuickSight.DescribeThemeAlias
import Amazonka.QuickSight.DescribeThemePermissions
import Amazonka.QuickSight.DescribeUser
import Amazonka.QuickSight.GenerateEmbedUrlForAnonymousUser
import Amazonka.QuickSight.GenerateEmbedUrlForRegisteredUser
import Amazonka.QuickSight.GetDashboardEmbedUrl
import Amazonka.QuickSight.GetSessionEmbedUrl
import Amazonka.QuickSight.ListAnalyses
import Amazonka.QuickSight.ListDashboardVersions
import Amazonka.QuickSight.ListDashboards
import Amazonka.QuickSight.ListDataSets
import Amazonka.QuickSight.ListDataSources
import Amazonka.QuickSight.ListFolderMembers
import Amazonka.QuickSight.ListFolders
import Amazonka.QuickSight.ListGroupMemberships
import Amazonka.QuickSight.ListGroups
import Amazonka.QuickSight.ListIAMPolicyAssignments
import Amazonka.QuickSight.ListIAMPolicyAssignmentsForUser
import Amazonka.QuickSight.ListIngestions
import Amazonka.QuickSight.ListNamespaces
import Amazonka.QuickSight.ListTagsForResource
import Amazonka.QuickSight.ListTemplateAliases
import Amazonka.QuickSight.ListTemplateVersions
import Amazonka.QuickSight.ListTemplates
import Amazonka.QuickSight.ListThemeAliases
import Amazonka.QuickSight.ListThemeVersions
import Amazonka.QuickSight.ListThemes
import Amazonka.QuickSight.ListUserGroups
import Amazonka.QuickSight.ListUsers
import Amazonka.QuickSight.RegisterUser
import Amazonka.QuickSight.RestoreAnalysis
import Amazonka.QuickSight.SearchAnalyses
import Amazonka.QuickSight.SearchDashboards
import Amazonka.QuickSight.SearchDataSets
import Amazonka.QuickSight.SearchDataSources
import Amazonka.QuickSight.SearchFolders
import Amazonka.QuickSight.SearchGroups
import Amazonka.QuickSight.TagResource
import Amazonka.QuickSight.Types.AccountCustomization
import Amazonka.QuickSight.Types.AccountInfo
import Amazonka.QuickSight.Types.AccountSettings
import Amazonka.QuickSight.Types.ActiveIAMPolicyAssignment
import Amazonka.QuickSight.Types.AdHocFilteringOption
import Amazonka.QuickSight.Types.AmazonElasticsearchParameters
import Amazonka.QuickSight.Types.AmazonOpenSearchParameters
import Amazonka.QuickSight.Types.Analysis
import Amazonka.QuickSight.Types.AnalysisError
import Amazonka.QuickSight.Types.AnalysisSearchFilter
import Amazonka.QuickSight.Types.AnalysisSourceEntity
import Amazonka.QuickSight.Types.AnalysisSourceTemplate
import Amazonka.QuickSight.Types.AnalysisSummary
import Amazonka.QuickSight.Types.AnonymousUserDashboardEmbeddingConfiguration
import Amazonka.QuickSight.Types.AnonymousUserDashboardVisualEmbeddingConfiguration
import Amazonka.QuickSight.Types.AnonymousUserEmbeddingExperienceConfiguration
import Amazonka.QuickSight.Types.AnonymousUserQSearchBarEmbeddingConfiguration
import Amazonka.QuickSight.Types.AthenaParameters
import Amazonka.QuickSight.Types.AuroraParameters
import Amazonka.QuickSight.Types.AuroraPostgreSqlParameters
import Amazonka.QuickSight.Types.AwsIotAnalyticsParameters
import Amazonka.QuickSight.Types.BorderStyle
import Amazonka.QuickSight.Types.CalculatedColumn
import Amazonka.QuickSight.Types.CastColumnTypeOperation
import Amazonka.QuickSight.Types.ColumnDescription
import Amazonka.QuickSight.Types.ColumnGroup
import Amazonka.QuickSight.Types.ColumnGroupColumnSchema
import Amazonka.QuickSight.Types.ColumnGroupSchema
import Amazonka.QuickSight.Types.ColumnLevelPermissionRule
import Amazonka.QuickSight.Types.ColumnSchema
import Amazonka.QuickSight.Types.ColumnTag
import Amazonka.QuickSight.Types.CreateColumnsOperation
import Amazonka.QuickSight.Types.CredentialPair
import Amazonka.QuickSight.Types.CustomSql
import Amazonka.QuickSight.Types.Dashboard
import Amazonka.QuickSight.Types.DashboardError
import Amazonka.QuickSight.Types.DashboardPublishOptions
import Amazonka.QuickSight.Types.DashboardSearchFilter
import Amazonka.QuickSight.Types.DashboardSourceEntity
import Amazonka.QuickSight.Types.DashboardSourceTemplate
import Amazonka.QuickSight.Types.DashboardSummary
import Amazonka.QuickSight.Types.DashboardVersion
import Amazonka.QuickSight.Types.DashboardVersionSummary
import Amazonka.QuickSight.Types.DashboardVisualId
import Amazonka.QuickSight.Types.DataColorPalette
import Amazonka.QuickSight.Types.DataSet
import Amazonka.QuickSight.Types.DataSetConfiguration
import Amazonka.QuickSight.Types.DataSetReference
import Amazonka.QuickSight.Types.DataSetSchema
import Amazonka.QuickSight.Types.DataSetSearchFilter
import Amazonka.QuickSight.Types.DataSetSummary
import Amazonka.QuickSight.Types.DataSetUsageConfiguration
import Amazonka.QuickSight.Types.DataSource
import Amazonka.QuickSight.Types.DataSourceCredentials
import Amazonka.QuickSight.Types.DataSourceErrorInfo
import Amazonka.QuickSight.Types.DataSourceParameters
import Amazonka.QuickSight.Types.DataSourceSearchFilter
import Amazonka.QuickSight.Types.DataSourceSummary
import Amazonka.QuickSight.Types.DatabricksParameters
import Amazonka.QuickSight.Types.DateTimeParameter
import Amazonka.QuickSight.Types.DecimalParameter
import Amazonka.QuickSight.Types.ErrorInfo
import Amazonka.QuickSight.Types.ExasolParameters
import Amazonka.QuickSight.Types.ExportToCSVOption
import Amazonka.QuickSight.Types.FieldFolder
import Amazonka.QuickSight.Types.FilterOperation
import Amazonka.QuickSight.Types.Folder
import Amazonka.QuickSight.Types.FolderMember
import Amazonka.QuickSight.Types.FolderSearchFilter
import Amazonka.QuickSight.Types.FolderSummary
import Amazonka.QuickSight.Types.GeoSpatialColumnGroup
import Amazonka.QuickSight.Types.Group
import Amazonka.QuickSight.Types.GroupMember
import Amazonka.QuickSight.Types.GroupSearchFilter
import Amazonka.QuickSight.Types.GutterStyle
import Amazonka.QuickSight.Types.IAMPolicyAssignment
import Amazonka.QuickSight.Types.IAMPolicyAssignmentSummary
import Amazonka.QuickSight.Types.Ingestion
import Amazonka.QuickSight.Types.InputColumn
import Amazonka.QuickSight.Types.IntegerParameter
import Amazonka.QuickSight.Types.JiraParameters
import Amazonka.QuickSight.Types.JoinInstruction
import Amazonka.QuickSight.Types.JoinKeyProperties
import Amazonka.QuickSight.Types.LinkSharingConfiguration
import Amazonka.QuickSight.Types.LogicalTable
import Amazonka.QuickSight.Types.LogicalTableSource
import Amazonka.QuickSight.Types.ManifestFileLocation
import Amazonka.QuickSight.Types.MarginStyle
import Amazonka.QuickSight.Types.MariaDbParameters
import Amazonka.QuickSight.Types.MemberIdArnPair
import Amazonka.QuickSight.Types.MySqlParameters
import Amazonka.QuickSight.Types.NamespaceError
import Amazonka.QuickSight.Types.NamespaceInfoV2
import Amazonka.QuickSight.Types.OracleParameters
import Amazonka.QuickSight.Types.OutputColumn
import Amazonka.QuickSight.Types.Parameters
import Amazonka.QuickSight.Types.PhysicalTable
import Amazonka.QuickSight.Types.PostgreSqlParameters
import Amazonka.QuickSight.Types.PrestoParameters
import Amazonka.QuickSight.Types.ProjectOperation
import Amazonka.QuickSight.Types.QueueInfo
import Amazonka.QuickSight.Types.RdsParameters
import Amazonka.QuickSight.Types.RedshiftParameters
import Amazonka.QuickSight.Types.RegisteredUserDashboardEmbeddingConfiguration
import Amazonka.QuickSight.Types.RegisteredUserDashboardVisualEmbeddingConfiguration
import Amazonka.QuickSight.Types.RegisteredUserEmbeddingExperienceConfiguration
import Amazonka.QuickSight.Types.RegisteredUserQSearchBarEmbeddingConfiguration
import Amazonka.QuickSight.Types.RegisteredUserQuickSightConsoleEmbeddingConfiguration
import Amazonka.QuickSight.Types.RelationalTable
import Amazonka.QuickSight.Types.RenameColumnOperation
import Amazonka.QuickSight.Types.ResourcePermission
import Amazonka.QuickSight.Types.RowInfo
import Amazonka.QuickSight.Types.RowLevelPermissionDataSet
import Amazonka.QuickSight.Types.RowLevelPermissionTagConfiguration
import Amazonka.QuickSight.Types.RowLevelPermissionTagRule
import Amazonka.QuickSight.Types.S3Parameters
import Amazonka.QuickSight.Types.S3Source
import Amazonka.QuickSight.Types.ServiceNowParameters
import Amazonka.QuickSight.Types.SessionTag
import Amazonka.QuickSight.Types.Sheet
import Amazonka.QuickSight.Types.SheetControlsOption
import Amazonka.QuickSight.Types.SheetStyle
import Amazonka.QuickSight.Types.SignupResponse
import Amazonka.QuickSight.Types.SnowflakeParameters
import Amazonka.QuickSight.Types.SparkParameters
import Amazonka.QuickSight.Types.SqlServerParameters
import Amazonka.QuickSight.Types.SslProperties
import Amazonka.QuickSight.Types.StringParameter
import Amazonka.QuickSight.Types.Tag
import Amazonka.QuickSight.Types.TagColumnOperation
import Amazonka.QuickSight.Types.Template
import Amazonka.QuickSight.Types.TemplateAlias
import Amazonka.QuickSight.Types.TemplateError
import Amazonka.QuickSight.Types.TemplateSourceAnalysis
import Amazonka.QuickSight.Types.TemplateSourceEntity
import Amazonka.QuickSight.Types.TemplateSourceTemplate
import Amazonka.QuickSight.Types.TemplateSummary
import Amazonka.QuickSight.Types.TemplateVersion
import Amazonka.QuickSight.Types.TemplateVersionSummary
import Amazonka.QuickSight.Types.TeradataParameters
import Amazonka.QuickSight.Types.Theme
import Amazonka.QuickSight.Types.ThemeAlias
import Amazonka.QuickSight.Types.ThemeConfiguration
import Amazonka.QuickSight.Types.ThemeError
import Amazonka.QuickSight.Types.ThemeSummary
import Amazonka.QuickSight.Types.ThemeVersion
import Amazonka.QuickSight.Types.ThemeVersionSummary
import Amazonka.QuickSight.Types.TileLayoutStyle
import Amazonka.QuickSight.Types.TileStyle
import Amazonka.QuickSight.Types.TransformOperation
import Amazonka.QuickSight.Types.TwitterParameters
import Amazonka.QuickSight.Types.UIColorPalette
import Amazonka.QuickSight.Types.UntagColumnOperation
import Amazonka.QuickSight.Types.UploadSettings
import Amazonka.QuickSight.Types.User
import Amazonka.QuickSight.Types.VpcConnectionProperties
import Amazonka.QuickSight.UntagResource
import Amazonka.QuickSight.UpdateAccountCustomization
import Amazonka.QuickSight.UpdateAccountSettings
import Amazonka.QuickSight.UpdateAnalysis
import Amazonka.QuickSight.UpdateAnalysisPermissions
import Amazonka.QuickSight.UpdateDashboard
import Amazonka.QuickSight.UpdateDashboardPermissions
import Amazonka.QuickSight.UpdateDashboardPublishedVersion
import Amazonka.QuickSight.UpdateDataSet
import Amazonka.QuickSight.UpdateDataSetPermissions
import Amazonka.QuickSight.UpdateDataSource
import Amazonka.QuickSight.UpdateDataSourcePermissions
import Amazonka.QuickSight.UpdateFolder
import Amazonka.QuickSight.UpdateFolderPermissions
import Amazonka.QuickSight.UpdateGroup
import Amazonka.QuickSight.UpdateIAMPolicyAssignment
import Amazonka.QuickSight.UpdateIpRestriction
import Amazonka.QuickSight.UpdatePublicSharingSettings
import Amazonka.QuickSight.UpdateTemplate
import Amazonka.QuickSight.UpdateTemplateAlias
import Amazonka.QuickSight.UpdateTemplatePermissions
import Amazonka.QuickSight.UpdateTheme
import Amazonka.QuickSight.UpdateThemeAlias
import Amazonka.QuickSight.UpdateThemePermissions
import Amazonka.QuickSight.UpdateUser
