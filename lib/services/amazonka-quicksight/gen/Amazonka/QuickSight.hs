{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.QuickSight
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2018-04-01@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon QuickSight API Reference
--
-- Amazon QuickSight is a fully managed, serverless business intelligence
-- service for the Amazon Web Services Cloud that makes it easy to extend
-- data and insights to every user in your organization. This API reference
-- contains documentation for a programming interface that you can use to
-- manage Amazon QuickSight.
module Amazonka.QuickSight
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** IdentityTypeNotSupportedException
    _IdentityTypeNotSupportedException,

    -- ** ResourceUnavailableException
    _ResourceUnavailableException,

    -- ** UnsupportedPricingPlanException
    _UnsupportedPricingPlanException,

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** PreconditionNotMetException
    _PreconditionNotMetException,

    -- ** QuickSightUserNotFoundException
    _QuickSightUserNotFoundException,

    -- ** ConcurrentUpdatingException
    _ConcurrentUpdatingException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** SessionLifetimeInMinutesInvalidException
    _SessionLifetimeInMinutesInvalidException,

    -- ** UnsupportedUserEditionException
    _UnsupportedUserEditionException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** InvalidNextTokenException
    _InvalidNextTokenException,

    -- ** ConflictException
    _ConflictException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** ResourceExistsException
    _ResourceExistsException,

    -- ** DomainNotWhitelistedException
    _DomainNotWhitelistedException,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** InvalidParameterValueException
    _InvalidParameterValueException,

    -- ** InternalFailureException
    _InternalFailureException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CancelIngestion
    CancelIngestion (CancelIngestion'),
    newCancelIngestion,
    CancelIngestionResponse (CancelIngestionResponse'),
    newCancelIngestionResponse,

    -- ** CreateAccountCustomization
    CreateAccountCustomization (CreateAccountCustomization'),
    newCreateAccountCustomization,
    CreateAccountCustomizationResponse (CreateAccountCustomizationResponse'),
    newCreateAccountCustomizationResponse,

    -- ** CreateAccountSubscription
    CreateAccountSubscription (CreateAccountSubscription'),
    newCreateAccountSubscription,
    CreateAccountSubscriptionResponse (CreateAccountSubscriptionResponse'),
    newCreateAccountSubscriptionResponse,

    -- ** CreateAnalysis
    CreateAnalysis (CreateAnalysis'),
    newCreateAnalysis,
    CreateAnalysisResponse (CreateAnalysisResponse'),
    newCreateAnalysisResponse,

    -- ** CreateDashboard
    CreateDashboard (CreateDashboard'),
    newCreateDashboard,
    CreateDashboardResponse (CreateDashboardResponse'),
    newCreateDashboardResponse,

    -- ** CreateDataSet
    CreateDataSet (CreateDataSet'),
    newCreateDataSet,
    CreateDataSetResponse (CreateDataSetResponse'),
    newCreateDataSetResponse,

    -- ** CreateDataSource
    CreateDataSource (CreateDataSource'),
    newCreateDataSource,
    CreateDataSourceResponse (CreateDataSourceResponse'),
    newCreateDataSourceResponse,

    -- ** CreateFolder
    CreateFolder (CreateFolder'),
    newCreateFolder,
    CreateFolderResponse (CreateFolderResponse'),
    newCreateFolderResponse,

    -- ** CreateFolderMembership
    CreateFolderMembership (CreateFolderMembership'),
    newCreateFolderMembership,
    CreateFolderMembershipResponse (CreateFolderMembershipResponse'),
    newCreateFolderMembershipResponse,

    -- ** CreateGroup
    CreateGroup (CreateGroup'),
    newCreateGroup,
    CreateGroupResponse (CreateGroupResponse'),
    newCreateGroupResponse,

    -- ** CreateGroupMembership
    CreateGroupMembership (CreateGroupMembership'),
    newCreateGroupMembership,
    CreateGroupMembershipResponse (CreateGroupMembershipResponse'),
    newCreateGroupMembershipResponse,

    -- ** CreateIAMPolicyAssignment
    CreateIAMPolicyAssignment (CreateIAMPolicyAssignment'),
    newCreateIAMPolicyAssignment,
    CreateIAMPolicyAssignmentResponse (CreateIAMPolicyAssignmentResponse'),
    newCreateIAMPolicyAssignmentResponse,

    -- ** CreateIngestion
    CreateIngestion (CreateIngestion'),
    newCreateIngestion,
    CreateIngestionResponse (CreateIngestionResponse'),
    newCreateIngestionResponse,

    -- ** CreateNamespace
    CreateNamespace (CreateNamespace'),
    newCreateNamespace,
    CreateNamespaceResponse (CreateNamespaceResponse'),
    newCreateNamespaceResponse,

    -- ** CreateTemplate
    CreateTemplate (CreateTemplate'),
    newCreateTemplate,
    CreateTemplateResponse (CreateTemplateResponse'),
    newCreateTemplateResponse,

    -- ** CreateTemplateAlias
    CreateTemplateAlias (CreateTemplateAlias'),
    newCreateTemplateAlias,
    CreateTemplateAliasResponse (CreateTemplateAliasResponse'),
    newCreateTemplateAliasResponse,

    -- ** CreateTheme
    CreateTheme (CreateTheme'),
    newCreateTheme,
    CreateThemeResponse (CreateThemeResponse'),
    newCreateThemeResponse,

    -- ** CreateThemeAlias
    CreateThemeAlias (CreateThemeAlias'),
    newCreateThemeAlias,
    CreateThemeAliasResponse (CreateThemeAliasResponse'),
    newCreateThemeAliasResponse,

    -- ** DeleteAccountCustomization
    DeleteAccountCustomization (DeleteAccountCustomization'),
    newDeleteAccountCustomization,
    DeleteAccountCustomizationResponse (DeleteAccountCustomizationResponse'),
    newDeleteAccountCustomizationResponse,

    -- ** DeleteAccountSubscription
    DeleteAccountSubscription (DeleteAccountSubscription'),
    newDeleteAccountSubscription,
    DeleteAccountSubscriptionResponse (DeleteAccountSubscriptionResponse'),
    newDeleteAccountSubscriptionResponse,

    -- ** DeleteAnalysis
    DeleteAnalysis (DeleteAnalysis'),
    newDeleteAnalysis,
    DeleteAnalysisResponse (DeleteAnalysisResponse'),
    newDeleteAnalysisResponse,

    -- ** DeleteDashboard
    DeleteDashboard (DeleteDashboard'),
    newDeleteDashboard,
    DeleteDashboardResponse (DeleteDashboardResponse'),
    newDeleteDashboardResponse,

    -- ** DeleteDataSet
    DeleteDataSet (DeleteDataSet'),
    newDeleteDataSet,
    DeleteDataSetResponse (DeleteDataSetResponse'),
    newDeleteDataSetResponse,

    -- ** DeleteDataSource
    DeleteDataSource (DeleteDataSource'),
    newDeleteDataSource,
    DeleteDataSourceResponse (DeleteDataSourceResponse'),
    newDeleteDataSourceResponse,

    -- ** DeleteFolder
    DeleteFolder (DeleteFolder'),
    newDeleteFolder,
    DeleteFolderResponse (DeleteFolderResponse'),
    newDeleteFolderResponse,

    -- ** DeleteFolderMembership
    DeleteFolderMembership (DeleteFolderMembership'),
    newDeleteFolderMembership,
    DeleteFolderMembershipResponse (DeleteFolderMembershipResponse'),
    newDeleteFolderMembershipResponse,

    -- ** DeleteGroup
    DeleteGroup (DeleteGroup'),
    newDeleteGroup,
    DeleteGroupResponse (DeleteGroupResponse'),
    newDeleteGroupResponse,

    -- ** DeleteGroupMembership
    DeleteGroupMembership (DeleteGroupMembership'),
    newDeleteGroupMembership,
    DeleteGroupMembershipResponse (DeleteGroupMembershipResponse'),
    newDeleteGroupMembershipResponse,

    -- ** DeleteIAMPolicyAssignment
    DeleteIAMPolicyAssignment (DeleteIAMPolicyAssignment'),
    newDeleteIAMPolicyAssignment,
    DeleteIAMPolicyAssignmentResponse (DeleteIAMPolicyAssignmentResponse'),
    newDeleteIAMPolicyAssignmentResponse,

    -- ** DeleteNamespace
    DeleteNamespace (DeleteNamespace'),
    newDeleteNamespace,
    DeleteNamespaceResponse (DeleteNamespaceResponse'),
    newDeleteNamespaceResponse,

    -- ** DeleteTemplate
    DeleteTemplate (DeleteTemplate'),
    newDeleteTemplate,
    DeleteTemplateResponse (DeleteTemplateResponse'),
    newDeleteTemplateResponse,

    -- ** DeleteTemplateAlias
    DeleteTemplateAlias (DeleteTemplateAlias'),
    newDeleteTemplateAlias,
    DeleteTemplateAliasResponse (DeleteTemplateAliasResponse'),
    newDeleteTemplateAliasResponse,

    -- ** DeleteTheme
    DeleteTheme (DeleteTheme'),
    newDeleteTheme,
    DeleteThemeResponse (DeleteThemeResponse'),
    newDeleteThemeResponse,

    -- ** DeleteThemeAlias
    DeleteThemeAlias (DeleteThemeAlias'),
    newDeleteThemeAlias,
    DeleteThemeAliasResponse (DeleteThemeAliasResponse'),
    newDeleteThemeAliasResponse,

    -- ** DeleteUser
    DeleteUser (DeleteUser'),
    newDeleteUser,
    DeleteUserResponse (DeleteUserResponse'),
    newDeleteUserResponse,

    -- ** DeleteUserByPrincipalId
    DeleteUserByPrincipalId (DeleteUserByPrincipalId'),
    newDeleteUserByPrincipalId,
    DeleteUserByPrincipalIdResponse (DeleteUserByPrincipalIdResponse'),
    newDeleteUserByPrincipalIdResponse,

    -- ** DescribeAccountCustomization
    DescribeAccountCustomization (DescribeAccountCustomization'),
    newDescribeAccountCustomization,
    DescribeAccountCustomizationResponse (DescribeAccountCustomizationResponse'),
    newDescribeAccountCustomizationResponse,

    -- ** DescribeAccountSettings
    DescribeAccountSettings (DescribeAccountSettings'),
    newDescribeAccountSettings,
    DescribeAccountSettingsResponse (DescribeAccountSettingsResponse'),
    newDescribeAccountSettingsResponse,

    -- ** DescribeAccountSubscription
    DescribeAccountSubscription (DescribeAccountSubscription'),
    newDescribeAccountSubscription,
    DescribeAccountSubscriptionResponse (DescribeAccountSubscriptionResponse'),
    newDescribeAccountSubscriptionResponse,

    -- ** DescribeAnalysis
    DescribeAnalysis (DescribeAnalysis'),
    newDescribeAnalysis,
    DescribeAnalysisResponse (DescribeAnalysisResponse'),
    newDescribeAnalysisResponse,

    -- ** DescribeAnalysisPermissions
    DescribeAnalysisPermissions (DescribeAnalysisPermissions'),
    newDescribeAnalysisPermissions,
    DescribeAnalysisPermissionsResponse (DescribeAnalysisPermissionsResponse'),
    newDescribeAnalysisPermissionsResponse,

    -- ** DescribeDashboard
    DescribeDashboard (DescribeDashboard'),
    newDescribeDashboard,
    DescribeDashboardResponse (DescribeDashboardResponse'),
    newDescribeDashboardResponse,

    -- ** DescribeDashboardPermissions
    DescribeDashboardPermissions (DescribeDashboardPermissions'),
    newDescribeDashboardPermissions,
    DescribeDashboardPermissionsResponse (DescribeDashboardPermissionsResponse'),
    newDescribeDashboardPermissionsResponse,

    -- ** DescribeDataSet
    DescribeDataSet (DescribeDataSet'),
    newDescribeDataSet,
    DescribeDataSetResponse (DescribeDataSetResponse'),
    newDescribeDataSetResponse,

    -- ** DescribeDataSetPermissions
    DescribeDataSetPermissions (DescribeDataSetPermissions'),
    newDescribeDataSetPermissions,
    DescribeDataSetPermissionsResponse (DescribeDataSetPermissionsResponse'),
    newDescribeDataSetPermissionsResponse,

    -- ** DescribeDataSource
    DescribeDataSource (DescribeDataSource'),
    newDescribeDataSource,
    DescribeDataSourceResponse (DescribeDataSourceResponse'),
    newDescribeDataSourceResponse,

    -- ** DescribeDataSourcePermissions
    DescribeDataSourcePermissions (DescribeDataSourcePermissions'),
    newDescribeDataSourcePermissions,
    DescribeDataSourcePermissionsResponse (DescribeDataSourcePermissionsResponse'),
    newDescribeDataSourcePermissionsResponse,

    -- ** DescribeFolder
    DescribeFolder (DescribeFolder'),
    newDescribeFolder,
    DescribeFolderResponse (DescribeFolderResponse'),
    newDescribeFolderResponse,

    -- ** DescribeFolderPermissions
    DescribeFolderPermissions (DescribeFolderPermissions'),
    newDescribeFolderPermissions,
    DescribeFolderPermissionsResponse (DescribeFolderPermissionsResponse'),
    newDescribeFolderPermissionsResponse,

    -- ** DescribeFolderResolvedPermissions
    DescribeFolderResolvedPermissions (DescribeFolderResolvedPermissions'),
    newDescribeFolderResolvedPermissions,
    DescribeFolderResolvedPermissionsResponse (DescribeFolderResolvedPermissionsResponse'),
    newDescribeFolderResolvedPermissionsResponse,

    -- ** DescribeGroup
    DescribeGroup (DescribeGroup'),
    newDescribeGroup,
    DescribeGroupResponse (DescribeGroupResponse'),
    newDescribeGroupResponse,

    -- ** DescribeGroupMembership
    DescribeGroupMembership (DescribeGroupMembership'),
    newDescribeGroupMembership,
    DescribeGroupMembershipResponse (DescribeGroupMembershipResponse'),
    newDescribeGroupMembershipResponse,

    -- ** DescribeIAMPolicyAssignment
    DescribeIAMPolicyAssignment (DescribeIAMPolicyAssignment'),
    newDescribeIAMPolicyAssignment,
    DescribeIAMPolicyAssignmentResponse (DescribeIAMPolicyAssignmentResponse'),
    newDescribeIAMPolicyAssignmentResponse,

    -- ** DescribeIngestion
    DescribeIngestion (DescribeIngestion'),
    newDescribeIngestion,
    DescribeIngestionResponse (DescribeIngestionResponse'),
    newDescribeIngestionResponse,

    -- ** DescribeIpRestriction
    DescribeIpRestriction (DescribeIpRestriction'),
    newDescribeIpRestriction,
    DescribeIpRestrictionResponse (DescribeIpRestrictionResponse'),
    newDescribeIpRestrictionResponse,

    -- ** DescribeNamespace
    DescribeNamespace (DescribeNamespace'),
    newDescribeNamespace,
    DescribeNamespaceResponse (DescribeNamespaceResponse'),
    newDescribeNamespaceResponse,

    -- ** DescribeTemplate
    DescribeTemplate (DescribeTemplate'),
    newDescribeTemplate,
    DescribeTemplateResponse (DescribeTemplateResponse'),
    newDescribeTemplateResponse,

    -- ** DescribeTemplateAlias
    DescribeTemplateAlias (DescribeTemplateAlias'),
    newDescribeTemplateAlias,
    DescribeTemplateAliasResponse (DescribeTemplateAliasResponse'),
    newDescribeTemplateAliasResponse,

    -- ** DescribeTemplatePermissions
    DescribeTemplatePermissions (DescribeTemplatePermissions'),
    newDescribeTemplatePermissions,
    DescribeTemplatePermissionsResponse (DescribeTemplatePermissionsResponse'),
    newDescribeTemplatePermissionsResponse,

    -- ** DescribeTheme
    DescribeTheme (DescribeTheme'),
    newDescribeTheme,
    DescribeThemeResponse (DescribeThemeResponse'),
    newDescribeThemeResponse,

    -- ** DescribeThemeAlias
    DescribeThemeAlias (DescribeThemeAlias'),
    newDescribeThemeAlias,
    DescribeThemeAliasResponse (DescribeThemeAliasResponse'),
    newDescribeThemeAliasResponse,

    -- ** DescribeThemePermissions
    DescribeThemePermissions (DescribeThemePermissions'),
    newDescribeThemePermissions,
    DescribeThemePermissionsResponse (DescribeThemePermissionsResponse'),
    newDescribeThemePermissionsResponse,

    -- ** DescribeUser
    DescribeUser (DescribeUser'),
    newDescribeUser,
    DescribeUserResponse (DescribeUserResponse'),
    newDescribeUserResponse,

    -- ** GenerateEmbedUrlForAnonymousUser
    GenerateEmbedUrlForAnonymousUser (GenerateEmbedUrlForAnonymousUser'),
    newGenerateEmbedUrlForAnonymousUser,
    GenerateEmbedUrlForAnonymousUserResponse (GenerateEmbedUrlForAnonymousUserResponse'),
    newGenerateEmbedUrlForAnonymousUserResponse,

    -- ** GenerateEmbedUrlForRegisteredUser
    GenerateEmbedUrlForRegisteredUser (GenerateEmbedUrlForRegisteredUser'),
    newGenerateEmbedUrlForRegisteredUser,
    GenerateEmbedUrlForRegisteredUserResponse (GenerateEmbedUrlForRegisteredUserResponse'),
    newGenerateEmbedUrlForRegisteredUserResponse,

    -- ** GetDashboardEmbedUrl
    GetDashboardEmbedUrl (GetDashboardEmbedUrl'),
    newGetDashboardEmbedUrl,
    GetDashboardEmbedUrlResponse (GetDashboardEmbedUrlResponse'),
    newGetDashboardEmbedUrlResponse,

    -- ** GetSessionEmbedUrl
    GetSessionEmbedUrl (GetSessionEmbedUrl'),
    newGetSessionEmbedUrl,
    GetSessionEmbedUrlResponse (GetSessionEmbedUrlResponse'),
    newGetSessionEmbedUrlResponse,

    -- ** ListAnalyses (Paginated)
    ListAnalyses (ListAnalyses'),
    newListAnalyses,
    ListAnalysesResponse (ListAnalysesResponse'),
    newListAnalysesResponse,

    -- ** ListDashboardVersions (Paginated)
    ListDashboardVersions (ListDashboardVersions'),
    newListDashboardVersions,
    ListDashboardVersionsResponse (ListDashboardVersionsResponse'),
    newListDashboardVersionsResponse,

    -- ** ListDashboards (Paginated)
    ListDashboards (ListDashboards'),
    newListDashboards,
    ListDashboardsResponse (ListDashboardsResponse'),
    newListDashboardsResponse,

    -- ** ListDataSets (Paginated)
    ListDataSets (ListDataSets'),
    newListDataSets,
    ListDataSetsResponse (ListDataSetsResponse'),
    newListDataSetsResponse,

    -- ** ListDataSources (Paginated)
    ListDataSources (ListDataSources'),
    newListDataSources,
    ListDataSourcesResponse (ListDataSourcesResponse'),
    newListDataSourcesResponse,

    -- ** ListFolderMembers
    ListFolderMembers (ListFolderMembers'),
    newListFolderMembers,
    ListFolderMembersResponse (ListFolderMembersResponse'),
    newListFolderMembersResponse,

    -- ** ListFolders
    ListFolders (ListFolders'),
    newListFolders,
    ListFoldersResponse (ListFoldersResponse'),
    newListFoldersResponse,

    -- ** ListGroupMemberships
    ListGroupMemberships (ListGroupMemberships'),
    newListGroupMemberships,
    ListGroupMembershipsResponse (ListGroupMembershipsResponse'),
    newListGroupMembershipsResponse,

    -- ** ListGroups
    ListGroups (ListGroups'),
    newListGroups,
    ListGroupsResponse (ListGroupsResponse'),
    newListGroupsResponse,

    -- ** ListIAMPolicyAssignments
    ListIAMPolicyAssignments (ListIAMPolicyAssignments'),
    newListIAMPolicyAssignments,
    ListIAMPolicyAssignmentsResponse (ListIAMPolicyAssignmentsResponse'),
    newListIAMPolicyAssignmentsResponse,

    -- ** ListIAMPolicyAssignmentsForUser
    ListIAMPolicyAssignmentsForUser (ListIAMPolicyAssignmentsForUser'),
    newListIAMPolicyAssignmentsForUser,
    ListIAMPolicyAssignmentsForUserResponse (ListIAMPolicyAssignmentsForUserResponse'),
    newListIAMPolicyAssignmentsForUserResponse,

    -- ** ListIngestions (Paginated)
    ListIngestions (ListIngestions'),
    newListIngestions,
    ListIngestionsResponse (ListIngestionsResponse'),
    newListIngestionsResponse,

    -- ** ListNamespaces (Paginated)
    ListNamespaces (ListNamespaces'),
    newListNamespaces,
    ListNamespacesResponse (ListNamespacesResponse'),
    newListNamespacesResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListTemplateAliases (Paginated)
    ListTemplateAliases (ListTemplateAliases'),
    newListTemplateAliases,
    ListTemplateAliasesResponse (ListTemplateAliasesResponse'),
    newListTemplateAliasesResponse,

    -- ** ListTemplateVersions (Paginated)
    ListTemplateVersions (ListTemplateVersions'),
    newListTemplateVersions,
    ListTemplateVersionsResponse (ListTemplateVersionsResponse'),
    newListTemplateVersionsResponse,

    -- ** ListTemplates (Paginated)
    ListTemplates (ListTemplates'),
    newListTemplates,
    ListTemplatesResponse (ListTemplatesResponse'),
    newListTemplatesResponse,

    -- ** ListThemeAliases
    ListThemeAliases (ListThemeAliases'),
    newListThemeAliases,
    ListThemeAliasesResponse (ListThemeAliasesResponse'),
    newListThemeAliasesResponse,

    -- ** ListThemeVersions (Paginated)
    ListThemeVersions (ListThemeVersions'),
    newListThemeVersions,
    ListThemeVersionsResponse (ListThemeVersionsResponse'),
    newListThemeVersionsResponse,

    -- ** ListThemes (Paginated)
    ListThemes (ListThemes'),
    newListThemes,
    ListThemesResponse (ListThemesResponse'),
    newListThemesResponse,

    -- ** ListUserGroups
    ListUserGroups (ListUserGroups'),
    newListUserGroups,
    ListUserGroupsResponse (ListUserGroupsResponse'),
    newListUserGroupsResponse,

    -- ** ListUsers
    ListUsers (ListUsers'),
    newListUsers,
    ListUsersResponse (ListUsersResponse'),
    newListUsersResponse,

    -- ** RegisterUser
    RegisterUser (RegisterUser'),
    newRegisterUser,
    RegisterUserResponse (RegisterUserResponse'),
    newRegisterUserResponse,

    -- ** RestoreAnalysis
    RestoreAnalysis (RestoreAnalysis'),
    newRestoreAnalysis,
    RestoreAnalysisResponse (RestoreAnalysisResponse'),
    newRestoreAnalysisResponse,

    -- ** SearchAnalyses (Paginated)
    SearchAnalyses (SearchAnalyses'),
    newSearchAnalyses,
    SearchAnalysesResponse (SearchAnalysesResponse'),
    newSearchAnalysesResponse,

    -- ** SearchDashboards (Paginated)
    SearchDashboards (SearchDashboards'),
    newSearchDashboards,
    SearchDashboardsResponse (SearchDashboardsResponse'),
    newSearchDashboardsResponse,

    -- ** SearchDataSets (Paginated)
    SearchDataSets (SearchDataSets'),
    newSearchDataSets,
    SearchDataSetsResponse (SearchDataSetsResponse'),
    newSearchDataSetsResponse,

    -- ** SearchDataSources (Paginated)
    SearchDataSources (SearchDataSources'),
    newSearchDataSources,
    SearchDataSourcesResponse (SearchDataSourcesResponse'),
    newSearchDataSourcesResponse,

    -- ** SearchFolders
    SearchFolders (SearchFolders'),
    newSearchFolders,
    SearchFoldersResponse (SearchFoldersResponse'),
    newSearchFoldersResponse,

    -- ** SearchGroups
    SearchGroups (SearchGroups'),
    newSearchGroups,
    SearchGroupsResponse (SearchGroupsResponse'),
    newSearchGroupsResponse,

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

    -- ** UpdateAccountCustomization
    UpdateAccountCustomization (UpdateAccountCustomization'),
    newUpdateAccountCustomization,
    UpdateAccountCustomizationResponse (UpdateAccountCustomizationResponse'),
    newUpdateAccountCustomizationResponse,

    -- ** UpdateAccountSettings
    UpdateAccountSettings (UpdateAccountSettings'),
    newUpdateAccountSettings,
    UpdateAccountSettingsResponse (UpdateAccountSettingsResponse'),
    newUpdateAccountSettingsResponse,

    -- ** UpdateAnalysis
    UpdateAnalysis (UpdateAnalysis'),
    newUpdateAnalysis,
    UpdateAnalysisResponse (UpdateAnalysisResponse'),
    newUpdateAnalysisResponse,

    -- ** UpdateAnalysisPermissions
    UpdateAnalysisPermissions (UpdateAnalysisPermissions'),
    newUpdateAnalysisPermissions,
    UpdateAnalysisPermissionsResponse (UpdateAnalysisPermissionsResponse'),
    newUpdateAnalysisPermissionsResponse,

    -- ** UpdateDashboard
    UpdateDashboard (UpdateDashboard'),
    newUpdateDashboard,
    UpdateDashboardResponse (UpdateDashboardResponse'),
    newUpdateDashboardResponse,

    -- ** UpdateDashboardPermissions
    UpdateDashboardPermissions (UpdateDashboardPermissions'),
    newUpdateDashboardPermissions,
    UpdateDashboardPermissionsResponse (UpdateDashboardPermissionsResponse'),
    newUpdateDashboardPermissionsResponse,

    -- ** UpdateDashboardPublishedVersion
    UpdateDashboardPublishedVersion (UpdateDashboardPublishedVersion'),
    newUpdateDashboardPublishedVersion,
    UpdateDashboardPublishedVersionResponse (UpdateDashboardPublishedVersionResponse'),
    newUpdateDashboardPublishedVersionResponse,

    -- ** UpdateDataSet
    UpdateDataSet (UpdateDataSet'),
    newUpdateDataSet,
    UpdateDataSetResponse (UpdateDataSetResponse'),
    newUpdateDataSetResponse,

    -- ** UpdateDataSetPermissions
    UpdateDataSetPermissions (UpdateDataSetPermissions'),
    newUpdateDataSetPermissions,
    UpdateDataSetPermissionsResponse (UpdateDataSetPermissionsResponse'),
    newUpdateDataSetPermissionsResponse,

    -- ** UpdateDataSource
    UpdateDataSource (UpdateDataSource'),
    newUpdateDataSource,
    UpdateDataSourceResponse (UpdateDataSourceResponse'),
    newUpdateDataSourceResponse,

    -- ** UpdateDataSourcePermissions
    UpdateDataSourcePermissions (UpdateDataSourcePermissions'),
    newUpdateDataSourcePermissions,
    UpdateDataSourcePermissionsResponse (UpdateDataSourcePermissionsResponse'),
    newUpdateDataSourcePermissionsResponse,

    -- ** UpdateFolder
    UpdateFolder (UpdateFolder'),
    newUpdateFolder,
    UpdateFolderResponse (UpdateFolderResponse'),
    newUpdateFolderResponse,

    -- ** UpdateFolderPermissions
    UpdateFolderPermissions (UpdateFolderPermissions'),
    newUpdateFolderPermissions,
    UpdateFolderPermissionsResponse (UpdateFolderPermissionsResponse'),
    newUpdateFolderPermissionsResponse,

    -- ** UpdateGroup
    UpdateGroup (UpdateGroup'),
    newUpdateGroup,
    UpdateGroupResponse (UpdateGroupResponse'),
    newUpdateGroupResponse,

    -- ** UpdateIAMPolicyAssignment
    UpdateIAMPolicyAssignment (UpdateIAMPolicyAssignment'),
    newUpdateIAMPolicyAssignment,
    UpdateIAMPolicyAssignmentResponse (UpdateIAMPolicyAssignmentResponse'),
    newUpdateIAMPolicyAssignmentResponse,

    -- ** UpdateIpRestriction
    UpdateIpRestriction (UpdateIpRestriction'),
    newUpdateIpRestriction,
    UpdateIpRestrictionResponse (UpdateIpRestrictionResponse'),
    newUpdateIpRestrictionResponse,

    -- ** UpdatePublicSharingSettings
    UpdatePublicSharingSettings (UpdatePublicSharingSettings'),
    newUpdatePublicSharingSettings,
    UpdatePublicSharingSettingsResponse (UpdatePublicSharingSettingsResponse'),
    newUpdatePublicSharingSettingsResponse,

    -- ** UpdateTemplate
    UpdateTemplate (UpdateTemplate'),
    newUpdateTemplate,
    UpdateTemplateResponse (UpdateTemplateResponse'),
    newUpdateTemplateResponse,

    -- ** UpdateTemplateAlias
    UpdateTemplateAlias (UpdateTemplateAlias'),
    newUpdateTemplateAlias,
    UpdateTemplateAliasResponse (UpdateTemplateAliasResponse'),
    newUpdateTemplateAliasResponse,

    -- ** UpdateTemplatePermissions
    UpdateTemplatePermissions (UpdateTemplatePermissions'),
    newUpdateTemplatePermissions,
    UpdateTemplatePermissionsResponse (UpdateTemplatePermissionsResponse'),
    newUpdateTemplatePermissionsResponse,

    -- ** UpdateTheme
    UpdateTheme (UpdateTheme'),
    newUpdateTheme,
    UpdateThemeResponse (UpdateThemeResponse'),
    newUpdateThemeResponse,

    -- ** UpdateThemeAlias
    UpdateThemeAlias (UpdateThemeAlias'),
    newUpdateThemeAlias,
    UpdateThemeAliasResponse (UpdateThemeAliasResponse'),
    newUpdateThemeAliasResponse,

    -- ** UpdateThemePermissions
    UpdateThemePermissions (UpdateThemePermissions'),
    newUpdateThemePermissions,
    UpdateThemePermissionsResponse (UpdateThemePermissionsResponse'),
    newUpdateThemePermissionsResponse,

    -- ** UpdateUser
    UpdateUser (UpdateUser'),
    newUpdateUser,
    UpdateUserResponse (UpdateUserResponse'),
    newUpdateUserResponse,

    -- * Types

    -- ** AnalysisErrorType
    AnalysisErrorType (..),

    -- ** AnalysisFilterAttribute
    AnalysisFilterAttribute (..),

    -- ** AssignmentStatus
    AssignmentStatus (..),

    -- ** AuthenticationMethodOption
    AuthenticationMethodOption (..),

    -- ** ColumnDataType
    ColumnDataType (..),

    -- ** ColumnTagName
    ColumnTagName (..),

    -- ** DashboardBehavior
    DashboardBehavior (..),

    -- ** DashboardErrorType
    DashboardErrorType (..),

    -- ** DashboardFilterAttribute
    DashboardFilterAttribute (..),

    -- ** DashboardUIState
    DashboardUIState (..),

    -- ** DataSetFilterAttribute
    DataSetFilterAttribute (..),

    -- ** DataSetImportMode
    DataSetImportMode (..),

    -- ** DataSourceErrorInfoType
    DataSourceErrorInfoType (..),

    -- ** DataSourceFilterAttribute
    DataSourceFilterAttribute (..),

    -- ** DataSourceType
    DataSourceType (..),

    -- ** Edition
    Edition (..),

    -- ** EmbeddingIdentityType
    EmbeddingIdentityType (..),

    -- ** FileFormat
    FileFormat (..),

    -- ** FilterOperator
    FilterOperator (..),

    -- ** FolderFilterAttribute
    FolderFilterAttribute (..),

    -- ** FolderType
    FolderType (..),

    -- ** GeoSpatialCountryCode
    GeoSpatialCountryCode (..),

    -- ** GeoSpatialDataRole
    GeoSpatialDataRole (..),

    -- ** GroupFilterAttribute
    GroupFilterAttribute (..),

    -- ** GroupFilterOperator
    GroupFilterOperator (..),

    -- ** IdentityStore
    IdentityStore (..),

    -- ** IdentityType
    IdentityType (..),

    -- ** IngestionErrorType
    IngestionErrorType (..),

    -- ** IngestionRequestSource
    IngestionRequestSource (..),

    -- ** IngestionRequestType
    IngestionRequestType (..),

    -- ** IngestionStatus
    IngestionStatus (..),

    -- ** IngestionType
    IngestionType (..),

    -- ** InputColumnDataType
    InputColumnDataType (..),

    -- ** JoinType
    JoinType (..),

    -- ** MemberType
    MemberType (..),

    -- ** NamespaceErrorType
    NamespaceErrorType (..),

    -- ** NamespaceStatus
    NamespaceStatus (..),

    -- ** ResourceStatus
    ResourceStatus (..),

    -- ** RowLevelPermissionFormatVersion
    RowLevelPermissionFormatVersion (..),

    -- ** RowLevelPermissionPolicy
    RowLevelPermissionPolicy (..),

    -- ** Status
    Status (..),

    -- ** TemplateErrorType
    TemplateErrorType (..),

    -- ** TextQualifier
    TextQualifier (..),

    -- ** ThemeErrorType
    ThemeErrorType (..),

    -- ** ThemeType
    ThemeType (..),

    -- ** UserRole
    UserRole (..),

    -- ** AccountCustomization
    AccountCustomization (AccountCustomization'),
    newAccountCustomization,

    -- ** AccountInfo
    AccountInfo (AccountInfo'),
    newAccountInfo,

    -- ** AccountSettings
    AccountSettings (AccountSettings'),
    newAccountSettings,

    -- ** ActiveIAMPolicyAssignment
    ActiveIAMPolicyAssignment (ActiveIAMPolicyAssignment'),
    newActiveIAMPolicyAssignment,

    -- ** AdHocFilteringOption
    AdHocFilteringOption (AdHocFilteringOption'),
    newAdHocFilteringOption,

    -- ** AmazonElasticsearchParameters
    AmazonElasticsearchParameters (AmazonElasticsearchParameters'),
    newAmazonElasticsearchParameters,

    -- ** AmazonOpenSearchParameters
    AmazonOpenSearchParameters (AmazonOpenSearchParameters'),
    newAmazonOpenSearchParameters,

    -- ** Analysis
    Analysis (Analysis'),
    newAnalysis,

    -- ** AnalysisError
    AnalysisError (AnalysisError'),
    newAnalysisError,

    -- ** AnalysisSearchFilter
    AnalysisSearchFilter (AnalysisSearchFilter'),
    newAnalysisSearchFilter,

    -- ** AnalysisSourceEntity
    AnalysisSourceEntity (AnalysisSourceEntity'),
    newAnalysisSourceEntity,

    -- ** AnalysisSourceTemplate
    AnalysisSourceTemplate (AnalysisSourceTemplate'),
    newAnalysisSourceTemplate,

    -- ** AnalysisSummary
    AnalysisSummary (AnalysisSummary'),
    newAnalysisSummary,

    -- ** AnonymousUserDashboardEmbeddingConfiguration
    AnonymousUserDashboardEmbeddingConfiguration (AnonymousUserDashboardEmbeddingConfiguration'),
    newAnonymousUserDashboardEmbeddingConfiguration,

    -- ** AnonymousUserDashboardVisualEmbeddingConfiguration
    AnonymousUserDashboardVisualEmbeddingConfiguration (AnonymousUserDashboardVisualEmbeddingConfiguration'),
    newAnonymousUserDashboardVisualEmbeddingConfiguration,

    -- ** AnonymousUserEmbeddingExperienceConfiguration
    AnonymousUserEmbeddingExperienceConfiguration (AnonymousUserEmbeddingExperienceConfiguration'),
    newAnonymousUserEmbeddingExperienceConfiguration,

    -- ** AnonymousUserQSearchBarEmbeddingConfiguration
    AnonymousUserQSearchBarEmbeddingConfiguration (AnonymousUserQSearchBarEmbeddingConfiguration'),
    newAnonymousUserQSearchBarEmbeddingConfiguration,

    -- ** AthenaParameters
    AthenaParameters (AthenaParameters'),
    newAthenaParameters,

    -- ** AuroraParameters
    AuroraParameters (AuroraParameters'),
    newAuroraParameters,

    -- ** AuroraPostgreSqlParameters
    AuroraPostgreSqlParameters (AuroraPostgreSqlParameters'),
    newAuroraPostgreSqlParameters,

    -- ** AwsIotAnalyticsParameters
    AwsIotAnalyticsParameters (AwsIotAnalyticsParameters'),
    newAwsIotAnalyticsParameters,

    -- ** BorderStyle
    BorderStyle (BorderStyle'),
    newBorderStyle,

    -- ** CalculatedColumn
    CalculatedColumn (CalculatedColumn'),
    newCalculatedColumn,

    -- ** CastColumnTypeOperation
    CastColumnTypeOperation (CastColumnTypeOperation'),
    newCastColumnTypeOperation,

    -- ** ColumnDescription
    ColumnDescription (ColumnDescription'),
    newColumnDescription,

    -- ** ColumnGroup
    ColumnGroup (ColumnGroup'),
    newColumnGroup,

    -- ** ColumnGroupColumnSchema
    ColumnGroupColumnSchema (ColumnGroupColumnSchema'),
    newColumnGroupColumnSchema,

    -- ** ColumnGroupSchema
    ColumnGroupSchema (ColumnGroupSchema'),
    newColumnGroupSchema,

    -- ** ColumnLevelPermissionRule
    ColumnLevelPermissionRule (ColumnLevelPermissionRule'),
    newColumnLevelPermissionRule,

    -- ** ColumnSchema
    ColumnSchema (ColumnSchema'),
    newColumnSchema,

    -- ** ColumnTag
    ColumnTag (ColumnTag'),
    newColumnTag,

    -- ** CreateColumnsOperation
    CreateColumnsOperation (CreateColumnsOperation'),
    newCreateColumnsOperation,

    -- ** CredentialPair
    CredentialPair (CredentialPair'),
    newCredentialPair,

    -- ** CustomSql
    CustomSql (CustomSql'),
    newCustomSql,

    -- ** Dashboard
    Dashboard (Dashboard'),
    newDashboard,

    -- ** DashboardError
    DashboardError (DashboardError'),
    newDashboardError,

    -- ** DashboardPublishOptions
    DashboardPublishOptions (DashboardPublishOptions'),
    newDashboardPublishOptions,

    -- ** DashboardSearchFilter
    DashboardSearchFilter (DashboardSearchFilter'),
    newDashboardSearchFilter,

    -- ** DashboardSourceEntity
    DashboardSourceEntity (DashboardSourceEntity'),
    newDashboardSourceEntity,

    -- ** DashboardSourceTemplate
    DashboardSourceTemplate (DashboardSourceTemplate'),
    newDashboardSourceTemplate,

    -- ** DashboardSummary
    DashboardSummary (DashboardSummary'),
    newDashboardSummary,

    -- ** DashboardVersion
    DashboardVersion (DashboardVersion'),
    newDashboardVersion,

    -- ** DashboardVersionSummary
    DashboardVersionSummary (DashboardVersionSummary'),
    newDashboardVersionSummary,

    -- ** DashboardVisualId
    DashboardVisualId (DashboardVisualId'),
    newDashboardVisualId,

    -- ** DataColorPalette
    DataColorPalette (DataColorPalette'),
    newDataColorPalette,

    -- ** DataSet
    DataSet (DataSet'),
    newDataSet,

    -- ** DataSetConfiguration
    DataSetConfiguration (DataSetConfiguration'),
    newDataSetConfiguration,

    -- ** DataSetReference
    DataSetReference (DataSetReference'),
    newDataSetReference,

    -- ** DataSetSchema
    DataSetSchema (DataSetSchema'),
    newDataSetSchema,

    -- ** DataSetSearchFilter
    DataSetSearchFilter (DataSetSearchFilter'),
    newDataSetSearchFilter,

    -- ** DataSetSummary
    DataSetSummary (DataSetSummary'),
    newDataSetSummary,

    -- ** DataSetUsageConfiguration
    DataSetUsageConfiguration (DataSetUsageConfiguration'),
    newDataSetUsageConfiguration,

    -- ** DataSource
    DataSource (DataSource'),
    newDataSource,

    -- ** DataSourceCredentials
    DataSourceCredentials (DataSourceCredentials'),
    newDataSourceCredentials,

    -- ** DataSourceErrorInfo
    DataSourceErrorInfo (DataSourceErrorInfo'),
    newDataSourceErrorInfo,

    -- ** DataSourceParameters
    DataSourceParameters (DataSourceParameters'),
    newDataSourceParameters,

    -- ** DataSourceSearchFilter
    DataSourceSearchFilter (DataSourceSearchFilter'),
    newDataSourceSearchFilter,

    -- ** DataSourceSummary
    DataSourceSummary (DataSourceSummary'),
    newDataSourceSummary,

    -- ** DatabricksParameters
    DatabricksParameters (DatabricksParameters'),
    newDatabricksParameters,

    -- ** DateTimeParameter
    DateTimeParameter (DateTimeParameter'),
    newDateTimeParameter,

    -- ** DecimalParameter
    DecimalParameter (DecimalParameter'),
    newDecimalParameter,

    -- ** ErrorInfo
    ErrorInfo (ErrorInfo'),
    newErrorInfo,

    -- ** ExasolParameters
    ExasolParameters (ExasolParameters'),
    newExasolParameters,

    -- ** ExportToCSVOption
    ExportToCSVOption (ExportToCSVOption'),
    newExportToCSVOption,

    -- ** FieldFolder
    FieldFolder (FieldFolder'),
    newFieldFolder,

    -- ** FilterOperation
    FilterOperation (FilterOperation'),
    newFilterOperation,

    -- ** Folder
    Folder (Folder'),
    newFolder,

    -- ** FolderMember
    FolderMember (FolderMember'),
    newFolderMember,

    -- ** FolderSearchFilter
    FolderSearchFilter (FolderSearchFilter'),
    newFolderSearchFilter,

    -- ** FolderSummary
    FolderSummary (FolderSummary'),
    newFolderSummary,

    -- ** GeoSpatialColumnGroup
    GeoSpatialColumnGroup (GeoSpatialColumnGroup'),
    newGeoSpatialColumnGroup,

    -- ** Group
    Group (Group'),
    newGroup,

    -- ** GroupMember
    GroupMember (GroupMember'),
    newGroupMember,

    -- ** GroupSearchFilter
    GroupSearchFilter (GroupSearchFilter'),
    newGroupSearchFilter,

    -- ** GutterStyle
    GutterStyle (GutterStyle'),
    newGutterStyle,

    -- ** IAMPolicyAssignment
    IAMPolicyAssignment (IAMPolicyAssignment'),
    newIAMPolicyAssignment,

    -- ** IAMPolicyAssignmentSummary
    IAMPolicyAssignmentSummary (IAMPolicyAssignmentSummary'),
    newIAMPolicyAssignmentSummary,

    -- ** Ingestion
    Ingestion (Ingestion'),
    newIngestion,

    -- ** InputColumn
    InputColumn (InputColumn'),
    newInputColumn,

    -- ** IntegerParameter
    IntegerParameter (IntegerParameter'),
    newIntegerParameter,

    -- ** JiraParameters
    JiraParameters (JiraParameters'),
    newJiraParameters,

    -- ** JoinInstruction
    JoinInstruction (JoinInstruction'),
    newJoinInstruction,

    -- ** JoinKeyProperties
    JoinKeyProperties (JoinKeyProperties'),
    newJoinKeyProperties,

    -- ** LinkSharingConfiguration
    LinkSharingConfiguration (LinkSharingConfiguration'),
    newLinkSharingConfiguration,

    -- ** LogicalTable
    LogicalTable (LogicalTable'),
    newLogicalTable,

    -- ** LogicalTableSource
    LogicalTableSource (LogicalTableSource'),
    newLogicalTableSource,

    -- ** ManifestFileLocation
    ManifestFileLocation (ManifestFileLocation'),
    newManifestFileLocation,

    -- ** MarginStyle
    MarginStyle (MarginStyle'),
    newMarginStyle,

    -- ** MariaDbParameters
    MariaDbParameters (MariaDbParameters'),
    newMariaDbParameters,

    -- ** MemberIdArnPair
    MemberIdArnPair (MemberIdArnPair'),
    newMemberIdArnPair,

    -- ** MySqlParameters
    MySqlParameters (MySqlParameters'),
    newMySqlParameters,

    -- ** NamespaceError
    NamespaceError (NamespaceError'),
    newNamespaceError,

    -- ** NamespaceInfoV2
    NamespaceInfoV2 (NamespaceInfoV2'),
    newNamespaceInfoV2,

    -- ** OracleParameters
    OracleParameters (OracleParameters'),
    newOracleParameters,

    -- ** OutputColumn
    OutputColumn (OutputColumn'),
    newOutputColumn,

    -- ** Parameters
    Parameters (Parameters'),
    newParameters,

    -- ** PhysicalTable
    PhysicalTable (PhysicalTable'),
    newPhysicalTable,

    -- ** PostgreSqlParameters
    PostgreSqlParameters (PostgreSqlParameters'),
    newPostgreSqlParameters,

    -- ** PrestoParameters
    PrestoParameters (PrestoParameters'),
    newPrestoParameters,

    -- ** ProjectOperation
    ProjectOperation (ProjectOperation'),
    newProjectOperation,

    -- ** QueueInfo
    QueueInfo (QueueInfo'),
    newQueueInfo,

    -- ** RdsParameters
    RdsParameters (RdsParameters'),
    newRdsParameters,

    -- ** RedshiftParameters
    RedshiftParameters (RedshiftParameters'),
    newRedshiftParameters,

    -- ** RegisteredUserDashboardEmbeddingConfiguration
    RegisteredUserDashboardEmbeddingConfiguration (RegisteredUserDashboardEmbeddingConfiguration'),
    newRegisteredUserDashboardEmbeddingConfiguration,

    -- ** RegisteredUserDashboardVisualEmbeddingConfiguration
    RegisteredUserDashboardVisualEmbeddingConfiguration (RegisteredUserDashboardVisualEmbeddingConfiguration'),
    newRegisteredUserDashboardVisualEmbeddingConfiguration,

    -- ** RegisteredUserEmbeddingExperienceConfiguration
    RegisteredUserEmbeddingExperienceConfiguration (RegisteredUserEmbeddingExperienceConfiguration'),
    newRegisteredUserEmbeddingExperienceConfiguration,

    -- ** RegisteredUserQSearchBarEmbeddingConfiguration
    RegisteredUserQSearchBarEmbeddingConfiguration (RegisteredUserQSearchBarEmbeddingConfiguration'),
    newRegisteredUserQSearchBarEmbeddingConfiguration,

    -- ** RegisteredUserQuickSightConsoleEmbeddingConfiguration
    RegisteredUserQuickSightConsoleEmbeddingConfiguration (RegisteredUserQuickSightConsoleEmbeddingConfiguration'),
    newRegisteredUserQuickSightConsoleEmbeddingConfiguration,

    -- ** RelationalTable
    RelationalTable (RelationalTable'),
    newRelationalTable,

    -- ** RenameColumnOperation
    RenameColumnOperation (RenameColumnOperation'),
    newRenameColumnOperation,

    -- ** ResourcePermission
    ResourcePermission (ResourcePermission'),
    newResourcePermission,

    -- ** RowInfo
    RowInfo (RowInfo'),
    newRowInfo,

    -- ** RowLevelPermissionDataSet
    RowLevelPermissionDataSet (RowLevelPermissionDataSet'),
    newRowLevelPermissionDataSet,

    -- ** RowLevelPermissionTagConfiguration
    RowLevelPermissionTagConfiguration (RowLevelPermissionTagConfiguration'),
    newRowLevelPermissionTagConfiguration,

    -- ** RowLevelPermissionTagRule
    RowLevelPermissionTagRule (RowLevelPermissionTagRule'),
    newRowLevelPermissionTagRule,

    -- ** S3Parameters
    S3Parameters (S3Parameters'),
    newS3Parameters,

    -- ** S3Source
    S3Source (S3Source'),
    newS3Source,

    -- ** ServiceNowParameters
    ServiceNowParameters (ServiceNowParameters'),
    newServiceNowParameters,

    -- ** SessionTag
    SessionTag (SessionTag'),
    newSessionTag,

    -- ** Sheet
    Sheet (Sheet'),
    newSheet,

    -- ** SheetControlsOption
    SheetControlsOption (SheetControlsOption'),
    newSheetControlsOption,

    -- ** SheetStyle
    SheetStyle (SheetStyle'),
    newSheetStyle,

    -- ** SignupResponse
    SignupResponse (SignupResponse'),
    newSignupResponse,

    -- ** SnowflakeParameters
    SnowflakeParameters (SnowflakeParameters'),
    newSnowflakeParameters,

    -- ** SparkParameters
    SparkParameters (SparkParameters'),
    newSparkParameters,

    -- ** SqlServerParameters
    SqlServerParameters (SqlServerParameters'),
    newSqlServerParameters,

    -- ** SslProperties
    SslProperties (SslProperties'),
    newSslProperties,

    -- ** StringParameter
    StringParameter (StringParameter'),
    newStringParameter,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** TagColumnOperation
    TagColumnOperation (TagColumnOperation'),
    newTagColumnOperation,

    -- ** Template
    Template (Template'),
    newTemplate,

    -- ** TemplateAlias
    TemplateAlias (TemplateAlias'),
    newTemplateAlias,

    -- ** TemplateError
    TemplateError (TemplateError'),
    newTemplateError,

    -- ** TemplateSourceAnalysis
    TemplateSourceAnalysis (TemplateSourceAnalysis'),
    newTemplateSourceAnalysis,

    -- ** TemplateSourceEntity
    TemplateSourceEntity (TemplateSourceEntity'),
    newTemplateSourceEntity,

    -- ** TemplateSourceTemplate
    TemplateSourceTemplate (TemplateSourceTemplate'),
    newTemplateSourceTemplate,

    -- ** TemplateSummary
    TemplateSummary (TemplateSummary'),
    newTemplateSummary,

    -- ** TemplateVersion
    TemplateVersion (TemplateVersion'),
    newTemplateVersion,

    -- ** TemplateVersionSummary
    TemplateVersionSummary (TemplateVersionSummary'),
    newTemplateVersionSummary,

    -- ** TeradataParameters
    TeradataParameters (TeradataParameters'),
    newTeradataParameters,

    -- ** Theme
    Theme (Theme'),
    newTheme,

    -- ** ThemeAlias
    ThemeAlias (ThemeAlias'),
    newThemeAlias,

    -- ** ThemeConfiguration
    ThemeConfiguration (ThemeConfiguration'),
    newThemeConfiguration,

    -- ** ThemeError
    ThemeError (ThemeError'),
    newThemeError,

    -- ** ThemeSummary
    ThemeSummary (ThemeSummary'),
    newThemeSummary,

    -- ** ThemeVersion
    ThemeVersion (ThemeVersion'),
    newThemeVersion,

    -- ** ThemeVersionSummary
    ThemeVersionSummary (ThemeVersionSummary'),
    newThemeVersionSummary,

    -- ** TileLayoutStyle
    TileLayoutStyle (TileLayoutStyle'),
    newTileLayoutStyle,

    -- ** TileStyle
    TileStyle (TileStyle'),
    newTileStyle,

    -- ** TransformOperation
    TransformOperation (TransformOperation'),
    newTransformOperation,

    -- ** TwitterParameters
    TwitterParameters (TwitterParameters'),
    newTwitterParameters,

    -- ** UIColorPalette
    UIColorPalette (UIColorPalette'),
    newUIColorPalette,

    -- ** UntagColumnOperation
    UntagColumnOperation (UntagColumnOperation'),
    newUntagColumnOperation,

    -- ** UploadSettings
    UploadSettings (UploadSettings'),
    newUploadSettings,

    -- ** User
    User (User'),
    newUser,

    -- ** VpcConnectionProperties
    VpcConnectionProperties (VpcConnectionProperties'),
    newVpcConnectionProperties,
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
import Amazonka.QuickSight.Lens
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
import Amazonka.QuickSight.Types
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
import Amazonka.QuickSight.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'QuickSight'.

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
