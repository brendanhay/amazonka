{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.QuickSight
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConcurrentUpdatingException
    _ConcurrentUpdatingException,

    -- ** ConflictException
    _ConflictException,

    -- ** DomainNotWhitelistedException
    _DomainNotWhitelistedException,

    -- ** IdentityTypeNotSupportedException
    _IdentityTypeNotSupportedException,

    -- ** InternalFailureException
    _InternalFailureException,

    -- ** InvalidNextTokenException
    _InvalidNextTokenException,

    -- ** InvalidParameterValueException
    _InvalidParameterValueException,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** PreconditionNotMetException
    _PreconditionNotMetException,

    -- ** QuickSightUserNotFoundException
    _QuickSightUserNotFoundException,

    -- ** ResourceExistsException
    _ResourceExistsException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ResourceUnavailableException
    _ResourceUnavailableException,

    -- ** SessionLifetimeInMinutesInvalidException
    _SessionLifetimeInMinutesInvalidException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** UnsupportedPricingPlanException
    _UnsupportedPricingPlanException,

    -- ** UnsupportedUserEditionException
    _UnsupportedUserEditionException,

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

    -- ** DescribeAnalysisDefinition
    DescribeAnalysisDefinition (DescribeAnalysisDefinition'),
    newDescribeAnalysisDefinition,
    DescribeAnalysisDefinitionResponse (DescribeAnalysisDefinitionResponse'),
    newDescribeAnalysisDefinitionResponse,

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

    -- ** DescribeDashboardDefinition
    DescribeDashboardDefinition (DescribeDashboardDefinition'),
    newDescribeDashboardDefinition,
    DescribeDashboardDefinitionResponse (DescribeDashboardDefinitionResponse'),
    newDescribeDashboardDefinitionResponse,

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

    -- ** DescribeTemplateDefinition
    DescribeTemplateDefinition (DescribeTemplateDefinition'),
    newDescribeTemplateDefinition,
    DescribeTemplateDefinitionResponse (DescribeTemplateDefinitionResponse'),
    newDescribeTemplateDefinitionResponse,

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

    -- ** AnchorOption
    AnchorOption (..),

    -- ** ArcThickness
    ArcThickness (..),

    -- ** ArcThicknessOptions
    ArcThicknessOptions (..),

    -- ** AssignmentStatus
    AssignmentStatus (..),

    -- ** AuthenticationMethodOption
    AuthenticationMethodOption (..),

    -- ** AxisBinding
    AxisBinding (..),

    -- ** BarChartOrientation
    BarChartOrientation (..),

    -- ** BarsArrangement
    BarsArrangement (..),

    -- ** BaseMapStyleType
    BaseMapStyleType (..),

    -- ** BoxPlotFillStyle
    BoxPlotFillStyle (..),

    -- ** CategoricalAggregationFunction
    CategoricalAggregationFunction (..),

    -- ** CategoryFilterMatchOperator
    CategoryFilterMatchOperator (..),

    -- ** CategoryFilterSelectAllOptions
    CategoryFilterSelectAllOptions (..),

    -- ** ColorFillType
    ColorFillType (..),

    -- ** ColumnDataType
    ColumnDataType (..),

    -- ** ColumnRole
    ColumnRole (..),

    -- ** ColumnTagName
    ColumnTagName (..),

    -- ** ComparisonMethod
    ComparisonMethod (..),

    -- ** ConditionalFormattingIconDisplayOption
    ConditionalFormattingIconDisplayOption (..),

    -- ** ConditionalFormattingIconSetType
    ConditionalFormattingIconSetType (..),

    -- ** CrossDatasetTypes
    CrossDatasetTypes (..),

    -- ** CustomContentImageScalingConfiguration
    CustomContentImageScalingConfiguration (..),

    -- ** CustomContentType
    CustomContentType (..),

    -- ** DashboardBehavior
    DashboardBehavior (..),

    -- ** DashboardErrorType
    DashboardErrorType (..),

    -- ** DashboardFilterAttribute
    DashboardFilterAttribute (..),

    -- ** DashboardUIState
    DashboardUIState (..),

    -- ** DataLabelContent
    DataLabelContent (..),

    -- ** DataLabelOverlap
    DataLabelOverlap (..),

    -- ** DataLabelPosition
    DataLabelPosition (..),

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

    -- ** DateAggregationFunction
    DateAggregationFunction (..),

    -- ** Edition
    Edition (..),

    -- ** EmbeddingIdentityType
    EmbeddingIdentityType (..),

    -- ** FileFormat
    FileFormat (..),

    -- ** FilterNullOption
    FilterNullOption (..),

    -- ** FilterOperator
    FilterOperator (..),

    -- ** FilterVisualScope
    FilterVisualScope (..),

    -- ** FolderFilterAttribute
    FolderFilterAttribute (..),

    -- ** FolderType
    FolderType (..),

    -- ** FontDecoration
    FontDecoration (..),

    -- ** FontStyle
    FontStyle (..),

    -- ** FontWeightName
    FontWeightName (..),

    -- ** ForecastComputationSeasonality
    ForecastComputationSeasonality (..),

    -- ** FunnelChartMeasureDataLabelStyle
    FunnelChartMeasureDataLabelStyle (..),

    -- ** GeoSpatialCountryCode
    GeoSpatialCountryCode (..),

    -- ** GeoSpatialDataRole
    GeoSpatialDataRole (..),

    -- ** GeospatialSelectedPointStyle
    GeospatialSelectedPointStyle (..),

    -- ** GroupFilterAttribute
    GroupFilterAttribute (..),

    -- ** GroupFilterOperator
    GroupFilterOperator (..),

    -- ** HistogramBinType
    HistogramBinType (..),

    -- ** HorizontalTextAlignment
    HorizontalTextAlignment (..),

    -- ** Icon
    Icon (..),

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

    -- ** LayoutElementType
    LayoutElementType (..),

    -- ** LegendPosition
    LegendPosition (..),

    -- ** LineChartLineStyle
    LineChartLineStyle (..),

    -- ** LineChartMarkerShape
    LineChartMarkerShape (..),

    -- ** LineChartType
    LineChartType (..),

    -- ** LineInterpolation
    LineInterpolation (..),

    -- ** MapZoomMode
    MapZoomMode (..),

    -- ** MaximumMinimumComputationType
    MaximumMinimumComputationType (..),

    -- ** MemberType
    MemberType (..),

    -- ** MissingDataTreatmentOption
    MissingDataTreatmentOption (..),

    -- ** NamespaceErrorType
    NamespaceErrorType (..),

    -- ** NamespaceStatus
    NamespaceStatus (..),

    -- ** NegativeValueDisplayMode
    NegativeValueDisplayMode (..),

    -- ** NumberScale
    NumberScale (..),

    -- ** NumericEqualityMatchOperator
    NumericEqualityMatchOperator (..),

    -- ** NumericFilterSelectAllOptions
    NumericFilterSelectAllOptions (..),

    -- ** NumericSeparatorSymbol
    NumericSeparatorSymbol (..),

    -- ** OtherCategories
    OtherCategories (..),

    -- ** PanelBorderStyle
    PanelBorderStyle (..),

    -- ** PaperOrientation
    PaperOrientation (..),

    -- ** PaperSize
    PaperSize (..),

    -- ** ParameterValueType
    ParameterValueType (..),

    -- ** PivotTableConditionalFormattingScopeRole
    PivotTableConditionalFormattingScopeRole (..),

    -- ** PivotTableMetricPlacement
    PivotTableMetricPlacement (..),

    -- ** PivotTableSubtotalLevel
    PivotTableSubtotalLevel (..),

    -- ** PrimaryValueDisplayType
    PrimaryValueDisplayType (..),

    -- ** ReferenceLineLabelHorizontalPosition
    ReferenceLineLabelHorizontalPosition (..),

    -- ** ReferenceLineLabelVerticalPosition
    ReferenceLineLabelVerticalPosition (..),

    -- ** ReferenceLinePatternType
    ReferenceLinePatternType (..),

    -- ** ReferenceLineValueLabelRelativePosition
    ReferenceLineValueLabelRelativePosition (..),

    -- ** RelativeDateType
    RelativeDateType (..),

    -- ** RelativeFontSize
    RelativeFontSize (..),

    -- ** ResizeOption
    ResizeOption (..),

    -- ** ResourceStatus
    ResourceStatus (..),

    -- ** RowLevelPermissionFormatVersion
    RowLevelPermissionFormatVersion (..),

    -- ** RowLevelPermissionPolicy
    RowLevelPermissionPolicy (..),

    -- ** SectionPageBreakStatus
    SectionPageBreakStatus (..),

    -- ** SelectAllValueOptions
    SelectAllValueOptions (..),

    -- ** SelectedFieldOptions
    SelectedFieldOptions (..),

    -- ** SelectedTooltipType
    SelectedTooltipType (..),

    -- ** SheetContentType
    SheetContentType (..),

    -- ** SheetControlDateTimePickerType
    SheetControlDateTimePickerType (..),

    -- ** SheetControlListType
    SheetControlListType (..),

    -- ** SheetControlSliderType
    SheetControlSliderType (..),

    -- ** SimpleNumericalAggregationFunction
    SimpleNumericalAggregationFunction (..),

    -- ** SortDirection
    SortDirection (..),

    -- ** Status
    Status (..),

    -- ** TableBorderStyle
    TableBorderStyle (..),

    -- ** TableCellImageScalingConfiguration
    TableCellImageScalingConfiguration (..),

    -- ** TableFieldIconSetType
    TableFieldIconSetType (..),

    -- ** TableOrientation
    TableOrientation (..),

    -- ** TableTotalsPlacement
    TableTotalsPlacement (..),

    -- ** TableTotalsScrollStatus
    TableTotalsScrollStatus (..),

    -- ** TargetVisualOptions
    TargetVisualOptions (..),

    -- ** TemplateErrorType
    TemplateErrorType (..),

    -- ** TextQualifier
    TextQualifier (..),

    -- ** TextWrap
    TextWrap (..),

    -- ** ThemeErrorType
    ThemeErrorType (..),

    -- ** ThemeType
    ThemeType (..),

    -- ** TimeGranularity
    TimeGranularity (..),

    -- ** TooltipTitleType
    TooltipTitleType (..),

    -- ** TopBottomComputationType
    TopBottomComputationType (..),

    -- ** TopBottomSortOrder
    TopBottomSortOrder (..),

    -- ** URLTargetConfiguration
    URLTargetConfiguration (..),

    -- ** UserRole
    UserRole (..),

    -- ** ValueWhenUnsetOption
    ValueWhenUnsetOption (..),

    -- ** VerticalTextAlignment
    VerticalTextAlignment (..),

    -- ** Visibility
    Visibility (..),

    -- ** VisualCustomActionTrigger
    VisualCustomActionTrigger (..),

    -- ** WidgetStatus
    WidgetStatus (..),

    -- ** WordCloudCloudLayout
    WordCloudCloudLayout (..),

    -- ** WordCloudWordCasing
    WordCloudWordCasing (..),

    -- ** WordCloudWordOrientation
    WordCloudWordOrientation (..),

    -- ** WordCloudWordPadding
    WordCloudWordPadding (..),

    -- ** WordCloudWordScaling
    WordCloudWordScaling (..),

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

    -- ** AggregationFunction
    AggregationFunction (AggregationFunction'),
    newAggregationFunction,

    -- ** AggregationSortConfiguration
    AggregationSortConfiguration (AggregationSortConfiguration'),
    newAggregationSortConfiguration,

    -- ** AmazonElasticsearchParameters
    AmazonElasticsearchParameters (AmazonElasticsearchParameters'),
    newAmazonElasticsearchParameters,

    -- ** AmazonOpenSearchParameters
    AmazonOpenSearchParameters (AmazonOpenSearchParameters'),
    newAmazonOpenSearchParameters,

    -- ** Analysis
    Analysis (Analysis'),
    newAnalysis,

    -- ** AnalysisDefaults
    AnalysisDefaults (AnalysisDefaults'),
    newAnalysisDefaults,

    -- ** AnalysisDefinition
    AnalysisDefinition (AnalysisDefinition'),
    newAnalysisDefinition,

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

    -- ** AnchorDateConfiguration
    AnchorDateConfiguration (AnchorDateConfiguration'),
    newAnchorDateConfiguration,

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

    -- ** ArcAxisConfiguration
    ArcAxisConfiguration (ArcAxisConfiguration'),
    newArcAxisConfiguration,

    -- ** ArcAxisDisplayRange
    ArcAxisDisplayRange (ArcAxisDisplayRange'),
    newArcAxisDisplayRange,

    -- ** ArcConfiguration
    ArcConfiguration (ArcConfiguration'),
    newArcConfiguration,

    -- ** ArcOptions
    ArcOptions (ArcOptions'),
    newArcOptions,

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

    -- ** AxisDataOptions
    AxisDataOptions (AxisDataOptions'),
    newAxisDataOptions,

    -- ** AxisDisplayDataDrivenRange
    AxisDisplayDataDrivenRange (AxisDisplayDataDrivenRange'),
    newAxisDisplayDataDrivenRange,

    -- ** AxisDisplayMinMaxRange
    AxisDisplayMinMaxRange (AxisDisplayMinMaxRange'),
    newAxisDisplayMinMaxRange,

    -- ** AxisDisplayOptions
    AxisDisplayOptions (AxisDisplayOptions'),
    newAxisDisplayOptions,

    -- ** AxisDisplayRange
    AxisDisplayRange (AxisDisplayRange'),
    newAxisDisplayRange,

    -- ** AxisLabelOptions
    AxisLabelOptions (AxisLabelOptions'),
    newAxisLabelOptions,

    -- ** AxisLabelReferenceOptions
    AxisLabelReferenceOptions (AxisLabelReferenceOptions'),
    newAxisLabelReferenceOptions,

    -- ** AxisLinearScale
    AxisLinearScale (AxisLinearScale'),
    newAxisLinearScale,

    -- ** AxisLogarithmicScale
    AxisLogarithmicScale (AxisLogarithmicScale'),
    newAxisLogarithmicScale,

    -- ** AxisScale
    AxisScale (AxisScale'),
    newAxisScale,

    -- ** AxisTickLabelOptions
    AxisTickLabelOptions (AxisTickLabelOptions'),
    newAxisTickLabelOptions,

    -- ** BarChartAggregatedFieldWells
    BarChartAggregatedFieldWells (BarChartAggregatedFieldWells'),
    newBarChartAggregatedFieldWells,

    -- ** BarChartConfiguration
    BarChartConfiguration (BarChartConfiguration'),
    newBarChartConfiguration,

    -- ** BarChartFieldWells
    BarChartFieldWells (BarChartFieldWells'),
    newBarChartFieldWells,

    -- ** BarChartSortConfiguration
    BarChartSortConfiguration (BarChartSortConfiguration'),
    newBarChartSortConfiguration,

    -- ** BarChartVisual
    BarChartVisual (BarChartVisual'),
    newBarChartVisual,

    -- ** BinCountOptions
    BinCountOptions (BinCountOptions'),
    newBinCountOptions,

    -- ** BinWidthOptions
    BinWidthOptions (BinWidthOptions'),
    newBinWidthOptions,

    -- ** BodySectionConfiguration
    BodySectionConfiguration (BodySectionConfiguration'),
    newBodySectionConfiguration,

    -- ** BodySectionContent
    BodySectionContent (BodySectionContent'),
    newBodySectionContent,

    -- ** BorderStyle
    BorderStyle (BorderStyle'),
    newBorderStyle,

    -- ** BoxPlotAggregatedFieldWells
    BoxPlotAggregatedFieldWells (BoxPlotAggregatedFieldWells'),
    newBoxPlotAggregatedFieldWells,

    -- ** BoxPlotChartConfiguration
    BoxPlotChartConfiguration (BoxPlotChartConfiguration'),
    newBoxPlotChartConfiguration,

    -- ** BoxPlotFieldWells
    BoxPlotFieldWells (BoxPlotFieldWells'),
    newBoxPlotFieldWells,

    -- ** BoxPlotOptions
    BoxPlotOptions (BoxPlotOptions'),
    newBoxPlotOptions,

    -- ** BoxPlotSortConfiguration
    BoxPlotSortConfiguration (BoxPlotSortConfiguration'),
    newBoxPlotSortConfiguration,

    -- ** BoxPlotStyleOptions
    BoxPlotStyleOptions (BoxPlotStyleOptions'),
    newBoxPlotStyleOptions,

    -- ** BoxPlotVisual
    BoxPlotVisual (BoxPlotVisual'),
    newBoxPlotVisual,

    -- ** CalculatedColumn
    CalculatedColumn (CalculatedColumn'),
    newCalculatedColumn,

    -- ** CalculatedField
    CalculatedField (CalculatedField'),
    newCalculatedField,

    -- ** CalculatedMeasureField
    CalculatedMeasureField (CalculatedMeasureField'),
    newCalculatedMeasureField,

    -- ** CascadingControlConfiguration
    CascadingControlConfiguration (CascadingControlConfiguration'),
    newCascadingControlConfiguration,

    -- ** CascadingControlSource
    CascadingControlSource (CascadingControlSource'),
    newCascadingControlSource,

    -- ** CastColumnTypeOperation
    CastColumnTypeOperation (CastColumnTypeOperation'),
    newCastColumnTypeOperation,

    -- ** CategoricalDimensionField
    CategoricalDimensionField (CategoricalDimensionField'),
    newCategoricalDimensionField,

    -- ** CategoricalMeasureField
    CategoricalMeasureField (CategoricalMeasureField'),
    newCategoricalMeasureField,

    -- ** CategoryDrillDownFilter
    CategoryDrillDownFilter (CategoryDrillDownFilter'),
    newCategoryDrillDownFilter,

    -- ** CategoryFilter
    CategoryFilter (CategoryFilter'),
    newCategoryFilter,

    -- ** CategoryFilterConfiguration
    CategoryFilterConfiguration (CategoryFilterConfiguration'),
    newCategoryFilterConfiguration,

    -- ** ChartAxisLabelOptions
    ChartAxisLabelOptions (ChartAxisLabelOptions'),
    newChartAxisLabelOptions,

    -- ** ClusterMarker
    ClusterMarker (ClusterMarker'),
    newClusterMarker,

    -- ** ClusterMarkerConfiguration
    ClusterMarkerConfiguration (ClusterMarkerConfiguration'),
    newClusterMarkerConfiguration,

    -- ** ColorScale
    ColorScale (ColorScale'),
    newColorScale,

    -- ** ColumnConfiguration
    ColumnConfiguration (ColumnConfiguration'),
    newColumnConfiguration,

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

    -- ** ColumnHierarchy
    ColumnHierarchy (ColumnHierarchy'),
    newColumnHierarchy,

    -- ** ColumnIdentifier
    ColumnIdentifier (ColumnIdentifier'),
    newColumnIdentifier,

    -- ** ColumnLevelPermissionRule
    ColumnLevelPermissionRule (ColumnLevelPermissionRule'),
    newColumnLevelPermissionRule,

    -- ** ColumnSchema
    ColumnSchema (ColumnSchema'),
    newColumnSchema,

    -- ** ColumnSort
    ColumnSort (ColumnSort'),
    newColumnSort,

    -- ** ColumnTag
    ColumnTag (ColumnTag'),
    newColumnTag,

    -- ** ColumnTooltipItem
    ColumnTooltipItem (ColumnTooltipItem'),
    newColumnTooltipItem,

    -- ** ComboChartAggregatedFieldWells
    ComboChartAggregatedFieldWells (ComboChartAggregatedFieldWells'),
    newComboChartAggregatedFieldWells,

    -- ** ComboChartConfiguration
    ComboChartConfiguration (ComboChartConfiguration'),
    newComboChartConfiguration,

    -- ** ComboChartFieldWells
    ComboChartFieldWells (ComboChartFieldWells'),
    newComboChartFieldWells,

    -- ** ComboChartSortConfiguration
    ComboChartSortConfiguration (ComboChartSortConfiguration'),
    newComboChartSortConfiguration,

    -- ** ComboChartVisual
    ComboChartVisual (ComboChartVisual'),
    newComboChartVisual,

    -- ** ComparisonConfiguration
    ComparisonConfiguration (ComparisonConfiguration'),
    newComparisonConfiguration,

    -- ** ComparisonFormatConfiguration
    ComparisonFormatConfiguration (ComparisonFormatConfiguration'),
    newComparisonFormatConfiguration,

    -- ** Computation
    Computation (Computation'),
    newComputation,

    -- ** ConditionalFormattingColor
    ConditionalFormattingColor (ConditionalFormattingColor'),
    newConditionalFormattingColor,

    -- ** ConditionalFormattingCustomIconCondition
    ConditionalFormattingCustomIconCondition (ConditionalFormattingCustomIconCondition'),
    newConditionalFormattingCustomIconCondition,

    -- ** ConditionalFormattingCustomIconOptions
    ConditionalFormattingCustomIconOptions (ConditionalFormattingCustomIconOptions'),
    newConditionalFormattingCustomIconOptions,

    -- ** ConditionalFormattingGradientColor
    ConditionalFormattingGradientColor (ConditionalFormattingGradientColor'),
    newConditionalFormattingGradientColor,

    -- ** ConditionalFormattingIcon
    ConditionalFormattingIcon (ConditionalFormattingIcon'),
    newConditionalFormattingIcon,

    -- ** ConditionalFormattingIconDisplayConfiguration
    ConditionalFormattingIconDisplayConfiguration (ConditionalFormattingIconDisplayConfiguration'),
    newConditionalFormattingIconDisplayConfiguration,

    -- ** ConditionalFormattingIconSet
    ConditionalFormattingIconSet (ConditionalFormattingIconSet'),
    newConditionalFormattingIconSet,

    -- ** ConditionalFormattingSolidColor
    ConditionalFormattingSolidColor (ConditionalFormattingSolidColor'),
    newConditionalFormattingSolidColor,

    -- ** ContributionAnalysisDefault
    ContributionAnalysisDefault (ContributionAnalysisDefault'),
    newContributionAnalysisDefault,

    -- ** CreateColumnsOperation
    CreateColumnsOperation (CreateColumnsOperation'),
    newCreateColumnsOperation,

    -- ** CredentialPair
    CredentialPair (CredentialPair'),
    newCredentialPair,

    -- ** CurrencyDisplayFormatConfiguration
    CurrencyDisplayFormatConfiguration (CurrencyDisplayFormatConfiguration'),
    newCurrencyDisplayFormatConfiguration,

    -- ** CustomActionFilterOperation
    CustomActionFilterOperation (CustomActionFilterOperation'),
    newCustomActionFilterOperation,

    -- ** CustomActionNavigationOperation
    CustomActionNavigationOperation (CustomActionNavigationOperation'),
    newCustomActionNavigationOperation,

    -- ** CustomActionSetParametersOperation
    CustomActionSetParametersOperation (CustomActionSetParametersOperation'),
    newCustomActionSetParametersOperation,

    -- ** CustomActionURLOperation
    CustomActionURLOperation (CustomActionURLOperation'),
    newCustomActionURLOperation,

    -- ** CustomContentConfiguration
    CustomContentConfiguration (CustomContentConfiguration'),
    newCustomContentConfiguration,

    -- ** CustomContentVisual
    CustomContentVisual (CustomContentVisual'),
    newCustomContentVisual,

    -- ** CustomFilterConfiguration
    CustomFilterConfiguration (CustomFilterConfiguration'),
    newCustomFilterConfiguration,

    -- ** CustomFilterListConfiguration
    CustomFilterListConfiguration (CustomFilterListConfiguration'),
    newCustomFilterListConfiguration,

    -- ** CustomNarrativeOptions
    CustomNarrativeOptions (CustomNarrativeOptions'),
    newCustomNarrativeOptions,

    -- ** CustomParameterValues
    CustomParameterValues (CustomParameterValues'),
    newCustomParameterValues,

    -- ** CustomSql
    CustomSql (CustomSql'),
    newCustomSql,

    -- ** CustomValuesConfiguration
    CustomValuesConfiguration (CustomValuesConfiguration'),
    newCustomValuesConfiguration,

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

    -- ** DashboardVersionDefinition
    DashboardVersionDefinition (DashboardVersionDefinition'),
    newDashboardVersionDefinition,

    -- ** DashboardVersionSummary
    DashboardVersionSummary (DashboardVersionSummary'),
    newDashboardVersionSummary,

    -- ** DashboardVisualId
    DashboardVisualId (DashboardVisualId'),
    newDashboardVisualId,

    -- ** DashboardVisualPublishOptions
    DashboardVisualPublishOptions (DashboardVisualPublishOptions'),
    newDashboardVisualPublishOptions,

    -- ** DataColor
    DataColor (DataColor'),
    newDataColor,

    -- ** DataColorPalette
    DataColorPalette (DataColorPalette'),
    newDataColorPalette,

    -- ** DataFieldSeriesItem
    DataFieldSeriesItem (DataFieldSeriesItem'),
    newDataFieldSeriesItem,

    -- ** DataLabelOptions
    DataLabelOptions (DataLabelOptions'),
    newDataLabelOptions,

    -- ** DataLabelType
    DataLabelType (DataLabelType'),
    newDataLabelType,

    -- ** DataPathColor
    DataPathColor (DataPathColor'),
    newDataPathColor,

    -- ** DataPathLabelType
    DataPathLabelType (DataPathLabelType'),
    newDataPathLabelType,

    -- ** DataPathSort
    DataPathSort (DataPathSort'),
    newDataPathSort,

    -- ** DataPathValue
    DataPathValue (DataPathValue'),
    newDataPathValue,

    -- ** DataSet
    DataSet (DataSet'),
    newDataSet,

    -- ** DataSetConfiguration
    DataSetConfiguration (DataSetConfiguration'),
    newDataSetConfiguration,

    -- ** DataSetIdentifierDeclaration
    DataSetIdentifierDeclaration (DataSetIdentifierDeclaration'),
    newDataSetIdentifierDeclaration,

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

    -- ** DateAxisOptions
    DateAxisOptions (DateAxisOptions'),
    newDateAxisOptions,

    -- ** DateDimensionField
    DateDimensionField (DateDimensionField'),
    newDateDimensionField,

    -- ** DateMeasureField
    DateMeasureField (DateMeasureField'),
    newDateMeasureField,

    -- ** DateTimeDefaultValues
    DateTimeDefaultValues (DateTimeDefaultValues'),
    newDateTimeDefaultValues,

    -- ** DateTimeFormatConfiguration
    DateTimeFormatConfiguration (DateTimeFormatConfiguration'),
    newDateTimeFormatConfiguration,

    -- ** DateTimeHierarchy
    DateTimeHierarchy (DateTimeHierarchy'),
    newDateTimeHierarchy,

    -- ** DateTimeParameter
    DateTimeParameter (DateTimeParameter'),
    newDateTimeParameter,

    -- ** DateTimeParameterDeclaration
    DateTimeParameterDeclaration (DateTimeParameterDeclaration'),
    newDateTimeParameterDeclaration,

    -- ** DateTimePickerControlDisplayOptions
    DateTimePickerControlDisplayOptions (DateTimePickerControlDisplayOptions'),
    newDateTimePickerControlDisplayOptions,

    -- ** DateTimeValueWhenUnsetConfiguration
    DateTimeValueWhenUnsetConfiguration (DateTimeValueWhenUnsetConfiguration'),
    newDateTimeValueWhenUnsetConfiguration,

    -- ** DecimalDefaultValues
    DecimalDefaultValues (DecimalDefaultValues'),
    newDecimalDefaultValues,

    -- ** DecimalParameter
    DecimalParameter (DecimalParameter'),
    newDecimalParameter,

    -- ** DecimalParameterDeclaration
    DecimalParameterDeclaration (DecimalParameterDeclaration'),
    newDecimalParameterDeclaration,

    -- ** DecimalPlacesConfiguration
    DecimalPlacesConfiguration (DecimalPlacesConfiguration'),
    newDecimalPlacesConfiguration,

    -- ** DecimalValueWhenUnsetConfiguration
    DecimalValueWhenUnsetConfiguration (DecimalValueWhenUnsetConfiguration'),
    newDecimalValueWhenUnsetConfiguration,

    -- ** DefaultFreeFormLayoutConfiguration
    DefaultFreeFormLayoutConfiguration (DefaultFreeFormLayoutConfiguration'),
    newDefaultFreeFormLayoutConfiguration,

    -- ** DefaultGridLayoutConfiguration
    DefaultGridLayoutConfiguration (DefaultGridLayoutConfiguration'),
    newDefaultGridLayoutConfiguration,

    -- ** DefaultInteractiveLayoutConfiguration
    DefaultInteractiveLayoutConfiguration (DefaultInteractiveLayoutConfiguration'),
    newDefaultInteractiveLayoutConfiguration,

    -- ** DefaultNewSheetConfiguration
    DefaultNewSheetConfiguration (DefaultNewSheetConfiguration'),
    newDefaultNewSheetConfiguration,

    -- ** DefaultPaginatedLayoutConfiguration
    DefaultPaginatedLayoutConfiguration (DefaultPaginatedLayoutConfiguration'),
    newDefaultPaginatedLayoutConfiguration,

    -- ** DefaultSectionBasedLayoutConfiguration
    DefaultSectionBasedLayoutConfiguration (DefaultSectionBasedLayoutConfiguration'),
    newDefaultSectionBasedLayoutConfiguration,

    -- ** DestinationParameterValueConfiguration
    DestinationParameterValueConfiguration (DestinationParameterValueConfiguration'),
    newDestinationParameterValueConfiguration,

    -- ** DimensionField
    DimensionField (DimensionField'),
    newDimensionField,

    -- ** DonutCenterOptions
    DonutCenterOptions (DonutCenterOptions'),
    newDonutCenterOptions,

    -- ** DonutOptions
    DonutOptions (DonutOptions'),
    newDonutOptions,

    -- ** DrillDownFilter
    DrillDownFilter (DrillDownFilter'),
    newDrillDownFilter,

    -- ** DropDownControlDisplayOptions
    DropDownControlDisplayOptions (DropDownControlDisplayOptions'),
    newDropDownControlDisplayOptions,

    -- ** DynamicDefaultValue
    DynamicDefaultValue (DynamicDefaultValue'),
    newDynamicDefaultValue,

    -- ** EmptyVisual
    EmptyVisual (EmptyVisual'),
    newEmptyVisual,

    -- ** Entity
    Entity (Entity'),
    newEntity,

    -- ** ErrorInfo
    ErrorInfo (ErrorInfo'),
    newErrorInfo,

    -- ** ExasolParameters
    ExasolParameters (ExasolParameters'),
    newExasolParameters,

    -- ** ExcludePeriodConfiguration
    ExcludePeriodConfiguration (ExcludePeriodConfiguration'),
    newExcludePeriodConfiguration,

    -- ** ExplicitHierarchy
    ExplicitHierarchy (ExplicitHierarchy'),
    newExplicitHierarchy,

    -- ** ExportHiddenFieldsOption
    ExportHiddenFieldsOption (ExportHiddenFieldsOption'),
    newExportHiddenFieldsOption,

    -- ** ExportToCSVOption
    ExportToCSVOption (ExportToCSVOption'),
    newExportToCSVOption,

    -- ** FieldBasedTooltip
    FieldBasedTooltip (FieldBasedTooltip'),
    newFieldBasedTooltip,

    -- ** FieldFolder
    FieldFolder (FieldFolder'),
    newFieldFolder,

    -- ** FieldLabelType
    FieldLabelType (FieldLabelType'),
    newFieldLabelType,

    -- ** FieldSeriesItem
    FieldSeriesItem (FieldSeriesItem'),
    newFieldSeriesItem,

    -- ** FieldSort
    FieldSort (FieldSort'),
    newFieldSort,

    -- ** FieldSortOptions
    FieldSortOptions (FieldSortOptions'),
    newFieldSortOptions,

    -- ** FieldTooltipItem
    FieldTooltipItem (FieldTooltipItem'),
    newFieldTooltipItem,

    -- ** FilledMapAggregatedFieldWells
    FilledMapAggregatedFieldWells (FilledMapAggregatedFieldWells'),
    newFilledMapAggregatedFieldWells,

    -- ** FilledMapConditionalFormatting
    FilledMapConditionalFormatting (FilledMapConditionalFormatting'),
    newFilledMapConditionalFormatting,

    -- ** FilledMapConditionalFormattingOption
    FilledMapConditionalFormattingOption (FilledMapConditionalFormattingOption'),
    newFilledMapConditionalFormattingOption,

    -- ** FilledMapConfiguration
    FilledMapConfiguration (FilledMapConfiguration'),
    newFilledMapConfiguration,

    -- ** FilledMapFieldWells
    FilledMapFieldWells (FilledMapFieldWells'),
    newFilledMapFieldWells,

    -- ** FilledMapShapeConditionalFormatting
    FilledMapShapeConditionalFormatting (FilledMapShapeConditionalFormatting'),
    newFilledMapShapeConditionalFormatting,

    -- ** FilledMapSortConfiguration
    FilledMapSortConfiguration (FilledMapSortConfiguration'),
    newFilledMapSortConfiguration,

    -- ** FilledMapVisual
    FilledMapVisual (FilledMapVisual'),
    newFilledMapVisual,

    -- ** Filter
    Filter (Filter'),
    newFilter,

    -- ** FilterControl
    FilterControl (FilterControl'),
    newFilterControl,

    -- ** FilterDateTimePickerControl
    FilterDateTimePickerControl (FilterDateTimePickerControl'),
    newFilterDateTimePickerControl,

    -- ** FilterDropDownControl
    FilterDropDownControl (FilterDropDownControl'),
    newFilterDropDownControl,

    -- ** FilterGroup
    FilterGroup (FilterGroup'),
    newFilterGroup,

    -- ** FilterListConfiguration
    FilterListConfiguration (FilterListConfiguration'),
    newFilterListConfiguration,

    -- ** FilterListControl
    FilterListControl (FilterListControl'),
    newFilterListControl,

    -- ** FilterOperation
    FilterOperation (FilterOperation'),
    newFilterOperation,

    -- ** FilterOperationSelectedFieldsConfiguration
    FilterOperationSelectedFieldsConfiguration (FilterOperationSelectedFieldsConfiguration'),
    newFilterOperationSelectedFieldsConfiguration,

    -- ** FilterOperationTargetVisualsConfiguration
    FilterOperationTargetVisualsConfiguration (FilterOperationTargetVisualsConfiguration'),
    newFilterOperationTargetVisualsConfiguration,

    -- ** FilterRelativeDateTimeControl
    FilterRelativeDateTimeControl (FilterRelativeDateTimeControl'),
    newFilterRelativeDateTimeControl,

    -- ** FilterScopeConfiguration
    FilterScopeConfiguration (FilterScopeConfiguration'),
    newFilterScopeConfiguration,

    -- ** FilterSelectableValues
    FilterSelectableValues (FilterSelectableValues'),
    newFilterSelectableValues,

    -- ** FilterSliderControl
    FilterSliderControl (FilterSliderControl'),
    newFilterSliderControl,

    -- ** FilterTextAreaControl
    FilterTextAreaControl (FilterTextAreaControl'),
    newFilterTextAreaControl,

    -- ** FilterTextFieldControl
    FilterTextFieldControl (FilterTextFieldControl'),
    newFilterTextFieldControl,

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

    -- ** Font
    Font (Font'),
    newFont,

    -- ** FontConfiguration
    FontConfiguration (FontConfiguration'),
    newFontConfiguration,

    -- ** FontSize
    FontSize (FontSize'),
    newFontSize,

    -- ** FontWeight
    FontWeight (FontWeight'),
    newFontWeight,

    -- ** ForecastComputation
    ForecastComputation (ForecastComputation'),
    newForecastComputation,

    -- ** ForecastConfiguration
    ForecastConfiguration (ForecastConfiguration'),
    newForecastConfiguration,

    -- ** ForecastScenario
    ForecastScenario (ForecastScenario'),
    newForecastScenario,

    -- ** FormatConfiguration
    FormatConfiguration (FormatConfiguration'),
    newFormatConfiguration,

    -- ** FreeFormLayoutCanvasSizeOptions
    FreeFormLayoutCanvasSizeOptions (FreeFormLayoutCanvasSizeOptions'),
    newFreeFormLayoutCanvasSizeOptions,

    -- ** FreeFormLayoutConfiguration
    FreeFormLayoutConfiguration (FreeFormLayoutConfiguration'),
    newFreeFormLayoutConfiguration,

    -- ** FreeFormLayoutElement
    FreeFormLayoutElement (FreeFormLayoutElement'),
    newFreeFormLayoutElement,

    -- ** FreeFormLayoutElementBackgroundStyle
    FreeFormLayoutElementBackgroundStyle (FreeFormLayoutElementBackgroundStyle'),
    newFreeFormLayoutElementBackgroundStyle,

    -- ** FreeFormLayoutElementBorderStyle
    FreeFormLayoutElementBorderStyle (FreeFormLayoutElementBorderStyle'),
    newFreeFormLayoutElementBorderStyle,

    -- ** FreeFormLayoutScreenCanvasSizeOptions
    FreeFormLayoutScreenCanvasSizeOptions (FreeFormLayoutScreenCanvasSizeOptions'),
    newFreeFormLayoutScreenCanvasSizeOptions,

    -- ** FreeFormSectionLayoutConfiguration
    FreeFormSectionLayoutConfiguration (FreeFormSectionLayoutConfiguration'),
    newFreeFormSectionLayoutConfiguration,

    -- ** FunnelChartAggregatedFieldWells
    FunnelChartAggregatedFieldWells (FunnelChartAggregatedFieldWells'),
    newFunnelChartAggregatedFieldWells,

    -- ** FunnelChartConfiguration
    FunnelChartConfiguration (FunnelChartConfiguration'),
    newFunnelChartConfiguration,

    -- ** FunnelChartDataLabelOptions
    FunnelChartDataLabelOptions (FunnelChartDataLabelOptions'),
    newFunnelChartDataLabelOptions,

    -- ** FunnelChartFieldWells
    FunnelChartFieldWells (FunnelChartFieldWells'),
    newFunnelChartFieldWells,

    -- ** FunnelChartSortConfiguration
    FunnelChartSortConfiguration (FunnelChartSortConfiguration'),
    newFunnelChartSortConfiguration,

    -- ** FunnelChartVisual
    FunnelChartVisual (FunnelChartVisual'),
    newFunnelChartVisual,

    -- ** GaugeChartArcConditionalFormatting
    GaugeChartArcConditionalFormatting (GaugeChartArcConditionalFormatting'),
    newGaugeChartArcConditionalFormatting,

    -- ** GaugeChartConditionalFormatting
    GaugeChartConditionalFormatting (GaugeChartConditionalFormatting'),
    newGaugeChartConditionalFormatting,

    -- ** GaugeChartConditionalFormattingOption
    GaugeChartConditionalFormattingOption (GaugeChartConditionalFormattingOption'),
    newGaugeChartConditionalFormattingOption,

    -- ** GaugeChartConfiguration
    GaugeChartConfiguration (GaugeChartConfiguration'),
    newGaugeChartConfiguration,

    -- ** GaugeChartFieldWells
    GaugeChartFieldWells (GaugeChartFieldWells'),
    newGaugeChartFieldWells,

    -- ** GaugeChartOptions
    GaugeChartOptions (GaugeChartOptions'),
    newGaugeChartOptions,

    -- ** GaugeChartPrimaryValueConditionalFormatting
    GaugeChartPrimaryValueConditionalFormatting (GaugeChartPrimaryValueConditionalFormatting'),
    newGaugeChartPrimaryValueConditionalFormatting,

    -- ** GaugeChartVisual
    GaugeChartVisual (GaugeChartVisual'),
    newGaugeChartVisual,

    -- ** GeoSpatialColumnGroup
    GeoSpatialColumnGroup (GeoSpatialColumnGroup'),
    newGeoSpatialColumnGroup,

    -- ** GeospatialCoordinateBounds
    GeospatialCoordinateBounds (GeospatialCoordinateBounds'),
    newGeospatialCoordinateBounds,

    -- ** GeospatialMapAggregatedFieldWells
    GeospatialMapAggregatedFieldWells (GeospatialMapAggregatedFieldWells'),
    newGeospatialMapAggregatedFieldWells,

    -- ** GeospatialMapConfiguration
    GeospatialMapConfiguration (GeospatialMapConfiguration'),
    newGeospatialMapConfiguration,

    -- ** GeospatialMapFieldWells
    GeospatialMapFieldWells (GeospatialMapFieldWells'),
    newGeospatialMapFieldWells,

    -- ** GeospatialMapStyleOptions
    GeospatialMapStyleOptions (GeospatialMapStyleOptions'),
    newGeospatialMapStyleOptions,

    -- ** GeospatialMapVisual
    GeospatialMapVisual (GeospatialMapVisual'),
    newGeospatialMapVisual,

    -- ** GeospatialPointStyleOptions
    GeospatialPointStyleOptions (GeospatialPointStyleOptions'),
    newGeospatialPointStyleOptions,

    -- ** GeospatialWindowOptions
    GeospatialWindowOptions (GeospatialWindowOptions'),
    newGeospatialWindowOptions,

    -- ** GlobalTableBorderOptions
    GlobalTableBorderOptions (GlobalTableBorderOptions'),
    newGlobalTableBorderOptions,

    -- ** GradientColor
    GradientColor (GradientColor'),
    newGradientColor,

    -- ** GradientStop
    GradientStop (GradientStop'),
    newGradientStop,

    -- ** GridLayoutCanvasSizeOptions
    GridLayoutCanvasSizeOptions (GridLayoutCanvasSizeOptions'),
    newGridLayoutCanvasSizeOptions,

    -- ** GridLayoutConfiguration
    GridLayoutConfiguration (GridLayoutConfiguration'),
    newGridLayoutConfiguration,

    -- ** GridLayoutElement
    GridLayoutElement (GridLayoutElement'),
    newGridLayoutElement,

    -- ** GridLayoutScreenCanvasSizeOptions
    GridLayoutScreenCanvasSizeOptions (GridLayoutScreenCanvasSizeOptions'),
    newGridLayoutScreenCanvasSizeOptions,

    -- ** Group
    Group (Group'),
    newGroup,

    -- ** GroupMember
    GroupMember (GroupMember'),
    newGroupMember,

    -- ** GroupSearchFilter
    GroupSearchFilter (GroupSearchFilter'),
    newGroupSearchFilter,

    -- ** GrowthRateComputation
    GrowthRateComputation (GrowthRateComputation'),
    newGrowthRateComputation,

    -- ** GutterStyle
    GutterStyle (GutterStyle'),
    newGutterStyle,

    -- ** HeaderFooterSectionConfiguration
    HeaderFooterSectionConfiguration (HeaderFooterSectionConfiguration'),
    newHeaderFooterSectionConfiguration,

    -- ** HeatMapAggregatedFieldWells
    HeatMapAggregatedFieldWells (HeatMapAggregatedFieldWells'),
    newHeatMapAggregatedFieldWells,

    -- ** HeatMapConfiguration
    HeatMapConfiguration (HeatMapConfiguration'),
    newHeatMapConfiguration,

    -- ** HeatMapFieldWells
    HeatMapFieldWells (HeatMapFieldWells'),
    newHeatMapFieldWells,

    -- ** HeatMapSortConfiguration
    HeatMapSortConfiguration (HeatMapSortConfiguration'),
    newHeatMapSortConfiguration,

    -- ** HeatMapVisual
    HeatMapVisual (HeatMapVisual'),
    newHeatMapVisual,

    -- ** HistogramAggregatedFieldWells
    HistogramAggregatedFieldWells (HistogramAggregatedFieldWells'),
    newHistogramAggregatedFieldWells,

    -- ** HistogramBinOptions
    HistogramBinOptions (HistogramBinOptions'),
    newHistogramBinOptions,

    -- ** HistogramConfiguration
    HistogramConfiguration (HistogramConfiguration'),
    newHistogramConfiguration,

    -- ** HistogramFieldWells
    HistogramFieldWells (HistogramFieldWells'),
    newHistogramFieldWells,

    -- ** HistogramVisual
    HistogramVisual (HistogramVisual'),
    newHistogramVisual,

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

    -- ** InsightConfiguration
    InsightConfiguration (InsightConfiguration'),
    newInsightConfiguration,

    -- ** InsightVisual
    InsightVisual (InsightVisual'),
    newInsightVisual,

    -- ** IntegerDefaultValues
    IntegerDefaultValues (IntegerDefaultValues'),
    newIntegerDefaultValues,

    -- ** IntegerParameter
    IntegerParameter (IntegerParameter'),
    newIntegerParameter,

    -- ** IntegerParameterDeclaration
    IntegerParameterDeclaration (IntegerParameterDeclaration'),
    newIntegerParameterDeclaration,

    -- ** IntegerValueWhenUnsetConfiguration
    IntegerValueWhenUnsetConfiguration (IntegerValueWhenUnsetConfiguration'),
    newIntegerValueWhenUnsetConfiguration,

    -- ** ItemsLimitConfiguration
    ItemsLimitConfiguration (ItemsLimitConfiguration'),
    newItemsLimitConfiguration,

    -- ** JiraParameters
    JiraParameters (JiraParameters'),
    newJiraParameters,

    -- ** JoinInstruction
    JoinInstruction (JoinInstruction'),
    newJoinInstruction,

    -- ** JoinKeyProperties
    JoinKeyProperties (JoinKeyProperties'),
    newJoinKeyProperties,

    -- ** KPIConditionalFormatting
    KPIConditionalFormatting (KPIConditionalFormatting'),
    newKPIConditionalFormatting,

    -- ** KPIConditionalFormattingOption
    KPIConditionalFormattingOption (KPIConditionalFormattingOption'),
    newKPIConditionalFormattingOption,

    -- ** KPIConfiguration
    KPIConfiguration (KPIConfiguration'),
    newKPIConfiguration,

    -- ** KPIFieldWells
    KPIFieldWells (KPIFieldWells'),
    newKPIFieldWells,

    -- ** KPIOptions
    KPIOptions (KPIOptions'),
    newKPIOptions,

    -- ** KPIPrimaryValueConditionalFormatting
    KPIPrimaryValueConditionalFormatting (KPIPrimaryValueConditionalFormatting'),
    newKPIPrimaryValueConditionalFormatting,

    -- ** KPIProgressBarConditionalFormatting
    KPIProgressBarConditionalFormatting (KPIProgressBarConditionalFormatting'),
    newKPIProgressBarConditionalFormatting,

    -- ** KPISortConfiguration
    KPISortConfiguration (KPISortConfiguration'),
    newKPISortConfiguration,

    -- ** KPIVisual
    KPIVisual (KPIVisual'),
    newKPIVisual,

    -- ** LabelOptions
    LabelOptions (LabelOptions'),
    newLabelOptions,

    -- ** Layout
    Layout (Layout'),
    newLayout,

    -- ** LayoutConfiguration
    LayoutConfiguration (LayoutConfiguration'),
    newLayoutConfiguration,

    -- ** LegendOptions
    LegendOptions (LegendOptions'),
    newLegendOptions,

    -- ** LineChartAggregatedFieldWells
    LineChartAggregatedFieldWells (LineChartAggregatedFieldWells'),
    newLineChartAggregatedFieldWells,

    -- ** LineChartConfiguration
    LineChartConfiguration (LineChartConfiguration'),
    newLineChartConfiguration,

    -- ** LineChartDefaultSeriesSettings
    LineChartDefaultSeriesSettings (LineChartDefaultSeriesSettings'),
    newLineChartDefaultSeriesSettings,

    -- ** LineChartFieldWells
    LineChartFieldWells (LineChartFieldWells'),
    newLineChartFieldWells,

    -- ** LineChartLineStyleSettings
    LineChartLineStyleSettings (LineChartLineStyleSettings'),
    newLineChartLineStyleSettings,

    -- ** LineChartMarkerStyleSettings
    LineChartMarkerStyleSettings (LineChartMarkerStyleSettings'),
    newLineChartMarkerStyleSettings,

    -- ** LineChartSeriesSettings
    LineChartSeriesSettings (LineChartSeriesSettings'),
    newLineChartSeriesSettings,

    -- ** LineChartSortConfiguration
    LineChartSortConfiguration (LineChartSortConfiguration'),
    newLineChartSortConfiguration,

    -- ** LineChartVisual
    LineChartVisual (LineChartVisual'),
    newLineChartVisual,

    -- ** LineSeriesAxisDisplayOptions
    LineSeriesAxisDisplayOptions (LineSeriesAxisDisplayOptions'),
    newLineSeriesAxisDisplayOptions,

    -- ** LinkSharingConfiguration
    LinkSharingConfiguration (LinkSharingConfiguration'),
    newLinkSharingConfiguration,

    -- ** ListControlDisplayOptions
    ListControlDisplayOptions (ListControlDisplayOptions'),
    newListControlDisplayOptions,

    -- ** ListControlSearchOptions
    ListControlSearchOptions (ListControlSearchOptions'),
    newListControlSearchOptions,

    -- ** ListControlSelectAllOptions
    ListControlSelectAllOptions (ListControlSelectAllOptions'),
    newListControlSelectAllOptions,

    -- ** LoadingAnimation
    LoadingAnimation (LoadingAnimation'),
    newLoadingAnimation,

    -- ** LocalNavigationConfiguration
    LocalNavigationConfiguration (LocalNavigationConfiguration'),
    newLocalNavigationConfiguration,

    -- ** LogicalTable
    LogicalTable (LogicalTable'),
    newLogicalTable,

    -- ** LogicalTableSource
    LogicalTableSource (LogicalTableSource'),
    newLogicalTableSource,

    -- ** LongFormatText
    LongFormatText (LongFormatText'),
    newLongFormatText,

    -- ** ManifestFileLocation
    ManifestFileLocation (ManifestFileLocation'),
    newManifestFileLocation,

    -- ** MarginStyle
    MarginStyle (MarginStyle'),
    newMarginStyle,

    -- ** MariaDbParameters
    MariaDbParameters (MariaDbParameters'),
    newMariaDbParameters,

    -- ** MaximumLabelType
    MaximumLabelType (MaximumLabelType'),
    newMaximumLabelType,

    -- ** MaximumMinimumComputation
    MaximumMinimumComputation (MaximumMinimumComputation'),
    newMaximumMinimumComputation,

    -- ** MeasureField
    MeasureField (MeasureField'),
    newMeasureField,

    -- ** MemberIdArnPair
    MemberIdArnPair (MemberIdArnPair'),
    newMemberIdArnPair,

    -- ** MetricComparisonComputation
    MetricComparisonComputation (MetricComparisonComputation'),
    newMetricComparisonComputation,

    -- ** MinimumLabelType
    MinimumLabelType (MinimumLabelType'),
    newMinimumLabelType,

    -- ** MissingDataConfiguration
    MissingDataConfiguration (MissingDataConfiguration'),
    newMissingDataConfiguration,

    -- ** MySqlParameters
    MySqlParameters (MySqlParameters'),
    newMySqlParameters,

    -- ** NamespaceError
    NamespaceError (NamespaceError'),
    newNamespaceError,

    -- ** NamespaceInfoV2
    NamespaceInfoV2 (NamespaceInfoV2'),
    newNamespaceInfoV2,

    -- ** NegativeValueConfiguration
    NegativeValueConfiguration (NegativeValueConfiguration'),
    newNegativeValueConfiguration,

    -- ** NullValueFormatConfiguration
    NullValueFormatConfiguration (NullValueFormatConfiguration'),
    newNullValueFormatConfiguration,

    -- ** NumberDisplayFormatConfiguration
    NumberDisplayFormatConfiguration (NumberDisplayFormatConfiguration'),
    newNumberDisplayFormatConfiguration,

    -- ** NumberFormatConfiguration
    NumberFormatConfiguration (NumberFormatConfiguration'),
    newNumberFormatConfiguration,

    -- ** NumericAxisOptions
    NumericAxisOptions (NumericAxisOptions'),
    newNumericAxisOptions,

    -- ** NumericEqualityDrillDownFilter
    NumericEqualityDrillDownFilter (NumericEqualityDrillDownFilter'),
    newNumericEqualityDrillDownFilter,

    -- ** NumericEqualityFilter
    NumericEqualityFilter (NumericEqualityFilter'),
    newNumericEqualityFilter,

    -- ** NumericFormatConfiguration
    NumericFormatConfiguration (NumericFormatConfiguration'),
    newNumericFormatConfiguration,

    -- ** NumericRangeFilter
    NumericRangeFilter (NumericRangeFilter'),
    newNumericRangeFilter,

    -- ** NumericRangeFilterValue
    NumericRangeFilterValue (NumericRangeFilterValue'),
    newNumericRangeFilterValue,

    -- ** NumericSeparatorConfiguration
    NumericSeparatorConfiguration (NumericSeparatorConfiguration'),
    newNumericSeparatorConfiguration,

    -- ** NumericalAggregationFunction
    NumericalAggregationFunction (NumericalAggregationFunction'),
    newNumericalAggregationFunction,

    -- ** NumericalDimensionField
    NumericalDimensionField (NumericalDimensionField'),
    newNumericalDimensionField,

    -- ** NumericalMeasureField
    NumericalMeasureField (NumericalMeasureField'),
    newNumericalMeasureField,

    -- ** OracleParameters
    OracleParameters (OracleParameters'),
    newOracleParameters,

    -- ** OutputColumn
    OutputColumn (OutputColumn'),
    newOutputColumn,

    -- ** PaginationConfiguration
    PaginationConfiguration (PaginationConfiguration'),
    newPaginationConfiguration,

    -- ** PanelConfiguration
    PanelConfiguration (PanelConfiguration'),
    newPanelConfiguration,

    -- ** PanelTitleOptions
    PanelTitleOptions (PanelTitleOptions'),
    newPanelTitleOptions,

    -- ** ParameterControl
    ParameterControl (ParameterControl'),
    newParameterControl,

    -- ** ParameterDateTimePickerControl
    ParameterDateTimePickerControl (ParameterDateTimePickerControl'),
    newParameterDateTimePickerControl,

    -- ** ParameterDeclaration
    ParameterDeclaration (ParameterDeclaration'),
    newParameterDeclaration,

    -- ** ParameterDropDownControl
    ParameterDropDownControl (ParameterDropDownControl'),
    newParameterDropDownControl,

    -- ** ParameterListControl
    ParameterListControl (ParameterListControl'),
    newParameterListControl,

    -- ** ParameterSelectableValues
    ParameterSelectableValues (ParameterSelectableValues'),
    newParameterSelectableValues,

    -- ** ParameterSliderControl
    ParameterSliderControl (ParameterSliderControl'),
    newParameterSliderControl,

    -- ** ParameterTextAreaControl
    ParameterTextAreaControl (ParameterTextAreaControl'),
    newParameterTextAreaControl,

    -- ** ParameterTextFieldControl
    ParameterTextFieldControl (ParameterTextFieldControl'),
    newParameterTextFieldControl,

    -- ** Parameters
    Parameters (Parameters'),
    newParameters,

    -- ** PercentVisibleRange
    PercentVisibleRange (PercentVisibleRange'),
    newPercentVisibleRange,

    -- ** PercentageDisplayFormatConfiguration
    PercentageDisplayFormatConfiguration (PercentageDisplayFormatConfiguration'),
    newPercentageDisplayFormatConfiguration,

    -- ** PercentileAggregation
    PercentileAggregation (PercentileAggregation'),
    newPercentileAggregation,

    -- ** PeriodOverPeriodComputation
    PeriodOverPeriodComputation (PeriodOverPeriodComputation'),
    newPeriodOverPeriodComputation,

    -- ** PeriodToDateComputation
    PeriodToDateComputation (PeriodToDateComputation'),
    newPeriodToDateComputation,

    -- ** PhysicalTable
    PhysicalTable (PhysicalTable'),
    newPhysicalTable,

    -- ** PieChartAggregatedFieldWells
    PieChartAggregatedFieldWells (PieChartAggregatedFieldWells'),
    newPieChartAggregatedFieldWells,

    -- ** PieChartConfiguration
    PieChartConfiguration (PieChartConfiguration'),
    newPieChartConfiguration,

    -- ** PieChartFieldWells
    PieChartFieldWells (PieChartFieldWells'),
    newPieChartFieldWells,

    -- ** PieChartSortConfiguration
    PieChartSortConfiguration (PieChartSortConfiguration'),
    newPieChartSortConfiguration,

    -- ** PieChartVisual
    PieChartVisual (PieChartVisual'),
    newPieChartVisual,

    -- ** PivotFieldSortOptions
    PivotFieldSortOptions (PivotFieldSortOptions'),
    newPivotFieldSortOptions,

    -- ** PivotTableAggregatedFieldWells
    PivotTableAggregatedFieldWells (PivotTableAggregatedFieldWells'),
    newPivotTableAggregatedFieldWells,

    -- ** PivotTableCellConditionalFormatting
    PivotTableCellConditionalFormatting (PivotTableCellConditionalFormatting'),
    newPivotTableCellConditionalFormatting,

    -- ** PivotTableConditionalFormatting
    PivotTableConditionalFormatting (PivotTableConditionalFormatting'),
    newPivotTableConditionalFormatting,

    -- ** PivotTableConditionalFormattingOption
    PivotTableConditionalFormattingOption (PivotTableConditionalFormattingOption'),
    newPivotTableConditionalFormattingOption,

    -- ** PivotTableConditionalFormattingScope
    PivotTableConditionalFormattingScope (PivotTableConditionalFormattingScope'),
    newPivotTableConditionalFormattingScope,

    -- ** PivotTableConfiguration
    PivotTableConfiguration (PivotTableConfiguration'),
    newPivotTableConfiguration,

    -- ** PivotTableDataPathOption
    PivotTableDataPathOption (PivotTableDataPathOption'),
    newPivotTableDataPathOption,

    -- ** PivotTableFieldOption
    PivotTableFieldOption (PivotTableFieldOption'),
    newPivotTableFieldOption,

    -- ** PivotTableFieldOptions
    PivotTableFieldOptions (PivotTableFieldOptions'),
    newPivotTableFieldOptions,

    -- ** PivotTableFieldSubtotalOptions
    PivotTableFieldSubtotalOptions (PivotTableFieldSubtotalOptions'),
    newPivotTableFieldSubtotalOptions,

    -- ** PivotTableFieldWells
    PivotTableFieldWells (PivotTableFieldWells'),
    newPivotTableFieldWells,

    -- ** PivotTableOptions
    PivotTableOptions (PivotTableOptions'),
    newPivotTableOptions,

    -- ** PivotTablePaginatedReportOptions
    PivotTablePaginatedReportOptions (PivotTablePaginatedReportOptions'),
    newPivotTablePaginatedReportOptions,

    -- ** PivotTableSortBy
    PivotTableSortBy (PivotTableSortBy'),
    newPivotTableSortBy,

    -- ** PivotTableSortConfiguration
    PivotTableSortConfiguration (PivotTableSortConfiguration'),
    newPivotTableSortConfiguration,

    -- ** PivotTableTotalOptions
    PivotTableTotalOptions (PivotTableTotalOptions'),
    newPivotTableTotalOptions,

    -- ** PivotTableVisual
    PivotTableVisual (PivotTableVisual'),
    newPivotTableVisual,

    -- ** PivotTotalOptions
    PivotTotalOptions (PivotTotalOptions'),
    newPivotTotalOptions,

    -- ** PostgreSqlParameters
    PostgreSqlParameters (PostgreSqlParameters'),
    newPostgreSqlParameters,

    -- ** PredefinedHierarchy
    PredefinedHierarchy (PredefinedHierarchy'),
    newPredefinedHierarchy,

    -- ** PrestoParameters
    PrestoParameters (PrestoParameters'),
    newPrestoParameters,

    -- ** ProgressBarOptions
    ProgressBarOptions (ProgressBarOptions'),
    newProgressBarOptions,

    -- ** ProjectOperation
    ProjectOperation (ProjectOperation'),
    newProjectOperation,

    -- ** QueueInfo
    QueueInfo (QueueInfo'),
    newQueueInfo,

    -- ** RangeEndsLabelType
    RangeEndsLabelType (RangeEndsLabelType'),
    newRangeEndsLabelType,

    -- ** RdsParameters
    RdsParameters (RdsParameters'),
    newRdsParameters,

    -- ** RedshiftParameters
    RedshiftParameters (RedshiftParameters'),
    newRedshiftParameters,

    -- ** ReferenceLine
    ReferenceLine (ReferenceLine'),
    newReferenceLine,

    -- ** ReferenceLineCustomLabelConfiguration
    ReferenceLineCustomLabelConfiguration (ReferenceLineCustomLabelConfiguration'),
    newReferenceLineCustomLabelConfiguration,

    -- ** ReferenceLineDataConfiguration
    ReferenceLineDataConfiguration (ReferenceLineDataConfiguration'),
    newReferenceLineDataConfiguration,

    -- ** ReferenceLineDynamicDataConfiguration
    ReferenceLineDynamicDataConfiguration (ReferenceLineDynamicDataConfiguration'),
    newReferenceLineDynamicDataConfiguration,

    -- ** ReferenceLineLabelConfiguration
    ReferenceLineLabelConfiguration (ReferenceLineLabelConfiguration'),
    newReferenceLineLabelConfiguration,

    -- ** ReferenceLineStaticDataConfiguration
    ReferenceLineStaticDataConfiguration (ReferenceLineStaticDataConfiguration'),
    newReferenceLineStaticDataConfiguration,

    -- ** ReferenceLineStyleConfiguration
    ReferenceLineStyleConfiguration (ReferenceLineStyleConfiguration'),
    newReferenceLineStyleConfiguration,

    -- ** ReferenceLineValueLabelConfiguration
    ReferenceLineValueLabelConfiguration (ReferenceLineValueLabelConfiguration'),
    newReferenceLineValueLabelConfiguration,

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

    -- ** RelativeDateTimeControlDisplayOptions
    RelativeDateTimeControlDisplayOptions (RelativeDateTimeControlDisplayOptions'),
    newRelativeDateTimeControlDisplayOptions,

    -- ** RelativeDatesFilter
    RelativeDatesFilter (RelativeDatesFilter'),
    newRelativeDatesFilter,

    -- ** RenameColumnOperation
    RenameColumnOperation (RenameColumnOperation'),
    newRenameColumnOperation,

    -- ** ResourcePermission
    ResourcePermission (ResourcePermission'),
    newResourcePermission,

    -- ** RollingDateConfiguration
    RollingDateConfiguration (RollingDateConfiguration'),
    newRollingDateConfiguration,

    -- ** RowAlternateColorOptions
    RowAlternateColorOptions (RowAlternateColorOptions'),
    newRowAlternateColorOptions,

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

    -- ** SameSheetTargetVisualConfiguration
    SameSheetTargetVisualConfiguration (SameSheetTargetVisualConfiguration'),
    newSameSheetTargetVisualConfiguration,

    -- ** SankeyDiagramAggregatedFieldWells
    SankeyDiagramAggregatedFieldWells (SankeyDiagramAggregatedFieldWells'),
    newSankeyDiagramAggregatedFieldWells,

    -- ** SankeyDiagramChartConfiguration
    SankeyDiagramChartConfiguration (SankeyDiagramChartConfiguration'),
    newSankeyDiagramChartConfiguration,

    -- ** SankeyDiagramFieldWells
    SankeyDiagramFieldWells (SankeyDiagramFieldWells'),
    newSankeyDiagramFieldWells,

    -- ** SankeyDiagramSortConfiguration
    SankeyDiagramSortConfiguration (SankeyDiagramSortConfiguration'),
    newSankeyDiagramSortConfiguration,

    -- ** SankeyDiagramVisual
    SankeyDiagramVisual (SankeyDiagramVisual'),
    newSankeyDiagramVisual,

    -- ** ScatterPlotCategoricallyAggregatedFieldWells
    ScatterPlotCategoricallyAggregatedFieldWells (ScatterPlotCategoricallyAggregatedFieldWells'),
    newScatterPlotCategoricallyAggregatedFieldWells,

    -- ** ScatterPlotConfiguration
    ScatterPlotConfiguration (ScatterPlotConfiguration'),
    newScatterPlotConfiguration,

    -- ** ScatterPlotFieldWells
    ScatterPlotFieldWells (ScatterPlotFieldWells'),
    newScatterPlotFieldWells,

    -- ** ScatterPlotUnaggregatedFieldWells
    ScatterPlotUnaggregatedFieldWells (ScatterPlotUnaggregatedFieldWells'),
    newScatterPlotUnaggregatedFieldWells,

    -- ** ScatterPlotVisual
    ScatterPlotVisual (ScatterPlotVisual'),
    newScatterPlotVisual,

    -- ** ScrollBarOptions
    ScrollBarOptions (ScrollBarOptions'),
    newScrollBarOptions,

    -- ** SecondaryValueOptions
    SecondaryValueOptions (SecondaryValueOptions'),
    newSecondaryValueOptions,

    -- ** SectionAfterPageBreak
    SectionAfterPageBreak (SectionAfterPageBreak'),
    newSectionAfterPageBreak,

    -- ** SectionBasedLayoutCanvasSizeOptions
    SectionBasedLayoutCanvasSizeOptions (SectionBasedLayoutCanvasSizeOptions'),
    newSectionBasedLayoutCanvasSizeOptions,

    -- ** SectionBasedLayoutConfiguration
    SectionBasedLayoutConfiguration (SectionBasedLayoutConfiguration'),
    newSectionBasedLayoutConfiguration,

    -- ** SectionBasedLayoutPaperCanvasSizeOptions
    SectionBasedLayoutPaperCanvasSizeOptions (SectionBasedLayoutPaperCanvasSizeOptions'),
    newSectionBasedLayoutPaperCanvasSizeOptions,

    -- ** SectionLayoutConfiguration
    SectionLayoutConfiguration (SectionLayoutConfiguration'),
    newSectionLayoutConfiguration,

    -- ** SectionPageBreakConfiguration
    SectionPageBreakConfiguration (SectionPageBreakConfiguration'),
    newSectionPageBreakConfiguration,

    -- ** SectionStyle
    SectionStyle (SectionStyle'),
    newSectionStyle,

    -- ** SelectedSheetsFilterScopeConfiguration
    SelectedSheetsFilterScopeConfiguration (SelectedSheetsFilterScopeConfiguration'),
    newSelectedSheetsFilterScopeConfiguration,

    -- ** SeriesItem
    SeriesItem (SeriesItem'),
    newSeriesItem,

    -- ** ServiceNowParameters
    ServiceNowParameters (ServiceNowParameters'),
    newServiceNowParameters,

    -- ** SessionTag
    SessionTag (SessionTag'),
    newSessionTag,

    -- ** SetParameterValueConfiguration
    SetParameterValueConfiguration (SetParameterValueConfiguration'),
    newSetParameterValueConfiguration,

    -- ** ShapeConditionalFormat
    ShapeConditionalFormat (ShapeConditionalFormat'),
    newShapeConditionalFormat,

    -- ** Sheet
    Sheet (Sheet'),
    newSheet,

    -- ** SheetControlLayout
    SheetControlLayout (SheetControlLayout'),
    newSheetControlLayout,

    -- ** SheetControlLayoutConfiguration
    SheetControlLayoutConfiguration (SheetControlLayoutConfiguration'),
    newSheetControlLayoutConfiguration,

    -- ** SheetControlsOption
    SheetControlsOption (SheetControlsOption'),
    newSheetControlsOption,

    -- ** SheetDefinition
    SheetDefinition (SheetDefinition'),
    newSheetDefinition,

    -- ** SheetElementConfigurationOverrides
    SheetElementConfigurationOverrides (SheetElementConfigurationOverrides'),
    newSheetElementConfigurationOverrides,

    -- ** SheetElementRenderingRule
    SheetElementRenderingRule (SheetElementRenderingRule'),
    newSheetElementRenderingRule,

    -- ** SheetStyle
    SheetStyle (SheetStyle'),
    newSheetStyle,

    -- ** SheetTextBox
    SheetTextBox (SheetTextBox'),
    newSheetTextBox,

    -- ** SheetVisualScopingConfiguration
    SheetVisualScopingConfiguration (SheetVisualScopingConfiguration'),
    newSheetVisualScopingConfiguration,

    -- ** ShortFormatText
    ShortFormatText (ShortFormatText'),
    newShortFormatText,

    -- ** SignupResponse
    SignupResponse (SignupResponse'),
    newSignupResponse,

    -- ** SimpleClusterMarker
    SimpleClusterMarker (SimpleClusterMarker'),
    newSimpleClusterMarker,

    -- ** SliderControlDisplayOptions
    SliderControlDisplayOptions (SliderControlDisplayOptions'),
    newSliderControlDisplayOptions,

    -- ** SmallMultiplesOptions
    SmallMultiplesOptions (SmallMultiplesOptions'),
    newSmallMultiplesOptions,

    -- ** SnowflakeParameters
    SnowflakeParameters (SnowflakeParameters'),
    newSnowflakeParameters,

    -- ** Spacing
    Spacing (Spacing'),
    newSpacing,

    -- ** SparkParameters
    SparkParameters (SparkParameters'),
    newSparkParameters,

    -- ** SqlServerParameters
    SqlServerParameters (SqlServerParameters'),
    newSqlServerParameters,

    -- ** SslProperties
    SslProperties (SslProperties'),
    newSslProperties,

    -- ** StringDefaultValues
    StringDefaultValues (StringDefaultValues'),
    newStringDefaultValues,

    -- ** StringFormatConfiguration
    StringFormatConfiguration (StringFormatConfiguration'),
    newStringFormatConfiguration,

    -- ** StringParameter
    StringParameter (StringParameter'),
    newStringParameter,

    -- ** StringParameterDeclaration
    StringParameterDeclaration (StringParameterDeclaration'),
    newStringParameterDeclaration,

    -- ** StringValueWhenUnsetConfiguration
    StringValueWhenUnsetConfiguration (StringValueWhenUnsetConfiguration'),
    newStringValueWhenUnsetConfiguration,

    -- ** SubtotalOptions
    SubtotalOptions (SubtotalOptions'),
    newSubtotalOptions,

    -- ** TableAggregatedFieldWells
    TableAggregatedFieldWells (TableAggregatedFieldWells'),
    newTableAggregatedFieldWells,

    -- ** TableBorderOptions
    TableBorderOptions (TableBorderOptions'),
    newTableBorderOptions,

    -- ** TableCellConditionalFormatting
    TableCellConditionalFormatting (TableCellConditionalFormatting'),
    newTableCellConditionalFormatting,

    -- ** TableCellImageSizingConfiguration
    TableCellImageSizingConfiguration (TableCellImageSizingConfiguration'),
    newTableCellImageSizingConfiguration,

    -- ** TableCellStyle
    TableCellStyle (TableCellStyle'),
    newTableCellStyle,

    -- ** TableConditionalFormatting
    TableConditionalFormatting (TableConditionalFormatting'),
    newTableConditionalFormatting,

    -- ** TableConditionalFormattingOption
    TableConditionalFormattingOption (TableConditionalFormattingOption'),
    newTableConditionalFormattingOption,

    -- ** TableConfiguration
    TableConfiguration (TableConfiguration'),
    newTableConfiguration,

    -- ** TableFieldCustomIconContent
    TableFieldCustomIconContent (TableFieldCustomIconContent'),
    newTableFieldCustomIconContent,

    -- ** TableFieldCustomTextContent
    TableFieldCustomTextContent (TableFieldCustomTextContent'),
    newTableFieldCustomTextContent,

    -- ** TableFieldImageConfiguration
    TableFieldImageConfiguration (TableFieldImageConfiguration'),
    newTableFieldImageConfiguration,

    -- ** TableFieldLinkConfiguration
    TableFieldLinkConfiguration (TableFieldLinkConfiguration'),
    newTableFieldLinkConfiguration,

    -- ** TableFieldLinkContentConfiguration
    TableFieldLinkContentConfiguration (TableFieldLinkContentConfiguration'),
    newTableFieldLinkContentConfiguration,

    -- ** TableFieldOption
    TableFieldOption (TableFieldOption'),
    newTableFieldOption,

    -- ** TableFieldOptions
    TableFieldOptions (TableFieldOptions'),
    newTableFieldOptions,

    -- ** TableFieldURLConfiguration
    TableFieldURLConfiguration (TableFieldURLConfiguration'),
    newTableFieldURLConfiguration,

    -- ** TableFieldWells
    TableFieldWells (TableFieldWells'),
    newTableFieldWells,

    -- ** TableOptions
    TableOptions (TableOptions'),
    newTableOptions,

    -- ** TablePaginatedReportOptions
    TablePaginatedReportOptions (TablePaginatedReportOptions'),
    newTablePaginatedReportOptions,

    -- ** TableRowConditionalFormatting
    TableRowConditionalFormatting (TableRowConditionalFormatting'),
    newTableRowConditionalFormatting,

    -- ** TableSideBorderOptions
    TableSideBorderOptions (TableSideBorderOptions'),
    newTableSideBorderOptions,

    -- ** TableSortConfiguration
    TableSortConfiguration (TableSortConfiguration'),
    newTableSortConfiguration,

    -- ** TableUnaggregatedFieldWells
    TableUnaggregatedFieldWells (TableUnaggregatedFieldWells'),
    newTableUnaggregatedFieldWells,

    -- ** TableVisual
    TableVisual (TableVisual'),
    newTableVisual,

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

    -- ** TemplateVersionDefinition
    TemplateVersionDefinition (TemplateVersionDefinition'),
    newTemplateVersionDefinition,

    -- ** TemplateVersionSummary
    TemplateVersionSummary (TemplateVersionSummary'),
    newTemplateVersionSummary,

    -- ** TeradataParameters
    TeradataParameters (TeradataParameters'),
    newTeradataParameters,

    -- ** TextAreaControlDisplayOptions
    TextAreaControlDisplayOptions (TextAreaControlDisplayOptions'),
    newTextAreaControlDisplayOptions,

    -- ** TextConditionalFormat
    TextConditionalFormat (TextConditionalFormat'),
    newTextConditionalFormat,

    -- ** TextControlPlaceholderOptions
    TextControlPlaceholderOptions (TextControlPlaceholderOptions'),
    newTextControlPlaceholderOptions,

    -- ** TextFieldControlDisplayOptions
    TextFieldControlDisplayOptions (TextFieldControlDisplayOptions'),
    newTextFieldControlDisplayOptions,

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

    -- ** ThousandSeparatorOptions
    ThousandSeparatorOptions (ThousandSeparatorOptions'),
    newThousandSeparatorOptions,

    -- ** TileLayoutStyle
    TileLayoutStyle (TileLayoutStyle'),
    newTileLayoutStyle,

    -- ** TileStyle
    TileStyle (TileStyle'),
    newTileStyle,

    -- ** TimeBasedForecastProperties
    TimeBasedForecastProperties (TimeBasedForecastProperties'),
    newTimeBasedForecastProperties,

    -- ** TimeEqualityFilter
    TimeEqualityFilter (TimeEqualityFilter'),
    newTimeEqualityFilter,

    -- ** TimeRangeDrillDownFilter
    TimeRangeDrillDownFilter (TimeRangeDrillDownFilter'),
    newTimeRangeDrillDownFilter,

    -- ** TimeRangeFilter
    TimeRangeFilter (TimeRangeFilter'),
    newTimeRangeFilter,

    -- ** TimeRangeFilterValue
    TimeRangeFilterValue (TimeRangeFilterValue'),
    newTimeRangeFilterValue,

    -- ** TooltipItem
    TooltipItem (TooltipItem'),
    newTooltipItem,

    -- ** TooltipOptions
    TooltipOptions (TooltipOptions'),
    newTooltipOptions,

    -- ** TopBottomFilter
    TopBottomFilter (TopBottomFilter'),
    newTopBottomFilter,

    -- ** TopBottomMoversComputation
    TopBottomMoversComputation (TopBottomMoversComputation'),
    newTopBottomMoversComputation,

    -- ** TopBottomRankedComputation
    TopBottomRankedComputation (TopBottomRankedComputation'),
    newTopBottomRankedComputation,

    -- ** TotalAggregationComputation
    TotalAggregationComputation (TotalAggregationComputation'),
    newTotalAggregationComputation,

    -- ** TotalOptions
    TotalOptions (TotalOptions'),
    newTotalOptions,

    -- ** TransformOperation
    TransformOperation (TransformOperation'),
    newTransformOperation,

    -- ** TreeMapAggregatedFieldWells
    TreeMapAggregatedFieldWells (TreeMapAggregatedFieldWells'),
    newTreeMapAggregatedFieldWells,

    -- ** TreeMapConfiguration
    TreeMapConfiguration (TreeMapConfiguration'),
    newTreeMapConfiguration,

    -- ** TreeMapFieldWells
    TreeMapFieldWells (TreeMapFieldWells'),
    newTreeMapFieldWells,

    -- ** TreeMapSortConfiguration
    TreeMapSortConfiguration (TreeMapSortConfiguration'),
    newTreeMapSortConfiguration,

    -- ** TreeMapVisual
    TreeMapVisual (TreeMapVisual'),
    newTreeMapVisual,

    -- ** TrendArrowOptions
    TrendArrowOptions (TrendArrowOptions'),
    newTrendArrowOptions,

    -- ** TwitterParameters
    TwitterParameters (TwitterParameters'),
    newTwitterParameters,

    -- ** Typography
    Typography (Typography'),
    newTypography,

    -- ** UIColorPalette
    UIColorPalette (UIColorPalette'),
    newUIColorPalette,

    -- ** UnaggregatedField
    UnaggregatedField (UnaggregatedField'),
    newUnaggregatedField,

    -- ** UniqueValuesComputation
    UniqueValuesComputation (UniqueValuesComputation'),
    newUniqueValuesComputation,

    -- ** UntagColumnOperation
    UntagColumnOperation (UntagColumnOperation'),
    newUntagColumnOperation,

    -- ** UploadSettings
    UploadSettings (UploadSettings'),
    newUploadSettings,

    -- ** User
    User (User'),
    newUser,

    -- ** VisibleRangeOptions
    VisibleRangeOptions (VisibleRangeOptions'),
    newVisibleRangeOptions,

    -- ** Visual
    Visual (Visual'),
    newVisual,

    -- ** VisualCustomAction
    VisualCustomAction (VisualCustomAction'),
    newVisualCustomAction,

    -- ** VisualCustomActionOperation
    VisualCustomActionOperation (VisualCustomActionOperation'),
    newVisualCustomActionOperation,

    -- ** VisualPalette
    VisualPalette (VisualPalette'),
    newVisualPalette,

    -- ** VisualSubtitleLabelOptions
    VisualSubtitleLabelOptions (VisualSubtitleLabelOptions'),
    newVisualSubtitleLabelOptions,

    -- ** VisualTitleLabelOptions
    VisualTitleLabelOptions (VisualTitleLabelOptions'),
    newVisualTitleLabelOptions,

    -- ** VpcConnectionProperties
    VpcConnectionProperties (VpcConnectionProperties'),
    newVpcConnectionProperties,

    -- ** WaterfallChartAggregatedFieldWells
    WaterfallChartAggregatedFieldWells (WaterfallChartAggregatedFieldWells'),
    newWaterfallChartAggregatedFieldWells,

    -- ** WaterfallChartConfiguration
    WaterfallChartConfiguration (WaterfallChartConfiguration'),
    newWaterfallChartConfiguration,

    -- ** WaterfallChartFieldWells
    WaterfallChartFieldWells (WaterfallChartFieldWells'),
    newWaterfallChartFieldWells,

    -- ** WaterfallChartOptions
    WaterfallChartOptions (WaterfallChartOptions'),
    newWaterfallChartOptions,

    -- ** WaterfallChartSortConfiguration
    WaterfallChartSortConfiguration (WaterfallChartSortConfiguration'),
    newWaterfallChartSortConfiguration,

    -- ** WaterfallVisual
    WaterfallVisual (WaterfallVisual'),
    newWaterfallVisual,

    -- ** WhatIfPointScenario
    WhatIfPointScenario (WhatIfPointScenario'),
    newWhatIfPointScenario,

    -- ** WhatIfRangeScenario
    WhatIfRangeScenario (WhatIfRangeScenario'),
    newWhatIfRangeScenario,

    -- ** WordCloudAggregatedFieldWells
    WordCloudAggregatedFieldWells (WordCloudAggregatedFieldWells'),
    newWordCloudAggregatedFieldWells,

    -- ** WordCloudChartConfiguration
    WordCloudChartConfiguration (WordCloudChartConfiguration'),
    newWordCloudChartConfiguration,

    -- ** WordCloudFieldWells
    WordCloudFieldWells (WordCloudFieldWells'),
    newWordCloudFieldWells,

    -- ** WordCloudOptions
    WordCloudOptions (WordCloudOptions'),
    newWordCloudOptions,

    -- ** WordCloudSortConfiguration
    WordCloudSortConfiguration (WordCloudSortConfiguration'),
    newWordCloudSortConfiguration,

    -- ** WordCloudVisual
    WordCloudVisual (WordCloudVisual'),
    newWordCloudVisual,
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
import Amazonka.QuickSight.DescribeAnalysisDefinition
import Amazonka.QuickSight.DescribeAnalysisPermissions
import Amazonka.QuickSight.DescribeDashboard
import Amazonka.QuickSight.DescribeDashboardDefinition
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
import Amazonka.QuickSight.DescribeTemplateDefinition
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
