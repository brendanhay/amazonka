{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.SecurityHub
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2018-10-26@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Security Hub provides you with a comprehensive view of the security
-- state of your Amazon Web Services environment and resources. It also
-- provides you with the readiness status of your environment based on
-- controls from supported security standards. Security Hub collects
-- security data from Amazon Web Services accounts, services, and
-- integrated third-party products and helps you analyze security trends in
-- your environment to identify the highest priority security issues. For
-- more information about Security Hub, see the
-- <https://docs.aws.amazon.com/securityhub/latest/userguide/what-is-securityhub.html Security HubUser Guide>
-- .
--
-- When you use operations in the Security Hub API, the requests are
-- executed only in the Amazon Web Services Region that is currently active
-- or in the specific Amazon Web Services Region that you specify in your
-- request. Any configuration or settings change that results from the
-- operation is applied only to that Region. To make the same change in
-- other Regions, execute the same command for each Region to apply the
-- change to.
--
-- For example, if your Region is set to @us-west-2@, when you use
-- @CreateMembers@ to add a member account to Security Hub, the association
-- of the member account with the administrator account is created only in
-- the @us-west-2@ Region. Security Hub must be enabled for the member
-- account in the same Region that the invitation was sent from.
--
-- The following throttling limits apply to using Security Hub API
-- operations.
--
-- -   @BatchEnableStandards@ - @RateLimit@ of 1 request per second,
--     @BurstLimit@ of 1 request per second.
--
-- -   @GetFindings@ - @RateLimit@ of 3 requests per second. @BurstLimit@
--     of 6 requests per second.
--
-- -   @BatchImportFindings@ - @RateLimit@ of 10 requests per second.
--     @BurstLimit@ of 30 requests per second.
--
-- -   @BatchUpdateFindings@ - @RateLimit@ of 10 requests per second.
--     @BurstLimit@ of 30 requests per second.
--
-- -   @UpdateStandardsControl@ - @RateLimit@ of 1 request per second,
--     @BurstLimit@ of 5 requests per second.
--
-- -   All other operations - @RateLimit@ of 10 requests per second.
--     @BurstLimit@ of 30 requests per second.
module Amazonka.SecurityHub
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** InternalException
    _InternalException,

    -- ** InvalidAccessException
    _InvalidAccessException,

    -- ** InvalidInputException
    _InvalidInputException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ResourceConflictException
    _ResourceConflictException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AcceptAdministratorInvitation
    AcceptAdministratorInvitation (AcceptAdministratorInvitation'),
    newAcceptAdministratorInvitation,
    AcceptAdministratorInvitationResponse (AcceptAdministratorInvitationResponse'),
    newAcceptAdministratorInvitationResponse,

    -- ** BatchDisableStandards
    BatchDisableStandards (BatchDisableStandards'),
    newBatchDisableStandards,
    BatchDisableStandardsResponse (BatchDisableStandardsResponse'),
    newBatchDisableStandardsResponse,

    -- ** BatchEnableStandards
    BatchEnableStandards (BatchEnableStandards'),
    newBatchEnableStandards,
    BatchEnableStandardsResponse (BatchEnableStandardsResponse'),
    newBatchEnableStandardsResponse,

    -- ** BatchImportFindings
    BatchImportFindings (BatchImportFindings'),
    newBatchImportFindings,
    BatchImportFindingsResponse (BatchImportFindingsResponse'),
    newBatchImportFindingsResponse,

    -- ** BatchUpdateFindings
    BatchUpdateFindings (BatchUpdateFindings'),
    newBatchUpdateFindings,
    BatchUpdateFindingsResponse (BatchUpdateFindingsResponse'),
    newBatchUpdateFindingsResponse,

    -- ** CreateActionTarget
    CreateActionTarget (CreateActionTarget'),
    newCreateActionTarget,
    CreateActionTargetResponse (CreateActionTargetResponse'),
    newCreateActionTargetResponse,

    -- ** CreateFindingAggregator
    CreateFindingAggregator (CreateFindingAggregator'),
    newCreateFindingAggregator,
    CreateFindingAggregatorResponse (CreateFindingAggregatorResponse'),
    newCreateFindingAggregatorResponse,

    -- ** CreateInsight
    CreateInsight (CreateInsight'),
    newCreateInsight,
    CreateInsightResponse (CreateInsightResponse'),
    newCreateInsightResponse,

    -- ** CreateMembers
    CreateMembers (CreateMembers'),
    newCreateMembers,
    CreateMembersResponse (CreateMembersResponse'),
    newCreateMembersResponse,

    -- ** DeclineInvitations
    DeclineInvitations (DeclineInvitations'),
    newDeclineInvitations,
    DeclineInvitationsResponse (DeclineInvitationsResponse'),
    newDeclineInvitationsResponse,

    -- ** DeleteActionTarget
    DeleteActionTarget (DeleteActionTarget'),
    newDeleteActionTarget,
    DeleteActionTargetResponse (DeleteActionTargetResponse'),
    newDeleteActionTargetResponse,

    -- ** DeleteFindingAggregator
    DeleteFindingAggregator (DeleteFindingAggregator'),
    newDeleteFindingAggregator,
    DeleteFindingAggregatorResponse (DeleteFindingAggregatorResponse'),
    newDeleteFindingAggregatorResponse,

    -- ** DeleteInsight
    DeleteInsight (DeleteInsight'),
    newDeleteInsight,
    DeleteInsightResponse (DeleteInsightResponse'),
    newDeleteInsightResponse,

    -- ** DeleteInvitations
    DeleteInvitations (DeleteInvitations'),
    newDeleteInvitations,
    DeleteInvitationsResponse (DeleteInvitationsResponse'),
    newDeleteInvitationsResponse,

    -- ** DeleteMembers
    DeleteMembers (DeleteMembers'),
    newDeleteMembers,
    DeleteMembersResponse (DeleteMembersResponse'),
    newDeleteMembersResponse,

    -- ** DescribeActionTargets (Paginated)
    DescribeActionTargets (DescribeActionTargets'),
    newDescribeActionTargets,
    DescribeActionTargetsResponse (DescribeActionTargetsResponse'),
    newDescribeActionTargetsResponse,

    -- ** DescribeHub
    DescribeHub (DescribeHub'),
    newDescribeHub,
    DescribeHubResponse (DescribeHubResponse'),
    newDescribeHubResponse,

    -- ** DescribeOrganizationConfiguration
    DescribeOrganizationConfiguration (DescribeOrganizationConfiguration'),
    newDescribeOrganizationConfiguration,
    DescribeOrganizationConfigurationResponse (DescribeOrganizationConfigurationResponse'),
    newDescribeOrganizationConfigurationResponse,

    -- ** DescribeProducts (Paginated)
    DescribeProducts (DescribeProducts'),
    newDescribeProducts,
    DescribeProductsResponse (DescribeProductsResponse'),
    newDescribeProductsResponse,

    -- ** DescribeStandards (Paginated)
    DescribeStandards (DescribeStandards'),
    newDescribeStandards,
    DescribeStandardsResponse (DescribeStandardsResponse'),
    newDescribeStandardsResponse,

    -- ** DescribeStandardsControls (Paginated)
    DescribeStandardsControls (DescribeStandardsControls'),
    newDescribeStandardsControls,
    DescribeStandardsControlsResponse (DescribeStandardsControlsResponse'),
    newDescribeStandardsControlsResponse,

    -- ** DisableImportFindingsForProduct
    DisableImportFindingsForProduct (DisableImportFindingsForProduct'),
    newDisableImportFindingsForProduct,
    DisableImportFindingsForProductResponse (DisableImportFindingsForProductResponse'),
    newDisableImportFindingsForProductResponse,

    -- ** DisableOrganizationAdminAccount
    DisableOrganizationAdminAccount (DisableOrganizationAdminAccount'),
    newDisableOrganizationAdminAccount,
    DisableOrganizationAdminAccountResponse (DisableOrganizationAdminAccountResponse'),
    newDisableOrganizationAdminAccountResponse,

    -- ** DisableSecurityHub
    DisableSecurityHub (DisableSecurityHub'),
    newDisableSecurityHub,
    DisableSecurityHubResponse (DisableSecurityHubResponse'),
    newDisableSecurityHubResponse,

    -- ** DisassociateFromAdministratorAccount
    DisassociateFromAdministratorAccount (DisassociateFromAdministratorAccount'),
    newDisassociateFromAdministratorAccount,
    DisassociateFromAdministratorAccountResponse (DisassociateFromAdministratorAccountResponse'),
    newDisassociateFromAdministratorAccountResponse,

    -- ** DisassociateMembers
    DisassociateMembers (DisassociateMembers'),
    newDisassociateMembers,
    DisassociateMembersResponse (DisassociateMembersResponse'),
    newDisassociateMembersResponse,

    -- ** EnableImportFindingsForProduct
    EnableImportFindingsForProduct (EnableImportFindingsForProduct'),
    newEnableImportFindingsForProduct,
    EnableImportFindingsForProductResponse (EnableImportFindingsForProductResponse'),
    newEnableImportFindingsForProductResponse,

    -- ** EnableOrganizationAdminAccount
    EnableOrganizationAdminAccount (EnableOrganizationAdminAccount'),
    newEnableOrganizationAdminAccount,
    EnableOrganizationAdminAccountResponse (EnableOrganizationAdminAccountResponse'),
    newEnableOrganizationAdminAccountResponse,

    -- ** EnableSecurityHub
    EnableSecurityHub (EnableSecurityHub'),
    newEnableSecurityHub,
    EnableSecurityHubResponse (EnableSecurityHubResponse'),
    newEnableSecurityHubResponse,

    -- ** GetAdministratorAccount
    GetAdministratorAccount (GetAdministratorAccount'),
    newGetAdministratorAccount,
    GetAdministratorAccountResponse (GetAdministratorAccountResponse'),
    newGetAdministratorAccountResponse,

    -- ** GetEnabledStandards (Paginated)
    GetEnabledStandards (GetEnabledStandards'),
    newGetEnabledStandards,
    GetEnabledStandardsResponse (GetEnabledStandardsResponse'),
    newGetEnabledStandardsResponse,

    -- ** GetFindingAggregator
    GetFindingAggregator (GetFindingAggregator'),
    newGetFindingAggregator,
    GetFindingAggregatorResponse (GetFindingAggregatorResponse'),
    newGetFindingAggregatorResponse,

    -- ** GetFindings (Paginated)
    GetFindings (GetFindings'),
    newGetFindings,
    GetFindingsResponse (GetFindingsResponse'),
    newGetFindingsResponse,

    -- ** GetInsightResults
    GetInsightResults (GetInsightResults'),
    newGetInsightResults,
    GetInsightResultsResponse (GetInsightResultsResponse'),
    newGetInsightResultsResponse,

    -- ** GetInsights (Paginated)
    GetInsights (GetInsights'),
    newGetInsights,
    GetInsightsResponse (GetInsightsResponse'),
    newGetInsightsResponse,

    -- ** GetInvitationsCount
    GetInvitationsCount (GetInvitationsCount'),
    newGetInvitationsCount,
    GetInvitationsCountResponse (GetInvitationsCountResponse'),
    newGetInvitationsCountResponse,

    -- ** GetMembers
    GetMembers (GetMembers'),
    newGetMembers,
    GetMembersResponse (GetMembersResponse'),
    newGetMembersResponse,

    -- ** InviteMembers
    InviteMembers (InviteMembers'),
    newInviteMembers,
    InviteMembersResponse (InviteMembersResponse'),
    newInviteMembersResponse,

    -- ** ListEnabledProductsForImport (Paginated)
    ListEnabledProductsForImport (ListEnabledProductsForImport'),
    newListEnabledProductsForImport,
    ListEnabledProductsForImportResponse (ListEnabledProductsForImportResponse'),
    newListEnabledProductsForImportResponse,

    -- ** ListFindingAggregators (Paginated)
    ListFindingAggregators (ListFindingAggregators'),
    newListFindingAggregators,
    ListFindingAggregatorsResponse (ListFindingAggregatorsResponse'),
    newListFindingAggregatorsResponse,

    -- ** ListInvitations (Paginated)
    ListInvitations (ListInvitations'),
    newListInvitations,
    ListInvitationsResponse (ListInvitationsResponse'),
    newListInvitationsResponse,

    -- ** ListMembers (Paginated)
    ListMembers (ListMembers'),
    newListMembers,
    ListMembersResponse (ListMembersResponse'),
    newListMembersResponse,

    -- ** ListOrganizationAdminAccounts (Paginated)
    ListOrganizationAdminAccounts (ListOrganizationAdminAccounts'),
    newListOrganizationAdminAccounts,
    ListOrganizationAdminAccountsResponse (ListOrganizationAdminAccountsResponse'),
    newListOrganizationAdminAccountsResponse,

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

    -- ** UpdateActionTarget
    UpdateActionTarget (UpdateActionTarget'),
    newUpdateActionTarget,
    UpdateActionTargetResponse (UpdateActionTargetResponse'),
    newUpdateActionTargetResponse,

    -- ** UpdateFindingAggregator
    UpdateFindingAggregator (UpdateFindingAggregator'),
    newUpdateFindingAggregator,
    UpdateFindingAggregatorResponse (UpdateFindingAggregatorResponse'),
    newUpdateFindingAggregatorResponse,

    -- ** UpdateFindings
    UpdateFindings (UpdateFindings'),
    newUpdateFindings,
    UpdateFindingsResponse (UpdateFindingsResponse'),
    newUpdateFindingsResponse,

    -- ** UpdateInsight
    UpdateInsight (UpdateInsight'),
    newUpdateInsight,
    UpdateInsightResponse (UpdateInsightResponse'),
    newUpdateInsightResponse,

    -- ** UpdateOrganizationConfiguration
    UpdateOrganizationConfiguration (UpdateOrganizationConfiguration'),
    newUpdateOrganizationConfiguration,
    UpdateOrganizationConfigurationResponse (UpdateOrganizationConfigurationResponse'),
    newUpdateOrganizationConfigurationResponse,

    -- ** UpdateSecurityHubConfiguration
    UpdateSecurityHubConfiguration (UpdateSecurityHubConfiguration'),
    newUpdateSecurityHubConfiguration,
    UpdateSecurityHubConfigurationResponse (UpdateSecurityHubConfigurationResponse'),
    newUpdateSecurityHubConfigurationResponse,

    -- ** UpdateStandardsControl
    UpdateStandardsControl (UpdateStandardsControl'),
    newUpdateStandardsControl,
    UpdateStandardsControlResponse (UpdateStandardsControlResponse'),
    newUpdateStandardsControlResponse,

    -- * Types

    -- ** AdminStatus
    AdminStatus (..),

    -- ** AutoEnableStandards
    AutoEnableStandards (..),

    -- ** AwsIamAccessKeyStatus
    AwsIamAccessKeyStatus (..),

    -- ** AwsS3BucketNotificationConfigurationS3KeyFilterRuleName
    AwsS3BucketNotificationConfigurationS3KeyFilterRuleName (..),

    -- ** ComplianceStatus
    ComplianceStatus (..),

    -- ** ControlStatus
    ControlStatus (..),

    -- ** DateRangeUnit
    DateRangeUnit (..),

    -- ** IntegrationType
    IntegrationType (..),

    -- ** MalwareState
    MalwareState (..),

    -- ** MalwareType
    MalwareType (..),

    -- ** MapFilterComparison
    MapFilterComparison (..),

    -- ** NetworkDirection
    NetworkDirection (..),

    -- ** Partition
    Partition (..),

    -- ** RecordState
    RecordState (..),

    -- ** SeverityLabel
    SeverityLabel (..),

    -- ** SeverityRating
    SeverityRating (..),

    -- ** SortOrder
    SortOrder (..),

    -- ** StandardsStatus
    StandardsStatus (..),

    -- ** StatusReasonCode
    StatusReasonCode (..),

    -- ** StringFilterComparison
    StringFilterComparison (..),

    -- ** ThreatIntelIndicatorCategory
    ThreatIntelIndicatorCategory (..),

    -- ** ThreatIntelIndicatorType
    ThreatIntelIndicatorType (..),

    -- ** VerificationState
    VerificationState (..),

    -- ** VulnerabilityFixAvailable
    VulnerabilityFixAvailable (..),

    -- ** WorkflowState
    WorkflowState (..),

    -- ** WorkflowStatus
    WorkflowStatus (..),

    -- ** AccountDetails
    AccountDetails (AccountDetails'),
    newAccountDetails,

    -- ** Action
    Action (Action'),
    newAction,

    -- ** ActionLocalIpDetails
    ActionLocalIpDetails (ActionLocalIpDetails'),
    newActionLocalIpDetails,

    -- ** ActionLocalPortDetails
    ActionLocalPortDetails (ActionLocalPortDetails'),
    newActionLocalPortDetails,

    -- ** ActionRemoteIpDetails
    ActionRemoteIpDetails (ActionRemoteIpDetails'),
    newActionRemoteIpDetails,

    -- ** ActionRemotePortDetails
    ActionRemotePortDetails (ActionRemotePortDetails'),
    newActionRemotePortDetails,

    -- ** ActionTarget
    ActionTarget (ActionTarget'),
    newActionTarget,

    -- ** Adjustment
    Adjustment (Adjustment'),
    newAdjustment,

    -- ** AdminAccount
    AdminAccount (AdminAccount'),
    newAdminAccount,

    -- ** AvailabilityZone
    AvailabilityZone (AvailabilityZone'),
    newAvailabilityZone,

    -- ** AwsApiCallAction
    AwsApiCallAction (AwsApiCallAction'),
    newAwsApiCallAction,

    -- ** AwsApiCallActionDomainDetails
    AwsApiCallActionDomainDetails (AwsApiCallActionDomainDetails'),
    newAwsApiCallActionDomainDetails,

    -- ** AwsApiGatewayAccessLogSettings
    AwsApiGatewayAccessLogSettings (AwsApiGatewayAccessLogSettings'),
    newAwsApiGatewayAccessLogSettings,

    -- ** AwsApiGatewayCanarySettings
    AwsApiGatewayCanarySettings (AwsApiGatewayCanarySettings'),
    newAwsApiGatewayCanarySettings,

    -- ** AwsApiGatewayEndpointConfiguration
    AwsApiGatewayEndpointConfiguration (AwsApiGatewayEndpointConfiguration'),
    newAwsApiGatewayEndpointConfiguration,

    -- ** AwsApiGatewayMethodSettings
    AwsApiGatewayMethodSettings (AwsApiGatewayMethodSettings'),
    newAwsApiGatewayMethodSettings,

    -- ** AwsApiGatewayRestApiDetails
    AwsApiGatewayRestApiDetails (AwsApiGatewayRestApiDetails'),
    newAwsApiGatewayRestApiDetails,

    -- ** AwsApiGatewayStageDetails
    AwsApiGatewayStageDetails (AwsApiGatewayStageDetails'),
    newAwsApiGatewayStageDetails,

    -- ** AwsApiGatewayV2ApiDetails
    AwsApiGatewayV2ApiDetails (AwsApiGatewayV2ApiDetails'),
    newAwsApiGatewayV2ApiDetails,

    -- ** AwsApiGatewayV2RouteSettings
    AwsApiGatewayV2RouteSettings (AwsApiGatewayV2RouteSettings'),
    newAwsApiGatewayV2RouteSettings,

    -- ** AwsApiGatewayV2StageDetails
    AwsApiGatewayV2StageDetails (AwsApiGatewayV2StageDetails'),
    newAwsApiGatewayV2StageDetails,

    -- ** AwsAutoScalingAutoScalingGroupAvailabilityZonesListDetails
    AwsAutoScalingAutoScalingGroupAvailabilityZonesListDetails (AwsAutoScalingAutoScalingGroupAvailabilityZonesListDetails'),
    newAwsAutoScalingAutoScalingGroupAvailabilityZonesListDetails,

    -- ** AwsAutoScalingAutoScalingGroupDetails
    AwsAutoScalingAutoScalingGroupDetails (AwsAutoScalingAutoScalingGroupDetails'),
    newAwsAutoScalingAutoScalingGroupDetails,

    -- ** AwsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification
    AwsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification (AwsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification'),
    newAwsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification,

    -- ** AwsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails
    AwsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails (AwsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails'),
    newAwsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails,

    -- ** AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails
    AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails (AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails'),
    newAwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails,

    -- ** AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateDetails
    AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateDetails (AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateDetails'),
    newAwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateDetails,

    -- ** AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification
    AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification (AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification'),
    newAwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification,

    -- ** AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateOverridesListDetails
    AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateOverridesListDetails (AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateOverridesListDetails'),
    newAwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateOverridesListDetails,

    -- ** AwsAutoScalingLaunchConfigurationBlockDeviceMappingsDetails
    AwsAutoScalingLaunchConfigurationBlockDeviceMappingsDetails (AwsAutoScalingLaunchConfigurationBlockDeviceMappingsDetails'),
    newAwsAutoScalingLaunchConfigurationBlockDeviceMappingsDetails,

    -- ** AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails
    AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails (AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails'),
    newAwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails,

    -- ** AwsAutoScalingLaunchConfigurationDetails
    AwsAutoScalingLaunchConfigurationDetails (AwsAutoScalingLaunchConfigurationDetails'),
    newAwsAutoScalingLaunchConfigurationDetails,

    -- ** AwsAutoScalingLaunchConfigurationInstanceMonitoringDetails
    AwsAutoScalingLaunchConfigurationInstanceMonitoringDetails (AwsAutoScalingLaunchConfigurationInstanceMonitoringDetails'),
    newAwsAutoScalingLaunchConfigurationInstanceMonitoringDetails,

    -- ** AwsAutoScalingLaunchConfigurationMetadataOptions
    AwsAutoScalingLaunchConfigurationMetadataOptions (AwsAutoScalingLaunchConfigurationMetadataOptions'),
    newAwsAutoScalingLaunchConfigurationMetadataOptions,

    -- ** AwsBackupBackupPlanAdvancedBackupSettingsDetails
    AwsBackupBackupPlanAdvancedBackupSettingsDetails (AwsBackupBackupPlanAdvancedBackupSettingsDetails'),
    newAwsBackupBackupPlanAdvancedBackupSettingsDetails,

    -- ** AwsBackupBackupPlanBackupPlanDetails
    AwsBackupBackupPlanBackupPlanDetails (AwsBackupBackupPlanBackupPlanDetails'),
    newAwsBackupBackupPlanBackupPlanDetails,

    -- ** AwsBackupBackupPlanDetails
    AwsBackupBackupPlanDetails (AwsBackupBackupPlanDetails'),
    newAwsBackupBackupPlanDetails,

    -- ** AwsBackupBackupPlanLifecycleDetails
    AwsBackupBackupPlanLifecycleDetails (AwsBackupBackupPlanLifecycleDetails'),
    newAwsBackupBackupPlanLifecycleDetails,

    -- ** AwsBackupBackupPlanRuleCopyActionsDetails
    AwsBackupBackupPlanRuleCopyActionsDetails (AwsBackupBackupPlanRuleCopyActionsDetails'),
    newAwsBackupBackupPlanRuleCopyActionsDetails,

    -- ** AwsBackupBackupPlanRuleDetails
    AwsBackupBackupPlanRuleDetails (AwsBackupBackupPlanRuleDetails'),
    newAwsBackupBackupPlanRuleDetails,

    -- ** AwsBackupBackupVaultDetails
    AwsBackupBackupVaultDetails (AwsBackupBackupVaultDetails'),
    newAwsBackupBackupVaultDetails,

    -- ** AwsBackupBackupVaultNotificationsDetails
    AwsBackupBackupVaultNotificationsDetails (AwsBackupBackupVaultNotificationsDetails'),
    newAwsBackupBackupVaultNotificationsDetails,

    -- ** AwsBackupRecoveryPointCalculatedLifecycleDetails
    AwsBackupRecoveryPointCalculatedLifecycleDetails (AwsBackupRecoveryPointCalculatedLifecycleDetails'),
    newAwsBackupRecoveryPointCalculatedLifecycleDetails,

    -- ** AwsBackupRecoveryPointCreatedByDetails
    AwsBackupRecoveryPointCreatedByDetails (AwsBackupRecoveryPointCreatedByDetails'),
    newAwsBackupRecoveryPointCreatedByDetails,

    -- ** AwsBackupRecoveryPointDetails
    AwsBackupRecoveryPointDetails (AwsBackupRecoveryPointDetails'),
    newAwsBackupRecoveryPointDetails,

    -- ** AwsBackupRecoveryPointLifecycleDetails
    AwsBackupRecoveryPointLifecycleDetails (AwsBackupRecoveryPointLifecycleDetails'),
    newAwsBackupRecoveryPointLifecycleDetails,

    -- ** AwsCertificateManagerCertificateDetails
    AwsCertificateManagerCertificateDetails (AwsCertificateManagerCertificateDetails'),
    newAwsCertificateManagerCertificateDetails,

    -- ** AwsCertificateManagerCertificateDomainValidationOption
    AwsCertificateManagerCertificateDomainValidationOption (AwsCertificateManagerCertificateDomainValidationOption'),
    newAwsCertificateManagerCertificateDomainValidationOption,

    -- ** AwsCertificateManagerCertificateExtendedKeyUsage
    AwsCertificateManagerCertificateExtendedKeyUsage (AwsCertificateManagerCertificateExtendedKeyUsage'),
    newAwsCertificateManagerCertificateExtendedKeyUsage,

    -- ** AwsCertificateManagerCertificateKeyUsage
    AwsCertificateManagerCertificateKeyUsage (AwsCertificateManagerCertificateKeyUsage'),
    newAwsCertificateManagerCertificateKeyUsage,

    -- ** AwsCertificateManagerCertificateOptions
    AwsCertificateManagerCertificateOptions (AwsCertificateManagerCertificateOptions'),
    newAwsCertificateManagerCertificateOptions,

    -- ** AwsCertificateManagerCertificateRenewalSummary
    AwsCertificateManagerCertificateRenewalSummary (AwsCertificateManagerCertificateRenewalSummary'),
    newAwsCertificateManagerCertificateRenewalSummary,

    -- ** AwsCertificateManagerCertificateResourceRecord
    AwsCertificateManagerCertificateResourceRecord (AwsCertificateManagerCertificateResourceRecord'),
    newAwsCertificateManagerCertificateResourceRecord,

    -- ** AwsCloudFormationStackDetails
    AwsCloudFormationStackDetails (AwsCloudFormationStackDetails'),
    newAwsCloudFormationStackDetails,

    -- ** AwsCloudFormationStackDriftInformationDetails
    AwsCloudFormationStackDriftInformationDetails (AwsCloudFormationStackDriftInformationDetails'),
    newAwsCloudFormationStackDriftInformationDetails,

    -- ** AwsCloudFormationStackOutputsDetails
    AwsCloudFormationStackOutputsDetails (AwsCloudFormationStackOutputsDetails'),
    newAwsCloudFormationStackOutputsDetails,

    -- ** AwsCloudFrontDistributionCacheBehavior
    AwsCloudFrontDistributionCacheBehavior (AwsCloudFrontDistributionCacheBehavior'),
    newAwsCloudFrontDistributionCacheBehavior,

    -- ** AwsCloudFrontDistributionCacheBehaviors
    AwsCloudFrontDistributionCacheBehaviors (AwsCloudFrontDistributionCacheBehaviors'),
    newAwsCloudFrontDistributionCacheBehaviors,

    -- ** AwsCloudFrontDistributionDefaultCacheBehavior
    AwsCloudFrontDistributionDefaultCacheBehavior (AwsCloudFrontDistributionDefaultCacheBehavior'),
    newAwsCloudFrontDistributionDefaultCacheBehavior,

    -- ** AwsCloudFrontDistributionDetails
    AwsCloudFrontDistributionDetails (AwsCloudFrontDistributionDetails'),
    newAwsCloudFrontDistributionDetails,

    -- ** AwsCloudFrontDistributionLogging
    AwsCloudFrontDistributionLogging (AwsCloudFrontDistributionLogging'),
    newAwsCloudFrontDistributionLogging,

    -- ** AwsCloudFrontDistributionOriginCustomOriginConfig
    AwsCloudFrontDistributionOriginCustomOriginConfig (AwsCloudFrontDistributionOriginCustomOriginConfig'),
    newAwsCloudFrontDistributionOriginCustomOriginConfig,

    -- ** AwsCloudFrontDistributionOriginGroup
    AwsCloudFrontDistributionOriginGroup (AwsCloudFrontDistributionOriginGroup'),
    newAwsCloudFrontDistributionOriginGroup,

    -- ** AwsCloudFrontDistributionOriginGroupFailover
    AwsCloudFrontDistributionOriginGroupFailover (AwsCloudFrontDistributionOriginGroupFailover'),
    newAwsCloudFrontDistributionOriginGroupFailover,

    -- ** AwsCloudFrontDistributionOriginGroupFailoverStatusCodes
    AwsCloudFrontDistributionOriginGroupFailoverStatusCodes (AwsCloudFrontDistributionOriginGroupFailoverStatusCodes'),
    newAwsCloudFrontDistributionOriginGroupFailoverStatusCodes,

    -- ** AwsCloudFrontDistributionOriginGroups
    AwsCloudFrontDistributionOriginGroups (AwsCloudFrontDistributionOriginGroups'),
    newAwsCloudFrontDistributionOriginGroups,

    -- ** AwsCloudFrontDistributionOriginItem
    AwsCloudFrontDistributionOriginItem (AwsCloudFrontDistributionOriginItem'),
    newAwsCloudFrontDistributionOriginItem,

    -- ** AwsCloudFrontDistributionOriginS3OriginConfig
    AwsCloudFrontDistributionOriginS3OriginConfig (AwsCloudFrontDistributionOriginS3OriginConfig'),
    newAwsCloudFrontDistributionOriginS3OriginConfig,

    -- ** AwsCloudFrontDistributionOriginSslProtocols
    AwsCloudFrontDistributionOriginSslProtocols (AwsCloudFrontDistributionOriginSslProtocols'),
    newAwsCloudFrontDistributionOriginSslProtocols,

    -- ** AwsCloudFrontDistributionOrigins
    AwsCloudFrontDistributionOrigins (AwsCloudFrontDistributionOrigins'),
    newAwsCloudFrontDistributionOrigins,

    -- ** AwsCloudFrontDistributionViewerCertificate
    AwsCloudFrontDistributionViewerCertificate (AwsCloudFrontDistributionViewerCertificate'),
    newAwsCloudFrontDistributionViewerCertificate,

    -- ** AwsCloudTrailTrailDetails
    AwsCloudTrailTrailDetails (AwsCloudTrailTrailDetails'),
    newAwsCloudTrailTrailDetails,

    -- ** AwsCloudWatchAlarmDetails
    AwsCloudWatchAlarmDetails (AwsCloudWatchAlarmDetails'),
    newAwsCloudWatchAlarmDetails,

    -- ** AwsCloudWatchAlarmDimensionsDetails
    AwsCloudWatchAlarmDimensionsDetails (AwsCloudWatchAlarmDimensionsDetails'),
    newAwsCloudWatchAlarmDimensionsDetails,

    -- ** AwsCodeBuildProjectArtifactsDetails
    AwsCodeBuildProjectArtifactsDetails (AwsCodeBuildProjectArtifactsDetails'),
    newAwsCodeBuildProjectArtifactsDetails,

    -- ** AwsCodeBuildProjectDetails
    AwsCodeBuildProjectDetails (AwsCodeBuildProjectDetails'),
    newAwsCodeBuildProjectDetails,

    -- ** AwsCodeBuildProjectEnvironment
    AwsCodeBuildProjectEnvironment (AwsCodeBuildProjectEnvironment'),
    newAwsCodeBuildProjectEnvironment,

    -- ** AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails
    AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails (AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails'),
    newAwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails,

    -- ** AwsCodeBuildProjectEnvironmentRegistryCredential
    AwsCodeBuildProjectEnvironmentRegistryCredential (AwsCodeBuildProjectEnvironmentRegistryCredential'),
    newAwsCodeBuildProjectEnvironmentRegistryCredential,

    -- ** AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails
    AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails (AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails'),
    newAwsCodeBuildProjectLogsConfigCloudWatchLogsDetails,

    -- ** AwsCodeBuildProjectLogsConfigDetails
    AwsCodeBuildProjectLogsConfigDetails (AwsCodeBuildProjectLogsConfigDetails'),
    newAwsCodeBuildProjectLogsConfigDetails,

    -- ** AwsCodeBuildProjectLogsConfigS3LogsDetails
    AwsCodeBuildProjectLogsConfigS3LogsDetails (AwsCodeBuildProjectLogsConfigS3LogsDetails'),
    newAwsCodeBuildProjectLogsConfigS3LogsDetails,

    -- ** AwsCodeBuildProjectSource
    AwsCodeBuildProjectSource (AwsCodeBuildProjectSource'),
    newAwsCodeBuildProjectSource,

    -- ** AwsCodeBuildProjectVpcConfig
    AwsCodeBuildProjectVpcConfig (AwsCodeBuildProjectVpcConfig'),
    newAwsCodeBuildProjectVpcConfig,

    -- ** AwsCorsConfiguration
    AwsCorsConfiguration (AwsCorsConfiguration'),
    newAwsCorsConfiguration,

    -- ** AwsDynamoDbTableAttributeDefinition
    AwsDynamoDbTableAttributeDefinition (AwsDynamoDbTableAttributeDefinition'),
    newAwsDynamoDbTableAttributeDefinition,

    -- ** AwsDynamoDbTableBillingModeSummary
    AwsDynamoDbTableBillingModeSummary (AwsDynamoDbTableBillingModeSummary'),
    newAwsDynamoDbTableBillingModeSummary,

    -- ** AwsDynamoDbTableDetails
    AwsDynamoDbTableDetails (AwsDynamoDbTableDetails'),
    newAwsDynamoDbTableDetails,

    -- ** AwsDynamoDbTableGlobalSecondaryIndex
    AwsDynamoDbTableGlobalSecondaryIndex (AwsDynamoDbTableGlobalSecondaryIndex'),
    newAwsDynamoDbTableGlobalSecondaryIndex,

    -- ** AwsDynamoDbTableKeySchema
    AwsDynamoDbTableKeySchema (AwsDynamoDbTableKeySchema'),
    newAwsDynamoDbTableKeySchema,

    -- ** AwsDynamoDbTableLocalSecondaryIndex
    AwsDynamoDbTableLocalSecondaryIndex (AwsDynamoDbTableLocalSecondaryIndex'),
    newAwsDynamoDbTableLocalSecondaryIndex,

    -- ** AwsDynamoDbTableProjection
    AwsDynamoDbTableProjection (AwsDynamoDbTableProjection'),
    newAwsDynamoDbTableProjection,

    -- ** AwsDynamoDbTableProvisionedThroughput
    AwsDynamoDbTableProvisionedThroughput (AwsDynamoDbTableProvisionedThroughput'),
    newAwsDynamoDbTableProvisionedThroughput,

    -- ** AwsDynamoDbTableProvisionedThroughputOverride
    AwsDynamoDbTableProvisionedThroughputOverride (AwsDynamoDbTableProvisionedThroughputOverride'),
    newAwsDynamoDbTableProvisionedThroughputOverride,

    -- ** AwsDynamoDbTableReplica
    AwsDynamoDbTableReplica (AwsDynamoDbTableReplica'),
    newAwsDynamoDbTableReplica,

    -- ** AwsDynamoDbTableReplicaGlobalSecondaryIndex
    AwsDynamoDbTableReplicaGlobalSecondaryIndex (AwsDynamoDbTableReplicaGlobalSecondaryIndex'),
    newAwsDynamoDbTableReplicaGlobalSecondaryIndex,

    -- ** AwsDynamoDbTableRestoreSummary
    AwsDynamoDbTableRestoreSummary (AwsDynamoDbTableRestoreSummary'),
    newAwsDynamoDbTableRestoreSummary,

    -- ** AwsDynamoDbTableSseDescription
    AwsDynamoDbTableSseDescription (AwsDynamoDbTableSseDescription'),
    newAwsDynamoDbTableSseDescription,

    -- ** AwsDynamoDbTableStreamSpecification
    AwsDynamoDbTableStreamSpecification (AwsDynamoDbTableStreamSpecification'),
    newAwsDynamoDbTableStreamSpecification,

    -- ** AwsEc2EipDetails
    AwsEc2EipDetails (AwsEc2EipDetails'),
    newAwsEc2EipDetails,

    -- ** AwsEc2InstanceDetails
    AwsEc2InstanceDetails (AwsEc2InstanceDetails'),
    newAwsEc2InstanceDetails,

    -- ** AwsEc2InstanceMetadataOptions
    AwsEc2InstanceMetadataOptions (AwsEc2InstanceMetadataOptions'),
    newAwsEc2InstanceMetadataOptions,

    -- ** AwsEc2InstanceNetworkInterfacesDetails
    AwsEc2InstanceNetworkInterfacesDetails (AwsEc2InstanceNetworkInterfacesDetails'),
    newAwsEc2InstanceNetworkInterfacesDetails,

    -- ** AwsEc2LaunchTemplateDataBlockDeviceMappingSetDetails
    AwsEc2LaunchTemplateDataBlockDeviceMappingSetDetails (AwsEc2LaunchTemplateDataBlockDeviceMappingSetDetails'),
    newAwsEc2LaunchTemplateDataBlockDeviceMappingSetDetails,

    -- ** AwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails
    AwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails (AwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails'),
    newAwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails,

    -- ** AwsEc2LaunchTemplateDataCapacityReservationSpecificationCapacityReservationTargetDetails
    AwsEc2LaunchTemplateDataCapacityReservationSpecificationCapacityReservationTargetDetails (AwsEc2LaunchTemplateDataCapacityReservationSpecificationCapacityReservationTargetDetails'),
    newAwsEc2LaunchTemplateDataCapacityReservationSpecificationCapacityReservationTargetDetails,

    -- ** AwsEc2LaunchTemplateDataCapacityReservationSpecificationDetails
    AwsEc2LaunchTemplateDataCapacityReservationSpecificationDetails (AwsEc2LaunchTemplateDataCapacityReservationSpecificationDetails'),
    newAwsEc2LaunchTemplateDataCapacityReservationSpecificationDetails,

    -- ** AwsEc2LaunchTemplateDataCpuOptionsDetails
    AwsEc2LaunchTemplateDataCpuOptionsDetails (AwsEc2LaunchTemplateDataCpuOptionsDetails'),
    newAwsEc2LaunchTemplateDataCpuOptionsDetails,

    -- ** AwsEc2LaunchTemplateDataCreditSpecificationDetails
    AwsEc2LaunchTemplateDataCreditSpecificationDetails (AwsEc2LaunchTemplateDataCreditSpecificationDetails'),
    newAwsEc2LaunchTemplateDataCreditSpecificationDetails,

    -- ** AwsEc2LaunchTemplateDataDetails
    AwsEc2LaunchTemplateDataDetails (AwsEc2LaunchTemplateDataDetails'),
    newAwsEc2LaunchTemplateDataDetails,

    -- ** AwsEc2LaunchTemplateDataElasticGpuSpecificationSetDetails
    AwsEc2LaunchTemplateDataElasticGpuSpecificationSetDetails (AwsEc2LaunchTemplateDataElasticGpuSpecificationSetDetails'),
    newAwsEc2LaunchTemplateDataElasticGpuSpecificationSetDetails,

    -- ** AwsEc2LaunchTemplateDataElasticInferenceAcceleratorSetDetails
    AwsEc2LaunchTemplateDataElasticInferenceAcceleratorSetDetails (AwsEc2LaunchTemplateDataElasticInferenceAcceleratorSetDetails'),
    newAwsEc2LaunchTemplateDataElasticInferenceAcceleratorSetDetails,

    -- ** AwsEc2LaunchTemplateDataEnclaveOptionsDetails
    AwsEc2LaunchTemplateDataEnclaveOptionsDetails (AwsEc2LaunchTemplateDataEnclaveOptionsDetails'),
    newAwsEc2LaunchTemplateDataEnclaveOptionsDetails,

    -- ** AwsEc2LaunchTemplateDataHibernationOptionsDetails
    AwsEc2LaunchTemplateDataHibernationOptionsDetails (AwsEc2LaunchTemplateDataHibernationOptionsDetails'),
    newAwsEc2LaunchTemplateDataHibernationOptionsDetails,

    -- ** AwsEc2LaunchTemplateDataIamInstanceProfileDetails
    AwsEc2LaunchTemplateDataIamInstanceProfileDetails (AwsEc2LaunchTemplateDataIamInstanceProfileDetails'),
    newAwsEc2LaunchTemplateDataIamInstanceProfileDetails,

    -- ** AwsEc2LaunchTemplateDataInstanceMarketOptionsDetails
    AwsEc2LaunchTemplateDataInstanceMarketOptionsDetails (AwsEc2LaunchTemplateDataInstanceMarketOptionsDetails'),
    newAwsEc2LaunchTemplateDataInstanceMarketOptionsDetails,

    -- ** AwsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails
    AwsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails (AwsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails'),
    newAwsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails,

    -- ** AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorCountDetails
    AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorCountDetails (AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorCountDetails'),
    newAwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorCountDetails,

    -- ** AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorTotalMemoryMiBDetails
    AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorTotalMemoryMiBDetails (AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorTotalMemoryMiBDetails'),
    newAwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorTotalMemoryMiBDetails,

    -- ** AwsEc2LaunchTemplateDataInstanceRequirementsBaselineEbsBandwidthMbpsDetails
    AwsEc2LaunchTemplateDataInstanceRequirementsBaselineEbsBandwidthMbpsDetails (AwsEc2LaunchTemplateDataInstanceRequirementsBaselineEbsBandwidthMbpsDetails'),
    newAwsEc2LaunchTemplateDataInstanceRequirementsBaselineEbsBandwidthMbpsDetails,

    -- ** AwsEc2LaunchTemplateDataInstanceRequirementsDetails
    AwsEc2LaunchTemplateDataInstanceRequirementsDetails (AwsEc2LaunchTemplateDataInstanceRequirementsDetails'),
    newAwsEc2LaunchTemplateDataInstanceRequirementsDetails,

    -- ** AwsEc2LaunchTemplateDataInstanceRequirementsMemoryGiBPerVCpuDetails
    AwsEc2LaunchTemplateDataInstanceRequirementsMemoryGiBPerVCpuDetails (AwsEc2LaunchTemplateDataInstanceRequirementsMemoryGiBPerVCpuDetails'),
    newAwsEc2LaunchTemplateDataInstanceRequirementsMemoryGiBPerVCpuDetails,

    -- ** AwsEc2LaunchTemplateDataInstanceRequirementsMemoryMiBDetails
    AwsEc2LaunchTemplateDataInstanceRequirementsMemoryMiBDetails (AwsEc2LaunchTemplateDataInstanceRequirementsMemoryMiBDetails'),
    newAwsEc2LaunchTemplateDataInstanceRequirementsMemoryMiBDetails,

    -- ** AwsEc2LaunchTemplateDataInstanceRequirementsNetworkInterfaceCountDetails
    AwsEc2LaunchTemplateDataInstanceRequirementsNetworkInterfaceCountDetails (AwsEc2LaunchTemplateDataInstanceRequirementsNetworkInterfaceCountDetails'),
    newAwsEc2LaunchTemplateDataInstanceRequirementsNetworkInterfaceCountDetails,

    -- ** AwsEc2LaunchTemplateDataInstanceRequirementsTotalLocalStorageGBDetails
    AwsEc2LaunchTemplateDataInstanceRequirementsTotalLocalStorageGBDetails (AwsEc2LaunchTemplateDataInstanceRequirementsTotalLocalStorageGBDetails'),
    newAwsEc2LaunchTemplateDataInstanceRequirementsTotalLocalStorageGBDetails,

    -- ** AwsEc2LaunchTemplateDataInstanceRequirementsVCpuCountDetails
    AwsEc2LaunchTemplateDataInstanceRequirementsVCpuCountDetails (AwsEc2LaunchTemplateDataInstanceRequirementsVCpuCountDetails'),
    newAwsEc2LaunchTemplateDataInstanceRequirementsVCpuCountDetails,

    -- ** AwsEc2LaunchTemplateDataLicenseSetDetails
    AwsEc2LaunchTemplateDataLicenseSetDetails (AwsEc2LaunchTemplateDataLicenseSetDetails'),
    newAwsEc2LaunchTemplateDataLicenseSetDetails,

    -- ** AwsEc2LaunchTemplateDataMaintenanceOptionsDetails
    AwsEc2LaunchTemplateDataMaintenanceOptionsDetails (AwsEc2LaunchTemplateDataMaintenanceOptionsDetails'),
    newAwsEc2LaunchTemplateDataMaintenanceOptionsDetails,

    -- ** AwsEc2LaunchTemplateDataMetadataOptionsDetails
    AwsEc2LaunchTemplateDataMetadataOptionsDetails (AwsEc2LaunchTemplateDataMetadataOptionsDetails'),
    newAwsEc2LaunchTemplateDataMetadataOptionsDetails,

    -- ** AwsEc2LaunchTemplateDataMonitoringDetails
    AwsEc2LaunchTemplateDataMonitoringDetails (AwsEc2LaunchTemplateDataMonitoringDetails'),
    newAwsEc2LaunchTemplateDataMonitoringDetails,

    -- ** AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails
    AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails (AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails'),
    newAwsEc2LaunchTemplateDataNetworkInterfaceSetDetails,

    -- ** AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv4PrefixesDetails
    AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv4PrefixesDetails (AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv4PrefixesDetails'),
    newAwsEc2LaunchTemplateDataNetworkInterfaceSetIpv4PrefixesDetails,

    -- ** AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6AddressesDetails
    AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6AddressesDetails (AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6AddressesDetails'),
    newAwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6AddressesDetails,

    -- ** AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6PrefixesDetails
    AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6PrefixesDetails (AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6PrefixesDetails'),
    newAwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6PrefixesDetails,

    -- ** AwsEc2LaunchTemplateDataNetworkInterfaceSetPrivateIpAddressesDetails
    AwsEc2LaunchTemplateDataNetworkInterfaceSetPrivateIpAddressesDetails (AwsEc2LaunchTemplateDataNetworkInterfaceSetPrivateIpAddressesDetails'),
    newAwsEc2LaunchTemplateDataNetworkInterfaceSetPrivateIpAddressesDetails,

    -- ** AwsEc2LaunchTemplateDataPlacementDetails
    AwsEc2LaunchTemplateDataPlacementDetails (AwsEc2LaunchTemplateDataPlacementDetails'),
    newAwsEc2LaunchTemplateDataPlacementDetails,

    -- ** AwsEc2LaunchTemplateDataPrivateDnsNameOptionsDetails
    AwsEc2LaunchTemplateDataPrivateDnsNameOptionsDetails (AwsEc2LaunchTemplateDataPrivateDnsNameOptionsDetails'),
    newAwsEc2LaunchTemplateDataPrivateDnsNameOptionsDetails,

    -- ** AwsEc2LaunchTemplateDetails
    AwsEc2LaunchTemplateDetails (AwsEc2LaunchTemplateDetails'),
    newAwsEc2LaunchTemplateDetails,

    -- ** AwsEc2NetworkAclAssociation
    AwsEc2NetworkAclAssociation (AwsEc2NetworkAclAssociation'),
    newAwsEc2NetworkAclAssociation,

    -- ** AwsEc2NetworkAclDetails
    AwsEc2NetworkAclDetails (AwsEc2NetworkAclDetails'),
    newAwsEc2NetworkAclDetails,

    -- ** AwsEc2NetworkAclEntry
    AwsEc2NetworkAclEntry (AwsEc2NetworkAclEntry'),
    newAwsEc2NetworkAclEntry,

    -- ** AwsEc2NetworkInterfaceAttachment
    AwsEc2NetworkInterfaceAttachment (AwsEc2NetworkInterfaceAttachment'),
    newAwsEc2NetworkInterfaceAttachment,

    -- ** AwsEc2NetworkInterfaceDetails
    AwsEc2NetworkInterfaceDetails (AwsEc2NetworkInterfaceDetails'),
    newAwsEc2NetworkInterfaceDetails,

    -- ** AwsEc2NetworkInterfaceIpV6AddressDetail
    AwsEc2NetworkInterfaceIpV6AddressDetail (AwsEc2NetworkInterfaceIpV6AddressDetail'),
    newAwsEc2NetworkInterfaceIpV6AddressDetail,

    -- ** AwsEc2NetworkInterfacePrivateIpAddressDetail
    AwsEc2NetworkInterfacePrivateIpAddressDetail (AwsEc2NetworkInterfacePrivateIpAddressDetail'),
    newAwsEc2NetworkInterfacePrivateIpAddressDetail,

    -- ** AwsEc2NetworkInterfaceSecurityGroup
    AwsEc2NetworkInterfaceSecurityGroup (AwsEc2NetworkInterfaceSecurityGroup'),
    newAwsEc2NetworkInterfaceSecurityGroup,

    -- ** AwsEc2SecurityGroupDetails
    AwsEc2SecurityGroupDetails (AwsEc2SecurityGroupDetails'),
    newAwsEc2SecurityGroupDetails,

    -- ** AwsEc2SecurityGroupIpPermission
    AwsEc2SecurityGroupIpPermission (AwsEc2SecurityGroupIpPermission'),
    newAwsEc2SecurityGroupIpPermission,

    -- ** AwsEc2SecurityGroupIpRange
    AwsEc2SecurityGroupIpRange (AwsEc2SecurityGroupIpRange'),
    newAwsEc2SecurityGroupIpRange,

    -- ** AwsEc2SecurityGroupIpv6Range
    AwsEc2SecurityGroupIpv6Range (AwsEc2SecurityGroupIpv6Range'),
    newAwsEc2SecurityGroupIpv6Range,

    -- ** AwsEc2SecurityGroupPrefixListId
    AwsEc2SecurityGroupPrefixListId (AwsEc2SecurityGroupPrefixListId'),
    newAwsEc2SecurityGroupPrefixListId,

    -- ** AwsEc2SecurityGroupUserIdGroupPair
    AwsEc2SecurityGroupUserIdGroupPair (AwsEc2SecurityGroupUserIdGroupPair'),
    newAwsEc2SecurityGroupUserIdGroupPair,

    -- ** AwsEc2SubnetDetails
    AwsEc2SubnetDetails (AwsEc2SubnetDetails'),
    newAwsEc2SubnetDetails,

    -- ** AwsEc2TransitGatewayDetails
    AwsEc2TransitGatewayDetails (AwsEc2TransitGatewayDetails'),
    newAwsEc2TransitGatewayDetails,

    -- ** AwsEc2VolumeAttachment
    AwsEc2VolumeAttachment (AwsEc2VolumeAttachment'),
    newAwsEc2VolumeAttachment,

    -- ** AwsEc2VolumeDetails
    AwsEc2VolumeDetails (AwsEc2VolumeDetails'),
    newAwsEc2VolumeDetails,

    -- ** AwsEc2VpcDetails
    AwsEc2VpcDetails (AwsEc2VpcDetails'),
    newAwsEc2VpcDetails,

    -- ** AwsEc2VpcEndpointServiceDetails
    AwsEc2VpcEndpointServiceDetails (AwsEc2VpcEndpointServiceDetails'),
    newAwsEc2VpcEndpointServiceDetails,

    -- ** AwsEc2VpcEndpointServiceServiceTypeDetails
    AwsEc2VpcEndpointServiceServiceTypeDetails (AwsEc2VpcEndpointServiceServiceTypeDetails'),
    newAwsEc2VpcEndpointServiceServiceTypeDetails,

    -- ** AwsEc2VpcPeeringConnectionDetails
    AwsEc2VpcPeeringConnectionDetails (AwsEc2VpcPeeringConnectionDetails'),
    newAwsEc2VpcPeeringConnectionDetails,

    -- ** AwsEc2VpcPeeringConnectionStatusDetails
    AwsEc2VpcPeeringConnectionStatusDetails (AwsEc2VpcPeeringConnectionStatusDetails'),
    newAwsEc2VpcPeeringConnectionStatusDetails,

    -- ** AwsEc2VpcPeeringConnectionVpcInfoDetails
    AwsEc2VpcPeeringConnectionVpcInfoDetails (AwsEc2VpcPeeringConnectionVpcInfoDetails'),
    newAwsEc2VpcPeeringConnectionVpcInfoDetails,

    -- ** AwsEc2VpnConnectionDetails
    AwsEc2VpnConnectionDetails (AwsEc2VpnConnectionDetails'),
    newAwsEc2VpnConnectionDetails,

    -- ** AwsEc2VpnConnectionOptionsDetails
    AwsEc2VpnConnectionOptionsDetails (AwsEc2VpnConnectionOptionsDetails'),
    newAwsEc2VpnConnectionOptionsDetails,

    -- ** AwsEc2VpnConnectionOptionsTunnelOptionsDetails
    AwsEc2VpnConnectionOptionsTunnelOptionsDetails (AwsEc2VpnConnectionOptionsTunnelOptionsDetails'),
    newAwsEc2VpnConnectionOptionsTunnelOptionsDetails,

    -- ** AwsEc2VpnConnectionRoutesDetails
    AwsEc2VpnConnectionRoutesDetails (AwsEc2VpnConnectionRoutesDetails'),
    newAwsEc2VpnConnectionRoutesDetails,

    -- ** AwsEc2VpnConnectionVgwTelemetryDetails
    AwsEc2VpnConnectionVgwTelemetryDetails (AwsEc2VpnConnectionVgwTelemetryDetails'),
    newAwsEc2VpnConnectionVgwTelemetryDetails,

    -- ** AwsEcrContainerImageDetails
    AwsEcrContainerImageDetails (AwsEcrContainerImageDetails'),
    newAwsEcrContainerImageDetails,

    -- ** AwsEcrRepositoryDetails
    AwsEcrRepositoryDetails (AwsEcrRepositoryDetails'),
    newAwsEcrRepositoryDetails,

    -- ** AwsEcrRepositoryImageScanningConfigurationDetails
    AwsEcrRepositoryImageScanningConfigurationDetails (AwsEcrRepositoryImageScanningConfigurationDetails'),
    newAwsEcrRepositoryImageScanningConfigurationDetails,

    -- ** AwsEcrRepositoryLifecyclePolicyDetails
    AwsEcrRepositoryLifecyclePolicyDetails (AwsEcrRepositoryLifecyclePolicyDetails'),
    newAwsEcrRepositoryLifecyclePolicyDetails,

    -- ** AwsEcsClusterClusterSettingsDetails
    AwsEcsClusterClusterSettingsDetails (AwsEcsClusterClusterSettingsDetails'),
    newAwsEcsClusterClusterSettingsDetails,

    -- ** AwsEcsClusterConfigurationDetails
    AwsEcsClusterConfigurationDetails (AwsEcsClusterConfigurationDetails'),
    newAwsEcsClusterConfigurationDetails,

    -- ** AwsEcsClusterConfigurationExecuteCommandConfigurationDetails
    AwsEcsClusterConfigurationExecuteCommandConfigurationDetails (AwsEcsClusterConfigurationExecuteCommandConfigurationDetails'),
    newAwsEcsClusterConfigurationExecuteCommandConfigurationDetails,

    -- ** AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails
    AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails (AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails'),
    newAwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails,

    -- ** AwsEcsClusterDefaultCapacityProviderStrategyDetails
    AwsEcsClusterDefaultCapacityProviderStrategyDetails (AwsEcsClusterDefaultCapacityProviderStrategyDetails'),
    newAwsEcsClusterDefaultCapacityProviderStrategyDetails,

    -- ** AwsEcsClusterDetails
    AwsEcsClusterDetails (AwsEcsClusterDetails'),
    newAwsEcsClusterDetails,

    -- ** AwsEcsContainerDetails
    AwsEcsContainerDetails (AwsEcsContainerDetails'),
    newAwsEcsContainerDetails,

    -- ** AwsEcsServiceCapacityProviderStrategyDetails
    AwsEcsServiceCapacityProviderStrategyDetails (AwsEcsServiceCapacityProviderStrategyDetails'),
    newAwsEcsServiceCapacityProviderStrategyDetails,

    -- ** AwsEcsServiceDeploymentConfigurationDeploymentCircuitBreakerDetails
    AwsEcsServiceDeploymentConfigurationDeploymentCircuitBreakerDetails (AwsEcsServiceDeploymentConfigurationDeploymentCircuitBreakerDetails'),
    newAwsEcsServiceDeploymentConfigurationDeploymentCircuitBreakerDetails,

    -- ** AwsEcsServiceDeploymentConfigurationDetails
    AwsEcsServiceDeploymentConfigurationDetails (AwsEcsServiceDeploymentConfigurationDetails'),
    newAwsEcsServiceDeploymentConfigurationDetails,

    -- ** AwsEcsServiceDeploymentControllerDetails
    AwsEcsServiceDeploymentControllerDetails (AwsEcsServiceDeploymentControllerDetails'),
    newAwsEcsServiceDeploymentControllerDetails,

    -- ** AwsEcsServiceDetails
    AwsEcsServiceDetails (AwsEcsServiceDetails'),
    newAwsEcsServiceDetails,

    -- ** AwsEcsServiceLoadBalancersDetails
    AwsEcsServiceLoadBalancersDetails (AwsEcsServiceLoadBalancersDetails'),
    newAwsEcsServiceLoadBalancersDetails,

    -- ** AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails
    AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails (AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails'),
    newAwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails,

    -- ** AwsEcsServiceNetworkConfigurationDetails
    AwsEcsServiceNetworkConfigurationDetails (AwsEcsServiceNetworkConfigurationDetails'),
    newAwsEcsServiceNetworkConfigurationDetails,

    -- ** AwsEcsServicePlacementConstraintsDetails
    AwsEcsServicePlacementConstraintsDetails (AwsEcsServicePlacementConstraintsDetails'),
    newAwsEcsServicePlacementConstraintsDetails,

    -- ** AwsEcsServicePlacementStrategiesDetails
    AwsEcsServicePlacementStrategiesDetails (AwsEcsServicePlacementStrategiesDetails'),
    newAwsEcsServicePlacementStrategiesDetails,

    -- ** AwsEcsServiceServiceRegistriesDetails
    AwsEcsServiceServiceRegistriesDetails (AwsEcsServiceServiceRegistriesDetails'),
    newAwsEcsServiceServiceRegistriesDetails,

    -- ** AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails
    AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails (AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails'),
    newAwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails,

    -- ** AwsEcsTaskDefinitionContainerDefinitionsDetails
    AwsEcsTaskDefinitionContainerDefinitionsDetails (AwsEcsTaskDefinitionContainerDefinitionsDetails'),
    newAwsEcsTaskDefinitionContainerDefinitionsDetails,

    -- ** AwsEcsTaskDefinitionContainerDefinitionsEnvironmentDetails
    AwsEcsTaskDefinitionContainerDefinitionsEnvironmentDetails (AwsEcsTaskDefinitionContainerDefinitionsEnvironmentDetails'),
    newAwsEcsTaskDefinitionContainerDefinitionsEnvironmentDetails,

    -- ** AwsEcsTaskDefinitionContainerDefinitionsEnvironmentFilesDetails
    AwsEcsTaskDefinitionContainerDefinitionsEnvironmentFilesDetails (AwsEcsTaskDefinitionContainerDefinitionsEnvironmentFilesDetails'),
    newAwsEcsTaskDefinitionContainerDefinitionsEnvironmentFilesDetails,

    -- ** AwsEcsTaskDefinitionContainerDefinitionsExtraHostsDetails
    AwsEcsTaskDefinitionContainerDefinitionsExtraHostsDetails (AwsEcsTaskDefinitionContainerDefinitionsExtraHostsDetails'),
    newAwsEcsTaskDefinitionContainerDefinitionsExtraHostsDetails,

    -- ** AwsEcsTaskDefinitionContainerDefinitionsFirelensConfigurationDetails
    AwsEcsTaskDefinitionContainerDefinitionsFirelensConfigurationDetails (AwsEcsTaskDefinitionContainerDefinitionsFirelensConfigurationDetails'),
    newAwsEcsTaskDefinitionContainerDefinitionsFirelensConfigurationDetails,

    -- ** AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails
    AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails (AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails'),
    newAwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails,

    -- ** AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails
    AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails (AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails'),
    newAwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails,

    -- ** AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails
    AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails (AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails'),
    newAwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails,

    -- ** AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails
    AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails (AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails'),
    newAwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails,

    -- ** AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails
    AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails (AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails'),
    newAwsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails,

    -- ** AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails
    AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails (AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails'),
    newAwsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails,

    -- ** AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationSecretOptionsDetails
    AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationSecretOptionsDetails (AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationSecretOptionsDetails'),
    newAwsEcsTaskDefinitionContainerDefinitionsLogConfigurationSecretOptionsDetails,

    -- ** AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails
    AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails (AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails'),
    newAwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails,

    -- ** AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails
    AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails (AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails'),
    newAwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails,

    -- ** AwsEcsTaskDefinitionContainerDefinitionsRepositoryCredentialsDetails
    AwsEcsTaskDefinitionContainerDefinitionsRepositoryCredentialsDetails (AwsEcsTaskDefinitionContainerDefinitionsRepositoryCredentialsDetails'),
    newAwsEcsTaskDefinitionContainerDefinitionsRepositoryCredentialsDetails,

    -- ** AwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails
    AwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails (AwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails'),
    newAwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails,

    -- ** AwsEcsTaskDefinitionContainerDefinitionsSecretsDetails
    AwsEcsTaskDefinitionContainerDefinitionsSecretsDetails (AwsEcsTaskDefinitionContainerDefinitionsSecretsDetails'),
    newAwsEcsTaskDefinitionContainerDefinitionsSecretsDetails,

    -- ** AwsEcsTaskDefinitionContainerDefinitionsSystemControlsDetails
    AwsEcsTaskDefinitionContainerDefinitionsSystemControlsDetails (AwsEcsTaskDefinitionContainerDefinitionsSystemControlsDetails'),
    newAwsEcsTaskDefinitionContainerDefinitionsSystemControlsDetails,

    -- ** AwsEcsTaskDefinitionContainerDefinitionsUlimitsDetails
    AwsEcsTaskDefinitionContainerDefinitionsUlimitsDetails (AwsEcsTaskDefinitionContainerDefinitionsUlimitsDetails'),
    newAwsEcsTaskDefinitionContainerDefinitionsUlimitsDetails,

    -- ** AwsEcsTaskDefinitionContainerDefinitionsVolumesFromDetails
    AwsEcsTaskDefinitionContainerDefinitionsVolumesFromDetails (AwsEcsTaskDefinitionContainerDefinitionsVolumesFromDetails'),
    newAwsEcsTaskDefinitionContainerDefinitionsVolumesFromDetails,

    -- ** AwsEcsTaskDefinitionDetails
    AwsEcsTaskDefinitionDetails (AwsEcsTaskDefinitionDetails'),
    newAwsEcsTaskDefinitionDetails,

    -- ** AwsEcsTaskDefinitionInferenceAcceleratorsDetails
    AwsEcsTaskDefinitionInferenceAcceleratorsDetails (AwsEcsTaskDefinitionInferenceAcceleratorsDetails'),
    newAwsEcsTaskDefinitionInferenceAcceleratorsDetails,

    -- ** AwsEcsTaskDefinitionPlacementConstraintsDetails
    AwsEcsTaskDefinitionPlacementConstraintsDetails (AwsEcsTaskDefinitionPlacementConstraintsDetails'),
    newAwsEcsTaskDefinitionPlacementConstraintsDetails,

    -- ** AwsEcsTaskDefinitionProxyConfigurationDetails
    AwsEcsTaskDefinitionProxyConfigurationDetails (AwsEcsTaskDefinitionProxyConfigurationDetails'),
    newAwsEcsTaskDefinitionProxyConfigurationDetails,

    -- ** AwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails
    AwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails (AwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails'),
    newAwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails,

    -- ** AwsEcsTaskDefinitionVolumesDetails
    AwsEcsTaskDefinitionVolumesDetails (AwsEcsTaskDefinitionVolumesDetails'),
    newAwsEcsTaskDefinitionVolumesDetails,

    -- ** AwsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails
    AwsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails (AwsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails'),
    newAwsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails,

    -- ** AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationAuthorizationConfigDetails
    AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationAuthorizationConfigDetails (AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationAuthorizationConfigDetails'),
    newAwsEcsTaskDefinitionVolumesEfsVolumeConfigurationAuthorizationConfigDetails,

    -- ** AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails
    AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails (AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails'),
    newAwsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails,

    -- ** AwsEcsTaskDefinitionVolumesHostDetails
    AwsEcsTaskDefinitionVolumesHostDetails (AwsEcsTaskDefinitionVolumesHostDetails'),
    newAwsEcsTaskDefinitionVolumesHostDetails,

    -- ** AwsEcsTaskDetails
    AwsEcsTaskDetails (AwsEcsTaskDetails'),
    newAwsEcsTaskDetails,

    -- ** AwsEcsTaskVolumeDetails
    AwsEcsTaskVolumeDetails (AwsEcsTaskVolumeDetails'),
    newAwsEcsTaskVolumeDetails,

    -- ** AwsEcsTaskVolumeHostDetails
    AwsEcsTaskVolumeHostDetails (AwsEcsTaskVolumeHostDetails'),
    newAwsEcsTaskVolumeHostDetails,

    -- ** AwsEfsAccessPointDetails
    AwsEfsAccessPointDetails (AwsEfsAccessPointDetails'),
    newAwsEfsAccessPointDetails,

    -- ** AwsEfsAccessPointPosixUserDetails
    AwsEfsAccessPointPosixUserDetails (AwsEfsAccessPointPosixUserDetails'),
    newAwsEfsAccessPointPosixUserDetails,

    -- ** AwsEfsAccessPointRootDirectoryCreationInfoDetails
    AwsEfsAccessPointRootDirectoryCreationInfoDetails (AwsEfsAccessPointRootDirectoryCreationInfoDetails'),
    newAwsEfsAccessPointRootDirectoryCreationInfoDetails,

    -- ** AwsEfsAccessPointRootDirectoryDetails
    AwsEfsAccessPointRootDirectoryDetails (AwsEfsAccessPointRootDirectoryDetails'),
    newAwsEfsAccessPointRootDirectoryDetails,

    -- ** AwsEksClusterDetails
    AwsEksClusterDetails (AwsEksClusterDetails'),
    newAwsEksClusterDetails,

    -- ** AwsEksClusterLoggingClusterLoggingDetails
    AwsEksClusterLoggingClusterLoggingDetails (AwsEksClusterLoggingClusterLoggingDetails'),
    newAwsEksClusterLoggingClusterLoggingDetails,

    -- ** AwsEksClusterLoggingDetails
    AwsEksClusterLoggingDetails (AwsEksClusterLoggingDetails'),
    newAwsEksClusterLoggingDetails,

    -- ** AwsEksClusterResourcesVpcConfigDetails
    AwsEksClusterResourcesVpcConfigDetails (AwsEksClusterResourcesVpcConfigDetails'),
    newAwsEksClusterResourcesVpcConfigDetails,

    -- ** AwsElasticBeanstalkEnvironmentDetails
    AwsElasticBeanstalkEnvironmentDetails (AwsElasticBeanstalkEnvironmentDetails'),
    newAwsElasticBeanstalkEnvironmentDetails,

    -- ** AwsElasticBeanstalkEnvironmentEnvironmentLink
    AwsElasticBeanstalkEnvironmentEnvironmentLink (AwsElasticBeanstalkEnvironmentEnvironmentLink'),
    newAwsElasticBeanstalkEnvironmentEnvironmentLink,

    -- ** AwsElasticBeanstalkEnvironmentOptionSetting
    AwsElasticBeanstalkEnvironmentOptionSetting (AwsElasticBeanstalkEnvironmentOptionSetting'),
    newAwsElasticBeanstalkEnvironmentOptionSetting,

    -- ** AwsElasticBeanstalkEnvironmentTier
    AwsElasticBeanstalkEnvironmentTier (AwsElasticBeanstalkEnvironmentTier'),
    newAwsElasticBeanstalkEnvironmentTier,

    -- ** AwsElasticsearchDomainDetails
    AwsElasticsearchDomainDetails (AwsElasticsearchDomainDetails'),
    newAwsElasticsearchDomainDetails,

    -- ** AwsElasticsearchDomainDomainEndpointOptions
    AwsElasticsearchDomainDomainEndpointOptions (AwsElasticsearchDomainDomainEndpointOptions'),
    newAwsElasticsearchDomainDomainEndpointOptions,

    -- ** AwsElasticsearchDomainElasticsearchClusterConfigDetails
    AwsElasticsearchDomainElasticsearchClusterConfigDetails (AwsElasticsearchDomainElasticsearchClusterConfigDetails'),
    newAwsElasticsearchDomainElasticsearchClusterConfigDetails,

    -- ** AwsElasticsearchDomainElasticsearchClusterConfigZoneAwarenessConfigDetails
    AwsElasticsearchDomainElasticsearchClusterConfigZoneAwarenessConfigDetails (AwsElasticsearchDomainElasticsearchClusterConfigZoneAwarenessConfigDetails'),
    newAwsElasticsearchDomainElasticsearchClusterConfigZoneAwarenessConfigDetails,

    -- ** AwsElasticsearchDomainEncryptionAtRestOptions
    AwsElasticsearchDomainEncryptionAtRestOptions (AwsElasticsearchDomainEncryptionAtRestOptions'),
    newAwsElasticsearchDomainEncryptionAtRestOptions,

    -- ** AwsElasticsearchDomainLogPublishingOptions
    AwsElasticsearchDomainLogPublishingOptions (AwsElasticsearchDomainLogPublishingOptions'),
    newAwsElasticsearchDomainLogPublishingOptions,

    -- ** AwsElasticsearchDomainLogPublishingOptionsLogConfig
    AwsElasticsearchDomainLogPublishingOptionsLogConfig (AwsElasticsearchDomainLogPublishingOptionsLogConfig'),
    newAwsElasticsearchDomainLogPublishingOptionsLogConfig,

    -- ** AwsElasticsearchDomainNodeToNodeEncryptionOptions
    AwsElasticsearchDomainNodeToNodeEncryptionOptions (AwsElasticsearchDomainNodeToNodeEncryptionOptions'),
    newAwsElasticsearchDomainNodeToNodeEncryptionOptions,

    -- ** AwsElasticsearchDomainServiceSoftwareOptions
    AwsElasticsearchDomainServiceSoftwareOptions (AwsElasticsearchDomainServiceSoftwareOptions'),
    newAwsElasticsearchDomainServiceSoftwareOptions,

    -- ** AwsElasticsearchDomainVPCOptions
    AwsElasticsearchDomainVPCOptions (AwsElasticsearchDomainVPCOptions'),
    newAwsElasticsearchDomainVPCOptions,

    -- ** AwsElbAppCookieStickinessPolicy
    AwsElbAppCookieStickinessPolicy (AwsElbAppCookieStickinessPolicy'),
    newAwsElbAppCookieStickinessPolicy,

    -- ** AwsElbLbCookieStickinessPolicy
    AwsElbLbCookieStickinessPolicy (AwsElbLbCookieStickinessPolicy'),
    newAwsElbLbCookieStickinessPolicy,

    -- ** AwsElbLoadBalancerAccessLog
    AwsElbLoadBalancerAccessLog (AwsElbLoadBalancerAccessLog'),
    newAwsElbLoadBalancerAccessLog,

    -- ** AwsElbLoadBalancerAdditionalAttribute
    AwsElbLoadBalancerAdditionalAttribute (AwsElbLoadBalancerAdditionalAttribute'),
    newAwsElbLoadBalancerAdditionalAttribute,

    -- ** AwsElbLoadBalancerAttributes
    AwsElbLoadBalancerAttributes (AwsElbLoadBalancerAttributes'),
    newAwsElbLoadBalancerAttributes,

    -- ** AwsElbLoadBalancerBackendServerDescription
    AwsElbLoadBalancerBackendServerDescription (AwsElbLoadBalancerBackendServerDescription'),
    newAwsElbLoadBalancerBackendServerDescription,

    -- ** AwsElbLoadBalancerConnectionDraining
    AwsElbLoadBalancerConnectionDraining (AwsElbLoadBalancerConnectionDraining'),
    newAwsElbLoadBalancerConnectionDraining,

    -- ** AwsElbLoadBalancerConnectionSettings
    AwsElbLoadBalancerConnectionSettings (AwsElbLoadBalancerConnectionSettings'),
    newAwsElbLoadBalancerConnectionSettings,

    -- ** AwsElbLoadBalancerCrossZoneLoadBalancing
    AwsElbLoadBalancerCrossZoneLoadBalancing (AwsElbLoadBalancerCrossZoneLoadBalancing'),
    newAwsElbLoadBalancerCrossZoneLoadBalancing,

    -- ** AwsElbLoadBalancerDetails
    AwsElbLoadBalancerDetails (AwsElbLoadBalancerDetails'),
    newAwsElbLoadBalancerDetails,

    -- ** AwsElbLoadBalancerHealthCheck
    AwsElbLoadBalancerHealthCheck (AwsElbLoadBalancerHealthCheck'),
    newAwsElbLoadBalancerHealthCheck,

    -- ** AwsElbLoadBalancerInstance
    AwsElbLoadBalancerInstance (AwsElbLoadBalancerInstance'),
    newAwsElbLoadBalancerInstance,

    -- ** AwsElbLoadBalancerListener
    AwsElbLoadBalancerListener (AwsElbLoadBalancerListener'),
    newAwsElbLoadBalancerListener,

    -- ** AwsElbLoadBalancerListenerDescription
    AwsElbLoadBalancerListenerDescription (AwsElbLoadBalancerListenerDescription'),
    newAwsElbLoadBalancerListenerDescription,

    -- ** AwsElbLoadBalancerPolicies
    AwsElbLoadBalancerPolicies (AwsElbLoadBalancerPolicies'),
    newAwsElbLoadBalancerPolicies,

    -- ** AwsElbLoadBalancerSourceSecurityGroup
    AwsElbLoadBalancerSourceSecurityGroup (AwsElbLoadBalancerSourceSecurityGroup'),
    newAwsElbLoadBalancerSourceSecurityGroup,

    -- ** AwsElbv2LoadBalancerAttribute
    AwsElbv2LoadBalancerAttribute (AwsElbv2LoadBalancerAttribute'),
    newAwsElbv2LoadBalancerAttribute,

    -- ** AwsElbv2LoadBalancerDetails
    AwsElbv2LoadBalancerDetails (AwsElbv2LoadBalancerDetails'),
    newAwsElbv2LoadBalancerDetails,

    -- ** AwsIamAccessKeyDetails
    AwsIamAccessKeyDetails (AwsIamAccessKeyDetails'),
    newAwsIamAccessKeyDetails,

    -- ** AwsIamAccessKeySessionContext
    AwsIamAccessKeySessionContext (AwsIamAccessKeySessionContext'),
    newAwsIamAccessKeySessionContext,

    -- ** AwsIamAccessKeySessionContextAttributes
    AwsIamAccessKeySessionContextAttributes (AwsIamAccessKeySessionContextAttributes'),
    newAwsIamAccessKeySessionContextAttributes,

    -- ** AwsIamAccessKeySessionContextSessionIssuer
    AwsIamAccessKeySessionContextSessionIssuer (AwsIamAccessKeySessionContextSessionIssuer'),
    newAwsIamAccessKeySessionContextSessionIssuer,

    -- ** AwsIamAttachedManagedPolicy
    AwsIamAttachedManagedPolicy (AwsIamAttachedManagedPolicy'),
    newAwsIamAttachedManagedPolicy,

    -- ** AwsIamGroupDetails
    AwsIamGroupDetails (AwsIamGroupDetails'),
    newAwsIamGroupDetails,

    -- ** AwsIamGroupPolicy
    AwsIamGroupPolicy (AwsIamGroupPolicy'),
    newAwsIamGroupPolicy,

    -- ** AwsIamInstanceProfile
    AwsIamInstanceProfile (AwsIamInstanceProfile'),
    newAwsIamInstanceProfile,

    -- ** AwsIamInstanceProfileRole
    AwsIamInstanceProfileRole (AwsIamInstanceProfileRole'),
    newAwsIamInstanceProfileRole,

    -- ** AwsIamPermissionsBoundary
    AwsIamPermissionsBoundary (AwsIamPermissionsBoundary'),
    newAwsIamPermissionsBoundary,

    -- ** AwsIamPolicyDetails
    AwsIamPolicyDetails (AwsIamPolicyDetails'),
    newAwsIamPolicyDetails,

    -- ** AwsIamPolicyVersion
    AwsIamPolicyVersion (AwsIamPolicyVersion'),
    newAwsIamPolicyVersion,

    -- ** AwsIamRoleDetails
    AwsIamRoleDetails (AwsIamRoleDetails'),
    newAwsIamRoleDetails,

    -- ** AwsIamRolePolicy
    AwsIamRolePolicy (AwsIamRolePolicy'),
    newAwsIamRolePolicy,

    -- ** AwsIamUserDetails
    AwsIamUserDetails (AwsIamUserDetails'),
    newAwsIamUserDetails,

    -- ** AwsIamUserPolicy
    AwsIamUserPolicy (AwsIamUserPolicy'),
    newAwsIamUserPolicy,

    -- ** AwsKinesisStreamDetails
    AwsKinesisStreamDetails (AwsKinesisStreamDetails'),
    newAwsKinesisStreamDetails,

    -- ** AwsKinesisStreamStreamEncryptionDetails
    AwsKinesisStreamStreamEncryptionDetails (AwsKinesisStreamStreamEncryptionDetails'),
    newAwsKinesisStreamStreamEncryptionDetails,

    -- ** AwsKmsKeyDetails
    AwsKmsKeyDetails (AwsKmsKeyDetails'),
    newAwsKmsKeyDetails,

    -- ** AwsLambdaFunctionCode
    AwsLambdaFunctionCode (AwsLambdaFunctionCode'),
    newAwsLambdaFunctionCode,

    -- ** AwsLambdaFunctionDeadLetterConfig
    AwsLambdaFunctionDeadLetterConfig (AwsLambdaFunctionDeadLetterConfig'),
    newAwsLambdaFunctionDeadLetterConfig,

    -- ** AwsLambdaFunctionDetails
    AwsLambdaFunctionDetails (AwsLambdaFunctionDetails'),
    newAwsLambdaFunctionDetails,

    -- ** AwsLambdaFunctionEnvironment
    AwsLambdaFunctionEnvironment (AwsLambdaFunctionEnvironment'),
    newAwsLambdaFunctionEnvironment,

    -- ** AwsLambdaFunctionEnvironmentError
    AwsLambdaFunctionEnvironmentError (AwsLambdaFunctionEnvironmentError'),
    newAwsLambdaFunctionEnvironmentError,

    -- ** AwsLambdaFunctionLayer
    AwsLambdaFunctionLayer (AwsLambdaFunctionLayer'),
    newAwsLambdaFunctionLayer,

    -- ** AwsLambdaFunctionTracingConfig
    AwsLambdaFunctionTracingConfig (AwsLambdaFunctionTracingConfig'),
    newAwsLambdaFunctionTracingConfig,

    -- ** AwsLambdaFunctionVpcConfig
    AwsLambdaFunctionVpcConfig (AwsLambdaFunctionVpcConfig'),
    newAwsLambdaFunctionVpcConfig,

    -- ** AwsLambdaLayerVersionDetails
    AwsLambdaLayerVersionDetails (AwsLambdaLayerVersionDetails'),
    newAwsLambdaLayerVersionDetails,

    -- ** AwsMountPoint
    AwsMountPoint (AwsMountPoint'),
    newAwsMountPoint,

    -- ** AwsNetworkFirewallFirewallDetails
    AwsNetworkFirewallFirewallDetails (AwsNetworkFirewallFirewallDetails'),
    newAwsNetworkFirewallFirewallDetails,

    -- ** AwsNetworkFirewallFirewallPolicyDetails
    AwsNetworkFirewallFirewallPolicyDetails (AwsNetworkFirewallFirewallPolicyDetails'),
    newAwsNetworkFirewallFirewallPolicyDetails,

    -- ** AwsNetworkFirewallFirewallSubnetMappingsDetails
    AwsNetworkFirewallFirewallSubnetMappingsDetails (AwsNetworkFirewallFirewallSubnetMappingsDetails'),
    newAwsNetworkFirewallFirewallSubnetMappingsDetails,

    -- ** AwsNetworkFirewallRuleGroupDetails
    AwsNetworkFirewallRuleGroupDetails (AwsNetworkFirewallRuleGroupDetails'),
    newAwsNetworkFirewallRuleGroupDetails,

    -- ** AwsOpenSearchServiceDomainAdvancedSecurityOptionsDetails
    AwsOpenSearchServiceDomainAdvancedSecurityOptionsDetails (AwsOpenSearchServiceDomainAdvancedSecurityOptionsDetails'),
    newAwsOpenSearchServiceDomainAdvancedSecurityOptionsDetails,

    -- ** AwsOpenSearchServiceDomainClusterConfigDetails
    AwsOpenSearchServiceDomainClusterConfigDetails (AwsOpenSearchServiceDomainClusterConfigDetails'),
    newAwsOpenSearchServiceDomainClusterConfigDetails,

    -- ** AwsOpenSearchServiceDomainClusterConfigZoneAwarenessConfigDetails
    AwsOpenSearchServiceDomainClusterConfigZoneAwarenessConfigDetails (AwsOpenSearchServiceDomainClusterConfigZoneAwarenessConfigDetails'),
    newAwsOpenSearchServiceDomainClusterConfigZoneAwarenessConfigDetails,

    -- ** AwsOpenSearchServiceDomainDetails
    AwsOpenSearchServiceDomainDetails (AwsOpenSearchServiceDomainDetails'),
    newAwsOpenSearchServiceDomainDetails,

    -- ** AwsOpenSearchServiceDomainDomainEndpointOptionsDetails
    AwsOpenSearchServiceDomainDomainEndpointOptionsDetails (AwsOpenSearchServiceDomainDomainEndpointOptionsDetails'),
    newAwsOpenSearchServiceDomainDomainEndpointOptionsDetails,

    -- ** AwsOpenSearchServiceDomainEncryptionAtRestOptionsDetails
    AwsOpenSearchServiceDomainEncryptionAtRestOptionsDetails (AwsOpenSearchServiceDomainEncryptionAtRestOptionsDetails'),
    newAwsOpenSearchServiceDomainEncryptionAtRestOptionsDetails,

    -- ** AwsOpenSearchServiceDomainLogPublishingOption
    AwsOpenSearchServiceDomainLogPublishingOption (AwsOpenSearchServiceDomainLogPublishingOption'),
    newAwsOpenSearchServiceDomainLogPublishingOption,

    -- ** AwsOpenSearchServiceDomainLogPublishingOptionsDetails
    AwsOpenSearchServiceDomainLogPublishingOptionsDetails (AwsOpenSearchServiceDomainLogPublishingOptionsDetails'),
    newAwsOpenSearchServiceDomainLogPublishingOptionsDetails,

    -- ** AwsOpenSearchServiceDomainMasterUserOptionsDetails
    AwsOpenSearchServiceDomainMasterUserOptionsDetails (AwsOpenSearchServiceDomainMasterUserOptionsDetails'),
    newAwsOpenSearchServiceDomainMasterUserOptionsDetails,

    -- ** AwsOpenSearchServiceDomainNodeToNodeEncryptionOptionsDetails
    AwsOpenSearchServiceDomainNodeToNodeEncryptionOptionsDetails (AwsOpenSearchServiceDomainNodeToNodeEncryptionOptionsDetails'),
    newAwsOpenSearchServiceDomainNodeToNodeEncryptionOptionsDetails,

    -- ** AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails
    AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails (AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails'),
    newAwsOpenSearchServiceDomainServiceSoftwareOptionsDetails,

    -- ** AwsOpenSearchServiceDomainVpcOptionsDetails
    AwsOpenSearchServiceDomainVpcOptionsDetails (AwsOpenSearchServiceDomainVpcOptionsDetails'),
    newAwsOpenSearchServiceDomainVpcOptionsDetails,

    -- ** AwsRdsDbClusterAssociatedRole
    AwsRdsDbClusterAssociatedRole (AwsRdsDbClusterAssociatedRole'),
    newAwsRdsDbClusterAssociatedRole,

    -- ** AwsRdsDbClusterDetails
    AwsRdsDbClusterDetails (AwsRdsDbClusterDetails'),
    newAwsRdsDbClusterDetails,

    -- ** AwsRdsDbClusterMember
    AwsRdsDbClusterMember (AwsRdsDbClusterMember'),
    newAwsRdsDbClusterMember,

    -- ** AwsRdsDbClusterOptionGroupMembership
    AwsRdsDbClusterOptionGroupMembership (AwsRdsDbClusterOptionGroupMembership'),
    newAwsRdsDbClusterOptionGroupMembership,

    -- ** AwsRdsDbClusterSnapshotDetails
    AwsRdsDbClusterSnapshotDetails (AwsRdsDbClusterSnapshotDetails'),
    newAwsRdsDbClusterSnapshotDetails,

    -- ** AwsRdsDbDomainMembership
    AwsRdsDbDomainMembership (AwsRdsDbDomainMembership'),
    newAwsRdsDbDomainMembership,

    -- ** AwsRdsDbInstanceAssociatedRole
    AwsRdsDbInstanceAssociatedRole (AwsRdsDbInstanceAssociatedRole'),
    newAwsRdsDbInstanceAssociatedRole,

    -- ** AwsRdsDbInstanceDetails
    AwsRdsDbInstanceDetails (AwsRdsDbInstanceDetails'),
    newAwsRdsDbInstanceDetails,

    -- ** AwsRdsDbInstanceEndpoint
    AwsRdsDbInstanceEndpoint (AwsRdsDbInstanceEndpoint'),
    newAwsRdsDbInstanceEndpoint,

    -- ** AwsRdsDbInstanceVpcSecurityGroup
    AwsRdsDbInstanceVpcSecurityGroup (AwsRdsDbInstanceVpcSecurityGroup'),
    newAwsRdsDbInstanceVpcSecurityGroup,

    -- ** AwsRdsDbOptionGroupMembership
    AwsRdsDbOptionGroupMembership (AwsRdsDbOptionGroupMembership'),
    newAwsRdsDbOptionGroupMembership,

    -- ** AwsRdsDbParameterGroup
    AwsRdsDbParameterGroup (AwsRdsDbParameterGroup'),
    newAwsRdsDbParameterGroup,

    -- ** AwsRdsDbPendingModifiedValues
    AwsRdsDbPendingModifiedValues (AwsRdsDbPendingModifiedValues'),
    newAwsRdsDbPendingModifiedValues,

    -- ** AwsRdsDbProcessorFeature
    AwsRdsDbProcessorFeature (AwsRdsDbProcessorFeature'),
    newAwsRdsDbProcessorFeature,

    -- ** AwsRdsDbSecurityGroupDetails
    AwsRdsDbSecurityGroupDetails (AwsRdsDbSecurityGroupDetails'),
    newAwsRdsDbSecurityGroupDetails,

    -- ** AwsRdsDbSecurityGroupEc2SecurityGroup
    AwsRdsDbSecurityGroupEc2SecurityGroup (AwsRdsDbSecurityGroupEc2SecurityGroup'),
    newAwsRdsDbSecurityGroupEc2SecurityGroup,

    -- ** AwsRdsDbSecurityGroupIpRange
    AwsRdsDbSecurityGroupIpRange (AwsRdsDbSecurityGroupIpRange'),
    newAwsRdsDbSecurityGroupIpRange,

    -- ** AwsRdsDbSnapshotDetails
    AwsRdsDbSnapshotDetails (AwsRdsDbSnapshotDetails'),
    newAwsRdsDbSnapshotDetails,

    -- ** AwsRdsDbStatusInfo
    AwsRdsDbStatusInfo (AwsRdsDbStatusInfo'),
    newAwsRdsDbStatusInfo,

    -- ** AwsRdsDbSubnetGroup
    AwsRdsDbSubnetGroup (AwsRdsDbSubnetGroup'),
    newAwsRdsDbSubnetGroup,

    -- ** AwsRdsDbSubnetGroupSubnet
    AwsRdsDbSubnetGroupSubnet (AwsRdsDbSubnetGroupSubnet'),
    newAwsRdsDbSubnetGroupSubnet,

    -- ** AwsRdsDbSubnetGroupSubnetAvailabilityZone
    AwsRdsDbSubnetGroupSubnetAvailabilityZone (AwsRdsDbSubnetGroupSubnetAvailabilityZone'),
    newAwsRdsDbSubnetGroupSubnetAvailabilityZone,

    -- ** AwsRdsEventSubscriptionDetails
    AwsRdsEventSubscriptionDetails (AwsRdsEventSubscriptionDetails'),
    newAwsRdsEventSubscriptionDetails,

    -- ** AwsRdsPendingCloudWatchLogsExports
    AwsRdsPendingCloudWatchLogsExports (AwsRdsPendingCloudWatchLogsExports'),
    newAwsRdsPendingCloudWatchLogsExports,

    -- ** AwsRedshiftClusterClusterNode
    AwsRedshiftClusterClusterNode (AwsRedshiftClusterClusterNode'),
    newAwsRedshiftClusterClusterNode,

    -- ** AwsRedshiftClusterClusterParameterGroup
    AwsRedshiftClusterClusterParameterGroup (AwsRedshiftClusterClusterParameterGroup'),
    newAwsRedshiftClusterClusterParameterGroup,

    -- ** AwsRedshiftClusterClusterParameterStatus
    AwsRedshiftClusterClusterParameterStatus (AwsRedshiftClusterClusterParameterStatus'),
    newAwsRedshiftClusterClusterParameterStatus,

    -- ** AwsRedshiftClusterClusterSecurityGroup
    AwsRedshiftClusterClusterSecurityGroup (AwsRedshiftClusterClusterSecurityGroup'),
    newAwsRedshiftClusterClusterSecurityGroup,

    -- ** AwsRedshiftClusterClusterSnapshotCopyStatus
    AwsRedshiftClusterClusterSnapshotCopyStatus (AwsRedshiftClusterClusterSnapshotCopyStatus'),
    newAwsRedshiftClusterClusterSnapshotCopyStatus,

    -- ** AwsRedshiftClusterDeferredMaintenanceWindow
    AwsRedshiftClusterDeferredMaintenanceWindow (AwsRedshiftClusterDeferredMaintenanceWindow'),
    newAwsRedshiftClusterDeferredMaintenanceWindow,

    -- ** AwsRedshiftClusterDetails
    AwsRedshiftClusterDetails (AwsRedshiftClusterDetails'),
    newAwsRedshiftClusterDetails,

    -- ** AwsRedshiftClusterElasticIpStatus
    AwsRedshiftClusterElasticIpStatus (AwsRedshiftClusterElasticIpStatus'),
    newAwsRedshiftClusterElasticIpStatus,

    -- ** AwsRedshiftClusterEndpoint
    AwsRedshiftClusterEndpoint (AwsRedshiftClusterEndpoint'),
    newAwsRedshiftClusterEndpoint,

    -- ** AwsRedshiftClusterHsmStatus
    AwsRedshiftClusterHsmStatus (AwsRedshiftClusterHsmStatus'),
    newAwsRedshiftClusterHsmStatus,

    -- ** AwsRedshiftClusterIamRole
    AwsRedshiftClusterIamRole (AwsRedshiftClusterIamRole'),
    newAwsRedshiftClusterIamRole,

    -- ** AwsRedshiftClusterLoggingStatus
    AwsRedshiftClusterLoggingStatus (AwsRedshiftClusterLoggingStatus'),
    newAwsRedshiftClusterLoggingStatus,

    -- ** AwsRedshiftClusterPendingModifiedValues
    AwsRedshiftClusterPendingModifiedValues (AwsRedshiftClusterPendingModifiedValues'),
    newAwsRedshiftClusterPendingModifiedValues,

    -- ** AwsRedshiftClusterResizeInfo
    AwsRedshiftClusterResizeInfo (AwsRedshiftClusterResizeInfo'),
    newAwsRedshiftClusterResizeInfo,

    -- ** AwsRedshiftClusterRestoreStatus
    AwsRedshiftClusterRestoreStatus (AwsRedshiftClusterRestoreStatus'),
    newAwsRedshiftClusterRestoreStatus,

    -- ** AwsRedshiftClusterVpcSecurityGroup
    AwsRedshiftClusterVpcSecurityGroup (AwsRedshiftClusterVpcSecurityGroup'),
    newAwsRedshiftClusterVpcSecurityGroup,

    -- ** AwsS3AccountPublicAccessBlockDetails
    AwsS3AccountPublicAccessBlockDetails (AwsS3AccountPublicAccessBlockDetails'),
    newAwsS3AccountPublicAccessBlockDetails,

    -- ** AwsS3BucketBucketLifecycleConfigurationDetails
    AwsS3BucketBucketLifecycleConfigurationDetails (AwsS3BucketBucketLifecycleConfigurationDetails'),
    newAwsS3BucketBucketLifecycleConfigurationDetails,

    -- ** AwsS3BucketBucketLifecycleConfigurationRulesAbortIncompleteMultipartUploadDetails
    AwsS3BucketBucketLifecycleConfigurationRulesAbortIncompleteMultipartUploadDetails (AwsS3BucketBucketLifecycleConfigurationRulesAbortIncompleteMultipartUploadDetails'),
    newAwsS3BucketBucketLifecycleConfigurationRulesAbortIncompleteMultipartUploadDetails,

    -- ** AwsS3BucketBucketLifecycleConfigurationRulesDetails
    AwsS3BucketBucketLifecycleConfigurationRulesDetails (AwsS3BucketBucketLifecycleConfigurationRulesDetails'),
    newAwsS3BucketBucketLifecycleConfigurationRulesDetails,

    -- ** AwsS3BucketBucketLifecycleConfigurationRulesFilterDetails
    AwsS3BucketBucketLifecycleConfigurationRulesFilterDetails (AwsS3BucketBucketLifecycleConfigurationRulesFilterDetails'),
    newAwsS3BucketBucketLifecycleConfigurationRulesFilterDetails,

    -- ** AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails
    AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails (AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails'),
    newAwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails,

    -- ** AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails
    AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails (AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails'),
    newAwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails,

    -- ** AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsTagDetails
    AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsTagDetails (AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsTagDetails'),
    newAwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsTagDetails,

    -- ** AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateTagDetails
    AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateTagDetails (AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateTagDetails'),
    newAwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateTagDetails,

    -- ** AwsS3BucketBucketLifecycleConfigurationRulesNoncurrentVersionTransitionsDetails
    AwsS3BucketBucketLifecycleConfigurationRulesNoncurrentVersionTransitionsDetails (AwsS3BucketBucketLifecycleConfigurationRulesNoncurrentVersionTransitionsDetails'),
    newAwsS3BucketBucketLifecycleConfigurationRulesNoncurrentVersionTransitionsDetails,

    -- ** AwsS3BucketBucketLifecycleConfigurationRulesTransitionsDetails
    AwsS3BucketBucketLifecycleConfigurationRulesTransitionsDetails (AwsS3BucketBucketLifecycleConfigurationRulesTransitionsDetails'),
    newAwsS3BucketBucketLifecycleConfigurationRulesTransitionsDetails,

    -- ** AwsS3BucketBucketVersioningConfiguration
    AwsS3BucketBucketVersioningConfiguration (AwsS3BucketBucketVersioningConfiguration'),
    newAwsS3BucketBucketVersioningConfiguration,

    -- ** AwsS3BucketDetails
    AwsS3BucketDetails (AwsS3BucketDetails'),
    newAwsS3BucketDetails,

    -- ** AwsS3BucketLoggingConfiguration
    AwsS3BucketLoggingConfiguration (AwsS3BucketLoggingConfiguration'),
    newAwsS3BucketLoggingConfiguration,

    -- ** AwsS3BucketNotificationConfiguration
    AwsS3BucketNotificationConfiguration (AwsS3BucketNotificationConfiguration'),
    newAwsS3BucketNotificationConfiguration,

    -- ** AwsS3BucketNotificationConfigurationDetail
    AwsS3BucketNotificationConfigurationDetail (AwsS3BucketNotificationConfigurationDetail'),
    newAwsS3BucketNotificationConfigurationDetail,

    -- ** AwsS3BucketNotificationConfigurationFilter
    AwsS3BucketNotificationConfigurationFilter (AwsS3BucketNotificationConfigurationFilter'),
    newAwsS3BucketNotificationConfigurationFilter,

    -- ** AwsS3BucketNotificationConfigurationS3KeyFilter
    AwsS3BucketNotificationConfigurationS3KeyFilter (AwsS3BucketNotificationConfigurationS3KeyFilter'),
    newAwsS3BucketNotificationConfigurationS3KeyFilter,

    -- ** AwsS3BucketNotificationConfigurationS3KeyFilterRule
    AwsS3BucketNotificationConfigurationS3KeyFilterRule (AwsS3BucketNotificationConfigurationS3KeyFilterRule'),
    newAwsS3BucketNotificationConfigurationS3KeyFilterRule,

    -- ** AwsS3BucketServerSideEncryptionByDefault
    AwsS3BucketServerSideEncryptionByDefault (AwsS3BucketServerSideEncryptionByDefault'),
    newAwsS3BucketServerSideEncryptionByDefault,

    -- ** AwsS3BucketServerSideEncryptionConfiguration
    AwsS3BucketServerSideEncryptionConfiguration (AwsS3BucketServerSideEncryptionConfiguration'),
    newAwsS3BucketServerSideEncryptionConfiguration,

    -- ** AwsS3BucketServerSideEncryptionRule
    AwsS3BucketServerSideEncryptionRule (AwsS3BucketServerSideEncryptionRule'),
    newAwsS3BucketServerSideEncryptionRule,

    -- ** AwsS3BucketWebsiteConfiguration
    AwsS3BucketWebsiteConfiguration (AwsS3BucketWebsiteConfiguration'),
    newAwsS3BucketWebsiteConfiguration,

    -- ** AwsS3BucketWebsiteConfigurationRedirectTo
    AwsS3BucketWebsiteConfigurationRedirectTo (AwsS3BucketWebsiteConfigurationRedirectTo'),
    newAwsS3BucketWebsiteConfigurationRedirectTo,

    -- ** AwsS3BucketWebsiteConfigurationRoutingRule
    AwsS3BucketWebsiteConfigurationRoutingRule (AwsS3BucketWebsiteConfigurationRoutingRule'),
    newAwsS3BucketWebsiteConfigurationRoutingRule,

    -- ** AwsS3BucketWebsiteConfigurationRoutingRuleCondition
    AwsS3BucketWebsiteConfigurationRoutingRuleCondition (AwsS3BucketWebsiteConfigurationRoutingRuleCondition'),
    newAwsS3BucketWebsiteConfigurationRoutingRuleCondition,

    -- ** AwsS3BucketWebsiteConfigurationRoutingRuleRedirect
    AwsS3BucketWebsiteConfigurationRoutingRuleRedirect (AwsS3BucketWebsiteConfigurationRoutingRuleRedirect'),
    newAwsS3BucketWebsiteConfigurationRoutingRuleRedirect,

    -- ** AwsS3ObjectDetails
    AwsS3ObjectDetails (AwsS3ObjectDetails'),
    newAwsS3ObjectDetails,

    -- ** AwsSageMakerNotebookInstanceDetails
    AwsSageMakerNotebookInstanceDetails (AwsSageMakerNotebookInstanceDetails'),
    newAwsSageMakerNotebookInstanceDetails,

    -- ** AwsSageMakerNotebookInstanceMetadataServiceConfigurationDetails
    AwsSageMakerNotebookInstanceMetadataServiceConfigurationDetails (AwsSageMakerNotebookInstanceMetadataServiceConfigurationDetails'),
    newAwsSageMakerNotebookInstanceMetadataServiceConfigurationDetails,

    -- ** AwsSecretsManagerSecretDetails
    AwsSecretsManagerSecretDetails (AwsSecretsManagerSecretDetails'),
    newAwsSecretsManagerSecretDetails,

    -- ** AwsSecretsManagerSecretRotationRules
    AwsSecretsManagerSecretRotationRules (AwsSecretsManagerSecretRotationRules'),
    newAwsSecretsManagerSecretRotationRules,

    -- ** AwsSecurityFinding
    AwsSecurityFinding (AwsSecurityFinding'),
    newAwsSecurityFinding,

    -- ** AwsSecurityFindingFilters
    AwsSecurityFindingFilters (AwsSecurityFindingFilters'),
    newAwsSecurityFindingFilters,

    -- ** AwsSecurityFindingIdentifier
    AwsSecurityFindingIdentifier (AwsSecurityFindingIdentifier'),
    newAwsSecurityFindingIdentifier,

    -- ** AwsSnsTopicDetails
    AwsSnsTopicDetails (AwsSnsTopicDetails'),
    newAwsSnsTopicDetails,

    -- ** AwsSnsTopicSubscription
    AwsSnsTopicSubscription (AwsSnsTopicSubscription'),
    newAwsSnsTopicSubscription,

    -- ** AwsSqsQueueDetails
    AwsSqsQueueDetails (AwsSqsQueueDetails'),
    newAwsSqsQueueDetails,

    -- ** AwsSsmComplianceSummary
    AwsSsmComplianceSummary (AwsSsmComplianceSummary'),
    newAwsSsmComplianceSummary,

    -- ** AwsSsmPatch
    AwsSsmPatch (AwsSsmPatch'),
    newAwsSsmPatch,

    -- ** AwsSsmPatchComplianceDetails
    AwsSsmPatchComplianceDetails (AwsSsmPatchComplianceDetails'),
    newAwsSsmPatchComplianceDetails,

    -- ** AwsWafRateBasedRuleDetails
    AwsWafRateBasedRuleDetails (AwsWafRateBasedRuleDetails'),
    newAwsWafRateBasedRuleDetails,

    -- ** AwsWafRateBasedRuleMatchPredicate
    AwsWafRateBasedRuleMatchPredicate (AwsWafRateBasedRuleMatchPredicate'),
    newAwsWafRateBasedRuleMatchPredicate,

    -- ** AwsWafRegionalRateBasedRuleDetails
    AwsWafRegionalRateBasedRuleDetails (AwsWafRegionalRateBasedRuleDetails'),
    newAwsWafRegionalRateBasedRuleDetails,

    -- ** AwsWafRegionalRateBasedRuleMatchPredicate
    AwsWafRegionalRateBasedRuleMatchPredicate (AwsWafRegionalRateBasedRuleMatchPredicate'),
    newAwsWafRegionalRateBasedRuleMatchPredicate,

    -- ** AwsWafRegionalRuleDetails
    AwsWafRegionalRuleDetails (AwsWafRegionalRuleDetails'),
    newAwsWafRegionalRuleDetails,

    -- ** AwsWafRegionalRuleGroupDetails
    AwsWafRegionalRuleGroupDetails (AwsWafRegionalRuleGroupDetails'),
    newAwsWafRegionalRuleGroupDetails,

    -- ** AwsWafRegionalRuleGroupRulesActionDetails
    AwsWafRegionalRuleGroupRulesActionDetails (AwsWafRegionalRuleGroupRulesActionDetails'),
    newAwsWafRegionalRuleGroupRulesActionDetails,

    -- ** AwsWafRegionalRuleGroupRulesDetails
    AwsWafRegionalRuleGroupRulesDetails (AwsWafRegionalRuleGroupRulesDetails'),
    newAwsWafRegionalRuleGroupRulesDetails,

    -- ** AwsWafRegionalRulePredicateListDetails
    AwsWafRegionalRulePredicateListDetails (AwsWafRegionalRulePredicateListDetails'),
    newAwsWafRegionalRulePredicateListDetails,

    -- ** AwsWafRegionalWebAclDetails
    AwsWafRegionalWebAclDetails (AwsWafRegionalWebAclDetails'),
    newAwsWafRegionalWebAclDetails,

    -- ** AwsWafRegionalWebAclRulesListActionDetails
    AwsWafRegionalWebAclRulesListActionDetails (AwsWafRegionalWebAclRulesListActionDetails'),
    newAwsWafRegionalWebAclRulesListActionDetails,

    -- ** AwsWafRegionalWebAclRulesListDetails
    AwsWafRegionalWebAclRulesListDetails (AwsWafRegionalWebAclRulesListDetails'),
    newAwsWafRegionalWebAclRulesListDetails,

    -- ** AwsWafRegionalWebAclRulesListOverrideActionDetails
    AwsWafRegionalWebAclRulesListOverrideActionDetails (AwsWafRegionalWebAclRulesListOverrideActionDetails'),
    newAwsWafRegionalWebAclRulesListOverrideActionDetails,

    -- ** AwsWafRuleDetails
    AwsWafRuleDetails (AwsWafRuleDetails'),
    newAwsWafRuleDetails,

    -- ** AwsWafRuleGroupDetails
    AwsWafRuleGroupDetails (AwsWafRuleGroupDetails'),
    newAwsWafRuleGroupDetails,

    -- ** AwsWafRuleGroupRulesActionDetails
    AwsWafRuleGroupRulesActionDetails (AwsWafRuleGroupRulesActionDetails'),
    newAwsWafRuleGroupRulesActionDetails,

    -- ** AwsWafRuleGroupRulesDetails
    AwsWafRuleGroupRulesDetails (AwsWafRuleGroupRulesDetails'),
    newAwsWafRuleGroupRulesDetails,

    -- ** AwsWafRulePredicateListDetails
    AwsWafRulePredicateListDetails (AwsWafRulePredicateListDetails'),
    newAwsWafRulePredicateListDetails,

    -- ** AwsWafWebAclDetails
    AwsWafWebAclDetails (AwsWafWebAclDetails'),
    newAwsWafWebAclDetails,

    -- ** AwsWafWebAclRule
    AwsWafWebAclRule (AwsWafWebAclRule'),
    newAwsWafWebAclRule,

    -- ** AwsWafv2ActionAllowDetails
    AwsWafv2ActionAllowDetails (AwsWafv2ActionAllowDetails'),
    newAwsWafv2ActionAllowDetails,

    -- ** AwsWafv2ActionBlockDetails
    AwsWafv2ActionBlockDetails (AwsWafv2ActionBlockDetails'),
    newAwsWafv2ActionBlockDetails,

    -- ** AwsWafv2CustomHttpHeader
    AwsWafv2CustomHttpHeader (AwsWafv2CustomHttpHeader'),
    newAwsWafv2CustomHttpHeader,

    -- ** AwsWafv2CustomRequestHandlingDetails
    AwsWafv2CustomRequestHandlingDetails (AwsWafv2CustomRequestHandlingDetails'),
    newAwsWafv2CustomRequestHandlingDetails,

    -- ** AwsWafv2CustomResponseDetails
    AwsWafv2CustomResponseDetails (AwsWafv2CustomResponseDetails'),
    newAwsWafv2CustomResponseDetails,

    -- ** AwsWafv2RuleGroupDetails
    AwsWafv2RuleGroupDetails (AwsWafv2RuleGroupDetails'),
    newAwsWafv2RuleGroupDetails,

    -- ** AwsWafv2RulesActionCaptchaDetails
    AwsWafv2RulesActionCaptchaDetails (AwsWafv2RulesActionCaptchaDetails'),
    newAwsWafv2RulesActionCaptchaDetails,

    -- ** AwsWafv2RulesActionCountDetails
    AwsWafv2RulesActionCountDetails (AwsWafv2RulesActionCountDetails'),
    newAwsWafv2RulesActionCountDetails,

    -- ** AwsWafv2RulesActionDetails
    AwsWafv2RulesActionDetails (AwsWafv2RulesActionDetails'),
    newAwsWafv2RulesActionDetails,

    -- ** AwsWafv2RulesDetails
    AwsWafv2RulesDetails (AwsWafv2RulesDetails'),
    newAwsWafv2RulesDetails,

    -- ** AwsWafv2VisibilityConfigDetails
    AwsWafv2VisibilityConfigDetails (AwsWafv2VisibilityConfigDetails'),
    newAwsWafv2VisibilityConfigDetails,

    -- ** AwsWafv2WebAclActionDetails
    AwsWafv2WebAclActionDetails (AwsWafv2WebAclActionDetails'),
    newAwsWafv2WebAclActionDetails,

    -- ** AwsWafv2WebAclCaptchaConfigDetails
    AwsWafv2WebAclCaptchaConfigDetails (AwsWafv2WebAclCaptchaConfigDetails'),
    newAwsWafv2WebAclCaptchaConfigDetails,

    -- ** AwsWafv2WebAclCaptchaConfigImmunityTimePropertyDetails
    AwsWafv2WebAclCaptchaConfigImmunityTimePropertyDetails (AwsWafv2WebAclCaptchaConfigImmunityTimePropertyDetails'),
    newAwsWafv2WebAclCaptchaConfigImmunityTimePropertyDetails,

    -- ** AwsWafv2WebAclDetails
    AwsWafv2WebAclDetails (AwsWafv2WebAclDetails'),
    newAwsWafv2WebAclDetails,

    -- ** AwsXrayEncryptionConfigDetails
    AwsXrayEncryptionConfigDetails (AwsXrayEncryptionConfigDetails'),
    newAwsXrayEncryptionConfigDetails,

    -- ** BatchUpdateFindingsUnprocessedFinding
    BatchUpdateFindingsUnprocessedFinding (BatchUpdateFindingsUnprocessedFinding'),
    newBatchUpdateFindingsUnprocessedFinding,

    -- ** BooleanFilter
    BooleanFilter (BooleanFilter'),
    newBooleanFilter,

    -- ** Cell
    Cell (Cell'),
    newCell,

    -- ** CidrBlockAssociation
    CidrBlockAssociation (CidrBlockAssociation'),
    newCidrBlockAssociation,

    -- ** City
    City (City'),
    newCity,

    -- ** ClassificationResult
    ClassificationResult (ClassificationResult'),
    newClassificationResult,

    -- ** ClassificationStatus
    ClassificationStatus (ClassificationStatus'),
    newClassificationStatus,

    -- ** Compliance
    Compliance (Compliance'),
    newCompliance,

    -- ** ContainerDetails
    ContainerDetails (ContainerDetails'),
    newContainerDetails,

    -- ** Country
    Country (Country'),
    newCountry,

    -- ** CustomDataIdentifiersDetections
    CustomDataIdentifiersDetections (CustomDataIdentifiersDetections'),
    newCustomDataIdentifiersDetections,

    -- ** CustomDataIdentifiersResult
    CustomDataIdentifiersResult (CustomDataIdentifiersResult'),
    newCustomDataIdentifiersResult,

    -- ** Cvss
    Cvss (Cvss'),
    newCvss,

    -- ** DataClassificationDetails
    DataClassificationDetails (DataClassificationDetails'),
    newDataClassificationDetails,

    -- ** DateFilter
    DateFilter (DateFilter'),
    newDateFilter,

    -- ** DateRange
    DateRange (DateRange'),
    newDateRange,

    -- ** DnsRequestAction
    DnsRequestAction (DnsRequestAction'),
    newDnsRequestAction,

    -- ** FilePaths
    FilePaths (FilePaths'),
    newFilePaths,

    -- ** FindingAggregator
    FindingAggregator (FindingAggregator'),
    newFindingAggregator,

    -- ** FindingProviderFields
    FindingProviderFields (FindingProviderFields'),
    newFindingProviderFields,

    -- ** FindingProviderSeverity
    FindingProviderSeverity (FindingProviderSeverity'),
    newFindingProviderSeverity,

    -- ** FirewallPolicyDetails
    FirewallPolicyDetails (FirewallPolicyDetails'),
    newFirewallPolicyDetails,

    -- ** FirewallPolicyStatefulRuleGroupReferencesDetails
    FirewallPolicyStatefulRuleGroupReferencesDetails (FirewallPolicyStatefulRuleGroupReferencesDetails'),
    newFirewallPolicyStatefulRuleGroupReferencesDetails,

    -- ** FirewallPolicyStatelessCustomActionsDetails
    FirewallPolicyStatelessCustomActionsDetails (FirewallPolicyStatelessCustomActionsDetails'),
    newFirewallPolicyStatelessCustomActionsDetails,

    -- ** FirewallPolicyStatelessRuleGroupReferencesDetails
    FirewallPolicyStatelessRuleGroupReferencesDetails (FirewallPolicyStatelessRuleGroupReferencesDetails'),
    newFirewallPolicyStatelessRuleGroupReferencesDetails,

    -- ** GeoLocation
    GeoLocation (GeoLocation'),
    newGeoLocation,

    -- ** IcmpTypeCode
    IcmpTypeCode (IcmpTypeCode'),
    newIcmpTypeCode,

    -- ** ImportFindingsError
    ImportFindingsError (ImportFindingsError'),
    newImportFindingsError,

    -- ** Insight
    Insight (Insight'),
    newInsight,

    -- ** InsightResultValue
    InsightResultValue (InsightResultValue'),
    newInsightResultValue,

    -- ** InsightResults
    InsightResults (InsightResults'),
    newInsightResults,

    -- ** Invitation
    Invitation (Invitation'),
    newInvitation,

    -- ** IpFilter
    IpFilter (IpFilter'),
    newIpFilter,

    -- ** IpOrganizationDetails
    IpOrganizationDetails (IpOrganizationDetails'),
    newIpOrganizationDetails,

    -- ** Ipv6CidrBlockAssociation
    Ipv6CidrBlockAssociation (Ipv6CidrBlockAssociation'),
    newIpv6CidrBlockAssociation,

    -- ** KeywordFilter
    KeywordFilter (KeywordFilter'),
    newKeywordFilter,

    -- ** LoadBalancerState
    LoadBalancerState (LoadBalancerState'),
    newLoadBalancerState,

    -- ** Malware
    Malware (Malware'),
    newMalware,

    -- ** MapFilter
    MapFilter (MapFilter'),
    newMapFilter,

    -- ** Member
    Member (Member'),
    newMember,

    -- ** Network
    Network (Network'),
    newNetwork,

    -- ** NetworkConnectionAction
    NetworkConnectionAction (NetworkConnectionAction'),
    newNetworkConnectionAction,

    -- ** NetworkHeader
    NetworkHeader (NetworkHeader'),
    newNetworkHeader,

    -- ** NetworkPathComponent
    NetworkPathComponent (NetworkPathComponent'),
    newNetworkPathComponent,

    -- ** NetworkPathComponentDetails
    NetworkPathComponentDetails (NetworkPathComponentDetails'),
    newNetworkPathComponentDetails,

    -- ** Note
    Note (Note'),
    newNote,

    -- ** NoteUpdate
    NoteUpdate (NoteUpdate'),
    newNoteUpdate,

    -- ** NumberFilter
    NumberFilter (NumberFilter'),
    newNumberFilter,

    -- ** Occurrences
    Occurrences (Occurrences'),
    newOccurrences,

    -- ** Page
    Page (Page'),
    newPage,

    -- ** PatchSummary
    PatchSummary (PatchSummary'),
    newPatchSummary,

    -- ** PortProbeAction
    PortProbeAction (PortProbeAction'),
    newPortProbeAction,

    -- ** PortProbeDetail
    PortProbeDetail (PortProbeDetail'),
    newPortProbeDetail,

    -- ** PortRange
    PortRange (PortRange'),
    newPortRange,

    -- ** PortRangeFromTo
    PortRangeFromTo (PortRangeFromTo'),
    newPortRangeFromTo,

    -- ** ProcessDetails
    ProcessDetails (ProcessDetails'),
    newProcessDetails,

    -- ** Product
    Product (Product'),
    newProduct,

    -- ** Range
    Range (Range'),
    newRange,

    -- ** Recommendation
    Recommendation (Recommendation'),
    newRecommendation,

    -- ** Record
    Record (Record'),
    newRecord,

    -- ** RelatedFinding
    RelatedFinding (RelatedFinding'),
    newRelatedFinding,

    -- ** Remediation
    Remediation (Remediation'),
    newRemediation,

    -- ** Resource
    Resource (Resource'),
    newResource,

    -- ** ResourceDetails
    ResourceDetails (ResourceDetails'),
    newResourceDetails,

    -- ** Result
    Result (Result'),
    newResult,

    -- ** RuleGroupDetails
    RuleGroupDetails (RuleGroupDetails'),
    newRuleGroupDetails,

    -- ** RuleGroupSource
    RuleGroupSource (RuleGroupSource'),
    newRuleGroupSource,

    -- ** RuleGroupSourceCustomActionsDetails
    RuleGroupSourceCustomActionsDetails (RuleGroupSourceCustomActionsDetails'),
    newRuleGroupSourceCustomActionsDetails,

    -- ** RuleGroupSourceListDetails
    RuleGroupSourceListDetails (RuleGroupSourceListDetails'),
    newRuleGroupSourceListDetails,

    -- ** RuleGroupSourceStatefulRulesDetails
    RuleGroupSourceStatefulRulesDetails (RuleGroupSourceStatefulRulesDetails'),
    newRuleGroupSourceStatefulRulesDetails,

    -- ** RuleGroupSourceStatefulRulesHeaderDetails
    RuleGroupSourceStatefulRulesHeaderDetails (RuleGroupSourceStatefulRulesHeaderDetails'),
    newRuleGroupSourceStatefulRulesHeaderDetails,

    -- ** RuleGroupSourceStatefulRulesOptionsDetails
    RuleGroupSourceStatefulRulesOptionsDetails (RuleGroupSourceStatefulRulesOptionsDetails'),
    newRuleGroupSourceStatefulRulesOptionsDetails,

    -- ** RuleGroupSourceStatelessRuleDefinition
    RuleGroupSourceStatelessRuleDefinition (RuleGroupSourceStatelessRuleDefinition'),
    newRuleGroupSourceStatelessRuleDefinition,

    -- ** RuleGroupSourceStatelessRuleMatchAttributes
    RuleGroupSourceStatelessRuleMatchAttributes (RuleGroupSourceStatelessRuleMatchAttributes'),
    newRuleGroupSourceStatelessRuleMatchAttributes,

    -- ** RuleGroupSourceStatelessRuleMatchAttributesDestinationPorts
    RuleGroupSourceStatelessRuleMatchAttributesDestinationPorts (RuleGroupSourceStatelessRuleMatchAttributesDestinationPorts'),
    newRuleGroupSourceStatelessRuleMatchAttributesDestinationPorts,

    -- ** RuleGroupSourceStatelessRuleMatchAttributesDestinations
    RuleGroupSourceStatelessRuleMatchAttributesDestinations (RuleGroupSourceStatelessRuleMatchAttributesDestinations'),
    newRuleGroupSourceStatelessRuleMatchAttributesDestinations,

    -- ** RuleGroupSourceStatelessRuleMatchAttributesSourcePorts
    RuleGroupSourceStatelessRuleMatchAttributesSourcePorts (RuleGroupSourceStatelessRuleMatchAttributesSourcePorts'),
    newRuleGroupSourceStatelessRuleMatchAttributesSourcePorts,

    -- ** RuleGroupSourceStatelessRuleMatchAttributesSources
    RuleGroupSourceStatelessRuleMatchAttributesSources (RuleGroupSourceStatelessRuleMatchAttributesSources'),
    newRuleGroupSourceStatelessRuleMatchAttributesSources,

    -- ** RuleGroupSourceStatelessRuleMatchAttributesTcpFlags
    RuleGroupSourceStatelessRuleMatchAttributesTcpFlags (RuleGroupSourceStatelessRuleMatchAttributesTcpFlags'),
    newRuleGroupSourceStatelessRuleMatchAttributesTcpFlags,

    -- ** RuleGroupSourceStatelessRulesAndCustomActionsDetails
    RuleGroupSourceStatelessRulesAndCustomActionsDetails (RuleGroupSourceStatelessRulesAndCustomActionsDetails'),
    newRuleGroupSourceStatelessRulesAndCustomActionsDetails,

    -- ** RuleGroupSourceStatelessRulesDetails
    RuleGroupSourceStatelessRulesDetails (RuleGroupSourceStatelessRulesDetails'),
    newRuleGroupSourceStatelessRulesDetails,

    -- ** RuleGroupVariables
    RuleGroupVariables (RuleGroupVariables'),
    newRuleGroupVariables,

    -- ** RuleGroupVariablesIpSetsDetails
    RuleGroupVariablesIpSetsDetails (RuleGroupVariablesIpSetsDetails'),
    newRuleGroupVariablesIpSetsDetails,

    -- ** RuleGroupVariablesPortSetsDetails
    RuleGroupVariablesPortSetsDetails (RuleGroupVariablesPortSetsDetails'),
    newRuleGroupVariablesPortSetsDetails,

    -- ** SensitiveDataDetections
    SensitiveDataDetections (SensitiveDataDetections'),
    newSensitiveDataDetections,

    -- ** SensitiveDataResult
    SensitiveDataResult (SensitiveDataResult'),
    newSensitiveDataResult,

    -- ** Severity
    Severity (Severity'),
    newSeverity,

    -- ** SeverityUpdate
    SeverityUpdate (SeverityUpdate'),
    newSeverityUpdate,

    -- ** SoftwarePackage
    SoftwarePackage (SoftwarePackage'),
    newSoftwarePackage,

    -- ** SortCriterion
    SortCriterion (SortCriterion'),
    newSortCriterion,

    -- ** Standard
    Standard (Standard'),
    newStandard,

    -- ** StandardsControl
    StandardsControl (StandardsControl'),
    newStandardsControl,

    -- ** StandardsManagedBy
    StandardsManagedBy (StandardsManagedBy'),
    newStandardsManagedBy,

    -- ** StandardsStatusReason
    StandardsStatusReason (StandardsStatusReason'),
    newStandardsStatusReason,

    -- ** StandardsSubscription
    StandardsSubscription (StandardsSubscription'),
    newStandardsSubscription,

    -- ** StandardsSubscriptionRequest
    StandardsSubscriptionRequest (StandardsSubscriptionRequest'),
    newStandardsSubscriptionRequest,

    -- ** StatelessCustomActionDefinition
    StatelessCustomActionDefinition (StatelessCustomActionDefinition'),
    newStatelessCustomActionDefinition,

    -- ** StatelessCustomPublishMetricAction
    StatelessCustomPublishMetricAction (StatelessCustomPublishMetricAction'),
    newStatelessCustomPublishMetricAction,

    -- ** StatelessCustomPublishMetricActionDimension
    StatelessCustomPublishMetricActionDimension (StatelessCustomPublishMetricActionDimension'),
    newStatelessCustomPublishMetricActionDimension,

    -- ** StatusReason
    StatusReason (StatusReason'),
    newStatusReason,

    -- ** StringFilter
    StringFilter (StringFilter'),
    newStringFilter,

    -- ** Threat
    Threat (Threat'),
    newThreat,

    -- ** ThreatIntelIndicator
    ThreatIntelIndicator (ThreatIntelIndicator'),
    newThreatIntelIndicator,

    -- ** VolumeMount
    VolumeMount (VolumeMount'),
    newVolumeMount,

    -- ** VpcInfoCidrBlockSetDetails
    VpcInfoCidrBlockSetDetails (VpcInfoCidrBlockSetDetails'),
    newVpcInfoCidrBlockSetDetails,

    -- ** VpcInfoIpv6CidrBlockSetDetails
    VpcInfoIpv6CidrBlockSetDetails (VpcInfoIpv6CidrBlockSetDetails'),
    newVpcInfoIpv6CidrBlockSetDetails,

    -- ** VpcInfoPeeringOptionsDetails
    VpcInfoPeeringOptionsDetails (VpcInfoPeeringOptionsDetails'),
    newVpcInfoPeeringOptionsDetails,

    -- ** Vulnerability
    Vulnerability (Vulnerability'),
    newVulnerability,

    -- ** VulnerabilityVendor
    VulnerabilityVendor (VulnerabilityVendor'),
    newVulnerabilityVendor,

    -- ** WafAction
    WafAction (WafAction'),
    newWafAction,

    -- ** WafExcludedRule
    WafExcludedRule (WafExcludedRule'),
    newWafExcludedRule,

    -- ** WafOverrideAction
    WafOverrideAction (WafOverrideAction'),
    newWafOverrideAction,

    -- ** Workflow
    Workflow (Workflow'),
    newWorkflow,

    -- ** WorkflowUpdate
    WorkflowUpdate (WorkflowUpdate'),
    newWorkflowUpdate,
  )
where

import Amazonka.SecurityHub.AcceptAdministratorInvitation
import Amazonka.SecurityHub.BatchDisableStandards
import Amazonka.SecurityHub.BatchEnableStandards
import Amazonka.SecurityHub.BatchImportFindings
import Amazonka.SecurityHub.BatchUpdateFindings
import Amazonka.SecurityHub.CreateActionTarget
import Amazonka.SecurityHub.CreateFindingAggregator
import Amazonka.SecurityHub.CreateInsight
import Amazonka.SecurityHub.CreateMembers
import Amazonka.SecurityHub.DeclineInvitations
import Amazonka.SecurityHub.DeleteActionTarget
import Amazonka.SecurityHub.DeleteFindingAggregator
import Amazonka.SecurityHub.DeleteInsight
import Amazonka.SecurityHub.DeleteInvitations
import Amazonka.SecurityHub.DeleteMembers
import Amazonka.SecurityHub.DescribeActionTargets
import Amazonka.SecurityHub.DescribeHub
import Amazonka.SecurityHub.DescribeOrganizationConfiguration
import Amazonka.SecurityHub.DescribeProducts
import Amazonka.SecurityHub.DescribeStandards
import Amazonka.SecurityHub.DescribeStandardsControls
import Amazonka.SecurityHub.DisableImportFindingsForProduct
import Amazonka.SecurityHub.DisableOrganizationAdminAccount
import Amazonka.SecurityHub.DisableSecurityHub
import Amazonka.SecurityHub.DisassociateFromAdministratorAccount
import Amazonka.SecurityHub.DisassociateMembers
import Amazonka.SecurityHub.EnableImportFindingsForProduct
import Amazonka.SecurityHub.EnableOrganizationAdminAccount
import Amazonka.SecurityHub.EnableSecurityHub
import Amazonka.SecurityHub.GetAdministratorAccount
import Amazonka.SecurityHub.GetEnabledStandards
import Amazonka.SecurityHub.GetFindingAggregator
import Amazonka.SecurityHub.GetFindings
import Amazonka.SecurityHub.GetInsightResults
import Amazonka.SecurityHub.GetInsights
import Amazonka.SecurityHub.GetInvitationsCount
import Amazonka.SecurityHub.GetMembers
import Amazonka.SecurityHub.InviteMembers
import Amazonka.SecurityHub.Lens
import Amazonka.SecurityHub.ListEnabledProductsForImport
import Amazonka.SecurityHub.ListFindingAggregators
import Amazonka.SecurityHub.ListInvitations
import Amazonka.SecurityHub.ListMembers
import Amazonka.SecurityHub.ListOrganizationAdminAccounts
import Amazonka.SecurityHub.ListTagsForResource
import Amazonka.SecurityHub.TagResource
import Amazonka.SecurityHub.Types
import Amazonka.SecurityHub.UntagResource
import Amazonka.SecurityHub.UpdateActionTarget
import Amazonka.SecurityHub.UpdateFindingAggregator
import Amazonka.SecurityHub.UpdateFindings
import Amazonka.SecurityHub.UpdateInsight
import Amazonka.SecurityHub.UpdateOrganizationConfiguration
import Amazonka.SecurityHub.UpdateSecurityHubConfiguration
import Amazonka.SecurityHub.UpdateStandardsControl
import Amazonka.SecurityHub.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'SecurityHub'.

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
