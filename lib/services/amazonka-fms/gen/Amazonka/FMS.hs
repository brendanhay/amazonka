{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.FMS
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2018-01-01@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- This is the /Firewall Manager API Reference/. This guide is for
-- developers who need detailed information about the Firewall Manager API
-- actions, data types, and errors. For detailed information about Firewall
-- Manager features, see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/fms-chapter.html Firewall Manager Developer Guide>.
--
-- Some API actions require explicit resource permissions. For information,
-- see the developer guide topic
-- <https://docs.aws.amazon.com/waf/latest/developerguide/fms-api-permissions-ref.html Firewall Manager required permissions for API actions>.
module Amazonka.FMS
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InvalidInputException
    _InvalidInputException,

    -- ** InvalidOperationException
    _InvalidOperationException,

    -- ** InternalErrorException
    _InternalErrorException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** InvalidTypeException
    _InvalidTypeException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AssociateAdminAccount
    AssociateAdminAccount (AssociateAdminAccount'),
    newAssociateAdminAccount,
    AssociateAdminAccountResponse (AssociateAdminAccountResponse'),
    newAssociateAdminAccountResponse,

    -- ** AssociateThirdPartyFirewall
    AssociateThirdPartyFirewall (AssociateThirdPartyFirewall'),
    newAssociateThirdPartyFirewall,
    AssociateThirdPartyFirewallResponse (AssociateThirdPartyFirewallResponse'),
    newAssociateThirdPartyFirewallResponse,

    -- ** BatchAssociateResource
    BatchAssociateResource (BatchAssociateResource'),
    newBatchAssociateResource,
    BatchAssociateResourceResponse (BatchAssociateResourceResponse'),
    newBatchAssociateResourceResponse,

    -- ** BatchDisassociateResource
    BatchDisassociateResource (BatchDisassociateResource'),
    newBatchDisassociateResource,
    BatchDisassociateResourceResponse (BatchDisassociateResourceResponse'),
    newBatchDisassociateResourceResponse,

    -- ** DeleteAppsList
    DeleteAppsList (DeleteAppsList'),
    newDeleteAppsList,
    DeleteAppsListResponse (DeleteAppsListResponse'),
    newDeleteAppsListResponse,

    -- ** DeleteNotificationChannel
    DeleteNotificationChannel (DeleteNotificationChannel'),
    newDeleteNotificationChannel,
    DeleteNotificationChannelResponse (DeleteNotificationChannelResponse'),
    newDeleteNotificationChannelResponse,

    -- ** DeletePolicy
    DeletePolicy (DeletePolicy'),
    newDeletePolicy,
    DeletePolicyResponse (DeletePolicyResponse'),
    newDeletePolicyResponse,

    -- ** DeleteProtocolsList
    DeleteProtocolsList (DeleteProtocolsList'),
    newDeleteProtocolsList,
    DeleteProtocolsListResponse (DeleteProtocolsListResponse'),
    newDeleteProtocolsListResponse,

    -- ** DeleteResourceSet
    DeleteResourceSet (DeleteResourceSet'),
    newDeleteResourceSet,
    DeleteResourceSetResponse (DeleteResourceSetResponse'),
    newDeleteResourceSetResponse,

    -- ** DisassociateAdminAccount
    DisassociateAdminAccount (DisassociateAdminAccount'),
    newDisassociateAdminAccount,
    DisassociateAdminAccountResponse (DisassociateAdminAccountResponse'),
    newDisassociateAdminAccountResponse,

    -- ** DisassociateThirdPartyFirewall
    DisassociateThirdPartyFirewall (DisassociateThirdPartyFirewall'),
    newDisassociateThirdPartyFirewall,
    DisassociateThirdPartyFirewallResponse (DisassociateThirdPartyFirewallResponse'),
    newDisassociateThirdPartyFirewallResponse,

    -- ** GetAdminAccount
    GetAdminAccount (GetAdminAccount'),
    newGetAdminAccount,
    GetAdminAccountResponse (GetAdminAccountResponse'),
    newGetAdminAccountResponse,

    -- ** GetAppsList
    GetAppsList (GetAppsList'),
    newGetAppsList,
    GetAppsListResponse (GetAppsListResponse'),
    newGetAppsListResponse,

    -- ** GetComplianceDetail
    GetComplianceDetail (GetComplianceDetail'),
    newGetComplianceDetail,
    GetComplianceDetailResponse (GetComplianceDetailResponse'),
    newGetComplianceDetailResponse,

    -- ** GetNotificationChannel
    GetNotificationChannel (GetNotificationChannel'),
    newGetNotificationChannel,
    GetNotificationChannelResponse (GetNotificationChannelResponse'),
    newGetNotificationChannelResponse,

    -- ** GetPolicy
    GetPolicy (GetPolicy'),
    newGetPolicy,
    GetPolicyResponse (GetPolicyResponse'),
    newGetPolicyResponse,

    -- ** GetProtectionStatus
    GetProtectionStatus (GetProtectionStatus'),
    newGetProtectionStatus,
    GetProtectionStatusResponse (GetProtectionStatusResponse'),
    newGetProtectionStatusResponse,

    -- ** GetProtocolsList
    GetProtocolsList (GetProtocolsList'),
    newGetProtocolsList,
    GetProtocolsListResponse (GetProtocolsListResponse'),
    newGetProtocolsListResponse,

    -- ** GetResourceSet
    GetResourceSet (GetResourceSet'),
    newGetResourceSet,
    GetResourceSetResponse (GetResourceSetResponse'),
    newGetResourceSetResponse,

    -- ** GetThirdPartyFirewallAssociationStatus
    GetThirdPartyFirewallAssociationStatus (GetThirdPartyFirewallAssociationStatus'),
    newGetThirdPartyFirewallAssociationStatus,
    GetThirdPartyFirewallAssociationStatusResponse (GetThirdPartyFirewallAssociationStatusResponse'),
    newGetThirdPartyFirewallAssociationStatusResponse,

    -- ** GetViolationDetails
    GetViolationDetails (GetViolationDetails'),
    newGetViolationDetails,
    GetViolationDetailsResponse (GetViolationDetailsResponse'),
    newGetViolationDetailsResponse,

    -- ** ListAppsLists (Paginated)
    ListAppsLists (ListAppsLists'),
    newListAppsLists,
    ListAppsListsResponse (ListAppsListsResponse'),
    newListAppsListsResponse,

    -- ** ListComplianceStatus (Paginated)
    ListComplianceStatus (ListComplianceStatus'),
    newListComplianceStatus,
    ListComplianceStatusResponse (ListComplianceStatusResponse'),
    newListComplianceStatusResponse,

    -- ** ListDiscoveredResources
    ListDiscoveredResources (ListDiscoveredResources'),
    newListDiscoveredResources,
    ListDiscoveredResourcesResponse (ListDiscoveredResourcesResponse'),
    newListDiscoveredResourcesResponse,

    -- ** ListMemberAccounts (Paginated)
    ListMemberAccounts (ListMemberAccounts'),
    newListMemberAccounts,
    ListMemberAccountsResponse (ListMemberAccountsResponse'),
    newListMemberAccountsResponse,

    -- ** ListPolicies (Paginated)
    ListPolicies (ListPolicies'),
    newListPolicies,
    ListPoliciesResponse (ListPoliciesResponse'),
    newListPoliciesResponse,

    -- ** ListProtocolsLists (Paginated)
    ListProtocolsLists (ListProtocolsLists'),
    newListProtocolsLists,
    ListProtocolsListsResponse (ListProtocolsListsResponse'),
    newListProtocolsListsResponse,

    -- ** ListResourceSetResources
    ListResourceSetResources (ListResourceSetResources'),
    newListResourceSetResources,
    ListResourceSetResourcesResponse (ListResourceSetResourcesResponse'),
    newListResourceSetResourcesResponse,

    -- ** ListResourceSets
    ListResourceSets (ListResourceSets'),
    newListResourceSets,
    ListResourceSetsResponse (ListResourceSetsResponse'),
    newListResourceSetsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListThirdPartyFirewallFirewallPolicies (Paginated)
    ListThirdPartyFirewallFirewallPolicies (ListThirdPartyFirewallFirewallPolicies'),
    newListThirdPartyFirewallFirewallPolicies,
    ListThirdPartyFirewallFirewallPoliciesResponse (ListThirdPartyFirewallFirewallPoliciesResponse'),
    newListThirdPartyFirewallFirewallPoliciesResponse,

    -- ** PutAppsList
    PutAppsList (PutAppsList'),
    newPutAppsList,
    PutAppsListResponse (PutAppsListResponse'),
    newPutAppsListResponse,

    -- ** PutNotificationChannel
    PutNotificationChannel (PutNotificationChannel'),
    newPutNotificationChannel,
    PutNotificationChannelResponse (PutNotificationChannelResponse'),
    newPutNotificationChannelResponse,

    -- ** PutPolicy
    PutPolicy (PutPolicy'),
    newPutPolicy,
    PutPolicyResponse (PutPolicyResponse'),
    newPutPolicyResponse,

    -- ** PutProtocolsList
    PutProtocolsList (PutProtocolsList'),
    newPutProtocolsList,
    PutProtocolsListResponse (PutProtocolsListResponse'),
    newPutProtocolsListResponse,

    -- ** PutResourceSet
    PutResourceSet (PutResourceSet'),
    newPutResourceSet,
    PutResourceSetResponse (PutResourceSetResponse'),
    newPutResourceSetResponse,

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

    -- * Types

    -- ** AccountRoleStatus
    AccountRoleStatus (..),

    -- ** CustomerPolicyScopeIdType
    CustomerPolicyScopeIdType (..),

    -- ** DependentServiceName
    DependentServiceName (..),

    -- ** DestinationType
    DestinationType (..),

    -- ** FailedItemReason
    FailedItemReason (..),

    -- ** FirewallDeploymentModel
    FirewallDeploymentModel (..),

    -- ** MarketplaceSubscriptionOnboardingStatus
    MarketplaceSubscriptionOnboardingStatus (..),

    -- ** NetworkFirewallOverrideAction
    NetworkFirewallOverrideAction (..),

    -- ** PolicyComplianceStatusType
    PolicyComplianceStatusType (..),

    -- ** RemediationActionType
    RemediationActionType (..),

    -- ** RuleOrder
    RuleOrder (..),

    -- ** SecurityServiceType
    SecurityServiceType (..),

    -- ** TargetType
    TargetType (..),

    -- ** ThirdPartyFirewall
    ThirdPartyFirewall (..),

    -- ** ThirdPartyFirewallAssociationStatus
    ThirdPartyFirewallAssociationStatus (..),

    -- ** ViolationReason
    ViolationReason (..),

    -- ** ActionTarget
    ActionTarget (ActionTarget'),
    newActionTarget,

    -- ** App
    App (App'),
    newApp,

    -- ** AppsListData
    AppsListData (AppsListData'),
    newAppsListData,

    -- ** AppsListDataSummary
    AppsListDataSummary (AppsListDataSummary'),
    newAppsListDataSummary,

    -- ** AwsEc2InstanceViolation
    AwsEc2InstanceViolation (AwsEc2InstanceViolation'),
    newAwsEc2InstanceViolation,

    -- ** AwsEc2NetworkInterfaceViolation
    AwsEc2NetworkInterfaceViolation (AwsEc2NetworkInterfaceViolation'),
    newAwsEc2NetworkInterfaceViolation,

    -- ** AwsVPCSecurityGroupViolation
    AwsVPCSecurityGroupViolation (AwsVPCSecurityGroupViolation'),
    newAwsVPCSecurityGroupViolation,

    -- ** ComplianceViolator
    ComplianceViolator (ComplianceViolator'),
    newComplianceViolator,

    -- ** DiscoveredResource
    DiscoveredResource (DiscoveredResource'),
    newDiscoveredResource,

    -- ** DnsDuplicateRuleGroupViolation
    DnsDuplicateRuleGroupViolation (DnsDuplicateRuleGroupViolation'),
    newDnsDuplicateRuleGroupViolation,

    -- ** DnsRuleGroupLimitExceededViolation
    DnsRuleGroupLimitExceededViolation (DnsRuleGroupLimitExceededViolation'),
    newDnsRuleGroupLimitExceededViolation,

    -- ** DnsRuleGroupPriorityConflictViolation
    DnsRuleGroupPriorityConflictViolation (DnsRuleGroupPriorityConflictViolation'),
    newDnsRuleGroupPriorityConflictViolation,

    -- ** EC2AssociateRouteTableAction
    EC2AssociateRouteTableAction (EC2AssociateRouteTableAction'),
    newEC2AssociateRouteTableAction,

    -- ** EC2CopyRouteTableAction
    EC2CopyRouteTableAction (EC2CopyRouteTableAction'),
    newEC2CopyRouteTableAction,

    -- ** EC2CreateRouteAction
    EC2CreateRouteAction (EC2CreateRouteAction'),
    newEC2CreateRouteAction,

    -- ** EC2CreateRouteTableAction
    EC2CreateRouteTableAction (EC2CreateRouteTableAction'),
    newEC2CreateRouteTableAction,

    -- ** EC2DeleteRouteAction
    EC2DeleteRouteAction (EC2DeleteRouteAction'),
    newEC2DeleteRouteAction,

    -- ** EC2ReplaceRouteAction
    EC2ReplaceRouteAction (EC2ReplaceRouteAction'),
    newEC2ReplaceRouteAction,

    -- ** EC2ReplaceRouteTableAssociationAction
    EC2ReplaceRouteTableAssociationAction (EC2ReplaceRouteTableAssociationAction'),
    newEC2ReplaceRouteTableAssociationAction,

    -- ** EvaluationResult
    EvaluationResult (EvaluationResult'),
    newEvaluationResult,

    -- ** ExpectedRoute
    ExpectedRoute (ExpectedRoute'),
    newExpectedRoute,

    -- ** FMSPolicyUpdateFirewallCreationConfigAction
    FMSPolicyUpdateFirewallCreationConfigAction (FMSPolicyUpdateFirewallCreationConfigAction'),
    newFMSPolicyUpdateFirewallCreationConfigAction,

    -- ** FailedItem
    FailedItem (FailedItem'),
    newFailedItem,

    -- ** FirewallSubnetIsOutOfScopeViolation
    FirewallSubnetIsOutOfScopeViolation (FirewallSubnetIsOutOfScopeViolation'),
    newFirewallSubnetIsOutOfScopeViolation,

    -- ** FirewallSubnetMissingVPCEndpointViolation
    FirewallSubnetMissingVPCEndpointViolation (FirewallSubnetMissingVPCEndpointViolation'),
    newFirewallSubnetMissingVPCEndpointViolation,

    -- ** NetworkFirewallBlackHoleRouteDetectedViolation
    NetworkFirewallBlackHoleRouteDetectedViolation (NetworkFirewallBlackHoleRouteDetectedViolation'),
    newNetworkFirewallBlackHoleRouteDetectedViolation,

    -- ** NetworkFirewallInternetTrafficNotInspectedViolation
    NetworkFirewallInternetTrafficNotInspectedViolation (NetworkFirewallInternetTrafficNotInspectedViolation'),
    newNetworkFirewallInternetTrafficNotInspectedViolation,

    -- ** NetworkFirewallInvalidRouteConfigurationViolation
    NetworkFirewallInvalidRouteConfigurationViolation (NetworkFirewallInvalidRouteConfigurationViolation'),
    newNetworkFirewallInvalidRouteConfigurationViolation,

    -- ** NetworkFirewallMissingExpectedRTViolation
    NetworkFirewallMissingExpectedRTViolation (NetworkFirewallMissingExpectedRTViolation'),
    newNetworkFirewallMissingExpectedRTViolation,

    -- ** NetworkFirewallMissingExpectedRoutesViolation
    NetworkFirewallMissingExpectedRoutesViolation (NetworkFirewallMissingExpectedRoutesViolation'),
    newNetworkFirewallMissingExpectedRoutesViolation,

    -- ** NetworkFirewallMissingFirewallViolation
    NetworkFirewallMissingFirewallViolation (NetworkFirewallMissingFirewallViolation'),
    newNetworkFirewallMissingFirewallViolation,

    -- ** NetworkFirewallMissingSubnetViolation
    NetworkFirewallMissingSubnetViolation (NetworkFirewallMissingSubnetViolation'),
    newNetworkFirewallMissingSubnetViolation,

    -- ** NetworkFirewallPolicy
    NetworkFirewallPolicy (NetworkFirewallPolicy'),
    newNetworkFirewallPolicy,

    -- ** NetworkFirewallPolicyDescription
    NetworkFirewallPolicyDescription (NetworkFirewallPolicyDescription'),
    newNetworkFirewallPolicyDescription,

    -- ** NetworkFirewallPolicyModifiedViolation
    NetworkFirewallPolicyModifiedViolation (NetworkFirewallPolicyModifiedViolation'),
    newNetworkFirewallPolicyModifiedViolation,

    -- ** NetworkFirewallStatefulRuleGroupOverride
    NetworkFirewallStatefulRuleGroupOverride (NetworkFirewallStatefulRuleGroupOverride'),
    newNetworkFirewallStatefulRuleGroupOverride,

    -- ** NetworkFirewallUnexpectedFirewallRoutesViolation
    NetworkFirewallUnexpectedFirewallRoutesViolation (NetworkFirewallUnexpectedFirewallRoutesViolation'),
    newNetworkFirewallUnexpectedFirewallRoutesViolation,

    -- ** NetworkFirewallUnexpectedGatewayRoutesViolation
    NetworkFirewallUnexpectedGatewayRoutesViolation (NetworkFirewallUnexpectedGatewayRoutesViolation'),
    newNetworkFirewallUnexpectedGatewayRoutesViolation,

    -- ** PartialMatch
    PartialMatch (PartialMatch'),
    newPartialMatch,

    -- ** Policy
    Policy (Policy'),
    newPolicy,

    -- ** PolicyComplianceDetail
    PolicyComplianceDetail (PolicyComplianceDetail'),
    newPolicyComplianceDetail,

    -- ** PolicyComplianceStatus
    PolicyComplianceStatus (PolicyComplianceStatus'),
    newPolicyComplianceStatus,

    -- ** PolicyOption
    PolicyOption (PolicyOption'),
    newPolicyOption,

    -- ** PolicySummary
    PolicySummary (PolicySummary'),
    newPolicySummary,

    -- ** PossibleRemediationAction
    PossibleRemediationAction (PossibleRemediationAction'),
    newPossibleRemediationAction,

    -- ** PossibleRemediationActions
    PossibleRemediationActions (PossibleRemediationActions'),
    newPossibleRemediationActions,

    -- ** ProtocolsListData
    ProtocolsListData (ProtocolsListData'),
    newProtocolsListData,

    -- ** ProtocolsListDataSummary
    ProtocolsListDataSummary (ProtocolsListDataSummary'),
    newProtocolsListDataSummary,

    -- ** RemediationAction
    RemediationAction (RemediationAction'),
    newRemediationAction,

    -- ** RemediationActionWithOrder
    RemediationActionWithOrder (RemediationActionWithOrder'),
    newRemediationActionWithOrder,

    -- ** Resource
    Resource (Resource'),
    newResource,

    -- ** ResourceSet
    ResourceSet (ResourceSet'),
    newResourceSet,

    -- ** ResourceSetSummary
    ResourceSetSummary (ResourceSetSummary'),
    newResourceSetSummary,

    -- ** ResourceTag
    ResourceTag (ResourceTag'),
    newResourceTag,

    -- ** ResourceViolation
    ResourceViolation (ResourceViolation'),
    newResourceViolation,

    -- ** Route
    Route (Route'),
    newRoute,

    -- ** RouteHasOutOfScopeEndpointViolation
    RouteHasOutOfScopeEndpointViolation (RouteHasOutOfScopeEndpointViolation'),
    newRouteHasOutOfScopeEndpointViolation,

    -- ** SecurityGroupRemediationAction
    SecurityGroupRemediationAction (SecurityGroupRemediationAction'),
    newSecurityGroupRemediationAction,

    -- ** SecurityGroupRuleDescription
    SecurityGroupRuleDescription (SecurityGroupRuleDescription'),
    newSecurityGroupRuleDescription,

    -- ** SecurityServicePolicyData
    SecurityServicePolicyData (SecurityServicePolicyData'),
    newSecurityServicePolicyData,

    -- ** StatefulEngineOptions
    StatefulEngineOptions (StatefulEngineOptions'),
    newStatefulEngineOptions,

    -- ** StatefulRuleGroup
    StatefulRuleGroup (StatefulRuleGroup'),
    newStatefulRuleGroup,

    -- ** StatelessRuleGroup
    StatelessRuleGroup (StatelessRuleGroup'),
    newStatelessRuleGroup,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** ThirdPartyFirewallFirewallPolicy
    ThirdPartyFirewallFirewallPolicy (ThirdPartyFirewallFirewallPolicy'),
    newThirdPartyFirewallFirewallPolicy,

    -- ** ThirdPartyFirewallMissingExpectedRouteTableViolation
    ThirdPartyFirewallMissingExpectedRouteTableViolation (ThirdPartyFirewallMissingExpectedRouteTableViolation'),
    newThirdPartyFirewallMissingExpectedRouteTableViolation,

    -- ** ThirdPartyFirewallMissingFirewallViolation
    ThirdPartyFirewallMissingFirewallViolation (ThirdPartyFirewallMissingFirewallViolation'),
    newThirdPartyFirewallMissingFirewallViolation,

    -- ** ThirdPartyFirewallMissingSubnetViolation
    ThirdPartyFirewallMissingSubnetViolation (ThirdPartyFirewallMissingSubnetViolation'),
    newThirdPartyFirewallMissingSubnetViolation,

    -- ** ThirdPartyFirewallPolicy
    ThirdPartyFirewallPolicy (ThirdPartyFirewallPolicy'),
    newThirdPartyFirewallPolicy,

    -- ** ViolationDetail
    ViolationDetail (ViolationDetail'),
    newViolationDetail,
  )
where

import Amazonka.FMS.AssociateAdminAccount
import Amazonka.FMS.AssociateThirdPartyFirewall
import Amazonka.FMS.BatchAssociateResource
import Amazonka.FMS.BatchDisassociateResource
import Amazonka.FMS.DeleteAppsList
import Amazonka.FMS.DeleteNotificationChannel
import Amazonka.FMS.DeletePolicy
import Amazonka.FMS.DeleteProtocolsList
import Amazonka.FMS.DeleteResourceSet
import Amazonka.FMS.DisassociateAdminAccount
import Amazonka.FMS.DisassociateThirdPartyFirewall
import Amazonka.FMS.GetAdminAccount
import Amazonka.FMS.GetAppsList
import Amazonka.FMS.GetComplianceDetail
import Amazonka.FMS.GetNotificationChannel
import Amazonka.FMS.GetPolicy
import Amazonka.FMS.GetProtectionStatus
import Amazonka.FMS.GetProtocolsList
import Amazonka.FMS.GetResourceSet
import Amazonka.FMS.GetThirdPartyFirewallAssociationStatus
import Amazonka.FMS.GetViolationDetails
import Amazonka.FMS.Lens
import Amazonka.FMS.ListAppsLists
import Amazonka.FMS.ListComplianceStatus
import Amazonka.FMS.ListDiscoveredResources
import Amazonka.FMS.ListMemberAccounts
import Amazonka.FMS.ListPolicies
import Amazonka.FMS.ListProtocolsLists
import Amazonka.FMS.ListResourceSetResources
import Amazonka.FMS.ListResourceSets
import Amazonka.FMS.ListTagsForResource
import Amazonka.FMS.ListThirdPartyFirewallFirewallPolicies
import Amazonka.FMS.PutAppsList
import Amazonka.FMS.PutNotificationChannel
import Amazonka.FMS.PutPolicy
import Amazonka.FMS.PutProtocolsList
import Amazonka.FMS.PutResourceSet
import Amazonka.FMS.TagResource
import Amazonka.FMS.Types
import Amazonka.FMS.UntagResource
import Amazonka.FMS.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'FMS'.

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
