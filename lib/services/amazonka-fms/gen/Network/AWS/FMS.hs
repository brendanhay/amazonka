{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.FMS
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.FMS
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InternalErrorException
    _InternalErrorException,

    -- ** InvalidInputException
    _InvalidInputException,

    -- ** InvalidOperationException
    _InvalidOperationException,

    -- ** InvalidTypeException
    _InvalidTypeException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListPolicies (Paginated)
    ListPolicies (ListPolicies'),
    newListPolicies,
    ListPoliciesResponse (ListPoliciesResponse'),
    newListPoliciesResponse,

    -- ** GetComplianceDetail
    GetComplianceDetail (GetComplianceDetail'),
    newGetComplianceDetail,
    GetComplianceDetailResponse (GetComplianceDetailResponse'),
    newGetComplianceDetailResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** GetNotificationChannel
    GetNotificationChannel (GetNotificationChannel'),
    newGetNotificationChannel,
    GetNotificationChannelResponse (GetNotificationChannelResponse'),
    newGetNotificationChannelResponse,

    -- ** GetAdminAccount
    GetAdminAccount (GetAdminAccount'),
    newGetAdminAccount,
    GetAdminAccountResponse (GetAdminAccountResponse'),
    newGetAdminAccountResponse,

    -- ** ListComplianceStatus (Paginated)
    ListComplianceStatus (ListComplianceStatus'),
    newListComplianceStatus,
    ListComplianceStatusResponse (ListComplianceStatusResponse'),
    newListComplianceStatusResponse,

    -- ** GetAppsList
    GetAppsList (GetAppsList'),
    newGetAppsList,
    GetAppsListResponse (GetAppsListResponse'),
    newGetAppsListResponse,

    -- ** PutPolicy
    PutPolicy (PutPolicy'),
    newPutPolicy,
    PutPolicyResponse (PutPolicyResponse'),
    newPutPolicyResponse,

    -- ** DeletePolicy
    DeletePolicy (DeletePolicy'),
    newDeletePolicy,
    DeletePolicyResponse (DeletePolicyResponse'),
    newDeletePolicyResponse,

    -- ** DisassociateAdminAccount
    DisassociateAdminAccount (DisassociateAdminAccount'),
    newDisassociateAdminAccount,
    DisassociateAdminAccountResponse (DisassociateAdminAccountResponse'),
    newDisassociateAdminAccountResponse,

    -- ** PutNotificationChannel
    PutNotificationChannel (PutNotificationChannel'),
    newPutNotificationChannel,
    PutNotificationChannelResponse (PutNotificationChannelResponse'),
    newPutNotificationChannelResponse,

    -- ** DeleteNotificationChannel
    DeleteNotificationChannel (DeleteNotificationChannel'),
    newDeleteNotificationChannel,
    DeleteNotificationChannelResponse (DeleteNotificationChannelResponse'),
    newDeleteNotificationChannelResponse,

    -- ** AssociateAdminAccount
    AssociateAdminAccount (AssociateAdminAccount'),
    newAssociateAdminAccount,
    AssociateAdminAccountResponse (AssociateAdminAccountResponse'),
    newAssociateAdminAccountResponse,

    -- ** GetViolationDetails
    GetViolationDetails (GetViolationDetails'),
    newGetViolationDetails,
    GetViolationDetailsResponse (GetViolationDetailsResponse'),
    newGetViolationDetailsResponse,

    -- ** ListMemberAccounts (Paginated)
    ListMemberAccounts (ListMemberAccounts'),
    newListMemberAccounts,
    ListMemberAccountsResponse (ListMemberAccountsResponse'),
    newListMemberAccountsResponse,

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

    -- ** DeleteProtocolsList
    DeleteProtocolsList (DeleteProtocolsList'),
    newDeleteProtocolsList,
    DeleteProtocolsListResponse (DeleteProtocolsListResponse'),
    newDeleteProtocolsListResponse,

    -- ** GetPolicy
    GetPolicy (GetPolicy'),
    newGetPolicy,
    GetPolicyResponse (GetPolicyResponse'),
    newGetPolicyResponse,

    -- ** ListProtocolsLists (Paginated)
    ListProtocolsLists (ListProtocolsLists'),
    newListProtocolsLists,
    ListProtocolsListsResponse (ListProtocolsListsResponse'),
    newListProtocolsListsResponse,

    -- ** PutProtocolsList
    PutProtocolsList (PutProtocolsList'),
    newPutProtocolsList,
    PutProtocolsListResponse (PutProtocolsListResponse'),
    newPutProtocolsListResponse,

    -- ** PutAppsList
    PutAppsList (PutAppsList'),
    newPutAppsList,
    PutAppsListResponse (PutAppsListResponse'),
    newPutAppsListResponse,

    -- ** DeleteAppsList
    DeleteAppsList (DeleteAppsList'),
    newDeleteAppsList,
    DeleteAppsListResponse (DeleteAppsListResponse'),
    newDeleteAppsListResponse,

    -- ** ListAppsLists (Paginated)
    ListAppsLists (ListAppsLists'),
    newListAppsLists,
    ListAppsListsResponse (ListAppsListsResponse'),
    newListAppsListsResponse,

    -- ** GetProtocolsList
    GetProtocolsList (GetProtocolsList'),
    newGetProtocolsList,
    GetProtocolsListResponse (GetProtocolsListResponse'),
    newGetProtocolsListResponse,

    -- ** GetProtectionStatus
    GetProtectionStatus (GetProtectionStatus'),
    newGetProtectionStatus,
    GetProtectionStatusResponse (GetProtectionStatusResponse'),
    newGetProtectionStatusResponse,

    -- * Types

    -- ** AccountRoleStatus
    AccountRoleStatus (..),

    -- ** CustomerPolicyScopeIdType
    CustomerPolicyScopeIdType (..),

    -- ** DependentServiceName
    DependentServiceName (..),

    -- ** DestinationType
    DestinationType (..),

    -- ** PolicyComplianceStatusType
    PolicyComplianceStatusType (..),

    -- ** RemediationActionType
    RemediationActionType (..),

    -- ** SecurityServiceType
    SecurityServiceType (..),

    -- ** TargetType
    TargetType (..),

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

    -- ** NetworkFirewallPolicyDescription
    NetworkFirewallPolicyDescription (NetworkFirewallPolicyDescription'),
    newNetworkFirewallPolicyDescription,

    -- ** NetworkFirewallPolicyModifiedViolation
    NetworkFirewallPolicyModifiedViolation (NetworkFirewallPolicyModifiedViolation'),
    newNetworkFirewallPolicyModifiedViolation,

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

    -- ** ResourceTag
    ResourceTag (ResourceTag'),
    newResourceTag,

    -- ** ResourceViolation
    ResourceViolation (ResourceViolation'),
    newResourceViolation,

    -- ** Route
    Route (Route'),
    newRoute,

    -- ** SecurityGroupRemediationAction
    SecurityGroupRemediationAction (SecurityGroupRemediationAction'),
    newSecurityGroupRemediationAction,

    -- ** SecurityGroupRuleDescription
    SecurityGroupRuleDescription (SecurityGroupRuleDescription'),
    newSecurityGroupRuleDescription,

    -- ** SecurityServicePolicyData
    SecurityServicePolicyData (SecurityServicePolicyData'),
    newSecurityServicePolicyData,

    -- ** StatefulRuleGroup
    StatefulRuleGroup (StatefulRuleGroup'),
    newStatefulRuleGroup,

    -- ** StatelessRuleGroup
    StatelessRuleGroup (StatelessRuleGroup'),
    newStatelessRuleGroup,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** ViolationDetail
    ViolationDetail (ViolationDetail'),
    newViolationDetail,
  )
where

import Network.AWS.FMS.AssociateAdminAccount
import Network.AWS.FMS.DeleteAppsList
import Network.AWS.FMS.DeleteNotificationChannel
import Network.AWS.FMS.DeletePolicy
import Network.AWS.FMS.DeleteProtocolsList
import Network.AWS.FMS.DisassociateAdminAccount
import Network.AWS.FMS.GetAdminAccount
import Network.AWS.FMS.GetAppsList
import Network.AWS.FMS.GetComplianceDetail
import Network.AWS.FMS.GetNotificationChannel
import Network.AWS.FMS.GetPolicy
import Network.AWS.FMS.GetProtectionStatus
import Network.AWS.FMS.GetProtocolsList
import Network.AWS.FMS.GetViolationDetails
import Network.AWS.FMS.Lens
import Network.AWS.FMS.ListAppsLists
import Network.AWS.FMS.ListComplianceStatus
import Network.AWS.FMS.ListMemberAccounts
import Network.AWS.FMS.ListPolicies
import Network.AWS.FMS.ListProtocolsLists
import Network.AWS.FMS.ListTagsForResource
import Network.AWS.FMS.PutAppsList
import Network.AWS.FMS.PutNotificationChannel
import Network.AWS.FMS.PutPolicy
import Network.AWS.FMS.PutProtocolsList
import Network.AWS.FMS.TagResource
import Network.AWS.FMS.Types
import Network.AWS.FMS.UntagResource
import Network.AWS.FMS.Waiters

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
