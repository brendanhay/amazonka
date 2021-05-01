{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS Firewall Manager
--
-- This is the /AWS Firewall Manager API Reference/. This guide is for
-- developers who need detailed information about the AWS Firewall Manager
-- API actions, data types, and errors. For detailed information about AWS
-- Firewall Manager features, see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/fms-chapter.html AWS Firewall Manager Developer Guide>.
--
-- Some API actions require explicit resource permissions. For information,
-- see the developer guide topic
-- <https://docs.aws.amazon.com/waf/latest/developerguide/fms-api-permissions-ref.html Firewall Manager required permissions for API actions>.
module Network.AWS.FMS
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InvalidTypeException
    _InvalidTypeException,

    -- ** InvalidOperationException
    _InvalidOperationException,

    -- ** InternalErrorException
    _InternalErrorException,

    -- ** InvalidInputException
    _InvalidInputException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DeleteNotificationChannel
    DeleteNotificationChannel (DeleteNotificationChannel'),
    newDeleteNotificationChannel,
    DeleteNotificationChannelResponse (DeleteNotificationChannelResponse'),
    newDeleteNotificationChannelResponse,

    -- ** ListPolicies (Paginated)
    ListPolicies (ListPolicies'),
    newListPolicies,
    ListPoliciesResponse (ListPoliciesResponse'),
    newListPoliciesResponse,

    -- ** DeletePolicy
    DeletePolicy (DeletePolicy'),
    newDeletePolicy,
    DeletePolicyResponse (DeletePolicyResponse'),
    newDeletePolicyResponse,

    -- ** ListAppsLists
    ListAppsLists (ListAppsLists'),
    newListAppsLists,
    ListAppsListsResponse (ListAppsListsResponse'),
    newListAppsListsResponse,

    -- ** GetProtectionStatus
    GetProtectionStatus (GetProtectionStatus'),
    newGetProtectionStatus,
    GetProtectionStatusResponse (GetProtectionStatusResponse'),
    newGetProtectionStatusResponse,

    -- ** PutAppsList
    PutAppsList (PutAppsList'),
    newPutAppsList,
    PutAppsListResponse (PutAppsListResponse'),
    newPutAppsListResponse,

    -- ** ListProtocolsLists
    ListProtocolsLists (ListProtocolsLists'),
    newListProtocolsLists,
    ListProtocolsListsResponse (ListProtocolsListsResponse'),
    newListProtocolsListsResponse,

    -- ** GetAppsList
    GetAppsList (GetAppsList'),
    newGetAppsList,
    GetAppsListResponse (GetAppsListResponse'),
    newGetAppsListResponse,

    -- ** PutProtocolsList
    PutProtocolsList (PutProtocolsList'),
    newPutProtocolsList,
    PutProtocolsListResponse (PutProtocolsListResponse'),
    newPutProtocolsListResponse,

    -- ** ListComplianceStatus (Paginated)
    ListComplianceStatus (ListComplianceStatus'),
    newListComplianceStatus,
    ListComplianceStatusResponse (ListComplianceStatusResponse'),
    newListComplianceStatusResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** GetNotificationChannel
    GetNotificationChannel (GetNotificationChannel'),
    newGetNotificationChannel,
    GetNotificationChannelResponse (GetNotificationChannelResponse'),
    newGetNotificationChannelResponse,

    -- ** AssociateAdminAccount
    AssociateAdminAccount (AssociateAdminAccount'),
    newAssociateAdminAccount,
    AssociateAdminAccountResponse (AssociateAdminAccountResponse'),
    newAssociateAdminAccountResponse,

    -- ** PutNotificationChannel
    PutNotificationChannel (PutNotificationChannel'),
    newPutNotificationChannel,
    PutNotificationChannelResponse (PutNotificationChannelResponse'),
    newPutNotificationChannelResponse,

    -- ** DisassociateAdminAccount
    DisassociateAdminAccount (DisassociateAdminAccount'),
    newDisassociateAdminAccount,
    DisassociateAdminAccountResponse (DisassociateAdminAccountResponse'),
    newDisassociateAdminAccountResponse,

    -- ** PutPolicy
    PutPolicy (PutPolicy'),
    newPutPolicy,
    PutPolicyResponse (PutPolicyResponse'),
    newPutPolicyResponse,

    -- ** DeleteAppsList
    DeleteAppsList (DeleteAppsList'),
    newDeleteAppsList,
    DeleteAppsListResponse (DeleteAppsListResponse'),
    newDeleteAppsListResponse,

    -- ** GetProtocolsList
    GetProtocolsList (GetProtocolsList'),
    newGetProtocolsList,
    GetProtocolsListResponse (GetProtocolsListResponse'),
    newGetProtocolsListResponse,

    -- ** GetPolicy
    GetPolicy (GetPolicy'),
    newGetPolicy,
    GetPolicyResponse (GetPolicyResponse'),
    newGetPolicyResponse,

    -- ** DeleteProtocolsList
    DeleteProtocolsList (DeleteProtocolsList'),
    newDeleteProtocolsList,
    DeleteProtocolsListResponse (DeleteProtocolsListResponse'),
    newDeleteProtocolsListResponse,

    -- ** GetAdminAccount
    GetAdminAccount (GetAdminAccount'),
    newGetAdminAccount,
    GetAdminAccountResponse (GetAdminAccountResponse'),
    newGetAdminAccountResponse,

    -- ** ListMemberAccounts (Paginated)
    ListMemberAccounts (ListMemberAccounts'),
    newListMemberAccounts,
    ListMemberAccountsResponse (ListMemberAccountsResponse'),
    newListMemberAccountsResponse,

    -- ** GetViolationDetails
    GetViolationDetails (GetViolationDetails'),
    newGetViolationDetails,
    GetViolationDetailsResponse (GetViolationDetailsResponse'),
    newGetViolationDetailsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** GetComplianceDetail
    GetComplianceDetail (GetComplianceDetail'),
    newGetComplianceDetail,
    GetComplianceDetailResponse (GetComplianceDetailResponse'),
    newGetComplianceDetailResponse,

    -- * Types

    -- ** AccountRoleStatus
    AccountRoleStatus (..),

    -- ** CustomerPolicyScopeIdType
    CustomerPolicyScopeIdType (..),

    -- ** DependentServiceName
    DependentServiceName (..),

    -- ** PolicyComplianceStatusType
    PolicyComplianceStatusType (..),

    -- ** RemediationActionType
    RemediationActionType (..),

    -- ** SecurityServiceType
    SecurityServiceType (..),

    -- ** ViolationReason
    ViolationReason (..),

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

    -- ** EvaluationResult
    EvaluationResult (EvaluationResult'),
    newEvaluationResult,

    -- ** NetworkFirewallMissingExpectedRTViolation
    NetworkFirewallMissingExpectedRTViolation (NetworkFirewallMissingExpectedRTViolation'),
    newNetworkFirewallMissingExpectedRTViolation,

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

    -- ** ProtocolsListData
    ProtocolsListData (ProtocolsListData'),
    newProtocolsListData,

    -- ** ProtocolsListDataSummary
    ProtocolsListDataSummary (ProtocolsListDataSummary'),
    newProtocolsListDataSummary,

    -- ** ResourceTag
    ResourceTag (ResourceTag'),
    newResourceTag,

    -- ** ResourceViolation
    ResourceViolation (ResourceViolation'),
    newResourceViolation,

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
