{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS Firewall Manager__
--
-- This is the /AWS Firewall Manager API Reference/ . This guide is for developers who need detailed information about the AWS Firewall Manager API actions, data types, and errors. For detailed information about AWS Firewall Manager features, see the <https://docs.aws.amazon.com/waf/latest/developerguide/fms-chapter.html AWS Firewall Manager Developer Guide> .
--
-- Some API actions require explicit resource permissions. For information, see the developer guide topic <https://docs.aws.amazon.com/waf/latest/developerguide/fms-api-permissions-ref.html Firewall Manager required permissions for API actions> .
module Network.AWS.FMS
  ( -- * Service Configuration
    fms,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListPolicies (Paginated)
    module Network.AWS.FMS.ListPolicies,

    -- ** GetComplianceDetail
    module Network.AWS.FMS.GetComplianceDetail,

    -- ** ListTagsForResource
    module Network.AWS.FMS.ListTagsForResource,

    -- ** GetNotificationChannel
    module Network.AWS.FMS.GetNotificationChannel,

    -- ** GetAdminAccount
    module Network.AWS.FMS.GetAdminAccount,

    -- ** ListComplianceStatus (Paginated)
    module Network.AWS.FMS.ListComplianceStatus,

    -- ** GetAppsList
    module Network.AWS.FMS.GetAppsList,

    -- ** PutPolicy
    module Network.AWS.FMS.PutPolicy,

    -- ** DeletePolicy
    module Network.AWS.FMS.DeletePolicy,

    -- ** DisassociateAdminAccount
    module Network.AWS.FMS.DisassociateAdminAccount,

    -- ** PutNotificationChannel
    module Network.AWS.FMS.PutNotificationChannel,

    -- ** DeleteNotificationChannel
    module Network.AWS.FMS.DeleteNotificationChannel,

    -- ** AssociateAdminAccount
    module Network.AWS.FMS.AssociateAdminAccount,

    -- ** GetViolationDetails
    module Network.AWS.FMS.GetViolationDetails,

    -- ** ListMemberAccounts (Paginated)
    module Network.AWS.FMS.ListMemberAccounts,

    -- ** TagResource
    module Network.AWS.FMS.TagResource,

    -- ** UntagResource
    module Network.AWS.FMS.UntagResource,

    -- ** DeleteProtocolsList
    module Network.AWS.FMS.DeleteProtocolsList,

    -- ** GetPolicy
    module Network.AWS.FMS.GetPolicy,

    -- ** ListProtocolsLists
    module Network.AWS.FMS.ListProtocolsLists,

    -- ** PutProtocolsList
    module Network.AWS.FMS.PutProtocolsList,

    -- ** PutAppsList
    module Network.AWS.FMS.PutAppsList,

    -- ** DeleteAppsList
    module Network.AWS.FMS.DeleteAppsList,

    -- ** ListAppsLists
    module Network.AWS.FMS.ListAppsLists,

    -- ** GetProtocolsList
    module Network.AWS.FMS.GetProtocolsList,

    -- ** GetProtectionStatus
    module Network.AWS.FMS.GetProtectionStatus,

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

    -- ** AWSEC2InstanceViolation
    AWSEC2InstanceViolation,
    awsEC2InstanceViolation,
    aeivViolationTarget,
    aeivAWSEC2NetworkInterfaceViolations,

    -- ** AWSEC2NetworkInterfaceViolation
    AWSEC2NetworkInterfaceViolation,
    awsEC2NetworkInterfaceViolation,
    aenivViolatingSecurityGroups,
    aenivViolationTarget,

    -- ** AWSVPCSecurityGroupViolation
    AWSVPCSecurityGroupViolation,
    awsVPCSecurityGroupViolation,
    avsgvViolationTargetDescription,
    avsgvPossibleSecurityGroupRemediationActions,
    avsgvViolationTarget,
    avsgvPartialMatches,

    -- ** App
    App,
    app,
    appAppName,
    appProtocol,
    appPort,

    -- ** AppsListData
    AppsListData,
    appsListData,
    aldListUpdateToken,
    aldListId,
    aldLastUpdateTime,
    aldPreviousAppsList,
    aldCreateTime,
    aldListName,
    aldAppsList,

    -- ** AppsListDataSummary
    AppsListDataSummary,
    appsListDataSummary,
    aldsListARN,
    aldsAppsList,
    aldsListId,
    aldsListName,

    -- ** ComplianceViolator
    ComplianceViolator,
    complianceViolator,
    cvResourceId,
    cvResourceType,
    cvViolationReason,

    -- ** EvaluationResult
    EvaluationResult,
    evaluationResult,
    erViolatorCount,
    erComplianceStatus,
    erEvaluationLimitExceeded,

    -- ** NetworkFirewallMissingExpectedRTViolation
    NetworkFirewallMissingExpectedRTViolation,
    networkFirewallMissingExpectedRTViolation,
    nfmertvCurrentRouteTable,
    nfmertvAvailabilityZone,
    nfmertvVPC,
    nfmertvViolationTarget,
    nfmertvExpectedRouteTable,

    -- ** NetworkFirewallMissingFirewallViolation
    NetworkFirewallMissingFirewallViolation,
    networkFirewallMissingFirewallViolation,
    nfmfvTargetViolationReason,
    nfmfvAvailabilityZone,
    nfmfvVPC,
    nfmfvViolationTarget,

    -- ** NetworkFirewallMissingSubnetViolation
    NetworkFirewallMissingSubnetViolation,
    networkFirewallMissingSubnetViolation,
    nfmsvTargetViolationReason,
    nfmsvAvailabilityZone,
    nfmsvVPC,
    nfmsvViolationTarget,

    -- ** NetworkFirewallPolicyDescription
    NetworkFirewallPolicyDescription,
    networkFirewallPolicyDescription,
    nfpdStatefulRuleGroups,
    nfpdStatelessRuleGroups,
    nfpdStatelessFragmentDefaultActions,
    nfpdStatelessCustomActions,
    nfpdStatelessDefaultActions,

    -- ** NetworkFirewallPolicyModifiedViolation
    NetworkFirewallPolicyModifiedViolation,
    networkFirewallPolicyModifiedViolation,
    nfpmvCurrentPolicyDescription,
    nfpmvViolationTarget,
    nfpmvExpectedPolicyDescription,

    -- ** PartialMatch
    PartialMatch,
    partialMatch,
    pmTargetViolationReasons,
    pmReference,

    -- ** Policy
    Policy,
    policy,
    pPolicyId,
    pResourceTypeList,
    pResourceTags,
    pPolicyUpdateToken,
    pExcludeMap,
    pIncludeMap,
    pPolicyName,
    pSecurityServicePolicyData,
    pResourceType,
    pExcludeResourceTags,
    pRemediationEnabled,

    -- ** PolicyComplianceDetail
    PolicyComplianceDetail,
    policyComplianceDetail,
    pcdExpiredAt,
    pcdPolicyId,
    pcdViolators,
    pcdEvaluationLimitExceeded,
    pcdIssueInfoMap,
    pcdPolicyOwner,
    pcdMemberAccount,

    -- ** PolicyComplianceStatus
    PolicyComplianceStatus,
    policyComplianceStatus,
    pcsEvaluationResults,
    pcsLastUpdated,
    pcsPolicyName,
    pcsPolicyId,
    pcsIssueInfoMap,
    pcsPolicyOwner,
    pcsMemberAccount,

    -- ** PolicySummary
    PolicySummary,
    policySummary,
    psPolicyName,
    psRemediationEnabled,
    psResourceType,
    psPolicyId,
    psPolicyARN,
    psSecurityServiceType,

    -- ** ProtocolsListData
    ProtocolsListData,
    protocolsListData,
    pldListUpdateToken,
    pldListId,
    pldLastUpdateTime,
    pldPreviousProtocolsList,
    pldCreateTime,
    pldListName,
    pldProtocolsList,

    -- ** ProtocolsListDataSummary
    ProtocolsListDataSummary,
    protocolsListDataSummary,
    pldsProtocolsList,
    pldsListARN,
    pldsListId,
    pldsListName,

    -- ** ResourceTag
    ResourceTag,
    resourceTag,
    rtValue,
    rtKey,

    -- ** ResourceViolation
    ResourceViolation,
    resourceViolation,
    rvNetworkFirewallMissingExpectedRTViolation,
    rvNetworkFirewallMissingFirewallViolation,
    rvNetworkFirewallMissingSubnetViolation,
    rvAWSEC2InstanceViolation,
    rvAWSVPCSecurityGroupViolation,
    rvNetworkFirewallPolicyModifiedViolation,
    rvAWSEC2NetworkInterfaceViolation,

    -- ** SecurityGroupRemediationAction
    SecurityGroupRemediationAction,
    securityGroupRemediationAction,
    sgraIsDefaultAction,
    sgraRemediationResult,
    sgraDescription,
    sgraRemediationActionType,

    -- ** SecurityGroupRuleDescription
    SecurityGroupRuleDescription,
    securityGroupRuleDescription,
    sgrdFromPort,
    sgrdProtocol,
    sgrdIPV4Range,
    sgrdPrefixListId,
    sgrdToPort,
    sgrdIPV6Range,

    -- ** SecurityServicePolicyData
    SecurityServicePolicyData,
    securityServicePolicyData,
    sspdManagedServiceData,
    sspdType,

    -- ** StatefulRuleGroup
    StatefulRuleGroup,
    statefulRuleGroup,
    srgResourceId,
    srgRuleGroupName,

    -- ** StatelessRuleGroup
    StatelessRuleGroup,
    statelessRuleGroup,
    sResourceId,
    sPriority,
    sRuleGroupName,

    -- ** Tag
    Tag,
    tag,
    tagKey,
    tagValue,

    -- ** ViolationDetail
    ViolationDetail,
    violationDetail,
    vdResourceTags,
    vdResourceDescription,
    vdPolicyId,
    vdMemberAccount,
    vdResourceId,
    vdResourceType,
    vdResourceViolations,
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
