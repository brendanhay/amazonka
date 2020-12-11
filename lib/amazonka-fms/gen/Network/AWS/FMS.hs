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
-- Some API actions require explicit resource permissions. For information, see the developer guide topic <https://docs.aws.amazon.com/waf/latest/developerguide/fms-api-permissions-ref.html Firewall Manager required permissions for API actions> .
module Network.AWS.FMS
  ( -- * Service configuration
    fmsService,

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
    AWSEC2InstanceViolation (..),
    mkAWSEC2InstanceViolation,
    aeivViolationTarget,
    aeivAWSEC2NetworkInterfaceViolations,

    -- ** AWSEC2NetworkInterfaceViolation
    AWSEC2NetworkInterfaceViolation (..),
    mkAWSEC2NetworkInterfaceViolation,
    aenivViolatingSecurityGroups,
    aenivViolationTarget,

    -- ** AWSVPCSecurityGroupViolation
    AWSVPCSecurityGroupViolation (..),
    mkAWSVPCSecurityGroupViolation,
    avsgvViolationTargetDescription,
    avsgvPossibleSecurityGroupRemediationActions,
    avsgvViolationTarget,
    avsgvPartialMatches,

    -- ** App
    App (..),
    mkApp,
    aAppName,
    aProtocol,
    aPort,

    -- ** AppsListData
    AppsListData (..),
    mkAppsListData,
    aldListUpdateToken,
    aldListId,
    aldLastUpdateTime,
    aldPreviousAppsList,
    aldCreateTime,
    aldListName,
    aldAppsList,

    -- ** AppsListDataSummary
    AppsListDataSummary (..),
    mkAppsListDataSummary,
    aldsListARN,
    aldsAppsList,
    aldsListId,
    aldsListName,

    -- ** ComplianceViolator
    ComplianceViolator (..),
    mkComplianceViolator,
    cvResourceId,
    cvResourceType,
    cvViolationReason,

    -- ** EvaluationResult
    EvaluationResult (..),
    mkEvaluationResult,
    erViolatorCount,
    erComplianceStatus,
    erEvaluationLimitExceeded,

    -- ** NetworkFirewallMissingExpectedRTViolation
    NetworkFirewallMissingExpectedRTViolation (..),
    mkNetworkFirewallMissingExpectedRTViolation,
    nfmertvCurrentRouteTable,
    nfmertvAvailabilityZone,
    nfmertvVPC,
    nfmertvViolationTarget,
    nfmertvExpectedRouteTable,

    -- ** NetworkFirewallMissingFirewallViolation
    NetworkFirewallMissingFirewallViolation (..),
    mkNetworkFirewallMissingFirewallViolation,
    nfmfvTargetViolationReason,
    nfmfvAvailabilityZone,
    nfmfvVPC,
    nfmfvViolationTarget,

    -- ** NetworkFirewallMissingSubnetViolation
    NetworkFirewallMissingSubnetViolation (..),
    mkNetworkFirewallMissingSubnetViolation,
    nfmsvTargetViolationReason,
    nfmsvAvailabilityZone,
    nfmsvVPC,
    nfmsvViolationTarget,

    -- ** NetworkFirewallPolicyDescription
    NetworkFirewallPolicyDescription (..),
    mkNetworkFirewallPolicyDescription,
    nfpdStatefulRuleGroups,
    nfpdStatelessRuleGroups,
    nfpdStatelessFragmentDefaultActions,
    nfpdStatelessCustomActions,
    nfpdStatelessDefaultActions,

    -- ** NetworkFirewallPolicyModifiedViolation
    NetworkFirewallPolicyModifiedViolation (..),
    mkNetworkFirewallPolicyModifiedViolation,
    nfpmvCurrentPolicyDescription,
    nfpmvViolationTarget,
    nfpmvExpectedPolicyDescription,

    -- ** PartialMatch
    PartialMatch (..),
    mkPartialMatch,
    pmTargetViolationReasons,
    pmReference,

    -- ** Policy
    Policy (..),
    mkPolicy,
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
    PolicyComplianceDetail (..),
    mkPolicyComplianceDetail,
    pcdExpiredAt,
    pcdPolicyId,
    pcdViolators,
    pcdEvaluationLimitExceeded,
    pcdIssueInfoMap,
    pcdPolicyOwner,
    pcdMemberAccount,

    -- ** PolicyComplianceStatus
    PolicyComplianceStatus (..),
    mkPolicyComplianceStatus,
    pcsEvaluationResults,
    pcsLastUpdated,
    pcsPolicyName,
    pcsPolicyId,
    pcsIssueInfoMap,
    pcsPolicyOwner,
    pcsMemberAccount,

    -- ** PolicySummary
    PolicySummary (..),
    mkPolicySummary,
    psPolicyName,
    psRemediationEnabled,
    psResourceType,
    psPolicyId,
    psPolicyARN,
    psSecurityServiceType,

    -- ** ProtocolsListData
    ProtocolsListData (..),
    mkProtocolsListData,
    pldListUpdateToken,
    pldListId,
    pldLastUpdateTime,
    pldPreviousProtocolsList,
    pldCreateTime,
    pldListName,
    pldProtocolsList,

    -- ** ProtocolsListDataSummary
    ProtocolsListDataSummary (..),
    mkProtocolsListDataSummary,
    pldsProtocolsList,
    pldsListARN,
    pldsListId,
    pldsListName,

    -- ** ResourceTag
    ResourceTag (..),
    mkResourceTag,
    rtValue,
    rtKey,

    -- ** ResourceViolation
    ResourceViolation (..),
    mkResourceViolation,
    rvNetworkFirewallMissingExpectedRTViolation,
    rvNetworkFirewallMissingFirewallViolation,
    rvNetworkFirewallMissingSubnetViolation,
    rvAWSEC2InstanceViolation,
    rvAWSVPCSecurityGroupViolation,
    rvNetworkFirewallPolicyModifiedViolation,
    rvAWSEC2NetworkInterfaceViolation,

    -- ** SecurityGroupRemediationAction
    SecurityGroupRemediationAction (..),
    mkSecurityGroupRemediationAction,
    sgraIsDefaultAction,
    sgraRemediationResult,
    sgraDescription,
    sgraRemediationActionType,

    -- ** SecurityGroupRuleDescription
    SecurityGroupRuleDescription (..),
    mkSecurityGroupRuleDescription,
    sgrdFromPort,
    sgrdProtocol,
    sgrdIPV4Range,
    sgrdPrefixListId,
    sgrdToPort,
    sgrdIPV6Range,

    -- ** SecurityServicePolicyData
    SecurityServicePolicyData (..),
    mkSecurityServicePolicyData,
    sspdManagedServiceData,
    sspdType,

    -- ** StatefulRuleGroup
    StatefulRuleGroup (..),
    mkStatefulRuleGroup,
    srgResourceId,
    srgRuleGroupName,

    -- ** StatelessRuleGroup
    StatelessRuleGroup (..),
    mkStatelessRuleGroup,
    sResourceId,
    sPriority,
    sRuleGroupName,

    -- ** Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- ** ViolationDetail
    ViolationDetail (..),
    mkViolationDetail,
    vdResourceTags,
    vdResourceDescription,
    vdPolicyId,
    vdMemberAccount,
    vdResourceId,
    vdResourceType,
    vdResourceViolations,

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.Time (..),
    Lude._Time,
    Lude.ISO8601,
    Lude.Timestamp,
    Lude.UTCTime,
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
import qualified Network.AWS.Prelude as Lude

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
