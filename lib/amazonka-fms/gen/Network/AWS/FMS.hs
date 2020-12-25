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
    mkServiceConfig,

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

    -- ** AppsListDataSummary
    AppsListDataSummary (..),
    mkAppsListDataSummary,
    aldsAppsList,
    aldsListArn,
    aldsListId,
    aldsListName,

    -- ** PolicyComplianceDetail
    PolicyComplianceDetail (..),
    mkPolicyComplianceDetail,
    pcdEvaluationLimitExceeded,
    pcdExpiredAt,
    pcdIssueInfoMap,
    pcdMemberAccount,
    pcdPolicyId,
    pcdPolicyOwner,
    pcdViolators,

    -- ** PaginationToken
    PaginationToken (..),

    -- ** ResourceTagKey
    ResourceTagKey (..),

    -- ** EvaluationResult
    EvaluationResult (..),
    mkEvaluationResult,
    erComplianceStatus,
    erEvaluationLimitExceeded,
    erViolatorCount,

    -- ** ProtocolsListData
    ProtocolsListData (..),
    mkProtocolsListData,
    pldListName,
    pldProtocolsList,
    pldCreateTime,
    pldLastUpdateTime,
    pldListId,
    pldListUpdateToken,
    pldPreviousProtocolsList,

    -- ** ResourceId
    ResourceId (..),

    -- ** Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- ** UpdateToken
    UpdateToken (..),

    -- ** ResourceType
    ResourceType (..),

    -- ** NetworkFirewallMissingExpectedRTViolation
    NetworkFirewallMissingExpectedRTViolation (..),
    mkNetworkFirewallMissingExpectedRTViolation,
    nfmertvAvailabilityZone,
    nfmertvCurrentRouteTable,
    nfmertvExpectedRouteTable,
    nfmertvVPC,
    nfmertvViolationTarget,

    -- ** ResourceName
    ResourceName (..),

    -- ** PolicyId
    PolicyId (..),

    -- ** AppsListData
    AppsListData (..),
    mkAppsListData,
    aldListName,
    aldAppsList,
    aldCreateTime,
    aldLastUpdateTime,
    aldListId,
    aldListUpdateToken,
    aldPreviousAppsList,

    -- ** DependentServiceName
    DependentServiceName (..),

    -- ** ProtectionData
    ProtectionData (..),

    -- ** NetworkFirewallMissingFirewallViolation
    NetworkFirewallMissingFirewallViolation (..),
    mkNetworkFirewallMissingFirewallViolation,
    nfmfvAvailabilityZone,
    nfmfvTargetViolationReason,
    nfmfvVPC,
    nfmfvViolationTarget,

    -- ** CustomerPolicyScopeId
    CustomerPolicyScopeId (..),

    -- ** Protocol
    Protocol (..),

    -- ** NetworkFirewallMissingSubnetViolation
    NetworkFirewallMissingSubnetViolation (..),
    mkNetworkFirewallMissingSubnetViolation,
    nfmsvAvailabilityZone,
    nfmsvTargetViolationReason,
    nfmsvVPC,
    nfmsvViolationTarget,

    -- ** SecurityGroupRuleDescription
    SecurityGroupRuleDescription (..),
    mkSecurityGroupRuleDescription,
    sgrdFromPort,
    sgrdIPV4Range,
    sgrdIPV6Range,
    sgrdPrefixListId,
    sgrdProtocol,
    sgrdToPort,

    -- ** AWSAccountId
    AWSAccountId (..),

    -- ** AwsEc2InstanceViolation
    AwsEc2InstanceViolation (..),
    mkAwsEc2InstanceViolation,
    aeivAwsEc2NetworkInterfaceViolations,
    aeivViolationTarget,

    -- ** ComplianceViolator
    ComplianceViolator (..),
    mkComplianceViolator,
    cvResourceId,
    cvResourceType,
    cvViolationReason,

    -- ** PolicyComplianceStatusType
    PolicyComplianceStatusType (..),

    -- ** App
    App (..),
    mkApp,
    aAppName,
    aProtocol,
    aPort,

    -- ** StatefulRuleGroup
    StatefulRuleGroup (..),
    mkStatefulRuleGroup,
    srgResourceId,
    srgRuleGroupName,

    -- ** TargetViolationReason
    TargetViolationReason (..),

    -- ** PolicySummary
    PolicySummary (..),
    mkPolicySummary,
    psPolicyArn,
    psPolicyId,
    psPolicyName,
    psRemediationEnabled,
    psResourceType,
    psSecurityServiceType,

    -- ** ListId
    ListId (..),

    -- ** ResourceArn
    ResourceArn (..),

    -- ** StatelessRuleGroup
    StatelessRuleGroup (..),
    mkStatelessRuleGroup,
    sPriority,
    sResourceId,
    sRuleGroupName,

    -- ** RemediationActionDescription
    RemediationActionDescription (..),

    -- ** LengthBoundedString
    LengthBoundedString (..),

    -- ** PolicyUpdateToken
    PolicyUpdateToken (..),

    -- ** ViolationReason
    ViolationReason (..),

    -- ** ReferenceRule
    ReferenceRule (..),

    -- ** TagKey
    TagKey (..),

    -- ** AwsVPCSecurityGroupViolation
    AwsVPCSecurityGroupViolation (..),
    mkAwsVPCSecurityGroupViolation,
    avpcsgvPartialMatches,
    avpcsgvPossibleSecurityGroupRemediationActions,
    avpcsgvViolationTarget,
    avpcsgvViolationTargetDescription,

    -- ** ViolationTarget
    ViolationTarget (..),

    -- ** NetworkFirewallPolicyModifiedViolation
    NetworkFirewallPolicyModifiedViolation (..),
    mkNetworkFirewallPolicyModifiedViolation,
    nfpmvCurrentPolicyDescription,
    nfpmvExpectedPolicyDescription,
    nfpmvViolationTarget,

    -- ** ResourceTag
    ResourceTag (..),
    mkResourceTag,
    rtKey,
    rtValue,

    -- ** ManagedServiceData
    ManagedServiceData (..),

    -- ** Policy
    Policy (..),
    mkPolicy,
    pPolicyName,
    pSecurityServicePolicyData,
    pResourceType,
    pExcludeResourceTags,
    pRemediationEnabled,
    pExcludeMap,
    pIncludeMap,
    pPolicyId,
    pPolicyUpdateToken,
    pResourceTags,
    pResourceTypeList,

    -- ** SecurityGroupRemediationAction
    SecurityGroupRemediationAction (..),
    mkSecurityGroupRemediationAction,
    sgraDescription,
    sgraIsDefaultAction,
    sgraRemediationActionType,
    sgraRemediationResult,

    -- ** PartialMatch
    PartialMatch (..),
    mkPartialMatch,
    pmReference,
    pmTargetViolationReasons,

    -- ** ViolationDetail
    ViolationDetail (..),
    mkViolationDetail,
    vdPolicyId,
    vdMemberAccount,
    vdResourceId,
    vdResourceType,
    vdResourceViolations,
    vdResourceDescription,
    vdResourceTags,

    -- ** ProtocolsListDataSummary
    ProtocolsListDataSummary (..),
    mkProtocolsListDataSummary,
    pldsListArn,
    pldsListId,
    pldsListName,
    pldsProtocolsList,

    -- ** AwsEc2NetworkInterfaceViolation
    AwsEc2NetworkInterfaceViolation (..),
    mkAwsEc2NetworkInterfaceViolation,
    aenivViolatingSecurityGroups,
    aenivViolationTarget,

    -- ** SecurityServiceType
    SecurityServiceType (..),

    -- ** CustomerPolicyScopeIdType
    CustomerPolicyScopeIdType (..),

    -- ** ResourceViolation
    ResourceViolation (..),
    mkResourceViolation,
    rvAwsEc2InstanceViolation,
    rvAwsEc2NetworkInterfaceViolation,
    rvAwsVPCSecurityGroupViolation,
    rvNetworkFirewallMissingExpectedRTViolation,
    rvNetworkFirewallMissingFirewallViolation,
    rvNetworkFirewallMissingSubnetViolation,
    rvNetworkFirewallPolicyModifiedViolation,

    -- ** SecurityServicePolicyData
    SecurityServicePolicyData (..),
    mkSecurityServicePolicyData,
    sspdType,
    sspdManagedServiceData,

    -- ** DetailedInfo
    DetailedInfo (..),

    -- ** NetworkFirewallAction
    NetworkFirewallAction (..),

    -- ** NetworkFirewallPolicyDescription
    NetworkFirewallPolicyDescription (..),
    mkNetworkFirewallPolicyDescription,
    nfpdStatefulRuleGroups,
    nfpdStatelessCustomActions,
    nfpdStatelessDefaultActions,
    nfpdStatelessFragmentDefaultActions,
    nfpdStatelessRuleGroups,

    -- ** PolicyComplianceStatus
    PolicyComplianceStatus (..),
    mkPolicyComplianceStatus,
    pcsEvaluationResults,
    pcsIssueInfoMap,
    pcsLastUpdated,
    pcsMemberAccount,
    pcsPolicyId,
    pcsPolicyName,
    pcsPolicyOwner,

    -- ** RemediationActionType
    RemediationActionType (..),

    -- ** PreviousListVersion
    PreviousListVersion (..),

    -- ** AccountRoleStatus
    AccountRoleStatus (..),

    -- ** ListArn
    ListArn (..),

    -- ** ListName
    ListName (..),

    -- ** MemberAccount
    MemberAccount (..),

    -- ** PolicyOwner
    PolicyOwner (..),

    -- ** ListUpdateToken
    ListUpdateToken (..),

    -- ** PolicyArn
    PolicyArn (..),

    -- ** Key
    Key (..),

    -- ** Value
    Value (..),

    -- ** ProtocolsListArn
    ProtocolsListArn (..),

    -- ** AvailabilityZone
    AvailabilityZone (..),

    -- ** SnsRoleName
    SnsRoleName (..),

    -- ** SnsTopicArn
    SnsTopicArn (..),

    -- ** AdminAccount
    AdminAccount (..),

    -- ** IPV4Range
    IPV4Range (..),

    -- ** IPV6Range
    IPV6Range (..),

    -- ** RuleGroupName
    RuleGroupName (..),

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.UTCTime,
    Lude.NominalDiffTime,
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
