-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    _InternalErrorException,
    _InvalidInputException,
    _InvalidOperationException,
    _InvalidTypeException,
    _ResourceNotFoundException,
    _LimitExceededException,

    -- * AppsListDataSummary
    AppsListDataSummary (..),
    mkAppsListDataSummary,
    aldsAppsList,
    aldsListArn,
    aldsListId,
    aldsListName,

    -- * PolicyComplianceDetail
    PolicyComplianceDetail (..),
    mkPolicyComplianceDetail,
    pcdEvaluationLimitExceeded,
    pcdExpiredAt,
    pcdIssueInfoMap,
    pcdMemberAccount,
    pcdPolicyId,
    pcdPolicyOwner,
    pcdViolators,

    -- * PaginationToken
    PaginationToken (..),

    -- * ResourceTagKey
    ResourceTagKey (..),

    -- * EvaluationResult
    EvaluationResult (..),
    mkEvaluationResult,
    erComplianceStatus,
    erEvaluationLimitExceeded,
    erViolatorCount,

    -- * ProtocolsListData
    ProtocolsListData (..),
    mkProtocolsListData,
    pldListName,
    pldProtocolsList,
    pldCreateTime,
    pldLastUpdateTime,
    pldListId,
    pldListUpdateToken,
    pldPreviousProtocolsList,

    -- * ResourceId
    ResourceId (..),

    -- * Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- * UpdateToken
    UpdateToken (..),

    -- * ResourceType
    ResourceType (..),

    -- * NetworkFirewallMissingExpectedRTViolation
    NetworkFirewallMissingExpectedRTViolation (..),
    mkNetworkFirewallMissingExpectedRTViolation,
    nfmertvAvailabilityZone,
    nfmertvCurrentRouteTable,
    nfmertvExpectedRouteTable,
    nfmertvVPC,
    nfmertvViolationTarget,

    -- * ResourceName
    ResourceName (..),

    -- * PolicyId
    PolicyId (..),

    -- * AppsListData
    AppsListData (..),
    mkAppsListData,
    aldListName,
    aldAppsList,
    aldCreateTime,
    aldLastUpdateTime,
    aldListId,
    aldListUpdateToken,
    aldPreviousAppsList,

    -- * DependentServiceName
    DependentServiceName (..),

    -- * ProtectionData
    ProtectionData (..),

    -- * NetworkFirewallMissingFirewallViolation
    NetworkFirewallMissingFirewallViolation (..),
    mkNetworkFirewallMissingFirewallViolation,
    nfmfvAvailabilityZone,
    nfmfvTargetViolationReason,
    nfmfvVPC,
    nfmfvViolationTarget,

    -- * CustomerPolicyScopeId
    CustomerPolicyScopeId (..),

    -- * Protocol
    Protocol (..),

    -- * NetworkFirewallMissingSubnetViolation
    NetworkFirewallMissingSubnetViolation (..),
    mkNetworkFirewallMissingSubnetViolation,
    nfmsvAvailabilityZone,
    nfmsvTargetViolationReason,
    nfmsvVPC,
    nfmsvViolationTarget,

    -- * SecurityGroupRuleDescription
    SecurityGroupRuleDescription (..),
    mkSecurityGroupRuleDescription,
    sgrdFromPort,
    sgrdIPV4Range,
    sgrdIPV6Range,
    sgrdPrefixListId,
    sgrdProtocol,
    sgrdToPort,

    -- * AWSAccountId
    AWSAccountId (..),

    -- * AwsEc2InstanceViolation
    AwsEc2InstanceViolation (..),
    mkAwsEc2InstanceViolation,
    aeivAwsEc2NetworkInterfaceViolations,
    aeivViolationTarget,

    -- * ComplianceViolator
    ComplianceViolator (..),
    mkComplianceViolator,
    cvResourceId,
    cvResourceType,
    cvViolationReason,

    -- * PolicyComplianceStatusType
    PolicyComplianceStatusType (..),

    -- * App
    App (..),
    mkApp,
    aAppName,
    aProtocol,
    aPort,

    -- * StatefulRuleGroup
    StatefulRuleGroup (..),
    mkStatefulRuleGroup,
    srgResourceId,
    srgRuleGroupName,

    -- * TargetViolationReason
    TargetViolationReason (..),

    -- * PolicySummary
    PolicySummary (..),
    mkPolicySummary,
    psPolicyArn,
    psPolicyId,
    psPolicyName,
    psRemediationEnabled,
    psResourceType,
    psSecurityServiceType,

    -- * ListId
    ListId (..),

    -- * ResourceArn
    ResourceArn (..),

    -- * StatelessRuleGroup
    StatelessRuleGroup (..),
    mkStatelessRuleGroup,
    sPriority,
    sResourceId,
    sRuleGroupName,

    -- * RemediationActionDescription
    RemediationActionDescription (..),

    -- * LengthBoundedString
    LengthBoundedString (..),

    -- * PolicyUpdateToken
    PolicyUpdateToken (..),

    -- * ViolationReason
    ViolationReason (..),

    -- * ReferenceRule
    ReferenceRule (..),

    -- * TagKey
    TagKey (..),

    -- * AwsVPCSecurityGroupViolation
    AwsVPCSecurityGroupViolation (..),
    mkAwsVPCSecurityGroupViolation,
    avpcsgvPartialMatches,
    avpcsgvPossibleSecurityGroupRemediationActions,
    avpcsgvViolationTarget,
    avpcsgvViolationTargetDescription,

    -- * ViolationTarget
    ViolationTarget (..),

    -- * NetworkFirewallPolicyModifiedViolation
    NetworkFirewallPolicyModifiedViolation (..),
    mkNetworkFirewallPolicyModifiedViolation,
    nfpmvCurrentPolicyDescription,
    nfpmvExpectedPolicyDescription,
    nfpmvViolationTarget,

    -- * ResourceTag
    ResourceTag (..),
    mkResourceTag,
    rtKey,
    rtValue,

    -- * ManagedServiceData
    ManagedServiceData (..),

    -- * Policy
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

    -- * SecurityGroupRemediationAction
    SecurityGroupRemediationAction (..),
    mkSecurityGroupRemediationAction,
    sgraDescription,
    sgraIsDefaultAction,
    sgraRemediationActionType,
    sgraRemediationResult,

    -- * PartialMatch
    PartialMatch (..),
    mkPartialMatch,
    pmReference,
    pmTargetViolationReasons,

    -- * ViolationDetail
    ViolationDetail (..),
    mkViolationDetail,
    vdPolicyId,
    vdMemberAccount,
    vdResourceId,
    vdResourceType,
    vdResourceViolations,
    vdResourceDescription,
    vdResourceTags,

    -- * ProtocolsListDataSummary
    ProtocolsListDataSummary (..),
    mkProtocolsListDataSummary,
    pldsListArn,
    pldsListId,
    pldsListName,
    pldsProtocolsList,

    -- * AwsEc2NetworkInterfaceViolation
    AwsEc2NetworkInterfaceViolation (..),
    mkAwsEc2NetworkInterfaceViolation,
    aenivViolatingSecurityGroups,
    aenivViolationTarget,

    -- * SecurityServiceType
    SecurityServiceType (..),

    -- * CustomerPolicyScopeIdType
    CustomerPolicyScopeIdType (..),

    -- * ResourceViolation
    ResourceViolation (..),
    mkResourceViolation,
    rvAwsEc2InstanceViolation,
    rvAwsEc2NetworkInterfaceViolation,
    rvAwsVPCSecurityGroupViolation,
    rvNetworkFirewallMissingExpectedRTViolation,
    rvNetworkFirewallMissingFirewallViolation,
    rvNetworkFirewallMissingSubnetViolation,
    rvNetworkFirewallPolicyModifiedViolation,

    -- * SecurityServicePolicyData
    SecurityServicePolicyData (..),
    mkSecurityServicePolicyData,
    sspdType,
    sspdManagedServiceData,

    -- * DetailedInfo
    DetailedInfo (..),

    -- * NetworkFirewallAction
    NetworkFirewallAction (..),

    -- * NetworkFirewallPolicyDescription
    NetworkFirewallPolicyDescription (..),
    mkNetworkFirewallPolicyDescription,
    nfpdStatefulRuleGroups,
    nfpdStatelessCustomActions,
    nfpdStatelessDefaultActions,
    nfpdStatelessFragmentDefaultActions,
    nfpdStatelessRuleGroups,

    -- * PolicyComplianceStatus
    PolicyComplianceStatus (..),
    mkPolicyComplianceStatus,
    pcsEvaluationResults,
    pcsIssueInfoMap,
    pcsLastUpdated,
    pcsMemberAccount,
    pcsPolicyId,
    pcsPolicyName,
    pcsPolicyOwner,

    -- * RemediationActionType
    RemediationActionType (..),

    -- * PreviousListVersion
    PreviousListVersion (..),

    -- * AccountRoleStatus
    AccountRoleStatus (..),

    -- * ListArn
    ListArn (..),

    -- * ListName
    ListName (..),

    -- * MemberAccount
    MemberAccount (..),

    -- * PolicyOwner
    PolicyOwner (..),

    -- * ListUpdateToken
    ListUpdateToken (..),

    -- * PolicyArn
    PolicyArn (..),

    -- * Key
    Key (..),

    -- * Value
    Value (..),

    -- * ProtocolsListArn
    ProtocolsListArn (..),

    -- * AvailabilityZone
    AvailabilityZone (..),

    -- * SnsRoleName
    SnsRoleName (..),

    -- * SnsTopicArn
    SnsTopicArn (..),

    -- * AdminAccount
    AdminAccount (..),

    -- * IPV4Range
    IPV4Range (..),

    -- * IPV6Range
    IPV6Range (..),

    -- * RuleGroupName
    RuleGroupName (..),
  )
where

import Network.AWS.FMS.Types.AWSAccountId
import Network.AWS.FMS.Types.AccountRoleStatus
import Network.AWS.FMS.Types.AdminAccount
import Network.AWS.FMS.Types.App
import Network.AWS.FMS.Types.AppsListData
import Network.AWS.FMS.Types.AppsListDataSummary
import Network.AWS.FMS.Types.AvailabilityZone
import Network.AWS.FMS.Types.AwsEc2InstanceViolation
import Network.AWS.FMS.Types.AwsEc2NetworkInterfaceViolation
import Network.AWS.FMS.Types.AwsVPCSecurityGroupViolation
import Network.AWS.FMS.Types.ComplianceViolator
import Network.AWS.FMS.Types.CustomerPolicyScopeId
import Network.AWS.FMS.Types.CustomerPolicyScopeIdType
import Network.AWS.FMS.Types.DependentServiceName
import Network.AWS.FMS.Types.DetailedInfo
import Network.AWS.FMS.Types.EvaluationResult
import Network.AWS.FMS.Types.IPV4Range
import Network.AWS.FMS.Types.IPV6Range
import Network.AWS.FMS.Types.Key
import Network.AWS.FMS.Types.LengthBoundedString
import Network.AWS.FMS.Types.ListArn
import Network.AWS.FMS.Types.ListId
import Network.AWS.FMS.Types.ListName
import Network.AWS.FMS.Types.ListUpdateToken
import Network.AWS.FMS.Types.ManagedServiceData
import Network.AWS.FMS.Types.MemberAccount
import Network.AWS.FMS.Types.NetworkFirewallAction
import Network.AWS.FMS.Types.NetworkFirewallMissingExpectedRTViolation
import Network.AWS.FMS.Types.NetworkFirewallMissingFirewallViolation
import Network.AWS.FMS.Types.NetworkFirewallMissingSubnetViolation
import Network.AWS.FMS.Types.NetworkFirewallPolicyDescription
import Network.AWS.FMS.Types.NetworkFirewallPolicyModifiedViolation
import Network.AWS.FMS.Types.PaginationToken
import Network.AWS.FMS.Types.PartialMatch
import Network.AWS.FMS.Types.Policy
import Network.AWS.FMS.Types.PolicyArn
import Network.AWS.FMS.Types.PolicyComplianceDetail
import Network.AWS.FMS.Types.PolicyComplianceStatus
import Network.AWS.FMS.Types.PolicyComplianceStatusType
import Network.AWS.FMS.Types.PolicyId
import Network.AWS.FMS.Types.PolicyOwner
import Network.AWS.FMS.Types.PolicySummary
import Network.AWS.FMS.Types.PolicyUpdateToken
import Network.AWS.FMS.Types.PreviousListVersion
import Network.AWS.FMS.Types.ProtectionData
import Network.AWS.FMS.Types.Protocol
import Network.AWS.FMS.Types.ProtocolsListArn
import Network.AWS.FMS.Types.ProtocolsListData
import Network.AWS.FMS.Types.ProtocolsListDataSummary
import Network.AWS.FMS.Types.ReferenceRule
import Network.AWS.FMS.Types.RemediationActionDescription
import Network.AWS.FMS.Types.RemediationActionType
import Network.AWS.FMS.Types.ResourceArn
import Network.AWS.FMS.Types.ResourceId
import Network.AWS.FMS.Types.ResourceName
import Network.AWS.FMS.Types.ResourceTag
import Network.AWS.FMS.Types.ResourceTagKey
import Network.AWS.FMS.Types.ResourceType
import Network.AWS.FMS.Types.ResourceViolation
import Network.AWS.FMS.Types.RuleGroupName
import Network.AWS.FMS.Types.SecurityGroupRemediationAction
import Network.AWS.FMS.Types.SecurityGroupRuleDescription
import Network.AWS.FMS.Types.SecurityServicePolicyData
import Network.AWS.FMS.Types.SecurityServiceType
import Network.AWS.FMS.Types.SnsRoleName
import Network.AWS.FMS.Types.SnsTopicArn
import Network.AWS.FMS.Types.StatefulRuleGroup
import Network.AWS.FMS.Types.StatelessRuleGroup
import Network.AWS.FMS.Types.Tag
import Network.AWS.FMS.Types.TagKey
import Network.AWS.FMS.Types.TargetViolationReason
import Network.AWS.FMS.Types.UpdateToken
import Network.AWS.FMS.Types.Value
import Network.AWS.FMS.Types.ViolationDetail
import Network.AWS.FMS.Types.ViolationReason
import Network.AWS.FMS.Types.ViolationTarget
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2018-01-01@ of the Amazon Firewall Management Service SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "FMS",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "fms",
      Core._svcVersion = "2018-01-01",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "FMS",
      Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
      | Lens.has
          (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling_exception"
      | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e =
        Core.Just "throttling"
      | Lens.has
          ( Core.hasCode "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
      | Core.otherwise = Core.Nothing

-- | The operation failed because of a system problem, even though the request was valid. Retry your request.
_InternalErrorException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalErrorException =
  Core._MatchServiceError mkServiceConfig "InternalErrorException"
{-# DEPRECATED _InternalErrorException "Use generic-lens or generic-optics instead." #-}

-- | The parameters of the request were invalid.
_InvalidInputException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidInputException =
  Core._MatchServiceError mkServiceConfig "InvalidInputException"
{-# DEPRECATED _InvalidInputException "Use generic-lens or generic-optics instead." #-}

-- | The operation failed because there was nothing to do or the operation wasn't possible. For example, you might have submitted an @AssociateAdminAccount@ request for an account ID that was already set as the AWS Firewall Manager administrator. Or you might have tried to access a Region that's disabled by default, and that you need to enable for the Firewall Manager administrator account and for AWS Organizations before you can access it.
_InvalidOperationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidOperationException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidOperationException"
{-# DEPRECATED _InvalidOperationException "Use generic-lens or generic-optics instead." #-}

-- | The value of the @Type@ parameter is invalid.
_InvalidTypeException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidTypeException =
  Core._MatchServiceError mkServiceConfig "InvalidTypeException"
{-# DEPRECATED _InvalidTypeException "Use generic-lens or generic-optics instead." #-}

-- | The specified resource was not found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    mkServiceConfig
    "ResourceNotFoundException"
{-# DEPRECATED _ResourceNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | The operation exceeds a resource limit, for example, the maximum number of @policy@ objects that you can create for an AWS account. For more information, see <https://docs.aws.amazon.com/waf/latest/developerguide/fms-limits.html Firewall Manager Limits> in the /AWS WAF Developer Guide/ .
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError mkServiceConfig "LimitExceededException"
{-# DEPRECATED _LimitExceededException "Use generic-lens or generic-optics instead." #-}
