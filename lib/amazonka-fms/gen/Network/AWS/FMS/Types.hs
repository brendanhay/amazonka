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
    fmsService,

    -- * Errors

    -- * AccountRoleStatus
    AccountRoleStatus (..),

    -- * CustomerPolicyScopeIdType
    CustomerPolicyScopeIdType (..),

    -- * DependentServiceName
    DependentServiceName (..),

    -- * PolicyComplianceStatusType
    PolicyComplianceStatusType (..),

    -- * RemediationActionType
    RemediationActionType (..),

    -- * SecurityServiceType
    SecurityServiceType (..),

    -- * ViolationReason
    ViolationReason (..),

    -- * AWSEC2InstanceViolation
    AWSEC2InstanceViolation (..),
    mkAWSEC2InstanceViolation,
    aeivViolationTarget,
    aeivAWSEC2NetworkInterfaceViolations,

    -- * AWSEC2NetworkInterfaceViolation
    AWSEC2NetworkInterfaceViolation (..),
    mkAWSEC2NetworkInterfaceViolation,
    aenivViolatingSecurityGroups,
    aenivViolationTarget,

    -- * AWSVPCSecurityGroupViolation
    AWSVPCSecurityGroupViolation (..),
    mkAWSVPCSecurityGroupViolation,
    avsgvViolationTargetDescription,
    avsgvPossibleSecurityGroupRemediationActions,
    avsgvViolationTarget,
    avsgvPartialMatches,

    -- * App
    App (..),
    mkApp,
    aAppName,
    aProtocol,
    aPort,

    -- * AppsListData
    AppsListData (..),
    mkAppsListData,
    aldListUpdateToken,
    aldAppsList,
    aldListId,
    aldListName,
    aldLastUpdateTime,
    aldPreviousAppsList,
    aldCreateTime,

    -- * AppsListDataSummary
    AppsListDataSummary (..),
    mkAppsListDataSummary,
    aldsListARN,
    aldsAppsList,
    aldsListId,
    aldsListName,

    -- * ComplianceViolator
    ComplianceViolator (..),
    mkComplianceViolator,
    cvResourceId,
    cvResourceType,
    cvViolationReason,

    -- * EvaluationResult
    EvaluationResult (..),
    mkEvaluationResult,
    erViolatorCount,
    erComplianceStatus,
    erEvaluationLimitExceeded,

    -- * NetworkFirewallMissingExpectedRTViolation
    NetworkFirewallMissingExpectedRTViolation (..),
    mkNetworkFirewallMissingExpectedRTViolation,
    nfmertvCurrentRouteTable,
    nfmertvAvailabilityZone,
    nfmertvVPC,
    nfmertvViolationTarget,
    nfmertvExpectedRouteTable,

    -- * NetworkFirewallMissingFirewallViolation
    NetworkFirewallMissingFirewallViolation (..),
    mkNetworkFirewallMissingFirewallViolation,
    nfmfvTargetViolationReason,
    nfmfvAvailabilityZone,
    nfmfvVPC,
    nfmfvViolationTarget,

    -- * NetworkFirewallMissingSubnetViolation
    NetworkFirewallMissingSubnetViolation (..),
    mkNetworkFirewallMissingSubnetViolation,
    nfmsvTargetViolationReason,
    nfmsvAvailabilityZone,
    nfmsvVPC,
    nfmsvViolationTarget,

    -- * NetworkFirewallPolicyDescription
    NetworkFirewallPolicyDescription (..),
    mkNetworkFirewallPolicyDescription,
    nfpdStatefulRuleGroups,
    nfpdStatelessRuleGroups,
    nfpdStatelessFragmentDefaultActions,
    nfpdStatelessCustomActions,
    nfpdStatelessDefaultActions,

    -- * NetworkFirewallPolicyModifiedViolation
    NetworkFirewallPolicyModifiedViolation (..),
    mkNetworkFirewallPolicyModifiedViolation,
    nfpmvCurrentPolicyDescription,
    nfpmvViolationTarget,
    nfpmvExpectedPolicyDescription,

    -- * PartialMatch
    PartialMatch (..),
    mkPartialMatch,
    pmTargetViolationReasons,
    pmReference,

    -- * Policy
    Policy (..),
    mkPolicy,
    pPolicyName,
    pRemediationEnabled,
    pResourceType,
    pExcludeResourceTags,
    pPolicyId,
    pResourceTypeList,
    pResourceTags,
    pPolicyUpdateToken,
    pExcludeMap,
    pIncludeMap,
    pSecurityServicePolicyData,

    -- * PolicyComplianceDetail
    PolicyComplianceDetail (..),
    mkPolicyComplianceDetail,
    pcdExpiredAt,
    pcdPolicyId,
    pcdViolators,
    pcdEvaluationLimitExceeded,
    pcdIssueInfoMap,
    pcdPolicyOwner,
    pcdMemberAccount,

    -- * PolicyComplianceStatus
    PolicyComplianceStatus (..),
    mkPolicyComplianceStatus,
    pcsEvaluationResults,
    pcsLastUpdated,
    pcsPolicyName,
    pcsPolicyId,
    pcsIssueInfoMap,
    pcsPolicyOwner,
    pcsMemberAccount,

    -- * PolicySummary
    PolicySummary (..),
    mkPolicySummary,
    psPolicyName,
    psRemediationEnabled,
    psResourceType,
    psPolicyId,
    psPolicyARN,
    psSecurityServiceType,

    -- * ProtocolsListData
    ProtocolsListData (..),
    mkProtocolsListData,
    pldProtocolsList,
    pldListUpdateToken,
    pldListId,
    pldListName,
    pldLastUpdateTime,
    pldPreviousProtocolsList,
    pldCreateTime,

    -- * ProtocolsListDataSummary
    ProtocolsListDataSummary (..),
    mkProtocolsListDataSummary,
    pldsProtocolsList,
    pldsListARN,
    pldsListId,
    pldsListName,

    -- * ResourceTag
    ResourceTag (..),
    mkResourceTag,
    rtValue,
    rtKey,

    -- * ResourceViolation
    ResourceViolation (..),
    mkResourceViolation,
    rvNetworkFirewallMissingExpectedRTViolation,
    rvNetworkFirewallMissingFirewallViolation,
    rvNetworkFirewallMissingSubnetViolation,
    rvAWSEC2InstanceViolation,
    rvAWSVPCSecurityGroupViolation,
    rvNetworkFirewallPolicyModifiedViolation,
    rvAWSEC2NetworkInterfaceViolation,

    -- * SecurityGroupRemediationAction
    SecurityGroupRemediationAction (..),
    mkSecurityGroupRemediationAction,
    sgraIsDefaultAction,
    sgraRemediationResult,
    sgraDescription,
    sgraRemediationActionType,

    -- * SecurityGroupRuleDescription
    SecurityGroupRuleDescription (..),
    mkSecurityGroupRuleDescription,
    sgrdFromPort,
    sgrdProtocol,
    sgrdIPV4Range,
    sgrdPrefixListId,
    sgrdToPort,
    sgrdIPV6Range,

    -- * SecurityServicePolicyData
    SecurityServicePolicyData (..),
    mkSecurityServicePolicyData,
    sspdManagedServiceData,
    sspdType,

    -- * StatefulRuleGroup
    StatefulRuleGroup (..),
    mkStatefulRuleGroup,
    srgResourceId,
    srgRuleGroupName,

    -- * StatelessRuleGroup
    StatelessRuleGroup (..),
    mkStatelessRuleGroup,
    sResourceId,
    sPriority,
    sRuleGroupName,

    -- * Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- * ViolationDetail
    ViolationDetail (..),
    mkViolationDetail,
    vdResourceId,
    vdResourceType,
    vdPolicyId,
    vdResourceTags,
    vdResourceDescription,
    vdResourceViolations,
    vdMemberAccount,
  )
where

import Network.AWS.FMS.Types.AWSEC2InstanceViolation
import Network.AWS.FMS.Types.AWSEC2NetworkInterfaceViolation
import Network.AWS.FMS.Types.AWSVPCSecurityGroupViolation
import Network.AWS.FMS.Types.AccountRoleStatus
import Network.AWS.FMS.Types.App
import Network.AWS.FMS.Types.AppsListData
import Network.AWS.FMS.Types.AppsListDataSummary
import Network.AWS.FMS.Types.ComplianceViolator
import Network.AWS.FMS.Types.CustomerPolicyScopeIdType
import Network.AWS.FMS.Types.DependentServiceName
import Network.AWS.FMS.Types.EvaluationResult
import Network.AWS.FMS.Types.NetworkFirewallMissingExpectedRTViolation
import Network.AWS.FMS.Types.NetworkFirewallMissingFirewallViolation
import Network.AWS.FMS.Types.NetworkFirewallMissingSubnetViolation
import Network.AWS.FMS.Types.NetworkFirewallPolicyDescription
import Network.AWS.FMS.Types.NetworkFirewallPolicyModifiedViolation
import Network.AWS.FMS.Types.PartialMatch
import Network.AWS.FMS.Types.Policy
import Network.AWS.FMS.Types.PolicyComplianceDetail
import Network.AWS.FMS.Types.PolicyComplianceStatus
import Network.AWS.FMS.Types.PolicyComplianceStatusType
import Network.AWS.FMS.Types.PolicySummary
import Network.AWS.FMS.Types.ProtocolsListData
import Network.AWS.FMS.Types.ProtocolsListDataSummary
import Network.AWS.FMS.Types.RemediationActionType
import Network.AWS.FMS.Types.ResourceTag
import Network.AWS.FMS.Types.ResourceViolation
import Network.AWS.FMS.Types.SecurityGroupRemediationAction
import Network.AWS.FMS.Types.SecurityGroupRuleDescription
import Network.AWS.FMS.Types.SecurityServicePolicyData
import Network.AWS.FMS.Types.SecurityServiceType
import Network.AWS.FMS.Types.StatefulRuleGroup
import Network.AWS.FMS.Types.StatelessRuleGroup
import Network.AWS.FMS.Types.Tag
import Network.AWS.FMS.Types.ViolationDetail
import Network.AWS.FMS.Types.ViolationReason
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2018-01-01@ of the Amazon Firewall Management Service SDK configuration.
fmsService :: Lude.Service
fmsService =
  Lude.Service
    { Lude._svcAbbrev = "FMS",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "fms",
      Lude._svcVersion = "2018-01-01",
      Lude._svcEndpoint = Lude.defaultEndpoint fmsService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "FMS",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
