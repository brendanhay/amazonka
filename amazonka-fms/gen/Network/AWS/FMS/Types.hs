{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidTypeException,
    _InvalidOperationException,
    _InternalErrorException,
    _InvalidInputException,
    _LimitExceededException,
    _ResourceNotFoundException,

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

    -- * App
    App (..),
    newApp,
    app_appName,
    app_protocol,
    app_port,

    -- * AppsListData
    AppsListData (..),
    newAppsListData,
    appsListData_lastUpdateTime,
    appsListData_listId,
    appsListData_previousAppsList,
    appsListData_createTime,
    appsListData_listUpdateToken,
    appsListData_listName,
    appsListData_appsList,

    -- * AppsListDataSummary
    AppsListDataSummary (..),
    newAppsListDataSummary,
    appsListDataSummary_appsList,
    appsListDataSummary_listName,
    appsListDataSummary_listArn,
    appsListDataSummary_listId,

    -- * AwsEc2InstanceViolation
    AwsEc2InstanceViolation (..),
    newAwsEc2InstanceViolation,
    awsEc2InstanceViolation_violationTarget,
    awsEc2InstanceViolation_awsEc2NetworkInterfaceViolations,

    -- * AwsEc2NetworkInterfaceViolation
    AwsEc2NetworkInterfaceViolation (..),
    newAwsEc2NetworkInterfaceViolation,
    awsEc2NetworkInterfaceViolation_violationTarget,
    awsEc2NetworkInterfaceViolation_violatingSecurityGroups,

    -- * AwsVPCSecurityGroupViolation
    AwsVPCSecurityGroupViolation (..),
    newAwsVPCSecurityGroupViolation,
    awsVPCSecurityGroupViolation_partialMatches,
    awsVPCSecurityGroupViolation_possibleSecurityGroupRemediationActions,
    awsVPCSecurityGroupViolation_violationTarget,
    awsVPCSecurityGroupViolation_violationTargetDescription,

    -- * ComplianceViolator
    ComplianceViolator (..),
    newComplianceViolator,
    complianceViolator_resourceId,
    complianceViolator_resourceType,
    complianceViolator_violationReason,

    -- * EvaluationResult
    EvaluationResult (..),
    newEvaluationResult,
    evaluationResult_complianceStatus,
    evaluationResult_evaluationLimitExceeded,
    evaluationResult_violatorCount,

    -- * NetworkFirewallMissingExpectedRTViolation
    NetworkFirewallMissingExpectedRTViolation (..),
    newNetworkFirewallMissingExpectedRTViolation,
    networkFirewallMissingExpectedRTViolation_currentRouteTable,
    networkFirewallMissingExpectedRTViolation_violationTarget,
    networkFirewallMissingExpectedRTViolation_availabilityZone,
    networkFirewallMissingExpectedRTViolation_expectedRouteTable,
    networkFirewallMissingExpectedRTViolation_vpc,

    -- * NetworkFirewallMissingFirewallViolation
    NetworkFirewallMissingFirewallViolation (..),
    newNetworkFirewallMissingFirewallViolation,
    networkFirewallMissingFirewallViolation_targetViolationReason,
    networkFirewallMissingFirewallViolation_violationTarget,
    networkFirewallMissingFirewallViolation_availabilityZone,
    networkFirewallMissingFirewallViolation_vpc,

    -- * NetworkFirewallMissingSubnetViolation
    NetworkFirewallMissingSubnetViolation (..),
    newNetworkFirewallMissingSubnetViolation,
    networkFirewallMissingSubnetViolation_targetViolationReason,
    networkFirewallMissingSubnetViolation_violationTarget,
    networkFirewallMissingSubnetViolation_availabilityZone,
    networkFirewallMissingSubnetViolation_vpc,

    -- * NetworkFirewallPolicyDescription
    NetworkFirewallPolicyDescription (..),
    newNetworkFirewallPolicyDescription,
    networkFirewallPolicyDescription_statelessRuleGroups,
    networkFirewallPolicyDescription_statelessFragmentDefaultActions,
    networkFirewallPolicyDescription_statelessDefaultActions,
    networkFirewallPolicyDescription_statefulRuleGroups,
    networkFirewallPolicyDescription_statelessCustomActions,

    -- * NetworkFirewallPolicyModifiedViolation
    NetworkFirewallPolicyModifiedViolation (..),
    newNetworkFirewallPolicyModifiedViolation,
    networkFirewallPolicyModifiedViolation_currentPolicyDescription,
    networkFirewallPolicyModifiedViolation_violationTarget,
    networkFirewallPolicyModifiedViolation_expectedPolicyDescription,

    -- * PartialMatch
    PartialMatch (..),
    newPartialMatch,
    partialMatch_targetViolationReasons,
    partialMatch_reference,

    -- * Policy
    Policy (..),
    newPolicy,
    policy_resourceTags,
    policy_resourceTypeList,
    policy_policyUpdateToken,
    policy_includeMap,
    policy_policyId,
    policy_excludeMap,
    policy_policyName,
    policy_securityServicePolicyData,
    policy_resourceType,
    policy_excludeResourceTags,
    policy_remediationEnabled,

    -- * PolicyComplianceDetail
    PolicyComplianceDetail (..),
    newPolicyComplianceDetail,
    policyComplianceDetail_violators,
    policyComplianceDetail_policyOwner,
    policyComplianceDetail_memberAccount,
    policyComplianceDetail_evaluationLimitExceeded,
    policyComplianceDetail_issueInfoMap,
    policyComplianceDetail_policyId,
    policyComplianceDetail_expiredAt,

    -- * PolicyComplianceStatus
    PolicyComplianceStatus (..),
    newPolicyComplianceStatus,
    policyComplianceStatus_policyName,
    policyComplianceStatus_policyOwner,
    policyComplianceStatus_memberAccount,
    policyComplianceStatus_lastUpdated,
    policyComplianceStatus_evaluationResults,
    policyComplianceStatus_issueInfoMap,
    policyComplianceStatus_policyId,

    -- * PolicySummary
    PolicySummary (..),
    newPolicySummary,
    policySummary_policyName,
    policySummary_securityServiceType,
    policySummary_resourceType,
    policySummary_remediationEnabled,
    policySummary_policyId,
    policySummary_policyArn,

    -- * ProtocolsListData
    ProtocolsListData (..),
    newProtocolsListData,
    protocolsListData_lastUpdateTime,
    protocolsListData_listId,
    protocolsListData_createTime,
    protocolsListData_previousProtocolsList,
    protocolsListData_listUpdateToken,
    protocolsListData_listName,
    protocolsListData_protocolsList,

    -- * ProtocolsListDataSummary
    ProtocolsListDataSummary (..),
    newProtocolsListDataSummary,
    protocolsListDataSummary_listName,
    protocolsListDataSummary_protocolsList,
    protocolsListDataSummary_listArn,
    protocolsListDataSummary_listId,

    -- * ResourceTag
    ResourceTag (..),
    newResourceTag,
    resourceTag_value,
    resourceTag_key,

    -- * ResourceViolation
    ResourceViolation (..),
    newResourceViolation,
    resourceViolation_networkFirewallMissingSubnetViolation,
    resourceViolation_awsEc2NetworkInterfaceViolation,
    resourceViolation_networkFirewallMissingExpectedRTViolation,
    resourceViolation_awsEc2InstanceViolation,
    resourceViolation_networkFirewallMissingFirewallViolation,
    resourceViolation_networkFirewallPolicyModifiedViolation,
    resourceViolation_awsVPCSecurityGroupViolation,

    -- * SecurityGroupRemediationAction
    SecurityGroupRemediationAction (..),
    newSecurityGroupRemediationAction,
    securityGroupRemediationAction_remediationActionType,
    securityGroupRemediationAction_remediationResult,
    securityGroupRemediationAction_isDefaultAction,
    securityGroupRemediationAction_description,

    -- * SecurityGroupRuleDescription
    SecurityGroupRuleDescription (..),
    newSecurityGroupRuleDescription,
    securityGroupRuleDescription_fromPort,
    securityGroupRuleDescription_iPV4Range,
    securityGroupRuleDescription_prefixListId,
    securityGroupRuleDescription_iPV6Range,
    securityGroupRuleDescription_protocol,
    securityGroupRuleDescription_toPort,

    -- * SecurityServicePolicyData
    SecurityServicePolicyData (..),
    newSecurityServicePolicyData,
    securityServicePolicyData_managedServiceData,
    securityServicePolicyData_type,

    -- * StatefulRuleGroup
    StatefulRuleGroup (..),
    newStatefulRuleGroup,
    statefulRuleGroup_resourceId,
    statefulRuleGroup_ruleGroupName,

    -- * StatelessRuleGroup
    StatelessRuleGroup (..),
    newStatelessRuleGroup,
    statelessRuleGroup_resourceId,
    statelessRuleGroup_priority,
    statelessRuleGroup_ruleGroupName,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * ViolationDetail
    ViolationDetail (..),
    newViolationDetail,
    violationDetail_resourceDescription,
    violationDetail_resourceTags,
    violationDetail_policyId,
    violationDetail_memberAccount,
    violationDetail_resourceId,
    violationDetail_resourceType,
    violationDetail_resourceViolations,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.FMS.Types.AccountRoleStatus
import Network.AWS.FMS.Types.App
import Network.AWS.FMS.Types.AppsListData
import Network.AWS.FMS.Types.AppsListDataSummary
import Network.AWS.FMS.Types.AwsEc2InstanceViolation
import Network.AWS.FMS.Types.AwsEc2NetworkInterfaceViolation
import Network.AWS.FMS.Types.AwsVPCSecurityGroupViolation
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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2018-01-01@ of the Amazon Firewall Management Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "FMS",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "fms",
      Core._serviceSigningName = "fms",
      Core._serviceVersion = "2018-01-01",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "FMS",
      Core._serviceRetry = retry
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
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | The value of the @Type@ parameter is invalid.
_InvalidTypeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTypeException =
  Core._MatchServiceError
    defaultService
    "InvalidTypeException"

-- | The operation failed because there was nothing to do or the operation
-- wasn\'t possible. For example, you might have submitted an
-- @AssociateAdminAccount@ request for an account ID that was already set
-- as the AWS Firewall Manager administrator. Or you might have tried to
-- access a Region that\'s disabled by default, and that you need to enable
-- for the Firewall Manager administrator account and for AWS Organizations
-- before you can access it.
_InvalidOperationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidOperationException =
  Core._MatchServiceError
    defaultService
    "InvalidOperationException"

-- | The operation failed because of a system problem, even though the
-- request was valid. Retry your request.
_InternalErrorException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalErrorException =
  Core._MatchServiceError
    defaultService
    "InternalErrorException"

-- | The parameters of the request were invalid.
_InvalidInputException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidInputException =
  Core._MatchServiceError
    defaultService
    "InvalidInputException"

-- | The operation exceeds a resource limit, for example, the maximum number
-- of @policy@ objects that you can create for an AWS account. For more
-- information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/fms-limits.html Firewall Manager Limits>
-- in the /AWS WAF Developer Guide/.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The specified resource was not found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
