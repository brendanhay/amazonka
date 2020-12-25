{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.DeployErrorCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.DeployErrorCode
  ( DeployErrorCode
      ( DeployErrorCode',
        DeployErrorCodeAgentIssue,
        DeployErrorCodeAlarmActive,
        DeployErrorCodeApplicationMissing,
        DeployErrorCodeAutoscalingValidationError,
        DeployErrorCodeAutoScalingConfiguration,
        DeployErrorCodeAutoScalingIamRolePermissions,
        DeployErrorCodeCodedeployResourceCannotBeFound,
        DeployErrorCodeCustomerApplicationUnhealthy,
        DeployErrorCodeDeploymentGroupMissing,
        DeployErrorCodeEcsUpdateError,
        DeployErrorCodeElasticLoadBalancingInvalid,
        DeployErrorCodeElbInvalidInstance,
        DeployErrorCodeHealthConstraints,
        DeployErrorCodeHealthConstraintsInvalid,
        DeployErrorCodeHookExecutionFailure,
        DeployErrorCodeIamRoleMissing,
        DeployErrorCodeIamRolePermissions,
        DeployErrorCodeInternalError,
        DeployErrorCodeInvalidEcsService,
        DeployErrorCodeInvalidLambdaConfiguration,
        DeployErrorCodeInvalidLambdaFunction,
        DeployErrorCodeInvalidRevision,
        DeployErrorCodeManualStop,
        DeployErrorCodeMissingBlueGreenDeploymentConfiguration,
        DeployErrorCodeMissingElbInformation,
        DeployErrorCodeMissingGithubToken,
        DeployErrorCodeNoEC2Subscription,
        DeployErrorCodeNoInstances,
        DeployErrorCodeOverMaxInstances,
        DeployErrorCodeResourceLimitExceeded,
        DeployErrorCodeRevisionMissing,
        DeployErrorCodeThrottled,
        DeployErrorCodeTimeout,
        DeployErrorCodeCloudformationStackFailure,
        fromDeployErrorCode
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype DeployErrorCode = DeployErrorCode'
  { fromDeployErrorCode ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern DeployErrorCodeAgentIssue :: DeployErrorCode
pattern DeployErrorCodeAgentIssue = DeployErrorCode' "AGENT_ISSUE"

pattern DeployErrorCodeAlarmActive :: DeployErrorCode
pattern DeployErrorCodeAlarmActive = DeployErrorCode' "ALARM_ACTIVE"

pattern DeployErrorCodeApplicationMissing :: DeployErrorCode
pattern DeployErrorCodeApplicationMissing = DeployErrorCode' "APPLICATION_MISSING"

pattern DeployErrorCodeAutoscalingValidationError :: DeployErrorCode
pattern DeployErrorCodeAutoscalingValidationError = DeployErrorCode' "AUTOSCALING_VALIDATION_ERROR"

pattern DeployErrorCodeAutoScalingConfiguration :: DeployErrorCode
pattern DeployErrorCodeAutoScalingConfiguration = DeployErrorCode' "AUTO_SCALING_CONFIGURATION"

pattern DeployErrorCodeAutoScalingIamRolePermissions :: DeployErrorCode
pattern DeployErrorCodeAutoScalingIamRolePermissions = DeployErrorCode' "AUTO_SCALING_IAM_ROLE_PERMISSIONS"

pattern DeployErrorCodeCodedeployResourceCannotBeFound :: DeployErrorCode
pattern DeployErrorCodeCodedeployResourceCannotBeFound = DeployErrorCode' "CODEDEPLOY_RESOURCE_CANNOT_BE_FOUND"

pattern DeployErrorCodeCustomerApplicationUnhealthy :: DeployErrorCode
pattern DeployErrorCodeCustomerApplicationUnhealthy = DeployErrorCode' "CUSTOMER_APPLICATION_UNHEALTHY"

pattern DeployErrorCodeDeploymentGroupMissing :: DeployErrorCode
pattern DeployErrorCodeDeploymentGroupMissing = DeployErrorCode' "DEPLOYMENT_GROUP_MISSING"

pattern DeployErrorCodeEcsUpdateError :: DeployErrorCode
pattern DeployErrorCodeEcsUpdateError = DeployErrorCode' "ECS_UPDATE_ERROR"

pattern DeployErrorCodeElasticLoadBalancingInvalid :: DeployErrorCode
pattern DeployErrorCodeElasticLoadBalancingInvalid = DeployErrorCode' "ELASTIC_LOAD_BALANCING_INVALID"

pattern DeployErrorCodeElbInvalidInstance :: DeployErrorCode
pattern DeployErrorCodeElbInvalidInstance = DeployErrorCode' "ELB_INVALID_INSTANCE"

pattern DeployErrorCodeHealthConstraints :: DeployErrorCode
pattern DeployErrorCodeHealthConstraints = DeployErrorCode' "HEALTH_CONSTRAINTS"

pattern DeployErrorCodeHealthConstraintsInvalid :: DeployErrorCode
pattern DeployErrorCodeHealthConstraintsInvalid = DeployErrorCode' "HEALTH_CONSTRAINTS_INVALID"

pattern DeployErrorCodeHookExecutionFailure :: DeployErrorCode
pattern DeployErrorCodeHookExecutionFailure = DeployErrorCode' "HOOK_EXECUTION_FAILURE"

pattern DeployErrorCodeIamRoleMissing :: DeployErrorCode
pattern DeployErrorCodeIamRoleMissing = DeployErrorCode' "IAM_ROLE_MISSING"

pattern DeployErrorCodeIamRolePermissions :: DeployErrorCode
pattern DeployErrorCodeIamRolePermissions = DeployErrorCode' "IAM_ROLE_PERMISSIONS"

pattern DeployErrorCodeInternalError :: DeployErrorCode
pattern DeployErrorCodeInternalError = DeployErrorCode' "INTERNAL_ERROR"

pattern DeployErrorCodeInvalidEcsService :: DeployErrorCode
pattern DeployErrorCodeInvalidEcsService = DeployErrorCode' "INVALID_ECS_SERVICE"

pattern DeployErrorCodeInvalidLambdaConfiguration :: DeployErrorCode
pattern DeployErrorCodeInvalidLambdaConfiguration = DeployErrorCode' "INVALID_LAMBDA_CONFIGURATION"

pattern DeployErrorCodeInvalidLambdaFunction :: DeployErrorCode
pattern DeployErrorCodeInvalidLambdaFunction = DeployErrorCode' "INVALID_LAMBDA_FUNCTION"

pattern DeployErrorCodeInvalidRevision :: DeployErrorCode
pattern DeployErrorCodeInvalidRevision = DeployErrorCode' "INVALID_REVISION"

pattern DeployErrorCodeManualStop :: DeployErrorCode
pattern DeployErrorCodeManualStop = DeployErrorCode' "MANUAL_STOP"

pattern DeployErrorCodeMissingBlueGreenDeploymentConfiguration :: DeployErrorCode
pattern DeployErrorCodeMissingBlueGreenDeploymentConfiguration = DeployErrorCode' "MISSING_BLUE_GREEN_DEPLOYMENT_CONFIGURATION"

pattern DeployErrorCodeMissingElbInformation :: DeployErrorCode
pattern DeployErrorCodeMissingElbInformation = DeployErrorCode' "MISSING_ELB_INFORMATION"

pattern DeployErrorCodeMissingGithubToken :: DeployErrorCode
pattern DeployErrorCodeMissingGithubToken = DeployErrorCode' "MISSING_GITHUB_TOKEN"

pattern DeployErrorCodeNoEC2Subscription :: DeployErrorCode
pattern DeployErrorCodeNoEC2Subscription = DeployErrorCode' "NO_EC2_SUBSCRIPTION"

pattern DeployErrorCodeNoInstances :: DeployErrorCode
pattern DeployErrorCodeNoInstances = DeployErrorCode' "NO_INSTANCES"

pattern DeployErrorCodeOverMaxInstances :: DeployErrorCode
pattern DeployErrorCodeOverMaxInstances = DeployErrorCode' "OVER_MAX_INSTANCES"

pattern DeployErrorCodeResourceLimitExceeded :: DeployErrorCode
pattern DeployErrorCodeResourceLimitExceeded = DeployErrorCode' "RESOURCE_LIMIT_EXCEEDED"

pattern DeployErrorCodeRevisionMissing :: DeployErrorCode
pattern DeployErrorCodeRevisionMissing = DeployErrorCode' "REVISION_MISSING"

pattern DeployErrorCodeThrottled :: DeployErrorCode
pattern DeployErrorCodeThrottled = DeployErrorCode' "THROTTLED"

pattern DeployErrorCodeTimeout :: DeployErrorCode
pattern DeployErrorCodeTimeout = DeployErrorCode' "TIMEOUT"

pattern DeployErrorCodeCloudformationStackFailure :: DeployErrorCode
pattern DeployErrorCodeCloudformationStackFailure = DeployErrorCode' "CLOUDFORMATION_STACK_FAILURE"

{-# COMPLETE
  DeployErrorCodeAgentIssue,
  DeployErrorCodeAlarmActive,
  DeployErrorCodeApplicationMissing,
  DeployErrorCodeAutoscalingValidationError,
  DeployErrorCodeAutoScalingConfiguration,
  DeployErrorCodeAutoScalingIamRolePermissions,
  DeployErrorCodeCodedeployResourceCannotBeFound,
  DeployErrorCodeCustomerApplicationUnhealthy,
  DeployErrorCodeDeploymentGroupMissing,
  DeployErrorCodeEcsUpdateError,
  DeployErrorCodeElasticLoadBalancingInvalid,
  DeployErrorCodeElbInvalidInstance,
  DeployErrorCodeHealthConstraints,
  DeployErrorCodeHealthConstraintsInvalid,
  DeployErrorCodeHookExecutionFailure,
  DeployErrorCodeIamRoleMissing,
  DeployErrorCodeIamRolePermissions,
  DeployErrorCodeInternalError,
  DeployErrorCodeInvalidEcsService,
  DeployErrorCodeInvalidLambdaConfiguration,
  DeployErrorCodeInvalidLambdaFunction,
  DeployErrorCodeInvalidRevision,
  DeployErrorCodeManualStop,
  DeployErrorCodeMissingBlueGreenDeploymentConfiguration,
  DeployErrorCodeMissingElbInformation,
  DeployErrorCodeMissingGithubToken,
  DeployErrorCodeNoEC2Subscription,
  DeployErrorCodeNoInstances,
  DeployErrorCodeOverMaxInstances,
  DeployErrorCodeResourceLimitExceeded,
  DeployErrorCodeRevisionMissing,
  DeployErrorCodeThrottled,
  DeployErrorCodeTimeout,
  DeployErrorCodeCloudformationStackFailure,
  DeployErrorCode'
  #-}
