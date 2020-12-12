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
        AgentIssue,
        AlarmActive,
        ApplicationMissing,
        AutoScalingConfiguration,
        AutoScalingIAMRolePermissions,
        AutoscalingValidationError,
        CloudformationStackFailure,
        CodedeployResourceCannotBeFound,
        CustomerApplicationUnhealthy,
        DeploymentGroupMissing,
        EcsUpdateError,
        ElasticLoadBalancingInvalid,
        ElbInvalidInstance,
        HealthConstraints,
        HealthConstraintsInvalid,
        HookExecutionFailure,
        IAMRoleMissing,
        IAMRolePermissions,
        InternalError,
        InvalidEcsService,
        InvalidLambdaConfiguration,
        InvalidLambdaFunction,
        InvalidRevision,
        ManualStop,
        MissingBlueGreenDeploymentConfiguration,
        MissingElbInformation,
        MissingGithubToken,
        NoEC2Subscription,
        NoInstances,
        OverMaxInstances,
        ResourceLimitExceeded,
        RevisionMissing,
        Throttled,
        Timeout
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype DeployErrorCode = DeployErrorCode' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern AgentIssue :: DeployErrorCode
pattern AgentIssue = DeployErrorCode' "AGENT_ISSUE"

pattern AlarmActive :: DeployErrorCode
pattern AlarmActive = DeployErrorCode' "ALARM_ACTIVE"

pattern ApplicationMissing :: DeployErrorCode
pattern ApplicationMissing = DeployErrorCode' "APPLICATION_MISSING"

pattern AutoScalingConfiguration :: DeployErrorCode
pattern AutoScalingConfiguration = DeployErrorCode' "AUTO_SCALING_CONFIGURATION"

pattern AutoScalingIAMRolePermissions :: DeployErrorCode
pattern AutoScalingIAMRolePermissions = DeployErrorCode' "AUTO_SCALING_IAM_ROLE_PERMISSIONS"

pattern AutoscalingValidationError :: DeployErrorCode
pattern AutoscalingValidationError = DeployErrorCode' "AUTOSCALING_VALIDATION_ERROR"

pattern CloudformationStackFailure :: DeployErrorCode
pattern CloudformationStackFailure = DeployErrorCode' "CLOUDFORMATION_STACK_FAILURE"

pattern CodedeployResourceCannotBeFound :: DeployErrorCode
pattern CodedeployResourceCannotBeFound = DeployErrorCode' "CODEDEPLOY_RESOURCE_CANNOT_BE_FOUND"

pattern CustomerApplicationUnhealthy :: DeployErrorCode
pattern CustomerApplicationUnhealthy = DeployErrorCode' "CUSTOMER_APPLICATION_UNHEALTHY"

pattern DeploymentGroupMissing :: DeployErrorCode
pattern DeploymentGroupMissing = DeployErrorCode' "DEPLOYMENT_GROUP_MISSING"

pattern EcsUpdateError :: DeployErrorCode
pattern EcsUpdateError = DeployErrorCode' "ECS_UPDATE_ERROR"

pattern ElasticLoadBalancingInvalid :: DeployErrorCode
pattern ElasticLoadBalancingInvalid = DeployErrorCode' "ELASTIC_LOAD_BALANCING_INVALID"

pattern ElbInvalidInstance :: DeployErrorCode
pattern ElbInvalidInstance = DeployErrorCode' "ELB_INVALID_INSTANCE"

pattern HealthConstraints :: DeployErrorCode
pattern HealthConstraints = DeployErrorCode' "HEALTH_CONSTRAINTS"

pattern HealthConstraintsInvalid :: DeployErrorCode
pattern HealthConstraintsInvalid = DeployErrorCode' "HEALTH_CONSTRAINTS_INVALID"

pattern HookExecutionFailure :: DeployErrorCode
pattern HookExecutionFailure = DeployErrorCode' "HOOK_EXECUTION_FAILURE"

pattern IAMRoleMissing :: DeployErrorCode
pattern IAMRoleMissing = DeployErrorCode' "IAM_ROLE_MISSING"

pattern IAMRolePermissions :: DeployErrorCode
pattern IAMRolePermissions = DeployErrorCode' "IAM_ROLE_PERMISSIONS"

pattern InternalError :: DeployErrorCode
pattern InternalError = DeployErrorCode' "INTERNAL_ERROR"

pattern InvalidEcsService :: DeployErrorCode
pattern InvalidEcsService = DeployErrorCode' "INVALID_ECS_SERVICE"

pattern InvalidLambdaConfiguration :: DeployErrorCode
pattern InvalidLambdaConfiguration = DeployErrorCode' "INVALID_LAMBDA_CONFIGURATION"

pattern InvalidLambdaFunction :: DeployErrorCode
pattern InvalidLambdaFunction = DeployErrorCode' "INVALID_LAMBDA_FUNCTION"

pattern InvalidRevision :: DeployErrorCode
pattern InvalidRevision = DeployErrorCode' "INVALID_REVISION"

pattern ManualStop :: DeployErrorCode
pattern ManualStop = DeployErrorCode' "MANUAL_STOP"

pattern MissingBlueGreenDeploymentConfiguration :: DeployErrorCode
pattern MissingBlueGreenDeploymentConfiguration = DeployErrorCode' "MISSING_BLUE_GREEN_DEPLOYMENT_CONFIGURATION"

pattern MissingElbInformation :: DeployErrorCode
pattern MissingElbInformation = DeployErrorCode' "MISSING_ELB_INFORMATION"

pattern MissingGithubToken :: DeployErrorCode
pattern MissingGithubToken = DeployErrorCode' "MISSING_GITHUB_TOKEN"

pattern NoEC2Subscription :: DeployErrorCode
pattern NoEC2Subscription = DeployErrorCode' "NO_EC2_SUBSCRIPTION"

pattern NoInstances :: DeployErrorCode
pattern NoInstances = DeployErrorCode' "NO_INSTANCES"

pattern OverMaxInstances :: DeployErrorCode
pattern OverMaxInstances = DeployErrorCode' "OVER_MAX_INSTANCES"

pattern ResourceLimitExceeded :: DeployErrorCode
pattern ResourceLimitExceeded = DeployErrorCode' "RESOURCE_LIMIT_EXCEEDED"

pattern RevisionMissing :: DeployErrorCode
pattern RevisionMissing = DeployErrorCode' "REVISION_MISSING"

pattern Throttled :: DeployErrorCode
pattern Throttled = DeployErrorCode' "THROTTLED"

pattern Timeout :: DeployErrorCode
pattern Timeout = DeployErrorCode' "TIMEOUT"

{-# COMPLETE
  AgentIssue,
  AlarmActive,
  ApplicationMissing,
  AutoScalingConfiguration,
  AutoScalingIAMRolePermissions,
  AutoscalingValidationError,
  CloudformationStackFailure,
  CodedeployResourceCannotBeFound,
  CustomerApplicationUnhealthy,
  DeploymentGroupMissing,
  EcsUpdateError,
  ElasticLoadBalancingInvalid,
  ElbInvalidInstance,
  HealthConstraints,
  HealthConstraintsInvalid,
  HookExecutionFailure,
  IAMRoleMissing,
  IAMRolePermissions,
  InternalError,
  InvalidEcsService,
  InvalidLambdaConfiguration,
  InvalidLambdaFunction,
  InvalidRevision,
  ManualStop,
  MissingBlueGreenDeploymentConfiguration,
  MissingElbInformation,
  MissingGithubToken,
  NoEC2Subscription,
  NoInstances,
  OverMaxInstances,
  ResourceLimitExceeded,
  RevisionMissing,
  Throttled,
  Timeout,
  DeployErrorCode'
  #-}
