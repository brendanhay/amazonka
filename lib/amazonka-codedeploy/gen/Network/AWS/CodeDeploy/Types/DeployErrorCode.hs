{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.DeployErrorCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.DeployErrorCode where

import Network.AWS.Prelude

data DeployErrorCode
  = AgentIssue
  | AlarmActive
  | ApplicationMissing
  | AutoScalingConfiguration
  | AutoScalingIAMRolePermissions
  | AutoscalingValidationError
  | CloudformationStackFailure
  | CodedeployResourceCannotBeFound
  | CustomerApplicationUnhealthy
  | DeploymentGroupMissing
  | EcsUpdateError
  | ElasticLoadBalancingInvalid
  | ElbInvalidInstance
  | HealthConstraints
  | HealthConstraintsInvalid
  | HookExecutionFailure
  | IAMRoleMissing
  | IAMRolePermissions
  | InternalError
  | InvalidEcsService
  | InvalidLambdaConfiguration
  | InvalidLambdaFunction
  | InvalidRevision
  | ManualStop
  | MissingBlueGreenDeploymentConfiguration
  | MissingElbInformation
  | MissingGithubToken
  | NoEC2Subscription
  | NoInstances
  | OverMaxInstances
  | ResourceLimitExceeded
  | RevisionMissing
  | Throttled
  | Timeout
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText DeployErrorCode where
  parser =
    takeLowerText >>= \case
      "agent_issue" -> pure AgentIssue
      "alarm_active" -> pure AlarmActive
      "application_missing" -> pure ApplicationMissing
      "auto_scaling_configuration" -> pure AutoScalingConfiguration
      "auto_scaling_iam_role_permissions" -> pure AutoScalingIAMRolePermissions
      "autoscaling_validation_error" -> pure AutoscalingValidationError
      "cloudformation_stack_failure" -> pure CloudformationStackFailure
      "codedeploy_resource_cannot_be_found" -> pure CodedeployResourceCannotBeFound
      "customer_application_unhealthy" -> pure CustomerApplicationUnhealthy
      "deployment_group_missing" -> pure DeploymentGroupMissing
      "ecs_update_error" -> pure EcsUpdateError
      "elastic_load_balancing_invalid" -> pure ElasticLoadBalancingInvalid
      "elb_invalid_instance" -> pure ElbInvalidInstance
      "health_constraints" -> pure HealthConstraints
      "health_constraints_invalid" -> pure HealthConstraintsInvalid
      "hook_execution_failure" -> pure HookExecutionFailure
      "iam_role_missing" -> pure IAMRoleMissing
      "iam_role_permissions" -> pure IAMRolePermissions
      "internal_error" -> pure InternalError
      "invalid_ecs_service" -> pure InvalidEcsService
      "invalid_lambda_configuration" -> pure InvalidLambdaConfiguration
      "invalid_lambda_function" -> pure InvalidLambdaFunction
      "invalid_revision" -> pure InvalidRevision
      "manual_stop" -> pure ManualStop
      "missing_blue_green_deployment_configuration" -> pure MissingBlueGreenDeploymentConfiguration
      "missing_elb_information" -> pure MissingElbInformation
      "missing_github_token" -> pure MissingGithubToken
      "no_ec2_subscription" -> pure NoEC2Subscription
      "no_instances" -> pure NoInstances
      "over_max_instances" -> pure OverMaxInstances
      "resource_limit_exceeded" -> pure ResourceLimitExceeded
      "revision_missing" -> pure RevisionMissing
      "throttled" -> pure Throttled
      "timeout" -> pure Timeout
      e ->
        fromTextError $
          "Failure parsing DeployErrorCode from value: '" <> e
            <> "'. Accepted values: agent_issue, alarm_active, application_missing, auto_scaling_configuration, auto_scaling_iam_role_permissions, autoscaling_validation_error, cloudformation_stack_failure, codedeploy_resource_cannot_be_found, customer_application_unhealthy, deployment_group_missing, ecs_update_error, elastic_load_balancing_invalid, elb_invalid_instance, health_constraints, health_constraints_invalid, hook_execution_failure, iam_role_missing, iam_role_permissions, internal_error, invalid_ecs_service, invalid_lambda_configuration, invalid_lambda_function, invalid_revision, manual_stop, missing_blue_green_deployment_configuration, missing_elb_information, missing_github_token, no_ec2_subscription, no_instances, over_max_instances, resource_limit_exceeded, revision_missing, throttled, timeout"

instance ToText DeployErrorCode where
  toText = \case
    AgentIssue -> "AGENT_ISSUE"
    AlarmActive -> "ALARM_ACTIVE"
    ApplicationMissing -> "APPLICATION_MISSING"
    AutoScalingConfiguration -> "AUTO_SCALING_CONFIGURATION"
    AutoScalingIAMRolePermissions -> "AUTO_SCALING_IAM_ROLE_PERMISSIONS"
    AutoscalingValidationError -> "AUTOSCALING_VALIDATION_ERROR"
    CloudformationStackFailure -> "CLOUDFORMATION_STACK_FAILURE"
    CodedeployResourceCannotBeFound -> "CODEDEPLOY_RESOURCE_CANNOT_BE_FOUND"
    CustomerApplicationUnhealthy -> "CUSTOMER_APPLICATION_UNHEALTHY"
    DeploymentGroupMissing -> "DEPLOYMENT_GROUP_MISSING"
    EcsUpdateError -> "ECS_UPDATE_ERROR"
    ElasticLoadBalancingInvalid -> "ELASTIC_LOAD_BALANCING_INVALID"
    ElbInvalidInstance -> "ELB_INVALID_INSTANCE"
    HealthConstraints -> "HEALTH_CONSTRAINTS"
    HealthConstraintsInvalid -> "HEALTH_CONSTRAINTS_INVALID"
    HookExecutionFailure -> "HOOK_EXECUTION_FAILURE"
    IAMRoleMissing -> "IAM_ROLE_MISSING"
    IAMRolePermissions -> "IAM_ROLE_PERMISSIONS"
    InternalError -> "INTERNAL_ERROR"
    InvalidEcsService -> "INVALID_ECS_SERVICE"
    InvalidLambdaConfiguration -> "INVALID_LAMBDA_CONFIGURATION"
    InvalidLambdaFunction -> "INVALID_LAMBDA_FUNCTION"
    InvalidRevision -> "INVALID_REVISION"
    ManualStop -> "MANUAL_STOP"
    MissingBlueGreenDeploymentConfiguration -> "MISSING_BLUE_GREEN_DEPLOYMENT_CONFIGURATION"
    MissingElbInformation -> "MISSING_ELB_INFORMATION"
    MissingGithubToken -> "MISSING_GITHUB_TOKEN"
    NoEC2Subscription -> "NO_EC2_SUBSCRIPTION"
    NoInstances -> "NO_INSTANCES"
    OverMaxInstances -> "OVER_MAX_INSTANCES"
    ResourceLimitExceeded -> "RESOURCE_LIMIT_EXCEEDED"
    RevisionMissing -> "REVISION_MISSING"
    Throttled -> "THROTTLED"
    Timeout -> "TIMEOUT"

instance Hashable DeployErrorCode

instance NFData DeployErrorCode

instance ToByteString DeployErrorCode

instance ToQuery DeployErrorCode

instance ToHeader DeployErrorCode

instance FromJSON DeployErrorCode where
  parseJSON = parseJSONText "DeployErrorCode"
