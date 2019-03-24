{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types.Sum where

import Network.AWS.Prelude

data ApplicationRevisionSortBy
  = FirstUsedTime
  | LastUsedTime
  | RegisterTime
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ApplicationRevisionSortBy where
    parser = takeLowerText >>= \case
        "firstusedtime" -> pure FirstUsedTime
        "lastusedtime" -> pure LastUsedTime
        "registertime" -> pure RegisterTime
        e -> fromTextError $ "Failure parsing ApplicationRevisionSortBy from value: '" <> e
           <> "'. Accepted values: firstusedtime, lastusedtime, registertime"

instance ToText ApplicationRevisionSortBy where
    toText = \case
        FirstUsedTime -> "firstUsedTime"
        LastUsedTime -> "lastUsedTime"
        RegisterTime -> "registerTime"

instance Hashable     ApplicationRevisionSortBy
instance NFData       ApplicationRevisionSortBy
instance ToByteString ApplicationRevisionSortBy
instance ToQuery      ApplicationRevisionSortBy
instance ToHeader     ApplicationRevisionSortBy

instance ToJSON ApplicationRevisionSortBy where
    toJSON = toJSONText

data AutoRollbackEvent
  = AREDeploymentFailure
  | AREDeploymentStopOnAlarm
  | AREDeploymentStopOnRequest
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AutoRollbackEvent where
    parser = takeLowerText >>= \case
        "deployment_failure" -> pure AREDeploymentFailure
        "deployment_stop_on_alarm" -> pure AREDeploymentStopOnAlarm
        "deployment_stop_on_request" -> pure AREDeploymentStopOnRequest
        e -> fromTextError $ "Failure parsing AutoRollbackEvent from value: '" <> e
           <> "'. Accepted values: deployment_failure, deployment_stop_on_alarm, deployment_stop_on_request"

instance ToText AutoRollbackEvent where
    toText = \case
        AREDeploymentFailure -> "DEPLOYMENT_FAILURE"
        AREDeploymentStopOnAlarm -> "DEPLOYMENT_STOP_ON_ALARM"
        AREDeploymentStopOnRequest -> "DEPLOYMENT_STOP_ON_REQUEST"

instance Hashable     AutoRollbackEvent
instance NFData       AutoRollbackEvent
instance ToByteString AutoRollbackEvent
instance ToQuery      AutoRollbackEvent
instance ToHeader     AutoRollbackEvent

instance ToJSON AutoRollbackEvent where
    toJSON = toJSONText

instance FromJSON AutoRollbackEvent where
    parseJSON = parseJSONText "AutoRollbackEvent"

data BundleType
  = JSON
  | TAR
  | TGZ
  | Yaml
  | Zip
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText BundleType where
    parser = takeLowerText >>= \case
        "json" -> pure JSON
        "tar" -> pure TAR
        "tgz" -> pure TGZ
        "yaml" -> pure Yaml
        "zip" -> pure Zip
        e -> fromTextError $ "Failure parsing BundleType from value: '" <> e
           <> "'. Accepted values: json, tar, tgz, yaml, zip"

instance ToText BundleType where
    toText = \case
        JSON -> "JSON"
        TAR -> "tar"
        TGZ -> "tgz"
        Yaml -> "YAML"
        Zip -> "zip"

instance Hashable     BundleType
instance NFData       BundleType
instance ToByteString BundleType
instance ToQuery      BundleType
instance ToHeader     BundleType

instance ToJSON BundleType where
    toJSON = toJSONText

instance FromJSON BundleType where
    parseJSON = parseJSONText "BundleType"

data ComputePlatform
  = Ecs
  | Lambda
  | Server
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ComputePlatform where
    parser = takeLowerText >>= \case
        "ecs" -> pure Ecs
        "lambda" -> pure Lambda
        "server" -> pure Server
        e -> fromTextError $ "Failure parsing ComputePlatform from value: '" <> e
           <> "'. Accepted values: ecs, lambda, server"

instance ToText ComputePlatform where
    toText = \case
        Ecs -> "ECS"
        Lambda -> "Lambda"
        Server -> "Server"

instance Hashable     ComputePlatform
instance NFData       ComputePlatform
instance ToByteString ComputePlatform
instance ToQuery      ComputePlatform
instance ToHeader     ComputePlatform

instance ToJSON ComputePlatform where
    toJSON = toJSONText

instance FromJSON ComputePlatform where
    parseJSON = parseJSONText "ComputePlatform"

data DeployErrorCode
  = AgentIssue
  | AlarmActive
  | ApplicationMissing
  | AutoScalingConfiguration
  | AutoScalingIAMRolePermissions
  | AutoscalingValidationError
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
  | RevisionMissing
  | Throttled
  | Timeout
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DeployErrorCode where
    parser = takeLowerText >>= \case
        "agent_issue" -> pure AgentIssue
        "alarm_active" -> pure AlarmActive
        "application_missing" -> pure ApplicationMissing
        "auto_scaling_configuration" -> pure AutoScalingConfiguration
        "auto_scaling_iam_role_permissions" -> pure AutoScalingIAMRolePermissions
        "autoscaling_validation_error" -> pure AutoscalingValidationError
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
        "revision_missing" -> pure RevisionMissing
        "throttled" -> pure Throttled
        "timeout" -> pure Timeout
        e -> fromTextError $ "Failure parsing DeployErrorCode from value: '" <> e
           <> "'. Accepted values: agent_issue, alarm_active, application_missing, auto_scaling_configuration, auto_scaling_iam_role_permissions, autoscaling_validation_error, deployment_group_missing, ecs_update_error, elastic_load_balancing_invalid, elb_invalid_instance, health_constraints, health_constraints_invalid, hook_execution_failure, iam_role_missing, iam_role_permissions, internal_error, invalid_ecs_service, invalid_lambda_configuration, invalid_lambda_function, invalid_revision, manual_stop, missing_blue_green_deployment_configuration, missing_elb_information, missing_github_token, no_ec2_subscription, no_instances, over_max_instances, revision_missing, throttled, timeout"

instance ToText DeployErrorCode where
    toText = \case
        AgentIssue -> "AGENT_ISSUE"
        AlarmActive -> "ALARM_ACTIVE"
        ApplicationMissing -> "APPLICATION_MISSING"
        AutoScalingConfiguration -> "AUTO_SCALING_CONFIGURATION"
        AutoScalingIAMRolePermissions -> "AUTO_SCALING_IAM_ROLE_PERMISSIONS"
        AutoscalingValidationError -> "AUTOSCALING_VALIDATION_ERROR"
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
        RevisionMissing -> "REVISION_MISSING"
        Throttled -> "THROTTLED"
        Timeout -> "TIMEOUT"

instance Hashable     DeployErrorCode
instance NFData       DeployErrorCode
instance ToByteString DeployErrorCode
instance ToQuery      DeployErrorCode
instance ToHeader     DeployErrorCode

instance FromJSON DeployErrorCode where
    parseJSON = parseJSONText "DeployErrorCode"

data DeploymentCreator
  = Autoscaling
  | CodeDeployRollback
  | User
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DeploymentCreator where
    parser = takeLowerText >>= \case
        "autoscaling" -> pure Autoscaling
        "codedeployrollback" -> pure CodeDeployRollback
        "user" -> pure User
        e -> fromTextError $ "Failure parsing DeploymentCreator from value: '" <> e
           <> "'. Accepted values: autoscaling, codedeployrollback, user"

instance ToText DeploymentCreator where
    toText = \case
        Autoscaling -> "autoscaling"
        CodeDeployRollback -> "codeDeployRollback"
        User -> "user"

instance Hashable     DeploymentCreator
instance NFData       DeploymentCreator
instance ToByteString DeploymentCreator
instance ToQuery      DeploymentCreator
instance ToHeader     DeploymentCreator

instance FromJSON DeploymentCreator where
    parseJSON = parseJSONText "DeploymentCreator"

data DeploymentOption
  = WithTrafficControl
  | WithoutTrafficControl
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DeploymentOption where
    parser = takeLowerText >>= \case
        "with_traffic_control" -> pure WithTrafficControl
        "without_traffic_control" -> pure WithoutTrafficControl
        e -> fromTextError $ "Failure parsing DeploymentOption from value: '" <> e
           <> "'. Accepted values: with_traffic_control, without_traffic_control"

instance ToText DeploymentOption where
    toText = \case
        WithTrafficControl -> "WITH_TRAFFIC_CONTROL"
        WithoutTrafficControl -> "WITHOUT_TRAFFIC_CONTROL"

instance Hashable     DeploymentOption
instance NFData       DeploymentOption
instance ToByteString DeploymentOption
instance ToQuery      DeploymentOption
instance ToHeader     DeploymentOption

instance ToJSON DeploymentOption where
    toJSON = toJSONText

instance FromJSON DeploymentOption where
    parseJSON = parseJSONText "DeploymentOption"

data DeploymentReadyAction
  = ContinueDeployment
  | StopDeployment
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DeploymentReadyAction where
    parser = takeLowerText >>= \case
        "continue_deployment" -> pure ContinueDeployment
        "stop_deployment" -> pure StopDeployment
        e -> fromTextError $ "Failure parsing DeploymentReadyAction from value: '" <> e
           <> "'. Accepted values: continue_deployment, stop_deployment"

instance ToText DeploymentReadyAction where
    toText = \case
        ContinueDeployment -> "CONTINUE_DEPLOYMENT"
        StopDeployment -> "STOP_DEPLOYMENT"

instance Hashable     DeploymentReadyAction
instance NFData       DeploymentReadyAction
instance ToByteString DeploymentReadyAction
instance ToQuery      DeploymentReadyAction
instance ToHeader     DeploymentReadyAction

instance ToJSON DeploymentReadyAction where
    toJSON = toJSONText

instance FromJSON DeploymentReadyAction where
    parseJSON = parseJSONText "DeploymentReadyAction"

data DeploymentStatus
  = Created
  | Failed
  | InProgress
  | Queued
  | Ready
  | Stopped
  | Succeeded
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DeploymentStatus where
    parser = takeLowerText >>= \case
        "created" -> pure Created
        "failed" -> pure Failed
        "inprogress" -> pure InProgress
        "queued" -> pure Queued
        "ready" -> pure Ready
        "stopped" -> pure Stopped
        "succeeded" -> pure Succeeded
        e -> fromTextError $ "Failure parsing DeploymentStatus from value: '" <> e
           <> "'. Accepted values: created, failed, inprogress, queued, ready, stopped, succeeded"

instance ToText DeploymentStatus where
    toText = \case
        Created -> "Created"
        Failed -> "Failed"
        InProgress -> "InProgress"
        Queued -> "Queued"
        Ready -> "Ready"
        Stopped -> "Stopped"
        Succeeded -> "Succeeded"

instance Hashable     DeploymentStatus
instance NFData       DeploymentStatus
instance ToByteString DeploymentStatus
instance ToQuery      DeploymentStatus
instance ToHeader     DeploymentStatus

instance ToJSON DeploymentStatus where
    toJSON = toJSONText

instance FromJSON DeploymentStatus where
    parseJSON = parseJSONText "DeploymentStatus"

data DeploymentTargetType
  = ECSTarget
  | InstanceTarget
  | LambdaTarget
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DeploymentTargetType where
    parser = takeLowerText >>= \case
        "ecstarget" -> pure ECSTarget
        "instancetarget" -> pure InstanceTarget
        "lambdatarget" -> pure LambdaTarget
        e -> fromTextError $ "Failure parsing DeploymentTargetType from value: '" <> e
           <> "'. Accepted values: ecstarget, instancetarget, lambdatarget"

instance ToText DeploymentTargetType where
    toText = \case
        ECSTarget -> "ECSTarget"
        InstanceTarget -> "InstanceTarget"
        LambdaTarget -> "LambdaTarget"

instance Hashable     DeploymentTargetType
instance NFData       DeploymentTargetType
instance ToByteString DeploymentTargetType
instance ToQuery      DeploymentTargetType
instance ToHeader     DeploymentTargetType

instance FromJSON DeploymentTargetType where
    parseJSON = parseJSONText "DeploymentTargetType"

data DeploymentType
  = BlueGreen
  | InPlace
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DeploymentType where
    parser = takeLowerText >>= \case
        "blue_green" -> pure BlueGreen
        "in_place" -> pure InPlace
        e -> fromTextError $ "Failure parsing DeploymentType from value: '" <> e
           <> "'. Accepted values: blue_green, in_place"

instance ToText DeploymentType where
    toText = \case
        BlueGreen -> "BLUE_GREEN"
        InPlace -> "IN_PLACE"

instance Hashable     DeploymentType
instance NFData       DeploymentType
instance ToByteString DeploymentType
instance ToQuery      DeploymentType
instance ToHeader     DeploymentType

instance ToJSON DeploymentType where
    toJSON = toJSONText

instance FromJSON DeploymentType where
    parseJSON = parseJSONText "DeploymentType"

data DeploymentWaitType
  = ReadyWait
  | TerminationWait
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DeploymentWaitType where
    parser = takeLowerText >>= \case
        "ready_wait" -> pure ReadyWait
        "termination_wait" -> pure TerminationWait
        e -> fromTextError $ "Failure parsing DeploymentWaitType from value: '" <> e
           <> "'. Accepted values: ready_wait, termination_wait"

instance ToText DeploymentWaitType where
    toText = \case
        ReadyWait -> "READY_WAIT"
        TerminationWait -> "TERMINATION_WAIT"

instance Hashable     DeploymentWaitType
instance NFData       DeploymentWaitType
instance ToByteString DeploymentWaitType
instance ToQuery      DeploymentWaitType
instance ToHeader     DeploymentWaitType

instance ToJSON DeploymentWaitType where
    toJSON = toJSONText

data EC2TagFilterType
  = KeyAndValue
  | KeyOnly
  | ValueOnly
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EC2TagFilterType where
    parser = takeLowerText >>= \case
        "key_and_value" -> pure KeyAndValue
        "key_only" -> pure KeyOnly
        "value_only" -> pure ValueOnly
        e -> fromTextError $ "Failure parsing EC2TagFilterType from value: '" <> e
           <> "'. Accepted values: key_and_value, key_only, value_only"

instance ToText EC2TagFilterType where
    toText = \case
        KeyAndValue -> "KEY_AND_VALUE"
        KeyOnly -> "KEY_ONLY"
        ValueOnly -> "VALUE_ONLY"

instance Hashable     EC2TagFilterType
instance NFData       EC2TagFilterType
instance ToByteString EC2TagFilterType
instance ToQuery      EC2TagFilterType
instance ToHeader     EC2TagFilterType

instance ToJSON EC2TagFilterType where
    toJSON = toJSONText

instance FromJSON EC2TagFilterType where
    parseJSON = parseJSONText "EC2TagFilterType"

data FileExistsBehavior
  = Disallow
  | Overwrite
  | Retain
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText FileExistsBehavior where
    parser = takeLowerText >>= \case
        "disallow" -> pure Disallow
        "overwrite" -> pure Overwrite
        "retain" -> pure Retain
        e -> fromTextError $ "Failure parsing FileExistsBehavior from value: '" <> e
           <> "'. Accepted values: disallow, overwrite, retain"

instance ToText FileExistsBehavior where
    toText = \case
        Disallow -> "DISALLOW"
        Overwrite -> "OVERWRITE"
        Retain -> "RETAIN"

instance Hashable     FileExistsBehavior
instance NFData       FileExistsBehavior
instance ToByteString FileExistsBehavior
instance ToQuery      FileExistsBehavior
instance ToHeader     FileExistsBehavior

instance ToJSON FileExistsBehavior where
    toJSON = toJSONText

instance FromJSON FileExistsBehavior where
    parseJSON = parseJSONText "FileExistsBehavior"

data GreenFleetProvisioningAction
  = CopyAutoScalingGroup
  | DiscoverExisting
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText GreenFleetProvisioningAction where
    parser = takeLowerText >>= \case
        "copy_auto_scaling_group" -> pure CopyAutoScalingGroup
        "discover_existing" -> pure DiscoverExisting
        e -> fromTextError $ "Failure parsing GreenFleetProvisioningAction from value: '" <> e
           <> "'. Accepted values: copy_auto_scaling_group, discover_existing"

instance ToText GreenFleetProvisioningAction where
    toText = \case
        CopyAutoScalingGroup -> "COPY_AUTO_SCALING_GROUP"
        DiscoverExisting -> "DISCOVER_EXISTING"

instance Hashable     GreenFleetProvisioningAction
instance NFData       GreenFleetProvisioningAction
instance ToByteString GreenFleetProvisioningAction
instance ToQuery      GreenFleetProvisioningAction
instance ToHeader     GreenFleetProvisioningAction

instance ToJSON GreenFleetProvisioningAction where
    toJSON = toJSONText

instance FromJSON GreenFleetProvisioningAction where
    parseJSON = parseJSONText "GreenFleetProvisioningAction"

data InstanceAction
  = KeepAlive
  | Terminate
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InstanceAction where
    parser = takeLowerText >>= \case
        "keep_alive" -> pure KeepAlive
        "terminate" -> pure Terminate
        e -> fromTextError $ "Failure parsing InstanceAction from value: '" <> e
           <> "'. Accepted values: keep_alive, terminate"

instance ToText InstanceAction where
    toText = \case
        KeepAlive -> "KEEP_ALIVE"
        Terminate -> "TERMINATE"

instance Hashable     InstanceAction
instance NFData       InstanceAction
instance ToByteString InstanceAction
instance ToQuery      InstanceAction
instance ToHeader     InstanceAction

instance ToJSON InstanceAction where
    toJSON = toJSONText

instance FromJSON InstanceAction where
    parseJSON = parseJSONText "InstanceAction"

data LifecycleErrorCode
  = ScriptFailed
  | ScriptMissing
  | ScriptNotExecutable
  | ScriptTimedOut
  | Success
  | UnknownError
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LifecycleErrorCode where
    parser = takeLowerText >>= \case
        "scriptfailed" -> pure ScriptFailed
        "scriptmissing" -> pure ScriptMissing
        "scriptnotexecutable" -> pure ScriptNotExecutable
        "scripttimedout" -> pure ScriptTimedOut
        "success" -> pure Success
        "unknownerror" -> pure UnknownError
        e -> fromTextError $ "Failure parsing LifecycleErrorCode from value: '" <> e
           <> "'. Accepted values: scriptfailed, scriptmissing, scriptnotexecutable, scripttimedout, success, unknownerror"

instance ToText LifecycleErrorCode where
    toText = \case
        ScriptFailed -> "ScriptFailed"
        ScriptMissing -> "ScriptMissing"
        ScriptNotExecutable -> "ScriptNotExecutable"
        ScriptTimedOut -> "ScriptTimedOut"
        Success -> "Success"
        UnknownError -> "UnknownError"

instance Hashable     LifecycleErrorCode
instance NFData       LifecycleErrorCode
instance ToByteString LifecycleErrorCode
instance ToQuery      LifecycleErrorCode
instance ToHeader     LifecycleErrorCode

instance FromJSON LifecycleErrorCode where
    parseJSON = parseJSONText "LifecycleErrorCode"

data LifecycleEventStatus
  = LESFailed
  | LESInProgress
  | LESPending
  | LESSkipped
  | LESSucceeded
  | LESUnknown
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LifecycleEventStatus where
    parser = takeLowerText >>= \case
        "failed" -> pure LESFailed
        "inprogress" -> pure LESInProgress
        "pending" -> pure LESPending
        "skipped" -> pure LESSkipped
        "succeeded" -> pure LESSucceeded
        "unknown" -> pure LESUnknown
        e -> fromTextError $ "Failure parsing LifecycleEventStatus from value: '" <> e
           <> "'. Accepted values: failed, inprogress, pending, skipped, succeeded, unknown"

instance ToText LifecycleEventStatus where
    toText = \case
        LESFailed -> "Failed"
        LESInProgress -> "InProgress"
        LESPending -> "Pending"
        LESSkipped -> "Skipped"
        LESSucceeded -> "Succeeded"
        LESUnknown -> "Unknown"

instance Hashable     LifecycleEventStatus
instance NFData       LifecycleEventStatus
instance ToByteString LifecycleEventStatus
instance ToQuery      LifecycleEventStatus
instance ToHeader     LifecycleEventStatus

instance ToJSON LifecycleEventStatus where
    toJSON = toJSONText

instance FromJSON LifecycleEventStatus where
    parseJSON = parseJSONText "LifecycleEventStatus"

data ListStateFilterAction
  = Exclude
  | Ignore
  | Include
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ListStateFilterAction where
    parser = takeLowerText >>= \case
        "exclude" -> pure Exclude
        "ignore" -> pure Ignore
        "include" -> pure Include
        e -> fromTextError $ "Failure parsing ListStateFilterAction from value: '" <> e
           <> "'. Accepted values: exclude, ignore, include"

instance ToText ListStateFilterAction where
    toText = \case
        Exclude -> "exclude"
        Ignore -> "ignore"
        Include -> "include"

instance Hashable     ListStateFilterAction
instance NFData       ListStateFilterAction
instance ToByteString ListStateFilterAction
instance ToQuery      ListStateFilterAction
instance ToHeader     ListStateFilterAction

instance ToJSON ListStateFilterAction where
    toJSON = toJSONText

data MinimumHealthyHostsType
  = FleetPercent
  | HostCount
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText MinimumHealthyHostsType where
    parser = takeLowerText >>= \case
        "fleet_percent" -> pure FleetPercent
        "host_count" -> pure HostCount
        e -> fromTextError $ "Failure parsing MinimumHealthyHostsType from value: '" <> e
           <> "'. Accepted values: fleet_percent, host_count"

instance ToText MinimumHealthyHostsType where
    toText = \case
        FleetPercent -> "FLEET_PERCENT"
        HostCount -> "HOST_COUNT"

instance Hashable     MinimumHealthyHostsType
instance NFData       MinimumHealthyHostsType
instance ToByteString MinimumHealthyHostsType
instance ToQuery      MinimumHealthyHostsType
instance ToHeader     MinimumHealthyHostsType

instance ToJSON MinimumHealthyHostsType where
    toJSON = toJSONText

instance FromJSON MinimumHealthyHostsType where
    parseJSON = parseJSONText "MinimumHealthyHostsType"

data RegistrationStatus
  = Deregistered
  | Registered
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RegistrationStatus where
    parser = takeLowerText >>= \case
        "deregistered" -> pure Deregistered
        "registered" -> pure Registered
        e -> fromTextError $ "Failure parsing RegistrationStatus from value: '" <> e
           <> "'. Accepted values: deregistered, registered"

instance ToText RegistrationStatus where
    toText = \case
        Deregistered -> "Deregistered"
        Registered -> "Registered"

instance Hashable     RegistrationStatus
instance NFData       RegistrationStatus
instance ToByteString RegistrationStatus
instance ToQuery      RegistrationStatus
instance ToHeader     RegistrationStatus

instance ToJSON RegistrationStatus where
    toJSON = toJSONText

data RevisionLocationType
  = AppSpecContent
  | GitHub
  | S3
  | String
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RevisionLocationType where
    parser = takeLowerText >>= \case
        "appspeccontent" -> pure AppSpecContent
        "github" -> pure GitHub
        "s3" -> pure S3
        "string" -> pure String
        e -> fromTextError $ "Failure parsing RevisionLocationType from value: '" <> e
           <> "'. Accepted values: appspeccontent, github, s3, string"

instance ToText RevisionLocationType where
    toText = \case
        AppSpecContent -> "AppSpecContent"
        GitHub -> "GitHub"
        S3 -> "S3"
        String -> "String"

instance Hashable     RevisionLocationType
instance NFData       RevisionLocationType
instance ToByteString RevisionLocationType
instance ToQuery      RevisionLocationType
instance ToHeader     RevisionLocationType

instance ToJSON RevisionLocationType where
    toJSON = toJSONText

instance FromJSON RevisionLocationType where
    parseJSON = parseJSONText "RevisionLocationType"

data SortOrder
  = Ascending
  | Descending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SortOrder where
    parser = takeLowerText >>= \case
        "ascending" -> pure Ascending
        "descending" -> pure Descending
        e -> fromTextError $ "Failure parsing SortOrder from value: '" <> e
           <> "'. Accepted values: ascending, descending"

instance ToText SortOrder where
    toText = \case
        Ascending -> "ascending"
        Descending -> "descending"

instance Hashable     SortOrder
instance NFData       SortOrder
instance ToByteString SortOrder
instance ToQuery      SortOrder
instance ToHeader     SortOrder

instance ToJSON SortOrder where
    toJSON = toJSONText

data StopStatus
  = SSPending
  | SSSucceeded
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StopStatus where
    parser = takeLowerText >>= \case
        "pending" -> pure SSPending
        "succeeded" -> pure SSSucceeded
        e -> fromTextError $ "Failure parsing StopStatus from value: '" <> e
           <> "'. Accepted values: pending, succeeded"

instance ToText StopStatus where
    toText = \case
        SSPending -> "Pending"
        SSSucceeded -> "Succeeded"

instance Hashable     StopStatus
instance NFData       StopStatus
instance ToByteString StopStatus
instance ToQuery      StopStatus
instance ToHeader     StopStatus

instance FromJSON StopStatus where
    parseJSON = parseJSONText "StopStatus"

data TagFilterType
  = TFTKeyAndValue
  | TFTKeyOnly
  | TFTValueOnly
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TagFilterType where
    parser = takeLowerText >>= \case
        "key_and_value" -> pure TFTKeyAndValue
        "key_only" -> pure TFTKeyOnly
        "value_only" -> pure TFTValueOnly
        e -> fromTextError $ "Failure parsing TagFilterType from value: '" <> e
           <> "'. Accepted values: key_and_value, key_only, value_only"

instance ToText TagFilterType where
    toText = \case
        TFTKeyAndValue -> "KEY_AND_VALUE"
        TFTKeyOnly -> "KEY_ONLY"
        TFTValueOnly -> "VALUE_ONLY"

instance Hashable     TagFilterType
instance NFData       TagFilterType
instance ToByteString TagFilterType
instance ToQuery      TagFilterType
instance ToHeader     TagFilterType

instance ToJSON TagFilterType where
    toJSON = toJSONText

instance FromJSON TagFilterType where
    parseJSON = parseJSONText "TagFilterType"

data TargetFilterName
  = ServerInstanceLabel
  | TargetStatus
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TargetFilterName where
    parser = takeLowerText >>= \case
        "serverinstancelabel" -> pure ServerInstanceLabel
        "targetstatus" -> pure TargetStatus
        e -> fromTextError $ "Failure parsing TargetFilterName from value: '" <> e
           <> "'. Accepted values: serverinstancelabel, targetstatus"

instance ToText TargetFilterName where
    toText = \case
        ServerInstanceLabel -> "ServerInstanceLabel"
        TargetStatus -> "TargetStatus"

instance Hashable     TargetFilterName
instance NFData       TargetFilterName
instance ToByteString TargetFilterName
instance ToQuery      TargetFilterName
instance ToHeader     TargetFilterName

instance ToJSON TargetFilterName where
    toJSON = toJSONText

data TargetLabel
  = TLBlue
  | TLGreen
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TargetLabel where
    parser = takeLowerText >>= \case
        "blue" -> pure TLBlue
        "green" -> pure TLGreen
        e -> fromTextError $ "Failure parsing TargetLabel from value: '" <> e
           <> "'. Accepted values: blue, green"

instance ToText TargetLabel where
    toText = \case
        TLBlue -> "Blue"
        TLGreen -> "Green"

instance Hashable     TargetLabel
instance NFData       TargetLabel
instance ToByteString TargetLabel
instance ToQuery      TargetLabel
instance ToHeader     TargetLabel

instance FromJSON TargetLabel where
    parseJSON = parseJSONText "TargetLabel"

data TargetStatus
  = TSFailed
  | TSInProgress
  | TSPending
  | TSReady
  | TSSkipped
  | TSSucceeded
  | TSUnknown
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TargetStatus where
    parser = takeLowerText >>= \case
        "failed" -> pure TSFailed
        "inprogress" -> pure TSInProgress
        "pending" -> pure TSPending
        "ready" -> pure TSReady
        "skipped" -> pure TSSkipped
        "succeeded" -> pure TSSucceeded
        "unknown" -> pure TSUnknown
        e -> fromTextError $ "Failure parsing TargetStatus from value: '" <> e
           <> "'. Accepted values: failed, inprogress, pending, ready, skipped, succeeded, unknown"

instance ToText TargetStatus where
    toText = \case
        TSFailed -> "Failed"
        TSInProgress -> "InProgress"
        TSPending -> "Pending"
        TSReady -> "Ready"
        TSSkipped -> "Skipped"
        TSSucceeded -> "Succeeded"
        TSUnknown -> "Unknown"

instance Hashable     TargetStatus
instance NFData       TargetStatus
instance ToByteString TargetStatus
instance ToQuery      TargetStatus
instance ToHeader     TargetStatus

instance FromJSON TargetStatus where
    parseJSON = parseJSONText "TargetStatus"

data TrafficRoutingType
  = AllAtOnce
  | TimeBasedCanary
  | TimeBasedLinear
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TrafficRoutingType where
    parser = takeLowerText >>= \case
        "allatonce" -> pure AllAtOnce
        "timebasedcanary" -> pure TimeBasedCanary
        "timebasedlinear" -> pure TimeBasedLinear
        e -> fromTextError $ "Failure parsing TrafficRoutingType from value: '" <> e
           <> "'. Accepted values: allatonce, timebasedcanary, timebasedlinear"

instance ToText TrafficRoutingType where
    toText = \case
        AllAtOnce -> "AllAtOnce"
        TimeBasedCanary -> "TimeBasedCanary"
        TimeBasedLinear -> "TimeBasedLinear"

instance Hashable     TrafficRoutingType
instance NFData       TrafficRoutingType
instance ToByteString TrafficRoutingType
instance ToQuery      TrafficRoutingType
instance ToHeader     TrafficRoutingType

instance ToJSON TrafficRoutingType where
    toJSON = toJSONText

instance FromJSON TrafficRoutingType where
    parseJSON = parseJSONText "TrafficRoutingType"

data TriggerEventType
  = DeploymentFailure
  | DeploymentReady
  | DeploymentRollback
  | DeploymentStart
  | DeploymentStop
  | DeploymentSuccess
  | InstanceFailure
  | InstanceReady
  | InstanceStart
  | InstanceSuccess
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TriggerEventType where
    parser = takeLowerText >>= \case
        "deploymentfailure" -> pure DeploymentFailure
        "deploymentready" -> pure DeploymentReady
        "deploymentrollback" -> pure DeploymentRollback
        "deploymentstart" -> pure DeploymentStart
        "deploymentstop" -> pure DeploymentStop
        "deploymentsuccess" -> pure DeploymentSuccess
        "instancefailure" -> pure InstanceFailure
        "instanceready" -> pure InstanceReady
        "instancestart" -> pure InstanceStart
        "instancesuccess" -> pure InstanceSuccess
        e -> fromTextError $ "Failure parsing TriggerEventType from value: '" <> e
           <> "'. Accepted values: deploymentfailure, deploymentready, deploymentrollback, deploymentstart, deploymentstop, deploymentsuccess, instancefailure, instanceready, instancestart, instancesuccess"

instance ToText TriggerEventType where
    toText = \case
        DeploymentFailure -> "DeploymentFailure"
        DeploymentReady -> "DeploymentReady"
        DeploymentRollback -> "DeploymentRollback"
        DeploymentStart -> "DeploymentStart"
        DeploymentStop -> "DeploymentStop"
        DeploymentSuccess -> "DeploymentSuccess"
        InstanceFailure -> "InstanceFailure"
        InstanceReady -> "InstanceReady"
        InstanceStart -> "InstanceStart"
        InstanceSuccess -> "InstanceSuccess"

instance Hashable     TriggerEventType
instance NFData       TriggerEventType
instance ToByteString TriggerEventType
instance ToQuery      TriggerEventType
instance ToHeader     TriggerEventType

instance ToJSON TriggerEventType where
    toJSON = toJSONText

instance FromJSON TriggerEventType where
    parseJSON = parseJSONText "TriggerEventType"
