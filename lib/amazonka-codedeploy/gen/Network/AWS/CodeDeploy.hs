{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS CodeDeploy__
--
-- AWS CodeDeploy is a deployment service that automates application deployments to Amazon EC2 instances, on-premises instances running in your own facility, serverless AWS Lambda functions, or applications in an Amazon ECS service.
-- You can deploy a nearly unlimited variety of application content, such as an updated Lambda function, updated applications in an Amazon ECS service, code, web and configuration files, executables, packages, scripts, multimedia files, and so on. AWS CodeDeploy can deploy application content stored in Amazon S3 buckets, GitHub repositories, or Bitbucket repositories. You do not need to make changes to your existing code before you can use AWS CodeDeploy.
-- AWS CodeDeploy makes it easier for you to rapidly release new features, helps you avoid downtime during application deployment, and handles the complexity of updating your applications, without many of the risks associated with error-prone manual deployments.
-- __AWS CodeDeploy Components__
-- Use the information in this guide to help you work with the following AWS CodeDeploy components:
--
--     * __Application__ : A name that uniquely identifies the application you want to deploy. AWS CodeDeploy uses this name, which functions as a container, to ensure the correct combination of revision, deployment configuration, and deployment group are referenced during a deployment.
--
--
--     * __Deployment group__ : A set of individual instances, CodeDeploy Lambda deployment configuration settings, or an Amazon ECS service and network details. A Lambda deployment group specifies how to route traffic to a new version of a Lambda function. An Amazon ECS deployment group specifies the service created in Amazon ECS to deploy, a load balancer, and a listener to reroute production traffic to an updated containerized application. An EC2/On-premises deployment group contains individually tagged instances, Amazon EC2 instances in Amazon EC2 Auto Scaling groups, or both. All deployment groups can specify optional trigger, alarm, and rollback settings.
--
--
--     * __Deployment configuration__ : A set of deployment rules and deployment success and failure conditions used by AWS CodeDeploy during a deployment.
--
--
--     * __Deployment__ : The process and the components used when updating a Lambda function, a containerized application in an Amazon ECS service, or of installing content on one or more instances.
--
--
--     * __Application revisions__ : For an AWS Lambda deployment, this is an AppSpec file that specifies the Lambda function to be updated and one or more functions to validate deployment lifecycle events. For an Amazon ECS deployment, this is an AppSpec file that specifies the Amazon ECS task definition, container, and port where production traffic is rerouted. For an EC2/On-premises deployment, this is an archive file that contains source content—source code, webpages, executable files, and deployment scripts—along with an AppSpec file. Revisions are stored in Amazon S3 buckets or GitHub repositories. For Amazon S3, a revision is uniquely identified by its Amazon S3 object key and its ETag, version, or both. For GitHub, a revision is uniquely identified by its commit ID.
--
--
-- This guide also contains information to help you get details about the instances in your deployments, to make on-premises instances available for AWS CodeDeploy deployments, to get details about a Lambda function deployment, and to get details about Amazon ECS service deployments.
-- __AWS CodeDeploy Information Resources__
--
--     * <https://docs.aws.amazon.com/codedeploy/latest/userguide AWS CodeDeploy User Guide>
--
--
--     * <https://docs.aws.amazon.com/codedeploy/latest/APIReference/ AWS CodeDeploy API Reference Guide>
--
--
--     * <https://docs.aws.amazon.com/cli/latest/reference/deploy/index.html AWS CLI Reference for AWS CodeDeploy>
--
--
--     * <https://forums.aws.amazon.com/forum.jspa?forumID=179 AWS CodeDeploy Developer Forum>
module Network.AWS.CodeDeploy
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    -- $errors

    -- ** LifecycleHookLimitExceededException
    _LifecycleHookLimitExceededException,

    -- ** InvalidTimeRangeException
    _InvalidTimeRangeException,

    -- ** InvalidComputePlatformException
    _InvalidComputePlatformException,

    -- ** InvalidTagException
    _InvalidTagException,

    -- ** InvalidFileExistsBehaviorException
    _InvalidFileExistsBehaviorException,

    -- ** InvalidAlarmConfigException
    _InvalidAlarmConfigException,

    -- ** InstanceNameAlreadyRegisteredException
    _InstanceNameAlreadyRegisteredException,

    -- ** IamUserArnRequiredException
    _IamUserArnRequiredException,

    -- ** InvalidDeploymentGroupNameException
    _InvalidDeploymentGroupNameException,

    -- ** InvalidInstanceTypeException
    _InvalidInstanceTypeException,

    -- ** IamSessionArnAlreadyRegisteredException
    _IamSessionArnAlreadyRegisteredException,

    -- ** InvalidTrafficRoutingConfigurationException
    _InvalidTrafficRoutingConfigurationException,

    -- ** DescriptionTooLongException
    _DescriptionTooLongException,

    -- ** InvalidIamUserArnException
    _InvalidIamUserArnException,

    -- ** InvalidOnPremisesTagCombinationException
    _InvalidOnPremisesTagCombinationException,

    -- ** DeploymentNotStartedException
    _DeploymentNotStartedException,

    -- ** DeploymentConfigLimitExceededException
    _DeploymentConfigLimitExceededException,

    -- ** RoleRequiredException
    _RoleRequiredException,

    -- ** InvalidLoadBalancerInfoException
    _InvalidLoadBalancerInfoException,

    -- ** InvalidBlueGreenDeploymentConfigurationException
    _InvalidBlueGreenDeploymentConfigurationException,

    -- ** InvalidRoleException
    _InvalidRoleException,

    -- ** DeploymentConfigAlreadyExistsException
    _DeploymentConfigAlreadyExistsException,

    -- ** InvalidTargetInstancesException
    _InvalidTargetInstancesException,

    -- ** InvalidTagsToAddException
    _InvalidTagsToAddException,

    -- ** DeploymentLimitExceededException
    _DeploymentLimitExceededException,

    -- ** IamUserArnAlreadyRegisteredException
    _IamUserArnAlreadyRegisteredException,

    -- ** InvalidIamSessionArnException
    _InvalidIamSessionArnException,

    -- ** InstanceLimitExceededException
    _InstanceLimitExceededException,

    -- ** InvalidLifecycleEventHookExecutionIdException
    _InvalidLifecycleEventHookExecutionIdException,

    -- ** InvalidDeploymentStyleException
    _InvalidDeploymentStyleException,

    -- ** InvalidTargetFilterNameException
    _InvalidTargetFilterNameException,

    -- ** DeploymentTargetListSizeExceededException
    _DeploymentTargetListSizeExceededException,

    -- ** InvalidDeployedStateFilterException
    _InvalidDeployedStateFilterException,

    -- ** InvalidAutoScalingGroupException
    _InvalidAutoScalingGroupException,

    -- ** InvalidApplicationNameException
    _InvalidApplicationNameException,

    -- ** GitHubAccountTokenDoesNotExistException
    _GitHubAccountTokenDoesNotExistException,

    -- ** ApplicationDoesNotExistException
    _ApplicationDoesNotExistException,

    -- ** InvalidMinimumHealthyHostValueException
    _InvalidMinimumHealthyHostValueException,

    -- ** UnsupportedActionForDeploymentTypeException
    _UnsupportedActionForDeploymentTypeException,

    -- ** ResourceValidationException
    _ResourceValidationException,

    -- ** ArnNotSupportedException
    _ArnNotSupportedException,

    -- ** InvalidGitHubAccountTokenException
    _InvalidGitHubAccountTokenException,

    -- ** InvalidEC2TagCombinationException
    _InvalidEC2TagCombinationException,

    -- ** InvalidLifecycleEventHookExecutionStatusException
    _InvalidLifecycleEventHookExecutionStatusException,

    -- ** AlarmsLimitExceededException
    _AlarmsLimitExceededException,

    -- ** OperationNotSupportedException
    _OperationNotSupportedException,

    -- ** DeploymentTargetDoesNotExistException
    _DeploymentTargetDoesNotExistException,

    -- ** InvalidTagFilterException
    _InvalidTagFilterException,

    -- ** InvalidTriggerConfigException
    _InvalidTriggerConfigException,

    -- ** InvalidDeploymentWaitTypeException
    _InvalidDeploymentWaitTypeException,

    -- ** InvalidIgnoreApplicationStopFailuresValueException
    _InvalidIgnoreApplicationStopFailuresValueException,

    -- ** InvalidUpdateOutdatedInstancesOnlyValueException
    _InvalidUpdateOutdatedInstancesOnlyValueException,

    -- ** TagRequiredException
    _TagRequiredException,

    -- ** DeploymentGroupNameRequiredException
    _DeploymentGroupNameRequiredException,

    -- ** BucketNameFilterRequiredException
    _BucketNameFilterRequiredException,

    -- ** DeploymentConfigDoesNotExistException
    _DeploymentConfigDoesNotExistException,

    -- ** InvalidBucketNameFilterException
    _InvalidBucketNameFilterException,

    -- ** DeploymentGroupAlreadyExistsException
    _DeploymentGroupAlreadyExistsException,

    -- ** InvalidSortByException
    _InvalidSortByException,

    -- ** RevisionDoesNotExistException
    _RevisionDoesNotExistException,

    -- ** InvalidTargetException
    _InvalidTargetException,

    -- ** DeploymentGroupLimitExceededException
    _DeploymentGroupLimitExceededException,

    -- ** DeploymentGroupDoesNotExistException
    _DeploymentGroupDoesNotExistException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** InvalidDeploymentConfigNameException
    _InvalidDeploymentConfigNameException,

    -- ** DeploymentConfigNameRequiredException
    _DeploymentConfigNameRequiredException,

    -- ** DeploymentIdRequiredException
    _DeploymentIdRequiredException,

    -- ** InvalidInstanceIdException
    _InvalidInstanceIdException,

    -- ** DeploymentIsNotInReadyStateException
    _DeploymentIsNotInReadyStateException,

    -- ** InvalidTargetGroupPairException
    _InvalidTargetGroupPairException,

    -- ** InvalidNextTokenException
    _InvalidNextTokenException,

    -- ** InstanceIdRequiredException
    _InstanceIdRequiredException,

    -- ** InvalidDeploymentIdException
    _InvalidDeploymentIdException,

    -- ** InvalidSortOrderException
    _InvalidSortOrderException,

    -- ** InvalidAutoRollbackConfigException
    _InvalidAutoRollbackConfigException,

    -- ** DeploymentAlreadyCompletedException
    _DeploymentAlreadyCompletedException,

    -- ** ECSServiceMappingLimitExceededException
    _ECSServiceMappingLimitExceededException,

    -- ** DeploymentDoesNotExistException
    _DeploymentDoesNotExistException,

    -- ** BatchLimitExceededException
    _BatchLimitExceededException,

    -- ** InvalidRevisionException
    _InvalidRevisionException,

    -- ** RevisionRequiredException
    _RevisionRequiredException,

    -- ** InstanceDoesNotExistException
    _InstanceDoesNotExistException,

    -- ** DeploymentConfigInUseException
    _DeploymentConfigInUseException,

    -- ** InvalidDeploymentConfigIdException
    _InvalidDeploymentConfigIdException,

    -- ** InvalidInputException
    _InvalidInputException,

    -- ** InvalidEC2TagException
    _InvalidEC2TagException,

    -- ** InvalidInstanceNameException
    _InvalidInstanceNameException,

    -- ** InstanceNameRequiredException
    _InstanceNameRequiredException,

    -- ** MultipleIamArnsProvidedException
    _MultipleIamArnsProvidedException,

    -- ** TriggerTargetsLimitExceededException
    _TriggerTargetsLimitExceededException,

    -- ** InvalidDeploymentStatusException
    _InvalidDeploymentStatusException,

    -- ** InvalidRegistrationStatusException
    _InvalidRegistrationStatusException,

    -- ** ApplicationNameRequiredException
    _ApplicationNameRequiredException,

    -- ** InstanceNotRegisteredException
    _InstanceNotRegisteredException,

    -- ** ApplicationAlreadyExistsException
    _ApplicationAlreadyExistsException,

    -- ** InvalidInstanceStatusException
    _InvalidInstanceStatusException,

    -- ** InvalidDeploymentTargetIdException
    _InvalidDeploymentTargetIdException,

    -- ** TagLimitExceededException
    _TagLimitExceededException,

    -- ** ApplicationLimitExceededException
    _ApplicationLimitExceededException,

    -- ** TagSetListLimitExceededException
    _TagSetListLimitExceededException,

    -- ** InvalidArnException
    _InvalidArnException,

    -- ** InvalidOperationException
    _InvalidOperationException,

    -- ** DeploymentTargetIdRequiredException
    _DeploymentTargetIdRequiredException,

    -- ** GitHubAccountTokenNameRequiredException
    _GitHubAccountTokenNameRequiredException,

    -- ** InvalidECSServiceException
    _InvalidECSServiceException,

    -- ** InvalidDeploymentInstanceTypeException
    _InvalidDeploymentInstanceTypeException,

    -- ** InvalidExternalIdException
    _InvalidExternalIdException,

    -- ** IamArnRequiredException
    _IamArnRequiredException,

    -- ** InvalidGitHubAccountTokenNameException
    _InvalidGitHubAccountTokenNameException,

    -- ** LifecycleEventAlreadyCompletedException
    _LifecycleEventAlreadyCompletedException,

    -- ** InvalidKeyPrefixFilterException
    _InvalidKeyPrefixFilterException,

    -- ** ResourceArnRequiredException
    _ResourceArnRequiredException,

    -- ** DeploymentAlreadyStartedException
    _DeploymentAlreadyStartedException,

    -- * Waiters
    -- $waiters

    -- ** DeploymentSuccessful
    mkDeploymentSuccessful,

    -- * Operations
    -- $operations

    -- ** RemoveTagsFromOnPremisesInstances
    module Network.AWS.CodeDeploy.RemoveTagsFromOnPremisesInstances,

    -- ** BatchGetDeploymentGroups
    module Network.AWS.CodeDeploy.BatchGetDeploymentGroups,

    -- ** DeleteDeploymentGroup
    module Network.AWS.CodeDeploy.DeleteDeploymentGroup,

    -- ** UpdateDeploymentGroup
    module Network.AWS.CodeDeploy.UpdateDeploymentGroup,

    -- ** ListOnPremisesInstances (Paginated)
    module Network.AWS.CodeDeploy.ListOnPremisesInstances,

    -- ** CreateDeploymentConfig
    module Network.AWS.CodeDeploy.CreateDeploymentConfig,

    -- ** GetApplicationRevision
    module Network.AWS.CodeDeploy.GetApplicationRevision,

    -- ** GetDeployment
    module Network.AWS.CodeDeploy.GetDeployment,

    -- ** DeleteDeploymentConfig
    module Network.AWS.CodeDeploy.DeleteDeploymentConfig,

    -- ** ListTagsForResource
    module Network.AWS.CodeDeploy.ListTagsForResource,

    -- ** GetDeploymentConfig
    module Network.AWS.CodeDeploy.GetDeploymentConfig,

    -- ** CreateDeployment
    module Network.AWS.CodeDeploy.CreateDeployment,

    -- ** BatchGetApplicationRevisions
    module Network.AWS.CodeDeploy.BatchGetApplicationRevisions,

    -- ** BatchGetDeployments
    module Network.AWS.CodeDeploy.BatchGetDeployments,

    -- ** GetOnPremisesInstance
    module Network.AWS.CodeDeploy.GetOnPremisesInstance,

    -- ** RegisterApplicationRevision
    module Network.AWS.CodeDeploy.RegisterApplicationRevision,

    -- ** ContinueDeployment
    module Network.AWS.CodeDeploy.ContinueDeployment,

    -- ** BatchGetApplications
    module Network.AWS.CodeDeploy.BatchGetApplications,

    -- ** DeleteApplication
    module Network.AWS.CodeDeploy.DeleteApplication,

    -- ** UpdateApplication
    module Network.AWS.CodeDeploy.UpdateApplication,

    -- ** DeleteGitHubAccountToken
    module Network.AWS.CodeDeploy.DeleteGitHubAccountToken,

    -- ** DeregisterOnPremisesInstance
    module Network.AWS.CodeDeploy.DeregisterOnPremisesInstance,

    -- ** PutLifecycleEventHookExecutionStatus
    module Network.AWS.CodeDeploy.PutLifecycleEventHookExecutionStatus,

    -- ** GetDeploymentTarget
    module Network.AWS.CodeDeploy.GetDeploymentTarget,

    -- ** CreateApplication
    module Network.AWS.CodeDeploy.CreateApplication,

    -- ** BatchGetDeploymentTargets
    module Network.AWS.CodeDeploy.BatchGetDeploymentTargets,

    -- ** StopDeployment
    module Network.AWS.CodeDeploy.StopDeployment,

    -- ** ListGitHubAccountTokenNames (Paginated)
    module Network.AWS.CodeDeploy.ListGitHubAccountTokenNames,

    -- ** GetApplication
    module Network.AWS.CodeDeploy.GetApplication,

    -- ** ListDeploymentGroups (Paginated)
    module Network.AWS.CodeDeploy.ListDeploymentGroups,

    -- ** BatchGetOnPremisesInstances
    module Network.AWS.CodeDeploy.BatchGetOnPremisesInstances,

    -- ** RegisterOnPremisesInstance
    module Network.AWS.CodeDeploy.RegisterOnPremisesInstance,

    -- ** CreateDeploymentGroup
    module Network.AWS.CodeDeploy.CreateDeploymentGroup,

    -- ** ListDeploymentConfigs (Paginated)
    module Network.AWS.CodeDeploy.ListDeploymentConfigs,

    -- ** GetDeploymentGroup
    module Network.AWS.CodeDeploy.GetDeploymentGroup,

    -- ** ListDeployments (Paginated)
    module Network.AWS.CodeDeploy.ListDeployments,

    -- ** TagResource
    module Network.AWS.CodeDeploy.TagResource,

    -- ** ListApplicationRevisions (Paginated)
    module Network.AWS.CodeDeploy.ListApplicationRevisions,

    -- ** ListApplications (Paginated)
    module Network.AWS.CodeDeploy.ListApplications,

    -- ** UntagResource
    module Network.AWS.CodeDeploy.UntagResource,

    -- ** DeleteResourcesByExternalId
    module Network.AWS.CodeDeploy.DeleteResourcesByExternalId,

    -- ** AddTagsToOnPremisesInstances
    module Network.AWS.CodeDeploy.AddTagsToOnPremisesInstances,

    -- ** ListDeploymentTargets (Paginated)
    module Network.AWS.CodeDeploy.ListDeploymentTargets,

    -- * Types

    -- ** CommitId
    CommitId (..),

    -- ** AlarmName
    AlarmName (..),

    -- ** TrafficRoute
    TrafficRoute (..),
    mkTrafficRoute,
    trListenerArns,

    -- ** DeploymentId
    DeploymentId (..),

    -- ** TriggerEventType
    TriggerEventType (..),

    -- ** GenericRevisionInfo
    GenericRevisionInfo (..),
    mkGenericRevisionInfo,
    griDeploymentGroups,
    griDescription,
    griFirstUsedTime,
    griLastUsedTime,
    griRegisterTime,

    -- ** EC2TagSet
    EC2TagSet (..),
    mkEC2TagSet,
    ectsEc2TagSetList,

    -- ** TargetId
    TargetId (..),

    -- ** DeploymentConfigName
    DeploymentConfigName (..),

    -- ** TargetArn
    TargetArn (..),

    -- ** InstanceArn
    InstanceArn (..),

    -- ** ApplicationInfo
    ApplicationInfo (..),
    mkApplicationInfo,
    aiApplicationId,
    aiApplicationName,
    aiComputePlatform,
    aiCreateTime,
    aiGitHubAccountName,
    aiLinkedToGitHub,

    -- ** DeploymentReadyAction
    DeploymentReadyAction (..),

    -- ** BundleType
    BundleType (..),

    -- ** TriggerName
    TriggerName (..),

    -- ** ETag
    ETag (..),

    -- ** OnPremisesTagSet
    OnPremisesTagSet (..),
    mkOnPremisesTagSet,
    optsOnPremisesTagSetList,

    -- ** ECSServiceName
    ECSServiceName (..),

    -- ** InstanceAction
    InstanceAction (..),

    -- ** TargetLabel
    TargetLabel (..),

    -- ** VersionId
    VersionId (..),

    -- ** S3Key
    S3Key (..),

    -- ** Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- ** Repository
    Repository (..),

    -- ** TimeBasedCanary
    TimeBasedCanary (..),
    mkTimeBasedCanary,
    tbcCanaryInterval,
    tbcCanaryPercentage,

    -- ** DeploymentOption
    DeploymentOption (..),

    -- ** TagFilterType
    TagFilterType (..),

    -- ** DeploymentWaitType
    DeploymentWaitType (..),

    -- ** TimeRange
    TimeRange (..),
    mkTimeRange,
    trEnd,
    trStart,

    -- ** LambdaFunctionName
    LambdaFunctionName (..),

    -- ** ComputePlatform
    ComputePlatform (..),

    -- ** DeploymentCreator
    DeploymentCreator (..),

    -- ** LifecycleEventName
    LifecycleEventName (..),

    -- ** LastDeploymentInfo
    LastDeploymentInfo (..),
    mkLastDeploymentInfo,
    ldiCreateTime,
    ldiDeploymentId,
    ldiEndTime,
    ldiStatus,

    -- ** AutoScalingGroup
    AutoScalingGroup (..),
    mkAutoScalingGroup,
    asgHook,
    asgName,

    -- ** Arn
    Arn (..),

    -- ** ELBInfo
    ELBInfo (..),
    mkELBInfo,
    elbiName,

    -- ** AutoRollbackEvent
    AutoRollbackEvent (..),

    -- ** LifecycleMessage
    LifecycleMessage (..),

    -- ** DeploymentGroupInfo
    DeploymentGroupInfo (..),
    mkDeploymentGroupInfo,
    dgiAlarmConfiguration,
    dgiApplicationName,
    dgiAutoRollbackConfiguration,
    dgiAutoScalingGroups,
    dgiBlueGreenDeploymentConfiguration,
    dgiComputePlatform,
    dgiDeploymentConfigName,
    dgiDeploymentGroupId,
    dgiDeploymentGroupName,
    dgiDeploymentStyle,
    dgiEc2TagFilters,
    dgiEc2TagSet,
    dgiEcsServices,
    dgiLastAttemptedDeployment,
    dgiLastSuccessfulDeployment,
    dgiLoadBalancerInfo,
    dgiOnPremisesInstanceTagFilters,
    dgiOnPremisesTagSet,
    dgiServiceRoleArn,
    dgiTargetRevision,
    dgiTriggerConfigurations,

    -- ** ApplicationRevisionSortBy
    ApplicationRevisionSortBy (..),

    -- ** ECSClusterName
    ECSClusterName (..),

    -- ** MinimumHealthyHosts
    MinimumHealthyHosts (..),
    mkMinimumHealthyHosts,
    mhhType,
    mhhValue,

    -- ** InstanceTarget
    InstanceTarget (..),
    mkInstanceTarget,
    itDeploymentId,
    itInstanceLabel,
    itLastUpdatedAt,
    itLifecycleEvents,
    itStatus,
    itTargetArn,
    itTargetId,

    -- ** TargetGroupInfo
    TargetGroupInfo (..),
    mkTargetGroupInfo,
    tgiName,

    -- ** DeploymentTarget
    DeploymentTarget (..),
    mkDeploymentTarget,
    dtCloudFormationTarget,
    dtDeploymentTargetType,
    dtEcsTarget,
    dtInstanceTarget,
    dtLambdaTarget,

    -- ** ListStateFilterAction
    ListStateFilterAction (..),

    -- ** CloudFormationTarget
    CloudFormationTarget (..),
    mkCloudFormationTarget,
    cftDeploymentId,
    cftLastUpdatedAt,
    cftLifecycleEvents,
    cftResourceType,
    cftStatus,
    cftTargetId,
    cftTargetVersionWeight,

    -- ** Value
    Value (..),

    -- ** LifecycleErrorCode
    LifecycleErrorCode (..),

    -- ** ListenerArn
    ListenerArn (..),

    -- ** RevisionLocation
    RevisionLocation (..),
    mkRevisionLocation,
    rlAppSpecContent,
    rlGitHubLocation,
    rlRevisionType,
    rlS3Location,
    rlString,

    -- ** LifecycleEventStatus
    LifecycleEventStatus (..),

    -- ** EC2TagFilter
    EC2TagFilter (..),
    mkEC2TagFilter,
    ectfKey,
    ectfType,
    ectfValue,

    -- ** ECSTaskSetStatus
    ECSTaskSetStatus (..),

    -- ** TargetFilterName
    TargetFilterName (..),

    -- ** DeploymentReadyOption
    DeploymentReadyOption (..),
    mkDeploymentReadyOption,
    droActionOnTimeout,
    droWaitTimeInMinutes,

    -- ** TimeBasedLinear
    TimeBasedLinear (..),
    mkTimeBasedLinear,
    tblLinearInterval,
    tblLinearPercentage,

    -- ** IamUserArn
    IamUserArn (..),

    -- ** DeploymentType
    DeploymentType (..),

    -- ** TrafficRoutingType
    TrafficRoutingType (..),

    -- ** RawString
    RawString (..),
    mkRawString,
    rsContent,
    rsSha256,

    -- ** ECSTarget
    ECSTarget (..),
    mkECSTarget,
    ecstDeploymentId,
    ecstLastUpdatedAt,
    ecstLifecycleEvents,
    ecstStatus,
    ecstTargetArn,
    ecstTargetId,
    ecstTaskSetsInfo,

    -- ** Diagnostics
    Diagnostics (..),
    mkDiagnostics,
    dErrorCode,
    dLogTail,
    dMessage,
    dScriptName,

    -- ** NextToken
    NextToken (..),

    -- ** TriggerTargetArn
    TriggerTargetArn (..),

    -- ** BlueGreenDeploymentConfiguration
    BlueGreenDeploymentConfiguration (..),
    mkBlueGreenDeploymentConfiguration,
    bgdcDeploymentReadyOption,
    bgdcGreenFleetProvisioningOption,
    bgdcTerminateBlueInstancesOnDeploymentSuccess,

    -- ** LogTail
    LogTail (..),

    -- ** StopStatus
    StopStatus (..),

    -- ** ErrorInformation
    ErrorInformation (..),
    mkErrorInformation,
    eiCode,
    eiMessage,

    -- ** LoadBalancerInfo
    LoadBalancerInfo (..),
    mkLoadBalancerInfo,
    lbiElbInfoList,
    lbiTargetGroupInfoList,
    lbiTargetGroupPairInfoList,

    -- ** SortOrder
    SortOrder (..),

    -- ** ApplicationId
    ApplicationId (..),

    -- ** InstanceInfo
    InstanceInfo (..),
    mkInstanceInfo,
    iiDeregisterTime,
    iiIamSessionArn,
    iiIamUserArn,
    iiInstanceArn,
    iiInstanceName,
    iiRegisterTime,
    iiTags,

    -- ** GreenFleetProvisioningOption
    GreenFleetProvisioningOption (..),
    mkGreenFleetProvisioningOption,
    gfpoAction,

    -- ** Key
    Key (..),

    -- ** DeploymentInfo
    DeploymentInfo (..),
    mkDeploymentInfo,
    diAdditionalDeploymentStatusInfo,
    diApplicationName,
    diAutoRollbackConfiguration,
    diBlueGreenDeploymentConfiguration,
    diCompleteTime,
    diComputePlatform,
    diCreateTime,
    diCreator,
    diDeploymentConfigName,
    diDeploymentGroupName,
    diDeploymentId,
    diDeploymentOverview,
    diDeploymentStatusMessages,
    diDeploymentStyle,
    diDescription,
    diErrorInformation,
    diExternalId,
    diFileExistsBehavior,
    diIgnoreApplicationStopFailures,
    diInstanceTerminationWaitTimeStarted,
    diLoadBalancerInfo,
    diPreviousRevision,
    diRevision,
    diRollbackInfo,
    diStartTime,
    diStatus,
    diTargetInstances,
    diUpdateOutdatedInstancesOnly,

    -- ** GreenFleetProvisioningAction
    GreenFleetProvisioningAction (..),

    -- ** RawStringSha256
    RawStringSha256 (..),

    -- ** TargetGroupPairInfo
    TargetGroupPairInfo (..),
    mkTargetGroupPairInfo,
    tgpiProdTrafficRoute,
    tgpiTargetGroups,
    tgpiTestTrafficRoute,

    -- ** FileExistsBehavior
    FileExistsBehavior (..),

    -- ** AutoScalingGroupName
    AutoScalingGroupName (..),

    -- ** TagFilter
    TagFilter (..),
    mkTagFilter,
    tfKey,
    tfType,
    tfValue,

    -- ** Version
    Version (..),

    -- ** LifecycleEvent
    LifecycleEvent (..),
    mkLifecycleEvent,
    leDiagnostics,
    leEndTime,
    leLifecycleEventName,
    leStartTime,
    leStatus,

    -- ** AdditionalDeploymentStatusInfo
    AdditionalDeploymentStatusInfo (..),

    -- ** ECSTaskSet
    ECSTaskSet (..),
    mkECSTaskSet,
    ecstsDesiredCount,
    ecstsIdentifer,
    ecstsPendingCount,
    ecstsRunningCount,
    ecstsStatus,
    ecstsTargetGroup,
    ecstsTaskSetLabel,
    ecstsTrafficWeight,

    -- ** TriggerConfig
    TriggerConfig (..),
    mkTriggerConfig,
    tcTriggerEvents,
    tcTriggerName,
    tcTriggerTargetArn,

    -- ** DeploymentOverview
    DeploymentOverview (..),
    mkDeploymentOverview,
    doFailed,
    doInProgress,
    doPending,
    doReady,
    doSkipped,
    doSucceeded,

    -- ** AppSpecContent
    AppSpecContent (..),
    mkAppSpecContent,
    ascContent,
    ascSha256,

    -- ** DeployErrorCode
    DeployErrorCode (..),

    -- ** ApplicationName
    ApplicationName (..),

    -- ** DeploymentConfigInfo
    DeploymentConfigInfo (..),
    mkDeploymentConfigInfo,
    dciComputePlatform,
    dciCreateTime,
    dciDeploymentConfigId,
    dciDeploymentConfigName,
    dciMinimumHealthyHosts,
    dciTrafficRoutingConfig,

    -- ** RevisionInfo
    RevisionInfo (..),
    mkRevisionInfo,
    riGenericRevisionInfo,
    riRevisionLocation,

    -- ** TargetStatus
    TargetStatus (..),

    -- ** DeploymentStatus
    DeploymentStatus (..),

    -- ** RegistrationStatus
    RegistrationStatus (..),

    -- ** S3Location
    S3Location (..),
    mkS3Location,
    slBucket,
    slBundleType,
    slETag,
    slKey,
    slVersion,

    -- ** ScriptName
    ScriptName (..),

    -- ** ECSService
    ECSService (..),
    mkECSService,
    ecssClusterName,
    ecssServiceName,

    -- ** BlueInstanceTerminationOption
    BlueInstanceTerminationOption (..),
    mkBlueInstanceTerminationOption,
    bitoAction,
    bitoTerminationWaitTimeInMinutes,

    -- ** RollbackInfo
    RollbackInfo (..),
    mkRollbackInfo,
    riRollbackDeploymentId,
    riRollbackMessage,
    riRollbackTriggeringDeploymentId,

    -- ** DeploymentTargetType
    DeploymentTargetType (..),

    -- ** MinimumHealthyHostsType
    MinimumHealthyHostsType (..),

    -- ** GitHubAccountTokenName
    GitHubAccountTokenName (..),

    -- ** ExternalId
    ExternalId (..),

    -- ** GitHubLocation
    GitHubLocation (..),
    mkGitHubLocation,
    ghlCommitId,
    ghlRepository,

    -- ** TrafficRoutingConfig
    TrafficRoutingConfig (..),
    mkTrafficRoutingConfig,
    trcTimeBasedCanary,
    trcTimeBasedLinear,
    trcType,

    -- ** FilterValue
    FilterValue (..),

    -- ** LambdaFunctionInfo
    LambdaFunctionInfo (..),
    mkLambdaFunctionInfo,
    lfiCurrentVersion,
    lfiFunctionAlias,
    lfiFunctionName,
    lfiTargetVersion,
    lfiTargetVersionWeight,

    -- ** DeploymentGroupId
    DeploymentGroupId (..),

    -- ** ErrorMessage
    ErrorMessage (..),

    -- ** Message
    Message (..),

    -- ** Alarm
    Alarm (..),
    mkAlarm,
    aName,

    -- ** AlarmConfiguration
    AlarmConfiguration (..),
    mkAlarmConfiguration,
    acAlarms,
    acEnabled,
    acIgnorePollAlarmFailure,

    -- ** TargetInstances
    TargetInstances (..),
    mkTargetInstances,
    tiAutoScalingGroups,
    tiEc2TagSet,
    tiTagFilters,

    -- ** LifecycleEventHookExecutionId
    LifecycleEventHookExecutionId (..),

    -- ** Description
    Description (..),

    -- ** InstanceName
    InstanceName (..),

    -- ** RevisionLocationType
    RevisionLocationType (..),

    -- ** DeploymentConfigId
    DeploymentConfigId (..),

    -- ** DeploymentStyle
    DeploymentStyle (..),
    mkDeploymentStyle,
    dsDeploymentOption,
    dsDeploymentType,

    -- ** LambdaTarget
    LambdaTarget (..),
    mkLambdaTarget,
    ltDeploymentId,
    ltLambdaFunctionInfo,
    ltLastUpdatedAt,
    ltLifecycleEvents,
    ltStatus,
    ltTargetArn,
    ltTargetId,

    -- ** EC2TagFilterType
    EC2TagFilterType (..),

    -- ** S3Bucket
    S3Bucket (..),

    -- ** IamSessionArn
    IamSessionArn (..),

    -- ** AutoRollbackConfiguration
    AutoRollbackConfiguration (..),
    mkAutoRollbackConfiguration,
    arcEnabled,
    arcEvents,

    -- ** DeploymentGroupName
    DeploymentGroupName (..),

    -- ** ServiceRoleArn
    ServiceRoleArn (..),

    -- ** GitHubAccountName
    GitHubAccountName (..),

    -- ** Hook
    Hook (..),

    -- ** Name
    Name (..),

    -- ** NewApplicationName
    NewApplicationName (..),

    -- ** TokenName
    TokenName (..),

    -- ** ResourceType
    ResourceType (..),

    -- ** StatusMessage
    StatusMessage (..),

    -- ** CurrentDeploymentGroupName
    CurrentDeploymentGroupName (..),

    -- ** NewDeploymentGroupName
    NewDeploymentGroupName (..),

    -- ** Content
    Content (..),

    -- ** Sha256
    Sha256 (..),

    -- ** Identifer
    Identifer (..),

    -- ** Bucket
    Bucket (..),

    -- ** RollbackMessage
    RollbackMessage (..),

    -- ** FunctionAlias
    FunctionAlias (..),

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.UTCTime,
    Lude.NominalDiffTime,
  )
where

import Network.AWS.CodeDeploy.AddTagsToOnPremisesInstances
import Network.AWS.CodeDeploy.BatchGetApplicationRevisions
import Network.AWS.CodeDeploy.BatchGetApplications
import Network.AWS.CodeDeploy.BatchGetDeploymentGroups
import Network.AWS.CodeDeploy.BatchGetDeploymentTargets
import Network.AWS.CodeDeploy.BatchGetDeployments
import Network.AWS.CodeDeploy.BatchGetOnPremisesInstances
import Network.AWS.CodeDeploy.ContinueDeployment
import Network.AWS.CodeDeploy.CreateApplication
import Network.AWS.CodeDeploy.CreateDeployment
import Network.AWS.CodeDeploy.CreateDeploymentConfig
import Network.AWS.CodeDeploy.CreateDeploymentGroup
import Network.AWS.CodeDeploy.DeleteApplication
import Network.AWS.CodeDeploy.DeleteDeploymentConfig
import Network.AWS.CodeDeploy.DeleteDeploymentGroup
import Network.AWS.CodeDeploy.DeleteGitHubAccountToken
import Network.AWS.CodeDeploy.DeleteResourcesByExternalId
import Network.AWS.CodeDeploy.DeregisterOnPremisesInstance
import Network.AWS.CodeDeploy.GetApplication
import Network.AWS.CodeDeploy.GetApplicationRevision
import Network.AWS.CodeDeploy.GetDeployment
import Network.AWS.CodeDeploy.GetDeploymentConfig
import Network.AWS.CodeDeploy.GetDeploymentGroup
import Network.AWS.CodeDeploy.GetDeploymentTarget
import Network.AWS.CodeDeploy.GetOnPremisesInstance
import Network.AWS.CodeDeploy.ListApplicationRevisions
import Network.AWS.CodeDeploy.ListApplications
import Network.AWS.CodeDeploy.ListDeploymentConfigs
import Network.AWS.CodeDeploy.ListDeploymentGroups
import Network.AWS.CodeDeploy.ListDeploymentTargets
import Network.AWS.CodeDeploy.ListDeployments
import Network.AWS.CodeDeploy.ListGitHubAccountTokenNames
import Network.AWS.CodeDeploy.ListOnPremisesInstances
import Network.AWS.CodeDeploy.ListTagsForResource
import Network.AWS.CodeDeploy.PutLifecycleEventHookExecutionStatus
import Network.AWS.CodeDeploy.RegisterApplicationRevision
import Network.AWS.CodeDeploy.RegisterOnPremisesInstance
import Network.AWS.CodeDeploy.RemoveTagsFromOnPremisesInstances
import Network.AWS.CodeDeploy.StopDeployment
import Network.AWS.CodeDeploy.TagResource
import Network.AWS.CodeDeploy.Types
import Network.AWS.CodeDeploy.UntagResource
import Network.AWS.CodeDeploy.UpdateApplication
import Network.AWS.CodeDeploy.UpdateDeploymentGroup
import Network.AWS.CodeDeploy.Waiters
import qualified Network.AWS.Prelude as Lude

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'CodeDeploy'.

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
