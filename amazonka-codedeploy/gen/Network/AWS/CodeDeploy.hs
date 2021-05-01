{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS CodeDeploy
--
-- AWS CodeDeploy is a deployment service that automates application
-- deployments to Amazon EC2 instances, on-premises instances running in
-- your own facility, serverless AWS Lambda functions, or applications in
-- an Amazon ECS service.
--
-- You can deploy a nearly unlimited variety of application content, such
-- as an updated Lambda function, updated applications in an Amazon ECS
-- service, code, web and configuration files, executables, packages,
-- scripts, multimedia files, and so on. AWS CodeDeploy can deploy
-- application content stored in Amazon S3 buckets, GitHub repositories, or
-- Bitbucket repositories. You do not need to make changes to your existing
-- code before you can use AWS CodeDeploy.
--
-- AWS CodeDeploy makes it easier for you to rapidly release new features,
-- helps you avoid downtime during application deployment, and handles the
-- complexity of updating your applications, without many of the risks
-- associated with error-prone manual deployments.
--
-- __AWS CodeDeploy Components__
--
-- Use the information in this guide to help you work with the following
-- AWS CodeDeploy components:
--
-- -   __Application__: A name that uniquely identifies the application you
--     want to deploy. AWS CodeDeploy uses this name, which functions as a
--     container, to ensure the correct combination of revision, deployment
--     configuration, and deployment group are referenced during a
--     deployment.
--
-- -   __Deployment group__: A set of individual instances, CodeDeploy
--     Lambda deployment configuration settings, or an Amazon ECS service
--     and network details. A Lambda deployment group specifies how to
--     route traffic to a new version of a Lambda function. An Amazon ECS
--     deployment group specifies the service created in Amazon ECS to
--     deploy, a load balancer, and a listener to reroute production
--     traffic to an updated containerized application. An EC2\/On-premises
--     deployment group contains individually tagged instances, Amazon EC2
--     instances in Amazon EC2 Auto Scaling groups, or both. All deployment
--     groups can specify optional trigger, alarm, and rollback settings.
--
-- -   __Deployment configuration__: A set of deployment rules and
--     deployment success and failure conditions used by AWS CodeDeploy
--     during a deployment.
--
-- -   __Deployment__: The process and the components used when updating a
--     Lambda function, a containerized application in an Amazon ECS
--     service, or of installing content on one or more instances.
--
-- -   __Application revisions__: For an AWS Lambda deployment, this is an
--     AppSpec file that specifies the Lambda function to be updated and
--     one or more functions to validate deployment lifecycle events. For
--     an Amazon ECS deployment, this is an AppSpec file that specifies the
--     Amazon ECS task definition, container, and port where production
--     traffic is rerouted. For an EC2\/On-premises deployment, this is an
--     archive file that contains source content—source code, webpages,
--     executable files, and deployment scripts—along with an AppSpec file.
--     Revisions are stored in Amazon S3 buckets or GitHub repositories.
--     For Amazon S3, a revision is uniquely identified by its Amazon S3
--     object key and its ETag, version, or both. For GitHub, a revision is
--     uniquely identified by its commit ID.
--
-- This guide also contains information to help you get details about the
-- instances in your deployments, to make on-premises instances available
-- for AWS CodeDeploy deployments, to get details about a Lambda function
-- deployment, and to get details about Amazon ECS service deployments.
--
-- __AWS CodeDeploy Information Resources__
--
-- -   <https://docs.aws.amazon.com/codedeploy/latest/userguide AWS CodeDeploy User Guide>
--
-- -   <https://docs.aws.amazon.com/codedeploy/latest/APIReference/ AWS CodeDeploy API Reference Guide>
--
-- -   <https://docs.aws.amazon.com/cli/latest/reference/deploy/index.html AWS CLI Reference for AWS CodeDeploy>
--
-- -   <https://forums.aws.amazon.com/forum.jspa?forumID=179 AWS CodeDeploy Developer Forum>
module Network.AWS.CodeDeploy
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** DeploymentGroupLimitExceededException
    _DeploymentGroupLimitExceededException,

    -- ** InstanceNameAlreadyRegisteredException
    _InstanceNameAlreadyRegisteredException,

    -- ** DeploymentGroupAlreadyExistsException
    _DeploymentGroupAlreadyExistsException,

    -- ** BucketNameFilterRequiredException
    _BucketNameFilterRequiredException,

    -- ** RevisionDoesNotExistException
    _RevisionDoesNotExistException,

    -- ** DeploymentConfigDoesNotExistException
    _DeploymentConfigDoesNotExistException,

    -- ** InvalidInstanceTypeException
    _InvalidInstanceTypeException,

    -- ** InvalidIamUserArnException
    _InvalidIamUserArnException,

    -- ** InvalidFileExistsBehaviorException
    _InvalidFileExistsBehaviorException,

    -- ** InvalidTagFilterException
    _InvalidTagFilterException,

    -- ** DeploymentTargetDoesNotExistException
    _DeploymentTargetDoesNotExistException,

    -- ** InvalidTriggerConfigException
    _InvalidTriggerConfigException,

    -- ** AlarmsLimitExceededException
    _AlarmsLimitExceededException,

    -- ** DeploymentAlreadyStartedException
    _DeploymentAlreadyStartedException,

    -- ** InvalidKeyPrefixFilterException
    _InvalidKeyPrefixFilterException,

    -- ** ArnNotSupportedException
    _ArnNotSupportedException,

    -- ** OperationNotSupportedException
    _OperationNotSupportedException,

    -- ** InvalidGitHubAccountTokenException
    _InvalidGitHubAccountTokenException,

    -- ** InvalidExternalIdException
    _InvalidExternalIdException,

    -- ** InvalidMinimumHealthyHostValueException
    _InvalidMinimumHealthyHostValueException,

    -- ** UnsupportedActionForDeploymentTypeException
    _UnsupportedActionForDeploymentTypeException,

    -- ** InvalidECSServiceException
    _InvalidECSServiceException,

    -- ** ResourceValidationException
    _ResourceValidationException,

    -- ** InvalidDeploymentInstanceTypeException
    _InvalidDeploymentInstanceTypeException,

    -- ** InvalidGitHubAccountTokenNameException
    _InvalidGitHubAccountTokenNameException,

    -- ** InvalidOperationException
    _InvalidOperationException,

    -- ** ApplicationAlreadyExistsException
    _ApplicationAlreadyExistsException,

    -- ** DeploymentTargetIdRequiredException
    _DeploymentTargetIdRequiredException,

    -- ** InvalidArnException
    _InvalidArnException,

    -- ** TagLimitExceededException
    _TagLimitExceededException,

    -- ** InvalidAutoScalingGroupException
    _InvalidAutoScalingGroupException,

    -- ** ApplicationLimitExceededException
    _ApplicationLimitExceededException,

    -- ** InvalidInputException
    _InvalidInputException,

    -- ** InstanceLimitExceededException
    _InstanceLimitExceededException,

    -- ** IamUserArnAlreadyRegisteredException
    _IamUserArnAlreadyRegisteredException,

    -- ** InstanceNameRequiredException
    _InstanceNameRequiredException,

    -- ** DeploymentLimitExceededException
    _DeploymentLimitExceededException,

    -- ** InvalidTargetFilterNameException
    _InvalidTargetFilterNameException,

    -- ** InvalidTargetInstancesException
    _InvalidTargetInstancesException,

    -- ** InvalidRevisionException
    _InvalidRevisionException,

    -- ** InvalidTagsToAddException
    _InvalidTagsToAddException,

    -- ** InvalidAutoRollbackConfigException
    _InvalidAutoRollbackConfigException,

    -- ** DeploymentIdRequiredException
    _DeploymentIdRequiredException,

    -- ** InvalidRoleException
    _InvalidRoleException,

    -- ** DeploymentConfigAlreadyExistsException
    _DeploymentConfigAlreadyExistsException,

    -- ** InvalidNextTokenException
    _InvalidNextTokenException,

    -- ** InstanceIdRequiredException
    _InstanceIdRequiredException,

    -- ** InvalidBlueGreenDeploymentConfigurationException
    _InvalidBlueGreenDeploymentConfigurationException,

    -- ** DeploymentConfigLimitExceededException
    _DeploymentConfigLimitExceededException,

    -- ** InvalidLoadBalancerInfoException
    _InvalidLoadBalancerInfoException,

    -- ** InvalidSortOrderException
    _InvalidSortOrderException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** InvalidTargetGroupPairException
    _InvalidTargetGroupPairException,

    -- ** DeploymentConfigNameRequiredException
    _DeploymentConfigNameRequiredException,

    -- ** InvalidOnPremisesTagCombinationException
    _InvalidOnPremisesTagCombinationException,

    -- ** InvalidAlarmConfigException
    _InvalidAlarmConfigException,

    -- ** InvalidSortByException
    _InvalidSortByException,

    -- ** InvalidTrafficRoutingConfigurationException
    _InvalidTrafficRoutingConfigurationException,

    -- ** DescriptionTooLongException
    _DescriptionTooLongException,

    -- ** IamUserArnRequiredException
    _IamUserArnRequiredException,

    -- ** IamSessionArnAlreadyRegisteredException
    _IamSessionArnAlreadyRegisteredException,

    -- ** InvalidDeploymentGroupNameException
    _InvalidDeploymentGroupNameException,

    -- ** InvalidBucketNameFilterException
    _InvalidBucketNameFilterException,

    -- ** InvalidTargetException
    _InvalidTargetException,

    -- ** DeploymentGroupNameRequiredException
    _DeploymentGroupNameRequiredException,

    -- ** InvalidTimeRangeException
    _InvalidTimeRangeException,

    -- ** TagRequiredException
    _TagRequiredException,

    -- ** InvalidIgnoreApplicationStopFailuresValueException
    _InvalidIgnoreApplicationStopFailuresValueException,

    -- ** InvalidUpdateOutdatedInstancesOnlyValueException
    _InvalidUpdateOutdatedInstancesOnlyValueException,

    -- ** InvalidTagException
    _InvalidTagException,

    -- ** InvalidDeploymentWaitTypeException
    _InvalidDeploymentWaitTypeException,

    -- ** InvalidComputePlatformException
    _InvalidComputePlatformException,

    -- ** LifecycleHookLimitExceededException
    _LifecycleHookLimitExceededException,

    -- ** InvalidLifecycleEventHookExecutionStatusException
    _InvalidLifecycleEventHookExecutionStatusException,

    -- ** ResourceArnRequiredException
    _ResourceArnRequiredException,

    -- ** InvalidEC2TagCombinationException
    _InvalidEC2TagCombinationException,

    -- ** IamArnRequiredException
    _IamArnRequiredException,

    -- ** GitHubAccountTokenNameRequiredException
    _GitHubAccountTokenNameRequiredException,

    -- ** LifecycleEventAlreadyCompletedException
    _LifecycleEventAlreadyCompletedException,

    -- ** ApplicationDoesNotExistException
    _ApplicationDoesNotExistException,

    -- ** MultipleIamArnsProvidedException
    _MultipleIamArnsProvidedException,

    -- ** InvalidDeploymentTargetIdException
    _InvalidDeploymentTargetIdException,

    -- ** InvalidRegistrationStatusException
    _InvalidRegistrationStatusException,

    -- ** InstanceNotRegisteredException
    _InstanceNotRegisteredException,

    -- ** InvalidDeployedStateFilterException
    _InvalidDeployedStateFilterException,

    -- ** InvalidInstanceStatusException
    _InvalidInstanceStatusException,

    -- ** ApplicationNameRequiredException
    _ApplicationNameRequiredException,

    -- ** InvalidDeploymentStatusException
    _InvalidDeploymentStatusException,

    -- ** TriggerTargetsLimitExceededException
    _TriggerTargetsLimitExceededException,

    -- ** TagSetListLimitExceededException
    _TagSetListLimitExceededException,

    -- ** GitHubAccountTokenDoesNotExistException
    _GitHubAccountTokenDoesNotExistException,

    -- ** InvalidApplicationNameException
    _InvalidApplicationNameException,

    -- ** DeploymentTargetListSizeExceededException
    _DeploymentTargetListSizeExceededException,

    -- ** DeploymentConfigInUseException
    _DeploymentConfigInUseException,

    -- ** InvalidInstanceNameException
    _InvalidInstanceNameException,

    -- ** InvalidIamSessionArnException
    _InvalidIamSessionArnException,

    -- ** InvalidLifecycleEventHookExecutionIdException
    _InvalidLifecycleEventHookExecutionIdException,

    -- ** InvalidEC2TagException
    _InvalidEC2TagException,

    -- ** InvalidDeploymentConfigIdException
    _InvalidDeploymentConfigIdException,

    -- ** InvalidDeploymentStyleException
    _InvalidDeploymentStyleException,

    -- ** RevisionRequiredException
    _RevisionRequiredException,

    -- ** InstanceDoesNotExistException
    _InstanceDoesNotExistException,

    -- ** DeploymentAlreadyCompletedException
    _DeploymentAlreadyCompletedException,

    -- ** ECSServiceMappingLimitExceededException
    _ECSServiceMappingLimitExceededException,

    -- ** DeploymentDoesNotExistException
    _DeploymentDoesNotExistException,

    -- ** BatchLimitExceededException
    _BatchLimitExceededException,

    -- ** InvalidDeploymentIdException
    _InvalidDeploymentIdException,

    -- ** InvalidDeploymentConfigNameException
    _InvalidDeploymentConfigNameException,

    -- ** DeploymentNotStartedException
    _DeploymentNotStartedException,

    -- ** DeploymentIsNotInReadyStateException
    _DeploymentIsNotInReadyStateException,

    -- ** InvalidInstanceIdException
    _InvalidInstanceIdException,

    -- ** DeploymentGroupDoesNotExistException
    _DeploymentGroupDoesNotExistException,

    -- ** RoleRequiredException
    _RoleRequiredException,

    -- * Waiters
    -- $waiters

    -- ** DeploymentSuccessful
    newDeploymentSuccessful,

    -- * Operations
    -- $operations

    -- ** BatchGetOnPremisesInstances
    BatchGetOnPremisesInstances (BatchGetOnPremisesInstances'),
    newBatchGetOnPremisesInstances,
    BatchGetOnPremisesInstancesResponse (BatchGetOnPremisesInstancesResponse'),
    newBatchGetOnPremisesInstancesResponse,

    -- ** GetApplicationRevision
    GetApplicationRevision (GetApplicationRevision'),
    newGetApplicationRevision,
    GetApplicationRevisionResponse (GetApplicationRevisionResponse'),
    newGetApplicationRevisionResponse,

    -- ** BatchGetDeploymentGroups
    BatchGetDeploymentGroups (BatchGetDeploymentGroups'),
    newBatchGetDeploymentGroups,
    BatchGetDeploymentGroupsResponse (BatchGetDeploymentGroupsResponse'),
    newBatchGetDeploymentGroupsResponse,

    -- ** CreateDeploymentConfig
    CreateDeploymentConfig (CreateDeploymentConfig'),
    newCreateDeploymentConfig,
    CreateDeploymentConfigResponse (CreateDeploymentConfigResponse'),
    newCreateDeploymentConfigResponse,

    -- ** StopDeployment
    StopDeployment (StopDeployment'),
    newStopDeployment,
    StopDeploymentResponse (StopDeploymentResponse'),
    newStopDeploymentResponse,

    -- ** ListDeploymentTargets (Paginated)
    ListDeploymentTargets (ListDeploymentTargets'),
    newListDeploymentTargets,
    ListDeploymentTargetsResponse (ListDeploymentTargetsResponse'),
    newListDeploymentTargetsResponse,

    -- ** CreateApplication
    CreateApplication (CreateApplication'),
    newCreateApplication,
    CreateApplicationResponse (CreateApplicationResponse'),
    newCreateApplicationResponse,

    -- ** AddTagsToOnPremisesInstances
    AddTagsToOnPremisesInstances (AddTagsToOnPremisesInstances'),
    newAddTagsToOnPremisesInstances,
    AddTagsToOnPremisesInstancesResponse (AddTagsToOnPremisesInstancesResponse'),
    newAddTagsToOnPremisesInstancesResponse,

    -- ** GetDeploymentTarget
    GetDeploymentTarget (GetDeploymentTarget'),
    newGetDeploymentTarget,
    GetDeploymentTargetResponse (GetDeploymentTargetResponse'),
    newGetDeploymentTargetResponse,

    -- ** DeleteResourcesByExternalId
    DeleteResourcesByExternalId (DeleteResourcesByExternalId'),
    newDeleteResourcesByExternalId,
    DeleteResourcesByExternalIdResponse (DeleteResourcesByExternalIdResponse'),
    newDeleteResourcesByExternalIdResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** BatchGetApplications
    BatchGetApplications (BatchGetApplications'),
    newBatchGetApplications,
    BatchGetApplicationsResponse (BatchGetApplicationsResponse'),
    newBatchGetApplicationsResponse,

    -- ** BatchGetApplicationRevisions
    BatchGetApplicationRevisions (BatchGetApplicationRevisions'),
    newBatchGetApplicationRevisions,
    BatchGetApplicationRevisionsResponse (BatchGetApplicationRevisionsResponse'),
    newBatchGetApplicationRevisionsResponse,

    -- ** ListDeployments (Paginated)
    ListDeployments (ListDeployments'),
    newListDeployments,
    ListDeploymentsResponse (ListDeploymentsResponse'),
    newListDeploymentsResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** ContinueDeployment
    ContinueDeployment (ContinueDeployment'),
    newContinueDeployment,
    ContinueDeploymentResponse (ContinueDeploymentResponse'),
    newContinueDeploymentResponse,

    -- ** GetDeploymentConfig
    GetDeploymentConfig (GetDeploymentConfig'),
    newGetDeploymentConfig,
    GetDeploymentConfigResponse (GetDeploymentConfigResponse'),
    newGetDeploymentConfigResponse,

    -- ** DeleteDeploymentConfig
    DeleteDeploymentConfig (DeleteDeploymentConfig'),
    newDeleteDeploymentConfig,
    DeleteDeploymentConfigResponse (DeleteDeploymentConfigResponse'),
    newDeleteDeploymentConfigResponse,

    -- ** CreateDeploymentGroup
    CreateDeploymentGroup (CreateDeploymentGroup'),
    newCreateDeploymentGroup,
    CreateDeploymentGroupResponse (CreateDeploymentGroupResponse'),
    newCreateDeploymentGroupResponse,

    -- ** ListDeploymentConfigs (Paginated)
    ListDeploymentConfigs (ListDeploymentConfigs'),
    newListDeploymentConfigs,
    ListDeploymentConfigsResponse (ListDeploymentConfigsResponse'),
    newListDeploymentConfigsResponse,

    -- ** DeleteDeploymentGroup
    DeleteDeploymentGroup (DeleteDeploymentGroup'),
    newDeleteDeploymentGroup,
    DeleteDeploymentGroupResponse (DeleteDeploymentGroupResponse'),
    newDeleteDeploymentGroupResponse,

    -- ** ListDeploymentGroups (Paginated)
    ListDeploymentGroups (ListDeploymentGroups'),
    newListDeploymentGroups,
    ListDeploymentGroupsResponse (ListDeploymentGroupsResponse'),
    newListDeploymentGroupsResponse,

    -- ** ListOnPremisesInstances (Paginated)
    ListOnPremisesInstances (ListOnPremisesInstances'),
    newListOnPremisesInstances,
    ListOnPremisesInstancesResponse (ListOnPremisesInstancesResponse'),
    newListOnPremisesInstancesResponse,

    -- ** UpdateDeploymentGroup
    UpdateDeploymentGroup (UpdateDeploymentGroup'),
    newUpdateDeploymentGroup,
    UpdateDeploymentGroupResponse (UpdateDeploymentGroupResponse'),
    newUpdateDeploymentGroupResponse,

    -- ** GetDeployment
    GetDeployment (GetDeployment'),
    newGetDeployment,
    GetDeploymentResponse (GetDeploymentResponse'),
    newGetDeploymentResponse,

    -- ** RegisterOnPremisesInstance
    RegisterOnPremisesInstance (RegisterOnPremisesInstance'),
    newRegisterOnPremisesInstance,
    RegisterOnPremisesInstanceResponse (RegisterOnPremisesInstanceResponse'),
    newRegisterOnPremisesInstanceResponse,

    -- ** RemoveTagsFromOnPremisesInstances
    RemoveTagsFromOnPremisesInstances (RemoveTagsFromOnPremisesInstances'),
    newRemoveTagsFromOnPremisesInstances,
    RemoveTagsFromOnPremisesInstancesResponse (RemoveTagsFromOnPremisesInstancesResponse'),
    newRemoveTagsFromOnPremisesInstancesResponse,

    -- ** GetApplication
    GetApplication (GetApplication'),
    newGetApplication,
    GetApplicationResponse (GetApplicationResponse'),
    newGetApplicationResponse,

    -- ** BatchGetDeploymentTargets
    BatchGetDeploymentTargets (BatchGetDeploymentTargets'),
    newBatchGetDeploymentTargets,
    BatchGetDeploymentTargetsResponse (BatchGetDeploymentTargetsResponse'),
    newBatchGetDeploymentTargetsResponse,

    -- ** ListGitHubAccountTokenNames (Paginated)
    ListGitHubAccountTokenNames (ListGitHubAccountTokenNames'),
    newListGitHubAccountTokenNames,
    ListGitHubAccountTokenNamesResponse (ListGitHubAccountTokenNamesResponse'),
    newListGitHubAccountTokenNamesResponse,

    -- ** DeleteGitHubAccountToken
    DeleteGitHubAccountToken (DeleteGitHubAccountToken'),
    newDeleteGitHubAccountToken,
    DeleteGitHubAccountTokenResponse (DeleteGitHubAccountTokenResponse'),
    newDeleteGitHubAccountTokenResponse,

    -- ** PutLifecycleEventHookExecutionStatus
    PutLifecycleEventHookExecutionStatus (PutLifecycleEventHookExecutionStatus'),
    newPutLifecycleEventHookExecutionStatus,
    PutLifecycleEventHookExecutionStatusResponse (PutLifecycleEventHookExecutionStatusResponse'),
    newPutLifecycleEventHookExecutionStatusResponse,

    -- ** DeregisterOnPremisesInstance
    DeregisterOnPremisesInstance (DeregisterOnPremisesInstance'),
    newDeregisterOnPremisesInstance,
    DeregisterOnPremisesInstanceResponse (DeregisterOnPremisesInstanceResponse'),
    newDeregisterOnPremisesInstanceResponse,

    -- ** DeleteApplication
    DeleteApplication (DeleteApplication'),
    newDeleteApplication,
    DeleteApplicationResponse (DeleteApplicationResponse'),
    newDeleteApplicationResponse,

    -- ** ListApplications (Paginated)
    ListApplications (ListApplications'),
    newListApplications,
    ListApplicationsResponse (ListApplicationsResponse'),
    newListApplicationsResponse,

    -- ** UpdateApplication
    UpdateApplication (UpdateApplication'),
    newUpdateApplication,
    UpdateApplicationResponse (UpdateApplicationResponse'),
    newUpdateApplicationResponse,

    -- ** RegisterApplicationRevision
    RegisterApplicationRevision (RegisterApplicationRevision'),
    newRegisterApplicationRevision,
    RegisterApplicationRevisionResponse (RegisterApplicationRevisionResponse'),
    newRegisterApplicationRevisionResponse,

    -- ** GetOnPremisesInstance
    GetOnPremisesInstance (GetOnPremisesInstance'),
    newGetOnPremisesInstance,
    GetOnPremisesInstanceResponse (GetOnPremisesInstanceResponse'),
    newGetOnPremisesInstanceResponse,

    -- ** ListApplicationRevisions (Paginated)
    ListApplicationRevisions (ListApplicationRevisions'),
    newListApplicationRevisions,
    ListApplicationRevisionsResponse (ListApplicationRevisionsResponse'),
    newListApplicationRevisionsResponse,

    -- ** BatchGetDeployments
    BatchGetDeployments (BatchGetDeployments'),
    newBatchGetDeployments,
    BatchGetDeploymentsResponse (BatchGetDeploymentsResponse'),
    newBatchGetDeploymentsResponse,

    -- ** GetDeploymentGroup
    GetDeploymentGroup (GetDeploymentGroup'),
    newGetDeploymentGroup,
    GetDeploymentGroupResponse (GetDeploymentGroupResponse'),
    newGetDeploymentGroupResponse,

    -- ** CreateDeployment
    CreateDeployment (CreateDeployment'),
    newCreateDeployment,
    CreateDeploymentResponse (CreateDeploymentResponse'),
    newCreateDeploymentResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- * Types

    -- ** ApplicationRevisionSortBy
    ApplicationRevisionSortBy (..),

    -- ** AutoRollbackEvent
    AutoRollbackEvent (..),

    -- ** BundleType
    BundleType (..),

    -- ** ComputePlatform
    ComputePlatform (..),

    -- ** DeployErrorCode
    DeployErrorCode (..),

    -- ** DeploymentCreator
    DeploymentCreator (..),

    -- ** DeploymentOption
    DeploymentOption (..),

    -- ** DeploymentReadyAction
    DeploymentReadyAction (..),

    -- ** DeploymentStatus
    DeploymentStatus (..),

    -- ** DeploymentTargetType
    DeploymentTargetType (..),

    -- ** DeploymentType
    DeploymentType (..),

    -- ** DeploymentWaitType
    DeploymentWaitType (..),

    -- ** EC2TagFilterType
    EC2TagFilterType (..),

    -- ** FileExistsBehavior
    FileExistsBehavior (..),

    -- ** GreenFleetProvisioningAction
    GreenFleetProvisioningAction (..),

    -- ** InstanceAction
    InstanceAction (..),

    -- ** LifecycleErrorCode
    LifecycleErrorCode (..),

    -- ** LifecycleEventStatus
    LifecycleEventStatus (..),

    -- ** ListStateFilterAction
    ListStateFilterAction (..),

    -- ** MinimumHealthyHostsType
    MinimumHealthyHostsType (..),

    -- ** RegistrationStatus
    RegistrationStatus (..),

    -- ** RevisionLocationType
    RevisionLocationType (..),

    -- ** SortOrder
    SortOrder (..),

    -- ** StopStatus
    StopStatus (..),

    -- ** TagFilterType
    TagFilterType (..),

    -- ** TargetFilterName
    TargetFilterName (..),

    -- ** TargetLabel
    TargetLabel (..),

    -- ** TargetStatus
    TargetStatus (..),

    -- ** TrafficRoutingType
    TrafficRoutingType (..),

    -- ** TriggerEventType
    TriggerEventType (..),

    -- ** Alarm
    Alarm (Alarm'),
    newAlarm,

    -- ** AlarmConfiguration
    AlarmConfiguration (AlarmConfiguration'),
    newAlarmConfiguration,

    -- ** AppSpecContent
    AppSpecContent (AppSpecContent'),
    newAppSpecContent,

    -- ** ApplicationInfo
    ApplicationInfo (ApplicationInfo'),
    newApplicationInfo,

    -- ** AutoRollbackConfiguration
    AutoRollbackConfiguration (AutoRollbackConfiguration'),
    newAutoRollbackConfiguration,

    -- ** AutoScalingGroup
    AutoScalingGroup (AutoScalingGroup'),
    newAutoScalingGroup,

    -- ** BlueGreenDeploymentConfiguration
    BlueGreenDeploymentConfiguration (BlueGreenDeploymentConfiguration'),
    newBlueGreenDeploymentConfiguration,

    -- ** BlueInstanceTerminationOption
    BlueInstanceTerminationOption (BlueInstanceTerminationOption'),
    newBlueInstanceTerminationOption,

    -- ** CloudFormationTarget
    CloudFormationTarget (CloudFormationTarget'),
    newCloudFormationTarget,

    -- ** DeploymentConfigInfo
    DeploymentConfigInfo (DeploymentConfigInfo'),
    newDeploymentConfigInfo,

    -- ** DeploymentGroupInfo
    DeploymentGroupInfo (DeploymentGroupInfo'),
    newDeploymentGroupInfo,

    -- ** DeploymentInfo
    DeploymentInfo (DeploymentInfo'),
    newDeploymentInfo,

    -- ** DeploymentOverview
    DeploymentOverview (DeploymentOverview'),
    newDeploymentOverview,

    -- ** DeploymentReadyOption
    DeploymentReadyOption (DeploymentReadyOption'),
    newDeploymentReadyOption,

    -- ** DeploymentStyle
    DeploymentStyle (DeploymentStyle'),
    newDeploymentStyle,

    -- ** DeploymentTarget
    DeploymentTarget (DeploymentTarget'),
    newDeploymentTarget,

    -- ** Diagnostics
    Diagnostics (Diagnostics'),
    newDiagnostics,

    -- ** EC2TagFilter
    EC2TagFilter (EC2TagFilter'),
    newEC2TagFilter,

    -- ** EC2TagSet
    EC2TagSet (EC2TagSet'),
    newEC2TagSet,

    -- ** ECSService
    ECSService (ECSService'),
    newECSService,

    -- ** ECSTarget
    ECSTarget (ECSTarget'),
    newECSTarget,

    -- ** ECSTaskSet
    ECSTaskSet (ECSTaskSet'),
    newECSTaskSet,

    -- ** ELBInfo
    ELBInfo (ELBInfo'),
    newELBInfo,

    -- ** ErrorInformation
    ErrorInformation (ErrorInformation'),
    newErrorInformation,

    -- ** GenericRevisionInfo
    GenericRevisionInfo (GenericRevisionInfo'),
    newGenericRevisionInfo,

    -- ** GitHubLocation
    GitHubLocation (GitHubLocation'),
    newGitHubLocation,

    -- ** GreenFleetProvisioningOption
    GreenFleetProvisioningOption (GreenFleetProvisioningOption'),
    newGreenFleetProvisioningOption,

    -- ** InstanceInfo
    InstanceInfo (InstanceInfo'),
    newInstanceInfo,

    -- ** InstanceTarget
    InstanceTarget (InstanceTarget'),
    newInstanceTarget,

    -- ** LambdaFunctionInfo
    LambdaFunctionInfo (LambdaFunctionInfo'),
    newLambdaFunctionInfo,

    -- ** LambdaTarget
    LambdaTarget (LambdaTarget'),
    newLambdaTarget,

    -- ** LastDeploymentInfo
    LastDeploymentInfo (LastDeploymentInfo'),
    newLastDeploymentInfo,

    -- ** LifecycleEvent
    LifecycleEvent (LifecycleEvent'),
    newLifecycleEvent,

    -- ** LoadBalancerInfo
    LoadBalancerInfo (LoadBalancerInfo'),
    newLoadBalancerInfo,

    -- ** MinimumHealthyHosts
    MinimumHealthyHosts (MinimumHealthyHosts'),
    newMinimumHealthyHosts,

    -- ** OnPremisesTagSet
    OnPremisesTagSet (OnPremisesTagSet'),
    newOnPremisesTagSet,

    -- ** RawString
    RawString (RawString'),
    newRawString,

    -- ** RevisionInfo
    RevisionInfo (RevisionInfo'),
    newRevisionInfo,

    -- ** RevisionLocation
    RevisionLocation (RevisionLocation'),
    newRevisionLocation,

    -- ** RollbackInfo
    RollbackInfo (RollbackInfo'),
    newRollbackInfo,

    -- ** S3Location
    S3Location (S3Location'),
    newS3Location,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** TagFilter
    TagFilter (TagFilter'),
    newTagFilter,

    -- ** TargetGroupInfo
    TargetGroupInfo (TargetGroupInfo'),
    newTargetGroupInfo,

    -- ** TargetGroupPairInfo
    TargetGroupPairInfo (TargetGroupPairInfo'),
    newTargetGroupPairInfo,

    -- ** TargetInstances
    TargetInstances (TargetInstances'),
    newTargetInstances,

    -- ** TimeBasedCanary
    TimeBasedCanary (TimeBasedCanary'),
    newTimeBasedCanary,

    -- ** TimeBasedLinear
    TimeBasedLinear (TimeBasedLinear'),
    newTimeBasedLinear,

    -- ** TimeRange
    TimeRange (TimeRange'),
    newTimeRange,

    -- ** TrafficRoute
    TrafficRoute (TrafficRoute'),
    newTrafficRoute,

    -- ** TrafficRoutingConfig
    TrafficRoutingConfig (TrafficRoutingConfig'),
    newTrafficRoutingConfig,

    -- ** TriggerConfig
    TriggerConfig (TriggerConfig'),
    newTriggerConfig,
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
import Network.AWS.CodeDeploy.Lens
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
