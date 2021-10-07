{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.CodeDeploy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2014-10-06@ of the AWS service descriptions, licensed under Apache 2.0.
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

    -- ** DeploymentGroupAlreadyExistsException
    _DeploymentGroupAlreadyExistsException,

    -- ** BucketNameFilterRequiredException
    _BucketNameFilterRequiredException,

    -- ** InvalidInstanceTypeException
    _InvalidInstanceTypeException,

    -- ** InvalidIamUserArnException
    _InvalidIamUserArnException,

    -- ** RevisionDoesNotExistException
    _RevisionDoesNotExistException,

    -- ** InstanceNameAlreadyRegisteredException
    _InstanceNameAlreadyRegisteredException,

    -- ** DeploymentConfigDoesNotExistException
    _DeploymentConfigDoesNotExistException,

    -- ** InvalidTriggerConfigException
    _InvalidTriggerConfigException,

    -- ** InvalidTagFilterException
    _InvalidTagFilterException,

    -- ** InvalidFileExistsBehaviorException
    _InvalidFileExistsBehaviorException,

    -- ** DeploymentTargetDoesNotExistException
    _DeploymentTargetDoesNotExistException,

    -- ** AlarmsLimitExceededException
    _AlarmsLimitExceededException,

    -- ** InvalidKeyPrefixFilterException
    _InvalidKeyPrefixFilterException,

    -- ** ArnNotSupportedException
    _ArnNotSupportedException,

    -- ** DeploymentAlreadyStartedException
    _DeploymentAlreadyStartedException,

    -- ** OperationNotSupportedException
    _OperationNotSupportedException,

    -- ** InvalidGitHubAccountTokenException
    _InvalidGitHubAccountTokenException,

    -- ** InvalidExternalIdException
    _InvalidExternalIdException,

    -- ** UnsupportedActionForDeploymentTypeException
    _UnsupportedActionForDeploymentTypeException,

    -- ** ResourceValidationException
    _ResourceValidationException,

    -- ** InvalidMinimumHealthyHostValueException
    _InvalidMinimumHealthyHostValueException,

    -- ** InvalidGitHubAccountTokenNameException
    _InvalidGitHubAccountTokenNameException,

    -- ** InvalidECSServiceException
    _InvalidECSServiceException,

    -- ** InvalidDeploymentInstanceTypeException
    _InvalidDeploymentInstanceTypeException,

    -- ** ApplicationLimitExceededException
    _ApplicationLimitExceededException,

    -- ** DeploymentTargetIdRequiredException
    _DeploymentTargetIdRequiredException,

    -- ** InvalidAutoScalingGroupException
    _InvalidAutoScalingGroupException,

    -- ** InvalidOperationException
    _InvalidOperationException,

    -- ** TagLimitExceededException
    _TagLimitExceededException,

    -- ** InvalidArnException
    _InvalidArnException,

    -- ** ApplicationAlreadyExistsException
    _ApplicationAlreadyExistsException,

    -- ** DeploymentLimitExceededException
    _DeploymentLimitExceededException,

    -- ** InvalidInputException
    _InvalidInputException,

    -- ** InstanceNameRequiredException
    _InstanceNameRequiredException,

    -- ** IamUserArnAlreadyRegisteredException
    _IamUserArnAlreadyRegisteredException,

    -- ** InvalidTargetFilterNameException
    _InvalidTargetFilterNameException,

    -- ** InstanceLimitExceededException
    _InstanceLimitExceededException,

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

    -- ** DeploymentConfigNameRequiredException
    _DeploymentConfigNameRequiredException,

    -- ** InvalidBlueGreenDeploymentConfigurationException
    _InvalidBlueGreenDeploymentConfigurationException,

    -- ** InstanceIdRequiredException
    _InstanceIdRequiredException,

    -- ** InvalidSortOrderException
    _InvalidSortOrderException,

    -- ** DeploymentConfigLimitExceededException
    _DeploymentConfigLimitExceededException,

    -- ** DeploymentConfigAlreadyExistsException
    _DeploymentConfigAlreadyExistsException,

    -- ** InvalidLoadBalancerInfoException
    _InvalidLoadBalancerInfoException,

    -- ** InvalidOnPremisesTagCombinationException
    _InvalidOnPremisesTagCombinationException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** InvalidTargetGroupPairException
    _InvalidTargetGroupPairException,

    -- ** InvalidNextTokenException
    _InvalidNextTokenException,

    -- ** DescriptionTooLongException
    _DescriptionTooLongException,

    -- ** IamUserArnRequiredException
    _IamUserArnRequiredException,

    -- ** InvalidTargetException
    _InvalidTargetException,

    -- ** InvalidDeploymentGroupNameException
    _InvalidDeploymentGroupNameException,

    -- ** InvalidSortByException
    _InvalidSortByException,

    -- ** InvalidBucketNameFilterException
    _InvalidBucketNameFilterException,

    -- ** InvalidTrafficRoutingConfigurationException
    _InvalidTrafficRoutingConfigurationException,

    -- ** DeploymentGroupNameRequiredException
    _DeploymentGroupNameRequiredException,

    -- ** InvalidAlarmConfigException
    _InvalidAlarmConfigException,

    -- ** IamSessionArnAlreadyRegisteredException
    _IamSessionArnAlreadyRegisteredException,

    -- ** InvalidComputePlatformException
    _InvalidComputePlatformException,

    -- ** InvalidDeploymentWaitTypeException
    _InvalidDeploymentWaitTypeException,

    -- ** InvalidTagException
    _InvalidTagException,

    -- ** LifecycleHookLimitExceededException
    _LifecycleHookLimitExceededException,

    -- ** InvalidIgnoreApplicationStopFailuresValueException
    _InvalidIgnoreApplicationStopFailuresValueException,

    -- ** InvalidTimeRangeException
    _InvalidTimeRangeException,

    -- ** TagRequiredException
    _TagRequiredException,

    -- ** InvalidUpdateOutdatedInstancesOnlyValueException
    _InvalidUpdateOutdatedInstancesOnlyValueException,

    -- ** InvalidLifecycleEventHookExecutionStatusException
    _InvalidLifecycleEventHookExecutionStatusException,

    -- ** InvalidEC2TagCombinationException
    _InvalidEC2TagCombinationException,

    -- ** ResourceArnRequiredException
    _ResourceArnRequiredException,

    -- ** GitHubAccountTokenNameRequiredException
    _GitHubAccountTokenNameRequiredException,

    -- ** IamArnRequiredException
    _IamArnRequiredException,

    -- ** ApplicationDoesNotExistException
    _ApplicationDoesNotExistException,

    -- ** LifecycleEventAlreadyCompletedException
    _LifecycleEventAlreadyCompletedException,

    -- ** TagSetListLimitExceededException
    _TagSetListLimitExceededException,

    -- ** GitHubAccountTokenDoesNotExistException
    _GitHubAccountTokenDoesNotExistException,

    -- ** InstanceNotRegisteredException
    _InstanceNotRegisteredException,

    -- ** InvalidInstanceStatusException
    _InvalidInstanceStatusException,

    -- ** DeploymentTargetListSizeExceededException
    _DeploymentTargetListSizeExceededException,

    -- ** ApplicationNameRequiredException
    _ApplicationNameRequiredException,

    -- ** InvalidApplicationNameException
    _InvalidApplicationNameException,

    -- ** InvalidDeploymentTargetIdException
    _InvalidDeploymentTargetIdException,

    -- ** InvalidDeploymentStatusException
    _InvalidDeploymentStatusException,

    -- ** TriggerTargetsLimitExceededException
    _TriggerTargetsLimitExceededException,

    -- ** InvalidDeployedStateFilterException
    _InvalidDeployedStateFilterException,

    -- ** MultipleIamArnsProvidedException
    _MultipleIamArnsProvidedException,

    -- ** InvalidRegistrationStatusException
    _InvalidRegistrationStatusException,

    -- ** InvalidEC2TagException
    _InvalidEC2TagException,

    -- ** InvalidInstanceNameException
    _InvalidInstanceNameException,

    -- ** InvalidDeploymentStyleException
    _InvalidDeploymentStyleException,

    -- ** InvalidLifecycleEventHookExecutionIdException
    _InvalidLifecycleEventHookExecutionIdException,

    -- ** DeploymentConfigInUseException
    _DeploymentConfigInUseException,

    -- ** InvalidIamSessionArnException
    _InvalidIamSessionArnException,

    -- ** BatchLimitExceededException
    _BatchLimitExceededException,

    -- ** RevisionRequiredException
    _RevisionRequiredException,

    -- ** ECSServiceMappingLimitExceededException
    _ECSServiceMappingLimitExceededException,

    -- ** DeploymentAlreadyCompletedException
    _DeploymentAlreadyCompletedException,

    -- ** DeploymentDoesNotExistException
    _DeploymentDoesNotExistException,

    -- ** InstanceDoesNotExistException
    _InstanceDoesNotExistException,

    -- ** DeploymentNotStartedException
    _DeploymentNotStartedException,

    -- ** RoleRequiredException
    _RoleRequiredException,

    -- ** InvalidDeploymentIdException
    _InvalidDeploymentIdException,

    -- ** InvalidInstanceIdException
    _InvalidInstanceIdException,

    -- ** InvalidDeploymentConfigNameException
    _InvalidDeploymentConfigNameException,

    -- ** DeploymentGroupDoesNotExistException
    _DeploymentGroupDoesNotExistException,

    -- ** DeploymentIsNotInReadyStateException
    _DeploymentIsNotInReadyStateException,

    -- * Waiters
    -- $waiters

    -- ** DeploymentSuccessful
    newDeploymentSuccessful,

    -- * Operations
    -- $operations

    -- ** CreateDeploymentConfig
    CreateDeploymentConfig (CreateDeploymentConfig'),
    newCreateDeploymentConfig,
    CreateDeploymentConfigResponse (CreateDeploymentConfigResponse'),
    newCreateDeploymentConfigResponse,

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

    -- ** BatchGetOnPremisesInstances
    BatchGetOnPremisesInstances (BatchGetOnPremisesInstances'),
    newBatchGetOnPremisesInstances,
    BatchGetOnPremisesInstancesResponse (BatchGetOnPremisesInstancesResponse'),
    newBatchGetOnPremisesInstancesResponse,

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

    -- ** GetDeploymentTarget
    GetDeploymentTarget (GetDeploymentTarget'),
    newGetDeploymentTarget,
    GetDeploymentTargetResponse (GetDeploymentTargetResponse'),
    newGetDeploymentTargetResponse,

    -- ** AddTagsToOnPremisesInstances
    AddTagsToOnPremisesInstances (AddTagsToOnPremisesInstances'),
    newAddTagsToOnPremisesInstances,
    AddTagsToOnPremisesInstancesResponse (AddTagsToOnPremisesInstancesResponse'),
    newAddTagsToOnPremisesInstancesResponse,

    -- ** CreateApplication
    CreateApplication (CreateApplication'),
    newCreateApplication,
    CreateApplicationResponse (CreateApplicationResponse'),
    newCreateApplicationResponse,

    -- ** DeleteResourcesByExternalId
    DeleteResourcesByExternalId (DeleteResourcesByExternalId'),
    newDeleteResourcesByExternalId,
    DeleteResourcesByExternalIdResponse (DeleteResourcesByExternalIdResponse'),
    newDeleteResourcesByExternalIdResponse,

    -- ** BatchGetApplications
    BatchGetApplications (BatchGetApplications'),
    newBatchGetApplications,
    BatchGetApplicationsResponse (BatchGetApplicationsResponse'),
    newBatchGetApplicationsResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** ContinueDeployment
    ContinueDeployment (ContinueDeployment'),
    newContinueDeployment,
    ContinueDeploymentResponse (ContinueDeploymentResponse'),
    newContinueDeploymentResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

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

    -- ** UpdateDeploymentGroup
    UpdateDeploymentGroup (UpdateDeploymentGroup'),
    newUpdateDeploymentGroup,
    UpdateDeploymentGroupResponse (UpdateDeploymentGroupResponse'),
    newUpdateDeploymentGroupResponse,

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

    -- ** RegisterOnPremisesInstance
    RegisterOnPremisesInstance (RegisterOnPremisesInstance'),
    newRegisterOnPremisesInstance,
    RegisterOnPremisesInstanceResponse (RegisterOnPremisesInstanceResponse'),
    newRegisterOnPremisesInstanceResponse,

    -- ** ListOnPremisesInstances (Paginated)
    ListOnPremisesInstances (ListOnPremisesInstances'),
    newListOnPremisesInstances,
    ListOnPremisesInstancesResponse (ListOnPremisesInstancesResponse'),
    newListOnPremisesInstancesResponse,

    -- ** GetDeployment
    GetDeployment (GetDeployment'),
    newGetDeployment,
    GetDeploymentResponse (GetDeploymentResponse'),
    newGetDeploymentResponse,

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

    -- ** ListGitHubAccountTokenNames (Paginated)
    ListGitHubAccountTokenNames (ListGitHubAccountTokenNames'),
    newListGitHubAccountTokenNames,
    ListGitHubAccountTokenNamesResponse (ListGitHubAccountTokenNamesResponse'),
    newListGitHubAccountTokenNamesResponse,

    -- ** BatchGetDeploymentTargets
    BatchGetDeploymentTargets (BatchGetDeploymentTargets'),
    newBatchGetDeploymentTargets,
    BatchGetDeploymentTargetsResponse (BatchGetDeploymentTargetsResponse'),
    newBatchGetDeploymentTargetsResponse,

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

    -- ** ListApplications (Paginated)
    ListApplications (ListApplications'),
    newListApplications,
    ListApplicationsResponse (ListApplicationsResponse'),
    newListApplicationsResponse,

    -- ** DeleteApplication
    DeleteApplication (DeleteApplication'),
    newDeleteApplication,
    DeleteApplicationResponse (DeleteApplicationResponse'),
    newDeleteApplicationResponse,

    -- ** UpdateApplication
    UpdateApplication (UpdateApplication'),
    newUpdateApplication,
    UpdateApplicationResponse (UpdateApplicationResponse'),
    newUpdateApplicationResponse,

    -- ** BatchGetDeployments
    BatchGetDeployments (BatchGetDeployments'),
    newBatchGetDeployments,
    BatchGetDeploymentsResponse (BatchGetDeploymentsResponse'),
    newBatchGetDeploymentsResponse,

    -- ** RegisterApplicationRevision
    RegisterApplicationRevision (RegisterApplicationRevision'),
    newRegisterApplicationRevision,
    RegisterApplicationRevisionResponse (RegisterApplicationRevisionResponse'),
    newRegisterApplicationRevisionResponse,

    -- ** GetDeploymentGroup
    GetDeploymentGroup (GetDeploymentGroup'),
    newGetDeploymentGroup,
    GetDeploymentGroupResponse (GetDeploymentGroupResponse'),
    newGetDeploymentGroupResponse,

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

    -- ** OutdatedInstancesStrategy
    OutdatedInstancesStrategy (..),

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

    -- ** RelatedDeployments
    RelatedDeployments (RelatedDeployments'),
    newRelatedDeployments,

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
