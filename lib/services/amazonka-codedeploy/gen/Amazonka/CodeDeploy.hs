{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.CodeDeploy
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
module Amazonka.CodeDeploy
  ( -- * Service Configuration
    defaultService,

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
    newDeploymentSuccessful,

    -- * Operations
    -- $operations

    -- ** RemoveTagsFromOnPremisesInstances
    RemoveTagsFromOnPremisesInstances (RemoveTagsFromOnPremisesInstances'),
    newRemoveTagsFromOnPremisesInstances,
    RemoveTagsFromOnPremisesInstancesResponse (RemoveTagsFromOnPremisesInstancesResponse'),
    newRemoveTagsFromOnPremisesInstancesResponse,

    -- ** BatchGetDeploymentGroups
    BatchGetDeploymentGroups (BatchGetDeploymentGroups'),
    newBatchGetDeploymentGroups,
    BatchGetDeploymentGroupsResponse (BatchGetDeploymentGroupsResponse'),
    newBatchGetDeploymentGroupsResponse,

    -- ** DeleteDeploymentGroup
    DeleteDeploymentGroup (DeleteDeploymentGroup'),
    newDeleteDeploymentGroup,
    DeleteDeploymentGroupResponse (DeleteDeploymentGroupResponse'),
    newDeleteDeploymentGroupResponse,

    -- ** UpdateDeploymentGroup
    UpdateDeploymentGroup (UpdateDeploymentGroup'),
    newUpdateDeploymentGroup,
    UpdateDeploymentGroupResponse (UpdateDeploymentGroupResponse'),
    newUpdateDeploymentGroupResponse,

    -- ** ListOnPremisesInstances (Paginated)
    ListOnPremisesInstances (ListOnPremisesInstances'),
    newListOnPremisesInstances,
    ListOnPremisesInstancesResponse (ListOnPremisesInstancesResponse'),
    newListOnPremisesInstancesResponse,

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

    -- ** GetDeployment
    GetDeployment (GetDeployment'),
    newGetDeployment,
    GetDeploymentResponse (GetDeploymentResponse'),
    newGetDeploymentResponse,

    -- ** DeleteDeploymentConfig
    DeleteDeploymentConfig (DeleteDeploymentConfig'),
    newDeleteDeploymentConfig,
    DeleteDeploymentConfigResponse (DeleteDeploymentConfigResponse'),
    newDeleteDeploymentConfigResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** GetDeploymentConfig
    GetDeploymentConfig (GetDeploymentConfig'),
    newGetDeploymentConfig,
    GetDeploymentConfigResponse (GetDeploymentConfigResponse'),
    newGetDeploymentConfigResponse,

    -- ** CreateDeployment
    CreateDeployment (CreateDeployment'),
    newCreateDeployment,
    CreateDeploymentResponse (CreateDeploymentResponse'),
    newCreateDeploymentResponse,

    -- ** BatchGetApplicationRevisions
    BatchGetApplicationRevisions (BatchGetApplicationRevisions'),
    newBatchGetApplicationRevisions,
    BatchGetApplicationRevisionsResponse (BatchGetApplicationRevisionsResponse'),
    newBatchGetApplicationRevisionsResponse,

    -- ** BatchGetDeployments
    BatchGetDeployments (BatchGetDeployments'),
    newBatchGetDeployments,
    BatchGetDeploymentsResponse (BatchGetDeploymentsResponse'),
    newBatchGetDeploymentsResponse,

    -- ** GetOnPremisesInstance
    GetOnPremisesInstance (GetOnPremisesInstance'),
    newGetOnPremisesInstance,
    GetOnPremisesInstanceResponse (GetOnPremisesInstanceResponse'),
    newGetOnPremisesInstanceResponse,

    -- ** RegisterApplicationRevision
    RegisterApplicationRevision (RegisterApplicationRevision'),
    newRegisterApplicationRevision,
    RegisterApplicationRevisionResponse (RegisterApplicationRevisionResponse'),
    newRegisterApplicationRevisionResponse,

    -- ** ContinueDeployment
    ContinueDeployment (ContinueDeployment'),
    newContinueDeployment,
    ContinueDeploymentResponse (ContinueDeploymentResponse'),
    newContinueDeploymentResponse,

    -- ** BatchGetApplications
    BatchGetApplications (BatchGetApplications'),
    newBatchGetApplications,
    BatchGetApplicationsResponse (BatchGetApplicationsResponse'),
    newBatchGetApplicationsResponse,

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

    -- ** DeleteGitHubAccountToken
    DeleteGitHubAccountToken (DeleteGitHubAccountToken'),
    newDeleteGitHubAccountToken,
    DeleteGitHubAccountTokenResponse (DeleteGitHubAccountTokenResponse'),
    newDeleteGitHubAccountTokenResponse,

    -- ** DeregisterOnPremisesInstance
    DeregisterOnPremisesInstance (DeregisterOnPremisesInstance'),
    newDeregisterOnPremisesInstance,
    DeregisterOnPremisesInstanceResponse (DeregisterOnPremisesInstanceResponse'),
    newDeregisterOnPremisesInstanceResponse,

    -- ** PutLifecycleEventHookExecutionStatus
    PutLifecycleEventHookExecutionStatus (PutLifecycleEventHookExecutionStatus'),
    newPutLifecycleEventHookExecutionStatus,
    PutLifecycleEventHookExecutionStatusResponse (PutLifecycleEventHookExecutionStatusResponse'),
    newPutLifecycleEventHookExecutionStatusResponse,

    -- ** GetDeploymentTarget
    GetDeploymentTarget (GetDeploymentTarget'),
    newGetDeploymentTarget,
    GetDeploymentTargetResponse (GetDeploymentTargetResponse'),
    newGetDeploymentTargetResponse,

    -- ** CreateApplication
    CreateApplication (CreateApplication'),
    newCreateApplication,
    CreateApplicationResponse (CreateApplicationResponse'),
    newCreateApplicationResponse,

    -- ** BatchGetDeploymentTargets
    BatchGetDeploymentTargets (BatchGetDeploymentTargets'),
    newBatchGetDeploymentTargets,
    BatchGetDeploymentTargetsResponse (BatchGetDeploymentTargetsResponse'),
    newBatchGetDeploymentTargetsResponse,

    -- ** StopDeployment
    StopDeployment (StopDeployment'),
    newStopDeployment,
    StopDeploymentResponse (StopDeploymentResponse'),
    newStopDeploymentResponse,

    -- ** ListGitHubAccountTokenNames (Paginated)
    ListGitHubAccountTokenNames (ListGitHubAccountTokenNames'),
    newListGitHubAccountTokenNames,
    ListGitHubAccountTokenNamesResponse (ListGitHubAccountTokenNamesResponse'),
    newListGitHubAccountTokenNamesResponse,

    -- ** GetApplication
    GetApplication (GetApplication'),
    newGetApplication,
    GetApplicationResponse (GetApplicationResponse'),
    newGetApplicationResponse,

    -- ** ListDeploymentGroups (Paginated)
    ListDeploymentGroups (ListDeploymentGroups'),
    newListDeploymentGroups,
    ListDeploymentGroupsResponse (ListDeploymentGroupsResponse'),
    newListDeploymentGroupsResponse,

    -- ** BatchGetOnPremisesInstances
    BatchGetOnPremisesInstances (BatchGetOnPremisesInstances'),
    newBatchGetOnPremisesInstances,
    BatchGetOnPremisesInstancesResponse (BatchGetOnPremisesInstancesResponse'),
    newBatchGetOnPremisesInstancesResponse,

    -- ** RegisterOnPremisesInstance
    RegisterOnPremisesInstance (RegisterOnPremisesInstance'),
    newRegisterOnPremisesInstance,
    RegisterOnPremisesInstanceResponse (RegisterOnPremisesInstanceResponse'),
    newRegisterOnPremisesInstanceResponse,

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

    -- ** GetDeploymentGroup
    GetDeploymentGroup (GetDeploymentGroup'),
    newGetDeploymentGroup,
    GetDeploymentGroupResponse (GetDeploymentGroupResponse'),
    newGetDeploymentGroupResponse,

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

    -- ** ListApplicationRevisions (Paginated)
    ListApplicationRevisions (ListApplicationRevisions'),
    newListApplicationRevisions,
    ListApplicationRevisionsResponse (ListApplicationRevisionsResponse'),
    newListApplicationRevisionsResponse,

    -- ** ListApplications (Paginated)
    ListApplications (ListApplications'),
    newListApplications,
    ListApplicationsResponse (ListApplicationsResponse'),
    newListApplicationsResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** DeleteResourcesByExternalId
    DeleteResourcesByExternalId (DeleteResourcesByExternalId'),
    newDeleteResourcesByExternalId,
    DeleteResourcesByExternalIdResponse (DeleteResourcesByExternalIdResponse'),
    newDeleteResourcesByExternalIdResponse,

    -- ** AddTagsToOnPremisesInstances
    AddTagsToOnPremisesInstances (AddTagsToOnPremisesInstances'),
    newAddTagsToOnPremisesInstances,
    AddTagsToOnPremisesInstancesResponse (AddTagsToOnPremisesInstancesResponse'),
    newAddTagsToOnPremisesInstancesResponse,

    -- ** ListDeploymentTargets (Paginated)
    ListDeploymentTargets (ListDeploymentTargets'),
    newListDeploymentTargets,
    ListDeploymentTargetsResponse (ListDeploymentTargetsResponse'),
    newListDeploymentTargetsResponse,

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

import Amazonka.CodeDeploy.AddTagsToOnPremisesInstances
import Amazonka.CodeDeploy.BatchGetApplicationRevisions
import Amazonka.CodeDeploy.BatchGetApplications
import Amazonka.CodeDeploy.BatchGetDeploymentGroups
import Amazonka.CodeDeploy.BatchGetDeploymentTargets
import Amazonka.CodeDeploy.BatchGetDeployments
import Amazonka.CodeDeploy.BatchGetOnPremisesInstances
import Amazonka.CodeDeploy.ContinueDeployment
import Amazonka.CodeDeploy.CreateApplication
import Amazonka.CodeDeploy.CreateDeployment
import Amazonka.CodeDeploy.CreateDeploymentConfig
import Amazonka.CodeDeploy.CreateDeploymentGroup
import Amazonka.CodeDeploy.DeleteApplication
import Amazonka.CodeDeploy.DeleteDeploymentConfig
import Amazonka.CodeDeploy.DeleteDeploymentGroup
import Amazonka.CodeDeploy.DeleteGitHubAccountToken
import Amazonka.CodeDeploy.DeleteResourcesByExternalId
import Amazonka.CodeDeploy.DeregisterOnPremisesInstance
import Amazonka.CodeDeploy.GetApplication
import Amazonka.CodeDeploy.GetApplicationRevision
import Amazonka.CodeDeploy.GetDeployment
import Amazonka.CodeDeploy.GetDeploymentConfig
import Amazonka.CodeDeploy.GetDeploymentGroup
import Amazonka.CodeDeploy.GetDeploymentTarget
import Amazonka.CodeDeploy.GetOnPremisesInstance
import Amazonka.CodeDeploy.Lens
import Amazonka.CodeDeploy.ListApplicationRevisions
import Amazonka.CodeDeploy.ListApplications
import Amazonka.CodeDeploy.ListDeploymentConfigs
import Amazonka.CodeDeploy.ListDeploymentGroups
import Amazonka.CodeDeploy.ListDeploymentTargets
import Amazonka.CodeDeploy.ListDeployments
import Amazonka.CodeDeploy.ListGitHubAccountTokenNames
import Amazonka.CodeDeploy.ListOnPremisesInstances
import Amazonka.CodeDeploy.ListTagsForResource
import Amazonka.CodeDeploy.PutLifecycleEventHookExecutionStatus
import Amazonka.CodeDeploy.RegisterApplicationRevision
import Amazonka.CodeDeploy.RegisterOnPremisesInstance
import Amazonka.CodeDeploy.RemoveTagsFromOnPremisesInstances
import Amazonka.CodeDeploy.StopDeployment
import Amazonka.CodeDeploy.TagResource
import Amazonka.CodeDeploy.Types
import Amazonka.CodeDeploy.UntagResource
import Amazonka.CodeDeploy.UpdateApplication
import Amazonka.CodeDeploy.UpdateDeploymentGroup
import Amazonka.CodeDeploy.Waiters

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
