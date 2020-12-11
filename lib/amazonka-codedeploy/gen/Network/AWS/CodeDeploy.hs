{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
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
    codeDeployService,

    -- * Errors
    -- $errors

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
    Alarm (..),
    mkAlarm,
    aName,

    -- ** AlarmConfiguration
    AlarmConfiguration (..),
    mkAlarmConfiguration,
    acIgnorePollAlarmFailure,
    acEnabled,
    acAlarms,

    -- ** AppSpecContent
    AppSpecContent (..),
    mkAppSpecContent,
    ascContent,
    ascSha256,

    -- ** ApplicationInfo
    ApplicationInfo (..),
    mkApplicationInfo,
    aiLinkedToGitHub,
    aiComputePlatform,
    aiApplicationId,
    aiApplicationName,
    aiGitHubAccountName,
    aiCreateTime,

    -- ** AutoRollbackConfiguration
    AutoRollbackConfiguration (..),
    mkAutoRollbackConfiguration,
    arcEnabled,
    arcEvents,

    -- ** AutoScalingGroup
    AutoScalingGroup (..),
    mkAutoScalingGroup,
    asgHook,
    asgName,

    -- ** BlueGreenDeploymentConfiguration
    BlueGreenDeploymentConfiguration (..),
    mkBlueGreenDeploymentConfiguration,
    bgdcDeploymentReadyOption,
    bgdcGreenFleetProvisioningOption,
    bgdcTerminateBlueInstancesOnDeploymentSuccess,

    -- ** BlueInstanceTerminationOption
    BlueInstanceTerminationOption (..),
    mkBlueInstanceTerminationOption,
    bitoAction,
    bitoTerminationWaitTimeInMinutes,

    -- ** CloudFormationTarget
    CloudFormationTarget (..),
    mkCloudFormationTarget,
    cftTargetId,
    cftStatus,
    cftDeploymentId,
    cftResourceType,
    cftLastUpdatedAt,
    cftLifecycleEvents,
    cftTargetVersionWeight,

    -- ** DeploymentConfigInfo
    DeploymentConfigInfo (..),
    mkDeploymentConfigInfo,
    dciDeploymentConfigName,
    dciComputePlatform,
    dciMinimumHealthyHosts,
    dciTrafficRoutingConfig,
    dciDeploymentConfigId,
    dciCreateTime,

    -- ** DeploymentGroupInfo
    DeploymentGroupInfo (..),
    mkDeploymentGroupInfo,
    dgiServiceRoleARN,
    dgiEc2TagSet,
    dgiDeploymentConfigName,
    dgiLastAttemptedDeployment,
    dgiOnPremisesTagSet,
    dgiComputePlatform,
    dgiTargetRevision,
    dgiEc2TagFilters,
    dgiEcsServices,
    dgiBlueGreenDeploymentConfiguration,
    dgiLoadBalancerInfo,
    dgiOnPremisesInstanceTagFilters,
    dgiLastSuccessfulDeployment,
    dgiApplicationName,
    dgiAlarmConfiguration,
    dgiTriggerConfigurations,
    dgiDeploymentGroupId,
    dgiAutoScalingGroups,
    dgiDeploymentStyle,
    dgiAutoRollbackConfiguration,
    dgiDeploymentGroupName,

    -- ** DeploymentInfo
    DeploymentInfo (..),
    mkDeploymentInfo,
    diCreator,
    diStatus,
    diDeploymentId,
    diDeploymentConfigName,
    diComputePlatform,
    diPreviousRevision,
    diInstanceTerminationWaitTimeStarted,
    diDeploymentStatusMessages,
    diStartTime,
    diCompleteTime,
    diBlueGreenDeploymentConfiguration,
    diErrorInformation,
    diLoadBalancerInfo,
    diAdditionalDeploymentStatusInfo,
    diDeploymentOverview,
    diFileExistsBehavior,
    diApplicationName,
    diRollbackInfo,
    diExternalId,
    diTargetInstances,
    diRevision,
    diDescription,
    diDeploymentStyle,
    diCreateTime,
    diAutoRollbackConfiguration,
    diUpdateOutdatedInstancesOnly,
    diDeploymentGroupName,
    diIgnoreApplicationStopFailures,

    -- ** DeploymentOverview
    DeploymentOverview (..),
    mkDeploymentOverview,
    doPending,
    doSkipped,
    doInProgress,
    doSucceeded,
    doReady,
    doFailed,

    -- ** DeploymentReadyOption
    DeploymentReadyOption (..),
    mkDeploymentReadyOption,
    droActionOnTimeout,
    droWaitTimeInMinutes,

    -- ** DeploymentStyle
    DeploymentStyle (..),
    mkDeploymentStyle,
    dsDeploymentOption,
    dsDeploymentType,

    -- ** DeploymentTarget
    DeploymentTarget (..),
    mkDeploymentTarget,
    dtInstanceTarget,
    dtCloudFormationTarget,
    dtEcsTarget,
    dtDeploymentTargetType,
    dtLambdaTarget,

    -- ** Diagnostics
    Diagnostics (..),
    mkDiagnostics,
    dLogTail,
    dErrorCode,
    dScriptName,
    dMessage,

    -- ** EC2TagFilter
    EC2TagFilter (..),
    mkEC2TagFilter,
    etfValue,
    etfKey,
    etfType,

    -- ** EC2TagSet
    EC2TagSet (..),
    mkEC2TagSet,
    etsEc2TagSetList,

    -- ** ECSService
    ECSService (..),
    mkECSService,
    ecssServiceName,
    ecssClusterName,

    -- ** ECSTarget
    ECSTarget (..),
    mkECSTarget,
    ecstTargetARN,
    ecstTargetId,
    ecstStatus,
    ecstDeploymentId,
    ecstLastUpdatedAt,
    ecstTaskSetsInfo,
    ecstLifecycleEvents,

    -- ** ECSTaskSet
    ECSTaskSet (..),
    mkECSTaskSet,
    ecstsRunningCount,
    ecstsStatus,
    ecstsIdentifer,
    ecstsDesiredCount,
    ecstsPendingCount,
    ecstsTrafficWeight,
    ecstsTargetGroup,
    ecstsTaskSetLabel,

    -- ** ELBInfo
    ELBInfo (..),
    mkELBInfo,
    elbiName,

    -- ** ErrorInformation
    ErrorInformation (..),
    mkErrorInformation,
    eiCode,
    eiMessage,

    -- ** GenericRevisionInfo
    GenericRevisionInfo (..),
    mkGenericRevisionInfo,
    griRegisterTime,
    griFirstUsedTime,
    griDeploymentGroups,
    griLastUsedTime,
    griDescription,

    -- ** GitHubLocation
    GitHubLocation (..),
    mkGitHubLocation,
    ghlCommitId,
    ghlRepository,

    -- ** GreenFleetProvisioningOption
    GreenFleetProvisioningOption (..),
    mkGreenFleetProvisioningOption,
    gfpoAction,

    -- ** InstanceInfo
    InstanceInfo (..),
    mkInstanceInfo,
    iiRegisterTime,
    iiInstanceARN,
    iiDeregisterTime,
    iiIamUserARN,
    iiInstanceName,
    iiIamSessionARN,
    iiTags,

    -- ** InstanceTarget
    InstanceTarget (..),
    mkInstanceTarget,
    itTargetARN,
    itTargetId,
    itStatus,
    itDeploymentId,
    itInstanceLabel,
    itLastUpdatedAt,
    itLifecycleEvents,

    -- ** LambdaFunctionInfo
    LambdaFunctionInfo (..),
    mkLambdaFunctionInfo,
    lfiCurrentVersion,
    lfiFunctionAlias,
    lfiFunctionName,
    lfiTargetVersion,
    lfiTargetVersionWeight,

    -- ** LambdaTarget
    LambdaTarget (..),
    mkLambdaTarget,
    ltTargetARN,
    ltTargetId,
    ltStatus,
    ltDeploymentId,
    ltLastUpdatedAt,
    ltLifecycleEvents,
    ltLambdaFunctionInfo,

    -- ** LastDeploymentInfo
    LastDeploymentInfo (..),
    mkLastDeploymentInfo,
    ldiStatus,
    ldiDeploymentId,
    ldiEndTime,
    ldiCreateTime,

    -- ** LifecycleEvent
    LifecycleEvent (..),
    mkLifecycleEvent,
    leStatus,
    leLifecycleEventName,
    leStartTime,
    leDiagnostics,
    leEndTime,

    -- ** LoadBalancerInfo
    LoadBalancerInfo (..),
    mkLoadBalancerInfo,
    lbiElbInfoList,
    lbiTargetGroupInfoList,
    lbiTargetGroupPairInfoList,

    -- ** MinimumHealthyHosts
    MinimumHealthyHosts (..),
    mkMinimumHealthyHosts,
    mhhValue,
    mhhType,

    -- ** OnPremisesTagSet
    OnPremisesTagSet (..),
    mkOnPremisesTagSet,
    optsOnPremisesTagSetList,

    -- ** RawString
    RawString (..),
    mkRawString,
    rsContent,
    rsSha256,

    -- ** RevisionInfo
    RevisionInfo (..),
    mkRevisionInfo,
    riGenericRevisionInfo,
    riRevisionLocation,

    -- ** RevisionLocation
    RevisionLocation (..),
    mkRevisionLocation,
    rlString,
    rlRevisionType,
    rlS3Location,
    rlAppSpecContent,
    rlGitHubLocation,

    -- ** RollbackInfo
    RollbackInfo (..),
    mkRollbackInfo,
    riRollbackTriggeringDeploymentId,
    riRollbackMessage,
    riRollbackDeploymentId,

    -- ** S3Location
    S3Location (..),
    mkS3Location,
    slBundleType,
    slETag,
    slBucket,
    slKey,
    slVersion,

    -- ** Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- ** TagFilter
    TagFilter (..),
    mkTagFilter,
    tfValue,
    tfKey,
    tfType,

    -- ** TargetGroupInfo
    TargetGroupInfo (..),
    mkTargetGroupInfo,
    tgiName,

    -- ** TargetGroupPairInfo
    TargetGroupPairInfo (..),
    mkTargetGroupPairInfo,
    tgpiProdTrafficRoute,
    tgpiTestTrafficRoute,
    tgpiTargetGroups,

    -- ** TargetInstances
    TargetInstances (..),
    mkTargetInstances,
    tiEc2TagSet,
    tiTagFilters,
    tiAutoScalingGroups,

    -- ** TimeBasedCanary
    TimeBasedCanary (..),
    mkTimeBasedCanary,
    tbcCanaryInterval,
    tbcCanaryPercentage,

    -- ** TimeBasedLinear
    TimeBasedLinear (..),
    mkTimeBasedLinear,
    tblLinearInterval,
    tblLinearPercentage,

    -- ** TimeRange
    TimeRange (..),
    mkTimeRange,
    trStart,
    trEnd,

    -- ** TrafficRoute
    TrafficRoute (..),
    mkTrafficRoute,
    trListenerARNs,

    -- ** TrafficRoutingConfig
    TrafficRoutingConfig (..),
    mkTrafficRoutingConfig,
    trcTimeBasedCanary,
    trcTimeBasedLinear,
    trcType,

    -- ** TriggerConfig
    TriggerConfig (..),
    mkTriggerConfig,
    tcTriggerName,
    tcTriggerEvents,
    tcTriggerTargetARN,

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.Time (..),
    Lude._Time,
    Lude.ISO8601,
    Lude.Timestamp,
    Lude.UTCTime,
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
