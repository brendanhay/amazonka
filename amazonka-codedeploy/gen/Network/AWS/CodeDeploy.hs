{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS CodeDeploy __Overview__
--
-- This reference guide provides descriptions of the AWS CodeDeploy APIs.
-- For more information about AWS CodeDeploy, see the
-- <docs.aws.amazon.com/codedeploy/latest/userguide AWS CodeDeploy User Guide>.
--
-- __Using the APIs__
--
-- You can use the AWS CodeDeploy APIs to work with the following:
--
-- -   Applications are unique identifiers used by AWS CodeDeploy to ensure
--     the correct combinations of revisions, deployment configurations,
--     and deployment groups are being referenced during deployments.
--
--     You can use the AWS CodeDeploy APIs to create, delete, get, list,
--     and update applications.
--
-- -   Deployment configurations are sets of deployment rules and success
--     and failure conditions used by AWS CodeDeploy during deployments.
--
--     You can use the AWS CodeDeploy APIs to create, delete, get, and list
--     deployment configurations.
--
-- -   Deployment groups are groups of instances to which application
--     revisions can be deployed.
--
--     You can use the AWS CodeDeploy APIs to create, delete, get, list,
--     and update deployment groups.
--
-- -   Instances represent Amazon EC2 instances to which application
--     revisions are deployed. Instances are identified by their Amazon EC2
--     tags or Auto Scaling group names. Instances belong to deployment
--     groups.
--
--     You can use the AWS CodeDeploy APIs to get and list instance.
--
-- -   Deployments represent the process of deploying revisions to
--     instances.
--
--     You can use the AWS CodeDeploy APIs to create, get, list, and stop
--     deployments.
--
-- -   Application revisions are archive files stored in Amazon S3 buckets
--     or GitHub repositories. These revisions contain source content (such
--     as source code, web pages, executable files, and deployment scripts)
--     along with an application specification (AppSpec) file. (The AppSpec
--     file is unique to AWS CodeDeploy; it defines the deployment actions
--     you want AWS CodeDeploy to execute.) Ffor application revisions
--     stored in Amazon S3 buckets, an application revision is uniquely
--     identified by its Amazon S3 object key and its ETag, version, or
--     both. For application revisions stored in GitHub repositories, an
--     application revision is uniquely identified by its repository name
--     and commit ID. Application revisions are deployed through deployment
--     groups.
--
--     You can use the AWS CodeDeploy APIs to get, list, and register
--     application revisions.
--
module Network.AWS.CodeDeploy
    (
    -- * Service Configuration
      codeDeploy

    -- * Errors
    -- $errors

    -- ** LifecycleHookLimitExceededException
    , _LifecycleHookLimitExceededException

    -- ** InvalidTimeRangeException
    , _InvalidTimeRangeException

    -- ** InvalidTagException
    , _InvalidTagException

    -- ** InstanceNameAlreadyRegisteredException
    , _InstanceNameAlreadyRegisteredException

    -- ** IAMUserARNRequiredException
    , _IAMUserARNRequiredException

    -- ** InvalidDeploymentGroupNameException
    , _InvalidDeploymentGroupNameException

    -- ** DescriptionTooLongException
    , _DescriptionTooLongException

    -- ** InvalidIAMUserARNException
    , _InvalidIAMUserARNException

    -- ** DeploymentNotStartedException
    , _DeploymentNotStartedException

    -- ** DeploymentConfigLimitExceededException
    , _DeploymentConfigLimitExceededException

    -- ** RoleRequiredException
    , _RoleRequiredException

    -- ** InvalidRoleException
    , _InvalidRoleException

    -- ** DeploymentConfigAlreadyExistsException
    , _DeploymentConfigAlreadyExistsException

    -- ** DeploymentLimitExceededException
    , _DeploymentLimitExceededException

    -- ** IAMUserARNAlreadyRegisteredException
    , _IAMUserARNAlreadyRegisteredException

    -- ** InstanceLimitExceededException
    , _InstanceLimitExceededException

    -- ** InvalidDeployedStateFilterException
    , _InvalidDeployedStateFilterException

    -- ** InvalidAutoScalingGroupException
    , _InvalidAutoScalingGroupException

    -- ** InvalidApplicationNameException
    , _InvalidApplicationNameException

    -- ** ApplicationDoesNotExistException
    , _ApplicationDoesNotExistException

    -- ** InvalidMinimumHealthyHostValueException
    , _InvalidMinimumHealthyHostValueException

    -- ** InvalidTagFilterException
    , _InvalidTagFilterException

    -- ** InvalidTriggerConfigException
    , _InvalidTriggerConfigException

    -- ** TagRequiredException
    , _TagRequiredException

    -- ** DeploymentGroupNameRequiredException
    , _DeploymentGroupNameRequiredException

    -- ** BucketNameFilterRequiredException
    , _BucketNameFilterRequiredException

    -- ** DeploymentConfigDoesNotExistException
    , _DeploymentConfigDoesNotExistException

    -- ** InvalidBucketNameFilterException
    , _InvalidBucketNameFilterException

    -- ** DeploymentGroupAlreadyExistsException
    , _DeploymentGroupAlreadyExistsException

    -- ** InvalidSortByException
    , _InvalidSortByException

    -- ** RevisionDoesNotExistException
    , _RevisionDoesNotExistException

    -- ** DeploymentGroupLimitExceededException
    , _DeploymentGroupLimitExceededException

    -- ** DeploymentGroupDoesNotExistException
    , _DeploymentGroupDoesNotExistException

    -- ** InvalidDeploymentConfigNameException
    , _InvalidDeploymentConfigNameException

    -- ** DeploymentConfigNameRequiredException
    , _DeploymentConfigNameRequiredException

    -- ** DeploymentIdRequiredException
    , _DeploymentIdRequiredException

    -- ** InvalidNextTokenException
    , _InvalidNextTokenException

    -- ** InstanceIdRequiredException
    , _InstanceIdRequiredException

    -- ** InvalidDeploymentIdException
    , _InvalidDeploymentIdException

    -- ** InvalidSortOrderException
    , _InvalidSortOrderException

    -- ** DeploymentAlreadyCompletedException
    , _DeploymentAlreadyCompletedException

    -- ** DeploymentDoesNotExistException
    , _DeploymentDoesNotExistException

    -- ** BatchLimitExceededException
    , _BatchLimitExceededException

    -- ** InvalidRevisionException
    , _InvalidRevisionException

    -- ** RevisionRequiredException
    , _RevisionRequiredException

    -- ** InstanceDoesNotExistException
    , _InstanceDoesNotExistException

    -- ** DeploymentConfigInUseException
    , _DeploymentConfigInUseException

    -- ** InvalidEC2TagException
    , _InvalidEC2TagException

    -- ** InvalidInstanceNameException
    , _InvalidInstanceNameException

    -- ** InstanceNameRequiredException
    , _InstanceNameRequiredException

    -- ** TriggerTargetsLimitExceededException
    , _TriggerTargetsLimitExceededException

    -- ** InvalidDeploymentStatusException
    , _InvalidDeploymentStatusException

    -- ** InvalidRegistrationStatusException
    , _InvalidRegistrationStatusException

    -- ** ApplicationNameRequiredException
    , _ApplicationNameRequiredException

    -- ** InstanceNotRegisteredException
    , _InstanceNotRegisteredException

    -- ** ApplicationAlreadyExistsException
    , _ApplicationAlreadyExistsException

    -- ** InvalidInstanceStatusException
    , _InvalidInstanceStatusException

    -- ** TagLimitExceededException
    , _TagLimitExceededException

    -- ** ApplicationLimitExceededException
    , _ApplicationLimitExceededException

    -- ** InvalidOperationException
    , _InvalidOperationException

    -- ** InvalidKeyPrefixFilterException
    , _InvalidKeyPrefixFilterException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** RemoveTagsFromOnPremisesInstances
    , module Network.AWS.CodeDeploy.RemoveTagsFromOnPremisesInstances

    -- ** BatchGetDeploymentGroups
    , module Network.AWS.CodeDeploy.BatchGetDeploymentGroups

    -- ** DeleteDeploymentGroup
    , module Network.AWS.CodeDeploy.DeleteDeploymentGroup

    -- ** UpdateDeploymentGroup
    , module Network.AWS.CodeDeploy.UpdateDeploymentGroup

    -- ** ListOnPremisesInstances
    , module Network.AWS.CodeDeploy.ListOnPremisesInstances

    -- ** CreateDeploymentConfig
    , module Network.AWS.CodeDeploy.CreateDeploymentConfig

    -- ** GetApplicationRevision
    , module Network.AWS.CodeDeploy.GetApplicationRevision

    -- ** GetDeployment
    , module Network.AWS.CodeDeploy.GetDeployment

    -- ** DeleteDeploymentConfig
    , module Network.AWS.CodeDeploy.DeleteDeploymentConfig

    -- ** GetDeploymentConfig
    , module Network.AWS.CodeDeploy.GetDeploymentConfig

    -- ** CreateDeployment
    , module Network.AWS.CodeDeploy.CreateDeployment

    -- ** BatchGetApplicationRevisions
    , module Network.AWS.CodeDeploy.BatchGetApplicationRevisions

    -- ** BatchGetDeployments
    , module Network.AWS.CodeDeploy.BatchGetDeployments

    -- ** GetOnPremisesInstance
    , module Network.AWS.CodeDeploy.GetOnPremisesInstance

    -- ** RegisterApplicationRevision
    , module Network.AWS.CodeDeploy.RegisterApplicationRevision

    -- ** BatchGetApplications
    , module Network.AWS.CodeDeploy.BatchGetApplications

    -- ** DeleteApplication
    , module Network.AWS.CodeDeploy.DeleteApplication

    -- ** UpdateApplication
    , module Network.AWS.CodeDeploy.UpdateApplication

    -- ** GetDeploymentInstance
    , module Network.AWS.CodeDeploy.GetDeploymentInstance

    -- ** DeregisterOnPremisesInstance
    , module Network.AWS.CodeDeploy.DeregisterOnPremisesInstance

    -- ** CreateApplication
    , module Network.AWS.CodeDeploy.CreateApplication

    -- ** StopDeployment
    , module Network.AWS.CodeDeploy.StopDeployment

    -- ** BatchGetDeploymentInstances
    , module Network.AWS.CodeDeploy.BatchGetDeploymentInstances

    -- ** GetApplication
    , module Network.AWS.CodeDeploy.GetApplication

    -- ** ListDeploymentGroups
    , module Network.AWS.CodeDeploy.ListDeploymentGroups

    -- ** BatchGetOnPremisesInstances
    , module Network.AWS.CodeDeploy.BatchGetOnPremisesInstances

    -- ** RegisterOnPremisesInstance
    , module Network.AWS.CodeDeploy.RegisterOnPremisesInstance

    -- ** CreateDeploymentGroup
    , module Network.AWS.CodeDeploy.CreateDeploymentGroup

    -- ** ListDeploymentConfigs
    , module Network.AWS.CodeDeploy.ListDeploymentConfigs

    -- ** GetDeploymentGroup
    , module Network.AWS.CodeDeploy.GetDeploymentGroup

    -- ** ListDeployments
    , module Network.AWS.CodeDeploy.ListDeployments

    -- ** ListApplicationRevisions
    , module Network.AWS.CodeDeploy.ListApplicationRevisions

    -- ** ListApplications
    , module Network.AWS.CodeDeploy.ListApplications

    -- ** AddTagsToOnPremisesInstances
    , module Network.AWS.CodeDeploy.AddTagsToOnPremisesInstances

    -- ** ListDeploymentInstances
    , module Network.AWS.CodeDeploy.ListDeploymentInstances

    -- * Types

    -- ** ApplicationRevisionSortBy
    , ApplicationRevisionSortBy (..)

    -- ** BundleType
    , BundleType (..)

    -- ** DeployErrorCode
    , DeployErrorCode (..)

    -- ** DeploymentCreator
    , DeploymentCreator (..)

    -- ** DeploymentStatus
    , DeploymentStatus (..)

    -- ** EC2TagFilterType
    , EC2TagFilterType (..)

    -- ** InstanceStatus
    , InstanceStatus (..)

    -- ** LifecycleErrorCode
    , LifecycleErrorCode (..)

    -- ** LifecycleEventStatus
    , LifecycleEventStatus (..)

    -- ** ListStateFilterAction
    , ListStateFilterAction (..)

    -- ** MinimumHealthyHostsType
    , MinimumHealthyHostsType (..)

    -- ** RegistrationStatus
    , RegistrationStatus (..)

    -- ** RevisionLocationType
    , RevisionLocationType (..)

    -- ** SortOrder
    , SortOrder (..)

    -- ** StopStatus
    , StopStatus (..)

    -- ** TagFilterType
    , TagFilterType (..)

    -- ** TriggerEventType
    , TriggerEventType (..)

    -- ** ApplicationInfo
    , ApplicationInfo
    , applicationInfo
    , aiLinkedToGitHub
    , aiApplicationId
    , aiApplicationName
    , aiCreateTime

    -- ** AutoScalingGroup
    , AutoScalingGroup
    , autoScalingGroup
    , asgHook
    , asgName

    -- ** DeploymentConfigInfo
    , DeploymentConfigInfo
    , deploymentConfigInfo
    , dciDeploymentConfigName
    , dciMinimumHealthyHosts
    , dciDeploymentConfigId
    , dciCreateTime

    -- ** DeploymentGroupInfo
    , DeploymentGroupInfo
    , deploymentGroupInfo
    , dgiServiceRoleARN
    , dgiDeploymentConfigName
    , dgiTargetRevision
    , dgiEc2TagFilters
    , dgiOnPremisesInstanceTagFilters
    , dgiApplicationName
    , dgiTriggerConfigurations
    , dgiDeploymentGroupId
    , dgiAutoScalingGroups
    , dgiDeploymentGroupName

    -- ** DeploymentInfo
    , DeploymentInfo
    , deploymentInfo
    , diCreator
    , diStatus
    , diDeploymentId
    , diDeploymentConfigName
    , diStartTime
    , diCompleteTime
    , diErrorInformation
    , diDeploymentOverview
    , diApplicationName
    , diRevision
    , diDescription
    , diCreateTime
    , diDeploymentGroupName
    , diIgnoreApplicationStopFailures

    -- ** DeploymentOverview
    , DeploymentOverview
    , deploymentOverview
    , doPending
    , doSkipped
    , doInProgress
    , doSucceeded
    , doFailed

    -- ** Diagnostics
    , Diagnostics
    , diagnostics
    , dLogTail
    , dErrorCode
    , dScriptName
    , dMessage

    -- ** EC2TagFilter
    , EC2TagFilter
    , ec2TagFilter
    , etfValue
    , etfKey
    , etfType

    -- ** ErrorInformation
    , ErrorInformation
    , errorInformation
    , eiCode
    , eiMessage

    -- ** GenericRevisionInfo
    , GenericRevisionInfo
    , genericRevisionInfo
    , griRegisterTime
    , griFirstUsedTime
    , griDeploymentGroups
    , griLastUsedTime
    , griDescription

    -- ** GitHubLocation
    , GitHubLocation
    , gitHubLocation
    , ghlCommitId
    , ghlRepository

    -- ** InstanceInfo
    , InstanceInfo
    , instanceInfo
    , iiRegisterTime
    , iiInstanceARN
    , iiDeregisterTime
    , iiIamUserARN
    , iiInstanceName
    , iiTags

    -- ** InstanceSummary
    , InstanceSummary
    , instanceSummary
    , isInstanceId
    , isStatus
    , isDeploymentId
    , isLastUpdatedAt
    , isLifecycleEvents

    -- ** LifecycleEvent
    , LifecycleEvent
    , lifecycleEvent
    , leStatus
    , leLifecycleEventName
    , leStartTime
    , leDiagnostics
    , leEndTime

    -- ** MinimumHealthyHosts
    , MinimumHealthyHosts
    , minimumHealthyHosts
    , mhhValue
    , mhhType

    -- ** RevisionInfo
    , RevisionInfo
    , revisionInfo
    , riGenericRevisionInfo
    , riRevisionLocation

    -- ** RevisionLocation
    , RevisionLocation
    , revisionLocation
    , rlRevisionType
    , rlS3Location
    , rlGitHubLocation

    -- ** S3Location
    , S3Location
    , s3Location
    , slBundleType
    , slETag
    , slBucket
    , slKey
    , slVersion

    -- ** Tag
    , Tag
    , tag
    , tagValue
    , tagKey

    -- ** TagFilter
    , TagFilter
    , tagFilter
    , tfValue
    , tfKey
    , tfType

    -- ** TimeRange
    , TimeRange
    , timeRange
    , trStart
    , trEnd

    -- ** TriggerConfig
    , TriggerConfig
    , triggerConfig
    , tcTriggerName
    , tcTriggerEvents
    , tcTriggerTargetARN
    ) where

import           Network.AWS.CodeDeploy.AddTagsToOnPremisesInstances
import           Network.AWS.CodeDeploy.BatchGetApplicationRevisions
import           Network.AWS.CodeDeploy.BatchGetApplications
import           Network.AWS.CodeDeploy.BatchGetDeploymentGroups
import           Network.AWS.CodeDeploy.BatchGetDeploymentInstances
import           Network.AWS.CodeDeploy.BatchGetDeployments
import           Network.AWS.CodeDeploy.BatchGetOnPremisesInstances
import           Network.AWS.CodeDeploy.CreateApplication
import           Network.AWS.CodeDeploy.CreateDeployment
import           Network.AWS.CodeDeploy.CreateDeploymentConfig
import           Network.AWS.CodeDeploy.CreateDeploymentGroup
import           Network.AWS.CodeDeploy.DeleteApplication
import           Network.AWS.CodeDeploy.DeleteDeploymentConfig
import           Network.AWS.CodeDeploy.DeleteDeploymentGroup
import           Network.AWS.CodeDeploy.DeregisterOnPremisesInstance
import           Network.AWS.CodeDeploy.GetApplication
import           Network.AWS.CodeDeploy.GetApplicationRevision
import           Network.AWS.CodeDeploy.GetDeployment
import           Network.AWS.CodeDeploy.GetDeploymentConfig
import           Network.AWS.CodeDeploy.GetDeploymentGroup
import           Network.AWS.CodeDeploy.GetDeploymentInstance
import           Network.AWS.CodeDeploy.GetOnPremisesInstance
import           Network.AWS.CodeDeploy.ListApplicationRevisions
import           Network.AWS.CodeDeploy.ListApplications
import           Network.AWS.CodeDeploy.ListDeploymentConfigs
import           Network.AWS.CodeDeploy.ListDeploymentGroups
import           Network.AWS.CodeDeploy.ListDeploymentInstances
import           Network.AWS.CodeDeploy.ListDeployments
import           Network.AWS.CodeDeploy.ListOnPremisesInstances
import           Network.AWS.CodeDeploy.RegisterApplicationRevision
import           Network.AWS.CodeDeploy.RegisterOnPremisesInstance
import           Network.AWS.CodeDeploy.RemoveTagsFromOnPremisesInstances
import           Network.AWS.CodeDeploy.StopDeployment
import           Network.AWS.CodeDeploy.Types
import           Network.AWS.CodeDeploy.UpdateApplication
import           Network.AWS.CodeDeploy.UpdateDeploymentGroup
import           Network.AWS.CodeDeploy.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'CodeDeploy'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
