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
-- This is the AWS CodeDeploy API Reference. This guide provides
-- descriptions of the AWS CodeDeploy APIs. For additional information, see
-- the
-- <http://docs.aws.amazon.com/codedeploy/latest/userguide AWS CodeDeploy User Guide>.
--
-- __Using the APIs__
--
-- You can use the AWS CodeDeploy APIs to work with the following items:
--
-- -   Applications are unique identifiers that AWS CodeDeploy uses to
--     ensure that the correct combinations of revisions, deployment
--     configurations, and deployment groups are being referenced during
--     deployments.
--
--     You can use the AWS CodeDeploy APIs to create, delete, get, list,
--     and update applications.
--
-- -   Deployment configurations are sets of deployment rules and
--     deployment success and failure conditions that AWS CodeDeploy uses
--     during deployments.
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
--     You can use the AWS CodeDeploy APIs to get and list instances.
--
-- -   Deployments represent the process of deploying revisions to
--     instances.
--
--     You can use the AWS CodeDeploy APIs to create, get, list, and stop
--     deployments.
--
-- -   Application revisions are archive files that are stored in Amazon S3
--     buckets or GitHub repositories. These revisions contain source
--     content (such as source code, web pages, executable files, any
--     deployment scripts, and similar) along with an Application
--     Specification file (AppSpec file). (The AppSpec file is unique to
--     AWS CodeDeploy; it defines a series of deployment actions that you
--     want AWS CodeDeploy to execute.) An application revision is uniquely
--     identified by its Amazon S3 object key and its ETag, version, or
--     both (for application revisions that are stored in Amazon S3
--     buckets) or by its repository name and commit ID (for applications
--     revisions that are stored in GitHub repositories). Application
--     revisions are deployed through deployment groups.
--
--     You can use the AWS CodeDeploy APIs to get, list, and register
--     application revisions.
--
--
-- /See:/ <http://docs.aws.amazon.com/codedeploy/latest/APIReference/Welcome.html AWS API Reference>
module Network.AWS.CodeDeploy
    (
    -- * Service Description
      CodeDeploy

    -- * Error Matchers
    -- $errors

    -- ** InvalidTimeRangeException
    , _InvalidTimeRangeException

    -- ** InvalidTagException
    , _InvalidTagException

    -- ** InstanceNameAlreadyRegisteredException
    , _InstanceNameAlreadyRegisteredException

    -- ** InvalidIAMUserARNException
    , _InvalidIAMUserARNException

    -- ** IAMUserARNRequiredException
    , _IAMUserARNRequiredException

    -- ** InvalidDeploymentGroupNameException
    , _InvalidDeploymentGroupNameException

    -- ** DescriptionTooLongException
    , _DescriptionTooLongException

    -- ** DeploymentConfigAlreadyExistsException
    , _DeploymentConfigAlreadyExistsException

    -- ** DeploymentConfigLimitExceededException
    , _DeploymentConfigLimitExceededException

    -- ** InvalidRoleException
    , _InvalidRoleException

    -- ** DeploymentNotStartedException
    , _DeploymentNotStartedException

    -- ** RoleRequiredException
    , _RoleRequiredException

    -- ** IAMUserARNAlreadyRegisteredException
    , _IAMUserARNAlreadyRegisteredException

    -- ** DeploymentLimitExceededException
    , _DeploymentLimitExceededException

    -- ** InstanceLimitExceededException
    , _InstanceLimitExceededException

    -- ** InvalidAutoScalingGroupException
    , _InvalidAutoScalingGroupException

    -- ** InvalidApplicationNameException
    , _InvalidApplicationNameException

    -- ** InvalidDeployedStateFilterException
    , _InvalidDeployedStateFilterException

    -- ** InvalidMinimumHealthyHostValueException
    , _InvalidMinimumHealthyHostValueException

    -- ** ApplicationDoesNotExistException
    , _ApplicationDoesNotExistException

    -- ** InvalidTagFilterException
    , _InvalidTagFilterException

    -- ** TagRequiredException
    , _TagRequiredException

    -- ** RevisionDoesNotExistException
    , _RevisionDoesNotExistException

    -- ** DeploymentGroupNameRequiredException
    , _DeploymentGroupNameRequiredException

    -- ** InvalidBucketNameFilterException
    , _InvalidBucketNameFilterException

    -- ** DeploymentConfigDoesNotExistException
    , _DeploymentConfigDoesNotExistException

    -- ** InvalidSortByException
    , _InvalidSortByException

    -- ** BucketNameFilterRequiredException
    , _BucketNameFilterRequiredException

    -- ** DeploymentGroupLimitExceededException
    , _DeploymentGroupLimitExceededException

    -- ** DeploymentGroupAlreadyExistsException
    , _DeploymentGroupAlreadyExistsException

    -- ** InvalidDeploymentIdException
    , _InvalidDeploymentIdException

    -- ** DeploymentGroupDoesNotExistException
    , _DeploymentGroupDoesNotExistException

    -- ** DeploymentIdRequiredException
    , _DeploymentIdRequiredException

    -- ** InstanceIdRequiredException
    , _InstanceIdRequiredException

    -- ** DeploymentConfigNameRequiredException
    , _DeploymentConfigNameRequiredException

    -- ** InvalidDeploymentConfigNameException
    , _InvalidDeploymentConfigNameException

    -- ** InvalidSortOrderException
    , _InvalidSortOrderException

    -- ** InvalidNextTokenException
    , _InvalidNextTokenException

    -- ** InvalidRevisionException
    , _InvalidRevisionException

    -- ** DeploymentAlreadyCompletedException
    , _DeploymentAlreadyCompletedException

    -- ** RevisionRequiredException
    , _RevisionRequiredException

    -- ** InstanceDoesNotExistException
    , _InstanceDoesNotExistException

    -- ** DeploymentDoesNotExistException
    , _DeploymentDoesNotExistException

    -- ** InstanceNameRequiredException
    , _InstanceNameRequiredException

    -- ** DeploymentConfigInUseException
    , _DeploymentConfigInUseException

    -- ** InvalidEC2TagException
    , _InvalidEC2TagException

    -- ** InvalidInstanceNameException
    , _InvalidInstanceNameException

    -- ** InvalidDeploymentStatusException
    , _InvalidDeploymentStatusException

    -- ** InvalidRegistrationStatusException
    , _InvalidRegistrationStatusException

    -- ** TagLimitExceededException
    , _TagLimitExceededException

    -- ** InstanceNotRegisteredException
    , _InstanceNotRegisteredException

    -- ** ApplicationLimitExceededException
    , _ApplicationLimitExceededException

    -- ** InvalidOperationException
    , _InvalidOperationException

    -- ** ApplicationAlreadyExistsException
    , _ApplicationAlreadyExistsException

    -- ** InvalidInstanceStatusException
    , _InvalidInstanceStatusException

    -- ** ApplicationNameRequiredException
    , _ApplicationNameRequiredException

    -- ** InvalidKeyPrefixFilterException
    , _InvalidKeyPrefixFilterException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** RemoveTagsFromOnPremisesInstances 
    , module Network.AWS.CodeDeploy.RemoveTagsFromOnPremisesInstances

    -- ** GetDeployment 
    , module Network.AWS.CodeDeploy.GetDeployment

    -- ** CreateDeploymentConfig 
    , module Network.AWS.CodeDeploy.CreateDeploymentConfig

    -- ** UpdateDeploymentGroup 
    , module Network.AWS.CodeDeploy.UpdateDeploymentGroup

    -- ** DeleteDeploymentGroup 
    , module Network.AWS.CodeDeploy.DeleteDeploymentGroup

    -- ** ListOnPremisesInstances 
    , module Network.AWS.CodeDeploy.ListOnPremisesInstances

    -- ** GetApplicationRevision 
    , module Network.AWS.CodeDeploy.GetApplicationRevision

    -- ** DeleteDeploymentConfig 
    , module Network.AWS.CodeDeploy.DeleteDeploymentConfig

    -- ** GetDeploymentConfig 
    , module Network.AWS.CodeDeploy.GetDeploymentConfig

    -- ** CreateDeployment 
    , module Network.AWS.CodeDeploy.CreateDeployment

    -- ** GetOnPremisesInstance 
    , module Network.AWS.CodeDeploy.GetOnPremisesInstance

    -- ** BatchGetDeployments 
    , module Network.AWS.CodeDeploy.BatchGetDeployments

    -- ** RegisterApplicationRevision 
    , module Network.AWS.CodeDeploy.RegisterApplicationRevision

    -- ** DeleteApplication 
    , module Network.AWS.CodeDeploy.DeleteApplication

    -- ** UpdateApplication 
    , module Network.AWS.CodeDeploy.UpdateApplication

    -- ** BatchGetApplications 
    , module Network.AWS.CodeDeploy.BatchGetApplications

    -- ** CreateApplication 
    , module Network.AWS.CodeDeploy.CreateApplication

    -- ** DeregisterOnPremisesInstance 
    , module Network.AWS.CodeDeploy.DeregisterOnPremisesInstance

    -- ** GetDeploymentInstance 
    , module Network.AWS.CodeDeploy.GetDeploymentInstance

    -- ** StopDeployment 
    , module Network.AWS.CodeDeploy.StopDeployment

    -- ** GetApplication 
    , module Network.AWS.CodeDeploy.GetApplication

    -- ** ListDeploymentGroups 
    , module Network.AWS.CodeDeploy.ListDeploymentGroups

    -- ** RegisterOnPremisesInstance 
    , module Network.AWS.CodeDeploy.RegisterOnPremisesInstance

    -- ** BatchGetOnPremisesInstances 
    , module Network.AWS.CodeDeploy.BatchGetOnPremisesInstances

    -- ** ListDeploymentConfigs 
    , module Network.AWS.CodeDeploy.ListDeploymentConfigs

    -- ** CreateDeploymentGroup 
    , module Network.AWS.CodeDeploy.CreateDeploymentGroup

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
    , dgiDeploymentGroupId
    , dgiAutoScalingGroups
    , dgiDeploymentGroupName

    -- ** DeploymentInfo
    , DeploymentInfo
    , deploymentInfo
    , diDeploymentId
    , diCreator
    , diStatus
    , diDeploymentConfigName
    , diStartTime
    , diCompleteTime
    , diErrorInformation
    , diDeploymentOverview
    , diApplicationName
    , diRevision
    , diDescription
    , diIgnoreApplicationStopFailures
    , diDeploymentGroupName
    , diCreateTime

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
    , iiInstanceARN
    , iiRegisterTime
    , iiDeregisterTime
    , iiIamUserARN
    , iiInstanceName
    , iiTags

    -- ** InstanceSummary
    , InstanceSummary
    , instanceSummary
    , isInstanceId
    , isDeploymentId
    , isStatus
    , isLastUpdatedAt
    , isLifecycleEvents

    -- ** LifecycleEvent
    , LifecycleEvent
    , lifecycleEvent
    , leStatus
    , leStartTime
    , leLifecycleEventName
    , leDiagnostics
    , leEndTime

    -- ** MinimumHealthyHosts
    , MinimumHealthyHosts
    , minimumHealthyHosts
    , mhhValue
    , mhhType

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
    ) where

import Network.AWS.CodeDeploy.AddTagsToOnPremisesInstances
import Network.AWS.CodeDeploy.BatchGetApplications
import Network.AWS.CodeDeploy.BatchGetDeployments
import Network.AWS.CodeDeploy.BatchGetOnPremisesInstances
import Network.AWS.CodeDeploy.CreateApplication
import Network.AWS.CodeDeploy.CreateDeployment
import Network.AWS.CodeDeploy.CreateDeploymentConfig
import Network.AWS.CodeDeploy.CreateDeploymentGroup
import Network.AWS.CodeDeploy.DeleteApplication
import Network.AWS.CodeDeploy.DeleteDeploymentConfig
import Network.AWS.CodeDeploy.DeleteDeploymentGroup
import Network.AWS.CodeDeploy.DeregisterOnPremisesInstance
import Network.AWS.CodeDeploy.GetApplication
import Network.AWS.CodeDeploy.GetApplicationRevision
import Network.AWS.CodeDeploy.GetDeployment
import Network.AWS.CodeDeploy.GetDeploymentConfig
import Network.AWS.CodeDeploy.GetDeploymentGroup
import Network.AWS.CodeDeploy.GetDeploymentInstance
import Network.AWS.CodeDeploy.GetOnPremisesInstance
import Network.AWS.CodeDeploy.ListApplicationRevisions
import Network.AWS.CodeDeploy.ListApplications
import Network.AWS.CodeDeploy.ListDeploymentConfigs
import Network.AWS.CodeDeploy.ListDeploymentGroups
import Network.AWS.CodeDeploy.ListDeploymentInstances
import Network.AWS.CodeDeploy.ListDeployments
import Network.AWS.CodeDeploy.ListOnPremisesInstances
import Network.AWS.CodeDeploy.RegisterApplicationRevision
import Network.AWS.CodeDeploy.RegisterOnPremisesInstance
import Network.AWS.CodeDeploy.RemoveTagsFromOnPremisesInstances
import Network.AWS.CodeDeploy.StopDeployment
import Network.AWS.CodeDeploy.Types
import Network.AWS.CodeDeploy.UpdateApplication
import Network.AWS.CodeDeploy.UpdateDeploymentGroup
import Network.AWS.CodeDeploy.Waiters

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

{- $pager
This operation can return paginated results.
-}
