{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- Module      : Network.AWS.AutoScaling.V2011_01_01.Trans
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Auto Scaling allows you to scale your Amazon EC2 capacity up or down
-- automatically according to conditions you define. With Auto Scaling, you
-- can ensure that the number of Amazon EC2 instances you’re using increases
-- seamlessly during demand spikes to maintain performance, and decreases
-- automatically during demand lulls to minimize costs. Auto Scaling is
-- particularly well suited for applications that experience hourly, daily, or
-- weekly variability in usage. Auto Scaling is enabled by Amazon CloudWatch
-- and available at no additional charge beyond Amazon CloudWatch fees.
--
-- The 'State' operator variants from 'Control.Lens' such as '.=' can be
-- used to modify any additional request parameters before sending.
module Network.AWS.AutoScaling.V2011_01_01.Trans
    (
    -- * AttachInstances
      attachInstances
    -- * CompleteLifecycleAction
    , completeLifecycleAction
    -- * CreateAutoScalingGroup
    , createAutoScalingGroup
    -- * CreateLaunchConfiguration
    , createLaunchConfiguration
    -- * CreateOrUpdateTags
    , createOrUpdateTags
    -- * DeleteAutoScalingGroup
    , deleteAutoScalingGroup
    -- * DeleteLaunchConfiguration
    , deleteLaunchConfiguration
    -- * DeleteLifecycleHook
    , deleteLifecycleHook
    -- * DeleteNotificationConfiguration
    , deleteNotificationConfiguration
    -- * DeletePolicy
    , deletePolicy
    -- * DeleteScheduledAction
    , deleteScheduledAction
    -- * DeleteTags
    , deleteTags
    -- * DescribeAccountLimits
    , describeAccountLimits
    -- * DescribeAdjustmentTypes
    , describeAdjustmentTypes
    -- * DescribeAutoScalingGroups
    , describeAutoScalingGroups
    -- * DescribeAutoScalingInstances
    , describeAutoScalingInstances
    -- * DescribeAutoScalingNotificationTypes
    , describeAutoScalingNotificationTypes
    -- * DescribeLaunchConfigurations
    , describeLaunchConfigurations
    -- * DescribeLifecycleHookTypes
    , describeLifecycleHookTypes
    -- * DescribeLifecycleHooks
    , describeLifecycleHooks
    -- * DescribeMetricCollectionTypes
    , describeMetricCollectionTypes
    -- * DescribeNotificationConfigurations
    , describeNotificationConfigurations
    -- * DescribePolicies
    , describePolicies
    -- * DescribeScalingActivities
    , describeScalingActivities
    -- * DescribeScalingProcessTypes
    , describeScalingProcessTypes
    -- * DescribeScheduledActions
    , describeScheduledActions
    -- * DescribeTags
    , describeTags
    -- * DescribeTerminationPolicyTypes
    , describeTerminationPolicyTypes
    -- * DetachInstances
    , detachInstances
    -- * DisableMetricsCollection
    , disableMetricsCollection
    -- * EnableMetricsCollection
    , enableMetricsCollection
    -- * EnterStandby
    , enterStandby
    -- * ExecutePolicy
    , executePolicy
    -- * ExitStandby
    , exitStandby
    -- * PutLifecycleHook
    , putLifecycleHook
    -- * PutNotificationConfiguration
    , putNotificationConfiguration
    -- * PutScalingPolicy
    , putScalingPolicy
    -- * PutScheduledUpdateGroupAction
    , putScheduledUpdateGroupAction
    -- * RecordLifecycleActionHeartbeat
    , recordLifecycleActionHeartbeat
    -- * ResumeProcesses
    , resumeProcesses
    -- * SetDesiredCapacity
    , setDesiredCapacity
    -- * SetInstanceHealth
    , setInstanceHealth
    -- * SuspendProcesses
    , suspendProcesses
    -- * TerminateInstanceInAutoScalingGroup
    , terminateInstanceInAutoScalingGroup
    -- * UpdateAutoScalingGroup
    , updateAutoScalingGroup

    -- * Re-exported
    , module Control.Monad.Trans.AWS
    , module Network.AWS.AutoScaling.V2011_01_01
    -- ** Lenses
    , (.=)
    , (?=)
    , (<>=)
    , (%=)
    ) where

import Control.Monad.Trans.AWS as AWS
import Network.AWS.Prelude
import Network.AWS.AutoScaling.V2011_01_01

-- | Attaches one or more Amazon EC2 instances to an existing Auto Scaling
-- group. After the instance(s) is attached, it becomes a part of the Auto
-- Scaling group. For more information, see Attach Amazon EC2 Instances to
-- Your Existing Auto Scaling Group in the Auto Scaling Developer Guide.
--
-- See: 'Network.AWS.AutoScaling.V2011_01_01.AttachInstances'
attachInstances :: ( MonadCatch m
                   , MonadResource m
                   , MonadError AWS.Error m
                   , MonadReader Env m
                   , AWSRequest a
                   )
                => List1 Text -- ^ 'aiInstanceIds'
                -> Text -- ^ 'aiAutoScalingGroupName'
                -> State AttachInstances a
                -> m AttachInstancesResponse
attachInstances p1 p2 s =
    send $ (mkAttachInstances p1 p2) &~ s

-- | Completes the lifecycle action for the associated token initiated under the
-- given lifecycle hook with the specified result. This operation is a part of
-- the basic sequence for adding a lifecycle hook to an Auto Scaling group:
-- Create a notification target. A target can be either an Amazon SQS queue or
-- an Amazon SNS topic. Create an IAM role. This role allows Auto Scaling to
-- publish lifecycle notifications to the designated SQS queue or SNS topic.
-- Create the lifecycle hook. You can create a hook that acts when instances
-- launch or when instances terminate. If necessary, record the lifecycle
-- action heartbeat to keep the instance in a pending state. Complete the
-- lifecycle action. To learn more, see Auto Scaling Pending State and Auto
-- Scaling Terminating State.
--
-- See: 'Network.AWS.AutoScaling.V2011_01_01.CompleteLifecycleAction'
completeLifecycleAction :: ( MonadCatch m
                           , MonadResource m
                           , MonadError AWS.Error m
                           , MonadReader Env m
                           , AWSRequest a
                           )
                        => Text -- ^ 'claLifecycleHookName'
                        -> Text -- ^ 'claAutoScalingGroupName'
                        -> Text -- ^ 'claLifecycleActionToken'
                        -> Text -- ^ 'claLifecycleActionResult'
                        -> State CompleteLifecycleAction a
                        -> m CompleteLifecycleActionResponse
completeLifecycleAction p1 p2 p3 p4 s =
    send $ (mkCompleteLifecycleAction p1 p2 p3 p4) &~ s

-- | Creates a new Auto Scaling group with the specified name and other
-- attributes. When the creation request is completed, the Auto Scaling group
-- is ready to be used in other calls. The Auto Scaling group name must be
-- unique within the scope of your AWS account.
-- https://autoscaling.amazonaws.com/?AutoScalingGroupName=my-test-asg
-- &AvailabilityZones.member.1=us-east-1a
-- &AvailabilityZones.member.2=us-east-1b &MinSize=2 &MaxSize=10
-- &DesiredCapacity=2 &LoadBalancerNames.member.1=my-test-asg-loadbalancer
-- &HealthCheckType=ELB &HealthCheckGracePeriod=120
-- &LaunchConfigurationName=my-test-lc &Version=2011-01-01
-- &Action=CreateAutoScalingGroup &AUTHPARAMS
-- 8d798a29-f083-11e1-bdfb-cb223EXAMPLE.
--
-- See: 'Network.AWS.AutoScaling.V2011_01_01.CreateAutoScalingGroup'
createAutoScalingGroup :: ( MonadCatch m
                          , MonadResource m
                          , MonadError AWS.Error m
                          , MonadReader Env m
                          , AWSRequest a
                          )
                       => Text -- ^ 'casgAutoScalingGroupName'
                       -> Integer -- ^ 'casgMinSize'
                       -> Integer -- ^ 'casgMaxSize'
                       -> State CreateAutoScalingGroup a
                       -> m CreateAutoScalingGroupResponse
createAutoScalingGroup p1 p4 p5 s =
    send $ (mkCreateAutoScalingGroup p1 p4 p5) &~ s

-- | Creates a new launch configuration. The launch configuration name must be
-- unique within the scope of the client's AWS account. The maximum limit of
-- launch configurations, which by default is 100, must not yet have been met;
-- otherwise, the call will fail. When created, the new launch configuration
-- is available for immediate use.
-- https://autoscaling.amazonaws.com/?LaunchConfigurationName=my-test-lc
-- &AssociatePublicIpAddress=true &PlacementTenancy=dedicated
-- &ImageId=ami-0078da69 &InstanceType=m1.small
-- &Action=CreateLaunchConfiguration &AUTHPARAMS
-- 7c6e177f-f082-11e1-ac58-3714bEXAMPLE.
--
-- See: 'Network.AWS.AutoScaling.V2011_01_01.CreateLaunchConfiguration'
createLaunchConfiguration :: ( MonadCatch m
                             , MonadResource m
                             , MonadError AWS.Error m
                             , MonadReader Env m
                             , AWSRequest a
                             )
                          => Text -- ^ 'clcLaunchConfigurationName'
                          -> State CreateLaunchConfiguration a
                          -> m CreateLaunchConfigurationResponse
createLaunchConfiguration p1 s =
    send $ (mkCreateLaunchConfiguration p1) &~ s

-- | Creates new tags or updates existing tags for an Auto Scaling group. A
-- tag's definition is composed of a resource ID, resource type, key and
-- value, and the propagate flag. Value and the propagate flag are optional
-- parameters. See the Request Parameters for more information. For
-- information on creating tags for your Auto Scaling group, see Tag Your Auto
-- Scaling Groups and Amazon EC2 Instances.
-- https://autoscaling.amazonaws.com/?Tags.member.1.ResourceId=my-test-asg
-- &Tags.member.1.ResourceType=auto-scaling-group &Tags.member.1.Key=version
-- &Tags.member.1.Value=1.0 &Tags.member.1.PropagateAtLaunch=true
-- &Version=2011-01-01 &Action=CreateOrUpdateTags &AUTHPARAMS
-- b0203919-bf1b-11e2-8a01-13263EXAMPLE.
--
-- See: 'Network.AWS.AutoScaling.V2011_01_01.CreateOrUpdateTags'
createOrUpdateTags :: ( MonadCatch m
                      , MonadResource m
                      , MonadError AWS.Error m
                      , MonadReader Env m
                      , AWSRequest a
                      )
                   => [Tag] -- ^ 'coutTags'
                   -> State CreateOrUpdateTags a
                   -> m CreateOrUpdateTagsResponse
createOrUpdateTags p1 s =
    send $ (mkCreateOrUpdateTags p1) &~ s

-- | Deletes the specified Auto Scaling group if the group has no instances and
-- no scaling activities in progress. To remove all instances before calling
-- DeleteAutoScalingGroup, you can call UpdateAutoScalingGroup to set the
-- minimum and maximum size of the AutoScalingGroup to zero.
-- https://autoscaling.amazonaws.com/?AutoScalingGroupName=my-test-asg
-- &ForceDelete=true &Version=2011-01-01 &Action=DeleteAutoScalingGroup
-- &AUTHPARAMS 70a76d42-9665-11e2-9fdf-211deEXAMPLE.
--
-- See: 'Network.AWS.AutoScaling.V2011_01_01.DeleteAutoScalingGroup'
deleteAutoScalingGroup :: ( MonadCatch m
                          , MonadResource m
                          , MonadError AWS.Error m
                          , MonadReader Env m
                          , AWSRequest a
                          )
                       => Text -- ^ 'dasgAutoScalingGroupName'
                       -> State DeleteAutoScalingGroup a
                       -> m DeleteAutoScalingGroupResponse
deleteAutoScalingGroup p1 s =
    send $ (mkDeleteAutoScalingGroup p1) &~ s

-- | Deletes the specified LaunchConfiguration. The specified launch
-- configuration must not be attached to an Auto Scaling group. When this call
-- completes, the launch configuration is no longer available for use.
-- https://autoscaling.amazonaws.com/?LaunchConfigurationName=my-test-lc
-- &Version=2011-01-01 &Action=DeleteLaunchConfiguration &AUTHPARAMS
-- 7347261f-97df-11e2-8756-35eEXAMPLE.
--
-- See: 'Network.AWS.AutoScaling.V2011_01_01.DeleteLaunchConfiguration'
deleteLaunchConfiguration :: ( MonadCatch m
                             , MonadResource m
                             , MonadError AWS.Error m
                             , MonadReader Env m
                             , AWSRequest a
                             )
                          => Text -- ^ 'dlcLaunchConfigurationName'
                          -> State DeleteLaunchConfiguration a
                          -> m DeleteLaunchConfigurationResponse
deleteLaunchConfiguration p1 s =
    send $ (mkDeleteLaunchConfiguration p1) &~ s

-- | Deletes the specified lifecycle hook. If there are any outstanding
-- lifecycle actions, they are completed first (ABANDON for launching
-- instances, CONTINUE for terminating instances).
--
-- See: 'Network.AWS.AutoScaling.V2011_01_01.DeleteLifecycleHook'
deleteLifecycleHook :: ( MonadCatch m
                       , MonadResource m
                       , MonadError AWS.Error m
                       , MonadReader Env m
                       , AWSRequest a
                       )
                    => Text -- ^ 'dlhLifecycleHookName'
                    -> Text -- ^ 'dlhAutoScalingGroupName'
                    -> State DeleteLifecycleHook a
                    -> m DeleteLifecycleHookResponse
deleteLifecycleHook p1 p2 s =
    send $ (mkDeleteLifecycleHook p1 p2) &~ s

-- | Deletes notifications created by PutNotificationConfiguration.
--
-- See: 'Network.AWS.AutoScaling.V2011_01_01.DeleteNotificationConfiguration'
deleteNotificationConfiguration :: ( MonadCatch m
                                   , MonadResource m
                                   , MonadError AWS.Error m
                                   , MonadReader Env m
                                   , AWSRequest a
                                   )
                                => Text -- ^ 'dncAutoScalingGroupName'
                                -> Text -- ^ 'dncTopicARN'
                                -> State DeleteNotificationConfiguration a
                                -> m DeleteNotificationConfigurationResponse
deleteNotificationConfiguration p1 p2 s =
    send $ (mkDeleteNotificationConfiguration p1 p2) &~ s

-- | Deletes a policy created by PutScalingPolicy.
--
-- See: 'Network.AWS.AutoScaling.V2011_01_01.DeletePolicy'
deletePolicy :: ( MonadCatch m
                , MonadResource m
                , MonadError AWS.Error m
                , MonadReader Env m
                , AWSRequest a
                )
             => Text -- ^ 'dpPolicyName'
             -> State DeletePolicy a
             -> m DeletePolicyResponse
deletePolicy p2 s =
    send $ (mkDeletePolicy p2) &~ s

-- | Deletes a scheduled action previously created using the
-- PutScheduledUpdateGroupAction.
--
-- See: 'Network.AWS.AutoScaling.V2011_01_01.DeleteScheduledAction'
deleteScheduledAction :: ( MonadCatch m
                         , MonadResource m
                         , MonadError AWS.Error m
                         , MonadReader Env m
                         , AWSRequest a
                         )
                      => Text -- ^ 'dsaScheduledActionName'
                      -> State DeleteScheduledAction a
                      -> m DeleteScheduledActionResponse
deleteScheduledAction p2 s =
    send $ (mkDeleteScheduledAction p2) &~ s

-- | Removes the specified tags or a set of tags from a set of resources.
--
-- See: 'Network.AWS.AutoScaling.V2011_01_01.DeleteTags'
deleteTags :: ( MonadCatch m
              , MonadResource m
              , MonadError AWS.Error m
              , MonadReader Env m
              , AWSRequest a
              )
           => [Tag] -- ^ 'dtTags'
           -> State DeleteTags a
           -> m DeleteTagsResponse
deleteTags p1 s =
    send $ (mkDeleteTags p1) &~ s

-- | Returns the limits for the Auto Scaling resources currently allowed for
-- your AWS account. Your AWS account comes with default limits on resources
-- for Auto Scaling. There is a default limit of 20 Auto Scaling groups and
-- 100 launch configurations per region. If you reach the limits for the
-- number of Auto Scaling groups or the launch configurations, you can go to
-- the Support Center and place a request to raise the limits.
-- https://autoscaling.amazonaws.com/?Version=2011-01-01
-- &Action=DescribeAccountLimits &AUTHPARAMS 100 20
-- a32bd184-519d-11e3-a8a4-c1c467cbcc3b.
--
-- See: 'Network.AWS.AutoScaling.V2011_01_01.DescribeAccountLimits'
describeAccountLimits :: ( MonadCatch m
                         , MonadResource m
                         , MonadError AWS.Error m
                         , MonadReader Env m
                         , AWSRequest a
                         )
                      => State DescribeAccountLimits a
                      -> m DescribeAccountLimitsResponse
describeAccountLimits s =
    send (mkDescribeAccountLimits &~ s)

-- | Returns policy adjustment types for use in the PutScalingPolicy action.
-- https://autoscaling.amazonaws.com/?Version=2011-01-01
-- &Action=DescribeAdjustmentTypes &AUTHPARAMS ChangeInCapacity ExactCapacity
-- PercentChangeInCapacity cc5f0337-b694-11e2-afc0-6544dEXAMPLE.
--
-- See: 'Network.AWS.AutoScaling.V2011_01_01.DescribeAdjustmentTypes'
describeAdjustmentTypes :: ( MonadCatch m
                           , MonadResource m
                           , MonadError AWS.Error m
                           , MonadReader Env m
                           , AWSRequest a
                           )
                        => State DescribeAdjustmentTypes a
                        -> m DescribeAdjustmentTypesResponse
describeAdjustmentTypes s =
    send (mkDescribeAdjustmentTypes &~ s)

-- | Returns a full description of each Auto Scaling group in the given list.
-- This includes all Amazon EC2 instances that are members of the group. If a
-- list of names is not provided, the service returns the full details of all
-- Auto Scaling groups. This action supports pagination by returning a token
-- if there are more pages to retrieve. To get the next page, call this action
-- again with the returned token as the NextToken parameter.
-- https://autoscaling.amazonaws.com/?AutoScalingGroupNames.member.1=my-test-asg-lbs
-- &MaxRecords=20 &Version=2011-01-01 &Action=DescribeAutoScalingGroups
-- &AUTHPARAMS my-test-asg-lbs ELB 2013-05-06T17:47:15.107Z my-test-lc 2
-- us-east-1b us-east-1a my-test-asg-loadbalancer 2 120 300
-- arn:aws:autoscaling:us-east-1:803981987763:autoScalingGroup:ca861182-c8f9-4ca7-b1eb-cd35505f5ebb
-- :autoScalingGroupName/my-test-asg-lbs Default 10
-- 0f02a07d-b677-11e2-9eb0-dd50EXAMPLE.
--
-- See: 'Network.AWS.AutoScaling.V2011_01_01.DescribeAutoScalingGroups'
describeAutoScalingGroups :: ( MonadCatch m
                             , MonadResource m
                             , MonadError AWS.Error m
                             , MonadReader Env (ResumableSource m)
                             , AWSPager a
                             )
                          => State DescribeAutoScalingGroups a
                          -> ResumableSource m DescribeAutoScalingGroupsResponse
describeAutoScalingGroups s =
    paginate (mkDescribeAutoScalingGroups &~ s)

-- | Returns a description of each Auto Scaling instance in the InstanceIds
-- list. If a list is not provided, the service returns the full details of
-- all instances up to a maximum of 50. By default, the service returns a list
-- of 20 items. This action supports pagination by returning a token if there
-- are more pages to retrieve. To get the next page, call this action again
-- with the returned token as the NextToken parameter.
-- https://autoscaling.amazonaws.com/?MaxRecords=20
-- &InstanceIds.member.1=i-78e0d40b &Version=2011-01-01
-- &Action=DescribeAutoScalingInstances &AUTHPARAMS Healthy my-test-asg
-- us-east-1e i-78e0d40b my-test-lc InService
-- df992dc3-b72f-11e2-81e1-750aa6EXAMPLE.
--
-- See: 'Network.AWS.AutoScaling.V2011_01_01.DescribeAutoScalingInstances'
describeAutoScalingInstances :: ( MonadCatch m
                                , MonadResource m
                                , MonadError AWS.Error m
                                , MonadReader Env (ResumableSource m)
                                , AWSPager a
                                )
                             => State DescribeAutoScalingInstances a
                             -> ResumableSource m DescribeAutoScalingInstancesResponse
describeAutoScalingInstances s =
    paginate (mkDescribeAutoScalingInstances &~ s)

-- | Returns a list of all notification types that are supported by Auto
-- Scaling.
--
-- See: 'Network.AWS.AutoScaling.V2011_01_01.DescribeAutoScalingNotificationTypes'
describeAutoScalingNotificationTypes :: ( MonadCatch m
                                        , MonadResource m
                                        , MonadError AWS.Error m
                                        , MonadReader Env m
                                        , AWSRequest a
                                        )
                                     => State DescribeAutoScalingNotificationTypes a
                                     -> m DescribeAutoScalingNotificationTypesResponse
describeAutoScalingNotificationTypes s =
    send (mkDescribeAutoScalingNotificationTypes &~ s)

-- | Returns a full description of the launch configurations, or the specified
-- launch configurations, if they exist. If no name is specified, then the
-- full details of all launch configurations are returned.
-- https://autoscaling.amazonaws.com/?LaunchConfigurationNames.member.1=my-test-lc
-- &MaxRecords=20 &Version=2011-01-01 &Action=DescribeLaunchConfigurations
-- &AUTHPARAMS true dedicated 2013-01-21T23:04:42.200Z my-test-lc m1.small
-- arn:aws:autoscaling:us-east-1:803981987763:launchConfiguration:
-- 9dbbbf87-6141-428a-a409-0752edbe6cad:launchConfigurationName/my-test-lc
-- ami-514ac838 true false d05a22f8-b690-11e2-bf8e-2113fEXAMPLE.
--
-- See: 'Network.AWS.AutoScaling.V2011_01_01.DescribeLaunchConfigurations'
describeLaunchConfigurations :: ( MonadCatch m
                                , MonadResource m
                                , MonadError AWS.Error m
                                , MonadReader Env (ResumableSource m)
                                , AWSPager a
                                )
                             => State DescribeLaunchConfigurations a
                             -> ResumableSource m DescribeLaunchConfigurationsResponse
describeLaunchConfigurations s =
    paginate (mkDescribeLaunchConfigurations &~ s)

-- | Describes the available types of lifecycle hooks.
--
-- See: 'Network.AWS.AutoScaling.V2011_01_01.DescribeLifecycleHookTypes'
describeLifecycleHookTypes :: ( MonadCatch m
                              , MonadResource m
                              , MonadError AWS.Error m
                              , MonadReader Env m
                              , AWSRequest a
                              )
                           => State DescribeLifecycleHookTypes a
                           -> m DescribeLifecycleHookTypesResponse
describeLifecycleHookTypes s =
    send (mkDescribeLifecycleHookTypes &~ s)

-- | Describes the lifecycle hooks that currently belong to the specified Auto
-- Scaling group.
--
-- See: 'Network.AWS.AutoScaling.V2011_01_01.DescribeLifecycleHooks'
describeLifecycleHooks :: ( MonadCatch m
                          , MonadResource m
                          , MonadError AWS.Error m
                          , MonadReader Env m
                          , AWSRequest a
                          )
                       => Text -- ^ 'dlh1AutoScalingGroupName'
                       -> State DescribeLifecycleHooks a
                       -> m DescribeLifecycleHooksResponse
describeLifecycleHooks p1 s =
    send $ (mkDescribeLifecycleHooks p1) &~ s

-- | Returns a list of metrics and a corresponding list of granularities for
-- each metric.
-- https://autoscaling.amazonaws.com/?Version=2011-01-01&Action=DescribeMetricCollectionTypes
-- &AUTHPARAMS oc/2011-01-01/"> GroupMinSize GroupMaxSize GroupDesiredCapacity
-- GroupInServiceInstances GroupPendingInstances GroupStandyInstances
-- GroupTerminatingInstances GroupTotalInstances 1Minute
-- 07f3fea2-bf3c-11e2-9b6f-f3cdbb80c073 The GroupStandbyInstances metric is
-- not returned by default. You must explicitly request it when calling
-- EnableMetricsCollection.
--
-- See: 'Network.AWS.AutoScaling.V2011_01_01.DescribeMetricCollectionTypes'
describeMetricCollectionTypes :: ( MonadCatch m
                                 , MonadResource m
                                 , MonadError AWS.Error m
                                 , MonadReader Env m
                                 , AWSRequest a
                                 )
                              => State DescribeMetricCollectionTypes a
                              -> m DescribeMetricCollectionTypesResponse
describeMetricCollectionTypes s =
    send (mkDescribeMetricCollectionTypes &~ s)

-- | Returns a list of notification actions associated with Auto Scaling groups
-- for specified events.
--
-- See: 'Network.AWS.AutoScaling.V2011_01_01.DescribeNotificationConfigurations'
describeNotificationConfigurations :: ( MonadCatch m
                                      , MonadResource m
                                      , MonadError AWS.Error m
                                      , MonadReader Env (ResumableSource m)
                                      , AWSPager a
                                      )
                                   => State DescribeNotificationConfigurations a
                                   -> ResumableSource m DescribeNotificationConfigurationsResponse
describeNotificationConfigurations s =
    paginate (mkDescribeNotificationConfigurations &~ s)

-- | Returns descriptions of what each policy does. This action supports
-- pagination. If the response includes a token, there are more records
-- available. To get the additional records, repeat the request with the
-- response token as the NextToken parameter.
-- https://autoscaling.amazonaws.com/?AutoScalingGroupName=my-test-asg
-- &MaxRecords=20 &Version=2011-01-01 &Action=DescribePolicies &AUTHPARAMS
-- arn:aws:autoscaling:us-east-1:803981987763:scalingPolicy:c322
-- 761b-3172-4d56-9a21-0ed9d6161d67:autoScalingGroupName/my-test-asg:policyName/MyScaleDownPolicy
-- ChangeInCapacity -1 MyScaleDownPolicy my-test-asg 60 TestQueue
-- arn:aws:cloudwatch:us-east-1:803981987763:alarm:TestQueue
-- arn:aws:autoscaling:us-east-1:803981987763:scalingPolicy:c55a5cdd-9be0-435b-b60b-a8dd313159f5:autoScalingGroupName/my-test-asg:policyName/MyScaleUpPolicy
-- ChangeInCapacity 1 MyScaleUpPolicy my-test-asg 60 TestQueue
-- arn:aws:cloudwatch:us-east-1:803981987763:alarm:TestQueue
-- ec3bffad-b739-11e2-b38d-15fbEXAMPLE.
--
-- See: 'Network.AWS.AutoScaling.V2011_01_01.DescribePolicies'
describePolicies :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env (ResumableSource m)
                    , AWSPager a
                    )
                 => State DescribePolicies a
                 -> ResumableSource m DescribePoliciesResponse
describePolicies s =
    paginate (mkDescribePolicies &~ s)

-- | Returns the scaling activities for the specified Auto Scaling group. If the
-- specified ActivityIds list is empty, all the activities from the past six
-- weeks are returned. Activities are sorted by the start time. Activities
-- still in progress appear first on the list. This action supports
-- pagination. If the response includes a token, there are more records
-- available. To get the additional records, repeat the request with the
-- response token as the NextToken parameter.
-- https://autoscaling.amazonaws.com/?AutoScalingGroupName=my-test-asg
-- &MaxRecords=20 &Version=2011-01-01 &Action=DescribeScalingActivities
-- &AUTHPARAMS Failed 0 063308ae-aa22-4a9b-94f4-9faeEXAMPLE
-- 2012-04-12T17:32:07.882Z my-test-asg At 2012-04-12T17:31:30Z a user request
-- created an AutoScalingGroup changing the desired capacity from 0 to 1. At
-- 2012-04-12T17:32:07Z an instance was started in response to a difference
-- between desired and actual capacity, increasing the capacity from 0 to 1.
-- {} Launching a new EC2 instance. Status Reason: The image id 'ami-4edb0327'
-- does not exist. Launching EC2 instance failed. 2012-04-12T17:32:08Z The
-- image id 'ami-4edb0327' does not exist. Launching EC2 instance failed.
-- 7a641adc-84c5-11e1-a8a5-217ebEXAMPLE.
--
-- See: 'Network.AWS.AutoScaling.V2011_01_01.DescribeScalingActivities'
describeScalingActivities :: ( MonadCatch m
                             , MonadResource m
                             , MonadError AWS.Error m
                             , MonadReader Env (ResumableSource m)
                             , AWSPager a
                             )
                          => State DescribeScalingActivities a
                          -> ResumableSource m DescribeScalingActivitiesResponse
describeScalingActivities s =
    paginate (mkDescribeScalingActivities &~ s)

-- | Returns scaling process types for use in the ResumeProcesses and
-- SuspendProcesses actions.
-- https://autoscaling.amazonaws.com/?Version=2011-01-01
-- &Action=DescribeScalingProcessTypes &AUTHPARAMS AZRebalance
-- AddToLoadBalancer AlarmNotification HealthCheck Launch ReplaceUnhealthy
-- ScheduledActions Terminate 27f2eacc-b73f-11e2-ad99-c7aba3a9c963.
--
-- See: 'Network.AWS.AutoScaling.V2011_01_01.DescribeScalingProcessTypes'
describeScalingProcessTypes :: ( MonadCatch m
                               , MonadResource m
                               , MonadError AWS.Error m
                               , MonadReader Env m
                               , AWSRequest a
                               )
                            => State DescribeScalingProcessTypes a
                            -> m DescribeScalingProcessTypesResponse
describeScalingProcessTypes s =
    send (mkDescribeScalingProcessTypes &~ s)

-- | Lists all the actions scheduled for your Auto Scaling group that haven't
-- been executed. To see a list of actions already executed, see the activity
-- record returned in DescribeScalingActivities.
--
-- See: 'Network.AWS.AutoScaling.V2011_01_01.DescribeScheduledActions'
describeScheduledActions :: ( MonadCatch m
                            , MonadResource m
                            , MonadError AWS.Error m
                            , MonadReader Env (ResumableSource m)
                            , AWSPager a
                            )
                         => State DescribeScheduledActions a
                         -> ResumableSource m DescribeScheduledActionsResponse
describeScheduledActions s =
    paginate (mkDescribeScheduledActions &~ s)

-- | Lists the Auto Scaling group tags. You can use filters to limit results
-- when describing tags. For example, you can query for tags of a particular
-- Auto Scaling group. You can specify multiple values for a filter. A tag
-- must match at least one of the specified values for it to be included in
-- the results. You can also specify multiple filters. The result includes
-- information for a particular tag only if it matches all your filters. If
-- there's no match, no special message is returned.
-- https://autoscaling.amazonaws.com/?Version=2011-01-01&Action=DescribeTags
-- &AUTHPARAMS my-test-asg true 1.0 version auto-scaling-group
-- 086265fd-bf3e-11e2-85fc-fbb1EXAMPLE.
--
-- See: 'Network.AWS.AutoScaling.V2011_01_01.DescribeTags'
describeTags :: ( MonadCatch m
                , MonadResource m
                , MonadError AWS.Error m
                , MonadReader Env (ResumableSource m)
                , AWSPager a
                )
             => State DescribeTags a
             -> ResumableSource m DescribeTagsResponse
describeTags s =
    paginate (mkDescribeTags &~ s)

-- | Returns a list of all termination policies supported by Auto Scaling.
-- https://autoscaling.amazonaws.com/?Version=2011-01-01
-- &Action=DescribeTerminationPolicyTypes &AUTHPARAMS
-- ClosestToNextInstanceHour Default NewestInstance OldestInstance
-- OldestLaunchConfiguration d9a05827-b735-11e2-a40c-c79a5EXAMPLE.
--
-- See: 'Network.AWS.AutoScaling.V2011_01_01.DescribeTerminationPolicyTypes'
describeTerminationPolicyTypes :: ( MonadCatch m
                                  , MonadResource m
                                  , MonadError AWS.Error m
                                  , MonadReader Env m
                                  , AWSRequest a
                                  )
                               => State DescribeTerminationPolicyTypes a
                               -> m DescribeTerminationPolicyTypesResponse
describeTerminationPolicyTypes s =
    send (mkDescribeTerminationPolicyTypes &~ s)

-- | Using DetachInstances, you can remove an instance from an Auto Scaling
-- group. After the instances are detached, you can manage them independently
-- from the rest of the Auto Scaling group. To learn more about detaching
-- instances, see Detach Amazon EC2 Instances From Your Auto Scaling Group.
-- https://autoscaling.amazonaws.com/?AutoScalingGroupName=my-asg&ShouldDecrementDesiredCapacity=true&InstanceIds.member.1=i-5f2e8a0d&Version=2011-01-01
-- 
-- &Action=DetachInstances&SignatureVersion=2&SignatureMethod=HmacSHA256&Timestamp=2014-06-14T00%3A07%3A29.962Z&AUTHPARAMS
-- e54ff599-bf05-4076-8b95-a0f090ed90bb 50 InProgress 2014-06-14T00:07:30.280Z
-- At 2014-06-14T00:07:30Z instance i-5f2e8a0d was detached in response to a
-- user request, shrinking the capacity from 4 to 3. my-asg {"Availability
-- Zone":"us-east-1a"} Detaching EC2 instance: i-5f2e8a0d
-- e04f3b11-f357-11e3-a434-7f10009d5849.
--
-- See: 'Network.AWS.AutoScaling.V2011_01_01.DetachInstances'
detachInstances :: ( MonadCatch m
                   , MonadResource m
                   , MonadError AWS.Error m
                   , MonadReader Env m
                   , AWSRequest a
                   )
                => Text -- ^ 'diAutoScalingGroupName'
                -> Bool -- ^ 'diShouldDecrementDesiredCapacity'
                -> State DetachInstances a
                -> m DetachInstancesResponse
detachInstances p2 p3 s =
    send $ (mkDetachInstances p2 p3) &~ s

-- | Disables monitoring of group metrics for the Auto Scaling group specified
-- in AutoScalingGroupName. You can specify the list of affected metrics with
-- the Metrics parameter.
--
-- See: 'Network.AWS.AutoScaling.V2011_01_01.DisableMetricsCollection'
disableMetricsCollection :: ( MonadCatch m
                            , MonadResource m
                            , MonadError AWS.Error m
                            , MonadReader Env m
                            , AWSRequest a
                            )
                         => Text -- ^ 'dmcAutoScalingGroupName'
                         -> State DisableMetricsCollection a
                         -> m DisableMetricsCollectionResponse
disableMetricsCollection p1 s =
    send $ (mkDisableMetricsCollection p1) &~ s

-- | Enables monitoring of group metrics for the Auto Scaling group specified in
-- AutoScalingGroupName. You can specify the list of enabled metrics with the
-- Metrics parameter. Auto Scaling metrics collection can be turned on only if
-- the InstanceMonitoring flag, in the Auto Scaling group's launch
-- configuration, is set to True.
--
-- See: 'Network.AWS.AutoScaling.V2011_01_01.EnableMetricsCollection'
enableMetricsCollection :: ( MonadCatch m
                           , MonadResource m
                           , MonadError AWS.Error m
                           , MonadReader Env m
                           , AWSRequest a
                           )
                        => Text -- ^ 'emcAutoScalingGroupName'
                        -> Text -- ^ 'emcGranularity'
                        -> State EnableMetricsCollection a
                        -> m EnableMetricsCollectionResponse
enableMetricsCollection p1 p3 s =
    send $ (mkEnableMetricsCollection p1 p3) &~ s

-- | Move instances in an Auto Scaling group into a Standby mode. To learn more
-- about how to put instances into a Standby mode, see Auto Scaling InService
-- State.
-- https://autoscaling.amazonaws.com/?AutoScalingGroupName=my-asg&ShouldDecrementDesiredCapacity=true&InstanceIds.member.1=i-5b73d709&Version=2011-01-01&Action=
-- 
-- EnterStandby&SignatureVersion=2&SignatureMethod=HmacSHA256&Timestamp=2014-06-13T22%3A35%3A50.567Z&AUTHPARAMS
-- 462b4bc3-ad3b-4e67-a58d-96cd00f02f9e 50 InProgress 2014-06-13T22:35:50.884Z
-- At 2014-06-13T22:35:50Z instance i-5b73d709 was moved to standby in
-- response to a user request, shrinking the capacity from 4 to 3. my-asg
-- {"Availability Zone":"us-east-1a"} Moving EC2 instance to Standby:
-- i-5b73d709 126f2f31-f34b-11e3-bc51-b35178f0274f.
--
-- See: 'Network.AWS.AutoScaling.V2011_01_01.EnterStandby'
enterStandby :: ( MonadCatch m
                , MonadResource m
                , MonadError AWS.Error m
                , MonadReader Env m
                , AWSRequest a
                )
             => Text -- ^ 'esAutoScalingGroupName'
             -> Bool -- ^ 'esShouldDecrementDesiredCapacity'
             -> State EnterStandby a
             -> m EnterStandbyResponse
enterStandby p2 p3 s =
    send $ (mkEnterStandby p2 p3) &~ s

-- | Executes the specified policy.
--
-- See: 'Network.AWS.AutoScaling.V2011_01_01.ExecutePolicy'
executePolicy :: ( MonadCatch m
                 , MonadResource m
                 , MonadError AWS.Error m
                 , MonadReader Env m
                 , AWSRequest a
                 )
              => Text -- ^ 'epPolicyName'
              -> State ExecutePolicy a
              -> m ExecutePolicyResponse
executePolicy p2 s =
    send $ (mkExecutePolicy p2) &~ s

-- | Move an instance out of Standby mode. To learn more about how to put
-- instances that are in a Standby mode back into service, see Auto Scaling
-- InService State.
-- https://autoscaling.amazonaws.com/?InstanceIds.member.1=i-5b73d709&AutoScalingGroupName=my-asg&Version=2011-01-01&Action=ExitStandby&SignatureVersion=2&SignatureMet
-- hod=HmacSHA256&Timestamp=2014-06-13T22%3A43%3A53.182Z&AUTHPARAMS
-- dca4efcf-eea6-4844-8064-cab1fecd1aa2 30 PreInService
-- 2014-06-13T22:43:53.523Z At 2014-06-13T22:43:53Z instance i-5b73d709 was
-- moved out of standby in response to a user request, increasing the capacity
-- from 3 to 4. my-asg {"Availability Zone":"us-east-1a"} Moving EC2 instance
-- out of Standby: i-5b73d709 321a11c8-f34c-11e3-a434-7f10009d5849.
--
-- See: 'Network.AWS.AutoScaling.V2011_01_01.ExitStandby'
exitStandby :: ( MonadCatch m
               , MonadResource m
               , MonadError AWS.Error m
               , MonadReader Env m
               , AWSRequest a
               )
            => Text -- ^ 'es1AutoScalingGroupName'
            -> State ExitStandby a
            -> m ExitStandbyResponse
exitStandby p2 s =
    send $ (mkExitStandby p2) &~ s

-- | Creates or updates a lifecycle hook for an Auto Scaling Group. A lifecycle
-- hook tells Auto Scaling that you want to perform an action on an instance
-- that is not actively in service; for example, either when the instance
-- launches or before the instance terminates. This operation is a part of the
-- basic sequence for adding a lifecycle hook to an Auto Scaling group: Create
-- a notification target. A target can be either an Amazon SQS queue or an
-- Amazon SNS topic. Create an IAM role. This role allows Auto Scaling to
-- publish lifecycle notifications to the designated SQS queue or SNS topic.
-- Create the lifecycle hook. You can create a hook that acts when instances
-- launch or when instances terminate. If necessary, record the lifecycle
-- action heartbeat to keep the instance in a pending state. Complete the
-- lifecycle action. To learn more, see Auto Scaling Pending State and Auto
-- Scaling Terminating State.
-- http://autoscaling.amazonaws.com/?RoleARN=arn%3Aaws%3Aiam%3A%3A896650972448%3Arole%2FAutoScaling&AutoScalingGroupName=my-asg&LifecycleHookName=ReadyForSoftwareInst
-- 
-- all&NotificationTargetARN=arn%3Aaws%3Asqs%3Aus-east-1%3A896650972448%3Alifecyclehookqueue&LifecycleTransition=autoscaling%3AEC2_INSTANCE_LAUNCHING&Version=2011-
-- 
-- 01-01&Action=PutLifecycleHook&SignatureVersion=2&SignatureMethod=HmacSHA256&Timestamp=2014-06-17T17%3A30%3A36.125Z&AUTHPARAMS
-- 1952f458-f645-11e3-bc51-b35178f0274f.
--
-- See: 'Network.AWS.AutoScaling.V2011_01_01.PutLifecycleHook'
putLifecycleHook :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    , AWSRequest a
                    )
                 => Text -- ^ 'plhLifecycleHookName'
                 -> Text -- ^ 'plhAutoScalingGroupName'
                 -> State PutLifecycleHook a
                 -> m PutLifecycleHookResponse
putLifecycleHook p1 p2 s =
    send $ (mkPutLifecycleHook p1 p2) &~ s

-- | Configures an Auto Scaling group to send notifications when specified
-- events take place. Subscribers to this topic can have messages for events
-- delivered to an endpoint such as a web server or email address. For more
-- information see Get Email Notifications When Your Auto Scaling Group
-- Changes A new PutNotificationConfiguration overwrites an existing
-- configuration.
--
-- See: 'Network.AWS.AutoScaling.V2011_01_01.PutNotificationConfiguration'
putNotificationConfiguration :: ( MonadCatch m
                                , MonadResource m
                                , MonadError AWS.Error m
                                , MonadReader Env m
                                , AWSRequest a
                                )
                             => Text -- ^ 'pncAutoScalingGroupName'
                             -> Text -- ^ 'pncTopicARN'
                             -> [Text] -- ^ 'pncNotificationTypes'
                             -> State PutNotificationConfiguration a
                             -> m PutNotificationConfigurationResponse
putNotificationConfiguration p1 p2 p3 s =
    send $ (mkPutNotificationConfiguration p1 p2 p3) &~ s

-- | Creates or updates a policy for an Auto Scaling group. To update an
-- existing policy, use the existing policy name and set the parameter(s) you
-- want to change. Any existing parameter not changed in an update to an
-- existing policy is not changed in this update request.
-- https://autoscaling.amazonaws.com/?AutoScalingGroupName=my-test-asg
-- &ScalingAdjustment=30 &AdjustmentType=PercentChangeInCapacity
-- &PolicyName=my-scaleout-policy &Version=2011-01-01 &Action=PutScalingPolicy
-- &AUTHPARAMS
-- arn:aws:autoscaling:us-east-1:803981987763:scalingPolicy:b0dcf5e8
-- -02e6-4e31-9719-0675d0dc31ae:autoScalingGroupName/my-test-asg:policyName/my-scal
-- eout-policy 3cfc6fef-c08b-11e2-a697-2922EXAMPLE.
--
-- See: 'Network.AWS.AutoScaling.V2011_01_01.PutScalingPolicy'
putScalingPolicy :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    , AWSRequest a
                    )
                 => Text -- ^ 'pspAutoScalingGroupName'
                 -> Text -- ^ 'pspPolicyName'
                 -> Integer -- ^ 'pspScalingAdjustment'
                 -> Text -- ^ 'pspAdjustmentType'
                 -> State PutScalingPolicy a
                 -> m PutScalingPolicyResponse
putScalingPolicy p1 p2 p3 p4 s =
    send $ (mkPutScalingPolicy p1 p2 p3 p4) &~ s

-- | Creates or updates a scheduled scaling action for an Auto Scaling group.
-- When updating a scheduled scaling action, if you leave a parameter
-- unspecified, the corresponding value remains unchanged in the affected Auto
-- Scaling group. For information on creating or updating a scheduled action
-- for your Auto Scaling group, see Scale Based on a Schedule. Auto Scaling
-- supports the date and time expressed in "YYYY-MM-DDThh:mm:ssZ" format in
-- UTC/GMT only. Schedule based on a specific date and time
-- https://autoscaling.amazonaws.com/?AutoScalingGroupName=my-test-asg
-- &ScheduledActionName=ScaleUp &StartTime=2013-05-25T08:00:00Z
-- &DesiredCapacity=3 &Version=2011-01-01
-- &Action=PutScheduledUpdateGroupAction &AUTHPARAMS
-- 3bc8c9bc-6a62-11e2-8a51-4b8a1EXAMPLE Recurring Schedule
-- https://autoscaling.amazonaws.com/?AutoScalingGroupName=my-test-asg
-- &ScheduledActionName=scaleup-schedule-year &Recurrence="30 0 1 1,6,12 *"
-- &DesiredCapacity=3 &Version=2011-01-01
-- &Action=PutScheduledUpdateGroupAction &AUTHPARAMS
-- 3bc8c9bc-6a62-11e2-8a51-4b8a1EXAMPLE.
--
-- See: 'Network.AWS.AutoScaling.V2011_01_01.PutScheduledUpdateGroupAction'
putScheduledUpdateGroupAction :: ( MonadCatch m
                                 , MonadResource m
                                 , MonadError AWS.Error m
                                 , MonadReader Env m
                                 , AWSRequest a
                                 )
                              => Text -- ^ 'psugaAutoScalingGroupName'
                              -> Text -- ^ 'psugaScheduledActionName'
                              -> State PutScheduledUpdateGroupAction a
                              -> m PutScheduledUpdateGroupActionResponse
putScheduledUpdateGroupAction p1 p2 s =
    send $ (mkPutScheduledUpdateGroupAction p1 p2) &~ s

-- | Records a heartbeat for the lifecycle action associated with a specific
-- token. This extends the timeout by the length of time defined by the
-- HeartbeatTimeout parameter of the PutLifecycleHook operation. This
-- operation is a part of the basic sequence for adding a lifecycle hook to an
-- Auto Scaling group: Create a notification target. A target can be either an
-- Amazon SQS queue or an Amazon SNS topic. Create an IAM role. This role
-- allows Auto Scaling to publish lifecycle notifications to the designated
-- SQS queue or SNS topic. Create the lifecycle hook. You can create a hook
-- that acts when instances launch or when instances terminate. If necessary,
-- record the lifecycle action heartbeat to keep the instance in a pending
-- state. Complete the lifecycle action. To learn more, see Auto Scaling
-- Pending State and Auto Scaling Terminating State.
--
-- See: 'Network.AWS.AutoScaling.V2011_01_01.RecordLifecycleActionHeartbeat'
recordLifecycleActionHeartbeat :: ( MonadCatch m
                                  , MonadResource m
                                  , MonadError AWS.Error m
                                  , MonadReader Env m
                                  , AWSRequest a
                                  )
                               => Text -- ^ 'rlahLifecycleHookName'
                               -> Text -- ^ 'rlahAutoScalingGroupName'
                               -> Text -- ^ 'rlahLifecycleActionToken'
                               -> State RecordLifecycleActionHeartbeat a
                               -> m RecordLifecycleActionHeartbeatResponse
recordLifecycleActionHeartbeat p1 p2 p3 s =
    send $ (mkRecordLifecycleActionHeartbeat p1 p2 p3) &~ s

-- | Resumes all suspended Auto Scaling processes for an Auto Scaling group. For
-- information on suspending and resuming Auto Scaling process, see Suspend
-- and Resume Auto Scaling Process.
--
-- See: 'Network.AWS.AutoScaling.V2011_01_01.ResumeProcesses'
resumeProcesses :: ( MonadCatch m
                   , MonadResource m
                   , MonadError AWS.Error m
                   , MonadReader Env m
                   , AWSRequest a
                   )
                => Text -- ^ 'rpAutoScalingGroupName'
                -> State ResumeProcesses a
                -> m ResumeProcessesResponse
resumeProcesses p1 s =
    send $ (mkResumeProcesses p1) &~ s

-- | Sets the desired size of the specified AutoScalingGroup.
-- https://autoscaling.amazonaws.com/?AutoScalingGroupName=my-test-asg
-- &HonorCooldown=false &DesiredCapacity=2 &Version=2011-01-01
-- &Action=SetDesiredCapacity &AUTHPARAMS
-- 9fb7e2db-6998-11e2-a985-57c82EXAMPLE.
--
-- See: 'Network.AWS.AutoScaling.V2011_01_01.SetDesiredCapacity'
setDesiredCapacity :: ( MonadCatch m
                      , MonadResource m
                      , MonadError AWS.Error m
                      , MonadReader Env m
                      , AWSRequest a
                      )
                   => Text -- ^ 'sdcAutoScalingGroupName'
                   -> Integer -- ^ 'sdcDesiredCapacity'
                   -> State SetDesiredCapacity a
                   -> m SetDesiredCapacityResponse
setDesiredCapacity p1 p2 s =
    send $ (mkSetDesiredCapacity p1 p2) &~ s

-- | Sets the health status of a specified instance that belongs to any of your
-- Auto Scaling groups. For more information, see Configure Health Checks for
-- Your Auto Scaling group.
--
-- See: 'Network.AWS.AutoScaling.V2011_01_01.SetInstanceHealth'
setInstanceHealth :: ( MonadCatch m
                     , MonadResource m
                     , MonadError AWS.Error m
                     , MonadReader Env m
                     , AWSRequest a
                     )
                  => Text -- ^ 'sihInstanceId'
                  -> Text -- ^ 'sihHealthStatus'
                  -> State SetInstanceHealth a
                  -> m SetInstanceHealthResponse
setInstanceHealth p1 p2 s =
    send $ (mkSetInstanceHealth p1 p2) &~ s

-- | Suspends Auto Scaling processes for an Auto Scaling group. To suspend
-- specific process types, specify them by name with the
-- ScalingProcesses.member.N parameter. To suspend all process types, omit the
-- ScalingProcesses.member.N parameter. Suspending either of the two primary
-- process types, Launch or Terminate, can prevent other process types from
-- functioning properly. To resume processes that have been suspended, use
-- ResumeProcesses For more information on suspending and resuming Auto
-- Scaling process, see Suspend and Resume Auto Scaling Process.
--
-- See: 'Network.AWS.AutoScaling.V2011_01_01.SuspendProcesses'
suspendProcesses :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    , AWSRequest a
                    )
                 => Text -- ^ 'sp1AutoScalingGroupName'
                 -> State SuspendProcesses a
                 -> m SuspendProcessesResponse
suspendProcesses p1 s =
    send $ (mkSuspendProcesses p1) &~ s

-- | Terminates the specified instance. Optionally, the desired group size can
-- be adjusted. This call simply registers a termination request. The
-- termination of the instance cannot happen immediately.
--
-- See: 'Network.AWS.AutoScaling.V2011_01_01.TerminateInstanceInAutoScalingGroup'
terminateInstanceInAutoScalingGroup :: ( MonadCatch m
                                       , MonadResource m
                                       , MonadError AWS.Error m
                                       , MonadReader Env m
                                       , AWSRequest a
                                       )
                                    => Text -- ^ 'tiiasgInstanceId'
                                    -> Bool -- ^ 'tiiasgShouldDecrementDesiredCapacity'
                                    -> State TerminateInstanceInAutoScalingGroup a
                                    -> m TerminateInstanceInAutoScalingGroupResponse
terminateInstanceInAutoScalingGroup p1 p2 s =
    send $ (mkTerminateInstanceInAutoScalingGroup p1 p2) &~ s

-- | Updates the configuration for the specified AutoScalingGroup. To update an
-- Auto Scaling group with a launch configuration that has the
-- InstanceMonitoring flag set to False, you must first ensure that collection
-- of group metrics is disabled. Otherwise, calls to UpdateAutoScalingGroup
-- will fail. If you have previously enabled group metrics collection, you can
-- disable collection of all group metrics by calling
-- DisableMetricsCollection. The new settings are registered upon the
-- completion of this call. Any launch configuration settings take effect on
-- any triggers after this call returns. Scaling activities that are currently
-- in progress aren't affected. If a new value is specified for MinSize
-- without specifying the value for DesiredCapacity, and if the new MinSize is
-- larger than the current size of the Auto Scaling Group, there will be an
-- implicit call to SetDesiredCapacity to set the group to the new MinSize. If
-- a new value is specified for MaxSize without specifying the value for
-- DesiredCapacity, and the new MaxSize is smaller than the current size of
-- the Auto Scaling Group, there will be an implicit call to
-- SetDesiredCapacity to set the group to the new MaxSize. All other optional
-- parameters are left unchanged if not passed in the request. Update existing
-- Auto Scaling group with ELB health check
-- https://autoscaling.amazonaws.com/?HealthCheckType=ELB
-- &HealthCheckGracePeriod=300 &AutoScalingGroupName=my-test-asg-lbs
-- &Version=2011-01-01 &Action=UpdateAutoScalingGroup &AUTHPARAMS
-- adafead0-ab8a-11e2-ba13-ab0ccEXAMPLE Update existing Auto Scaling group
-- with a new Availability Zone
-- https://autoscaling.amazonaws.com/?AutoScalingGroupName=my-test-asg-lbs
-- &AvailabilityZones.member.1=us-east-1a
-- &AvailabilityZones.member.2=us-east-1b
-- &AvailabilityZones.member.3=us-east-1c &MinSize=3 &Version=2011-01-01
-- &Action=UpdateAutoScalingGroup &AUTHPARAMS
-- adafead0-ab8a-11e2-ba13-ab0ccEXAMPLE.
--
-- See: 'Network.AWS.AutoScaling.V2011_01_01.UpdateAutoScalingGroup'
updateAutoScalingGroup :: ( MonadCatch m
                          , MonadResource m
                          , MonadError AWS.Error m
                          , MonadReader Env m
                          , AWSRequest a
                          )
                       => Text -- ^ 'uasgAutoScalingGroupName'
                       -> State UpdateAutoScalingGroup a
                       -> m UpdateAutoScalingGroupResponse
updateAutoScalingGroup p1 s =
    send $ (mkUpdateAutoScalingGroup p1) &~ s
