{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Auto Scaling
--
-- Auto Scaling is designed to automatically launch or terminate EC2
-- instances based on user-defined policies, schedules, and health checks.
-- Use this service in conjunction with the Amazon CloudWatch and Elastic
-- Load Balancing services.
module Network.AWS.AutoScaling
    ( module Export
    ) where

import           Network.AWS.AutoScaling.AttachInstances                      as Export
import           Network.AWS.AutoScaling.AttachLoadBalancers                  as Export
import           Network.AWS.AutoScaling.CompleteLifecycleAction              as Export
import           Network.AWS.AutoScaling.CreateAutoScalingGroup               as Export
import           Network.AWS.AutoScaling.CreateLaunchConfiguration            as Export
import           Network.AWS.AutoScaling.CreateOrUpdateTags                   as Export
import           Network.AWS.AutoScaling.DeleteAutoScalingGroup               as Export
import           Network.AWS.AutoScaling.DeleteLaunchConfiguration            as Export
import           Network.AWS.AutoScaling.DeleteLifecycleHook                  as Export
import           Network.AWS.AutoScaling.DeleteNotificationConfiguration      as Export
import           Network.AWS.AutoScaling.DeletePolicy                         as Export
import           Network.AWS.AutoScaling.DeleteScheduledAction                as Export
import           Network.AWS.AutoScaling.DeleteTags                           as Export
import           Network.AWS.AutoScaling.DescribeAccountLimits                as Export
import           Network.AWS.AutoScaling.DescribeAdjustmentTypes              as Export
import           Network.AWS.AutoScaling.DescribeAutoScalingGroups            as Export
import           Network.AWS.AutoScaling.DescribeAutoScalingInstances         as Export
import           Network.AWS.AutoScaling.DescribeAutoScalingNotificationTypes as Export
import           Network.AWS.AutoScaling.DescribeLaunchConfigurations         as Export
import           Network.AWS.AutoScaling.DescribeLifecycleHooks               as Export
import           Network.AWS.AutoScaling.DescribeLifecycleHookTypes           as Export
import           Network.AWS.AutoScaling.DescribeLoadBalancers                as Export
import           Network.AWS.AutoScaling.DescribeMetricCollectionTypes        as Export
import           Network.AWS.AutoScaling.DescribeNotificationConfigurations   as Export
import           Network.AWS.AutoScaling.DescribePolicies                     as Export
import           Network.AWS.AutoScaling.DescribeScalingActivities            as Export
import           Network.AWS.AutoScaling.DescribeScalingProcessTypes          as Export
import           Network.AWS.AutoScaling.DescribeScheduledActions             as Export
import           Network.AWS.AutoScaling.DescribeTags                         as Export
import           Network.AWS.AutoScaling.DescribeTerminationPolicyTypes       as Export
import           Network.AWS.AutoScaling.DetachInstances                      as Export
import           Network.AWS.AutoScaling.DetachLoadBalancers                  as Export
import           Network.AWS.AutoScaling.DisableMetricsCollection             as Export
import           Network.AWS.AutoScaling.EnableMetricsCollection              as Export
import           Network.AWS.AutoScaling.EnterStandby                         as Export
import           Network.AWS.AutoScaling.ExecutePolicy                        as Export
import           Network.AWS.AutoScaling.ExitStandby                          as Export
import           Network.AWS.AutoScaling.PutLifecycleHook                     as Export
import           Network.AWS.AutoScaling.PutNotificationConfiguration         as Export
import           Network.AWS.AutoScaling.PutScalingPolicy                     as Export
import           Network.AWS.AutoScaling.PutScheduledUpdateGroupAction        as Export
import           Network.AWS.AutoScaling.RecordLifecycleActionHeartbeat       as Export
import           Network.AWS.AutoScaling.ResumeProcesses                      as Export
import           Network.AWS.AutoScaling.SetDesiredCapacity                   as Export
import           Network.AWS.AutoScaling.SetInstanceHealth                    as Export
import           Network.AWS.AutoScaling.SuspendProcesses                     as Export
import           Network.AWS.AutoScaling.TerminateInstanceInAutoScalingGroup  as Export
import           Network.AWS.AutoScaling.Types                                as Export
import           Network.AWS.AutoScaling.Types.Product                        as Export
import           Network.AWS.AutoScaling.Types.Sum                            as Export
import           Network.AWS.AutoScaling.UpdateAutoScalingGroup               as Export
import           Network.AWS.AutoScaling.Waiters                              as Export
