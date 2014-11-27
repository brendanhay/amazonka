-- Module      : Network.AWS.AutoScaling
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Auto Scaling allows you to scale your Amazon EC2 capacity up or down
-- automatically according to conditions you define. With Auto Scaling, you can
-- ensure that the number of Amazon EC2 instances you’re using increases
-- seamlessly during demand spikes to maintain performance, and decreases
-- automatically during demand lulls to minimize costs. Auto Scaling is
-- particularly well suited for applications that experience hourly, daily, or
-- weekly variability in usage. Auto Scaling is enabled by Amazon CloudWatch and
-- available at no additional charge beyond Amazon CloudWatch fees.
module Network.AWS.AutoScaling
    ( module Network.AWS.AutoScaling.AttachInstances
    , module Network.AWS.AutoScaling.CompleteLifecycleAction
    , module Network.AWS.AutoScaling.CreateAutoScalingGroup
    , module Network.AWS.AutoScaling.CreateLaunchConfiguration
    , module Network.AWS.AutoScaling.CreateOrUpdateTags
    , module Network.AWS.AutoScaling.DeleteAutoScalingGroup
    , module Network.AWS.AutoScaling.DeleteLaunchConfiguration
    , module Network.AWS.AutoScaling.DeleteLifecycleHook
    , module Network.AWS.AutoScaling.DeleteNotificationConfiguration
    , module Network.AWS.AutoScaling.DeletePolicy
    , module Network.AWS.AutoScaling.DeleteScheduledAction
    , module Network.AWS.AutoScaling.DeleteTags
    , module Network.AWS.AutoScaling.DescribeAccountLimits
    , module Network.AWS.AutoScaling.DescribeAdjustmentTypes
    , module Network.AWS.AutoScaling.DescribeAutoScalingGroups
    , module Network.AWS.AutoScaling.DescribeAutoScalingInstances
    , module Network.AWS.AutoScaling.DescribeAutoScalingNotificationTypes
    , module Network.AWS.AutoScaling.DescribeLaunchConfigurations
    , module Network.AWS.AutoScaling.DescribeLifecycleHookTypes
    , module Network.AWS.AutoScaling.DescribeLifecycleHooks
    , module Network.AWS.AutoScaling.DescribeMetricCollectionTypes
    , module Network.AWS.AutoScaling.DescribeNotificationConfigurations
    , module Network.AWS.AutoScaling.DescribePolicies
    , module Network.AWS.AutoScaling.DescribeScalingActivities
    , module Network.AWS.AutoScaling.DescribeScalingProcessTypes
    , module Network.AWS.AutoScaling.DescribeScheduledActions
    , module Network.AWS.AutoScaling.DescribeTags
    , module Network.AWS.AutoScaling.DescribeTerminationPolicyTypes
    , module Network.AWS.AutoScaling.DetachInstances
    , module Network.AWS.AutoScaling.DisableMetricsCollection
    , module Network.AWS.AutoScaling.EnableMetricsCollection
    , module Network.AWS.AutoScaling.EnterStandby
    , module Network.AWS.AutoScaling.ExecutePolicy
    , module Network.AWS.AutoScaling.ExitStandby
    , module Network.AWS.AutoScaling.PutLifecycleHook
    , module Network.AWS.AutoScaling.PutNotificationConfiguration
    , module Network.AWS.AutoScaling.PutScalingPolicy
    , module Network.AWS.AutoScaling.PutScheduledUpdateGroupAction
    , module Network.AWS.AutoScaling.RecordLifecycleActionHeartbeat
    , module Network.AWS.AutoScaling.ResumeProcesses
    , module Network.AWS.AutoScaling.SetDesiredCapacity
    , module Network.AWS.AutoScaling.SetInstanceHealth
    , module Network.AWS.AutoScaling.SuspendProcesses
    , module Network.AWS.AutoScaling.TerminateInstanceInAutoScalingGroup
    , module Network.AWS.AutoScaling.Types
    , module Network.AWS.AutoScaling.UpdateAutoScalingGroup
    ) where

import Network.AWS.AutoScaling.AttachInstances
import Network.AWS.AutoScaling.CompleteLifecycleAction
import Network.AWS.AutoScaling.CreateAutoScalingGroup
import Network.AWS.AutoScaling.CreateLaunchConfiguration
import Network.AWS.AutoScaling.CreateOrUpdateTags
import Network.AWS.AutoScaling.DeleteAutoScalingGroup
import Network.AWS.AutoScaling.DeleteLaunchConfiguration
import Network.AWS.AutoScaling.DeleteLifecycleHook
import Network.AWS.AutoScaling.DeleteNotificationConfiguration
import Network.AWS.AutoScaling.DeletePolicy
import Network.AWS.AutoScaling.DeleteScheduledAction
import Network.AWS.AutoScaling.DeleteTags
import Network.AWS.AutoScaling.DescribeAccountLimits
import Network.AWS.AutoScaling.DescribeAdjustmentTypes
import Network.AWS.AutoScaling.DescribeAutoScalingGroups
import Network.AWS.AutoScaling.DescribeAutoScalingInstances
import Network.AWS.AutoScaling.DescribeAutoScalingNotificationTypes
import Network.AWS.AutoScaling.DescribeLaunchConfigurations
import Network.AWS.AutoScaling.DescribeLifecycleHookTypes
import Network.AWS.AutoScaling.DescribeLifecycleHooks
import Network.AWS.AutoScaling.DescribeMetricCollectionTypes
import Network.AWS.AutoScaling.DescribeNotificationConfigurations
import Network.AWS.AutoScaling.DescribePolicies
import Network.AWS.AutoScaling.DescribeScalingActivities
import Network.AWS.AutoScaling.DescribeScalingProcessTypes
import Network.AWS.AutoScaling.DescribeScheduledActions
import Network.AWS.AutoScaling.DescribeTags
import Network.AWS.AutoScaling.DescribeTerminationPolicyTypes
import Network.AWS.AutoScaling.DetachInstances
import Network.AWS.AutoScaling.DisableMetricsCollection
import Network.AWS.AutoScaling.EnableMetricsCollection
import Network.AWS.AutoScaling.EnterStandby
import Network.AWS.AutoScaling.ExecutePolicy
import Network.AWS.AutoScaling.ExitStandby
import Network.AWS.AutoScaling.PutLifecycleHook
import Network.AWS.AutoScaling.PutNotificationConfiguration
import Network.AWS.AutoScaling.PutScalingPolicy
import Network.AWS.AutoScaling.PutScheduledUpdateGroupAction
import Network.AWS.AutoScaling.RecordLifecycleActionHeartbeat
import Network.AWS.AutoScaling.ResumeProcesses
import Network.AWS.AutoScaling.SetDesiredCapacity
import Network.AWS.AutoScaling.SetInstanceHealth
import Network.AWS.AutoScaling.SuspendProcesses
import Network.AWS.AutoScaling.TerminateInstanceInAutoScalingGroup
import Network.AWS.AutoScaling.Types
import Network.AWS.AutoScaling.UpdateAutoScalingGroup
