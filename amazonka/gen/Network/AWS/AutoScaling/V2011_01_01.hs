-- Module      : Network.AWS.AutoScaling.V2011_01_01
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
module Network.AWS.AutoScaling.V2011_01_01 (module Export) where

import Network.AWS.AutoScaling.V2011_01_01.AttachInstances as Export
import Network.AWS.AutoScaling.V2011_01_01.CreateAutoScalingGroup as Export
import Network.AWS.AutoScaling.V2011_01_01.CreateLaunchConfiguration as Export
import Network.AWS.AutoScaling.V2011_01_01.CreateOrUpdateTags as Export
import Network.AWS.AutoScaling.V2011_01_01.DeleteAutoScalingGroup as Export
import Network.AWS.AutoScaling.V2011_01_01.DeleteLaunchConfiguration as Export
import Network.AWS.AutoScaling.V2011_01_01.DeleteNotificationConfiguration as Export
import Network.AWS.AutoScaling.V2011_01_01.DeletePolicy as Export
import Network.AWS.AutoScaling.V2011_01_01.DeleteScheduledAction as Export
import Network.AWS.AutoScaling.V2011_01_01.DeleteTags as Export
import Network.AWS.AutoScaling.V2011_01_01.DescribeAccountLimits as Export
import Network.AWS.AutoScaling.V2011_01_01.DescribeAdjustmentTypes as Export
import Network.AWS.AutoScaling.V2011_01_01.DescribeAutoScalingGroups as Export
import Network.AWS.AutoScaling.V2011_01_01.DescribeAutoScalingInstances as Export
import Network.AWS.AutoScaling.V2011_01_01.DescribeAutoScalingNotificationTypes as Export
import Network.AWS.AutoScaling.V2011_01_01.DescribeLaunchConfigurations as Export
import Network.AWS.AutoScaling.V2011_01_01.DescribeMetricCollectionTypes as Export
import Network.AWS.AutoScaling.V2011_01_01.DescribeNotificationConfigurations as Export
import Network.AWS.AutoScaling.V2011_01_01.DescribePolicies as Export
import Network.AWS.AutoScaling.V2011_01_01.DescribeScalingActivities as Export
import Network.AWS.AutoScaling.V2011_01_01.DescribeScalingProcessTypes as Export
import Network.AWS.AutoScaling.V2011_01_01.DescribeScheduledActions as Export
import Network.AWS.AutoScaling.V2011_01_01.DescribeTags as Export
import Network.AWS.AutoScaling.V2011_01_01.DescribeTerminationPolicyTypes as Export
import Network.AWS.AutoScaling.V2011_01_01.DisableMetricsCollection as Export
import Network.AWS.AutoScaling.V2011_01_01.EnableMetricsCollection as Export
import Network.AWS.AutoScaling.V2011_01_01.ExecutePolicy as Export
import Network.AWS.AutoScaling.V2011_01_01.PutNotificationConfiguration as Export
import Network.AWS.AutoScaling.V2011_01_01.PutScalingPolicy as Export
import Network.AWS.AutoScaling.V2011_01_01.PutScheduledUpdateGroupAction as Export
import Network.AWS.AutoScaling.V2011_01_01.ResumeProcesses as Export
import Network.AWS.AutoScaling.V2011_01_01.SetDesiredCapacity as Export
import Network.AWS.AutoScaling.V2011_01_01.SetInstanceHealth as Export
import Network.AWS.AutoScaling.V2011_01_01.SuspendProcesses as Export
import Network.AWS.AutoScaling.V2011_01_01.TerminateInstanceInAutoScalingGroup as Export
import Network.AWS.AutoScaling.V2011_01_01.Types as Export
import Network.AWS.AutoScaling.V2011_01_01.UpdateAutoScalingGroup as Export
