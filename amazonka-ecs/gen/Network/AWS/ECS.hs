-- Module      : Network.AWS.ECS
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

-- | Amazon EC2 Container Service is a highly scalable, high performance container
-- management service that supports Docker containers and allows you to easily
-- run distributed applications on a managed cluster of Amazon EC2 instances.
-- Amazon EC2 Container Service lets you launch and stop container-enabled
-- applications with simple API calls, allows you to query the state of your
-- cluster from a centralized service, and gives you access to many familiar
-- Amazon EC2 features like security groups, EBS volumes and IAM roles. You can
-- use EC2 Container Service to schedule the placement of containers across your
-- cluster based on your resource needs, isolation policies, and availability
-- requirements. Amazon EC2 Container Service eliminates the need for you to
-- operate your own cluster management and configuration management systems or
-- worry about scaling your management infrastructure.
module Network.AWS.ECS
    ( module Network.AWS.ECS.CreateCluster
    , module Network.AWS.ECS.CreateService
    , module Network.AWS.ECS.DeleteCluster
    , module Network.AWS.ECS.DeleteService
    , module Network.AWS.ECS.DeregisterContainerInstance
    , module Network.AWS.ECS.DeregisterTaskDefinition
    , module Network.AWS.ECS.DescribeClusters
    , module Network.AWS.ECS.DescribeContainerInstances
    , module Network.AWS.ECS.DescribeServices
    , module Network.AWS.ECS.DescribeTaskDefinition
    , module Network.AWS.ECS.DescribeTasks
    , module Network.AWS.ECS.DiscoverPollEndpoint
    , module Network.AWS.ECS.ListClusters
    , module Network.AWS.ECS.ListContainerInstances
    , module Network.AWS.ECS.ListServices
    , module Network.AWS.ECS.ListTaskDefinitionFamilies
    , module Network.AWS.ECS.ListTaskDefinitions
    , module Network.AWS.ECS.ListTasks
    , module Network.AWS.ECS.RegisterContainerInstance
    , module Network.AWS.ECS.RegisterTaskDefinition
    , module Network.AWS.ECS.RunTask
    , module Network.AWS.ECS.StartTask
    , module Network.AWS.ECS.StopTask
    , module Network.AWS.ECS.SubmitContainerStateChange
    , module Network.AWS.ECS.SubmitTaskStateChange
    , module Network.AWS.ECS.Types
    , module Network.AWS.ECS.UpdateService
    ) where

import Network.AWS.ECS.CreateCluster
import Network.AWS.ECS.CreateService
import Network.AWS.ECS.DeleteCluster
import Network.AWS.ECS.DeleteService
import Network.AWS.ECS.DeregisterContainerInstance
import Network.AWS.ECS.DeregisterTaskDefinition
import Network.AWS.ECS.DescribeClusters
import Network.AWS.ECS.DescribeContainerInstances
import Network.AWS.ECS.DescribeServices
import Network.AWS.ECS.DescribeTaskDefinition
import Network.AWS.ECS.DescribeTasks
import Network.AWS.ECS.DiscoverPollEndpoint
import Network.AWS.ECS.ListClusters
import Network.AWS.ECS.ListContainerInstances
import Network.AWS.ECS.ListServices
import Network.AWS.ECS.ListTaskDefinitionFamilies
import Network.AWS.ECS.ListTaskDefinitions
import Network.AWS.ECS.ListTasks
import Network.AWS.ECS.RegisterContainerInstance
import Network.AWS.ECS.RegisterTaskDefinition
import Network.AWS.ECS.RunTask
import Network.AWS.ECS.StartTask
import Network.AWS.ECS.StopTask
import Network.AWS.ECS.SubmitContainerStateChange
import Network.AWS.ECS.SubmitTaskStateChange
import Network.AWS.ECS.Types
import Network.AWS.ECS.UpdateService
