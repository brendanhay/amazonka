{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | Amazon EC2 Container Service (Amazon ECS) is a highly scalable, fast,
-- container management service that makes it easy to run, stop, and manage
-- Docker containers on a cluster of Amazon EC2 instances. Amazon ECS lets
-- you launch and stop container-enabled applications with simple API
-- calls, allows you to get the state of your cluster from a centralized
-- service, and gives you access to many familiar Amazon EC2 features like
-- security groups, Amazon EBS volumes, and IAM roles.
--
-- You can use Amazon ECS to schedule the placement of containers across
-- your cluster based on your resource needs, isolation policies, and
-- availability requirements. Amazon EC2 Container Service eliminates the
-- need for you to operate your own cluster management and configuration
-- management systems or worry about scaling your management
-- infrastructure.
module Network.AWS.ECS
    ( module Export
    ) where

import           Network.AWS.ECS.CreateCluster               as Export
import           Network.AWS.ECS.CreateService               as Export
import           Network.AWS.ECS.DeleteCluster               as Export
import           Network.AWS.ECS.DeleteService               as Export
import           Network.AWS.ECS.DeregisterContainerInstance as Export
import           Network.AWS.ECS.DeregisterTaskDefinition    as Export
import           Network.AWS.ECS.DescribeClusters            as Export
import           Network.AWS.ECS.DescribeContainerInstances  as Export
import           Network.AWS.ECS.DescribeServices            as Export
import           Network.AWS.ECS.DescribeTaskDefinition      as Export
import           Network.AWS.ECS.DescribeTasks               as Export
import           Network.AWS.ECS.DiscoverPollEndpoint        as Export
import           Network.AWS.ECS.ListClusters                as Export
import           Network.AWS.ECS.ListContainerInstances      as Export
import           Network.AWS.ECS.ListServices                as Export
import           Network.AWS.ECS.ListTaskDefinitionFamilies  as Export
import           Network.AWS.ECS.ListTaskDefinitions         as Export
import           Network.AWS.ECS.ListTasks                   as Export
import           Network.AWS.ECS.RegisterContainerInstance   as Export
import           Network.AWS.ECS.RegisterTaskDefinition      as Export
import           Network.AWS.ECS.RunTask                     as Export
import           Network.AWS.ECS.StartTask                   as Export
import           Network.AWS.ECS.StopTask                    as Export
import           Network.AWS.ECS.SubmitContainerStateChange  as Export
import           Network.AWS.ECS.SubmitTaskStateChange       as Export
import           Network.AWS.ECS.Types                       as Export
import           Network.AWS.ECS.Types.Product               as Export
import           Network.AWS.ECS.Types.Sum                   as Export
import           Network.AWS.ECS.UpdateContainerAgent        as Export
import           Network.AWS.ECS.UpdateService               as Export
import           Network.AWS.ECS.Waiters                     as Export
