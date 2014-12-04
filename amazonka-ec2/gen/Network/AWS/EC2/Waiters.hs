{-# LANGUAGE TypeFamilies #-}

-- Module      : Network.AWS.EC2.Waiters
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

module Network.AWS.EC2.Waiters where

import Network.AWS.EC2.DescribeBundleTasks
import Network.AWS.EC2.DescribeConversionTasks
import Network.AWS.EC2.DescribeCustomerGateways
import Network.AWS.EC2.DescribeExportTasks
import Network.AWS.EC2.DescribeInstances
import Network.AWS.EC2.DescribeSnapshots
import Network.AWS.EC2.DescribeSubnets
import Network.AWS.EC2.DescribeVolumes
import Network.AWS.EC2.DescribeVpcs
import Network.AWS.EC2.DescribeVpnConnections
import Network.AWS.Types

data BundleTaskComplete = BundleTaskComplete
    deriving (Show)

instance AWSWaiter BundleTaskComplete where
    type Rq BundleTaskComplete = DescribeBundleTasks

    waiter BundleTaskComplete = Waiter
        { _waitDelay     = 15
        , _waitAttempts  = 40
        , _waitAccept    = const True
        }

data ConversionTaskCancelled = ConversionTaskCancelled
    deriving (Show)

instance AWSWaiter ConversionTaskCancelled where
    type Rq ConversionTaskCancelled = DescribeConversionTasks

    waiter ConversionTaskCancelled = Waiter
        { _waitDelay     = 15
        , _waitAttempts  = 40
        , _waitAccept    = const True
        }

data ConversionTaskCompleted = ConversionTaskCompleted
    deriving (Show)

instance AWSWaiter ConversionTaskCompleted where
    type Rq ConversionTaskCompleted = DescribeConversionTasks

    waiter ConversionTaskCompleted = Waiter
        { _waitDelay     = 15
        , _waitAttempts  = 40
        , _waitAccept    = const True
        }

data ConversionTaskDeleted = ConversionTaskDeleted
    deriving (Show)

instance AWSWaiter ConversionTaskDeleted where
    type Rq ConversionTaskDeleted = DescribeConversionTasks

    waiter ConversionTaskDeleted = Waiter
        { _waitDelay     = 15
        , _waitAttempts  = 40
        , _waitAccept    = const True
        }

data CustomerGatewayAvailable = CustomerGatewayAvailable
    deriving (Show)

instance AWSWaiter CustomerGatewayAvailable where
    type Rq CustomerGatewayAvailable = DescribeCustomerGateways

    waiter CustomerGatewayAvailable = Waiter
        { _waitDelay     = 15
        , _waitAttempts  = 40
        , _waitAccept    = const True
        }

data ExportTaskCancelled = ExportTaskCancelled
    deriving (Show)

instance AWSWaiter ExportTaskCancelled where
    type Rq ExportTaskCancelled = DescribeExportTasks

    waiter ExportTaskCancelled = Waiter
        { _waitDelay     = 15
        , _waitAttempts  = 40
        , _waitAccept    = const True
        }

data ExportTaskCompleted = ExportTaskCompleted
    deriving (Show)

instance AWSWaiter ExportTaskCompleted where
    type Rq ExportTaskCompleted = DescribeExportTasks

    waiter ExportTaskCompleted = Waiter
        { _waitDelay     = 15
        , _waitAttempts  = 40
        , _waitAccept    = const True
        }

data InstanceRunning = InstanceRunning
    deriving (Show)

instance AWSWaiter InstanceRunning where
    type Rq InstanceRunning = DescribeInstances

    waiter InstanceRunning = Waiter
        { _waitDelay     = 15
        , _waitAttempts  = 40
        , _waitAccept    = const True
        }

data InstanceStopped = InstanceStopped
    deriving (Show)

instance AWSWaiter InstanceStopped where
    type Rq InstanceStopped = DescribeInstances

    waiter InstanceStopped = Waiter
        { _waitDelay     = 15
        , _waitAttempts  = 40
        , _waitAccept    = const True
        }

data InstanceTerminated = InstanceTerminated
    deriving (Show)

instance AWSWaiter InstanceTerminated where
    type Rq InstanceTerminated = DescribeInstances

    waiter InstanceTerminated = Waiter
        { _waitDelay     = 15
        , _waitAttempts  = 40
        , _waitAccept    = const True
        }

data SnapshotCompleted = SnapshotCompleted
    deriving (Show)

instance AWSWaiter SnapshotCompleted where
    type Rq SnapshotCompleted = DescribeSnapshots

    waiter SnapshotCompleted = Waiter
        { _waitDelay     = 15
        , _waitAttempts  = 40
        , _waitAccept    = const True
        }

data SubnetAvailable = SubnetAvailable
    deriving (Show)

instance AWSWaiter SubnetAvailable where
    type Rq SubnetAvailable = DescribeSubnets

    waiter SubnetAvailable = Waiter
        { _waitDelay     = 15
        , _waitAttempts  = 40
        , _waitAccept    = const True
        }

data VolumeAvailable = VolumeAvailable
    deriving (Show)

instance AWSWaiter VolumeAvailable where
    type Rq VolumeAvailable = DescribeVolumes

    waiter VolumeAvailable = Waiter
        { _waitDelay     = 15
        , _waitAttempts  = 40
        , _waitAccept    = const True
        }

data VolumeDeleted = VolumeDeleted
    deriving (Show)

instance AWSWaiter VolumeDeleted where
    type Rq VolumeDeleted = DescribeVolumes

    waiter VolumeDeleted = Waiter
        { _waitDelay     = 15
        , _waitAttempts  = 40
        , _waitAccept    = const True
        }

data VolumeInUse = VolumeInUse
    deriving (Show)

instance AWSWaiter VolumeInUse where
    type Rq VolumeInUse = DescribeVolumes

    waiter VolumeInUse = Waiter
        { _waitDelay     = 15
        , _waitAttempts  = 40
        , _waitAccept    = const True
        }

data VpcAvailable = VpcAvailable
    deriving (Show)

instance AWSWaiter VpcAvailable where
    type Rq VpcAvailable = DescribeVpcs

    waiter VpcAvailable = Waiter
        { _waitDelay     = 15
        , _waitAttempts  = 40
        , _waitAccept    = const True
        }

data VpnConnectionAvailable = VpnConnectionAvailable
    deriving (Show)

instance AWSWaiter VpnConnectionAvailable where
    type Rq VpnConnectionAvailable = DescribeVpnConnections

    waiter VpnConnectionAvailable = Waiter
        { _waitDelay     = 15
        , _waitAttempts  = 40
        , _waitAccept    = const True
        }

data VpnConnectionDeleted = VpnConnectionDeleted
    deriving (Show)

instance AWSWaiter VpnConnectionDeleted where
    type Rq VpnConnectionDeleted = DescribeVpnConnections

    waiter VpnConnectionDeleted = Waiter
        { _waitDelay     = 15
        , _waitAttempts  = 40
        , _waitAccept    = const True
        }
