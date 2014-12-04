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
import Network.AWS.EC2.Types
import Network.AWS.Types

data BundleTaskComplete = BundleTaskComplete

instance AWSWaiter BundleTaskComplete where
    type Rq BundleTaskComplete = DescribeBundleTasks

    waiter BundleTaskComplete x = Waiter
        { _waitDelay     = 15
        , _waitAttempts  = 40
        , _waitOperation = x
        , _waitAccept    = const False
        }

data ConversionTaskCancelled = ConversionTaskCancelled

instance AWSWaiter ConversionTaskCancelled where
    type Rq ConversionTaskCancelled = DescribeConversionTasks

    waiter ConversionTaskCancelled x = Waiter
        { _waitDelay     = 15
        , _waitAttempts  = 40
        , _waitOperation = x
        , _waitAccept    = const False
        }

data ConversionTaskCompleted = ConversionTaskCompleted

instance AWSWaiter ConversionTaskCompleted where
    type Rq ConversionTaskCompleted = DescribeConversionTasks

    waiter ConversionTaskCompleted x = Waiter
        { _waitDelay     = 15
        , _waitAttempts  = 40
        , _waitOperation = x
        , _waitAccept    = const False
        }

data ConversionTaskDeleted = ConversionTaskDeleted

instance AWSWaiter ConversionTaskDeleted where
    type Rq ConversionTaskDeleted = DescribeConversionTasks

    waiter ConversionTaskDeleted x = Waiter
        { _waitDelay     = 15
        , _waitAttempts  = 40
        , _waitOperation = x
        , _waitAccept    = const False
        }

data CustomerGatewayAvailable = CustomerGatewayAvailable

instance AWSWaiter CustomerGatewayAvailable where
    type Rq CustomerGatewayAvailable = DescribeCustomerGateways

    waiter CustomerGatewayAvailable x = Waiter
        { _waitDelay     = 15
        , _waitAttempts  = 40
        , _waitOperation = x
        , _waitAccept    = const False
        }

data ExportTaskCancelled = ExportTaskCancelled

instance AWSWaiter ExportTaskCancelled where
    type Rq ExportTaskCancelled = DescribeExportTasks

    waiter ExportTaskCancelled x = Waiter
        { _waitDelay     = 15
        , _waitAttempts  = 40
        , _waitOperation = x
        , _waitAccept    = const False
        }

data ExportTaskCompleted = ExportTaskCompleted

instance AWSWaiter ExportTaskCompleted where
    type Rq ExportTaskCompleted = DescribeExportTasks

    waiter ExportTaskCompleted x = Waiter
        { _waitDelay     = 15
        , _waitAttempts  = 40
        , _waitOperation = x
        , _waitAccept    = const False
        }

data InstanceRunning = InstanceRunning

instance AWSWaiter InstanceRunning where
    type Rq InstanceRunning = DescribeInstances

    waiter InstanceRunning x = Waiter
        { _waitDelay     = 15
        , _waitAttempts  = 40
        , _waitOperation = x
        , _waitAccept    = const False
        }

data InstanceStopped = InstanceStopped

instance AWSWaiter InstanceStopped where
    type Rq InstanceStopped = DescribeInstances

    waiter InstanceStopped x = Waiter
        { _waitDelay     = 15
        , _waitAttempts  = 40
        , _waitOperation = x
        , _waitAccept    = const False
        }

data InstanceTerminated = InstanceTerminated

instance AWSWaiter InstanceTerminated where
    type Rq InstanceTerminated = DescribeInstances

    waiter InstanceTerminated x = Waiter
        { _waitDelay     = 15
        , _waitAttempts  = 40
        , _waitOperation = x
        , _waitAccept    = const False
        }

data SnapshotCompleted = SnapshotCompleted

instance AWSWaiter SnapshotCompleted where
    type Rq SnapshotCompleted = DescribeSnapshots

    waiter SnapshotCompleted x = Waiter
        { _waitDelay     = 15
        , _waitAttempts  = 40
        , _waitOperation = x
        , _waitAccept    = const False
        }

data SubnetAvailable = SubnetAvailable

instance AWSWaiter SubnetAvailable where
    type Rq SubnetAvailable = DescribeSubnets

    waiter SubnetAvailable x = Waiter
        { _waitDelay     = 15
        , _waitAttempts  = 40
        , _waitOperation = x
        , _waitAccept    = const False
        }

data VolumeAvailable = VolumeAvailable

instance AWSWaiter VolumeAvailable where
    type Rq VolumeAvailable = DescribeVolumes

    waiter VolumeAvailable x = Waiter
        { _waitDelay     = 15
        , _waitAttempts  = 40
        , _waitOperation = x
        , _waitAccept    = const False
        }

data VolumeDeleted = VolumeDeleted

instance AWSWaiter VolumeDeleted where
    type Rq VolumeDeleted = DescribeVolumes

    waiter VolumeDeleted x = Waiter
        { _waitDelay     = 15
        , _waitAttempts  = 40
        , _waitOperation = x
        , _waitAccept    = const False
        }

data VolumeInUse = VolumeInUse

instance AWSWaiter VolumeInUse where
    type Rq VolumeInUse = DescribeVolumes

    waiter VolumeInUse x = Waiter
        { _waitDelay     = 15
        , _waitAttempts  = 40
        , _waitOperation = x
        , _waitAccept    = const False
        }

data VpcAvailable = VpcAvailable

instance AWSWaiter VpcAvailable where
    type Rq VpcAvailable = DescribeVpcs

    waiter VpcAvailable x = Waiter
        { _waitDelay     = 15
        , _waitAttempts  = 40
        , _waitOperation = x
        , _waitAccept    = const False
        }

data VpnConnectionAvailable = VpnConnectionAvailable

instance AWSWaiter VpnConnectionAvailable where
    type Rq VpnConnectionAvailable = DescribeVpnConnections

    waiter VpnConnectionAvailable x = Waiter
        { _waitDelay     = 15
        , _waitAttempts  = 40
        , _waitOperation = x
        , _waitAccept    = const False
        }

data VpnConnectionDeleted = VpnConnectionDeleted

instance AWSWaiter VpnConnectionDeleted where
    type Rq VpnConnectionDeleted = DescribeVpnConnections

    waiter VpnConnectionDeleted x = Waiter
        { _waitDelay     = 15
        , _waitAttempts  = 40
        , _waitOperation = x
        , _waitAccept    = const False
        }
