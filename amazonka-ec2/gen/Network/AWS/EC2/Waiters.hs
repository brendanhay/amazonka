{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

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
import Network.AWS.Waiter

bundleTaskComplete :: Wait DescribeBundleTasks
bundleTaskComplete = Wait
    { _waitName      = "BundleTaskComplete"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ pathAll (dbtrBundleTasks . btState) "complete" Success
        , pathAny (dbtrBundleTasks . btState) "failed" Failure
        ]
    }

conversionTaskCancelled :: Wait DescribeConversionTasks
conversionTaskCancelled = Wait
    { _waitName      = "ConversionTaskCancelled"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ pathAll (dctrConversionTasks . ctState) "cancelled" Success
        ]
    }

conversionTaskCompleted :: Wait DescribeConversionTasks
conversionTaskCompleted = Wait
    { _waitName      = "ConversionTaskCompleted"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ pathAll (dctrConversionTasks . ctState) "completed" Success
        , pathAny (dctrConversionTasks . ctState) "cancelled" Failure
        , pathAny (dctrConversionTasks . ctState) "cancelling" Failure
        ]
    }

conversionTaskDeleted :: Wait DescribeConversionTasks
conversionTaskDeleted = Wait
    { _waitName      = "ConversionTaskDeleted"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ pathAll (dctrConversionTasks . ctState) "deleted" Success
        ]
    }

customerGatewayAvailable :: Wait DescribeCustomerGateways
customerGatewayAvailable = Wait
    { _waitName      = "CustomerGatewayAvailable"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ pathAll (dcgrCustomerGateways . cgState) "available" Success
        , pathAny (dcgrCustomerGateways . cgState) "deleted" Failure
        , pathAny (dcgrCustomerGateways . cgState) "deleting" Failure
        ]
    }

exportTaskCancelled :: Wait DescribeExportTasks
exportTaskCancelled = Wait
    { _waitName      = "ExportTaskCancelled"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ pathAll (detrExportTasks . etState) "cancelled" Success
        ]
    }

exportTaskCompleted :: Wait DescribeExportTasks
exportTaskCompleted = Wait
    { _waitName      = "ExportTaskCompleted"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ pathAll (detrExportTasks . etState) "completed" Success
        ]
    }

instanceRunning :: Wait DescribeInstances
instanceRunning = Wait
    { _waitName      = "InstanceRunning"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ pathAll (dirReservations . rInstances . i1State . isName) "running" Success
        , pathAny (dirReservations . rInstances . i1State . isName) "shutting-down" Failure
        , pathAny (dirReservations . rInstances . i1State . isName) "terminated" Failure
        , pathAny (dirReservations . rInstances . i1State . isName) "stopping" Failure
        ]
    }

instanceStopped :: Wait DescribeInstances
instanceStopped = Wait
    { _waitName      = "InstanceStopped"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ pathAll (dirReservations . rInstances . i1State . isName) "stopped" Success
        , pathAny (dirReservations . rInstances . i1State . isName) "pending" Failure
        , pathAny (dirReservations . rInstances . i1State . isName) "terminated" Failure
        ]
    }

instanceTerminated :: Wait DescribeInstances
instanceTerminated = Wait
    { _waitName      = "InstanceTerminated"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ pathAll (dirReservations . rInstances . i1State . isName) "terminated" Success
        , pathAny (dirReservations . rInstances . i1State . isName) "pending" Failure
        , pathAny (dirReservations . rInstances . i1State . isName) "stopping" Failure
        ]
    }

snapshotCompleted :: Wait DescribeSnapshots
snapshotCompleted = Wait
    { _waitName      = "SnapshotCompleted"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ pathAll (dsrSnapshots . sState) "completed" Success
        ]
    }

subnetAvailable :: Wait DescribeSubnets
subnetAvailable = Wait
    { _waitName      = "SubnetAvailable"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ pathAll (dsrSubnets . s1State) "available" Success
        ]
    }

volumeAvailable :: Wait DescribeVolumes
volumeAvailable = Wait
    { _waitName      = "VolumeAvailable"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ pathAll (dvrVolumes . vState) "available" Success
        , pathAny (dvrVolumes . vState) "deleted" Failure
        ]
    }

volumeDeleted :: Wait DescribeVolumes
volumeDeleted = Wait
    { _waitName      = "VolumeDeleted"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ pathAll (dvrVolumes . vState) "deleted" Success
        ]
    }

volumeInUse :: Wait DescribeVolumes
volumeInUse = Wait
    { _waitName      = "VolumeInUse"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ pathAll (dvrVolumes . vState) "in-use" Success
        , pathAny (dvrVolumes . vState) "deleted" Failure
        ]
    }

vpcAvailable :: Wait DescribeVpcs
vpcAvailable = Wait
    { _waitName      = "VpcAvailable"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ pathAll (dvrVpcs . vpcState) "available" Success
        ]
    }

vpnConnectionAvailable :: Wait DescribeVpnConnections
vpnConnectionAvailable = Wait
    { _waitName      = "VpnConnectionAvailable"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ pathAll (dvcrVpnConnections . vcState) "available" Success
        , pathAny (dvcrVpnConnections . vcState) "deleting" Failure
        , pathAny (dvcrVpnConnections . vcState) "deleted" Failure
        ]
    }

vpnConnectionDeleted :: Wait DescribeVpnConnections
vpnConnectionDeleted = Wait
    { _waitName      = "VpnConnectionDeleted"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ pathAll (dvcrVpnConnections . vcState) "deleted" Success
        , pathAny (dvcrVpnConnections . vcState) "pending" Failure
        ]
    }
