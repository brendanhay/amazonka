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
        [ pathAll Success {"contents":["dbtrBundleTasks",{"contents":"btState","type":"access"}],"type":"indexed"} "complete"
        , pathAny Failure {"contents":["dbtrBundleTasks",{"contents":"btState","type":"access"}],"type":"indexed"} "failed"
        ]
    }

conversionTaskCancelled :: Wait DescribeConversionTasks
conversionTaskCancelled = Wait
    { _waitName      = "ConversionTaskCancelled"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ pathAll Success {"contents":["dctrConversionTasks",{"contents":"ctState","type":"access"}],"type":"indexed"} "cancelled"
        ]
    }

conversionTaskCompleted :: Wait DescribeConversionTasks
conversionTaskCompleted = Wait
    { _waitName      = "ConversionTaskCompleted"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ pathAll Success {"contents":["dctrConversionTasks",{"contents":"ctState","type":"access"}],"type":"indexed"} "completed"
        , pathAny Failure {"contents":["dctrConversionTasks",{"contents":"ctState","type":"access"}],"type":"indexed"} "cancelled"
        , pathAny Failure {"contents":["dctrConversionTasks",{"contents":"ctState","type":"access"}],"type":"indexed"} "cancelling"
        ]
    }

conversionTaskDeleted :: Wait DescribeConversionTasks
conversionTaskDeleted = Wait
    { _waitName      = "ConversionTaskDeleted"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ pathAll Success {"contents":["dctrConversionTasks",{"contents":"ctState","type":"access"}],"type":"indexed"} "deleted"
        ]
    }

customerGatewayAvailable :: Wait DescribeCustomerGateways
customerGatewayAvailable = Wait
    { _waitName      = "CustomerGatewayAvailable"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ pathAll Success {"contents":["dcgrCustomerGateways",{"contents":"cgState","type":"access"}],"type":"indexed"} "available"
        , pathAny Failure {"contents":["dcgrCustomerGateways",{"contents":"cgState","type":"access"}],"type":"indexed"} "deleted"
        , pathAny Failure {"contents":["dcgrCustomerGateways",{"contents":"cgState","type":"access"}],"type":"indexed"} "deleting"
        ]
    }

exportTaskCancelled :: Wait DescribeExportTasks
exportTaskCancelled = Wait
    { _waitName      = "ExportTaskCancelled"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ pathAll Success {"contents":["detrExportTasks",{"contents":"etState","type":"access"}],"type":"indexed"} "cancelled"
        ]
    }

exportTaskCompleted :: Wait DescribeExportTasks
exportTaskCompleted = Wait
    { _waitName      = "ExportTaskCompleted"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ pathAll Success {"contents":["detrExportTasks",{"contents":"etState","type":"access"}],"type":"indexed"} "completed"
        ]
    }

instanceRunning :: Wait DescribeInstances
instanceRunning = Wait
    { _waitName      = "InstanceRunning"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ pathAll Success {"contents":["dirReservations",{"contents":["rInstances",{"contents":["i1State",{"contents":"isName","type":"access"}],"type":"nested"}],"type":"indexed"}],"type":"indexed"} "running"
        , pathAny Failure {"contents":["dirReservations",{"contents":["rInstances",{"contents":["i1State",{"contents":"isName","type":"access"}],"type":"nested"}],"type":"indexed"}],"type":"indexed"} "shutting-down"
        , pathAny Failure {"contents":["dirReservations",{"contents":["rInstances",{"contents":["i1State",{"contents":"isName","type":"access"}],"type":"nested"}],"type":"indexed"}],"type":"indexed"} "terminated"
        , pathAny Failure {"contents":["dirReservations",{"contents":["rInstances",{"contents":["i1State",{"contents":"isName","type":"access"}],"type":"nested"}],"type":"indexed"}],"type":"indexed"} "stopping"
        ]
    }

instanceStopped :: Wait DescribeInstances
instanceStopped = Wait
    { _waitName      = "InstanceStopped"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ pathAll Success {"contents":["dirReservations",{"contents":["rInstances",{"contents":["i1State",{"contents":"isName","type":"access"}],"type":"nested"}],"type":"indexed"}],"type":"indexed"} "stopped"
        , pathAny Failure {"contents":["dirReservations",{"contents":["rInstances",{"contents":["i1State",{"contents":"isName","type":"access"}],"type":"nested"}],"type":"indexed"}],"type":"indexed"} "pending"
        , pathAny Failure {"contents":["dirReservations",{"contents":["rInstances",{"contents":["i1State",{"contents":"isName","type":"access"}],"type":"nested"}],"type":"indexed"}],"type":"indexed"} "terminated"
        ]
    }

instanceTerminated :: Wait DescribeInstances
instanceTerminated = Wait
    { _waitName      = "InstanceTerminated"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ pathAll Success {"contents":["dirReservations",{"contents":["rInstances",{"contents":["i1State",{"contents":"isName","type":"access"}],"type":"nested"}],"type":"indexed"}],"type":"indexed"} "terminated"
        , pathAny Failure {"contents":["dirReservations",{"contents":["rInstances",{"contents":["i1State",{"contents":"isName","type":"access"}],"type":"nested"}],"type":"indexed"}],"type":"indexed"} "pending"
        , pathAny Failure {"contents":["dirReservations",{"contents":["rInstances",{"contents":["i1State",{"contents":"isName","type":"access"}],"type":"nested"}],"type":"indexed"}],"type":"indexed"} "stopping"
        ]
    }

snapshotCompleted :: Wait DescribeSnapshots
snapshotCompleted = Wait
    { _waitName      = "SnapshotCompleted"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ pathAll Success {"contents":["dsrSnapshots",{"contents":"sState","type":"access"}],"type":"indexed"} "completed"
        ]
    }

subnetAvailable :: Wait DescribeSubnets
subnetAvailable = Wait
    { _waitName      = "SubnetAvailable"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ pathAll Success {"contents":["dsrSubnets",{"contents":"s1State","type":"access"}],"type":"indexed"} "available"
        ]
    }

volumeAvailable :: Wait DescribeVolumes
volumeAvailable = Wait
    { _waitName      = "VolumeAvailable"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ pathAll Success {"contents":["dvrVolumes",{"contents":"vState","type":"access"}],"type":"indexed"} "available"
        , pathAny Failure {"contents":["dvrVolumes",{"contents":"vState","type":"access"}],"type":"indexed"} "deleted"
        ]
    }

volumeDeleted :: Wait DescribeVolumes
volumeDeleted = Wait
    { _waitName      = "VolumeDeleted"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ pathAll Success {"contents":["dvrVolumes",{"contents":"vState","type":"access"}],"type":"indexed"} "deleted"
        ]
    }

volumeInUse :: Wait DescribeVolumes
volumeInUse = Wait
    { _waitName      = "VolumeInUse"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ pathAll Success {"contents":["dvrVolumes",{"contents":"vState","type":"access"}],"type":"indexed"} "in-use"
        , pathAny Failure {"contents":["dvrVolumes",{"contents":"vState","type":"access"}],"type":"indexed"} "deleted"
        ]
    }

vpcAvailable :: Wait DescribeVpcs
vpcAvailable = Wait
    { _waitName      = "VpcAvailable"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ pathAll Success {"contents":["dvrVpcs",{"contents":"vpcState","type":"access"}],"type":"indexed"} "available"
        ]
    }

vpnConnectionAvailable :: Wait DescribeVpnConnections
vpnConnectionAvailable = Wait
    { _waitName      = "VpnConnectionAvailable"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ pathAll Success {"contents":["dvcrVpnConnections",{"contents":"vcState","type":"access"}],"type":"indexed"} "available"
        , pathAny Failure {"contents":["dvcrVpnConnections",{"contents":"vcState","type":"access"}],"type":"indexed"} "deleting"
        , pathAny Failure {"contents":["dvcrVpnConnections",{"contents":"vcState","type":"access"}],"type":"indexed"} "deleted"
        ]
    }

vpnConnectionDeleted :: Wait DescribeVpnConnections
vpnConnectionDeleted = Wait
    { _waitName      = "VpnConnectionDeleted"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ pathAll Success {"contents":["dvcrVpnConnections",{"contents":"vcState","type":"access"}],"type":"indexed"} "deleted"
        , pathAny Failure {"contents":["dvcrVpnConnections",{"contents":"vcState","type":"access"}],"type":"indexed"} "pending"
        ]
    }
