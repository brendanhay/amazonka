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
import Network.AWS.EC2.Types
import Network.AWS.Waiters

bundleTaskComplete :: Wait DescribeBundleTasks
bundleTaskComplete = Wait
    { _waitName      = "BundleTaskComplete"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ matchAll BTSComplete Success
            (folding (concatOf dbtrBundleTasks) . btState)
        , matchAny BTSFailed Failure
            (folding (concatOf dbtrBundleTasks) . btState)
        ]
    }

conversionTaskCancelled :: Wait DescribeConversionTasks
conversionTaskCancelled = Wait
    { _waitName      = "ConversionTaskCancelled"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ matchAll CTSCancelled Success
            (folding (concatOf dctrConversionTasks) . ctState)
        ]
    }

conversionTaskCompleted :: Wait DescribeConversionTasks
conversionTaskCompleted = Wait
    { _waitName      = "ConversionTaskCompleted"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ matchAll CTSCompleted Success
            (folding (concatOf dctrConversionTasks) . ctState)
        , matchAny CTSCancelled Failure
            (folding (concatOf dctrConversionTasks) . ctState)
        , matchAny CTSCancelling Failure
            (folding (concatOf dctrConversionTasks) . ctState)
        ]
    }

customerGatewayAvailable :: Wait DescribeCustomerGateways
customerGatewayAvailable = Wait
    { _waitName      = "CustomerGatewayAvailable"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ matchAll "available" Success
            (folding (concatOf dcgrCustomerGateways) . cgState)
        , matchAny "deleted" Failure
            (folding (concatOf dcgrCustomerGateways) . cgState)
        , matchAny "deleting" Failure
            (folding (concatOf dcgrCustomerGateways) . cgState)
        ]
    }

exportTaskCancelled :: Wait DescribeExportTasks
exportTaskCancelled = Wait
    { _waitName      = "ExportTaskCancelled"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ matchAll ETSCancelled Success
            (folding (concatOf detrExportTasks) . etState)
        ]
    }

exportTaskCompleted :: Wait DescribeExportTasks
exportTaskCompleted = Wait
    { _waitName      = "ExportTaskCompleted"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ matchAll ETSCompleted Success
            (folding (concatOf detrExportTasks) . etState)
        ]
    }

instanceRunning :: Wait DescribeInstances
instanceRunning = Wait
    { _waitName      = "InstanceRunning"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ matchAll ISNRunning Success
            (folding (concatOf dirReservations) . folding (concatOf rInstances) . i1State . isName)
        , matchAny ISNShuttingDown Failure
            (folding (concatOf dirReservations) . folding (concatOf rInstances) . i1State . isName)
        , matchAny ISNTerminated Failure
            (folding (concatOf dirReservations) . folding (concatOf rInstances) . i1State . isName)
        , matchAny ISNStopping Failure
            (folding (concatOf dirReservations) . folding (concatOf rInstances) . i1State . isName)
        ]
    }

instanceStopped :: Wait DescribeInstances
instanceStopped = Wait
    { _waitName      = "InstanceStopped"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ matchAll ISNStopped Success
            (folding (concatOf dirReservations) . folding (concatOf rInstances) . i1State . isName)
        , matchAny ISNPending Failure
            (folding (concatOf dirReservations) . folding (concatOf rInstances) . i1State . isName)
        , matchAny ISNTerminated Failure
            (folding (concatOf dirReservations) . folding (concatOf rInstances) . i1State . isName)
        ]
    }

instanceTerminated :: Wait DescribeInstances
instanceTerminated = Wait
    { _waitName      = "InstanceTerminated"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ matchAll ISNTerminated Success
            (folding (concatOf dirReservations) . folding (concatOf rInstances) . i1State . isName)
        , matchAny ISNPending Failure
            (folding (concatOf dirReservations) . folding (concatOf rInstances) . i1State . isName)
        , matchAny ISNStopping Failure
            (folding (concatOf dirReservations) . folding (concatOf rInstances) . i1State . isName)
        ]
    }

snapshotCompleted :: Wait DescribeSnapshots
snapshotCompleted = Wait
    { _waitName      = "SnapshotCompleted"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ matchAll Completed Success
            (folding (concatOf dsrSnapshots) . sState)
        ]
    }

subnetAvailable :: Wait DescribeSubnets
subnetAvailable = Wait
    { _waitName      = "SubnetAvailable"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ matchAll SSAvailable Success
            (folding (concatOf dsrSubnets) . s1State)
        ]
    }

volumeAvailable :: Wait DescribeVolumes
volumeAvailable = Wait
    { _waitName      = "VolumeAvailable"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ matchAll VSAvailable Success
            (folding (concatOf dvrVolumes) . vState)
        , matchAny VSDeleted Failure
            (folding (concatOf dvrVolumes) . vState)
        ]
    }

volumeDeleted :: Wait DescribeVolumes
volumeDeleted = Wait
    { _waitName      = "VolumeDeleted"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ matchAll VSDeleted Success
            (folding (concatOf dvrVolumes) . vState)
        ]
    }

volumeInUse :: Wait DescribeVolumes
volumeInUse = Wait
    { _waitName      = "VolumeInUse"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ matchAll VSInUse Success
            (folding (concatOf dvrVolumes) . vState)
        , matchAny VSDeleted Failure
            (folding (concatOf dvrVolumes) . vState)
        ]
    }

vpcAvailable :: Wait DescribeVpcs
vpcAvailable = Wait
    { _waitName      = "VpcAvailable"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ matchAll VpcStateAvailable Success
            (folding (concatOf dvrVpcs) . vpcState)
        ]
    }

vpnConnectionAvailable :: Wait DescribeVpnConnections
vpnConnectionAvailable = Wait
    { _waitName      = "VpnConnectionAvailable"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ matchAll VpnStateAvailable Success
            (folding (concatOf dvcrVpnConnections) . vcState)
        , matchAny VpnStateDeleting Failure
            (folding (concatOf dvcrVpnConnections) . vcState)
        , matchAny VpnStateDeleted Failure
            (folding (concatOf dvcrVpnConnections) . vcState)
        ]
    }

vpnConnectionDeleted :: Wait DescribeVpnConnections
vpnConnectionDeleted = Wait
    { _waitName      = "VpnConnectionDeleted"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ matchAll VpnStateDeleted Success
            (folding (concatOf dvcrVpnConnections) . vcState)
        , matchAny VpnStatePending Failure
            (folding (concatOf dvcrVpnConnections) . vcState)
        ]
    }
