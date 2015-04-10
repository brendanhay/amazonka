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
import Network.AWS.EC2.DescribeImages
import Network.AWS.EC2.DescribeInstanceStatus
import Network.AWS.EC2.DescribeInstances
import Network.AWS.EC2.DescribeSnapshots
import Network.AWS.EC2.DescribeSpotInstanceRequests
import Network.AWS.EC2.DescribeSubnets
import Network.AWS.EC2.DescribeVolumes
import Network.AWS.EC2.DescribeVpcs
import Network.AWS.EC2.DescribeVpnConnections
import Network.AWS.EC2.GetPasswordData
import Network.AWS.EC2.Types
import Network.AWS.Waiters

bundleTaskComplete :: Wait DescribeBundleTasks
bundleTaskComplete = Wait
    { _waitName      = "BundleTaskComplete"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ matchAll BTSComplete AcceptSuccess
            (folding (concatOf dbtrBundleTasks) . btState)
        , matchAny BTSFailed AcceptFailure
            (folding (concatOf dbtrBundleTasks) . btState)
        ]
    }

conversionTaskCancelled :: Wait DescribeConversionTasks
conversionTaskCancelled = Wait
    { _waitName      = "ConversionTaskCancelled"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ matchAll CTSCancelled AcceptSuccess
            (folding (concatOf dctrConversionTasks) . ctState)
        ]
    }

conversionTaskCompleted :: Wait DescribeConversionTasks
conversionTaskCompleted = Wait
    { _waitName      = "ConversionTaskCompleted"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ matchAll CTSCompleted AcceptSuccess
            (folding (concatOf dctrConversionTasks) . ctState)
        , matchAny CTSCancelled AcceptFailure
            (folding (concatOf dctrConversionTasks) . ctState)
        , matchAny CTSCancelling AcceptFailure
            (folding (concatOf dctrConversionTasks) . ctState)
        ]
    }

customerGatewayAvailable :: Wait DescribeCustomerGateways
customerGatewayAvailable = Wait
    { _waitName      = "CustomerGatewayAvailable"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ matchAll "available" AcceptSuccess
            (folding (concatOf dcgrCustomerGateways) . cgState)
        , matchAny "deleted" AcceptFailure
            (folding (concatOf dcgrCustomerGateways) . cgState)
        , matchAny "deleting" AcceptFailure
            (folding (concatOf dcgrCustomerGateways) . cgState)
        ]
    }

exportTaskCancelled :: Wait DescribeExportTasks
exportTaskCancelled = Wait
    { _waitName      = "ExportTaskCancelled"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ matchAll ETSCancelled AcceptSuccess
            (folding (concatOf detrExportTasks) . etState)
        ]
    }

exportTaskCompleted :: Wait DescribeExportTasks
exportTaskCompleted = Wait
    { _waitName      = "ExportTaskCompleted"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ matchAll ETSCompleted AcceptSuccess
            (folding (concatOf detrExportTasks) . etState)
        ]
    }

imageAvailable :: Wait DescribeImages
imageAvailable = Wait
    { _waitName      = "ImageAvailable"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ matchAll ISAvailable AcceptSuccess
            (folding (concatOf dirImages) . iState)
        , matchAny "failed" AcceptFailure
            (folding (concatOf dirImages) . iState)
        ]
    }

instanceRunning :: Wait DescribeInstances
instanceRunning = Wait
    { _waitName      = "InstanceRunning"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ matchAll ISNRunning AcceptSuccess
            (folding (concatOf dirReservations) . folding (concatOf rInstances) . i1State . isName)
        , matchAny ISNShuttingDown AcceptFailure
            (folding (concatOf dirReservations) . folding (concatOf rInstances) . i1State . isName)
        , matchAny ISNTerminated AcceptFailure
            (folding (concatOf dirReservations) . folding (concatOf rInstances) . i1State . isName)
        , matchAny ISNStopping AcceptFailure
            (folding (concatOf dirReservations) . folding (concatOf rInstances) . i1State . isName)
        ]
    }

instanceStatusOk :: Wait DescribeInstanceStatus
instanceStatusOk = Wait
    { _waitName      = "InstanceStatusOk"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ matchAll SSOk AcceptSuccess
            (folding (concatOf disrInstanceStatuses) . isInstanceStatus . issStatus . _Just)
        ]
    }

instanceStopped :: Wait DescribeInstances
instanceStopped = Wait
    { _waitName      = "InstanceStopped"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ matchAll ISNStopped AcceptSuccess
            (folding (concatOf dirReservations) . folding (concatOf rInstances) . i1State . isName)
        , matchAny ISNPending AcceptFailure
            (folding (concatOf dirReservations) . folding (concatOf rInstances) . i1State . isName)
        , matchAny ISNTerminated AcceptFailure
            (folding (concatOf dirReservations) . folding (concatOf rInstances) . i1State . isName)
        ]
    }

instanceTerminated :: Wait DescribeInstances
instanceTerminated = Wait
    { _waitName      = "InstanceTerminated"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ matchAll ISNTerminated AcceptSuccess
            (folding (concatOf dirReservations) . folding (concatOf rInstances) . i1State . isName)
        , matchAny ISNPending AcceptFailure
            (folding (concatOf dirReservations) . folding (concatOf rInstances) . i1State . isName)
        , matchAny ISNStopping AcceptFailure
            (folding (concatOf dirReservations) . folding (concatOf rInstances) . i1State . isName)
        ]
    }

passwordDataAvailable :: Wait GetPasswordData
passwordDataAvailable = Wait
    { _waitName      = "PasswordDataAvailable"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ matchAll True AcceptSuccess
            (length gpdrPasswordData > 0)
        ]
    }

snapshotCompleted :: Wait DescribeSnapshots
snapshotCompleted = Wait
    { _waitName      = "SnapshotCompleted"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ matchAll Completed AcceptSuccess
            (folding (concatOf dsrSnapshots) . sState)
        ]
    }

spotInstanceRequestFulfilled :: Wait DescribeSpotInstanceRequests
spotInstanceRequestFulfilled = Wait
    { _waitName      = "SpotInstanceRequestFulfilled"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ matchAll "fulfilled" AcceptSuccess
            (folding (concatOf dsirrSpotInstanceRequests) . siStatus . sisCode . _Just)
        , matchAny "schedule-expired" AcceptFailure
            (folding (concatOf dsirrSpotInstanceRequests) . siStatus . sisCode . _Just)
        , matchAny "canceled-before-fulfillment" AcceptFailure
            (folding (concatOf dsirrSpotInstanceRequests) . siStatus . sisCode . _Just)
        , matchAny "bad-parameters" AcceptFailure
            (folding (concatOf dsirrSpotInstanceRequests) . siStatus . sisCode . _Just)
        , matchAny "system-error" AcceptFailure
            (folding (concatOf dsirrSpotInstanceRequests) . siStatus . sisCode . _Just)
        ]
    }

subnetAvailable :: Wait DescribeSubnets
subnetAvailable = Wait
    { _waitName      = "SubnetAvailable"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ matchAll SSAvailable AcceptSuccess
            (folding (concatOf dsrSubnets) . s1State)
        ]
    }

systemStatusOk :: Wait DescribeInstanceStatus
systemStatusOk = Wait
    { _waitName      = "SystemStatusOk"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ matchAll SSOk AcceptSuccess
            (folding (concatOf disrInstanceStatuses) . isSystemStatus . issStatus . _Just)
        ]
    }

volumeAvailable :: Wait DescribeVolumes
volumeAvailable = Wait
    { _waitName      = "VolumeAvailable"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ matchAll VSAvailable AcceptSuccess
            (folding (concatOf dvrVolumes) . vState)
        , matchAny VSDeleted AcceptFailure
            (folding (concatOf dvrVolumes) . vState)
        ]
    }

volumeDeleted :: Wait DescribeVolumes
volumeDeleted = Wait
    { _waitName      = "VolumeDeleted"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ matchAll VSDeleted AcceptSuccess
            (folding (concatOf dvrVolumes) . vState)
        ]
    }

volumeInUse :: Wait DescribeVolumes
volumeInUse = Wait
    { _waitName      = "VolumeInUse"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ matchAll VSInUse AcceptSuccess
            (folding (concatOf dvrVolumes) . vState)
        , matchAny VSDeleted AcceptFailure
            (folding (concatOf dvrVolumes) . vState)
        ]
    }

vpcAvailable :: Wait DescribeVpcs
vpcAvailable = Wait
    { _waitName      = "VpcAvailable"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ matchAll VpcStateAvailable AcceptSuccess
            (folding (concatOf dvrVpcs) . vpcState)
        ]
    }

vpnConnectionAvailable :: Wait DescribeVpnConnections
vpnConnectionAvailable = Wait
    { _waitName      = "VpnConnectionAvailable"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ matchAll VpnStateAvailable AcceptSuccess
            (folding (concatOf dvcrVpnConnections) . vcState)
        , matchAny VpnStateDeleting AcceptFailure
            (folding (concatOf dvcrVpnConnections) . vcState)
        , matchAny VpnStateDeleted AcceptFailure
            (folding (concatOf dvcrVpnConnections) . vcState)
        ]
    }

vpnConnectionDeleted :: Wait DescribeVpnConnections
vpnConnectionDeleted = Wait
    { _waitName      = "VpnConnectionDeleted"
    , _waitAttempts  = 40
    , _waitDelay     = 15
    , _waitAcceptors =
        [ matchAll VpnStateDeleted AcceptSuccess
            (folding (concatOf dvcrVpnConnections) . vcState)
        , matchAny VpnStatePending AcceptFailure
            (folding (concatOf dvcrVpnConnections) . vcState)
        ]
    }
