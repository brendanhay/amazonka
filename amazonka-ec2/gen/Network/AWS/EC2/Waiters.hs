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
import Network.AWS.Types

bundleTaskComplete :: Wait DescribeBundleTasks
bundleTaskComplete = Wait
    { _waitName     = "BundleTaskComplete"
    , _waitDelay    = 15
    , _waitAttempts = 40
    , _waitAccept   = const True
    }

conversionTaskCancelled :: Wait DescribeConversionTasks
conversionTaskCancelled = Wait
    { _waitName     = "ConversionTaskCancelled"
    , _waitDelay    = 15
    , _waitAttempts = 40
    , _waitAccept   = const True
    }

conversionTaskCompleted :: Wait DescribeConversionTasks
conversionTaskCompleted = Wait
    { _waitName     = "ConversionTaskCompleted"
    , _waitDelay    = 15
    , _waitAttempts = 40
    , _waitAccept   = const True
    }

conversionTaskDeleted :: Wait DescribeConversionTasks
conversionTaskDeleted = Wait
    { _waitName     = "ConversionTaskDeleted"
    , _waitDelay    = 15
    , _waitAttempts = 40
    , _waitAccept   = const True
    }

customerGatewayAvailable :: Wait DescribeCustomerGateways
customerGatewayAvailable = Wait
    { _waitName     = "CustomerGatewayAvailable"
    , _waitDelay    = 15
    , _waitAttempts = 40
    , _waitAccept   = const True
    }

exportTaskCancelled :: Wait DescribeExportTasks
exportTaskCancelled = Wait
    { _waitName     = "ExportTaskCancelled"
    , _waitDelay    = 15
    , _waitAttempts = 40
    , _waitAccept   = const True
    }

exportTaskCompleted :: Wait DescribeExportTasks
exportTaskCompleted = Wait
    { _waitName     = "ExportTaskCompleted"
    , _waitDelay    = 15
    , _waitAttempts = 40
    , _waitAccept   = const True
    }

instanceRunning :: Wait DescribeInstances
instanceRunning = Wait
    { _waitName     = "InstanceRunning"
    , _waitDelay    = 15
    , _waitAttempts = 40
    , _waitAccept   = const True
    }

instanceStopped :: Wait DescribeInstances
instanceStopped = Wait
    { _waitName     = "InstanceStopped"
    , _waitDelay    = 15
    , _waitAttempts = 40
    , _waitAccept   = const True
    }

instanceTerminated :: Wait DescribeInstances
instanceTerminated = Wait
    { _waitName     = "InstanceTerminated"
    , _waitDelay    = 15
    , _waitAttempts = 40
    , _waitAccept   = const True
    }

snapshotCompleted :: Wait DescribeSnapshots
snapshotCompleted = Wait
    { _waitName     = "SnapshotCompleted"
    , _waitDelay    = 15
    , _waitAttempts = 40
    , _waitAccept   = const True
    }

subnetAvailable :: Wait DescribeSubnets
subnetAvailable = Wait
    { _waitName     = "SubnetAvailable"
    , _waitDelay    = 15
    , _waitAttempts = 40
    , _waitAccept   = const True
    }

volumeAvailable :: Wait DescribeVolumes
volumeAvailable = Wait
    { _waitName     = "VolumeAvailable"
    , _waitDelay    = 15
    , _waitAttempts = 40
    , _waitAccept   = const True
    }

volumeDeleted :: Wait DescribeVolumes
volumeDeleted = Wait
    { _waitName     = "VolumeDeleted"
    , _waitDelay    = 15
    , _waitAttempts = 40
    , _waitAccept   = const True
    }

volumeInUse :: Wait DescribeVolumes
volumeInUse = Wait
    { _waitName     = "VolumeInUse"
    , _waitDelay    = 15
    , _waitAttempts = 40
    , _waitAccept   = const True
    }

vpcAvailable :: Wait DescribeVpcs
vpcAvailable = Wait
    { _waitName     = "VpcAvailable"
    , _waitDelay    = 15
    , _waitAttempts = 40
    , _waitAccept   = const True
    }

vpnConnectionAvailable :: Wait DescribeVpnConnections
vpnConnectionAvailable = Wait
    { _waitName     = "VpnConnectionAvailable"
    , _waitDelay    = 15
    , _waitAttempts = 40
    , _waitAccept   = const True
    }

vpnConnectionDeleted :: Wait DescribeVpnConnections
vpnConnectionDeleted = Wait
    { _waitName     = "VpnConnectionDeleted"
    , _waitDelay    = 15
    , _waitAttempts = 40
    , _waitAccept   = const True
    }
