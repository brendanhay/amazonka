{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Waiters
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Waiters where

import           Network.AWS.EC2.DescribeBundleTasks
import           Network.AWS.EC2.DescribeConversionTasks
import           Network.AWS.EC2.DescribeConversionTasks
import           Network.AWS.EC2.DescribeConversionTasks
import           Network.AWS.EC2.DescribeCustomerGateways
import           Network.AWS.EC2.DescribeExportTasks
import           Network.AWS.EC2.DescribeExportTasks
import           Network.AWS.EC2.DescribeImages
import           Network.AWS.EC2.DescribeInstances
import           Network.AWS.EC2.DescribeInstances
import           Network.AWS.EC2.DescribeInstances
import           Network.AWS.EC2.DescribeInstances
import           Network.AWS.EC2.DescribeInstanceStatus
import           Network.AWS.EC2.DescribeInstanceStatus
import           Network.AWS.EC2.DescribeSnapshots
import           Network.AWS.EC2.DescribeSpotInstanceRequests
import           Network.AWS.EC2.DescribeSubnets
import           Network.AWS.EC2.DescribeVolumes
import           Network.AWS.EC2.DescribeVolumes
import           Network.AWS.EC2.DescribeVolumes
import           Network.AWS.EC2.DescribeVPCs
import           Network.AWS.EC2.DescribeVPNConnections
import           Network.AWS.EC2.DescribeVPNConnections
import           Network.AWS.EC2.GetPasswordData
import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Waiter

instanceTerminated :: Wait DescribeInstances
instanceTerminated =
    Wait
    { _waitName = "InstanceTerminated"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors = [ matchAll
                             "terminated"
                             AcceptSuccess
                             (folding (concatOf diirsReservations) .
                              folding (concatOf rInstances) .
                              insState . isName . to toText)
                       , matchAny
                             "pending"
                             AcceptFailure
                             (folding (concatOf diirsReservations) .
                              folding (concatOf rInstances) .
                              insState . isName . to toText)
                       , matchAny
                             "stopping"
                             AcceptFailure
                             (folding (concatOf diirsReservations) .
                              folding (concatOf rInstances) .
                              insState . isName . to toText)]
    }

volumeInUse :: Wait DescribeVolumes
volumeInUse =
    Wait
    { _waitName = "VolumeInUse"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors = [ matchAll
                             "in-use"
                             AcceptSuccess
                             (folding (concatOf desrsVolumes) .
                              vState . to toText)
                       , matchAny
                             "deleted"
                             AcceptFailure
                             (folding (concatOf desrsVolumes) .
                              vState . to toText)]
    }

subnetAvailable :: Wait DescribeSubnets
subnetAvailable =
    Wait
    { _waitName = "SubnetAvailable"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors = [ matchAll
                             "available"
                             AcceptSuccess
                             (folding (concatOf dsrsSubnets) .
                              subState . to toText)]
    }

systemStatusOK :: Wait DescribeInstanceStatus
systemStatusOK =
    Wait
    { _waitName = "SystemStatusOk"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors = [ matchAll
                             "ok"
                             AcceptSuccess
                             (folding (concatOf disrsInstanceStatuses) .
                              isSystemStatus . _Just . issStatus . to toText)]
    }

customerGatewayAvailable :: Wait DescribeCustomerGateways
customerGatewayAvailable =
    Wait
    { _waitName = "CustomerGatewayAvailable"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors = [ matchAll
                             "available"
                             AcceptSuccess
                             (folding (concatOf dcgrsCustomerGateways) .
                              cgState . to toText)
                       , matchAny
                             "deleted"
                             AcceptFailure
                             (folding (concatOf dcgrsCustomerGateways) .
                              cgState . to toText)
                       , matchAny
                             "deleting"
                             AcceptFailure
                             (folding (concatOf dcgrsCustomerGateways) .
                              cgState . to toText)]
    }

conversionTaskCompleted :: Wait DescribeConversionTasks
conversionTaskCompleted =
    Wait
    { _waitName = "ConversionTaskCompleted"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors = [ matchAll
                             "completed"
                             AcceptSuccess
                             (folding (concatOf dctrsConversionTasks) .
                              ctState . to toText)
                       , matchAny
                             "cancelled"
                             AcceptFailure
                             (folding (concatOf dctrsConversionTasks) .
                              ctState . to toText)
                       , matchAny
                             "cancelling"
                             AcceptFailure
                             (folding (concatOf dctrsConversionTasks) .
                              ctState . to toText)]
    }

conversionTaskDeleted :: Wait DescribeConversionTasks
conversionTaskDeleted =
    Wait
    { _waitName = "ConversionTaskDeleted"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors = [ matchAll
                             "deleted"
                             AcceptSuccess
                             (folding (concatOf dctrsConversionTasks) .
                              ctState . to toText)]
    }

instanceStopped :: Wait DescribeInstances
instanceStopped =
    Wait
    { _waitName = "InstanceStopped"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors = [ matchAll
                             "stopped"
                             AcceptSuccess
                             (folding (concatOf diirsReservations) .
                              folding (concatOf rInstances) .
                              insState . isName . to toText)
                       , matchAny
                             "pending"
                             AcceptFailure
                             (folding (concatOf diirsReservations) .
                              folding (concatOf rInstances) .
                              insState . isName . to toText)
                       , matchAny
                             "terminated"
                             AcceptFailure
                             (folding (concatOf diirsReservations) .
                              folding (concatOf rInstances) .
                              insState . isName . to toText)]
    }

passwordDataAvailable :: Wait GetPasswordData
passwordDataAvailable =
    Wait
    { _waitName = "PasswordDataAvailable"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors = [ matchAll
                             True
                             AcceptSuccess
                             (nonEmpty gpdrsPasswordData)]
    }

instanceRunning :: Wait DescribeInstances
instanceRunning =
    Wait
    { _waitName = "InstanceRunning"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors = [ matchAll
                             "running"
                             AcceptSuccess
                             (folding (concatOf diirsReservations) .
                              folding (concatOf rInstances) .
                              insState . isName . to toText)
                       , matchAny
                             "shutting-down"
                             AcceptFailure
                             (folding (concatOf diirsReservations) .
                              folding (concatOf rInstances) .
                              insState . isName . to toText)
                       , matchAny
                             "terminated"
                             AcceptFailure
                             (folding (concatOf diirsReservations) .
                              folding (concatOf rInstances) .
                              insState . isName . to toText)
                       , matchAny
                             "stopping"
                             AcceptFailure
                             (folding (concatOf diirsReservations) .
                              folding (concatOf rInstances) .
                              insState . isName . to toText)]
    }

spotInstanceRequestFulfilled :: Wait DescribeSpotInstanceRequests
spotInstanceRequestFulfilled =
    Wait
    { _waitName = "SpotInstanceRequestFulfilled"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors = [ matchAll
                             "fulfilled"
                             AcceptSuccess
                             (folding (concatOf dsirrsSpotInstanceRequests) .
                              sirStatus . _Just . sisCode . _Just . to toText)
                       , matchAny
                             "schedule-expired"
                             AcceptFailure
                             (folding (concatOf dsirrsSpotInstanceRequests) .
                              sirStatus . _Just . sisCode . _Just . to toText)
                       , matchAny
                             "canceled-before-fulfillment"
                             AcceptFailure
                             (folding (concatOf dsirrsSpotInstanceRequests) .
                              sirStatus . _Just . sisCode . _Just . to toText)
                       , matchAny
                             "bad-parameters"
                             AcceptFailure
                             (folding (concatOf dsirrsSpotInstanceRequests) .
                              sirStatus . _Just . sisCode . _Just . to toText)
                       , matchAny
                             "system-error"
                             AcceptFailure
                             (folding (concatOf dsirrsSpotInstanceRequests) .
                              sirStatus . _Just . sisCode . _Just . to toText)]
    }

exportTaskCompleted :: Wait DescribeExportTasks
exportTaskCompleted =
    Wait
    { _waitName = "ExportTaskCompleted"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors = [ matchAll
                             "completed"
                             AcceptSuccess
                             (folding (concatOf detrsExportTasks) .
                              etState . to toText)]
    }

vpcAvailable :: Wait DescribeVPCs
vpcAvailable =
    Wait
    { _waitName = "VpcAvailable"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors = [ matchAll
                             "available"
                             AcceptSuccess
                             (folding (concatOf dvsrsVPCs) .
                              vpcState . to toText)]
    }

vpnConnectionAvailable :: Wait DescribeVPNConnections
vpnConnectionAvailable =
    Wait
    { _waitName = "VpnConnectionAvailable"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors = [ matchAll
                             "available"
                             AcceptSuccess
                             (folding (concatOf dvpncrsVPNConnections) .
                              vcState . to toText)
                       , matchAny
                             "deleting"
                             AcceptFailure
                             (folding (concatOf dvpncrsVPNConnections) .
                              vcState . to toText)
                       , matchAny
                             "deleted"
                             AcceptFailure
                             (folding (concatOf dvpncrsVPNConnections) .
                              vcState . to toText)]
    }

exportTaskCancelled :: Wait DescribeExportTasks
exportTaskCancelled =
    Wait
    { _waitName = "ExportTaskCancelled"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors = [ matchAll
                             "cancelled"
                             AcceptSuccess
                             (folding (concatOf detrsExportTasks) .
                              etState . to toText)]
    }

volumeDeleted :: Wait DescribeVolumes
volumeDeleted =
    Wait
    { _waitName = "VolumeDeleted"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors = [ matchAll
                             "deleted"
                             AcceptSuccess
                             (folding (concatOf desrsVolumes) .
                              vState . to toText)
                       , matchError "InvalidVolumeNotFound" AcceptSuccess]
    }

bundleTaskComplete :: Wait DescribeBundleTasks
bundleTaskComplete =
    Wait
    { _waitName = "BundleTaskComplete"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors = [ matchAll
                             "complete"
                             AcceptSuccess
                             (folding (concatOf dbtrsBundleTasks) .
                              btState . to toText)
                       , matchAny
                             "failed"
                             AcceptFailure
                             (folding (concatOf dbtrsBundleTasks) .
                              btState . to toText)]
    }

vpnConnectionDeleted :: Wait DescribeVPNConnections
vpnConnectionDeleted =
    Wait
    { _waitName = "VpnConnectionDeleted"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors = [ matchAll
                             "deleted"
                             AcceptSuccess
                             (folding (concatOf dvpncrsVPNConnections) .
                              vcState . to toText)
                       , matchAny
                             "pending"
                             AcceptFailure
                             (folding (concatOf dvpncrsVPNConnections) .
                              vcState . to toText)]
    }

imageAvailable :: Wait DescribeImages
imageAvailable =
    Wait
    { _waitName = "ImageAvailable"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors = [ matchAll
                             "available"
                             AcceptSuccess
                             (folding (concatOf dessrsImages) .
                              iState . to toText)
                       , matchAny
                             "deregistered"
                             AcceptFailure
                             (folding (concatOf dessrsImages) .
                              iState . to toText)]
    }

conversionTaskCancelled :: Wait DescribeConversionTasks
conversionTaskCancelled =
    Wait
    { _waitName = "ConversionTaskCancelled"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors = [ matchAll
                             "cancelled"
                             AcceptSuccess
                             (folding (concatOf dctrsConversionTasks) .
                              ctState . to toText)]
    }

instanceExists :: Wait DescribeInstances
instanceExists =
    Wait
    { _waitName = "InstanceExists"
    , _waitAttempts = 40
    , _waitDelay = 5
    , _waitAcceptors = [ matchStatus 200 AcceptSuccess
                       , matchError "InvalidInstanceIDNotFound" AcceptRetry]
    }

volumeAvailable :: Wait DescribeVolumes
volumeAvailable =
    Wait
    { _waitName = "VolumeAvailable"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors = [ matchAll
                             "available"
                             AcceptSuccess
                             (folding (concatOf desrsVolumes) .
                              vState . to toText)
                       , matchAny
                             "deleted"
                             AcceptFailure
                             (folding (concatOf desrsVolumes) .
                              vState . to toText)]
    }

snapshotCompleted :: Wait DescribeSnapshots
snapshotCompleted =
    Wait
    { _waitName = "SnapshotCompleted"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors = [ matchAll
                             "completed"
                             AcceptSuccess
                             (folding (concatOf dssrsSnapshots) .
                              sState . to toText)]
    }

instanceStatusOK :: Wait DescribeInstanceStatus
instanceStatusOK =
    Wait
    { _waitName = "InstanceStatusOk"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors = [ matchAll
                             "ok"
                             AcceptSuccess
                             (folding (concatOf disrsInstanceStatuses) .
                              isInstanceStatus . _Just . issStatus . to toText)]
    }
