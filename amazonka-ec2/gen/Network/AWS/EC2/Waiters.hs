{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.Waiters
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
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
import Network.AWS.EC2.DescribeConversionTasks
import Network.AWS.EC2.DescribeConversionTasks
import Network.AWS.EC2.DescribeCustomerGateways
import Network.AWS.EC2.DescribeExportTasks
import Network.AWS.EC2.DescribeExportTasks
import Network.AWS.EC2.DescribeImages
import Network.AWS.EC2.DescribeInstanceStatus
import Network.AWS.EC2.DescribeInstanceStatus
import Network.AWS.EC2.DescribeInstances
import Network.AWS.EC2.DescribeInstances
import Network.AWS.EC2.DescribeInstances
import Network.AWS.EC2.DescribeInstances
import Network.AWS.EC2.DescribeSnapshots
import Network.AWS.EC2.DescribeSpotInstanceRequests
import Network.AWS.EC2.DescribeSubnets
import Network.AWS.EC2.DescribeVPCs
import Network.AWS.EC2.DescribeVPNConnections
import Network.AWS.EC2.DescribeVPNConnections
import Network.AWS.EC2.DescribeVolumes
import Network.AWS.EC2.DescribeVolumes
import Network.AWS.EC2.DescribeVolumes
import Network.AWS.EC2.GetPasswordData
import Network.AWS.EC2.Types
import Network.AWS.Prelude
import Network.AWS.Waiters

instanceTerminated :: Wait DescribeInstances
instanceTerminated = Wait{_waitName = "InstanceTerminated", _waitAttempts = 40, _waitDelay = 15, _waitAcceptors = [matchAll "terminated" AcceptSuccess (folding (concatOf dirReservations) . folding (concatOf resInstances) . insState . isName . to toText), matchAny "pending" AcceptFailure (folding (concatOf dirReservations) . folding (concatOf resInstances) . insState . isName . to toText), matchAny "stopping" AcceptFailure (folding (concatOf dirReservations) . folding (concatOf resInstances) . insState . isName . to toText)]};

volumeInUse :: Wait DescribeVolumes
volumeInUse = Wait{_waitName = "VolumeInUse", _waitAttempts = 40, _waitDelay = 15, _waitAcceptors = [matchAll "in-use" AcceptSuccess (folding (concatOf dvrVolumes) . volState . to toText), matchAny "deleted" AcceptFailure (folding (concatOf dvrVolumes) . volState . to toText)]};

subnetAvailable :: Wait DescribeSubnets
subnetAvailable = Wait{_waitName = "SubnetAvailable", _waitAttempts = 40, _waitDelay = 15, _waitAcceptors = [matchAll "available" AcceptSuccess (folding (concatOf dsrSubnets) . subState . to toText)]};

systemStatusOK :: Wait DescribeInstanceStatus
systemStatusOK = Wait{_waitName = "SystemStatusOk", _waitAttempts = 40, _waitDelay = 15, _waitAcceptors = [matchAll "ok" AcceptSuccess (folding (concatOf disrInstanceStatuses) . isSystemStatus . _Just . issStatus . to toText)]};

customerGatewayAvailable :: Wait DescribeCustomerGateways
customerGatewayAvailable = Wait{_waitName = "CustomerGatewayAvailable", _waitAttempts = 40, _waitDelay = 15, _waitAcceptors = [matchAll "available" AcceptSuccess (folding (concatOf dcgrCustomerGateways) . cgState . to toText), matchAny "deleted" AcceptFailure (folding (concatOf dcgrCustomerGateways) . cgState . to toText), matchAny "deleting" AcceptFailure (folding (concatOf dcgrCustomerGateways) . cgState . to toText)]};

conversionTaskCompleted :: Wait DescribeConversionTasks
conversionTaskCompleted = Wait{_waitName = "ConversionTaskCompleted", _waitAttempts = 40, _waitDelay = 15, _waitAcceptors = [matchAll "completed" AcceptSuccess (folding (concatOf dctrConversionTasks) . ctState . to toText), matchAny "cancelled" AcceptFailure (folding (concatOf dctrConversionTasks) . ctState . to toText), matchAny "cancelling" AcceptFailure (folding (concatOf dctrConversionTasks) . ctState . to toText)]};

conversionTaskDeleted :: Wait DescribeConversionTasks
conversionTaskDeleted = Wait{_waitName = "ConversionTaskDeleted", _waitAttempts = 40, _waitDelay = 15, _waitAcceptors = [matchAll "deleted" AcceptSuccess (folding (concatOf dctrConversionTasks) . ctState . to toText)]};

instanceStopped :: Wait DescribeInstances
instanceStopped = Wait{_waitName = "InstanceStopped", _waitAttempts = 40, _waitDelay = 15, _waitAcceptors = [matchAll "stopped" AcceptSuccess (folding (concatOf dirReservations) . folding (concatOf resInstances) . insState . isName . to toText), matchAny "pending" AcceptFailure (folding (concatOf dirReservations) . folding (concatOf resInstances) . insState . isName . to toText), matchAny "terminated" AcceptFailure (folding (concatOf dirReservations) . folding (concatOf resInstances) . insState . isName . to toText)]};

passwordDataAvailable :: Wait GetPasswordData
passwordDataAvailable = Wait{_waitName = "PasswordDataAvailable", _waitAttempts = 40, _waitDelay = 15, _waitAcceptors = [matchAll True AcceptSuccess (nonEmpty gpdrPasswordData)]};

instanceRunning :: Wait DescribeInstances
instanceRunning = Wait{_waitName = "InstanceRunning", _waitAttempts = 40, _waitDelay = 15, _waitAcceptors = [matchAll "running" AcceptSuccess (folding (concatOf dirReservations) . folding (concatOf resInstances) . insState . isName . to toText), matchAny "shutting-down" AcceptFailure (folding (concatOf dirReservations) . folding (concatOf resInstances) . insState . isName . to toText), matchAny "terminated" AcceptFailure (folding (concatOf dirReservations) . folding (concatOf resInstances) . insState . isName . to toText), matchAny "stopping" AcceptFailure (folding (concatOf dirReservations) . folding (concatOf resInstances) . insState . isName . to toText)]};

spotInstanceRequestFulfilled :: Wait DescribeSpotInstanceRequests
spotInstanceRequestFulfilled = Wait{_waitName = "SpotInstanceRequestFulfilled", _waitAttempts = 40, _waitDelay = 15, _waitAcceptors = [matchAll "fulfilled" AcceptSuccess (folding (concatOf dsirrSpotInstanceRequests) . sirStatus . _Just . sisCode . _Just . to toText), matchAny "schedule-expired" AcceptFailure (folding (concatOf dsirrSpotInstanceRequests) . sirStatus . _Just . sisCode . _Just . to toText), matchAny "canceled-before-fulfillment" AcceptFailure (folding (concatOf dsirrSpotInstanceRequests) . sirStatus . _Just . sisCode . _Just . to toText), matchAny "bad-parameters" AcceptFailure (folding (concatOf dsirrSpotInstanceRequests) . sirStatus . _Just . sisCode . _Just . to toText), matchAny "system-error" AcceptFailure (folding (concatOf dsirrSpotInstanceRequests) . sirStatus . _Just . sisCode . _Just . to toText)]};

exportTaskCompleted :: Wait DescribeExportTasks
exportTaskCompleted = Wait{_waitName = "ExportTaskCompleted", _waitAttempts = 40, _waitDelay = 15, _waitAcceptors = [matchAll "completed" AcceptSuccess (folding (concatOf detrExportTasks) . etState . to toText)]};

vpcAvailable :: Wait DescribeVPCs
vpcAvailable = Wait{_waitName = "VpcAvailable", _waitAttempts = 40, _waitDelay = 15, _waitAcceptors = [matchAll "available" AcceptSuccess (folding (concatOf dvrVPCs) . vpcState . to toText)]};

vpnConnectionAvailable :: Wait DescribeVPNConnections
vpnConnectionAvailable = Wait{_waitName = "VpnConnectionAvailable", _waitAttempts = 40, _waitDelay = 15, _waitAcceptors = [matchAll "available" AcceptSuccess (folding (concatOf dvcrVPNConnections) . vcState . to toText), matchAny "deleting" AcceptFailure (folding (concatOf dvcrVPNConnections) . vcState . to toText), matchAny "deleted" AcceptFailure (folding (concatOf dvcrVPNConnections) . vcState . to toText)]};

exportTaskCancelled :: Wait DescribeExportTasks
exportTaskCancelled = Wait{_waitName = "ExportTaskCancelled", _waitAttempts = 40, _waitDelay = 15, _waitAcceptors = [matchAll "cancelled" AcceptSuccess (folding (concatOf detrExportTasks) . etState . to toText)]};

volumeDeleted :: Wait DescribeVolumes
volumeDeleted = Wait{_waitName = "VolumeDeleted", _waitAttempts = 40, _waitDelay = 15, _waitAcceptors = [matchAll "deleted" AcceptSuccess (folding (concatOf dvrVolumes) . volState . to toText), matchError "InvalidVolumeNotFound" AcceptSuccess]};

bundleTaskComplete :: Wait DescribeBundleTasks
bundleTaskComplete = Wait{_waitName = "BundleTaskComplete", _waitAttempts = 40, _waitDelay = 15, _waitAcceptors = [matchAll "complete" AcceptSuccess (folding (concatOf dbtrBundleTasks) . btState . to toText), matchAny "failed" AcceptFailure (folding (concatOf dbtrBundleTasks) . btState . to toText)]};

vpnConnectionDeleted :: Wait DescribeVPNConnections
vpnConnectionDeleted = Wait{_waitName = "VpnConnectionDeleted", _waitAttempts = 40, _waitDelay = 15, _waitAcceptors = [matchAll "deleted" AcceptSuccess (folding (concatOf dvcrVPNConnections) . vcState . to toText), matchAny "pending" AcceptFailure (folding (concatOf dvcrVPNConnections) . vcState . to toText)]};

imageAvailable :: Wait DescribeImages
imageAvailable = Wait{_waitName = "ImageAvailable", _waitAttempts = 40, _waitDelay = 15, _waitAcceptors = [matchAll "available" AcceptSuccess (folding (concatOf dirImages) . imaState . to toText), matchAny "deregistered" AcceptFailure (folding (concatOf dirImages) . imaState . to toText)]};

conversionTaskCancelled :: Wait DescribeConversionTasks
conversionTaskCancelled = Wait{_waitName = "ConversionTaskCancelled", _waitAttempts = 40, _waitDelay = 15, _waitAcceptors = [matchAll "cancelled" AcceptSuccess (folding (concatOf dctrConversionTasks) . ctState . to toText)]};

instanceExists :: Wait DescribeInstances
instanceExists = Wait{_waitName = "InstanceExists", _waitAttempts = 40, _waitDelay = 5, _waitAcceptors = [matchStatus 200 AcceptSuccess, matchError "InvalidInstanceIDNotFound" AcceptRetry]};

volumeAvailable :: Wait DescribeVolumes
volumeAvailable = Wait{_waitName = "VolumeAvailable", _waitAttempts = 40, _waitDelay = 15, _waitAcceptors = [matchAll "available" AcceptSuccess (folding (concatOf dvrVolumes) . volState . to toText), matchAny "deleted" AcceptFailure (folding (concatOf dvrVolumes) . volState . to toText)]};

snapshotCompleted :: Wait DescribeSnapshots
snapshotCompleted = Wait{_waitName = "SnapshotCompleted", _waitAttempts = 40, _waitDelay = 15, _waitAcceptors = [matchAll "completed" AcceptSuccess (folding (concatOf dsrSnapshots) . snaState . to toText)]};

instanceStatusOK :: Wait DescribeInstanceStatus
instanceStatusOK = Wait{_waitName = "InstanceStatusOk", _waitAttempts = 40, _waitDelay = 15, _waitAcceptors = [matchAll "ok" AcceptSuccess (folding (concatOf disrInstanceStatuses) . isInstanceStatus . _Just . issStatus . to toText)]};
