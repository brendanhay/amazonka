{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Waiters
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Waiters where

import Network.AWS.EC2.DescribeBundleTasks
import Network.AWS.EC2.DescribeConversionTasks
import Network.AWS.EC2.DescribeConversionTasks
import Network.AWS.EC2.DescribeConversionTasks
import Network.AWS.EC2.DescribeCustomerGateways
import Network.AWS.EC2.DescribeExportTasks
import Network.AWS.EC2.DescribeExportTasks
import Network.AWS.EC2.DescribeImages
import Network.AWS.EC2.DescribeInstances
import Network.AWS.EC2.DescribeInstances
import Network.AWS.EC2.DescribeInstances
import Network.AWS.EC2.DescribeInstances
import Network.AWS.EC2.DescribeInstanceStatus
import Network.AWS.EC2.DescribeInstanceStatus
import Network.AWS.EC2.DescribeNatGateways
import Network.AWS.EC2.DescribeNetworkInterfaces
import Network.AWS.EC2.DescribeSnapshots
import Network.AWS.EC2.DescribeSpotInstanceRequests
import Network.AWS.EC2.DescribeSubnets
import Network.AWS.EC2.DescribeVolumes
import Network.AWS.EC2.DescribeVolumes
import Network.AWS.EC2.DescribeVolumes
import Network.AWS.EC2.DescribeVPCPeeringConnections
import Network.AWS.EC2.DescribeVPCPeeringConnections
import Network.AWS.EC2.DescribeVPCs
import Network.AWS.EC2.DescribeVPCs
import Network.AWS.EC2.DescribeVPNConnections
import Network.AWS.EC2.DescribeVPNConnections
import Network.AWS.EC2.GetPasswordData
import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Waiter

-- | Polls 'Network.AWS.EC2.DescribeInstances' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
instanceTerminated :: Wait DescribeInstances
instanceTerminated =
  Wait
    { _waitName = "InstanceTerminated"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors =
        [ matchAll
            "terminated"
            AcceptSuccess
            (folding (concatOf dirsReservations) .
             folding (concatOf rInstances) . insState . isName . to toTextCI)
        , matchAny
            "pending"
            AcceptFailure
            (folding (concatOf dirsReservations) .
             folding (concatOf rInstances) . insState . isName . to toTextCI)
        , matchAny
            "stopping"
            AcceptFailure
            (folding (concatOf dirsReservations) .
             folding (concatOf rInstances) . insState . isName . to toTextCI)
        ]
    }


-- | Polls 'Network.AWS.EC2.DescribeVolumes' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
volumeInUse :: Wait DescribeVolumes
volumeInUse =
  Wait
    { _waitName = "VolumeInUse"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors =
        [ matchAll
            "in-use"
            AcceptSuccess
            (folding (concatOf dvvrsVolumes) . vState . to toTextCI)
        , matchAny
            "deleted"
            AcceptFailure
            (folding (concatOf dvvrsVolumes) . vState . to toTextCI)
        ]
    }


-- | Polls 'Network.AWS.EC2.DescribeNatGateways' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
natGatewayAvailable :: Wait DescribeNatGateways
natGatewayAvailable =
  Wait
    { _waitName = "NatGatewayAvailable"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors =
        [ matchAll
            "available"
            AcceptSuccess
            (folding (concatOf dngrsNatGateways) . ngState . _Just . to toTextCI)
        , matchAny
            "failed"
            AcceptFailure
            (folding (concatOf dngrsNatGateways) . ngState . _Just . to toTextCI)
        , matchAny
            "deleting"
            AcceptFailure
            (folding (concatOf dngrsNatGateways) . ngState . _Just . to toTextCI)
        , matchAny
            "deleted"
            AcceptFailure
            (folding (concatOf dngrsNatGateways) . ngState . _Just . to toTextCI)
        , matchError "NatGatewayNotFound" AcceptRetry
        ]
    }


-- | Polls 'Network.AWS.EC2.DescribeSubnets' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
subnetAvailable :: Wait DescribeSubnets
subnetAvailable =
  Wait
    { _waitName = "SubnetAvailable"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors =
        [ matchAll
            "available"
            AcceptSuccess
            (folding (concatOf dsrsSubnets) . subState . to toTextCI)
        ]
    }


-- | Polls 'Network.AWS.EC2.DescribeNetworkInterfaces' every 20 seconds until a successful state is reached. An error is returned after 10 failed checks.
networkInterfaceAvailable :: Wait DescribeNetworkInterfaces
networkInterfaceAvailable =
  Wait
    { _waitName = "NetworkInterfaceAvailable"
    , _waitAttempts = 10
    , _waitDelay = 20
    , _waitAcceptors =
        [ matchAll
            "available"
            AcceptSuccess
            (folding (concatOf dnirsNetworkInterfaces) .
             niStatus . _Just . to toTextCI)
        , matchError "InvalidNetworkInterfaceID.NotFound" AcceptFailure
        ]
    }


-- | Polls 'Network.AWS.EC2.DescribeInstanceStatus' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
systemStatusOK :: Wait DescribeInstanceStatus
systemStatusOK =
  Wait
    { _waitName = "SystemStatusOk"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors =
        [ matchAll
            "ok"
            AcceptSuccess
            (folding (concatOf disrsInstanceStatuses) .
             isSystemStatus . _Just . issStatus . to toTextCI)
        ]
    }


-- | Polls 'Network.AWS.EC2.DescribeCustomerGateways' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
customerGatewayAvailable :: Wait DescribeCustomerGateways
customerGatewayAvailable =
  Wait
    { _waitName = "CustomerGatewayAvailable"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors =
        [ matchAll
            "available"
            AcceptSuccess
            (folding (concatOf dcgrsCustomerGateways) . cgState . to toTextCI)
        , matchAny
            "deleted"
            AcceptFailure
            (folding (concatOf dcgrsCustomerGateways) . cgState . to toTextCI)
        , matchAny
            "deleting"
            AcceptFailure
            (folding (concatOf dcgrsCustomerGateways) . cgState . to toTextCI)
        ]
    }


-- | Polls 'Network.AWS.EC2.DescribeConversionTasks' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
conversionTaskCompleted :: Wait DescribeConversionTasks
conversionTaskCompleted =
  Wait
    { _waitName = "ConversionTaskCompleted"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors =
        [ matchAll
            "completed"
            AcceptSuccess
            (folding (concatOf dctrsConversionTasks) .
             ctState . _Just . to toTextCI)
        , matchAny
            "cancelled"
            AcceptFailure
            (folding (concatOf dctrsConversionTasks) .
             ctState . _Just . to toTextCI)
        , matchAny
            "cancelling"
            AcceptFailure
            (folding (concatOf dctrsConversionTasks) .
             ctState . _Just . to toTextCI)
        ]
    }


-- | Polls 'Network.AWS.EC2.DescribeInstances' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
instanceStopped :: Wait DescribeInstances
instanceStopped =
  Wait
    { _waitName = "InstanceStopped"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors =
        [ matchAll
            "stopped"
            AcceptSuccess
            (folding (concatOf dirsReservations) .
             folding (concatOf rInstances) . insState . isName . to toTextCI)
        , matchAny
            "pending"
            AcceptFailure
            (folding (concatOf dirsReservations) .
             folding (concatOf rInstances) . insState . isName . to toTextCI)
        , matchAny
            "terminated"
            AcceptFailure
            (folding (concatOf dirsReservations) .
             folding (concatOf rInstances) . insState . isName . to toTextCI)
        ]
    }


-- | Polls 'Network.AWS.EC2.DescribeConversionTasks' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
conversionTaskDeleted :: Wait DescribeConversionTasks
conversionTaskDeleted =
  Wait
    { _waitName = "ConversionTaskDeleted"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors =
        [ matchAll
            "deleted"
            AcceptSuccess
            (folding (concatOf dctrsConversionTasks) .
             ctState . _Just . to toTextCI)
        ]
    }


-- | Polls 'Network.AWS.EC2.GetPasswordData' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
passwordDataAvailable :: Wait GetPasswordData
passwordDataAvailable =
  Wait
    { _waitName = "PasswordDataAvailable"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors =
        [matchAll True AcceptSuccess (nonEmptyText gpdrsPasswordData)]
    }


-- | Polls 'Network.AWS.EC2.DescribeInstances' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
instanceRunning :: Wait DescribeInstances
instanceRunning =
  Wait
    { _waitName = "InstanceRunning"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors =
        [ matchAll
            "running"
            AcceptSuccess
            (folding (concatOf dirsReservations) .
             folding (concatOf rInstances) . insState . isName . to toTextCI)
        , matchAny
            "shutting-down"
            AcceptFailure
            (folding (concatOf dirsReservations) .
             folding (concatOf rInstances) . insState . isName . to toTextCI)
        , matchAny
            "terminated"
            AcceptFailure
            (folding (concatOf dirsReservations) .
             folding (concatOf rInstances) . insState . isName . to toTextCI)
        , matchAny
            "stopping"
            AcceptFailure
            (folding (concatOf dirsReservations) .
             folding (concatOf rInstances) . insState . isName . to toTextCI)
        , matchError "InvalidInstanceID.NotFound" AcceptRetry
        ]
    }


-- | Polls 'Network.AWS.EC2.DescribeSpotInstanceRequests' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
spotInstanceRequestFulfilled :: Wait DescribeSpotInstanceRequests
spotInstanceRequestFulfilled =
  Wait
    { _waitName = "SpotInstanceRequestFulfilled"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors =
        [ matchAll
            "fulfilled"
            AcceptSuccess
            (folding (concatOf dsirrsSpotInstanceRequests) .
             sirStatus . _Just . sisCode . _Just . to toTextCI)
        , matchAll
            "request-canceled-and-instance-running"
            AcceptSuccess
            (folding (concatOf dsirrsSpotInstanceRequests) .
             sirStatus . _Just . sisCode . _Just . to toTextCI)
        , matchAny
            "schedule-expired"
            AcceptFailure
            (folding (concatOf dsirrsSpotInstanceRequests) .
             sirStatus . _Just . sisCode . _Just . to toTextCI)
        , matchAny
            "canceled-before-fulfillment"
            AcceptFailure
            (folding (concatOf dsirrsSpotInstanceRequests) .
             sirStatus . _Just . sisCode . _Just . to toTextCI)
        , matchAny
            "bad-parameters"
            AcceptFailure
            (folding (concatOf dsirrsSpotInstanceRequests) .
             sirStatus . _Just . sisCode . _Just . to toTextCI)
        , matchAny
            "system-error"
            AcceptFailure
            (folding (concatOf dsirrsSpotInstanceRequests) .
             sirStatus . _Just . sisCode . _Just . to toTextCI)
        , matchError "InvalidSpotInstanceRequestID.NotFound" AcceptRetry
        ]
    }


-- | Polls 'Network.AWS.EC2.DescribeVPCs' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
vpcAvailable :: Wait DescribeVPCs
vpcAvailable =
  Wait
    { _waitName = "VpcAvailable"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors =
        [ matchAll
            "available"
            AcceptSuccess
            (folding (concatOf dvrsVPCs) . vpcState . to toTextCI)
        ]
    }


-- | Polls 'Network.AWS.EC2.DescribeExportTasks' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
exportTaskCompleted :: Wait DescribeExportTasks
exportTaskCompleted =
  Wait
    { _waitName = "ExportTaskCompleted"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors =
        [ matchAll
            "completed"
            AcceptSuccess
            (folding (concatOf detrsExportTasks) . etState . to toTextCI)
        ]
    }


-- | Polls 'Network.AWS.EC2.DescribeVPCPeeringConnections' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
vpcPeeringConnectionDeleted :: Wait DescribeVPCPeeringConnections
vpcPeeringConnectionDeleted =
  Wait
    { _waitName = "VpcPeeringConnectionDeleted"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors =
        [ matchAll
            "deleted"
            AcceptSuccess
            (folding (concatOf dvpcpcrsVPCPeeringConnections) .
             vpcpcStatus . _Just . vpcsrCode . _Just . to toTextCI)
        , matchError "InvalidVpcPeeringConnectionID.NotFound" AcceptSuccess
        ]
    }


-- | Polls 'Network.AWS.EC2.DescribeVPNConnections' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
vpnConnectionAvailable :: Wait DescribeVPNConnections
vpnConnectionAvailable =
  Wait
    { _waitName = "VpnConnectionAvailable"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors =
        [ matchAll
            "available"
            AcceptSuccess
            (folding (concatOf dvcrsVPNConnections) . vcState . to toTextCI)
        , matchAny
            "deleting"
            AcceptFailure
            (folding (concatOf dvcrsVPNConnections) . vcState . to toTextCI)
        , matchAny
            "deleted"
            AcceptFailure
            (folding (concatOf dvcrsVPNConnections) . vcState . to toTextCI)
        ]
    }


-- | Polls 'Network.AWS.EC2.DescribeExportTasks' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
exportTaskCancelled :: Wait DescribeExportTasks
exportTaskCancelled =
  Wait
    { _waitName = "ExportTaskCancelled"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors =
        [ matchAll
            "cancelled"
            AcceptSuccess
            (folding (concatOf detrsExportTasks) . etState . to toTextCI)
        ]
    }


-- | Polls 'Network.AWS.EC2.DescribeVolumes' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
volumeDeleted :: Wait DescribeVolumes
volumeDeleted =
  Wait
    { _waitName = "VolumeDeleted"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors =
        [ matchAll
            "deleted"
            AcceptSuccess
            (folding (concatOf dvvrsVolumes) . vState . to toTextCI)
        , matchError "InvalidVolume.NotFound" AcceptSuccess
        ]
    }


-- | Polls 'Network.AWS.EC2.DescribeVPCs' every 1 seconds until a successful state is reached. An error is returned after 5 failed checks.
vpcExists :: Wait DescribeVPCs
vpcExists =
  Wait
    { _waitName = "VpcExists"
    , _waitAttempts = 5
    , _waitDelay = 1
    , _waitAcceptors =
        [ matchStatus 200 AcceptSuccess
        , matchError "InvalidVpcID.NotFound" AcceptRetry
        ]
    }


-- | Polls 'Network.AWS.EC2.DescribeBundleTasks' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
bundleTaskComplete :: Wait DescribeBundleTasks
bundleTaskComplete =
  Wait
    { _waitName = "BundleTaskComplete"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors =
        [ matchAll
            "complete"
            AcceptSuccess
            (folding (concatOf dbtrsBundleTasks) . btState . to toTextCI)
        , matchAny
            "failed"
            AcceptFailure
            (folding (concatOf dbtrsBundleTasks) . btState . to toTextCI)
        ]
    }


-- | Polls 'Network.AWS.EC2.DescribeVPNConnections' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
vpnConnectionDeleted :: Wait DescribeVPNConnections
vpnConnectionDeleted =
  Wait
    { _waitName = "VpnConnectionDeleted"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors =
        [ matchAll
            "deleted"
            AcceptSuccess
            (folding (concatOf dvcrsVPNConnections) . vcState . to toTextCI)
        , matchAny
            "pending"
            AcceptFailure
            (folding (concatOf dvcrsVPNConnections) . vcState . to toTextCI)
        ]
    }


-- | Polls 'Network.AWS.EC2.DescribeConversionTasks' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
conversionTaskCancelled :: Wait DescribeConversionTasks
conversionTaskCancelled =
  Wait
    { _waitName = "ConversionTaskCancelled"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors =
        [ matchAll
            "cancelled"
            AcceptSuccess
            (folding (concatOf dctrsConversionTasks) .
             ctState . _Just . to toTextCI)
        ]
    }


-- | Polls 'Network.AWS.EC2.DescribeImages' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
imageAvailable :: Wait DescribeImages
imageAvailable =
  Wait
    { _waitName = "ImageAvailable"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors =
        [ matchAll
            "available"
            AcceptSuccess
            (folding (concatOf diirsImages) . iState . to toTextCI)
        , matchAny
            "deregistered"
            AcceptFailure
            (folding (concatOf diirsImages) . iState . to toTextCI)
        ]
    }


-- | Polls 'Network.AWS.EC2.DescribeVPCPeeringConnections' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
vpcPeeringConnectionExists :: Wait DescribeVPCPeeringConnections
vpcPeeringConnectionExists =
  Wait
    { _waitName = "VpcPeeringConnectionExists"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors =
        [ matchStatus 200 AcceptSuccess
        , matchError "InvalidVpcPeeringConnectionID.NotFound" AcceptRetry
        ]
    }


-- | Polls 'Network.AWS.EC2.DescribeSnapshots' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
snapshotCompleted :: Wait DescribeSnapshots
snapshotCompleted =
  Wait
    { _waitName = "SnapshotCompleted"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors =
        [ matchAll
            "completed"
            AcceptSuccess
            (folding (concatOf dssrsSnapshots) . sState . to toTextCI)
        ]
    }


-- | Polls 'Network.AWS.EC2.DescribeInstances' every 5 seconds until a successful state is reached. An error is returned after 40 failed checks.
instanceExists :: Wait DescribeInstances
instanceExists =
  Wait
    { _waitName = "InstanceExists"
    , _waitAttempts = 40
    , _waitDelay = 5
    , _waitAcceptors =
        [ matchStatus 200 AcceptSuccess
        , matchError "InvalidInstanceIDNotFound" AcceptRetry
        ]
    }


-- | Polls 'Network.AWS.EC2.DescribeInstanceStatus' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
instanceStatusOK :: Wait DescribeInstanceStatus
instanceStatusOK =
  Wait
    { _waitName = "InstanceStatusOk"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors =
        [ matchAll
            "ok"
            AcceptSuccess
            (folding (concatOf disrsInstanceStatuses) .
             isInstanceStatus . _Just . issStatus . to toTextCI)
        , matchError "InvalidInstanceID.NotFound" AcceptRetry
        ]
    }


-- | Polls 'Network.AWS.EC2.DescribeVolumes' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
volumeAvailable :: Wait DescribeVolumes
volumeAvailable =
  Wait
    { _waitName = "VolumeAvailable"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors =
        [ matchAll
            "available"
            AcceptSuccess
            (folding (concatOf dvvrsVolumes) . vState . to toTextCI)
        , matchAny
            "deleted"
            AcceptFailure
            (folding (concatOf dvvrsVolumes) . vState . to toTextCI)
        ]
    }

