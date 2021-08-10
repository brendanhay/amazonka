{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Waiters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Waiters where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.DescribeBundleTasks
import Network.AWS.EC2.DescribeConversionTasks
import Network.AWS.EC2.DescribeCustomerGateways
import Network.AWS.EC2.DescribeExportTasks
import Network.AWS.EC2.DescribeImages
import Network.AWS.EC2.DescribeInstanceStatus
import Network.AWS.EC2.DescribeInstances
import Network.AWS.EC2.DescribeKeyPairs
import Network.AWS.EC2.DescribeNatGateways
import Network.AWS.EC2.DescribeNetworkInterfaces
import Network.AWS.EC2.DescribeSecurityGroups
import Network.AWS.EC2.DescribeSnapshots
import Network.AWS.EC2.DescribeSpotInstanceRequests
import Network.AWS.EC2.DescribeSubnets
import Network.AWS.EC2.DescribeVolumes
import Network.AWS.EC2.DescribeVpcPeeringConnections
import Network.AWS.EC2.DescribeVpcs
import Network.AWS.EC2.DescribeVpnConnections
import Network.AWS.EC2.GetPasswordData
import Network.AWS.EC2.Lens
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Polls 'Network.AWS.EC2.DescribeInstances' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newInstanceTerminated :: Core.Wait DescribeInstances
newInstanceTerminated =
  Core.Wait
    { Core._waitName = "InstanceTerminated",
      Core._waitAttempts = 40,
      Core._waitDelay = 15,
      Core._waitAcceptors =
        [ Core.matchAll
            "terminated"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_reservations
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. Lens.folding
                  ( Lens.concatOf
                      (reservation_instances Prelude.. Lens._Just)
                  )
                Prelude.. instance_state
                Prelude.. instanceState_name
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "pending"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_reservations
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. Lens.folding
                  ( Lens.concatOf
                      (reservation_instances Prelude.. Lens._Just)
                  )
                Prelude.. instance_state
                Prelude.. instanceState_name
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "stopping"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_reservations
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. Lens.folding
                  ( Lens.concatOf
                      (reservation_instances Prelude.. Lens._Just)
                  )
                Prelude.. instance_state
                Prelude.. instanceState_name
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeVpcPeeringConnections' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newVpcPeeringConnectionDeleted :: Core.Wait DescribeVpcPeeringConnections
newVpcPeeringConnectionDeleted =
  Core.Wait
    { Core._waitName =
        "VpcPeeringConnectionDeleted",
      Core._waitAttempts = 40,
      Core._waitDelay = 15,
      Core._waitAcceptors =
        [ Core.matchAll
            "deleted"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeVpcPeeringConnectionsResponse_vpcPeeringConnections
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. vpcPeeringConnection_status
                Prelude.. Lens._Just
                Prelude.. vpcPeeringConnectionStateReason_code
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchError
            "InvalidVpcPeeringConnectionID.NotFound"
            Core.AcceptSuccess
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeExportTasks' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newExportTaskCompleted :: Core.Wait DescribeExportTasks
newExportTaskCompleted =
  Core.Wait
    { Core._waitName = "ExportTaskCompleted",
      Core._waitAttempts = 40,
      Core._waitDelay = 15,
      Core._waitAcceptors =
        [ Core.matchAll
            "completed"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeExportTasksResponse_exportTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. exportTask_state
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeSnapshots' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newSnapshotCompleted :: Core.Wait DescribeSnapshots
newSnapshotCompleted =
  Core.Wait
    { Core._waitName = "SnapshotCompleted",
      Core._waitAttempts = 40,
      Core._waitDelay = 15,
      Core._waitAcceptors =
        [ Core.matchAll
            "completed"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeSnapshotsResponse_snapshots
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. snapshot_state
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeSpotInstanceRequests' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newSpotInstanceRequestFulfilled :: Core.Wait DescribeSpotInstanceRequests
newSpotInstanceRequestFulfilled =
  Core.Wait
    { Core._waitName =
        "SpotInstanceRequestFulfilled",
      Core._waitAttempts = 40,
      Core._waitDelay = 15,
      Core._waitAcceptors =
        [ Core.matchAll
            "fulfilled"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeSpotInstanceRequestsResponse_spotInstanceRequests
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. spotInstanceRequest_status
                Prelude.. Lens._Just
                Prelude.. spotInstanceStatus_code
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "request-canceled-and-instance-running"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeSpotInstanceRequestsResponse_spotInstanceRequests
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. spotInstanceRequest_status
                Prelude.. Lens._Just
                Prelude.. spotInstanceStatus_code
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "schedule-expired"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeSpotInstanceRequestsResponse_spotInstanceRequests
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. spotInstanceRequest_status
                Prelude.. Lens._Just
                Prelude.. spotInstanceStatus_code
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "canceled-before-fulfillment"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeSpotInstanceRequestsResponse_spotInstanceRequests
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. spotInstanceRequest_status
                Prelude.. Lens._Just
                Prelude.. spotInstanceStatus_code
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "bad-parameters"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeSpotInstanceRequestsResponse_spotInstanceRequests
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. spotInstanceRequest_status
                Prelude.. Lens._Just
                Prelude.. spotInstanceStatus_code
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "system-error"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeSpotInstanceRequestsResponse_spotInstanceRequests
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. spotInstanceRequest_status
                Prelude.. Lens._Just
                Prelude.. spotInstanceStatus_code
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchError
            "InvalidSpotInstanceRequestID.NotFound"
            Core.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeVolumes' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newVolumeAvailable :: Core.Wait DescribeVolumes
newVolumeAvailable =
  Core.Wait
    { Core._waitName = "VolumeAvailable",
      Core._waitAttempts = 40,
      Core._waitDelay = 15,
      Core._waitAcceptors =
        [ Core.matchAll
            "available"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeVolumesResponse_volumes
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. volume_state
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "deleted"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeVolumesResponse_volumes
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. volume_state
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeImages' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newImageAvailable :: Core.Wait DescribeImages
newImageAvailable =
  Core.Wait
    { Core._waitName = "ImageAvailable",
      Core._waitAttempts = 40,
      Core._waitDelay = 15,
      Core._waitAcceptors =
        [ Core.matchAll
            "available"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (describeImagesResponse_images Prelude.. Lens._Just)
                )
                Prelude.. image_state
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "deregistered"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeImagesResponse_images Prelude.. Lens._Just)
                )
                Prelude.. image_state
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.GetPasswordData' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newPasswordDataAvailable :: Core.Wait GetPasswordData
newPasswordDataAvailable =
  Core.Wait
    { Core._waitName = "PasswordDataAvailable",
      Core._waitAttempts = 40,
      Core._waitDelay = 15,
      Core._waitAcceptors =
        [ Core.matchAll
            Prelude.True
            Core.AcceptSuccess
            ( Core.nonEmptyText
                getPasswordDataResponse_passwordData
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeInstances' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newInstanceRunning :: Core.Wait DescribeInstances
newInstanceRunning =
  Core.Wait
    { Core._waitName = "InstanceRunning",
      Core._waitAttempts = 40,
      Core._waitDelay = 15,
      Core._waitAcceptors =
        [ Core.matchAll
            "running"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_reservations
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. Lens.folding
                  ( Lens.concatOf
                      (reservation_instances Prelude.. Lens._Just)
                  )
                Prelude.. instance_state
                Prelude.. instanceState_name
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "shutting-down"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_reservations
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. Lens.folding
                  ( Lens.concatOf
                      (reservation_instances Prelude.. Lens._Just)
                  )
                Prelude.. instance_state
                Prelude.. instanceState_name
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "terminated"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_reservations
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. Lens.folding
                  ( Lens.concatOf
                      (reservation_instances Prelude.. Lens._Just)
                  )
                Prelude.. instance_state
                Prelude.. instanceState_name
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "stopping"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_reservations
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. Lens.folding
                  ( Lens.concatOf
                      (reservation_instances Prelude.. Lens._Just)
                  )
                Prelude.. instance_state
                Prelude.. instanceState_name
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchError
            "InvalidInstanceID.NotFound"
            Core.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeKeyPairs' every 5 seconds until a successful state is reached. An error is returned after 6 failed checks.
newKeyPairExists :: Core.Wait DescribeKeyPairs
newKeyPairExists =
  Core.Wait
    { Core._waitName = "KeyPairExists",
      Core._waitAttempts = 6,
      Core._waitDelay = 5,
      Core._waitAcceptors =
        [ Core.matchNonEmpty
            Prelude.True
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeKeyPairsResponse_keyPairs
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. keyPairInfo_keyName
                Prelude.. Lens._Just
            ),
          Core.matchError
            "InvalidKeyPair.NotFound"
            Core.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeExportTasks' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newExportTaskCancelled :: Core.Wait DescribeExportTasks
newExportTaskCancelled =
  Core.Wait
    { Core._waitName = "ExportTaskCancelled",
      Core._waitAttempts = 40,
      Core._waitDelay = 15,
      Core._waitAcceptors =
        [ Core.matchAll
            "cancelled"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeExportTasksResponse_exportTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. exportTask_state
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeVpnConnections' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newVpnConnectionAvailable :: Core.Wait DescribeVpnConnections
newVpnConnectionAvailable =
  Core.Wait
    { Core._waitName =
        "VpnConnectionAvailable",
      Core._waitAttempts = 40,
      Core._waitDelay = 15,
      Core._waitAcceptors =
        [ Core.matchAll
            "available"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeVpnConnectionsResponse_vpnConnections
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. vpnConnection_state
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "deleting"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeVpnConnectionsResponse_vpnConnections
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. vpnConnection_state
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "deleted"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeVpnConnectionsResponse_vpnConnections
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. vpnConnection_state
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeImages' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newImageExists :: Core.Wait DescribeImages
newImageExists =
  Core.Wait
    { Core._waitName = "ImageExists",
      Core._waitAttempts = 40,
      Core._waitDelay = 15,
      Core._waitAcceptors =
        [ Core.matchNonEmpty
            Prelude.True
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeImagesResponse_images
                        Prelude.. Lens._Just
                    )
                )
            ),
          Core.matchError
            "InvalidAMIID.NotFound"
            Core.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeVpcs' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newVpcAvailable :: Core.Wait DescribeVpcs
newVpcAvailable =
  Core.Wait
    { Core._waitName = "VpcAvailable",
      Core._waitAttempts = 40,
      Core._waitDelay = 15,
      Core._waitAcceptors =
        [ Core.matchAll
            "available"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (describeVpcsResponse_vpcs Prelude.. Lens._Just)
                )
                Prelude.. vpc_state
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeVolumes' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newVolumeInUse :: Core.Wait DescribeVolumes
newVolumeInUse =
  Core.Wait
    { Core._waitName = "VolumeInUse",
      Core._waitAttempts = 40,
      Core._waitDelay = 15,
      Core._waitAcceptors =
        [ Core.matchAll
            "in-use"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeVolumesResponse_volumes
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. volume_state
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "deleted"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeVolumesResponse_volumes
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. volume_state
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeInstances' every 5 seconds until a successful state is reached. An error is returned after 40 failed checks.
newInstanceExists :: Core.Wait DescribeInstances
newInstanceExists =
  Core.Wait
    { Core._waitName = "InstanceExists",
      Core._waitAttempts = 40,
      Core._waitDelay = 5,
      Core._waitAcceptors =
        [ Core.matchStatus 200 Core.AcceptSuccess,
          Core.matchError
            "InvalidInstanceIDNotFound"
            Core.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeVpcPeeringConnections' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newVpcPeeringConnectionExists :: Core.Wait DescribeVpcPeeringConnections
newVpcPeeringConnectionExists =
  Core.Wait
    { Core._waitName =
        "VpcPeeringConnectionExists",
      Core._waitAttempts = 40,
      Core._waitDelay = 15,
      Core._waitAcceptors =
        [ Core.matchStatus 200 Core.AcceptSuccess,
          Core.matchError
            "InvalidVpcPeeringConnectionID.NotFound"
            Core.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeInstanceStatus' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newInstanceStatusOk :: Core.Wait DescribeInstanceStatus
newInstanceStatusOk =
  Core.Wait
    { Core._waitName = "InstanceStatusOk",
      Core._waitAttempts = 40,
      Core._waitDelay = 15,
      Core._waitAcceptors =
        [ Core.matchAll
            "ok"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstanceStatusResponse_instanceStatuses
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instanceStatus_instanceStatus
                Prelude.. Lens._Just
                Prelude.. instanceStatusSummary_status
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchError
            "InvalidInstanceID.NotFound"
            Core.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeSecurityGroups' every 5 seconds until a successful state is reached. An error is returned after 6 failed checks.
newSecurityGroupExists :: Core.Wait DescribeSecurityGroups
newSecurityGroupExists =
  Core.Wait
    { Core._waitName = "SecurityGroupExists",
      Core._waitAttempts = 6,
      Core._waitDelay = 5,
      Core._waitAcceptors =
        [ Core.matchNonEmpty
            Prelude.True
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeSecurityGroupsResponse_securityGroups
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. securityGroup_groupId
            ),
          Core.matchError
            "InvalidGroupNotFound"
            Core.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeConversionTasks' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newConversionTaskCancelled :: Core.Wait DescribeConversionTasks
newConversionTaskCancelled =
  Core.Wait
    { Core._waitName =
        "ConversionTaskCancelled",
      Core._waitAttempts = 40,
      Core._waitDelay = 15,
      Core._waitAcceptors =
        [ Core.matchAll
            "cancelled"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeConversionTasksResponse_conversionTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. conversionTask_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeVpnConnections' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newVpnConnectionDeleted :: Core.Wait DescribeVpnConnections
newVpnConnectionDeleted =
  Core.Wait
    { Core._waitName = "VpnConnectionDeleted",
      Core._waitAttempts = 40,
      Core._waitDelay = 15,
      Core._waitAcceptors =
        [ Core.matchAll
            "deleted"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeVpnConnectionsResponse_vpnConnections
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. vpnConnection_state
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "pending"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeVpnConnectionsResponse_vpnConnections
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. vpnConnection_state
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeInstances' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newInstanceStopped :: Core.Wait DescribeInstances
newInstanceStopped =
  Core.Wait
    { Core._waitName = "InstanceStopped",
      Core._waitAttempts = 40,
      Core._waitDelay = 15,
      Core._waitAcceptors =
        [ Core.matchAll
            "stopped"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_reservations
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. Lens.folding
                  ( Lens.concatOf
                      (reservation_instances Prelude.. Lens._Just)
                  )
                Prelude.. instance_state
                Prelude.. instanceState_name
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "pending"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_reservations
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. Lens.folding
                  ( Lens.concatOf
                      (reservation_instances Prelude.. Lens._Just)
                  )
                Prelude.. instance_state
                Prelude.. instanceState_name
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "terminated"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_reservations
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. Lens.folding
                  ( Lens.concatOf
                      (reservation_instances Prelude.. Lens._Just)
                  )
                Prelude.. instance_state
                Prelude.. instanceState_name
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeConversionTasks' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newConversionTaskCompleted :: Core.Wait DescribeConversionTasks
newConversionTaskCompleted =
  Core.Wait
    { Core._waitName =
        "ConversionTaskCompleted",
      Core._waitAttempts = 40,
      Core._waitDelay = 15,
      Core._waitAcceptors =
        [ Core.matchAll
            "completed"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeConversionTasksResponse_conversionTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. conversionTask_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "cancelled"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeConversionTasksResponse_conversionTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. conversionTask_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "cancelling"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeConversionTasksResponse_conversionTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. conversionTask_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeBundleTasks' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newBundleTaskComplete :: Core.Wait DescribeBundleTasks
newBundleTaskComplete =
  Core.Wait
    { Core._waitName = "BundleTaskComplete",
      Core._waitAttempts = 40,
      Core._waitDelay = 15,
      Core._waitAcceptors =
        [ Core.matchAll
            "complete"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeBundleTasksResponse_bundleTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. bundleTask_state
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "failed"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeBundleTasksResponse_bundleTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. bundleTask_state
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeConversionTasks' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newConversionTaskDeleted :: Core.Wait DescribeConversionTasks
newConversionTaskDeleted =
  Core.Wait
    { Core._waitName = "ConversionTaskDeleted",
      Core._waitAttempts = 40,
      Core._waitDelay = 15,
      Core._waitAcceptors =
        [ Core.matchAll
            "deleted"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeConversionTasksResponse_conversionTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. conversionTask_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeVolumes' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newVolumeDeleted :: Core.Wait DescribeVolumes
newVolumeDeleted =
  Core.Wait
    { Core._waitName = "VolumeDeleted",
      Core._waitAttempts = 40,
      Core._waitDelay = 15,
      Core._waitAcceptors =
        [ Core.matchAll
            "deleted"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeVolumesResponse_volumes
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. volume_state
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchError
            "InvalidVolume.NotFound"
            Core.AcceptSuccess
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeCustomerGateways' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newCustomerGatewayAvailable :: Core.Wait DescribeCustomerGateways
newCustomerGatewayAvailable =
  Core.Wait
    { Core._waitName =
        "CustomerGatewayAvailable",
      Core._waitAttempts = 40,
      Core._waitDelay = 15,
      Core._waitAcceptors =
        [ Core.matchAll
            "available"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeCustomerGatewaysResponse_customerGateways
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. customerGateway_state
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "deleted"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeCustomerGatewaysResponse_customerGateways
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. customerGateway_state
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "deleting"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeCustomerGatewaysResponse_customerGateways
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. customerGateway_state
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeVpcs' every 1 seconds until a successful state is reached. An error is returned after 5 failed checks.
newVpcExists :: Core.Wait DescribeVpcs
newVpcExists =
  Core.Wait
    { Core._waitName = "VpcExists",
      Core._waitAttempts = 5,
      Core._waitDelay = 1,
      Core._waitAcceptors =
        [ Core.matchStatus 200 Core.AcceptSuccess,
          Core.matchError
            "InvalidVpcID.NotFound"
            Core.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeInstanceStatus' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newSystemStatusOk :: Core.Wait DescribeInstanceStatus
newSystemStatusOk =
  Core.Wait
    { Core._waitName = "SystemStatusOk",
      Core._waitAttempts = 40,
      Core._waitDelay = 15,
      Core._waitAcceptors =
        [ Core.matchAll
            "ok"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstanceStatusResponse_instanceStatuses
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instanceStatus_systemStatus
                Prelude.. Lens._Just
                Prelude.. instanceStatusSummary_status
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeNetworkInterfaces' every 20 seconds until a successful state is reached. An error is returned after 10 failed checks.
newNetworkInterfaceAvailable :: Core.Wait DescribeNetworkInterfaces
newNetworkInterfaceAvailable =
  Core.Wait
    { Core._waitName =
        "NetworkInterfaceAvailable",
      Core._waitAttempts = 10,
      Core._waitDelay = 20,
      Core._waitAcceptors =
        [ Core.matchAll
            "available"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeNetworkInterfacesResponse_networkInterfaces
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. networkInterface_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchError
            "InvalidNetworkInterfaceID.NotFound"
            Core.AcceptFailure
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeNatGateways' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newNatGatewayAvailable :: Core.Wait DescribeNatGateways
newNatGatewayAvailable =
  Core.Wait
    { Core._waitName = "NatGatewayAvailable",
      Core._waitAttempts = 40,
      Core._waitDelay = 15,
      Core._waitAcceptors =
        [ Core.matchAll
            "available"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeNatGatewaysResponse_natGateways
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. natGateway_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "failed"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeNatGatewaysResponse_natGateways
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. natGateway_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "deleting"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeNatGatewaysResponse_natGateways
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. natGateway_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "deleted"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeNatGatewaysResponse_natGateways
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. natGateway_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchError
            "NatGatewayNotFound"
            Core.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeSubnets' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newSubnetAvailable :: Core.Wait DescribeSubnets
newSubnetAvailable =
  Core.Wait
    { Core._waitName = "SubnetAvailable",
      Core._waitAttempts = 40,
      Core._waitDelay = 15,
      Core._waitAcceptors =
        [ Core.matchAll
            "available"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeSubnetsResponse_subnets
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. subnet_state
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }
