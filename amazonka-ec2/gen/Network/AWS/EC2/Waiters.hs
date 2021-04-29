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
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.EC2.DescribeInstances' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newInstanceTerminated :: Waiter.Wait DescribeInstances
newInstanceTerminated =
  Waiter.Wait
    { Waiter._waitName =
        "InstanceTerminated",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "terminated"
            Waiter.AcceptSuccess
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
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "pending"
            Waiter.AcceptFailure
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
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "stopping"
            Waiter.AcceptFailure
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
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeVpcPeeringConnections' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newVpcPeeringConnectionDeleted :: Waiter.Wait DescribeVpcPeeringConnections
newVpcPeeringConnectionDeleted =
  Waiter.Wait
    { Waiter._waitName =
        "VpcPeeringConnectionDeleted",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "deleted"
            Waiter.AcceptSuccess
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
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchError
            "InvalidVpcPeeringConnectionID.NotFound"
            Waiter.AcceptSuccess
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeExportTasks' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newExportTaskCompleted :: Waiter.Wait DescribeExportTasks
newExportTaskCompleted =
  Waiter.Wait
    { Waiter._waitName =
        "ExportTaskCompleted",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "completed"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeExportTasksResponse_exportTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. exportTask_state
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeSnapshots' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newSnapshotCompleted :: Waiter.Wait DescribeSnapshots
newSnapshotCompleted =
  Waiter.Wait
    { Waiter._waitName = "SnapshotCompleted",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "completed"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeSnapshotsResponse_snapshots
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. snapshot_state
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeSpotInstanceRequests' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newSpotInstanceRequestFulfilled :: Waiter.Wait DescribeSpotInstanceRequests
newSpotInstanceRequestFulfilled =
  Waiter.Wait
    { Waiter._waitName =
        "SpotInstanceRequestFulfilled",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "fulfilled"
            Waiter.AcceptSuccess
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
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAll
            "request-canceled-and-instance-running"
            Waiter.AcceptSuccess
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
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "schedule-expired"
            Waiter.AcceptFailure
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
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "canceled-before-fulfillment"
            Waiter.AcceptFailure
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
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "bad-parameters"
            Waiter.AcceptFailure
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
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "system-error"
            Waiter.AcceptFailure
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
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchError
            "InvalidSpotInstanceRequestID.NotFound"
            Waiter.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeVolumes' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newVolumeAvailable :: Waiter.Wait DescribeVolumes
newVolumeAvailable =
  Waiter.Wait
    { Waiter._waitName = "VolumeAvailable",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "available"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeVolumesResponse_volumes
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. volume_state
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "deleted"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeVolumesResponse_volumes
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. volume_state
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeImages' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newImageAvailable :: Waiter.Wait DescribeImages
newImageAvailable =
  Waiter.Wait
    { Waiter._waitName = "ImageAvailable",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "available"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (describeImagesResponse_images Prelude.. Lens._Just)
                )
                Prelude.. image_state
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "deregistered"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeImagesResponse_images Prelude.. Lens._Just)
                )
                Prelude.. image_state
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.GetPasswordData' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newPasswordDataAvailable :: Waiter.Wait GetPasswordData
newPasswordDataAvailable =
  Waiter.Wait
    { Waiter._waitName =
        "PasswordDataAvailable",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            Prelude.True
            Waiter.AcceptSuccess
            ( Waiter.nonEmptyText
                getPasswordDataResponse_passwordData
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeInstances' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newInstanceRunning :: Waiter.Wait DescribeInstances
newInstanceRunning =
  Waiter.Wait
    { Waiter._waitName = "InstanceRunning",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "running"
            Waiter.AcceptSuccess
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
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "shutting-down"
            Waiter.AcceptFailure
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
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "terminated"
            Waiter.AcceptFailure
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
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "stopping"
            Waiter.AcceptFailure
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
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchError
            "InvalidInstanceID.NotFound"
            Waiter.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeKeyPairs' every 5 seconds until a successful state is reached. An error is returned after 6 failed checks.
newKeyPairExists :: Waiter.Wait DescribeKeyPairs
newKeyPairExists =
  Waiter.Wait
    { Waiter._waitName = "KeyPairExists",
      Waiter._waitAttempts = 6,
      Waiter._waitDelay = 5,
      Waiter._waitAcceptors =
        [ Waiter.matchNonEmpty
            Prelude.True
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeKeyPairsResponse_keyPairs
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. keyPairInfo_keyName
                Prelude.. Lens._Just
            ),
          Waiter.matchError
            "InvalidKeyPair.NotFound"
            Waiter.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeExportTasks' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newExportTaskCancelled :: Waiter.Wait DescribeExportTasks
newExportTaskCancelled =
  Waiter.Wait
    { Waiter._waitName =
        "ExportTaskCancelled",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "cancelled"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeExportTasksResponse_exportTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. exportTask_state
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeVpnConnections' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newVpnConnectionAvailable :: Waiter.Wait DescribeVpnConnections
newVpnConnectionAvailable =
  Waiter.Wait
    { Waiter._waitName =
        "VpnConnectionAvailable",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "available"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeVpnConnectionsResponse_vpnConnections
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. vpnConnection_state
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "deleting"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeVpnConnectionsResponse_vpnConnections
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. vpnConnection_state
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "deleted"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeVpnConnectionsResponse_vpnConnections
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. vpnConnection_state
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeImages' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newImageExists :: Waiter.Wait DescribeImages
newImageExists =
  Waiter.Wait
    { Waiter._waitName = "ImageExists",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchNonEmpty
            Prelude.True
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeImagesResponse_images
                        Prelude.. Lens._Just
                    )
                )
            ),
          Waiter.matchError
            "InvalidAMIID.NotFound"
            Waiter.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeVpcs' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newVpcAvailable :: Waiter.Wait DescribeVpcs
newVpcAvailable =
  Waiter.Wait
    { Waiter._waitName = "VpcAvailable",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "available"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (describeVpcsResponse_vpcs Prelude.. Lens._Just)
                )
                Prelude.. vpc_state
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeVolumes' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newVolumeInUse :: Waiter.Wait DescribeVolumes
newVolumeInUse =
  Waiter.Wait
    { Waiter._waitName = "VolumeInUse",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "in-use"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeVolumesResponse_volumes
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. volume_state
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "deleted"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeVolumesResponse_volumes
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. volume_state
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeInstances' every 5 seconds until a successful state is reached. An error is returned after 40 failed checks.
newInstanceExists :: Waiter.Wait DescribeInstances
newInstanceExists =
  Waiter.Wait
    { Waiter._waitName = "InstanceExists",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 5,
      Waiter._waitAcceptors =
        [ Waiter.matchStatus 200 Waiter.AcceptSuccess,
          Waiter.matchError
            "InvalidInstanceIDNotFound"
            Waiter.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeVpcPeeringConnections' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newVpcPeeringConnectionExists :: Waiter.Wait DescribeVpcPeeringConnections
newVpcPeeringConnectionExists =
  Waiter.Wait
    { Waiter._waitName =
        "VpcPeeringConnectionExists",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchStatus 200 Waiter.AcceptSuccess,
          Waiter.matchError
            "InvalidVpcPeeringConnectionID.NotFound"
            Waiter.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeInstanceStatus' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newInstanceStatusOk :: Waiter.Wait DescribeInstanceStatus
newInstanceStatusOk =
  Waiter.Wait
    { Waiter._waitName = "InstanceStatusOk",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "ok"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstanceStatusResponse_instanceStatuses
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instanceStatus_instanceStatus
                Prelude.. Lens._Just
                Prelude.. instanceStatusSummary_status
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchError
            "InvalidInstanceID.NotFound"
            Waiter.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeSecurityGroups' every 5 seconds until a successful state is reached. An error is returned after 6 failed checks.
newSecurityGroupExists :: Waiter.Wait DescribeSecurityGroups
newSecurityGroupExists =
  Waiter.Wait
    { Waiter._waitName =
        "SecurityGroupExists",
      Waiter._waitAttempts = 6,
      Waiter._waitDelay = 5,
      Waiter._waitAcceptors =
        [ Waiter.matchNonEmpty
            Prelude.True
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeSecurityGroupsResponse_securityGroups
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. securityGroup_groupId
            ),
          Waiter.matchError
            "InvalidGroupNotFound"
            Waiter.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeConversionTasks' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newConversionTaskCancelled :: Waiter.Wait DescribeConversionTasks
newConversionTaskCancelled =
  Waiter.Wait
    { Waiter._waitName =
        "ConversionTaskCancelled",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "cancelled"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeConversionTasksResponse_conversionTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. conversionTask_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeVpnConnections' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newVpnConnectionDeleted :: Waiter.Wait DescribeVpnConnections
newVpnConnectionDeleted =
  Waiter.Wait
    { Waiter._waitName =
        "VpnConnectionDeleted",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "deleted"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeVpnConnectionsResponse_vpnConnections
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. vpnConnection_state
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "pending"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeVpnConnectionsResponse_vpnConnections
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. vpnConnection_state
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeInstances' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newInstanceStopped :: Waiter.Wait DescribeInstances
newInstanceStopped =
  Waiter.Wait
    { Waiter._waitName = "InstanceStopped",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "stopped"
            Waiter.AcceptSuccess
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
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "pending"
            Waiter.AcceptFailure
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
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "terminated"
            Waiter.AcceptFailure
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
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeConversionTasks' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newConversionTaskCompleted :: Waiter.Wait DescribeConversionTasks
newConversionTaskCompleted =
  Waiter.Wait
    { Waiter._waitName =
        "ConversionTaskCompleted",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "completed"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeConversionTasksResponse_conversionTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. conversionTask_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "cancelled"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeConversionTasksResponse_conversionTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. conversionTask_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "cancelling"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeConversionTasksResponse_conversionTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. conversionTask_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeBundleTasks' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newBundleTaskComplete :: Waiter.Wait DescribeBundleTasks
newBundleTaskComplete =
  Waiter.Wait
    { Waiter._waitName =
        "BundleTaskComplete",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "complete"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeBundleTasksResponse_bundleTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. bundleTask_state
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "failed"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeBundleTasksResponse_bundleTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. bundleTask_state
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeConversionTasks' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newConversionTaskDeleted :: Waiter.Wait DescribeConversionTasks
newConversionTaskDeleted =
  Waiter.Wait
    { Waiter._waitName =
        "ConversionTaskDeleted",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "deleted"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeConversionTasksResponse_conversionTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. conversionTask_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeVolumes' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newVolumeDeleted :: Waiter.Wait DescribeVolumes
newVolumeDeleted =
  Waiter.Wait
    { Waiter._waitName = "VolumeDeleted",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "deleted"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeVolumesResponse_volumes
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. volume_state
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchError
            "InvalidVolume.NotFound"
            Waiter.AcceptSuccess
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeCustomerGateways' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newCustomerGatewayAvailable :: Waiter.Wait DescribeCustomerGateways
newCustomerGatewayAvailable =
  Waiter.Wait
    { Waiter._waitName =
        "CustomerGatewayAvailable",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "available"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeCustomerGatewaysResponse_customerGateways
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. customerGateway_state
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "deleted"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeCustomerGatewaysResponse_customerGateways
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. customerGateway_state
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "deleting"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeCustomerGatewaysResponse_customerGateways
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. customerGateway_state
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeVpcs' every 1 seconds until a successful state is reached. An error is returned after 5 failed checks.
newVpcExists :: Waiter.Wait DescribeVpcs
newVpcExists =
  Waiter.Wait
    { Waiter._waitName = "VpcExists",
      Waiter._waitAttempts = 5,
      Waiter._waitDelay = 1,
      Waiter._waitAcceptors =
        [ Waiter.matchStatus 200 Waiter.AcceptSuccess,
          Waiter.matchError
            "InvalidVpcID.NotFound"
            Waiter.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeInstanceStatus' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newSystemStatusOk :: Waiter.Wait DescribeInstanceStatus
newSystemStatusOk =
  Waiter.Wait
    { Waiter._waitName = "SystemStatusOk",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "ok"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstanceStatusResponse_instanceStatuses
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instanceStatus_systemStatus
                Prelude.. Lens._Just
                Prelude.. instanceStatusSummary_status
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeNetworkInterfaces' every 20 seconds until a successful state is reached. An error is returned after 10 failed checks.
newNetworkInterfaceAvailable :: Waiter.Wait DescribeNetworkInterfaces
newNetworkInterfaceAvailable =
  Waiter.Wait
    { Waiter._waitName =
        "NetworkInterfaceAvailable",
      Waiter._waitAttempts = 10,
      Waiter._waitDelay = 20,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "available"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeNetworkInterfacesResponse_networkInterfaces
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. networkInterface_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchError
            "InvalidNetworkInterfaceID.NotFound"
            Waiter.AcceptFailure
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeNatGateways' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newNatGatewayAvailable :: Waiter.Wait DescribeNatGateways
newNatGatewayAvailable =
  Waiter.Wait
    { Waiter._waitName =
        "NatGatewayAvailable",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "available"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeNatGatewaysResponse_natGateways
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. natGateway_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "failed"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeNatGatewaysResponse_natGateways
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. natGateway_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "deleting"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeNatGatewaysResponse_natGateways
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. natGateway_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "deleted"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeNatGatewaysResponse_natGateways
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. natGateway_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchError
            "NatGatewayNotFound"
            Waiter.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeSubnets' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newSubnetAvailable :: Waiter.Wait DescribeSubnets
newSubnetAvailable =
  Waiter.Wait
    { Waiter._waitName = "SubnetAvailable",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "available"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeSubnetsResponse_subnets
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. subnet_state
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }
