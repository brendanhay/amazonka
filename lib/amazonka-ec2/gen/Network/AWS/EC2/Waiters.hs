{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Waiters
  ( -- * InstanceTerminated
    mkInstanceTerminated,

    -- * VolumeInUse
    mkVolumeInUse,

    -- * ImageExists
    mkImageExists,

    -- * NatGatewayAvailable
    mkNatGatewayAvailable,

    -- * SubnetAvailable
    mkSubnetAvailable,

    -- * NetworkInterfaceAvailable
    mkNetworkInterfaceAvailable,

    -- * KeyPairExists
    mkKeyPairExists,

    -- * SystemStatusOk
    mkSystemStatusOk,

    -- * CustomerGatewayAvailable
    mkCustomerGatewayAvailable,

    -- * ConversionTaskCompleted
    mkConversionTaskCompleted,

    -- * InstanceStopped
    mkInstanceStopped,

    -- * ConversionTaskDeleted
    mkConversionTaskDeleted,

    -- * PasswordDataAvailable
    mkPasswordDataAvailable,

    -- * InstanceRunning
    mkInstanceRunning,

    -- * SecurityGroupExists
    mkSecurityGroupExists,

    -- * SpotInstanceRequestFulfilled
    mkSpotInstanceRequestFulfilled,

    -- * VpcAvailable
    mkVpcAvailable,

    -- * ExportTaskCompleted
    mkExportTaskCompleted,

    -- * VpcPeeringConnectionDeleted
    mkVpcPeeringConnectionDeleted,

    -- * VpnConnectionAvailable
    mkVpnConnectionAvailable,

    -- * ExportTaskCancelled
    mkExportTaskCancelled,

    -- * VolumeDeleted
    mkVolumeDeleted,

    -- * VpcExists
    mkVpcExists,

    -- * BundleTaskComplete
    mkBundleTaskComplete,

    -- * VpnConnectionDeleted
    mkVpnConnectionDeleted,

    -- * ConversionTaskCancelled
    mkConversionTaskCancelled,

    -- * ImageAvailable
    mkImageAvailable,

    -- * VpcPeeringConnectionExists
    mkVpcPeeringConnectionExists,

    -- * SnapshotCompleted
    mkSnapshotCompleted,

    -- * InstanceExists
    mkInstanceExists,

    -- * InstanceStatusOk
    mkInstanceStatusOk,

    -- * VolumeAvailable
    mkVolumeAvailable,
  )
where

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
import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.EC2.DescribeInstances' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkInstanceTerminated :: Waiter.Wait DescribeInstances
mkInstanceTerminated =
  Waiter.Wait
    { Waiter._waitName = "InstanceTerminated",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "terminated"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"reservations" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.folding
                  ( Lens.concatOf
                      ( Lens.field @"instances" Core.. Lens._Just
                          Core.. Lens.to Core.toList
                      )
                  )
                Core.. Lens.field @"state"
                Core.. Lens.field @"name"
            ),
          Waiter.matchAny
            "pending"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"reservations" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.folding
                  ( Lens.concatOf
                      ( Lens.field @"instances" Core.. Lens._Just
                          Core.. Lens.to Core.toList
                      )
                  )
                Core.. Lens.field @"state"
                Core.. Lens.field @"name"
            ),
          Waiter.matchAny
            "stopping"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"reservations" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.folding
                  ( Lens.concatOf
                      ( Lens.field @"instances" Core.. Lens._Just
                          Core.. Lens.to Core.toList
                      )
                  )
                Core.. Lens.field @"state"
                Core.. Lens.field @"name"
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeVolumes' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkVolumeInUse :: Waiter.Wait DescribeVolumes
mkVolumeInUse =
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
                    ( Lens.field @"volumes" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"state"
            ),
          Waiter.matchAny
            "deleted"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"volumes" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"state"
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeImages' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkImageExists :: Waiter.Wait DescribeImages
mkImageExists =
  Waiter.Wait
    { Waiter._waitName = "ImageExists",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchNonEmpty
            Core.True
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"images" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Core._isNonEmpty
            ),
          Waiter.matchError "InvalidAMIID.NotFound" Waiter.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeNatGateways' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkNatGatewayAvailable :: Waiter.Wait DescribeNatGateways
mkNatGatewayAvailable =
  Waiter.Wait
    { Waiter._waitName = "NatGatewayAvailable",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "available"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"natGateways" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"state"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "failed"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"natGateways" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"state"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "deleting"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"natGateways" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"state"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "deleted"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"natGateways" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"state"
                Core.. Lens._Just
            ),
          Waiter.matchError "NatGatewayNotFound" Waiter.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeSubnets' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkSubnetAvailable :: Waiter.Wait DescribeSubnets
mkSubnetAvailable =
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
                    ( Lens.field @"subnets" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"state"
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeNetworkInterfaces' every 20 seconds until a successful state is reached. An error is returned after 10 failed checks.
mkNetworkInterfaceAvailable :: Waiter.Wait DescribeNetworkInterfaces
mkNetworkInterfaceAvailable =
  Waiter.Wait
    { Waiter._waitName = "NetworkInterfaceAvailable",
      Waiter._waitAttempts = 10,
      Waiter._waitDelay = 20,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "available"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"networkInterfaces" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"status"
                Core.. Lens._Just
            ),
          Waiter.matchError
            "InvalidNetworkInterfaceID.NotFound"
            Waiter.AcceptFailure
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeKeyPairs' every 5 seconds until a successful state is reached. An error is returned after 6 failed checks.
mkKeyPairExists :: Waiter.Wait DescribeKeyPairs
mkKeyPairExists =
  Waiter.Wait
    { Waiter._waitName = "KeyPairExists",
      Waiter._waitAttempts = 6,
      Waiter._waitDelay = 5,
      Waiter._waitAcceptors =
        [ Waiter.matchNonEmpty
            Core.True
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"keyPairs" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"keyName"
                Core.. Lens._Just
                Core.. Core._isNonEmpty
            ),
          Waiter.matchError "InvalidKeyPair.NotFound" Waiter.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeInstanceStatus' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkSystemStatusOk :: Waiter.Wait DescribeInstanceStatus
mkSystemStatusOk =
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
                    ( Lens.field @"instanceStatuses" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"systemStatus"
                Core.. Lens._Just
                Core.. Lens.field @"status"
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeCustomerGateways' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkCustomerGatewayAvailable :: Waiter.Wait DescribeCustomerGateways
mkCustomerGatewayAvailable =
  Waiter.Wait
    { Waiter._waitName = "CustomerGatewayAvailable",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "available"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"customerGateways" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"state"
            ),
          Waiter.matchAny
            "deleted"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"customerGateways" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"state"
            ),
          Waiter.matchAny
            "deleting"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"customerGateways" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"state"
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeConversionTasks' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkConversionTaskCompleted :: Waiter.Wait DescribeConversionTasks
mkConversionTaskCompleted =
  Waiter.Wait
    { Waiter._waitName = "ConversionTaskCompleted",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "completed"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"conversionTasks" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"state"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "cancelled"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"conversionTasks" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"state"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "cancelling"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"conversionTasks" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"state"
                Core.. Lens._Just
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeInstances' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkInstanceStopped :: Waiter.Wait DescribeInstances
mkInstanceStopped =
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
                    ( Lens.field @"reservations" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.folding
                  ( Lens.concatOf
                      ( Lens.field @"instances" Core.. Lens._Just
                          Core.. Lens.to Core.toList
                      )
                  )
                Core.. Lens.field @"state"
                Core.. Lens.field @"name"
            ),
          Waiter.matchAny
            "pending"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"reservations" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.folding
                  ( Lens.concatOf
                      ( Lens.field @"instances" Core.. Lens._Just
                          Core.. Lens.to Core.toList
                      )
                  )
                Core.. Lens.field @"state"
                Core.. Lens.field @"name"
            ),
          Waiter.matchAny
            "terminated"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"reservations" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.folding
                  ( Lens.concatOf
                      ( Lens.field @"instances" Core.. Lens._Just
                          Core.. Lens.to Core.toList
                      )
                  )
                Core.. Lens.field @"state"
                Core.. Lens.field @"name"
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeConversionTasks' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkConversionTaskDeleted :: Waiter.Wait DescribeConversionTasks
mkConversionTaskDeleted =
  Waiter.Wait
    { Waiter._waitName = "ConversionTaskDeleted",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "deleted"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"conversionTasks" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"state"
                Core.. Lens._Just
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.GetPasswordData' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkPasswordDataAvailable :: Waiter.Wait GetPasswordData
mkPasswordDataAvailable =
  Waiter.Wait
    { Waiter._waitName = "PasswordDataAvailable",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchNonEmpty
            Core.True
            Waiter.AcceptSuccess
            (Lens.field @"passwordData" Core.. Core._isNonEmpty)
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeInstances' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkInstanceRunning :: Waiter.Wait DescribeInstances
mkInstanceRunning =
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
                    ( Lens.field @"reservations" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.folding
                  ( Lens.concatOf
                      ( Lens.field @"instances" Core.. Lens._Just
                          Core.. Lens.to Core.toList
                      )
                  )
                Core.. Lens.field @"state"
                Core.. Lens.field @"name"
            ),
          Waiter.matchAny
            "shutting-down"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"reservations" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.folding
                  ( Lens.concatOf
                      ( Lens.field @"instances" Core.. Lens._Just
                          Core.. Lens.to Core.toList
                      )
                  )
                Core.. Lens.field @"state"
                Core.. Lens.field @"name"
            ),
          Waiter.matchAny
            "terminated"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"reservations" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.folding
                  ( Lens.concatOf
                      ( Lens.field @"instances" Core.. Lens._Just
                          Core.. Lens.to Core.toList
                      )
                  )
                Core.. Lens.field @"state"
                Core.. Lens.field @"name"
            ),
          Waiter.matchAny
            "stopping"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"reservations" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.folding
                  ( Lens.concatOf
                      ( Lens.field @"instances" Core.. Lens._Just
                          Core.. Lens.to Core.toList
                      )
                  )
                Core.. Lens.field @"state"
                Core.. Lens.field @"name"
            ),
          Waiter.matchError "InvalidInstanceID.NotFound" Waiter.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeSecurityGroups' every 5 seconds until a successful state is reached. An error is returned after 6 failed checks.
mkSecurityGroupExists :: Waiter.Wait DescribeSecurityGroups
mkSecurityGroupExists =
  Waiter.Wait
    { Waiter._waitName = "SecurityGroupExists",
      Waiter._waitAttempts = 6,
      Waiter._waitDelay = 5,
      Waiter._waitAcceptors =
        [ Waiter.matchNonEmpty
            Core.True
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"securityGroups" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"groupId"
                Core.. Core._isNonEmpty
            ),
          Waiter.matchError "InvalidGroupNotFound" Waiter.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeSpotInstanceRequests' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkSpotInstanceRequestFulfilled :: Waiter.Wait DescribeSpotInstanceRequests
mkSpotInstanceRequestFulfilled =
  Waiter.Wait
    { Waiter._waitName = "SpotInstanceRequestFulfilled",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "fulfilled"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"spotInstanceRequests" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"status"
                Core.. Lens._Just
                Core.. Lens.field @"code"
                Core.. Lens._Just
            ),
          Waiter.matchAll
            "request-canceled-and-instance-running"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"spotInstanceRequests" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"status"
                Core.. Lens._Just
                Core.. Lens.field @"code"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "schedule-expired"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"spotInstanceRequests" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"status"
                Core.. Lens._Just
                Core.. Lens.field @"code"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "canceled-before-fulfillment"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"spotInstanceRequests" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"status"
                Core.. Lens._Just
                Core.. Lens.field @"code"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "bad-parameters"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"spotInstanceRequests" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"status"
                Core.. Lens._Just
                Core.. Lens.field @"code"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "system-error"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"spotInstanceRequests" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"status"
                Core.. Lens._Just
                Core.. Lens.field @"code"
                Core.. Lens._Just
            ),
          Waiter.matchError
            "InvalidSpotInstanceRequestID.NotFound"
            Waiter.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeVpcs' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkVpcAvailable :: Waiter.Wait DescribeVpcs
mkVpcAvailable =
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
                    (Lens.field @"vpcs" Core.. Lens._Just Core.. Lens.to Core.toList)
                )
                Core.. Lens.field @"state"
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeExportTasks' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkExportTaskCompleted :: Waiter.Wait DescribeExportTasks
mkExportTaskCompleted =
  Waiter.Wait
    { Waiter._waitName = "ExportTaskCompleted",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "completed"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"exportTasks" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"state"
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeVpcPeeringConnections' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkVpcPeeringConnectionDeleted :: Waiter.Wait DescribeVpcPeeringConnections
mkVpcPeeringConnectionDeleted =
  Waiter.Wait
    { Waiter._waitName = "VpcPeeringConnectionDeleted",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "deleted"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"vpcPeeringConnections" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"status"
                Core.. Lens._Just
                Core.. Lens.field @"code"
                Core.. Lens._Just
            ),
          Waiter.matchError
            "InvalidVpcPeeringConnectionID.NotFound"
            Waiter.AcceptSuccess
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeVpnConnections' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkVpnConnectionAvailable :: Waiter.Wait DescribeVpnConnections
mkVpnConnectionAvailable =
  Waiter.Wait
    { Waiter._waitName = "VpnConnectionAvailable",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "available"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"vpnConnections" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"state"
            ),
          Waiter.matchAny
            "deleting"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"vpnConnections" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"state"
            ),
          Waiter.matchAny
            "deleted"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"vpnConnections" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"state"
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeExportTasks' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkExportTaskCancelled :: Waiter.Wait DescribeExportTasks
mkExportTaskCancelled =
  Waiter.Wait
    { Waiter._waitName = "ExportTaskCancelled",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "cancelled"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"exportTasks" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"state"
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeVolumes' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkVolumeDeleted :: Waiter.Wait DescribeVolumes
mkVolumeDeleted =
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
                    ( Lens.field @"volumes" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"state"
            ),
          Waiter.matchError "InvalidVolume.NotFound" Waiter.AcceptSuccess
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeVpcs' every 1 seconds until a successful state is reached. An error is returned after 5 failed checks.
mkVpcExists :: Waiter.Wait DescribeVpcs
mkVpcExists =
  Waiter.Wait
    { Waiter._waitName = "VpcExists",
      Waiter._waitAttempts = 5,
      Waiter._waitDelay = 1,
      Waiter._waitAcceptors =
        [ Waiter.matchStatus 200 Waiter.AcceptSuccess,
          Waiter.matchError "InvalidVpcID.NotFound" Waiter.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeBundleTasks' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkBundleTaskComplete :: Waiter.Wait DescribeBundleTasks
mkBundleTaskComplete =
  Waiter.Wait
    { Waiter._waitName = "BundleTaskComplete",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "complete"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"bundleTasks" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"state"
            ),
          Waiter.matchAny
            "failed"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"bundleTasks" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"state"
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeVpnConnections' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkVpnConnectionDeleted :: Waiter.Wait DescribeVpnConnections
mkVpnConnectionDeleted =
  Waiter.Wait
    { Waiter._waitName = "VpnConnectionDeleted",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "deleted"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"vpnConnections" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"state"
            ),
          Waiter.matchAny
            "pending"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"vpnConnections" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"state"
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeConversionTasks' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkConversionTaskCancelled :: Waiter.Wait DescribeConversionTasks
mkConversionTaskCancelled =
  Waiter.Wait
    { Waiter._waitName = "ConversionTaskCancelled",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "cancelled"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"conversionTasks" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"state"
                Core.. Lens._Just
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeImages' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkImageAvailable :: Waiter.Wait DescribeImages
mkImageAvailable =
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
                    ( Lens.field @"images" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"state"
            ),
          Waiter.matchAny
            "deregistered"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"images" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"state"
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeVpcPeeringConnections' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkVpcPeeringConnectionExists :: Waiter.Wait DescribeVpcPeeringConnections
mkVpcPeeringConnectionExists =
  Waiter.Wait
    { Waiter._waitName = "VpcPeeringConnectionExists",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchStatus 200 Waiter.AcceptSuccess,
          Waiter.matchError
            "InvalidVpcPeeringConnectionID.NotFound"
            Waiter.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeSnapshots' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkSnapshotCompleted :: Waiter.Wait DescribeSnapshots
mkSnapshotCompleted =
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
                    ( Lens.field @"snapshots" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"state"
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeInstances' every 5 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkInstanceExists :: Waiter.Wait DescribeInstances
mkInstanceExists =
  Waiter.Wait
    { Waiter._waitName = "InstanceExists",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 5,
      Waiter._waitAcceptors =
        [ Waiter.matchStatus 200 Waiter.AcceptSuccess,
          Waiter.matchError "InvalidInstanceIDNotFound" Waiter.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeInstanceStatus' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkInstanceStatusOk :: Waiter.Wait DescribeInstanceStatus
mkInstanceStatusOk =
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
                    ( Lens.field @"instanceStatuses" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"instanceStatus"
                Core.. Lens._Just
                Core.. Lens.field @"status"
            ),
          Waiter.matchError "InvalidInstanceID.NotFound" Waiter.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeVolumes' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkVolumeAvailable :: Waiter.Wait DescribeVolumes
mkVolumeAvailable =
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
                    ( Lens.field @"volumes" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"state"
            ),
          Waiter.matchAny
            "deleted"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"volumes" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"state"
            )
        ]
    }
