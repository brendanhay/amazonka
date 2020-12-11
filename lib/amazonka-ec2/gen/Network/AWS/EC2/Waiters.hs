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

    -- * SystemStatusOK
    mkSystemStatusOK,

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

    -- * VPCAvailable
    mkVPCAvailable,

    -- * ExportTaskCompleted
    mkExportTaskCompleted,

    -- * VPCPeeringConnectionDeleted
    mkVPCPeeringConnectionDeleted,

    -- * VPNConnectionAvailable
    mkVPNConnectionAvailable,

    -- * ExportTaskCancelled
    mkExportTaskCancelled,

    -- * VolumeDeleted
    mkVolumeDeleted,

    -- * VPCExists
    mkVPCExists,

    -- * BundleTaskComplete
    mkBundleTaskComplete,

    -- * VPNConnectionDeleted
    mkVPNConnectionDeleted,

    -- * ConversionTaskCancelled
    mkConversionTaskCancelled,

    -- * ImageAvailable
    mkImageAvailable,

    -- * VPCPeeringConnectionExists
    mkVPCPeeringConnectionExists,

    -- * SnapshotCompleted
    mkSnapshotCompleted,

    -- * InstanceExists
    mkInstanceExists,

    -- * InstanceStatusOK
    mkInstanceStatusOK,

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
import Network.AWS.EC2.DescribeVPCPeeringConnections
import Network.AWS.EC2.DescribeVPCs
import Network.AWS.EC2.DescribeVPNConnections
import Network.AWS.EC2.DescribeVolumes
import Network.AWS.EC2.GetPasswordData
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Waiter as Wait

-- | Polls 'Network.AWS.EC2.DescribeInstances' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkInstanceTerminated :: Wait.Wait DescribeInstances
mkInstanceTerminated =
  Wait.Wait
    { Wait._waitName = "InstanceTerminated",
      Wait._waitAttempts = 40,
      Wait._waitDelay = 15,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "terminated"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (dirsReservations Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. Lens.folding
                  ( Lens.concatOf
                      (rInstances Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                  )
                Lude.. insState
                Lude.. isName
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "pending"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dirsReservations Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. Lens.folding
                  ( Lens.concatOf
                      (rInstances Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                  )
                Lude.. insState
                Lude.. isName
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "stopping"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dirsReservations Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. Lens.folding
                  ( Lens.concatOf
                      (rInstances Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                  )
                Lude.. insState
                Lude.. isName
                Lude.. Lens.to Lude.toText
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeVolumes' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkVolumeInUse :: Wait.Wait DescribeVolumes
mkVolumeInUse =
  Wait.Wait
    { Wait._waitName = "VolumeInUse",
      Wait._waitAttempts = 40,
      Wait._waitDelay = 15,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "in-use"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (dvvrsVolumes Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. vState
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "deleted"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dvvrsVolumes Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. vState
                Lude.. Lens.to Lude.toText
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeImages' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkImageExists :: Wait.Wait DescribeImages
mkImageExists =
  Wait.Wait
    { Wait._waitName = "ImageExists",
      Wait._waitAttempts = 40,
      Wait._waitDelay = 15,
      Wait._waitAcceptors =
        [ Lude.matchNonEmpty
            Lude.True
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (diirsImages Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
            ),
          Wait.matchError "InvalidAMIID.NotFound" Wait.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeNatGateways' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkNatGatewayAvailable :: Wait.Wait DescribeNatGateways
mkNatGatewayAvailable =
  Wait.Wait
    { Wait._waitName = "NatGatewayAvailable",
      Wait._waitAttempts = 40,
      Wait._waitDelay = 15,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "available"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (dngrsNatGateways Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. ngState
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "failed"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dngrsNatGateways Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. ngState
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "deleting"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dngrsNatGateways Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. ngState
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "deleted"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dngrsNatGateways Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. ngState
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchError "NatGatewayNotFound" Wait.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeSubnets' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkSubnetAvailable :: Wait.Wait DescribeSubnets
mkSubnetAvailable =
  Wait.Wait
    { Wait._waitName = "SubnetAvailable",
      Wait._waitAttempts = 40,
      Wait._waitDelay = 15,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "available"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (dsrsSubnets Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. subState
                Lude.. Lens.to Lude.toText
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeNetworkInterfaces' every 20 seconds until a successful state is reached. An error is returned after 10 failed checks.
mkNetworkInterfaceAvailable :: Wait.Wait DescribeNetworkInterfaces
mkNetworkInterfaceAvailable =
  Wait.Wait
    { Wait._waitName = "NetworkInterfaceAvailable",
      Wait._waitAttempts = 10,
      Wait._waitDelay = 20,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "available"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( dnirsNetworkInterfaces Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. niStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchError
            "InvalidNetworkInterfaceID.NotFound"
            Wait.AcceptFailure
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeKeyPairs' every 5 seconds until a successful state is reached. An error is returned after 6 failed checks.
mkKeyPairExists :: Wait.Wait DescribeKeyPairs
mkKeyPairExists =
  Wait.Wait
    { Wait._waitName = "KeyPairExists",
      Wait._waitAttempts = 6,
      Wait._waitDelay = 5,
      Wait._waitAcceptors =
        [ Lude.matchNonEmpty
            Lude.True
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (dkprsKeyPairs Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. kpiKeyName
                Lude.. Lens._Just
            ),
          Wait.matchError "InvalidKeyPair.NotFound" Wait.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeInstanceStatus' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkSystemStatusOK :: Wait.Wait DescribeInstanceStatus
mkSystemStatusOK =
  Wait.Wait
    { Wait._waitName = "SystemStatusOk",
      Wait._waitAttempts = 40,
      Wait._waitDelay = 15,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "ok"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( disrsInstanceStatuses Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. iSystemStatus
                Lude.. Lens._Just
                Lude.. issStatus
                Lude.. Lens.to Lude.toText
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeCustomerGateways' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkCustomerGatewayAvailable :: Wait.Wait DescribeCustomerGateways
mkCustomerGatewayAvailable =
  Wait.Wait
    { Wait._waitName = "CustomerGatewayAvailable",
      Wait._waitAttempts = 40,
      Wait._waitDelay = 15,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "available"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( dcgcrsCustomerGateways Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. cusState
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "deleted"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( dcgcrsCustomerGateways Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. cusState
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "deleting"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( dcgcrsCustomerGateways Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. cusState
                Lude.. Lens.to Lude.toText
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeConversionTasks' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkConversionTaskCompleted :: Wait.Wait DescribeConversionTasks
mkConversionTaskCompleted =
  Wait.Wait
    { Wait._waitName = "ConversionTaskCompleted",
      Wait._waitAttempts = 40,
      Wait._waitDelay = 15,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "completed"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( dctrsConversionTasks Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. ctState
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "cancelled"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( dctrsConversionTasks Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. ctState
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "cancelling"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( dctrsConversionTasks Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. ctState
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeInstances' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkInstanceStopped :: Wait.Wait DescribeInstances
mkInstanceStopped =
  Wait.Wait
    { Wait._waitName = "InstanceStopped",
      Wait._waitAttempts = 40,
      Wait._waitDelay = 15,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "stopped"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (dirsReservations Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. Lens.folding
                  ( Lens.concatOf
                      (rInstances Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                  )
                Lude.. insState
                Lude.. isName
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "pending"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dirsReservations Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. Lens.folding
                  ( Lens.concatOf
                      (rInstances Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                  )
                Lude.. insState
                Lude.. isName
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "terminated"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dirsReservations Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. Lens.folding
                  ( Lens.concatOf
                      (rInstances Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                  )
                Lude.. insState
                Lude.. isName
                Lude.. Lens.to Lude.toText
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeConversionTasks' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkConversionTaskDeleted :: Wait.Wait DescribeConversionTasks
mkConversionTaskDeleted =
  Wait.Wait
    { Wait._waitName = "ConversionTaskDeleted",
      Wait._waitAttempts = 40,
      Wait._waitDelay = 15,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "deleted"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( dctrsConversionTasks Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. ctState
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.GetPasswordData' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkPasswordDataAvailable :: Wait.Wait GetPasswordData
mkPasswordDataAvailable =
  Wait.Wait
    { Wait._waitName = "PasswordDataAvailable",
      Wait._waitAttempts = 40,
      Wait._waitDelay = 15,
      Wait._waitAcceptors =
        [ Lude.matchNonEmpty
            Lude.True
            Wait.AcceptSuccess
            gpdrsPasswordData
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeInstances' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkInstanceRunning :: Wait.Wait DescribeInstances
mkInstanceRunning =
  Wait.Wait
    { Wait._waitName = "InstanceRunning",
      Wait._waitAttempts = 40,
      Wait._waitDelay = 15,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "running"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (dirsReservations Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. Lens.folding
                  ( Lens.concatOf
                      (rInstances Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                  )
                Lude.. insState
                Lude.. isName
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "shutting-down"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dirsReservations Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. Lens.folding
                  ( Lens.concatOf
                      (rInstances Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                  )
                Lude.. insState
                Lude.. isName
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "terminated"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dirsReservations Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. Lens.folding
                  ( Lens.concatOf
                      (rInstances Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                  )
                Lude.. insState
                Lude.. isName
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "stopping"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dirsReservations Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. Lens.folding
                  ( Lens.concatOf
                      (rInstances Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                  )
                Lude.. insState
                Lude.. isName
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchError "InvalidInstanceID.NotFound" Wait.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeSecurityGroups' every 5 seconds until a successful state is reached. An error is returned after 6 failed checks.
mkSecurityGroupExists :: Wait.Wait DescribeSecurityGroups
mkSecurityGroupExists =
  Wait.Wait
    { Wait._waitName = "SecurityGroupExists",
      Wait._waitAttempts = 6,
      Wait._waitDelay = 5,
      Wait._waitAcceptors =
        [ Lude.matchNonEmpty
            Lude.True
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (dsgrsSecurityGroups Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. sgGroupId
            ),
          Wait.matchError "InvalidGroupNotFound" Wait.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeSpotInstanceRequests' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkSpotInstanceRequestFulfilled :: Wait.Wait DescribeSpotInstanceRequests
mkSpotInstanceRequestFulfilled =
  Wait.Wait
    { Wait._waitName = "SpotInstanceRequestFulfilled",
      Wait._waitAttempts = 40,
      Wait._waitDelay = 15,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "fulfilled"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( dsirrsSpotInstanceRequests Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. sirStatus
                Lude.. Lens._Just
                Lude.. sisCode
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAll
            "request-canceled-and-instance-running"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( dsirrsSpotInstanceRequests Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. sirStatus
                Lude.. Lens._Just
                Lude.. sisCode
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "schedule-expired"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( dsirrsSpotInstanceRequests Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. sirStatus
                Lude.. Lens._Just
                Lude.. sisCode
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "canceled-before-fulfillment"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( dsirrsSpotInstanceRequests Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. sirStatus
                Lude.. Lens._Just
                Lude.. sisCode
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "bad-parameters"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( dsirrsSpotInstanceRequests Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. sirStatus
                Lude.. Lens._Just
                Lude.. sisCode
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "system-error"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( dsirrsSpotInstanceRequests Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. sirStatus
                Lude.. Lens._Just
                Lude.. sisCode
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchError
            "InvalidSpotInstanceRequestID.NotFound"
            Wait.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeVPCs' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkVPCAvailable :: Wait.Wait DescribeVPCs
mkVPCAvailable =
  Wait.Wait
    { Wait._waitName = "VpcAvailable",
      Wait._waitAttempts = 40,
      Wait._waitDelay = 15,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "available"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (dvrsVPCs Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. vpcState
                Lude.. Lens.to Lude.toText
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeExportTasks' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkExportTaskCompleted :: Wait.Wait DescribeExportTasks
mkExportTaskCompleted =
  Wait.Wait
    { Wait._waitName = "ExportTaskCompleted",
      Wait._waitAttempts = 40,
      Wait._waitDelay = 15,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "completed"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (detrsExportTasks Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. etState
                Lude.. Lens.to Lude.toText
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeVPCPeeringConnections' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkVPCPeeringConnectionDeleted :: Wait.Wait DescribeVPCPeeringConnections
mkVPCPeeringConnectionDeleted =
  Wait.Wait
    { Wait._waitName = "VpcPeeringConnectionDeleted",
      Wait._waitAttempts = 40,
      Wait._waitDelay = 15,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "deleted"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( dvpcpcrsVPCPeeringConnections Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. vpcpcStatus
                Lude.. Lens._Just
                Lude.. vpcsrCode
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchError
            "InvalidVpcPeeringConnectionID.NotFound"
            Wait.AcceptSuccess
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeVPNConnections' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkVPNConnectionAvailable :: Wait.Wait DescribeVPNConnections
mkVPNConnectionAvailable =
  Wait.Wait
    { Wait._waitName = "VpnConnectionAvailable",
      Wait._waitAttempts = 40,
      Wait._waitDelay = 15,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "available"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (dvcrsVPNConnections Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. vcState
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "deleting"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dvcrsVPNConnections Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. vcState
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "deleted"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dvcrsVPNConnections Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. vcState
                Lude.. Lens.to Lude.toText
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeExportTasks' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkExportTaskCancelled :: Wait.Wait DescribeExportTasks
mkExportTaskCancelled =
  Wait.Wait
    { Wait._waitName = "ExportTaskCancelled",
      Wait._waitAttempts = 40,
      Wait._waitDelay = 15,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "cancelled"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (detrsExportTasks Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. etState
                Lude.. Lens.to Lude.toText
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeVolumes' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkVolumeDeleted :: Wait.Wait DescribeVolumes
mkVolumeDeleted =
  Wait.Wait
    { Wait._waitName = "VolumeDeleted",
      Wait._waitAttempts = 40,
      Wait._waitDelay = 15,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "deleted"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (dvvrsVolumes Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. vState
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchError "InvalidVolume.NotFound" Wait.AcceptSuccess
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeVPCs' every 1 seconds until a successful state is reached. An error is returned after 5 failed checks.
mkVPCExists :: Wait.Wait DescribeVPCs
mkVPCExists =
  Wait.Wait
    { Wait._waitName = "VpcExists",
      Wait._waitAttempts = 5,
      Wait._waitDelay = 1,
      Wait._waitAcceptors =
        [ Wait.matchStatus 200 Wait.AcceptSuccess,
          Wait.matchError "InvalidVpcID.NotFound" Wait.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeBundleTasks' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkBundleTaskComplete :: Wait.Wait DescribeBundleTasks
mkBundleTaskComplete =
  Wait.Wait
    { Wait._waitName = "BundleTaskComplete",
      Wait._waitAttempts = 40,
      Wait._waitDelay = 15,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "complete"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (dbtrsBundleTasks Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. btState
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "failed"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dbtrsBundleTasks Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. btState
                Lude.. Lens.to Lude.toText
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeVPNConnections' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkVPNConnectionDeleted :: Wait.Wait DescribeVPNConnections
mkVPNConnectionDeleted =
  Wait.Wait
    { Wait._waitName = "VpnConnectionDeleted",
      Wait._waitAttempts = 40,
      Wait._waitDelay = 15,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "deleted"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (dvcrsVPNConnections Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. vcState
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "pending"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dvcrsVPNConnections Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. vcState
                Lude.. Lens.to Lude.toText
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeConversionTasks' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkConversionTaskCancelled :: Wait.Wait DescribeConversionTasks
mkConversionTaskCancelled =
  Wait.Wait
    { Wait._waitName = "ConversionTaskCancelled",
      Wait._waitAttempts = 40,
      Wait._waitDelay = 15,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "cancelled"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( dctrsConversionTasks Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. ctState
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeImages' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkImageAvailable :: Wait.Wait DescribeImages
mkImageAvailable =
  Wait.Wait
    { Wait._waitName = "ImageAvailable",
      Wait._waitAttempts = 40,
      Wait._waitDelay = 15,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "available"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (diirsImages Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. iState
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "deregistered"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (diirsImages Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. iState
                Lude.. Lens.to Lude.toText
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeVPCPeeringConnections' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkVPCPeeringConnectionExists :: Wait.Wait DescribeVPCPeeringConnections
mkVPCPeeringConnectionExists =
  Wait.Wait
    { Wait._waitName = "VpcPeeringConnectionExists",
      Wait._waitAttempts = 40,
      Wait._waitDelay = 15,
      Wait._waitAcceptors =
        [ Wait.matchStatus 200 Wait.AcceptSuccess,
          Wait.matchError
            "InvalidVpcPeeringConnectionID.NotFound"
            Wait.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeSnapshots' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkSnapshotCompleted :: Wait.Wait DescribeSnapshots
mkSnapshotCompleted =
  Wait.Wait
    { Wait._waitName = "SnapshotCompleted",
      Wait._waitAttempts = 40,
      Wait._waitDelay = 15,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "completed"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (dssrsSnapshots Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. sState
                Lude.. Lens.to Lude.toText
            )
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeInstances' every 5 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkInstanceExists :: Wait.Wait DescribeInstances
mkInstanceExists =
  Wait.Wait
    { Wait._waitName = "InstanceExists",
      Wait._waitAttempts = 40,
      Wait._waitDelay = 5,
      Wait._waitAcceptors =
        [ Wait.matchStatus 200 Wait.AcceptSuccess,
          Wait.matchError "InvalidInstanceIDNotFound" Wait.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeInstanceStatus' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkInstanceStatusOK :: Wait.Wait DescribeInstanceStatus
mkInstanceStatusOK =
  Wait.Wait
    { Wait._waitName = "InstanceStatusOk",
      Wait._waitAttempts = 40,
      Wait._waitDelay = 15,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "ok"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( disrsInstanceStatuses Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. iInstanceStatus
                Lude.. Lens._Just
                Lude.. issStatus
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchError "InvalidInstanceID.NotFound" Wait.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.EC2.DescribeVolumes' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkVolumeAvailable :: Wait.Wait DescribeVolumes
mkVolumeAvailable =
  Wait.Wait
    { Wait._waitName = "VolumeAvailable",
      Wait._waitAttempts = 40,
      Wait._waitDelay = 15,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "available"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (dvvrsVolumes Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. vState
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "deleted"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dvvrsVolumes Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. vState
                Lude.. Lens.to Lude.toText
            )
        ]
    }
