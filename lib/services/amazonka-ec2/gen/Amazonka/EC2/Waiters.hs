{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EC2.Waiters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Waiters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.DescribeBundleTasks
import Amazonka.EC2.DescribeConversionTasks
import Amazonka.EC2.DescribeCustomerGateways
import Amazonka.EC2.DescribeExportTasks
import Amazonka.EC2.DescribeImages
import Amazonka.EC2.DescribeInstanceStatus
import Amazonka.EC2.DescribeInstances
import Amazonka.EC2.DescribeInternetGateways
import Amazonka.EC2.DescribeKeyPairs
import Amazonka.EC2.DescribeNatGateways
import Amazonka.EC2.DescribeNetworkInterfaces
import Amazonka.EC2.DescribeSecurityGroups
import Amazonka.EC2.DescribeSnapshots
import Amazonka.EC2.DescribeSpotInstanceRequests
import Amazonka.EC2.DescribeSubnets
import Amazonka.EC2.DescribeVolumes
import Amazonka.EC2.DescribeVpcPeeringConnections
import Amazonka.EC2.DescribeVpcs
import Amazonka.EC2.DescribeVpnConnections
import Amazonka.EC2.GetPasswordData
import Amazonka.EC2.Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude

-- | Polls 'Amazonka.EC2.DescribeBundleTasks' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newBundleTaskComplete :: Core.Wait DescribeBundleTasks
newBundleTaskComplete =
  Core.Wait
    { Core.name = "BundleTaskComplete",
      Core.attempts = 40,
      Core.delay = 15,
      Core.acceptors =
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.EC2.DescribeConversionTasks' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newConversionTaskCancelled :: Core.Wait DescribeConversionTasks
newConversionTaskCancelled =
  Core.Wait
    { Core.name = "ConversionTaskCancelled",
      Core.attempts = 40,
      Core.delay = 15,
      Core.acceptors =
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
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.EC2.DescribeConversionTasks' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newConversionTaskCompleted :: Core.Wait DescribeConversionTasks
newConversionTaskCompleted =
  Core.Wait
    { Core.name = "ConversionTaskCompleted",
      Core.attempts = 40,
      Core.delay = 15,
      Core.acceptors =
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.EC2.DescribeConversionTasks' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newConversionTaskDeleted :: Core.Wait DescribeConversionTasks
newConversionTaskDeleted =
  Core.Wait
    { Core.name = "ConversionTaskDeleted",
      Core.attempts = 40,
      Core.delay = 15,
      Core.acceptors =
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
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.EC2.DescribeCustomerGateways' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newCustomerGatewayAvailable :: Core.Wait DescribeCustomerGateways
newCustomerGatewayAvailable =
  Core.Wait
    { Core.name = "CustomerGatewayAvailable",
      Core.attempts = 40,
      Core.delay = 15,
      Core.acceptors =
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.EC2.DescribeExportTasks' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newExportTaskCancelled :: Core.Wait DescribeExportTasks
newExportTaskCancelled =
  Core.Wait
    { Core.name = "ExportTaskCancelled",
      Core.attempts = 40,
      Core.delay = 15,
      Core.acceptors =
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
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.EC2.DescribeExportTasks' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newExportTaskCompleted :: Core.Wait DescribeExportTasks
newExportTaskCompleted =
  Core.Wait
    { Core.name = "ExportTaskCompleted",
      Core.attempts = 40,
      Core.delay = 15,
      Core.acceptors =
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
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.EC2.DescribeImages' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newImageAvailable :: Core.Wait DescribeImages
newImageAvailable =
  Core.Wait
    { Core.name = "ImageAvailable",
      Core.attempts = 40,
      Core.delay = 15,
      Core.acceptors =
        [ Core.matchAll
            "available"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (describeImagesResponse_images Prelude.. Lens._Just)
                )
                Prelude.. image_state
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "deregistered"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeImagesResponse_images Prelude.. Lens._Just)
                )
                Prelude.. image_state
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.EC2.DescribeImages' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newImageExists :: Core.Wait DescribeImages
newImageExists =
  Core.Wait
    { Core.name = "ImageExists",
      Core.attempts = 40,
      Core.delay = 15,
      Core.acceptors =
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

-- | Polls 'Amazonka.EC2.DescribeInstances' every 5 seconds until a successful state is reached. An error is returned after 40 failed checks.
newInstanceExists :: Core.Wait DescribeInstances
newInstanceExists =
  Core.Wait
    { Core.name = "InstanceExists",
      Core.attempts = 40,
      Core.delay = 5,
      Core.acceptors =
        [ Core.matchStatus 200 Core.AcceptSuccess,
          Core.matchError
            "InvalidInstanceIDNotFound"
            Core.AcceptRetry
        ]
    }

-- | Polls 'Amazonka.EC2.DescribeInstances' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newInstanceRunning :: Core.Wait DescribeInstances
newInstanceRunning =
  Core.Wait
    { Core.name = "InstanceRunning",
      Core.attempts = 40,
      Core.delay = 15,
      Core.acceptors =
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchError
            "InvalidInstanceID.NotFound"
            Core.AcceptRetry
        ]
    }

-- | Polls 'Amazonka.EC2.DescribeInstanceStatus' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newInstanceStatusOk :: Core.Wait DescribeInstanceStatus
newInstanceStatusOk =
  Core.Wait
    { Core.name = "InstanceStatusOk",
      Core.attempts = 40,
      Core.delay = 15,
      Core.acceptors =
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
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchError
            "InvalidInstanceID.NotFound"
            Core.AcceptRetry
        ]
    }

-- | Polls 'Amazonka.EC2.DescribeInstances' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newInstanceStopped :: Core.Wait DescribeInstances
newInstanceStopped =
  Core.Wait
    { Core.name = "InstanceStopped",
      Core.attempts = 40,
      Core.delay = 15,
      Core.acceptors =
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.EC2.DescribeInstances' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newInstanceTerminated :: Core.Wait DescribeInstances
newInstanceTerminated =
  Core.Wait
    { Core.name = "InstanceTerminated",
      Core.attempts = 40,
      Core.delay = 15,
      Core.acceptors =
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.EC2.DescribeInternetGateways' every 5 seconds until a successful state is reached. An error is returned after 6 failed checks.
newInternetGatewayExists :: Core.Wait DescribeInternetGateways
newInternetGatewayExists =
  Core.Wait
    { Core.name = "InternetGatewayExists",
      Core.attempts = 6,
      Core.delay = 5,
      Core.acceptors =
        [ Core.matchNonEmpty
            Prelude.True
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInternetGatewaysResponse_internetGateways
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. internetGateway_internetGatewayId
            ),
          Core.matchError
            "InvalidInternetGateway.NotFound"
            Core.AcceptRetry
        ]
    }

-- | Polls 'Amazonka.EC2.DescribeKeyPairs' every 5 seconds until a successful state is reached. An error is returned after 6 failed checks.
newKeyPairExists :: Core.Wait DescribeKeyPairs
newKeyPairExists =
  Core.Wait
    { Core.name = "KeyPairExists",
      Core.attempts = 6,
      Core.delay = 5,
      Core.acceptors =
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

-- | Polls 'Amazonka.EC2.DescribeNatGateways' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newNatGatewayAvailable :: Core.Wait DescribeNatGateways
newNatGatewayAvailable =
  Core.Wait
    { Core.name = "NatGatewayAvailable",
      Core.attempts = 40,
      Core.delay = 15,
      Core.acceptors =
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchError
            "NatGatewayNotFound"
            Core.AcceptRetry
        ]
    }

-- | Polls 'Amazonka.EC2.DescribeNatGateways' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newNatGatewayDeleted :: Core.Wait DescribeNatGateways
newNatGatewayDeleted =
  Core.Wait
    { Core.name = "NatGatewayDeleted",
      Core.attempts = 40,
      Core.delay = 15,
      Core.acceptors =
        [ Core.matchAll
            "deleted"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeNatGatewaysResponse_natGateways
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. natGateway_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchError
            "NatGatewayNotFound"
            Core.AcceptSuccess
        ]
    }

-- | Polls 'Amazonka.EC2.DescribeNetworkInterfaces' every 20 seconds until a successful state is reached. An error is returned after 10 failed checks.
newNetworkInterfaceAvailable :: Core.Wait DescribeNetworkInterfaces
newNetworkInterfaceAvailable =
  Core.Wait
    { Core.name = "NetworkInterfaceAvailable",
      Core.attempts = 10,
      Core.delay = 20,
      Core.acceptors =
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
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchError
            "InvalidNetworkInterfaceID.NotFound"
            Core.AcceptFailure
        ]
    }

-- | Polls 'Amazonka.EC2.GetPasswordData' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newPasswordDataAvailable :: Core.Wait GetPasswordData
newPasswordDataAvailable =
  Core.Wait
    { Core.name = "PasswordDataAvailable",
      Core.attempts = 40,
      Core.delay = 15,
      Core.acceptors =
        [ Core.matchAll
            Prelude.True
            Core.AcceptSuccess
            ( Core.nonEmptyText
                getPasswordDataResponse_passwordData
            )
        ]
    }

-- | Polls 'Amazonka.EC2.DescribeSecurityGroups' every 5 seconds until a successful state is reached. An error is returned after 6 failed checks.
newSecurityGroupExists :: Core.Wait DescribeSecurityGroups
newSecurityGroupExists =
  Core.Wait
    { Core.name = "SecurityGroupExists",
      Core.attempts = 6,
      Core.delay = 5,
      Core.acceptors =
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
            "InvalidGroup.NotFound"
            Core.AcceptRetry
        ]
    }

-- | Polls 'Amazonka.EC2.DescribeSnapshots' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newSnapshotCompleted :: Core.Wait DescribeSnapshots
newSnapshotCompleted =
  Core.Wait
    { Core.name = "SnapshotCompleted",
      Core.attempts = 40,
      Core.delay = 15,
      Core.acceptors =
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
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "error"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeSnapshotsResponse_snapshots
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. snapshot_state
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.EC2.DescribeSpotInstanceRequests' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newSpotInstanceRequestFulfilled :: Core.Wait DescribeSpotInstanceRequests
newSpotInstanceRequestFulfilled =
  Core.Wait
    { Core.name =
        "SpotInstanceRequestFulfilled",
      Core.attempts = 40,
      Core.delay = 15,
      Core.acceptors =
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchError
            "InvalidSpotInstanceRequestID.NotFound"
            Core.AcceptRetry
        ]
    }

-- | Polls 'Amazonka.EC2.DescribeSubnets' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newSubnetAvailable :: Core.Wait DescribeSubnets
newSubnetAvailable =
  Core.Wait
    { Core.name = "SubnetAvailable",
      Core.attempts = 40,
      Core.delay = 15,
      Core.acceptors =
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
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.EC2.DescribeInstanceStatus' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newSystemStatusOk :: Core.Wait DescribeInstanceStatus
newSystemStatusOk =
  Core.Wait
    { Core.name = "SystemStatusOk",
      Core.attempts = 40,
      Core.delay = 15,
      Core.acceptors =
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
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.EC2.DescribeVolumes' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newVolumeAvailable :: Core.Wait DescribeVolumes
newVolumeAvailable =
  Core.Wait
    { Core.name = "VolumeAvailable",
      Core.attempts = 40,
      Core.delay = 15,
      Core.acceptors =
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.EC2.DescribeVolumes' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newVolumeDeleted :: Core.Wait DescribeVolumes
newVolumeDeleted =
  Core.Wait
    { Core.name = "VolumeDeleted",
      Core.attempts = 40,
      Core.delay = 15,
      Core.acceptors =
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
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchError
            "InvalidVolume.NotFound"
            Core.AcceptSuccess
        ]
    }

-- | Polls 'Amazonka.EC2.DescribeVolumes' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newVolumeInUse :: Core.Wait DescribeVolumes
newVolumeInUse =
  Core.Wait
    { Core.name = "VolumeInUse",
      Core.attempts = 40,
      Core.delay = 15,
      Core.acceptors =
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.EC2.DescribeVpcs' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newVpcAvailable :: Core.Wait DescribeVpcs
newVpcAvailable =
  Core.Wait
    { Core.name = "VpcAvailable",
      Core.attempts = 40,
      Core.delay = 15,
      Core.acceptors =
        [ Core.matchAll
            "available"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (describeVpcsResponse_vpcs Prelude.. Lens._Just)
                )
                Prelude.. vpc_state
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.EC2.DescribeVpcs' every 1 seconds until a successful state is reached. An error is returned after 5 failed checks.
newVpcExists :: Core.Wait DescribeVpcs
newVpcExists =
  Core.Wait
    { Core.name = "VpcExists",
      Core.attempts = 5,
      Core.delay = 1,
      Core.acceptors =
        [ Core.matchStatus 200 Core.AcceptSuccess,
          Core.matchError
            "InvalidVpcID.NotFound"
            Core.AcceptRetry
        ]
    }

-- | Polls 'Amazonka.EC2.DescribeVpcPeeringConnections' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newVpcPeeringConnectionDeleted :: Core.Wait DescribeVpcPeeringConnections
newVpcPeeringConnectionDeleted =
  Core.Wait
    { Core.name =
        "VpcPeeringConnectionDeleted",
      Core.attempts = 40,
      Core.delay = 15,
      Core.acceptors =
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
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchError
            "InvalidVpcPeeringConnectionID.NotFound"
            Core.AcceptSuccess
        ]
    }

-- | Polls 'Amazonka.EC2.DescribeVpcPeeringConnections' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newVpcPeeringConnectionExists :: Core.Wait DescribeVpcPeeringConnections
newVpcPeeringConnectionExists =
  Core.Wait
    { Core.name = "VpcPeeringConnectionExists",
      Core.attempts = 40,
      Core.delay = 15,
      Core.acceptors =
        [ Core.matchStatus 200 Core.AcceptSuccess,
          Core.matchError
            "InvalidVpcPeeringConnectionID.NotFound"
            Core.AcceptRetry
        ]
    }

-- | Polls 'Amazonka.EC2.DescribeVpnConnections' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newVpnConnectionAvailable :: Core.Wait DescribeVpnConnections
newVpnConnectionAvailable =
  Core.Wait
    { Core.name = "VpnConnectionAvailable",
      Core.attempts = 40,
      Core.delay = 15,
      Core.acceptors =
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.EC2.DescribeVpnConnections' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newVpnConnectionDeleted :: Core.Wait DescribeVpnConnections
newVpnConnectionDeleted =
  Core.Wait
    { Core.name = "VpnConnectionDeleted",
      Core.attempts = 40,
      Core.delay = 15,
      Core.acceptors =
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }
