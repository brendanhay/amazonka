{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EKS.Waiters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EKS.Waiters where

import qualified Network.AWS.Core as Core
import Network.AWS.EKS.DescribeAddon
import Network.AWS.EKS.DescribeCluster
import Network.AWS.EKS.DescribeFargateProfile
import Network.AWS.EKS.DescribeNodegroup
import Network.AWS.EKS.Lens
import Network.AWS.EKS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Polls 'Network.AWS.EKS.DescribeFargateProfile' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
newFargateProfileDeleted :: Core.Wait DescribeFargateProfile
newFargateProfileDeleted =
  Core.Wait
    { Core._waitName = "FargateProfileDeleted",
      Core._waitAttempts = 60,
      Core._waitDelay = 30,
      Core._waitAcceptors =
        [ Core.matchAll
            "DELETE_FAILED"
            Core.AcceptFailure
            ( describeFargateProfileResponse_fargateProfile
                Prelude.. Lens._Just
                Prelude.. fargateProfile_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchError
            "ResourceNotFoundException"
            Core.AcceptSuccess
        ]
    }

-- | Polls 'Network.AWS.EKS.DescribeCluster' every 30 seconds until a successful state is reached. An error is returned after 40 failed checks.
newClusterActive :: Core.Wait DescribeCluster
newClusterActive =
  Core.Wait
    { Core._waitName = "ClusterActive",
      Core._waitAttempts = 40,
      Core._waitDelay = 30,
      Core._waitAcceptors =
        [ Core.matchAll
            "DELETING"
            Core.AcceptFailure
            ( describeClusterResponse_cluster Prelude.. Lens._Just
                Prelude.. cluster_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "FAILED"
            Core.AcceptFailure
            ( describeClusterResponse_cluster Prelude.. Lens._Just
                Prelude.. cluster_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "ACTIVE"
            Core.AcceptSuccess
            ( describeClusterResponse_cluster Prelude.. Lens._Just
                Prelude.. cluster_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.EKS.DescribeNodegroup' every 30 seconds until a successful state is reached. An error is returned after 80 failed checks.
newNodegroupActive :: Core.Wait DescribeNodegroup
newNodegroupActive =
  Core.Wait
    { Core._waitName = "NodegroupActive",
      Core._waitAttempts = 80,
      Core._waitDelay = 30,
      Core._waitAcceptors =
        [ Core.matchAll
            "CREATE_FAILED"
            Core.AcceptFailure
            ( describeNodegroupResponse_nodegroup
                Prelude.. Lens._Just
                Prelude.. nodegroup_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "ACTIVE"
            Core.AcceptSuccess
            ( describeNodegroupResponse_nodegroup
                Prelude.. Lens._Just
                Prelude.. nodegroup_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.EKS.DescribeNodegroup' every 30 seconds until a successful state is reached. An error is returned after 40 failed checks.
newNodegroupDeleted :: Core.Wait DescribeNodegroup
newNodegroupDeleted =
  Core.Wait
    { Core._waitName = "NodegroupDeleted",
      Core._waitAttempts = 40,
      Core._waitDelay = 30,
      Core._waitAcceptors =
        [ Core.matchAll
            "DELETE_FAILED"
            Core.AcceptFailure
            ( describeNodegroupResponse_nodegroup
                Prelude.. Lens._Just
                Prelude.. nodegroup_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchError
            "ResourceNotFoundException"
            Core.AcceptSuccess
        ]
    }

-- | Polls 'Network.AWS.EKS.DescribeCluster' every 30 seconds until a successful state is reached. An error is returned after 40 failed checks.
newClusterDeleted :: Core.Wait DescribeCluster
newClusterDeleted =
  Core.Wait
    { Core._waitName = "ClusterDeleted",
      Core._waitAttempts = 40,
      Core._waitDelay = 30,
      Core._waitAcceptors =
        [ Core.matchAll
            "ACTIVE"
            Core.AcceptFailure
            ( describeClusterResponse_cluster Prelude.. Lens._Just
                Prelude.. cluster_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "CREATING"
            Core.AcceptFailure
            ( describeClusterResponse_cluster Prelude.. Lens._Just
                Prelude.. cluster_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "PENDING"
            Core.AcceptFailure
            ( describeClusterResponse_cluster Prelude.. Lens._Just
                Prelude.. cluster_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchError
            "ResourceNotFoundException"
            Core.AcceptSuccess
        ]
    }

-- | Polls 'Network.AWS.EKS.DescribeAddon' every 10 seconds until a successful state is reached. An error is returned after 60 failed checks.
newAddonActive :: Core.Wait DescribeAddon
newAddonActive =
  Core.Wait
    { Core._waitName = "AddonActive",
      Core._waitAttempts = 60,
      Core._waitDelay = 10,
      Core._waitAcceptors =
        [ Core.matchAll
            "CREATE_FAILED"
            Core.AcceptFailure
            ( describeAddonResponse_addon Prelude.. Lens._Just
                Prelude.. addon_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "DEGRADED"
            Core.AcceptFailure
            ( describeAddonResponse_addon Prelude.. Lens._Just
                Prelude.. addon_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "ACTIVE"
            Core.AcceptSuccess
            ( describeAddonResponse_addon Prelude.. Lens._Just
                Prelude.. addon_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.EKS.DescribeFargateProfile' every 10 seconds until a successful state is reached. An error is returned after 60 failed checks.
newFargateProfileActive :: Core.Wait DescribeFargateProfile
newFargateProfileActive =
  Core.Wait
    { Core._waitName = "FargateProfileActive",
      Core._waitAttempts = 60,
      Core._waitDelay = 10,
      Core._waitAcceptors =
        [ Core.matchAll
            "CREATE_FAILED"
            Core.AcceptFailure
            ( describeFargateProfileResponse_fargateProfile
                Prelude.. Lens._Just
                Prelude.. fargateProfile_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "ACTIVE"
            Core.AcceptSuccess
            ( describeFargateProfileResponse_fargateProfile
                Prelude.. Lens._Just
                Prelude.. fargateProfile_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.EKS.DescribeAddon' every 10 seconds until a successful state is reached. An error is returned after 60 failed checks.
newAddonDeleted :: Core.Wait DescribeAddon
newAddonDeleted =
  Core.Wait
    { Core._waitName = "AddonDeleted",
      Core._waitAttempts = 60,
      Core._waitDelay = 10,
      Core._waitAcceptors =
        [ Core.matchAll
            "DELETE_FAILED"
            Core.AcceptFailure
            ( describeAddonResponse_addon Prelude.. Lens._Just
                Prelude.. addon_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchError
            "ResourceNotFoundException"
            Core.AcceptSuccess
        ]
    }
