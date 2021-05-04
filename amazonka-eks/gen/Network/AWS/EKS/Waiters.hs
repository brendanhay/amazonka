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

import Network.AWS.EKS.DescribeAddon
import Network.AWS.EKS.DescribeCluster
import Network.AWS.EKS.DescribeNodegroup
import Network.AWS.EKS.Lens
import Network.AWS.EKS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.EKS.DescribeCluster' every 30 seconds until a successful state is reached. An error is returned after 40 failed checks.
newClusterActive :: Waiter.Wait DescribeCluster
newClusterActive =
  Waiter.Wait
    { Waiter._waitName = "ClusterActive",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 30,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "DELETING"
            Waiter.AcceptFailure
            ( describeClusterResponse_cluster Prelude.. Lens._Just
                Prelude.. cluster_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAll
            "FAILED"
            Waiter.AcceptFailure
            ( describeClusterResponse_cluster Prelude.. Lens._Just
                Prelude.. cluster_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAll
            "ACTIVE"
            Waiter.AcceptSuccess
            ( describeClusterResponse_cluster Prelude.. Lens._Just
                Prelude.. cluster_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.EKS.DescribeNodegroup' every 30 seconds until a successful state is reached. An error is returned after 80 failed checks.
newNodegroupActive :: Waiter.Wait DescribeNodegroup
newNodegroupActive =
  Waiter.Wait
    { Waiter._waitName = "NodegroupActive",
      Waiter._waitAttempts = 80,
      Waiter._waitDelay = 30,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "CREATE_FAILED"
            Waiter.AcceptFailure
            ( describeNodegroupResponse_nodegroup
                Prelude.. Lens._Just
                Prelude.. nodegroup_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAll
            "ACTIVE"
            Waiter.AcceptSuccess
            ( describeNodegroupResponse_nodegroup
                Prelude.. Lens._Just
                Prelude.. nodegroup_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.EKS.DescribeNodegroup' every 30 seconds until a successful state is reached. An error is returned after 40 failed checks.
newNodegroupDeleted :: Waiter.Wait DescribeNodegroup
newNodegroupDeleted =
  Waiter.Wait
    { Waiter._waitName = "NodegroupDeleted",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 30,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "DELETE_FAILED"
            Waiter.AcceptFailure
            ( describeNodegroupResponse_nodegroup
                Prelude.. Lens._Just
                Prelude.. nodegroup_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchError
            "ResourceNotFoundException"
            Waiter.AcceptSuccess
        ]
    }

-- | Polls 'Network.AWS.EKS.DescribeCluster' every 30 seconds until a successful state is reached. An error is returned after 40 failed checks.
newClusterDeleted :: Waiter.Wait DescribeCluster
newClusterDeleted =
  Waiter.Wait
    { Waiter._waitName = "ClusterDeleted",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 30,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "ACTIVE"
            Waiter.AcceptFailure
            ( describeClusterResponse_cluster Prelude.. Lens._Just
                Prelude.. cluster_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAll
            "CREATING"
            Waiter.AcceptFailure
            ( describeClusterResponse_cluster Prelude.. Lens._Just
                Prelude.. cluster_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchError
            "ResourceNotFoundException"
            Waiter.AcceptSuccess
        ]
    }

-- | Polls 'Network.AWS.EKS.DescribeAddon' every 10 seconds until a successful state is reached. An error is returned after 60 failed checks.
newAddonActive :: Waiter.Wait DescribeAddon
newAddonActive =
  Waiter.Wait
    { Waiter._waitName = "AddonActive",
      Waiter._waitAttempts = 60,
      Waiter._waitDelay = 10,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "CREATE_FAILED"
            Waiter.AcceptFailure
            ( describeAddonResponse_addon Prelude.. Lens._Just
                Prelude.. addon_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAll
            "ACTIVE"
            Waiter.AcceptSuccess
            ( describeAddonResponse_addon Prelude.. Lens._Just
                Prelude.. addon_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.EKS.DescribeAddon' every 10 seconds until a successful state is reached. An error is returned after 60 failed checks.
newAddonDeleted :: Waiter.Wait DescribeAddon
newAddonDeleted =
  Waiter.Wait
    { Waiter._waitName = "AddonDeleted",
      Waiter._waitAttempts = 60,
      Waiter._waitDelay = 10,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "DELETE_FAILED"
            Waiter.AcceptFailure
            ( describeAddonResponse_addon Prelude.. Lens._Just
                Prelude.. addon_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchError
            "ResourceNotFoundException"
            Waiter.AcceptSuccess
        ]
    }
