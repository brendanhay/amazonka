{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EKS.Waiters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EKS.Waiters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EKS.DescribeAddon
import Amazonka.EKS.DescribeCluster
import Amazonka.EKS.DescribeFargateProfile
import Amazonka.EKS.DescribeNodegroup
import Amazonka.EKS.Lens
import Amazonka.EKS.Types
import qualified Amazonka.Prelude as Prelude

-- | Polls 'Amazonka.EKS.DescribeAddon' every 10 seconds until a successful state is reached. An error is returned after 60 failed checks.
newAddonActive :: Core.Wait DescribeAddon
newAddonActive =
  Core.Wait
    { Core.name = "AddonActive",
      Core.attempts = 60,
      Core.delay = 10,
      Core.acceptors =
        [ Core.matchAll
            "CREATE_FAILED"
            Core.AcceptFailure
            ( describeAddonResponse_addon Prelude.. Lens._Just
                Prelude.. addon_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "DEGRADED"
            Core.AcceptFailure
            ( describeAddonResponse_addon Prelude.. Lens._Just
                Prelude.. addon_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "ACTIVE"
            Core.AcceptSuccess
            ( describeAddonResponse_addon Prelude.. Lens._Just
                Prelude.. addon_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.EKS.DescribeAddon' every 10 seconds until a successful state is reached. An error is returned after 60 failed checks.
newAddonDeleted :: Core.Wait DescribeAddon
newAddonDeleted =
  Core.Wait
    { Core.name = "AddonDeleted",
      Core.attempts = 60,
      Core.delay = 10,
      Core.acceptors =
        [ Core.matchAll
            "DELETE_FAILED"
            Core.AcceptFailure
            ( describeAddonResponse_addon Prelude.. Lens._Just
                Prelude.. addon_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchError
            "ResourceNotFoundException"
            Core.AcceptSuccess
        ]
    }

-- | Polls 'Amazonka.EKS.DescribeCluster' every 30 seconds until a successful state is reached. An error is returned after 40 failed checks.
newClusterActive :: Core.Wait DescribeCluster
newClusterActive =
  Core.Wait
    { Core.name = "ClusterActive",
      Core.attempts = 40,
      Core.delay = 30,
      Core.acceptors =
        [ Core.matchAll
            "DELETING"
            Core.AcceptFailure
            ( describeClusterResponse_cluster Prelude.. Lens._Just
                Prelude.. cluster_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "FAILED"
            Core.AcceptFailure
            ( describeClusterResponse_cluster Prelude.. Lens._Just
                Prelude.. cluster_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "ACTIVE"
            Core.AcceptSuccess
            ( describeClusterResponse_cluster Prelude.. Lens._Just
                Prelude.. cluster_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.EKS.DescribeCluster' every 30 seconds until a successful state is reached. An error is returned after 40 failed checks.
newClusterDeleted :: Core.Wait DescribeCluster
newClusterDeleted =
  Core.Wait
    { Core.name = "ClusterDeleted",
      Core.attempts = 40,
      Core.delay = 30,
      Core.acceptors =
        [ Core.matchAll
            "ACTIVE"
            Core.AcceptFailure
            ( describeClusterResponse_cluster Prelude.. Lens._Just
                Prelude.. cluster_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "CREATING"
            Core.AcceptFailure
            ( describeClusterResponse_cluster Prelude.. Lens._Just
                Prelude.. cluster_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "PENDING"
            Core.AcceptFailure
            ( describeClusterResponse_cluster Prelude.. Lens._Just
                Prelude.. cluster_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchError
            "ResourceNotFoundException"
            Core.AcceptSuccess
        ]
    }

-- | Polls 'Amazonka.EKS.DescribeFargateProfile' every 10 seconds until a successful state is reached. An error is returned after 60 failed checks.
newFargateProfileActive :: Core.Wait DescribeFargateProfile
newFargateProfileActive =
  Core.Wait
    { Core.name = "FargateProfileActive",
      Core.attempts = 60,
      Core.delay = 10,
      Core.acceptors =
        [ Core.matchAll
            "CREATE_FAILED"
            Core.AcceptFailure
            ( describeFargateProfileResponse_fargateProfile
                Prelude.. Lens._Just
                Prelude.. fargateProfile_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "ACTIVE"
            Core.AcceptSuccess
            ( describeFargateProfileResponse_fargateProfile
                Prelude.. Lens._Just
                Prelude.. fargateProfile_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.EKS.DescribeFargateProfile' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
newFargateProfileDeleted :: Core.Wait DescribeFargateProfile
newFargateProfileDeleted =
  Core.Wait
    { Core.name = "FargateProfileDeleted",
      Core.attempts = 60,
      Core.delay = 30,
      Core.acceptors =
        [ Core.matchAll
            "DELETE_FAILED"
            Core.AcceptFailure
            ( describeFargateProfileResponse_fargateProfile
                Prelude.. Lens._Just
                Prelude.. fargateProfile_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchError
            "ResourceNotFoundException"
            Core.AcceptSuccess
        ]
    }

-- | Polls 'Amazonka.EKS.DescribeNodegroup' every 30 seconds until a successful state is reached. An error is returned after 80 failed checks.
newNodegroupActive :: Core.Wait DescribeNodegroup
newNodegroupActive =
  Core.Wait
    { Core.name = "NodegroupActive",
      Core.attempts = 80,
      Core.delay = 30,
      Core.acceptors =
        [ Core.matchAll
            "CREATE_FAILED"
            Core.AcceptFailure
            ( describeNodegroupResponse_nodegroup
                Prelude.. Lens._Just
                Prelude.. nodegroup_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "ACTIVE"
            Core.AcceptSuccess
            ( describeNodegroupResponse_nodegroup
                Prelude.. Lens._Just
                Prelude.. nodegroup_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.EKS.DescribeNodegroup' every 30 seconds until a successful state is reached. An error is returned after 40 failed checks.
newNodegroupDeleted :: Core.Wait DescribeNodegroup
newNodegroupDeleted =
  Core.Wait
    { Core.name = "NodegroupDeleted",
      Core.attempts = 40,
      Core.delay = 30,
      Core.acceptors =
        [ Core.matchAll
            "DELETE_FAILED"
            Core.AcceptFailure
            ( describeNodegroupResponse_nodegroup
                Prelude.. Lens._Just
                Prelude.. nodegroup_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchError
            "ResourceNotFoundException"
            Core.AcceptSuccess
        ]
    }
