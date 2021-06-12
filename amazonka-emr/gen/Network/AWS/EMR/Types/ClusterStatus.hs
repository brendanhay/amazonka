{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.ClusterStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.ClusterStatus where

import qualified Network.AWS.Core as Core
import Network.AWS.EMR.Types.ClusterState
import Network.AWS.EMR.Types.ClusterStateChangeReason
import Network.AWS.EMR.Types.ClusterTimeline
import qualified Network.AWS.Lens as Lens

-- | The detailed status of the cluster.
--
-- /See:/ 'newClusterStatus' smart constructor.
data ClusterStatus = ClusterStatus'
  { -- | The reason for the cluster status change.
    stateChangeReason :: Core.Maybe ClusterStateChangeReason,
    -- | The current state of the cluster.
    state :: Core.Maybe ClusterState,
    -- | A timeline that represents the status of a cluster over the lifetime of
    -- the cluster.
    timeline :: Core.Maybe ClusterTimeline
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ClusterStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stateChangeReason', 'clusterStatus_stateChangeReason' - The reason for the cluster status change.
--
-- 'state', 'clusterStatus_state' - The current state of the cluster.
--
-- 'timeline', 'clusterStatus_timeline' - A timeline that represents the status of a cluster over the lifetime of
-- the cluster.
newClusterStatus ::
  ClusterStatus
newClusterStatus =
  ClusterStatus'
    { stateChangeReason = Core.Nothing,
      state = Core.Nothing,
      timeline = Core.Nothing
    }

-- | The reason for the cluster status change.
clusterStatus_stateChangeReason :: Lens.Lens' ClusterStatus (Core.Maybe ClusterStateChangeReason)
clusterStatus_stateChangeReason = Lens.lens (\ClusterStatus' {stateChangeReason} -> stateChangeReason) (\s@ClusterStatus' {} a -> s {stateChangeReason = a} :: ClusterStatus)

-- | The current state of the cluster.
clusterStatus_state :: Lens.Lens' ClusterStatus (Core.Maybe ClusterState)
clusterStatus_state = Lens.lens (\ClusterStatus' {state} -> state) (\s@ClusterStatus' {} a -> s {state = a} :: ClusterStatus)

-- | A timeline that represents the status of a cluster over the lifetime of
-- the cluster.
clusterStatus_timeline :: Lens.Lens' ClusterStatus (Core.Maybe ClusterTimeline)
clusterStatus_timeline = Lens.lens (\ClusterStatus' {timeline} -> timeline) (\s@ClusterStatus' {} a -> s {timeline = a} :: ClusterStatus)

instance Core.FromJSON ClusterStatus where
  parseJSON =
    Core.withObject
      "ClusterStatus"
      ( \x ->
          ClusterStatus'
            Core.<$> (x Core..:? "StateChangeReason")
            Core.<*> (x Core..:? "State")
            Core.<*> (x Core..:? "Timeline")
      )

instance Core.Hashable ClusterStatus

instance Core.NFData ClusterStatus
