{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.EMR.Types.ClusterState
import Network.AWS.EMR.Types.ClusterStateChangeReason
import Network.AWS.EMR.Types.ClusterTimeline
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The detailed status of the cluster.
--
-- /See:/ 'newClusterStatus' smart constructor.
data ClusterStatus = ClusterStatus'
  { -- | The reason for the cluster status change.
    stateChangeReason :: Prelude.Maybe ClusterStateChangeReason,
    -- | The current state of the cluster.
    state :: Prelude.Maybe ClusterState,
    -- | A timeline that represents the status of a cluster over the lifetime of
    -- the cluster.
    timeline :: Prelude.Maybe ClusterTimeline
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { stateChangeReason = Prelude.Nothing,
      state = Prelude.Nothing,
      timeline = Prelude.Nothing
    }

-- | The reason for the cluster status change.
clusterStatus_stateChangeReason :: Lens.Lens' ClusterStatus (Prelude.Maybe ClusterStateChangeReason)
clusterStatus_stateChangeReason = Lens.lens (\ClusterStatus' {stateChangeReason} -> stateChangeReason) (\s@ClusterStatus' {} a -> s {stateChangeReason = a} :: ClusterStatus)

-- | The current state of the cluster.
clusterStatus_state :: Lens.Lens' ClusterStatus (Prelude.Maybe ClusterState)
clusterStatus_state = Lens.lens (\ClusterStatus' {state} -> state) (\s@ClusterStatus' {} a -> s {state = a} :: ClusterStatus)

-- | A timeline that represents the status of a cluster over the lifetime of
-- the cluster.
clusterStatus_timeline :: Lens.Lens' ClusterStatus (Prelude.Maybe ClusterTimeline)
clusterStatus_timeline = Lens.lens (\ClusterStatus' {timeline} -> timeline) (\s@ClusterStatus' {} a -> s {timeline = a} :: ClusterStatus)

instance Prelude.FromJSON ClusterStatus where
  parseJSON =
    Prelude.withObject
      "ClusterStatus"
      ( \x ->
          ClusterStatus'
            Prelude.<$> (x Prelude..:? "StateChangeReason")
            Prelude.<*> (x Prelude..:? "State")
            Prelude.<*> (x Prelude..:? "Timeline")
      )

instance Prelude.Hashable ClusterStatus

instance Prelude.NFData ClusterStatus
