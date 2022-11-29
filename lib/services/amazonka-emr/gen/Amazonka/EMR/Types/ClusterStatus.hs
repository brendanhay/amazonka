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
-- Module      : Amazonka.EMR.Types.ClusterStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.ClusterStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EMR.Types.ClusterState
import Amazonka.EMR.Types.ClusterStateChangeReason
import Amazonka.EMR.Types.ClusterTimeline
import qualified Amazonka.Prelude as Prelude

-- | The detailed status of the cluster.
--
-- /See:/ 'newClusterStatus' smart constructor.
data ClusterStatus = ClusterStatus'
  { -- | The reason for the cluster status change.
    stateChangeReason :: Prelude.Maybe ClusterStateChangeReason,
    -- | A timeline that represents the status of a cluster over the lifetime of
    -- the cluster.
    timeline :: Prelude.Maybe ClusterTimeline,
    -- | The current state of the cluster.
    state :: Prelude.Maybe ClusterState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'timeline', 'clusterStatus_timeline' - A timeline that represents the status of a cluster over the lifetime of
-- the cluster.
--
-- 'state', 'clusterStatus_state' - The current state of the cluster.
newClusterStatus ::
  ClusterStatus
newClusterStatus =
  ClusterStatus'
    { stateChangeReason = Prelude.Nothing,
      timeline = Prelude.Nothing,
      state = Prelude.Nothing
    }

-- | The reason for the cluster status change.
clusterStatus_stateChangeReason :: Lens.Lens' ClusterStatus (Prelude.Maybe ClusterStateChangeReason)
clusterStatus_stateChangeReason = Lens.lens (\ClusterStatus' {stateChangeReason} -> stateChangeReason) (\s@ClusterStatus' {} a -> s {stateChangeReason = a} :: ClusterStatus)

-- | A timeline that represents the status of a cluster over the lifetime of
-- the cluster.
clusterStatus_timeline :: Lens.Lens' ClusterStatus (Prelude.Maybe ClusterTimeline)
clusterStatus_timeline = Lens.lens (\ClusterStatus' {timeline} -> timeline) (\s@ClusterStatus' {} a -> s {timeline = a} :: ClusterStatus)

-- | The current state of the cluster.
clusterStatus_state :: Lens.Lens' ClusterStatus (Prelude.Maybe ClusterState)
clusterStatus_state = Lens.lens (\ClusterStatus' {state} -> state) (\s@ClusterStatus' {} a -> s {state = a} :: ClusterStatus)

instance Core.FromJSON ClusterStatus where
  parseJSON =
    Core.withObject
      "ClusterStatus"
      ( \x ->
          ClusterStatus'
            Prelude.<$> (x Core..:? "StateChangeReason")
            Prelude.<*> (x Core..:? "Timeline")
            Prelude.<*> (x Core..:? "State")
      )

instance Prelude.Hashable ClusterStatus where
  hashWithSalt _salt ClusterStatus' {..} =
    _salt `Prelude.hashWithSalt` stateChangeReason
      `Prelude.hashWithSalt` timeline
      `Prelude.hashWithSalt` state

instance Prelude.NFData ClusterStatus where
  rnf ClusterStatus' {..} =
    Prelude.rnf stateChangeReason
      `Prelude.seq` Prelude.rnf timeline
      `Prelude.seq` Prelude.rnf state
