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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.ClusterStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types.ClusterState
import Amazonka.EMR.Types.ClusterStateChangeReason
import Amazonka.EMR.Types.ClusterTimeline
import qualified Amazonka.Prelude as Prelude

-- | The detailed status of the cluster.
--
-- /See:/ 'newClusterStatus' smart constructor.
data ClusterStatus = ClusterStatus'
  { -- | The current state of the cluster.
    state :: Prelude.Maybe ClusterState,
    -- | The reason for the cluster status change.
    stateChangeReason :: Prelude.Maybe ClusterStateChangeReason,
    -- | A timeline that represents the status of a cluster over the lifetime of
    -- the cluster.
    timeline :: Prelude.Maybe ClusterTimeline
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
-- 'state', 'clusterStatus_state' - The current state of the cluster.
--
-- 'stateChangeReason', 'clusterStatus_stateChangeReason' - The reason for the cluster status change.
--
-- 'timeline', 'clusterStatus_timeline' - A timeline that represents the status of a cluster over the lifetime of
-- the cluster.
newClusterStatus ::
  ClusterStatus
newClusterStatus =
  ClusterStatus'
    { state = Prelude.Nothing,
      stateChangeReason = Prelude.Nothing,
      timeline = Prelude.Nothing
    }

-- | The current state of the cluster.
clusterStatus_state :: Lens.Lens' ClusterStatus (Prelude.Maybe ClusterState)
clusterStatus_state = Lens.lens (\ClusterStatus' {state} -> state) (\s@ClusterStatus' {} a -> s {state = a} :: ClusterStatus)

-- | The reason for the cluster status change.
clusterStatus_stateChangeReason :: Lens.Lens' ClusterStatus (Prelude.Maybe ClusterStateChangeReason)
clusterStatus_stateChangeReason = Lens.lens (\ClusterStatus' {stateChangeReason} -> stateChangeReason) (\s@ClusterStatus' {} a -> s {stateChangeReason = a} :: ClusterStatus)

-- | A timeline that represents the status of a cluster over the lifetime of
-- the cluster.
clusterStatus_timeline :: Lens.Lens' ClusterStatus (Prelude.Maybe ClusterTimeline)
clusterStatus_timeline = Lens.lens (\ClusterStatus' {timeline} -> timeline) (\s@ClusterStatus' {} a -> s {timeline = a} :: ClusterStatus)

instance Data.FromJSON ClusterStatus where
  parseJSON =
    Data.withObject
      "ClusterStatus"
      ( \x ->
          ClusterStatus'
            Prelude.<$> (x Data..:? "State")
            Prelude.<*> (x Data..:? "StateChangeReason")
            Prelude.<*> (x Data..:? "Timeline")
      )

instance Prelude.Hashable ClusterStatus where
  hashWithSalt _salt ClusterStatus' {..} =
    _salt
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` stateChangeReason
      `Prelude.hashWithSalt` timeline

instance Prelude.NFData ClusterStatus where
  rnf ClusterStatus' {..} =
    Prelude.rnf state `Prelude.seq`
      Prelude.rnf stateChangeReason `Prelude.seq`
        Prelude.rnf timeline
