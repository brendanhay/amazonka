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
-- Module      : Network.AWS.SMS.Types.ReplicationRunStageDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.ReplicationRunStageDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Details of the current stage of a replication run.
--
-- /See:/ 'newReplicationRunStageDetails' smart constructor.
data ReplicationRunStageDetails = ReplicationRunStageDetails'
  { -- | The current stage of a replication run.
    stage :: Core.Maybe Core.Text,
    -- | The progress of the current stage of a replication run.
    stageProgress :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ReplicationRunStageDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stage', 'replicationRunStageDetails_stage' - The current stage of a replication run.
--
-- 'stageProgress', 'replicationRunStageDetails_stageProgress' - The progress of the current stage of a replication run.
newReplicationRunStageDetails ::
  ReplicationRunStageDetails
newReplicationRunStageDetails =
  ReplicationRunStageDetails'
    { stage = Core.Nothing,
      stageProgress = Core.Nothing
    }

-- | The current stage of a replication run.
replicationRunStageDetails_stage :: Lens.Lens' ReplicationRunStageDetails (Core.Maybe Core.Text)
replicationRunStageDetails_stage = Lens.lens (\ReplicationRunStageDetails' {stage} -> stage) (\s@ReplicationRunStageDetails' {} a -> s {stage = a} :: ReplicationRunStageDetails)

-- | The progress of the current stage of a replication run.
replicationRunStageDetails_stageProgress :: Lens.Lens' ReplicationRunStageDetails (Core.Maybe Core.Text)
replicationRunStageDetails_stageProgress = Lens.lens (\ReplicationRunStageDetails' {stageProgress} -> stageProgress) (\s@ReplicationRunStageDetails' {} a -> s {stageProgress = a} :: ReplicationRunStageDetails)

instance Core.FromJSON ReplicationRunStageDetails where
  parseJSON =
    Core.withObject
      "ReplicationRunStageDetails"
      ( \x ->
          ReplicationRunStageDetails'
            Core.<$> (x Core..:? "stage")
            Core.<*> (x Core..:? "stageProgress")
      )

instance Core.Hashable ReplicationRunStageDetails

instance Core.NFData ReplicationRunStageDetails
