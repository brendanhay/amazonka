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
-- Module      : Amazonka.SMS.Types.ReplicationRunStageDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SMS.Types.ReplicationRunStageDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details of the current stage of a replication run.
--
-- /See:/ 'newReplicationRunStageDetails' smart constructor.
data ReplicationRunStageDetails = ReplicationRunStageDetails'
  { -- | The progress of the current stage of a replication run.
    stageProgress :: Prelude.Maybe Prelude.Text,
    -- | The current stage of a replication run.
    stage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplicationRunStageDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stageProgress', 'replicationRunStageDetails_stageProgress' - The progress of the current stage of a replication run.
--
-- 'stage', 'replicationRunStageDetails_stage' - The current stage of a replication run.
newReplicationRunStageDetails ::
  ReplicationRunStageDetails
newReplicationRunStageDetails =
  ReplicationRunStageDetails'
    { stageProgress =
        Prelude.Nothing,
      stage = Prelude.Nothing
    }

-- | The progress of the current stage of a replication run.
replicationRunStageDetails_stageProgress :: Lens.Lens' ReplicationRunStageDetails (Prelude.Maybe Prelude.Text)
replicationRunStageDetails_stageProgress = Lens.lens (\ReplicationRunStageDetails' {stageProgress} -> stageProgress) (\s@ReplicationRunStageDetails' {} a -> s {stageProgress = a} :: ReplicationRunStageDetails)

-- | The current stage of a replication run.
replicationRunStageDetails_stage :: Lens.Lens' ReplicationRunStageDetails (Prelude.Maybe Prelude.Text)
replicationRunStageDetails_stage = Lens.lens (\ReplicationRunStageDetails' {stage} -> stage) (\s@ReplicationRunStageDetails' {} a -> s {stage = a} :: ReplicationRunStageDetails)

instance Data.FromJSON ReplicationRunStageDetails where
  parseJSON =
    Data.withObject
      "ReplicationRunStageDetails"
      ( \x ->
          ReplicationRunStageDetails'
            Prelude.<$> (x Data..:? "stageProgress")
            Prelude.<*> (x Data..:? "stage")
      )

instance Prelude.Hashable ReplicationRunStageDetails where
  hashWithSalt _salt ReplicationRunStageDetails' {..} =
    _salt `Prelude.hashWithSalt` stageProgress
      `Prelude.hashWithSalt` stage

instance Prelude.NFData ReplicationRunStageDetails where
  rnf ReplicationRunStageDetails' {..} =
    Prelude.rnf stageProgress
      `Prelude.seq` Prelude.rnf stage
