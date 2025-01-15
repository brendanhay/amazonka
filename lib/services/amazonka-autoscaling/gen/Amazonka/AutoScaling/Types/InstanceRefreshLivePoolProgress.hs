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
-- Module      : Amazonka.AutoScaling.Types.InstanceRefreshLivePoolProgress
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.InstanceRefreshLivePoolProgress where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Reports the progress of an instance refresh on instances that are in the
-- Auto Scaling group.
--
-- /See:/ 'newInstanceRefreshLivePoolProgress' smart constructor.
data InstanceRefreshLivePoolProgress = InstanceRefreshLivePoolProgress'
  { -- | The number of instances remaining to update.
    instancesToUpdate :: Prelude.Maybe Prelude.Natural,
    -- | The percentage of instances in the Auto Scaling group that have been
    -- replaced. For each instance replacement, Amazon EC2 Auto Scaling tracks
    -- the instance\'s health status and warm-up time. When the instance\'s
    -- health status changes to healthy and the specified warm-up time passes,
    -- the instance is considered updated and is added to the percentage
    -- complete.
    percentageComplete :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceRefreshLivePoolProgress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instancesToUpdate', 'instanceRefreshLivePoolProgress_instancesToUpdate' - The number of instances remaining to update.
--
-- 'percentageComplete', 'instanceRefreshLivePoolProgress_percentageComplete' - The percentage of instances in the Auto Scaling group that have been
-- replaced. For each instance replacement, Amazon EC2 Auto Scaling tracks
-- the instance\'s health status and warm-up time. When the instance\'s
-- health status changes to healthy and the specified warm-up time passes,
-- the instance is considered updated and is added to the percentage
-- complete.
newInstanceRefreshLivePoolProgress ::
  InstanceRefreshLivePoolProgress
newInstanceRefreshLivePoolProgress =
  InstanceRefreshLivePoolProgress'
    { instancesToUpdate =
        Prelude.Nothing,
      percentageComplete = Prelude.Nothing
    }

-- | The number of instances remaining to update.
instanceRefreshLivePoolProgress_instancesToUpdate :: Lens.Lens' InstanceRefreshLivePoolProgress (Prelude.Maybe Prelude.Natural)
instanceRefreshLivePoolProgress_instancesToUpdate = Lens.lens (\InstanceRefreshLivePoolProgress' {instancesToUpdate} -> instancesToUpdate) (\s@InstanceRefreshLivePoolProgress' {} a -> s {instancesToUpdate = a} :: InstanceRefreshLivePoolProgress)

-- | The percentage of instances in the Auto Scaling group that have been
-- replaced. For each instance replacement, Amazon EC2 Auto Scaling tracks
-- the instance\'s health status and warm-up time. When the instance\'s
-- health status changes to healthy and the specified warm-up time passes,
-- the instance is considered updated and is added to the percentage
-- complete.
instanceRefreshLivePoolProgress_percentageComplete :: Lens.Lens' InstanceRefreshLivePoolProgress (Prelude.Maybe Prelude.Natural)
instanceRefreshLivePoolProgress_percentageComplete = Lens.lens (\InstanceRefreshLivePoolProgress' {percentageComplete} -> percentageComplete) (\s@InstanceRefreshLivePoolProgress' {} a -> s {percentageComplete = a} :: InstanceRefreshLivePoolProgress)

instance Data.FromXML InstanceRefreshLivePoolProgress where
  parseXML x =
    InstanceRefreshLivePoolProgress'
      Prelude.<$> (x Data..@? "InstancesToUpdate")
      Prelude.<*> (x Data..@? "PercentageComplete")

instance
  Prelude.Hashable
    InstanceRefreshLivePoolProgress
  where
  hashWithSalt
    _salt
    InstanceRefreshLivePoolProgress' {..} =
      _salt
        `Prelude.hashWithSalt` instancesToUpdate
        `Prelude.hashWithSalt` percentageComplete

instance
  Prelude.NFData
    InstanceRefreshLivePoolProgress
  where
  rnf InstanceRefreshLivePoolProgress' {..} =
    Prelude.rnf instancesToUpdate `Prelude.seq`
      Prelude.rnf percentageComplete
