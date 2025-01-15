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
-- Module      : Amazonka.AutoScaling.Types.InstanceRefreshWarmPoolProgress
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.InstanceRefreshWarmPoolProgress where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Reports the progress of an instance refresh on instances that are in the
-- warm pool.
--
-- /See:/ 'newInstanceRefreshWarmPoolProgress' smart constructor.
data InstanceRefreshWarmPoolProgress = InstanceRefreshWarmPoolProgress'
  { -- | The number of instances remaining to update.
    instancesToUpdate :: Prelude.Maybe Prelude.Natural,
    -- | The percentage of instances in the warm pool that have been replaced.
    -- For each instance replacement, Amazon EC2 Auto Scaling tracks the
    -- instance\'s health status and warm-up time. When the instance\'s health
    -- status changes to healthy and the specified warm-up time passes, the
    -- instance is considered updated and is added to the percentage complete.
    percentageComplete :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceRefreshWarmPoolProgress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instancesToUpdate', 'instanceRefreshWarmPoolProgress_instancesToUpdate' - The number of instances remaining to update.
--
-- 'percentageComplete', 'instanceRefreshWarmPoolProgress_percentageComplete' - The percentage of instances in the warm pool that have been replaced.
-- For each instance replacement, Amazon EC2 Auto Scaling tracks the
-- instance\'s health status and warm-up time. When the instance\'s health
-- status changes to healthy and the specified warm-up time passes, the
-- instance is considered updated and is added to the percentage complete.
newInstanceRefreshWarmPoolProgress ::
  InstanceRefreshWarmPoolProgress
newInstanceRefreshWarmPoolProgress =
  InstanceRefreshWarmPoolProgress'
    { instancesToUpdate =
        Prelude.Nothing,
      percentageComplete = Prelude.Nothing
    }

-- | The number of instances remaining to update.
instanceRefreshWarmPoolProgress_instancesToUpdate :: Lens.Lens' InstanceRefreshWarmPoolProgress (Prelude.Maybe Prelude.Natural)
instanceRefreshWarmPoolProgress_instancesToUpdate = Lens.lens (\InstanceRefreshWarmPoolProgress' {instancesToUpdate} -> instancesToUpdate) (\s@InstanceRefreshWarmPoolProgress' {} a -> s {instancesToUpdate = a} :: InstanceRefreshWarmPoolProgress)

-- | The percentage of instances in the warm pool that have been replaced.
-- For each instance replacement, Amazon EC2 Auto Scaling tracks the
-- instance\'s health status and warm-up time. When the instance\'s health
-- status changes to healthy and the specified warm-up time passes, the
-- instance is considered updated and is added to the percentage complete.
instanceRefreshWarmPoolProgress_percentageComplete :: Lens.Lens' InstanceRefreshWarmPoolProgress (Prelude.Maybe Prelude.Natural)
instanceRefreshWarmPoolProgress_percentageComplete = Lens.lens (\InstanceRefreshWarmPoolProgress' {percentageComplete} -> percentageComplete) (\s@InstanceRefreshWarmPoolProgress' {} a -> s {percentageComplete = a} :: InstanceRefreshWarmPoolProgress)

instance Data.FromXML InstanceRefreshWarmPoolProgress where
  parseXML x =
    InstanceRefreshWarmPoolProgress'
      Prelude.<$> (x Data..@? "InstancesToUpdate")
      Prelude.<*> (x Data..@? "PercentageComplete")

instance
  Prelude.Hashable
    InstanceRefreshWarmPoolProgress
  where
  hashWithSalt
    _salt
    InstanceRefreshWarmPoolProgress' {..} =
      _salt
        `Prelude.hashWithSalt` instancesToUpdate
        `Prelude.hashWithSalt` percentageComplete

instance
  Prelude.NFData
    InstanceRefreshWarmPoolProgress
  where
  rnf InstanceRefreshWarmPoolProgress' {..} =
    Prelude.rnf instancesToUpdate `Prelude.seq`
      Prelude.rnf percentageComplete
