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
-- Module      : Network.AWS.SageMaker.Types.LabelCountersForWorkteam
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.LabelCountersForWorkteam where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides counts for human-labeled tasks in the labeling job.
--
-- /See:/ 'newLabelCountersForWorkteam' smart constructor.
data LabelCountersForWorkteam = LabelCountersForWorkteam'
  { -- | The total number of data objects that need to be labeled by a human
    -- worker.
    pendingHuman :: Core.Maybe Core.Natural,
    -- | The total number of tasks in the labeling job.
    total :: Core.Maybe Core.Natural,
    -- | The total number of data objects labeled by a human worker.
    humanLabeled :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LabelCountersForWorkteam' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pendingHuman', 'labelCountersForWorkteam_pendingHuman' - The total number of data objects that need to be labeled by a human
-- worker.
--
-- 'total', 'labelCountersForWorkteam_total' - The total number of tasks in the labeling job.
--
-- 'humanLabeled', 'labelCountersForWorkteam_humanLabeled' - The total number of data objects labeled by a human worker.
newLabelCountersForWorkteam ::
  LabelCountersForWorkteam
newLabelCountersForWorkteam =
  LabelCountersForWorkteam'
    { pendingHuman =
        Core.Nothing,
      total = Core.Nothing,
      humanLabeled = Core.Nothing
    }

-- | The total number of data objects that need to be labeled by a human
-- worker.
labelCountersForWorkteam_pendingHuman :: Lens.Lens' LabelCountersForWorkteam (Core.Maybe Core.Natural)
labelCountersForWorkteam_pendingHuman = Lens.lens (\LabelCountersForWorkteam' {pendingHuman} -> pendingHuman) (\s@LabelCountersForWorkteam' {} a -> s {pendingHuman = a} :: LabelCountersForWorkteam)

-- | The total number of tasks in the labeling job.
labelCountersForWorkteam_total :: Lens.Lens' LabelCountersForWorkteam (Core.Maybe Core.Natural)
labelCountersForWorkteam_total = Lens.lens (\LabelCountersForWorkteam' {total} -> total) (\s@LabelCountersForWorkteam' {} a -> s {total = a} :: LabelCountersForWorkteam)

-- | The total number of data objects labeled by a human worker.
labelCountersForWorkteam_humanLabeled :: Lens.Lens' LabelCountersForWorkteam (Core.Maybe Core.Natural)
labelCountersForWorkteam_humanLabeled = Lens.lens (\LabelCountersForWorkteam' {humanLabeled} -> humanLabeled) (\s@LabelCountersForWorkteam' {} a -> s {humanLabeled = a} :: LabelCountersForWorkteam)

instance Core.FromJSON LabelCountersForWorkteam where
  parseJSON =
    Core.withObject
      "LabelCountersForWorkteam"
      ( \x ->
          LabelCountersForWorkteam'
            Core.<$> (x Core..:? "PendingHuman")
            Core.<*> (x Core..:? "Total")
            Core.<*> (x Core..:? "HumanLabeled")
      )

instance Core.Hashable LabelCountersForWorkteam

instance Core.NFData LabelCountersForWorkteam
