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
-- Module      : Amazonka.SageMaker.Types.LabelCountersForWorkteam
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.LabelCountersForWorkteam where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides counts for human-labeled tasks in the labeling job.
--
-- /See:/ 'newLabelCountersForWorkteam' smart constructor.
data LabelCountersForWorkteam = LabelCountersForWorkteam'
  { -- | The total number of data objects that need to be labeled by a human
    -- worker.
    pendingHuman :: Prelude.Maybe Prelude.Natural,
    -- | The total number of tasks in the labeling job.
    total :: Prelude.Maybe Prelude.Natural,
    -- | The total number of data objects labeled by a human worker.
    humanLabeled :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing,
      total = Prelude.Nothing,
      humanLabeled = Prelude.Nothing
    }

-- | The total number of data objects that need to be labeled by a human
-- worker.
labelCountersForWorkteam_pendingHuman :: Lens.Lens' LabelCountersForWorkteam (Prelude.Maybe Prelude.Natural)
labelCountersForWorkteam_pendingHuman = Lens.lens (\LabelCountersForWorkteam' {pendingHuman} -> pendingHuman) (\s@LabelCountersForWorkteam' {} a -> s {pendingHuman = a} :: LabelCountersForWorkteam)

-- | The total number of tasks in the labeling job.
labelCountersForWorkteam_total :: Lens.Lens' LabelCountersForWorkteam (Prelude.Maybe Prelude.Natural)
labelCountersForWorkteam_total = Lens.lens (\LabelCountersForWorkteam' {total} -> total) (\s@LabelCountersForWorkteam' {} a -> s {total = a} :: LabelCountersForWorkteam)

-- | The total number of data objects labeled by a human worker.
labelCountersForWorkteam_humanLabeled :: Lens.Lens' LabelCountersForWorkteam (Prelude.Maybe Prelude.Natural)
labelCountersForWorkteam_humanLabeled = Lens.lens (\LabelCountersForWorkteam' {humanLabeled} -> humanLabeled) (\s@LabelCountersForWorkteam' {} a -> s {humanLabeled = a} :: LabelCountersForWorkteam)

instance Core.FromJSON LabelCountersForWorkteam where
  parseJSON =
    Core.withObject
      "LabelCountersForWorkteam"
      ( \x ->
          LabelCountersForWorkteam'
            Prelude.<$> (x Core..:? "PendingHuman")
            Prelude.<*> (x Core..:? "Total")
            Prelude.<*> (x Core..:? "HumanLabeled")
      )

instance Prelude.Hashable LabelCountersForWorkteam where
  hashWithSalt _salt LabelCountersForWorkteam' {..} =
    _salt `Prelude.hashWithSalt` pendingHuman
      `Prelude.hashWithSalt` total
      `Prelude.hashWithSalt` humanLabeled

instance Prelude.NFData LabelCountersForWorkteam where
  rnf LabelCountersForWorkteam' {..} =
    Prelude.rnf pendingHuman
      `Prelude.seq` Prelude.rnf total
      `Prelude.seq` Prelude.rnf humanLabeled
