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
-- Module      : Network.AWS.SageMaker.Types.LabelCounters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.LabelCounters where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides a breakdown of the number of objects labeled.
--
-- /See:/ 'newLabelCounters' smart constructor.
data LabelCounters = LabelCounters'
  { -- | The total number of objects not yet labeled.
    unlabeled :: Core.Maybe Core.Natural,
    -- | The total number of objects that could not be labeled due to an error.
    failedNonRetryableError :: Core.Maybe Core.Natural,
    -- | The total number of objects labeled by automated data labeling.
    machineLabeled :: Core.Maybe Core.Natural,
    -- | The total number of objects labeled by a human worker.
    humanLabeled :: Core.Maybe Core.Natural,
    -- | The total number of objects labeled.
    totalLabeled :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LabelCounters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unlabeled', 'labelCounters_unlabeled' - The total number of objects not yet labeled.
--
-- 'failedNonRetryableError', 'labelCounters_failedNonRetryableError' - The total number of objects that could not be labeled due to an error.
--
-- 'machineLabeled', 'labelCounters_machineLabeled' - The total number of objects labeled by automated data labeling.
--
-- 'humanLabeled', 'labelCounters_humanLabeled' - The total number of objects labeled by a human worker.
--
-- 'totalLabeled', 'labelCounters_totalLabeled' - The total number of objects labeled.
newLabelCounters ::
  LabelCounters
newLabelCounters =
  LabelCounters'
    { unlabeled = Core.Nothing,
      failedNonRetryableError = Core.Nothing,
      machineLabeled = Core.Nothing,
      humanLabeled = Core.Nothing,
      totalLabeled = Core.Nothing
    }

-- | The total number of objects not yet labeled.
labelCounters_unlabeled :: Lens.Lens' LabelCounters (Core.Maybe Core.Natural)
labelCounters_unlabeled = Lens.lens (\LabelCounters' {unlabeled} -> unlabeled) (\s@LabelCounters' {} a -> s {unlabeled = a} :: LabelCounters)

-- | The total number of objects that could not be labeled due to an error.
labelCounters_failedNonRetryableError :: Lens.Lens' LabelCounters (Core.Maybe Core.Natural)
labelCounters_failedNonRetryableError = Lens.lens (\LabelCounters' {failedNonRetryableError} -> failedNonRetryableError) (\s@LabelCounters' {} a -> s {failedNonRetryableError = a} :: LabelCounters)

-- | The total number of objects labeled by automated data labeling.
labelCounters_machineLabeled :: Lens.Lens' LabelCounters (Core.Maybe Core.Natural)
labelCounters_machineLabeled = Lens.lens (\LabelCounters' {machineLabeled} -> machineLabeled) (\s@LabelCounters' {} a -> s {machineLabeled = a} :: LabelCounters)

-- | The total number of objects labeled by a human worker.
labelCounters_humanLabeled :: Lens.Lens' LabelCounters (Core.Maybe Core.Natural)
labelCounters_humanLabeled = Lens.lens (\LabelCounters' {humanLabeled} -> humanLabeled) (\s@LabelCounters' {} a -> s {humanLabeled = a} :: LabelCounters)

-- | The total number of objects labeled.
labelCounters_totalLabeled :: Lens.Lens' LabelCounters (Core.Maybe Core.Natural)
labelCounters_totalLabeled = Lens.lens (\LabelCounters' {totalLabeled} -> totalLabeled) (\s@LabelCounters' {} a -> s {totalLabeled = a} :: LabelCounters)

instance Core.FromJSON LabelCounters where
  parseJSON =
    Core.withObject
      "LabelCounters"
      ( \x ->
          LabelCounters'
            Core.<$> (x Core..:? "Unlabeled")
            Core.<*> (x Core..:? "FailedNonRetryableError")
            Core.<*> (x Core..:? "MachineLabeled")
            Core.<*> (x Core..:? "HumanLabeled")
            Core.<*> (x Core..:? "TotalLabeled")
      )

instance Core.Hashable LabelCounters

instance Core.NFData LabelCounters
