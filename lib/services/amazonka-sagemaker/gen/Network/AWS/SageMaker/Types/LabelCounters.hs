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
import qualified Network.AWS.Prelude as Prelude

-- | Provides a breakdown of the number of objects labeled.
--
-- /See:/ 'newLabelCounters' smart constructor.
data LabelCounters = LabelCounters'
  { -- | The total number of objects not yet labeled.
    unlabeled :: Prelude.Maybe Prelude.Natural,
    -- | The total number of objects that could not be labeled due to an error.
    failedNonRetryableError :: Prelude.Maybe Prelude.Natural,
    -- | The total number of objects labeled by automated data labeling.
    machineLabeled :: Prelude.Maybe Prelude.Natural,
    -- | The total number of objects labeled by a human worker.
    humanLabeled :: Prelude.Maybe Prelude.Natural,
    -- | The total number of objects labeled.
    totalLabeled :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { unlabeled = Prelude.Nothing,
      failedNonRetryableError = Prelude.Nothing,
      machineLabeled = Prelude.Nothing,
      humanLabeled = Prelude.Nothing,
      totalLabeled = Prelude.Nothing
    }

-- | The total number of objects not yet labeled.
labelCounters_unlabeled :: Lens.Lens' LabelCounters (Prelude.Maybe Prelude.Natural)
labelCounters_unlabeled = Lens.lens (\LabelCounters' {unlabeled} -> unlabeled) (\s@LabelCounters' {} a -> s {unlabeled = a} :: LabelCounters)

-- | The total number of objects that could not be labeled due to an error.
labelCounters_failedNonRetryableError :: Lens.Lens' LabelCounters (Prelude.Maybe Prelude.Natural)
labelCounters_failedNonRetryableError = Lens.lens (\LabelCounters' {failedNonRetryableError} -> failedNonRetryableError) (\s@LabelCounters' {} a -> s {failedNonRetryableError = a} :: LabelCounters)

-- | The total number of objects labeled by automated data labeling.
labelCounters_machineLabeled :: Lens.Lens' LabelCounters (Prelude.Maybe Prelude.Natural)
labelCounters_machineLabeled = Lens.lens (\LabelCounters' {machineLabeled} -> machineLabeled) (\s@LabelCounters' {} a -> s {machineLabeled = a} :: LabelCounters)

-- | The total number of objects labeled by a human worker.
labelCounters_humanLabeled :: Lens.Lens' LabelCounters (Prelude.Maybe Prelude.Natural)
labelCounters_humanLabeled = Lens.lens (\LabelCounters' {humanLabeled} -> humanLabeled) (\s@LabelCounters' {} a -> s {humanLabeled = a} :: LabelCounters)

-- | The total number of objects labeled.
labelCounters_totalLabeled :: Lens.Lens' LabelCounters (Prelude.Maybe Prelude.Natural)
labelCounters_totalLabeled = Lens.lens (\LabelCounters' {totalLabeled} -> totalLabeled) (\s@LabelCounters' {} a -> s {totalLabeled = a} :: LabelCounters)

instance Core.FromJSON LabelCounters where
  parseJSON =
    Core.withObject
      "LabelCounters"
      ( \x ->
          LabelCounters'
            Prelude.<$> (x Core..:? "Unlabeled")
            Prelude.<*> (x Core..:? "FailedNonRetryableError")
            Prelude.<*> (x Core..:? "MachineLabeled")
            Prelude.<*> (x Core..:? "HumanLabeled")
            Prelude.<*> (x Core..:? "TotalLabeled")
      )

instance Prelude.Hashable LabelCounters

instance Prelude.NFData LabelCounters
