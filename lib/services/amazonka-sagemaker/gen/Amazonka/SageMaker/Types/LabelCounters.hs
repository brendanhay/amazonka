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
-- Module      : Amazonka.SageMaker.Types.LabelCounters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.LabelCounters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides a breakdown of the number of objects labeled.
--
-- /See:/ 'newLabelCounters' smart constructor.
data LabelCounters = LabelCounters'
  { -- | The total number of objects that could not be labeled due to an error.
    failedNonRetryableError :: Prelude.Maybe Prelude.Natural,
    -- | The total number of objects labeled by a human worker.
    humanLabeled :: Prelude.Maybe Prelude.Natural,
    -- | The total number of objects labeled by automated data labeling.
    machineLabeled :: Prelude.Maybe Prelude.Natural,
    -- | The total number of objects labeled.
    totalLabeled :: Prelude.Maybe Prelude.Natural,
    -- | The total number of objects not yet labeled.
    unlabeled :: Prelude.Maybe Prelude.Natural
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
-- 'failedNonRetryableError', 'labelCounters_failedNonRetryableError' - The total number of objects that could not be labeled due to an error.
--
-- 'humanLabeled', 'labelCounters_humanLabeled' - The total number of objects labeled by a human worker.
--
-- 'machineLabeled', 'labelCounters_machineLabeled' - The total number of objects labeled by automated data labeling.
--
-- 'totalLabeled', 'labelCounters_totalLabeled' - The total number of objects labeled.
--
-- 'unlabeled', 'labelCounters_unlabeled' - The total number of objects not yet labeled.
newLabelCounters ::
  LabelCounters
newLabelCounters =
  LabelCounters'
    { failedNonRetryableError =
        Prelude.Nothing,
      humanLabeled = Prelude.Nothing,
      machineLabeled = Prelude.Nothing,
      totalLabeled = Prelude.Nothing,
      unlabeled = Prelude.Nothing
    }

-- | The total number of objects that could not be labeled due to an error.
labelCounters_failedNonRetryableError :: Lens.Lens' LabelCounters (Prelude.Maybe Prelude.Natural)
labelCounters_failedNonRetryableError = Lens.lens (\LabelCounters' {failedNonRetryableError} -> failedNonRetryableError) (\s@LabelCounters' {} a -> s {failedNonRetryableError = a} :: LabelCounters)

-- | The total number of objects labeled by a human worker.
labelCounters_humanLabeled :: Lens.Lens' LabelCounters (Prelude.Maybe Prelude.Natural)
labelCounters_humanLabeled = Lens.lens (\LabelCounters' {humanLabeled} -> humanLabeled) (\s@LabelCounters' {} a -> s {humanLabeled = a} :: LabelCounters)

-- | The total number of objects labeled by automated data labeling.
labelCounters_machineLabeled :: Lens.Lens' LabelCounters (Prelude.Maybe Prelude.Natural)
labelCounters_machineLabeled = Lens.lens (\LabelCounters' {machineLabeled} -> machineLabeled) (\s@LabelCounters' {} a -> s {machineLabeled = a} :: LabelCounters)

-- | The total number of objects labeled.
labelCounters_totalLabeled :: Lens.Lens' LabelCounters (Prelude.Maybe Prelude.Natural)
labelCounters_totalLabeled = Lens.lens (\LabelCounters' {totalLabeled} -> totalLabeled) (\s@LabelCounters' {} a -> s {totalLabeled = a} :: LabelCounters)

-- | The total number of objects not yet labeled.
labelCounters_unlabeled :: Lens.Lens' LabelCounters (Prelude.Maybe Prelude.Natural)
labelCounters_unlabeled = Lens.lens (\LabelCounters' {unlabeled} -> unlabeled) (\s@LabelCounters' {} a -> s {unlabeled = a} :: LabelCounters)

instance Data.FromJSON LabelCounters where
  parseJSON =
    Data.withObject
      "LabelCounters"
      ( \x ->
          LabelCounters'
            Prelude.<$> (x Data..:? "FailedNonRetryableError")
            Prelude.<*> (x Data..:? "HumanLabeled")
            Prelude.<*> (x Data..:? "MachineLabeled")
            Prelude.<*> (x Data..:? "TotalLabeled")
            Prelude.<*> (x Data..:? "Unlabeled")
      )

instance Prelude.Hashable LabelCounters where
  hashWithSalt _salt LabelCounters' {..} =
    _salt
      `Prelude.hashWithSalt` failedNonRetryableError
      `Prelude.hashWithSalt` humanLabeled
      `Prelude.hashWithSalt` machineLabeled
      `Prelude.hashWithSalt` totalLabeled
      `Prelude.hashWithSalt` unlabeled

instance Prelude.NFData LabelCounters where
  rnf LabelCounters' {..} =
    Prelude.rnf failedNonRetryableError
      `Prelude.seq` Prelude.rnf humanLabeled
      `Prelude.seq` Prelude.rnf machineLabeled
      `Prelude.seq` Prelude.rnf totalLabeled
      `Prelude.seq` Prelude.rnf unlabeled
