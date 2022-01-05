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
-- Module      : Amazonka.RobOMaker.Types.FinishedWorldsSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RobOMaker.Types.FinishedWorldsSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RobOMaker.Types.FailureSummary

-- | Information about worlds that finished.
--
-- /See:/ 'newFinishedWorldsSummary' smart constructor.
data FinishedWorldsSummary = FinishedWorldsSummary'
  { -- | A list of worlds that succeeded.
    succeededWorlds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Information about worlds that failed.
    failureSummary :: Prelude.Maybe FailureSummary,
    -- | The total number of finished worlds.
    finishedCount :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FinishedWorldsSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'succeededWorlds', 'finishedWorldsSummary_succeededWorlds' - A list of worlds that succeeded.
--
-- 'failureSummary', 'finishedWorldsSummary_failureSummary' - Information about worlds that failed.
--
-- 'finishedCount', 'finishedWorldsSummary_finishedCount' - The total number of finished worlds.
newFinishedWorldsSummary ::
  FinishedWorldsSummary
newFinishedWorldsSummary =
  FinishedWorldsSummary'
    { succeededWorlds =
        Prelude.Nothing,
      failureSummary = Prelude.Nothing,
      finishedCount = Prelude.Nothing
    }

-- | A list of worlds that succeeded.
finishedWorldsSummary_succeededWorlds :: Lens.Lens' FinishedWorldsSummary (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
finishedWorldsSummary_succeededWorlds = Lens.lens (\FinishedWorldsSummary' {succeededWorlds} -> succeededWorlds) (\s@FinishedWorldsSummary' {} a -> s {succeededWorlds = a} :: FinishedWorldsSummary) Prelude.. Lens.mapping Lens.coerced

-- | Information about worlds that failed.
finishedWorldsSummary_failureSummary :: Lens.Lens' FinishedWorldsSummary (Prelude.Maybe FailureSummary)
finishedWorldsSummary_failureSummary = Lens.lens (\FinishedWorldsSummary' {failureSummary} -> failureSummary) (\s@FinishedWorldsSummary' {} a -> s {failureSummary = a} :: FinishedWorldsSummary)

-- | The total number of finished worlds.
finishedWorldsSummary_finishedCount :: Lens.Lens' FinishedWorldsSummary (Prelude.Maybe Prelude.Int)
finishedWorldsSummary_finishedCount = Lens.lens (\FinishedWorldsSummary' {finishedCount} -> finishedCount) (\s@FinishedWorldsSummary' {} a -> s {finishedCount = a} :: FinishedWorldsSummary)

instance Core.FromJSON FinishedWorldsSummary where
  parseJSON =
    Core.withObject
      "FinishedWorldsSummary"
      ( \x ->
          FinishedWorldsSummary'
            Prelude.<$> (x Core..:? "succeededWorlds")
            Prelude.<*> (x Core..:? "failureSummary")
            Prelude.<*> (x Core..:? "finishedCount")
      )

instance Prelude.Hashable FinishedWorldsSummary where
  hashWithSalt _salt FinishedWorldsSummary' {..} =
    _salt `Prelude.hashWithSalt` succeededWorlds
      `Prelude.hashWithSalt` failureSummary
      `Prelude.hashWithSalt` finishedCount

instance Prelude.NFData FinishedWorldsSummary where
  rnf FinishedWorldsSummary' {..} =
    Prelude.rnf succeededWorlds
      `Prelude.seq` Prelude.rnf failureSummary
      `Prelude.seq` Prelude.rnf finishedCount
