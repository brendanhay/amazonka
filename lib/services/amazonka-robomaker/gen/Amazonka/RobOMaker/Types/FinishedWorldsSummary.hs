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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RobOMaker.Types.FinishedWorldsSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RobOMaker.Types.FailureSummary

-- | Information about worlds that finished.
--
-- /See:/ 'newFinishedWorldsSummary' smart constructor.
data FinishedWorldsSummary = FinishedWorldsSummary'
  { -- | Information about worlds that failed.
    failureSummary :: Prelude.Maybe FailureSummary,
    -- | A list of worlds that succeeded.
    succeededWorlds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
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
-- 'failureSummary', 'finishedWorldsSummary_failureSummary' - Information about worlds that failed.
--
-- 'succeededWorlds', 'finishedWorldsSummary_succeededWorlds' - A list of worlds that succeeded.
--
-- 'finishedCount', 'finishedWorldsSummary_finishedCount' - The total number of finished worlds.
newFinishedWorldsSummary ::
  FinishedWorldsSummary
newFinishedWorldsSummary =
  FinishedWorldsSummary'
    { failureSummary =
        Prelude.Nothing,
      succeededWorlds = Prelude.Nothing,
      finishedCount = Prelude.Nothing
    }

-- | Information about worlds that failed.
finishedWorldsSummary_failureSummary :: Lens.Lens' FinishedWorldsSummary (Prelude.Maybe FailureSummary)
finishedWorldsSummary_failureSummary = Lens.lens (\FinishedWorldsSummary' {failureSummary} -> failureSummary) (\s@FinishedWorldsSummary' {} a -> s {failureSummary = a} :: FinishedWorldsSummary)

-- | A list of worlds that succeeded.
finishedWorldsSummary_succeededWorlds :: Lens.Lens' FinishedWorldsSummary (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
finishedWorldsSummary_succeededWorlds = Lens.lens (\FinishedWorldsSummary' {succeededWorlds} -> succeededWorlds) (\s@FinishedWorldsSummary' {} a -> s {succeededWorlds = a} :: FinishedWorldsSummary) Prelude.. Lens.mapping Lens.coerced

-- | The total number of finished worlds.
finishedWorldsSummary_finishedCount :: Lens.Lens' FinishedWorldsSummary (Prelude.Maybe Prelude.Int)
finishedWorldsSummary_finishedCount = Lens.lens (\FinishedWorldsSummary' {finishedCount} -> finishedCount) (\s@FinishedWorldsSummary' {} a -> s {finishedCount = a} :: FinishedWorldsSummary)

instance Data.FromJSON FinishedWorldsSummary where
  parseJSON =
    Data.withObject
      "FinishedWorldsSummary"
      ( \x ->
          FinishedWorldsSummary'
            Prelude.<$> (x Data..:? "failureSummary")
            Prelude.<*> (x Data..:? "succeededWorlds")
            Prelude.<*> (x Data..:? "finishedCount")
      )

instance Prelude.Hashable FinishedWorldsSummary where
  hashWithSalt _salt FinishedWorldsSummary' {..} =
    _salt `Prelude.hashWithSalt` failureSummary
      `Prelude.hashWithSalt` succeededWorlds
      `Prelude.hashWithSalt` finishedCount

instance Prelude.NFData FinishedWorldsSummary where
  rnf FinishedWorldsSummary' {..} =
    Prelude.rnf failureSummary
      `Prelude.seq` Prelude.rnf succeededWorlds
      `Prelude.seq` Prelude.rnf finishedCount
