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
-- Module      : Amazonka.RobOMaker.Types.FailureSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RobOMaker.Types.FailureSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RobOMaker.Types.WorldFailure

-- | Information about worlds that failed.
--
-- /See:/ 'newFailureSummary' smart constructor.
data FailureSummary = FailureSummary'
  { -- | The worlds that failed.
    failures :: Prelude.Maybe [WorldFailure],
    -- | The total number of failures.
    totalFailureCount :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FailureSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failures', 'failureSummary_failures' - The worlds that failed.
--
-- 'totalFailureCount', 'failureSummary_totalFailureCount' - The total number of failures.
newFailureSummary ::
  FailureSummary
newFailureSummary =
  FailureSummary'
    { failures = Prelude.Nothing,
      totalFailureCount = Prelude.Nothing
    }

-- | The worlds that failed.
failureSummary_failures :: Lens.Lens' FailureSummary (Prelude.Maybe [WorldFailure])
failureSummary_failures = Lens.lens (\FailureSummary' {failures} -> failures) (\s@FailureSummary' {} a -> s {failures = a} :: FailureSummary) Prelude.. Lens.mapping Lens.coerced

-- | The total number of failures.
failureSummary_totalFailureCount :: Lens.Lens' FailureSummary (Prelude.Maybe Prelude.Int)
failureSummary_totalFailureCount = Lens.lens (\FailureSummary' {totalFailureCount} -> totalFailureCount) (\s@FailureSummary' {} a -> s {totalFailureCount = a} :: FailureSummary)

instance Data.FromJSON FailureSummary where
  parseJSON =
    Data.withObject
      "FailureSummary"
      ( \x ->
          FailureSummary'
            Prelude.<$> (x Data..:? "failures" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "totalFailureCount")
      )

instance Prelude.Hashable FailureSummary where
  hashWithSalt _salt FailureSummary' {..} =
    _salt `Prelude.hashWithSalt` failures
      `Prelude.hashWithSalt` totalFailureCount

instance Prelude.NFData FailureSummary where
  rnf FailureSummary' {..} =
    Prelude.rnf failures
      `Prelude.seq` Prelude.rnf totalFailureCount
