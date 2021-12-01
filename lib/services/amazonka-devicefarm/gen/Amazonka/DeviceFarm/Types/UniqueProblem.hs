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
-- Module      : Amazonka.DeviceFarm.Types.UniqueProblem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DeviceFarm.Types.UniqueProblem where

import qualified Amazonka.Core as Core
import Amazonka.DeviceFarm.Types.Problem
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A collection of one or more problems, grouped by their result.
--
-- /See:/ 'newUniqueProblem' smart constructor.
data UniqueProblem = UniqueProblem'
  { -- | Information about the problems.
    problems :: Prelude.Maybe [Problem],
    -- | A message about the unique problems\' result.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UniqueProblem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'problems', 'uniqueProblem_problems' - Information about the problems.
--
-- 'message', 'uniqueProblem_message' - A message about the unique problems\' result.
newUniqueProblem ::
  UniqueProblem
newUniqueProblem =
  UniqueProblem'
    { problems = Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | Information about the problems.
uniqueProblem_problems :: Lens.Lens' UniqueProblem (Prelude.Maybe [Problem])
uniqueProblem_problems = Lens.lens (\UniqueProblem' {problems} -> problems) (\s@UniqueProblem' {} a -> s {problems = a} :: UniqueProblem) Prelude.. Lens.mapping Lens.coerced

-- | A message about the unique problems\' result.
uniqueProblem_message :: Lens.Lens' UniqueProblem (Prelude.Maybe Prelude.Text)
uniqueProblem_message = Lens.lens (\UniqueProblem' {message} -> message) (\s@UniqueProblem' {} a -> s {message = a} :: UniqueProblem)

instance Core.FromJSON UniqueProblem where
  parseJSON =
    Core.withObject
      "UniqueProblem"
      ( \x ->
          UniqueProblem'
            Prelude.<$> (x Core..:? "problems" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "message")
      )

instance Prelude.Hashable UniqueProblem where
  hashWithSalt salt' UniqueProblem' {..} =
    salt' `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` problems

instance Prelude.NFData UniqueProblem where
  rnf UniqueProblem' {..} =
    Prelude.rnf problems
      `Prelude.seq` Prelude.rnf message
