{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.DeviceFarm.Types.UniqueProblem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.UniqueProblem where

import Network.AWS.DeviceFarm.Types.Problem
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A collection of one or more problems, grouped by their result.
--
-- /See:/ 'newUniqueProblem' smart constructor.
data UniqueProblem = UniqueProblem'
  { -- | A message about the unique problems\' result.
    message :: Prelude.Maybe Prelude.Text,
    -- | Information about the problems.
    problems :: Prelude.Maybe [Problem]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UniqueProblem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'uniqueProblem_message' - A message about the unique problems\' result.
--
-- 'problems', 'uniqueProblem_problems' - Information about the problems.
newUniqueProblem ::
  UniqueProblem
newUniqueProblem =
  UniqueProblem'
    { message = Prelude.Nothing,
      problems = Prelude.Nothing
    }

-- | A message about the unique problems\' result.
uniqueProblem_message :: Lens.Lens' UniqueProblem (Prelude.Maybe Prelude.Text)
uniqueProblem_message = Lens.lens (\UniqueProblem' {message} -> message) (\s@UniqueProblem' {} a -> s {message = a} :: UniqueProblem)

-- | Information about the problems.
uniqueProblem_problems :: Lens.Lens' UniqueProblem (Prelude.Maybe [Problem])
uniqueProblem_problems = Lens.lens (\UniqueProblem' {problems} -> problems) (\s@UniqueProblem' {} a -> s {problems = a} :: UniqueProblem) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON UniqueProblem where
  parseJSON =
    Prelude.withObject
      "UniqueProblem"
      ( \x ->
          UniqueProblem'
            Prelude.<$> (x Prelude..:? "message")
            Prelude.<*> ( x Prelude..:? "problems"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable UniqueProblem

instance Prelude.NFData UniqueProblem
