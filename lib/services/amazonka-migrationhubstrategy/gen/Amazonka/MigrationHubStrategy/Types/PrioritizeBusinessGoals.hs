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
-- Module      : Amazonka.MigrationHubStrategy.Types.PrioritizeBusinessGoals
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.PrioritizeBusinessGoals where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubStrategy.Types.BusinessGoals
import qualified Amazonka.Prelude as Prelude

-- | Rank of business goals based on priority.
--
-- /See:/ 'newPrioritizeBusinessGoals' smart constructor.
data PrioritizeBusinessGoals = PrioritizeBusinessGoals'
  { -- | Rank of business goals based on priority.
    businessGoals :: Prelude.Maybe BusinessGoals
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PrioritizeBusinessGoals' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'businessGoals', 'prioritizeBusinessGoals_businessGoals' - Rank of business goals based on priority.
newPrioritizeBusinessGoals ::
  PrioritizeBusinessGoals
newPrioritizeBusinessGoals =
  PrioritizeBusinessGoals'
    { businessGoals =
        Prelude.Nothing
    }

-- | Rank of business goals based on priority.
prioritizeBusinessGoals_businessGoals :: Lens.Lens' PrioritizeBusinessGoals (Prelude.Maybe BusinessGoals)
prioritizeBusinessGoals_businessGoals = Lens.lens (\PrioritizeBusinessGoals' {businessGoals} -> businessGoals) (\s@PrioritizeBusinessGoals' {} a -> s {businessGoals = a} :: PrioritizeBusinessGoals)

instance Data.FromJSON PrioritizeBusinessGoals where
  parseJSON =
    Data.withObject
      "PrioritizeBusinessGoals"
      ( \x ->
          PrioritizeBusinessGoals'
            Prelude.<$> (x Data..:? "businessGoals")
      )

instance Prelude.Hashable PrioritizeBusinessGoals where
  hashWithSalt _salt PrioritizeBusinessGoals' {..} =
    _salt `Prelude.hashWithSalt` businessGoals

instance Prelude.NFData PrioritizeBusinessGoals where
  rnf PrioritizeBusinessGoals' {..} =
    Prelude.rnf businessGoals

instance Data.ToJSON PrioritizeBusinessGoals where
  toJSON PrioritizeBusinessGoals' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("businessGoals" Data..=)
              Prelude.<$> businessGoals
          ]
      )
