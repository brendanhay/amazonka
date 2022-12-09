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
-- Module      : Amazonka.DeviceFarm.Types.TrialMinutes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DeviceFarm.Types.TrialMinutes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents information about free trial device minutes for an AWS
-- account.
--
-- /See:/ 'newTrialMinutes' smart constructor.
data TrialMinutes = TrialMinutes'
  { -- | The number of free trial minutes remaining in the account.
    remaining :: Prelude.Maybe Prelude.Double,
    -- | The total number of free trial minutes that the account started with.
    total :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrialMinutes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'remaining', 'trialMinutes_remaining' - The number of free trial minutes remaining in the account.
--
-- 'total', 'trialMinutes_total' - The total number of free trial minutes that the account started with.
newTrialMinutes ::
  TrialMinutes
newTrialMinutes =
  TrialMinutes'
    { remaining = Prelude.Nothing,
      total = Prelude.Nothing
    }

-- | The number of free trial minutes remaining in the account.
trialMinutes_remaining :: Lens.Lens' TrialMinutes (Prelude.Maybe Prelude.Double)
trialMinutes_remaining = Lens.lens (\TrialMinutes' {remaining} -> remaining) (\s@TrialMinutes' {} a -> s {remaining = a} :: TrialMinutes)

-- | The total number of free trial minutes that the account started with.
trialMinutes_total :: Lens.Lens' TrialMinutes (Prelude.Maybe Prelude.Double)
trialMinutes_total = Lens.lens (\TrialMinutes' {total} -> total) (\s@TrialMinutes' {} a -> s {total = a} :: TrialMinutes)

instance Data.FromJSON TrialMinutes where
  parseJSON =
    Data.withObject
      "TrialMinutes"
      ( \x ->
          TrialMinutes'
            Prelude.<$> (x Data..:? "remaining")
            Prelude.<*> (x Data..:? "total")
      )

instance Prelude.Hashable TrialMinutes where
  hashWithSalt _salt TrialMinutes' {..} =
    _salt `Prelude.hashWithSalt` remaining
      `Prelude.hashWithSalt` total

instance Prelude.NFData TrialMinutes where
  rnf TrialMinutes' {..} =
    Prelude.rnf remaining
      `Prelude.seq` Prelude.rnf total
