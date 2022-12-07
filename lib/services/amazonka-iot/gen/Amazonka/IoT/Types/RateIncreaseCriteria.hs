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
-- Module      : Amazonka.IoT.Types.RateIncreaseCriteria
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.RateIncreaseCriteria where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Allows you to define a criteria to initiate the increase in rate of
-- rollout for a job.
--
-- /See:/ 'newRateIncreaseCriteria' smart constructor.
data RateIncreaseCriteria = RateIncreaseCriteria'
  { -- | The threshold for number of succeeded things that will initiate the
    -- increase in rate of rollout.
    numberOfSucceededThings :: Prelude.Maybe Prelude.Natural,
    -- | The threshold for number of notified things that will initiate the
    -- increase in rate of rollout.
    numberOfNotifiedThings :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RateIncreaseCriteria' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'numberOfSucceededThings', 'rateIncreaseCriteria_numberOfSucceededThings' - The threshold for number of succeeded things that will initiate the
-- increase in rate of rollout.
--
-- 'numberOfNotifiedThings', 'rateIncreaseCriteria_numberOfNotifiedThings' - The threshold for number of notified things that will initiate the
-- increase in rate of rollout.
newRateIncreaseCriteria ::
  RateIncreaseCriteria
newRateIncreaseCriteria =
  RateIncreaseCriteria'
    { numberOfSucceededThings =
        Prelude.Nothing,
      numberOfNotifiedThings = Prelude.Nothing
    }

-- | The threshold for number of succeeded things that will initiate the
-- increase in rate of rollout.
rateIncreaseCriteria_numberOfSucceededThings :: Lens.Lens' RateIncreaseCriteria (Prelude.Maybe Prelude.Natural)
rateIncreaseCriteria_numberOfSucceededThings = Lens.lens (\RateIncreaseCriteria' {numberOfSucceededThings} -> numberOfSucceededThings) (\s@RateIncreaseCriteria' {} a -> s {numberOfSucceededThings = a} :: RateIncreaseCriteria)

-- | The threshold for number of notified things that will initiate the
-- increase in rate of rollout.
rateIncreaseCriteria_numberOfNotifiedThings :: Lens.Lens' RateIncreaseCriteria (Prelude.Maybe Prelude.Natural)
rateIncreaseCriteria_numberOfNotifiedThings = Lens.lens (\RateIncreaseCriteria' {numberOfNotifiedThings} -> numberOfNotifiedThings) (\s@RateIncreaseCriteria' {} a -> s {numberOfNotifiedThings = a} :: RateIncreaseCriteria)

instance Data.FromJSON RateIncreaseCriteria where
  parseJSON =
    Data.withObject
      "RateIncreaseCriteria"
      ( \x ->
          RateIncreaseCriteria'
            Prelude.<$> (x Data..:? "numberOfSucceededThings")
            Prelude.<*> (x Data..:? "numberOfNotifiedThings")
      )

instance Prelude.Hashable RateIncreaseCriteria where
  hashWithSalt _salt RateIncreaseCriteria' {..} =
    _salt
      `Prelude.hashWithSalt` numberOfSucceededThings
      `Prelude.hashWithSalt` numberOfNotifiedThings

instance Prelude.NFData RateIncreaseCriteria where
  rnf RateIncreaseCriteria' {..} =
    Prelude.rnf numberOfSucceededThings
      `Prelude.seq` Prelude.rnf numberOfNotifiedThings

instance Data.ToJSON RateIncreaseCriteria where
  toJSON RateIncreaseCriteria' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("numberOfSucceededThings" Data..=)
              Prelude.<$> numberOfSucceededThings,
            ("numberOfNotifiedThings" Data..=)
              Prelude.<$> numberOfNotifiedThings
          ]
      )
