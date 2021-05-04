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
-- Module      : Network.AWS.IoT.Types.RateIncreaseCriteria
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.RateIncreaseCriteria where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Allows you to define a criteria to initiate the increase in rate of
-- rollout for a job.
--
-- /See:/ 'newRateIncreaseCriteria' smart constructor.
data RateIncreaseCriteria = RateIncreaseCriteria'
  { -- | The threshold for number of notified things that will initiate the
    -- increase in rate of rollout.
    numberOfNotifiedThings :: Prelude.Maybe Prelude.Natural,
    -- | The threshold for number of succeeded things that will initiate the
    -- increase in rate of rollout.
    numberOfSucceededThings :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RateIncreaseCriteria' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'numberOfNotifiedThings', 'rateIncreaseCriteria_numberOfNotifiedThings' - The threshold for number of notified things that will initiate the
-- increase in rate of rollout.
--
-- 'numberOfSucceededThings', 'rateIncreaseCriteria_numberOfSucceededThings' - The threshold for number of succeeded things that will initiate the
-- increase in rate of rollout.
newRateIncreaseCriteria ::
  RateIncreaseCriteria
newRateIncreaseCriteria =
  RateIncreaseCriteria'
    { numberOfNotifiedThings =
        Prelude.Nothing,
      numberOfSucceededThings = Prelude.Nothing
    }

-- | The threshold for number of notified things that will initiate the
-- increase in rate of rollout.
rateIncreaseCriteria_numberOfNotifiedThings :: Lens.Lens' RateIncreaseCriteria (Prelude.Maybe Prelude.Natural)
rateIncreaseCriteria_numberOfNotifiedThings = Lens.lens (\RateIncreaseCriteria' {numberOfNotifiedThings} -> numberOfNotifiedThings) (\s@RateIncreaseCriteria' {} a -> s {numberOfNotifiedThings = a} :: RateIncreaseCriteria)

-- | The threshold for number of succeeded things that will initiate the
-- increase in rate of rollout.
rateIncreaseCriteria_numberOfSucceededThings :: Lens.Lens' RateIncreaseCriteria (Prelude.Maybe Prelude.Natural)
rateIncreaseCriteria_numberOfSucceededThings = Lens.lens (\RateIncreaseCriteria' {numberOfSucceededThings} -> numberOfSucceededThings) (\s@RateIncreaseCriteria' {} a -> s {numberOfSucceededThings = a} :: RateIncreaseCriteria)

instance Prelude.FromJSON RateIncreaseCriteria where
  parseJSON =
    Prelude.withObject
      "RateIncreaseCriteria"
      ( \x ->
          RateIncreaseCriteria'
            Prelude.<$> (x Prelude..:? "numberOfNotifiedThings")
            Prelude.<*> (x Prelude..:? "numberOfSucceededThings")
      )

instance Prelude.Hashable RateIncreaseCriteria

instance Prelude.NFData RateIncreaseCriteria

instance Prelude.ToJSON RateIncreaseCriteria where
  toJSON RateIncreaseCriteria' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("numberOfNotifiedThings" Prelude..=)
              Prelude.<$> numberOfNotifiedThings,
            ("numberOfSucceededThings" Prelude..=)
              Prelude.<$> numberOfSucceededThings
          ]
      )
