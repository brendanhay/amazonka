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
-- Module      : Network.AWS.IoT.Types.ExponentialRolloutRate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ExponentialRolloutRate where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types.RateIncreaseCriteria
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Allows you to create an exponential rate of rollout for a job.
--
-- /See:/ 'newExponentialRolloutRate' smart constructor.
data ExponentialRolloutRate = ExponentialRolloutRate'
  { -- | The minimum number of things that will be notified of a pending job, per
    -- minute at the start of job rollout. This parameter allows you to define
    -- the initial rate of rollout.
    baseRatePerMinute :: Prelude.Natural,
    -- | The exponential factor to increase the rate of rollout for a job.
    --
    -- AWS IoT supports up to one digit after the decimal (for example, 1.5,
    -- but not 1.55).
    incrementFactor :: Prelude.Double,
    -- | The criteria to initiate the increase in rate of rollout for a job.
    rateIncreaseCriteria :: RateIncreaseCriteria
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExponentialRolloutRate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'baseRatePerMinute', 'exponentialRolloutRate_baseRatePerMinute' - The minimum number of things that will be notified of a pending job, per
-- minute at the start of job rollout. This parameter allows you to define
-- the initial rate of rollout.
--
-- 'incrementFactor', 'exponentialRolloutRate_incrementFactor' - The exponential factor to increase the rate of rollout for a job.
--
-- AWS IoT supports up to one digit after the decimal (for example, 1.5,
-- but not 1.55).
--
-- 'rateIncreaseCriteria', 'exponentialRolloutRate_rateIncreaseCriteria' - The criteria to initiate the increase in rate of rollout for a job.
newExponentialRolloutRate ::
  -- | 'baseRatePerMinute'
  Prelude.Natural ->
  -- | 'incrementFactor'
  Prelude.Double ->
  -- | 'rateIncreaseCriteria'
  RateIncreaseCriteria ->
  ExponentialRolloutRate
newExponentialRolloutRate
  pBaseRatePerMinute_
  pIncrementFactor_
  pRateIncreaseCriteria_ =
    ExponentialRolloutRate'
      { baseRatePerMinute =
          pBaseRatePerMinute_,
        incrementFactor = pIncrementFactor_,
        rateIncreaseCriteria = pRateIncreaseCriteria_
      }

-- | The minimum number of things that will be notified of a pending job, per
-- minute at the start of job rollout. This parameter allows you to define
-- the initial rate of rollout.
exponentialRolloutRate_baseRatePerMinute :: Lens.Lens' ExponentialRolloutRate Prelude.Natural
exponentialRolloutRate_baseRatePerMinute = Lens.lens (\ExponentialRolloutRate' {baseRatePerMinute} -> baseRatePerMinute) (\s@ExponentialRolloutRate' {} a -> s {baseRatePerMinute = a} :: ExponentialRolloutRate)

-- | The exponential factor to increase the rate of rollout for a job.
--
-- AWS IoT supports up to one digit after the decimal (for example, 1.5,
-- but not 1.55).
exponentialRolloutRate_incrementFactor :: Lens.Lens' ExponentialRolloutRate Prelude.Double
exponentialRolloutRate_incrementFactor = Lens.lens (\ExponentialRolloutRate' {incrementFactor} -> incrementFactor) (\s@ExponentialRolloutRate' {} a -> s {incrementFactor = a} :: ExponentialRolloutRate)

-- | The criteria to initiate the increase in rate of rollout for a job.
exponentialRolloutRate_rateIncreaseCriteria :: Lens.Lens' ExponentialRolloutRate RateIncreaseCriteria
exponentialRolloutRate_rateIncreaseCriteria = Lens.lens (\ExponentialRolloutRate' {rateIncreaseCriteria} -> rateIncreaseCriteria) (\s@ExponentialRolloutRate' {} a -> s {rateIncreaseCriteria = a} :: ExponentialRolloutRate)

instance Core.FromJSON ExponentialRolloutRate where
  parseJSON =
    Core.withObject
      "ExponentialRolloutRate"
      ( \x ->
          ExponentialRolloutRate'
            Prelude.<$> (x Core..: "baseRatePerMinute")
            Prelude.<*> (x Core..: "incrementFactor")
            Prelude.<*> (x Core..: "rateIncreaseCriteria")
      )

instance Prelude.Hashable ExponentialRolloutRate

instance Prelude.NFData ExponentialRolloutRate

instance Core.ToJSON ExponentialRolloutRate where
  toJSON ExponentialRolloutRate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("baseRatePerMinute" Core..= baseRatePerMinute),
            Prelude.Just
              ("incrementFactor" Core..= incrementFactor),
            Prelude.Just
              ( "rateIncreaseCriteria"
                  Core..= rateIncreaseCriteria
              )
          ]
      )
