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
-- Module      : Amazonka.IoT.Types.AwsJobExponentialRolloutRate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.AwsJobExponentialRolloutRate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.AwsJobRateIncreaseCriteria
import qualified Amazonka.Prelude as Prelude

-- | The rate of increase for a job rollout. This parameter allows you to
-- define an exponential rate increase for a job rollout.
--
-- /See:/ 'newAwsJobExponentialRolloutRate' smart constructor.
data AwsJobExponentialRolloutRate = AwsJobExponentialRolloutRate'
  { -- | The minimum number of things that will be notified of a pending job, per
    -- minute, at the start of the job rollout. This is the initial rate of the
    -- rollout.
    baseRatePerMinute :: Prelude.Natural,
    -- | The rate of increase for a job rollout. The number of things notified is
    -- multiplied by this factor.
    incrementFactor :: Prelude.Double,
    -- | The criteria to initiate the increase in rate of rollout for a job.
    --
    -- Amazon Web Services IoT Core supports up to one digit after the decimal
    -- (for example, 1.5, but not 1.55).
    rateIncreaseCriteria :: AwsJobRateIncreaseCriteria
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsJobExponentialRolloutRate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'baseRatePerMinute', 'awsJobExponentialRolloutRate_baseRatePerMinute' - The minimum number of things that will be notified of a pending job, per
-- minute, at the start of the job rollout. This is the initial rate of the
-- rollout.
--
-- 'incrementFactor', 'awsJobExponentialRolloutRate_incrementFactor' - The rate of increase for a job rollout. The number of things notified is
-- multiplied by this factor.
--
-- 'rateIncreaseCriteria', 'awsJobExponentialRolloutRate_rateIncreaseCriteria' - The criteria to initiate the increase in rate of rollout for a job.
--
-- Amazon Web Services IoT Core supports up to one digit after the decimal
-- (for example, 1.5, but not 1.55).
newAwsJobExponentialRolloutRate ::
  -- | 'baseRatePerMinute'
  Prelude.Natural ->
  -- | 'incrementFactor'
  Prelude.Double ->
  -- | 'rateIncreaseCriteria'
  AwsJobRateIncreaseCriteria ->
  AwsJobExponentialRolloutRate
newAwsJobExponentialRolloutRate
  pBaseRatePerMinute_
  pIncrementFactor_
  pRateIncreaseCriteria_ =
    AwsJobExponentialRolloutRate'
      { baseRatePerMinute =
          pBaseRatePerMinute_,
        incrementFactor = pIncrementFactor_,
        rateIncreaseCriteria = pRateIncreaseCriteria_
      }

-- | The minimum number of things that will be notified of a pending job, per
-- minute, at the start of the job rollout. This is the initial rate of the
-- rollout.
awsJobExponentialRolloutRate_baseRatePerMinute :: Lens.Lens' AwsJobExponentialRolloutRate Prelude.Natural
awsJobExponentialRolloutRate_baseRatePerMinute = Lens.lens (\AwsJobExponentialRolloutRate' {baseRatePerMinute} -> baseRatePerMinute) (\s@AwsJobExponentialRolloutRate' {} a -> s {baseRatePerMinute = a} :: AwsJobExponentialRolloutRate)

-- | The rate of increase for a job rollout. The number of things notified is
-- multiplied by this factor.
awsJobExponentialRolloutRate_incrementFactor :: Lens.Lens' AwsJobExponentialRolloutRate Prelude.Double
awsJobExponentialRolloutRate_incrementFactor = Lens.lens (\AwsJobExponentialRolloutRate' {incrementFactor} -> incrementFactor) (\s@AwsJobExponentialRolloutRate' {} a -> s {incrementFactor = a} :: AwsJobExponentialRolloutRate)

-- | The criteria to initiate the increase in rate of rollout for a job.
--
-- Amazon Web Services IoT Core supports up to one digit after the decimal
-- (for example, 1.5, but not 1.55).
awsJobExponentialRolloutRate_rateIncreaseCriteria :: Lens.Lens' AwsJobExponentialRolloutRate AwsJobRateIncreaseCriteria
awsJobExponentialRolloutRate_rateIncreaseCriteria = Lens.lens (\AwsJobExponentialRolloutRate' {rateIncreaseCriteria} -> rateIncreaseCriteria) (\s@AwsJobExponentialRolloutRate' {} a -> s {rateIncreaseCriteria = a} :: AwsJobExponentialRolloutRate)

instance Data.FromJSON AwsJobExponentialRolloutRate where
  parseJSON =
    Data.withObject
      "AwsJobExponentialRolloutRate"
      ( \x ->
          AwsJobExponentialRolloutRate'
            Prelude.<$> (x Data..: "baseRatePerMinute")
            Prelude.<*> (x Data..: "incrementFactor")
            Prelude.<*> (x Data..: "rateIncreaseCriteria")
      )

instance
  Prelude.Hashable
    AwsJobExponentialRolloutRate
  where
  hashWithSalt _salt AwsJobExponentialRolloutRate' {..} =
    _salt
      `Prelude.hashWithSalt` baseRatePerMinute
      `Prelude.hashWithSalt` incrementFactor
      `Prelude.hashWithSalt` rateIncreaseCriteria

instance Prelude.NFData AwsJobExponentialRolloutRate where
  rnf AwsJobExponentialRolloutRate' {..} =
    Prelude.rnf baseRatePerMinute `Prelude.seq`
      Prelude.rnf incrementFactor `Prelude.seq`
        Prelude.rnf rateIncreaseCriteria

instance Data.ToJSON AwsJobExponentialRolloutRate where
  toJSON AwsJobExponentialRolloutRate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("baseRatePerMinute" Data..= baseRatePerMinute),
            Prelude.Just
              ("incrementFactor" Data..= incrementFactor),
            Prelude.Just
              ( "rateIncreaseCriteria"
                  Data..= rateIncreaseCriteria
              )
          ]
      )
