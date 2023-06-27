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
-- Module      : Amazonka.GreengrassV2.Types.IoTJobExponentialRolloutRate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GreengrassV2.Types.IoTJobExponentialRolloutRate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GreengrassV2.Types.IoTJobRateIncreaseCriteria
import qualified Amazonka.Prelude as Prelude

-- | Contains information about an exponential rollout rate for a
-- configuration deployment job.
--
-- /See:/ 'newIoTJobExponentialRolloutRate' smart constructor.
data IoTJobExponentialRolloutRate = IoTJobExponentialRolloutRate'
  { -- | The minimum number of devices that receive a pending job notification,
    -- per minute, when the job starts. This parameter defines the initial
    -- rollout rate of the job.
    baseRatePerMinute :: Prelude.Natural,
    -- | The exponential factor to increase the rollout rate for the job.
    --
    -- This parameter supports up to one digit after the decimal (for example,
    -- you can specify @1.5@, but not @1.55@).
    incrementFactor :: Prelude.Double,
    -- | The criteria to increase the rollout rate for the job.
    rateIncreaseCriteria :: IoTJobRateIncreaseCriteria
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IoTJobExponentialRolloutRate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'baseRatePerMinute', 'ioTJobExponentialRolloutRate_baseRatePerMinute' - The minimum number of devices that receive a pending job notification,
-- per minute, when the job starts. This parameter defines the initial
-- rollout rate of the job.
--
-- 'incrementFactor', 'ioTJobExponentialRolloutRate_incrementFactor' - The exponential factor to increase the rollout rate for the job.
--
-- This parameter supports up to one digit after the decimal (for example,
-- you can specify @1.5@, but not @1.55@).
--
-- 'rateIncreaseCriteria', 'ioTJobExponentialRolloutRate_rateIncreaseCriteria' - The criteria to increase the rollout rate for the job.
newIoTJobExponentialRolloutRate ::
  -- | 'baseRatePerMinute'
  Prelude.Natural ->
  -- | 'incrementFactor'
  Prelude.Double ->
  -- | 'rateIncreaseCriteria'
  IoTJobRateIncreaseCriteria ->
  IoTJobExponentialRolloutRate
newIoTJobExponentialRolloutRate
  pBaseRatePerMinute_
  pIncrementFactor_
  pRateIncreaseCriteria_ =
    IoTJobExponentialRolloutRate'
      { baseRatePerMinute =
          pBaseRatePerMinute_,
        incrementFactor = pIncrementFactor_,
        rateIncreaseCriteria = pRateIncreaseCriteria_
      }

-- | The minimum number of devices that receive a pending job notification,
-- per minute, when the job starts. This parameter defines the initial
-- rollout rate of the job.
ioTJobExponentialRolloutRate_baseRatePerMinute :: Lens.Lens' IoTJobExponentialRolloutRate Prelude.Natural
ioTJobExponentialRolloutRate_baseRatePerMinute = Lens.lens (\IoTJobExponentialRolloutRate' {baseRatePerMinute} -> baseRatePerMinute) (\s@IoTJobExponentialRolloutRate' {} a -> s {baseRatePerMinute = a} :: IoTJobExponentialRolloutRate)

-- | The exponential factor to increase the rollout rate for the job.
--
-- This parameter supports up to one digit after the decimal (for example,
-- you can specify @1.5@, but not @1.55@).
ioTJobExponentialRolloutRate_incrementFactor :: Lens.Lens' IoTJobExponentialRolloutRate Prelude.Double
ioTJobExponentialRolloutRate_incrementFactor = Lens.lens (\IoTJobExponentialRolloutRate' {incrementFactor} -> incrementFactor) (\s@IoTJobExponentialRolloutRate' {} a -> s {incrementFactor = a} :: IoTJobExponentialRolloutRate)

-- | The criteria to increase the rollout rate for the job.
ioTJobExponentialRolloutRate_rateIncreaseCriteria :: Lens.Lens' IoTJobExponentialRolloutRate IoTJobRateIncreaseCriteria
ioTJobExponentialRolloutRate_rateIncreaseCriteria = Lens.lens (\IoTJobExponentialRolloutRate' {rateIncreaseCriteria} -> rateIncreaseCriteria) (\s@IoTJobExponentialRolloutRate' {} a -> s {rateIncreaseCriteria = a} :: IoTJobExponentialRolloutRate)

instance Data.FromJSON IoTJobExponentialRolloutRate where
  parseJSON =
    Data.withObject
      "IoTJobExponentialRolloutRate"
      ( \x ->
          IoTJobExponentialRolloutRate'
            Prelude.<$> (x Data..: "baseRatePerMinute")
            Prelude.<*> (x Data..: "incrementFactor")
            Prelude.<*> (x Data..: "rateIncreaseCriteria")
      )

instance
  Prelude.Hashable
    IoTJobExponentialRolloutRate
  where
  hashWithSalt _salt IoTJobExponentialRolloutRate' {..} =
    _salt
      `Prelude.hashWithSalt` baseRatePerMinute
      `Prelude.hashWithSalt` incrementFactor
      `Prelude.hashWithSalt` rateIncreaseCriteria

instance Prelude.NFData IoTJobExponentialRolloutRate where
  rnf IoTJobExponentialRolloutRate' {..} =
    Prelude.rnf baseRatePerMinute
      `Prelude.seq` Prelude.rnf incrementFactor
      `Prelude.seq` Prelude.rnf rateIncreaseCriteria

instance Data.ToJSON IoTJobExponentialRolloutRate where
  toJSON IoTJobExponentialRolloutRate' {..} =
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
