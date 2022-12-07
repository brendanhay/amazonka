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
-- Module      : Amazonka.GreengrassV2.Types.IoTJobRateIncreaseCriteria
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GreengrassV2.Types.IoTJobRateIncreaseCriteria where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about criteria to meet before a job increases its
-- rollout rate. Specify either @numberOfNotifiedThings@ or
-- @numberOfSucceededThings@.
--
-- /See:/ 'newIoTJobRateIncreaseCriteria' smart constructor.
data IoTJobRateIncreaseCriteria = IoTJobRateIncreaseCriteria'
  { -- | The number of devices to successfully run the configuration job before
    -- the rollout rate increases.
    numberOfSucceededThings :: Prelude.Maybe Prelude.Natural,
    -- | The number of devices to receive the job notification before the rollout
    -- rate increases.
    numberOfNotifiedThings :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IoTJobRateIncreaseCriteria' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'numberOfSucceededThings', 'ioTJobRateIncreaseCriteria_numberOfSucceededThings' - The number of devices to successfully run the configuration job before
-- the rollout rate increases.
--
-- 'numberOfNotifiedThings', 'ioTJobRateIncreaseCriteria_numberOfNotifiedThings' - The number of devices to receive the job notification before the rollout
-- rate increases.
newIoTJobRateIncreaseCriteria ::
  IoTJobRateIncreaseCriteria
newIoTJobRateIncreaseCriteria =
  IoTJobRateIncreaseCriteria'
    { numberOfSucceededThings =
        Prelude.Nothing,
      numberOfNotifiedThings = Prelude.Nothing
    }

-- | The number of devices to successfully run the configuration job before
-- the rollout rate increases.
ioTJobRateIncreaseCriteria_numberOfSucceededThings :: Lens.Lens' IoTJobRateIncreaseCriteria (Prelude.Maybe Prelude.Natural)
ioTJobRateIncreaseCriteria_numberOfSucceededThings = Lens.lens (\IoTJobRateIncreaseCriteria' {numberOfSucceededThings} -> numberOfSucceededThings) (\s@IoTJobRateIncreaseCriteria' {} a -> s {numberOfSucceededThings = a} :: IoTJobRateIncreaseCriteria)

-- | The number of devices to receive the job notification before the rollout
-- rate increases.
ioTJobRateIncreaseCriteria_numberOfNotifiedThings :: Lens.Lens' IoTJobRateIncreaseCriteria (Prelude.Maybe Prelude.Natural)
ioTJobRateIncreaseCriteria_numberOfNotifiedThings = Lens.lens (\IoTJobRateIncreaseCriteria' {numberOfNotifiedThings} -> numberOfNotifiedThings) (\s@IoTJobRateIncreaseCriteria' {} a -> s {numberOfNotifiedThings = a} :: IoTJobRateIncreaseCriteria)

instance Data.FromJSON IoTJobRateIncreaseCriteria where
  parseJSON =
    Data.withObject
      "IoTJobRateIncreaseCriteria"
      ( \x ->
          IoTJobRateIncreaseCriteria'
            Prelude.<$> (x Data..:? "numberOfSucceededThings")
            Prelude.<*> (x Data..:? "numberOfNotifiedThings")
      )

instance Prelude.Hashable IoTJobRateIncreaseCriteria where
  hashWithSalt _salt IoTJobRateIncreaseCriteria' {..} =
    _salt
      `Prelude.hashWithSalt` numberOfSucceededThings
      `Prelude.hashWithSalt` numberOfNotifiedThings

instance Prelude.NFData IoTJobRateIncreaseCriteria where
  rnf IoTJobRateIncreaseCriteria' {..} =
    Prelude.rnf numberOfSucceededThings
      `Prelude.seq` Prelude.rnf numberOfNotifiedThings

instance Data.ToJSON IoTJobRateIncreaseCriteria where
  toJSON IoTJobRateIncreaseCriteria' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("numberOfSucceededThings" Data..=)
              Prelude.<$> numberOfSucceededThings,
            ("numberOfNotifiedThings" Data..=)
              Prelude.<$> numberOfNotifiedThings
          ]
      )
