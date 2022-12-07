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
-- Module      : Amazonka.IoT.Types.AwsJobRateIncreaseCriteria
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.AwsJobRateIncreaseCriteria where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The criteria to initiate the increase in rate of rollout for a job.
--
-- /See:/ 'newAwsJobRateIncreaseCriteria' smart constructor.
data AwsJobRateIncreaseCriteria = AwsJobRateIncreaseCriteria'
  { -- | When this number of things have succeeded in their job execution, it
    -- will initiate an increase in the rollout rate.
    numberOfSucceededThings :: Prelude.Maybe Prelude.Natural,
    -- | When this number of things have been notified, it will initiate an
    -- increase in the rollout rate.
    numberOfNotifiedThings :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsJobRateIncreaseCriteria' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'numberOfSucceededThings', 'awsJobRateIncreaseCriteria_numberOfSucceededThings' - When this number of things have succeeded in their job execution, it
-- will initiate an increase in the rollout rate.
--
-- 'numberOfNotifiedThings', 'awsJobRateIncreaseCriteria_numberOfNotifiedThings' - When this number of things have been notified, it will initiate an
-- increase in the rollout rate.
newAwsJobRateIncreaseCriteria ::
  AwsJobRateIncreaseCriteria
newAwsJobRateIncreaseCriteria =
  AwsJobRateIncreaseCriteria'
    { numberOfSucceededThings =
        Prelude.Nothing,
      numberOfNotifiedThings = Prelude.Nothing
    }

-- | When this number of things have succeeded in their job execution, it
-- will initiate an increase in the rollout rate.
awsJobRateIncreaseCriteria_numberOfSucceededThings :: Lens.Lens' AwsJobRateIncreaseCriteria (Prelude.Maybe Prelude.Natural)
awsJobRateIncreaseCriteria_numberOfSucceededThings = Lens.lens (\AwsJobRateIncreaseCriteria' {numberOfSucceededThings} -> numberOfSucceededThings) (\s@AwsJobRateIncreaseCriteria' {} a -> s {numberOfSucceededThings = a} :: AwsJobRateIncreaseCriteria)

-- | When this number of things have been notified, it will initiate an
-- increase in the rollout rate.
awsJobRateIncreaseCriteria_numberOfNotifiedThings :: Lens.Lens' AwsJobRateIncreaseCriteria (Prelude.Maybe Prelude.Natural)
awsJobRateIncreaseCriteria_numberOfNotifiedThings = Lens.lens (\AwsJobRateIncreaseCriteria' {numberOfNotifiedThings} -> numberOfNotifiedThings) (\s@AwsJobRateIncreaseCriteria' {} a -> s {numberOfNotifiedThings = a} :: AwsJobRateIncreaseCriteria)

instance Data.FromJSON AwsJobRateIncreaseCriteria where
  parseJSON =
    Data.withObject
      "AwsJobRateIncreaseCriteria"
      ( \x ->
          AwsJobRateIncreaseCriteria'
            Prelude.<$> (x Data..:? "numberOfSucceededThings")
            Prelude.<*> (x Data..:? "numberOfNotifiedThings")
      )

instance Prelude.Hashable AwsJobRateIncreaseCriteria where
  hashWithSalt _salt AwsJobRateIncreaseCriteria' {..} =
    _salt
      `Prelude.hashWithSalt` numberOfSucceededThings
      `Prelude.hashWithSalt` numberOfNotifiedThings

instance Prelude.NFData AwsJobRateIncreaseCriteria where
  rnf AwsJobRateIncreaseCriteria' {..} =
    Prelude.rnf numberOfSucceededThings
      `Prelude.seq` Prelude.rnf numberOfNotifiedThings

instance Data.ToJSON AwsJobRateIncreaseCriteria where
  toJSON AwsJobRateIncreaseCriteria' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("numberOfSucceededThings" Data..=)
              Prelude.<$> numberOfSucceededThings,
            ("numberOfNotifiedThings" Data..=)
              Prelude.<$> numberOfNotifiedThings
          ]
      )
