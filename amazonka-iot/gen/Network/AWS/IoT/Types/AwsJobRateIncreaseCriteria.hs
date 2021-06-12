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
-- Module      : Network.AWS.IoT.Types.AwsJobRateIncreaseCriteria
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AwsJobRateIncreaseCriteria where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The criteria to initiate the increase in rate of rollout for a job.
--
-- /See:/ 'newAwsJobRateIncreaseCriteria' smart constructor.
data AwsJobRateIncreaseCriteria = AwsJobRateIncreaseCriteria'
  { -- | When this number of things have been notified, it will initiate an
    -- increase in the rollout rate.
    numberOfNotifiedThings :: Core.Maybe Core.Natural,
    -- | When this number of things have succeeded in their job execution, it
    -- will initiate an increase in the rollout rate.
    numberOfSucceededThings :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AwsJobRateIncreaseCriteria' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'numberOfNotifiedThings', 'awsJobRateIncreaseCriteria_numberOfNotifiedThings' - When this number of things have been notified, it will initiate an
-- increase in the rollout rate.
--
-- 'numberOfSucceededThings', 'awsJobRateIncreaseCriteria_numberOfSucceededThings' - When this number of things have succeeded in their job execution, it
-- will initiate an increase in the rollout rate.
newAwsJobRateIncreaseCriteria ::
  AwsJobRateIncreaseCriteria
newAwsJobRateIncreaseCriteria =
  AwsJobRateIncreaseCriteria'
    { numberOfNotifiedThings =
        Core.Nothing,
      numberOfSucceededThings = Core.Nothing
    }

-- | When this number of things have been notified, it will initiate an
-- increase in the rollout rate.
awsJobRateIncreaseCriteria_numberOfNotifiedThings :: Lens.Lens' AwsJobRateIncreaseCriteria (Core.Maybe Core.Natural)
awsJobRateIncreaseCriteria_numberOfNotifiedThings = Lens.lens (\AwsJobRateIncreaseCriteria' {numberOfNotifiedThings} -> numberOfNotifiedThings) (\s@AwsJobRateIncreaseCriteria' {} a -> s {numberOfNotifiedThings = a} :: AwsJobRateIncreaseCriteria)

-- | When this number of things have succeeded in their job execution, it
-- will initiate an increase in the rollout rate.
awsJobRateIncreaseCriteria_numberOfSucceededThings :: Lens.Lens' AwsJobRateIncreaseCriteria (Core.Maybe Core.Natural)
awsJobRateIncreaseCriteria_numberOfSucceededThings = Lens.lens (\AwsJobRateIncreaseCriteria' {numberOfSucceededThings} -> numberOfSucceededThings) (\s@AwsJobRateIncreaseCriteria' {} a -> s {numberOfSucceededThings = a} :: AwsJobRateIncreaseCriteria)

instance Core.FromJSON AwsJobRateIncreaseCriteria where
  parseJSON =
    Core.withObject
      "AwsJobRateIncreaseCriteria"
      ( \x ->
          AwsJobRateIncreaseCriteria'
            Core.<$> (x Core..:? "numberOfNotifiedThings")
            Core.<*> (x Core..:? "numberOfSucceededThings")
      )

instance Core.Hashable AwsJobRateIncreaseCriteria

instance Core.NFData AwsJobRateIncreaseCriteria

instance Core.ToJSON AwsJobRateIncreaseCriteria where
  toJSON AwsJobRateIncreaseCriteria' {..} =
    Core.object
      ( Core.catMaybes
          [ ("numberOfNotifiedThings" Core..=)
              Core.<$> numberOfNotifiedThings,
            ("numberOfSucceededThings" Core..=)
              Core.<$> numberOfSucceededThings
          ]
      )
