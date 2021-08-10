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
-- Module      : Network.AWS.IoT.Types.AwsJobExecutionsRolloutConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AwsJobExecutionsRolloutConfig where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types.AwsJobExponentialRolloutRate
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Configuration for the rollout of OTA updates.
--
-- /See:/ 'newAwsJobExecutionsRolloutConfig' smart constructor.
data AwsJobExecutionsRolloutConfig = AwsJobExecutionsRolloutConfig'
  { -- | The rate of increase for a job rollout. This parameter allows you to
    -- define an exponential rate increase for a job rollout.
    exponentialRate :: Prelude.Maybe AwsJobExponentialRolloutRate,
    -- | The maximum number of OTA update job executions started per minute.
    maximumPerMinute :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsJobExecutionsRolloutConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exponentialRate', 'awsJobExecutionsRolloutConfig_exponentialRate' - The rate of increase for a job rollout. This parameter allows you to
-- define an exponential rate increase for a job rollout.
--
-- 'maximumPerMinute', 'awsJobExecutionsRolloutConfig_maximumPerMinute' - The maximum number of OTA update job executions started per minute.
newAwsJobExecutionsRolloutConfig ::
  AwsJobExecutionsRolloutConfig
newAwsJobExecutionsRolloutConfig =
  AwsJobExecutionsRolloutConfig'
    { exponentialRate =
        Prelude.Nothing,
      maximumPerMinute = Prelude.Nothing
    }

-- | The rate of increase for a job rollout. This parameter allows you to
-- define an exponential rate increase for a job rollout.
awsJobExecutionsRolloutConfig_exponentialRate :: Lens.Lens' AwsJobExecutionsRolloutConfig (Prelude.Maybe AwsJobExponentialRolloutRate)
awsJobExecutionsRolloutConfig_exponentialRate = Lens.lens (\AwsJobExecutionsRolloutConfig' {exponentialRate} -> exponentialRate) (\s@AwsJobExecutionsRolloutConfig' {} a -> s {exponentialRate = a} :: AwsJobExecutionsRolloutConfig)

-- | The maximum number of OTA update job executions started per minute.
awsJobExecutionsRolloutConfig_maximumPerMinute :: Lens.Lens' AwsJobExecutionsRolloutConfig (Prelude.Maybe Prelude.Natural)
awsJobExecutionsRolloutConfig_maximumPerMinute = Lens.lens (\AwsJobExecutionsRolloutConfig' {maximumPerMinute} -> maximumPerMinute) (\s@AwsJobExecutionsRolloutConfig' {} a -> s {maximumPerMinute = a} :: AwsJobExecutionsRolloutConfig)

instance Core.FromJSON AwsJobExecutionsRolloutConfig where
  parseJSON =
    Core.withObject
      "AwsJobExecutionsRolloutConfig"
      ( \x ->
          AwsJobExecutionsRolloutConfig'
            Prelude.<$> (x Core..:? "exponentialRate")
            Prelude.<*> (x Core..:? "maximumPerMinute")
      )

instance
  Prelude.Hashable
    AwsJobExecutionsRolloutConfig

instance Prelude.NFData AwsJobExecutionsRolloutConfig

instance Core.ToJSON AwsJobExecutionsRolloutConfig where
  toJSON AwsJobExecutionsRolloutConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("exponentialRate" Core..=)
              Prelude.<$> exponentialRate,
            ("maximumPerMinute" Core..=)
              Prelude.<$> maximumPerMinute
          ]
      )
