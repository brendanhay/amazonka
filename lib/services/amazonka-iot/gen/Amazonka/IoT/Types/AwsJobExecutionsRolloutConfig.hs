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
-- Module      : Amazonka.IoT.Types.AwsJobExecutionsRolloutConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.AwsJobExecutionsRolloutConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.AwsJobExponentialRolloutRate
import qualified Amazonka.Prelude as Prelude

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

instance Data.FromJSON AwsJobExecutionsRolloutConfig where
  parseJSON =
    Data.withObject
      "AwsJobExecutionsRolloutConfig"
      ( \x ->
          AwsJobExecutionsRolloutConfig'
            Prelude.<$> (x Data..:? "exponentialRate")
            Prelude.<*> (x Data..:? "maximumPerMinute")
      )

instance
  Prelude.Hashable
    AwsJobExecutionsRolloutConfig
  where
  hashWithSalt _salt AwsJobExecutionsRolloutConfig' {..} =
    _salt `Prelude.hashWithSalt` exponentialRate
      `Prelude.hashWithSalt` maximumPerMinute

instance Prelude.NFData AwsJobExecutionsRolloutConfig where
  rnf AwsJobExecutionsRolloutConfig' {..} =
    Prelude.rnf exponentialRate
      `Prelude.seq` Prelude.rnf maximumPerMinute

instance Data.ToJSON AwsJobExecutionsRolloutConfig where
  toJSON AwsJobExecutionsRolloutConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("exponentialRate" Data..=)
              Prelude.<$> exponentialRate,
            ("maximumPerMinute" Data..=)
              Prelude.<$> maximumPerMinute
          ]
      )
