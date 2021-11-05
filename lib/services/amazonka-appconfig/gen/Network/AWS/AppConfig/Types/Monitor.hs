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
-- Module      : Amazonka.AppConfig.Types.Monitor
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppConfig.Types.Monitor where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Amazon CloudWatch alarms to monitor during the deployment process.
--
-- /See:/ 'newMonitor' smart constructor.
data Monitor = Monitor'
  { -- | ARN of an IAM role for AppConfig to monitor @AlarmArn@.
    alarmRoleArn :: Prelude.Maybe Prelude.Text,
    -- | ARN of the Amazon CloudWatch alarm.
    alarmArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Monitor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alarmRoleArn', 'monitor_alarmRoleArn' - ARN of an IAM role for AppConfig to monitor @AlarmArn@.
--
-- 'alarmArn', 'monitor_alarmArn' - ARN of the Amazon CloudWatch alarm.
newMonitor ::
  Monitor
newMonitor =
  Monitor'
    { alarmRoleArn = Prelude.Nothing,
      alarmArn = Prelude.Nothing
    }

-- | ARN of an IAM role for AppConfig to monitor @AlarmArn@.
monitor_alarmRoleArn :: Lens.Lens' Monitor (Prelude.Maybe Prelude.Text)
monitor_alarmRoleArn = Lens.lens (\Monitor' {alarmRoleArn} -> alarmRoleArn) (\s@Monitor' {} a -> s {alarmRoleArn = a} :: Monitor)

-- | ARN of the Amazon CloudWatch alarm.
monitor_alarmArn :: Lens.Lens' Monitor (Prelude.Maybe Prelude.Text)
monitor_alarmArn = Lens.lens (\Monitor' {alarmArn} -> alarmArn) (\s@Monitor' {} a -> s {alarmArn = a} :: Monitor)

instance Core.FromJSON Monitor where
  parseJSON =
    Core.withObject
      "Monitor"
      ( \x ->
          Monitor'
            Prelude.<$> (x Core..:? "AlarmRoleArn")
            Prelude.<*> (x Core..:? "AlarmArn")
      )

instance Prelude.Hashable Monitor

instance Prelude.NFData Monitor

instance Core.ToJSON Monitor where
  toJSON Monitor' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AlarmRoleArn" Core..=) Prelude.<$> alarmRoleArn,
            ("AlarmArn" Core..=) Prelude.<$> alarmArn
          ]
      )
