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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppConfig.Types.Monitor where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Amazon CloudWatch alarms to monitor during the deployment process.
--
-- /See:/ 'newMonitor' smart constructor.
data Monitor = Monitor'
  { -- | ARN of an Identity and Access Management (IAM) role for AppConfig to
    -- monitor @AlarmArn@.
    alarmRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Amazon Resource Name (ARN) of the Amazon CloudWatch alarm.
    alarmArn :: Prelude.Text
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
-- 'alarmRoleArn', 'monitor_alarmRoleArn' - ARN of an Identity and Access Management (IAM) role for AppConfig to
-- monitor @AlarmArn@.
--
-- 'alarmArn', 'monitor_alarmArn' - Amazon Resource Name (ARN) of the Amazon CloudWatch alarm.
newMonitor ::
  -- | 'alarmArn'
  Prelude.Text ->
  Monitor
newMonitor pAlarmArn_ =
  Monitor'
    { alarmRoleArn = Prelude.Nothing,
      alarmArn = pAlarmArn_
    }

-- | ARN of an Identity and Access Management (IAM) role for AppConfig to
-- monitor @AlarmArn@.
monitor_alarmRoleArn :: Lens.Lens' Monitor (Prelude.Maybe Prelude.Text)
monitor_alarmRoleArn = Lens.lens (\Monitor' {alarmRoleArn} -> alarmRoleArn) (\s@Monitor' {} a -> s {alarmRoleArn = a} :: Monitor)

-- | Amazon Resource Name (ARN) of the Amazon CloudWatch alarm.
monitor_alarmArn :: Lens.Lens' Monitor Prelude.Text
monitor_alarmArn = Lens.lens (\Monitor' {alarmArn} -> alarmArn) (\s@Monitor' {} a -> s {alarmArn = a} :: Monitor)

instance Data.FromJSON Monitor where
  parseJSON =
    Data.withObject
      "Monitor"
      ( \x ->
          Monitor'
            Prelude.<$> (x Data..:? "AlarmRoleArn")
            Prelude.<*> (x Data..: "AlarmArn")
      )

instance Prelude.Hashable Monitor where
  hashWithSalt _salt Monitor' {..} =
    _salt
      `Prelude.hashWithSalt` alarmRoleArn
      `Prelude.hashWithSalt` alarmArn

instance Prelude.NFData Monitor where
  rnf Monitor' {..} =
    Prelude.rnf alarmRoleArn
      `Prelude.seq` Prelude.rnf alarmArn

instance Data.ToJSON Monitor where
  toJSON Monitor' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AlarmRoleArn" Data..=) Prelude.<$> alarmRoleArn,
            Prelude.Just ("AlarmArn" Data..= alarmArn)
          ]
      )
