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
-- Module      : Amazonka.IoT.Types.CloudwatchAlarmAction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.CloudwatchAlarmAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes an action that updates a CloudWatch alarm.
--
-- /See:/ 'newCloudwatchAlarmAction' smart constructor.
data CloudwatchAlarmAction = CloudwatchAlarmAction'
  { -- | The IAM role that allows access to the CloudWatch alarm.
    roleArn :: Prelude.Text,
    -- | The CloudWatch alarm name.
    alarmName :: Prelude.Text,
    -- | The reason for the alarm change.
    stateReason :: Prelude.Text,
    -- | The value of the alarm state. Acceptable values are: OK, ALARM,
    -- INSUFFICIENT_DATA.
    stateValue :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CloudwatchAlarmAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'cloudwatchAlarmAction_roleArn' - The IAM role that allows access to the CloudWatch alarm.
--
-- 'alarmName', 'cloudwatchAlarmAction_alarmName' - The CloudWatch alarm name.
--
-- 'stateReason', 'cloudwatchAlarmAction_stateReason' - The reason for the alarm change.
--
-- 'stateValue', 'cloudwatchAlarmAction_stateValue' - The value of the alarm state. Acceptable values are: OK, ALARM,
-- INSUFFICIENT_DATA.
newCloudwatchAlarmAction ::
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'alarmName'
  Prelude.Text ->
  -- | 'stateReason'
  Prelude.Text ->
  -- | 'stateValue'
  Prelude.Text ->
  CloudwatchAlarmAction
newCloudwatchAlarmAction
  pRoleArn_
  pAlarmName_
  pStateReason_
  pStateValue_ =
    CloudwatchAlarmAction'
      { roleArn = pRoleArn_,
        alarmName = pAlarmName_,
        stateReason = pStateReason_,
        stateValue = pStateValue_
      }

-- | The IAM role that allows access to the CloudWatch alarm.
cloudwatchAlarmAction_roleArn :: Lens.Lens' CloudwatchAlarmAction Prelude.Text
cloudwatchAlarmAction_roleArn = Lens.lens (\CloudwatchAlarmAction' {roleArn} -> roleArn) (\s@CloudwatchAlarmAction' {} a -> s {roleArn = a} :: CloudwatchAlarmAction)

-- | The CloudWatch alarm name.
cloudwatchAlarmAction_alarmName :: Lens.Lens' CloudwatchAlarmAction Prelude.Text
cloudwatchAlarmAction_alarmName = Lens.lens (\CloudwatchAlarmAction' {alarmName} -> alarmName) (\s@CloudwatchAlarmAction' {} a -> s {alarmName = a} :: CloudwatchAlarmAction)

-- | The reason for the alarm change.
cloudwatchAlarmAction_stateReason :: Lens.Lens' CloudwatchAlarmAction Prelude.Text
cloudwatchAlarmAction_stateReason = Lens.lens (\CloudwatchAlarmAction' {stateReason} -> stateReason) (\s@CloudwatchAlarmAction' {} a -> s {stateReason = a} :: CloudwatchAlarmAction)

-- | The value of the alarm state. Acceptable values are: OK, ALARM,
-- INSUFFICIENT_DATA.
cloudwatchAlarmAction_stateValue :: Lens.Lens' CloudwatchAlarmAction Prelude.Text
cloudwatchAlarmAction_stateValue = Lens.lens (\CloudwatchAlarmAction' {stateValue} -> stateValue) (\s@CloudwatchAlarmAction' {} a -> s {stateValue = a} :: CloudwatchAlarmAction)

instance Core.FromJSON CloudwatchAlarmAction where
  parseJSON =
    Core.withObject
      "CloudwatchAlarmAction"
      ( \x ->
          CloudwatchAlarmAction'
            Prelude.<$> (x Core..: "roleArn")
            Prelude.<*> (x Core..: "alarmName")
            Prelude.<*> (x Core..: "stateReason")
            Prelude.<*> (x Core..: "stateValue")
      )

instance Prelude.Hashable CloudwatchAlarmAction where
  hashWithSalt _salt CloudwatchAlarmAction' {..} =
    _salt `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` alarmName
      `Prelude.hashWithSalt` stateReason
      `Prelude.hashWithSalt` stateValue

instance Prelude.NFData CloudwatchAlarmAction where
  rnf CloudwatchAlarmAction' {..} =
    Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf alarmName
      `Prelude.seq` Prelude.rnf stateReason
      `Prelude.seq` Prelude.rnf stateValue

instance Core.ToJSON CloudwatchAlarmAction where
  toJSON CloudwatchAlarmAction' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("roleArn" Core..= roleArn),
            Prelude.Just ("alarmName" Core..= alarmName),
            Prelude.Just ("stateReason" Core..= stateReason),
            Prelude.Just ("stateValue" Core..= stateValue)
          ]
      )
