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
-- Module      : Amazonka.SecretsManager.Types.RotationRulesType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecretsManager.Types.RotationRulesType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that defines the rotation configuration for the secret.
--
-- /See:/ 'newRotationRulesType' smart constructor.
data RotationRulesType = RotationRulesType'
  { -- | A @cron()@ or @rate()@ expression that defines the schedule for rotating
    -- your secret. Secrets Manager rotation schedules use UTC time zone.
    -- Secrets Manager rotates your secret any time during a rotation window.
    --
    -- Secrets Manager @rate()@ expressions represent the interval in hours or
    -- days that you want to rotate your secret, for example @rate(12 hours)@
    -- or @rate(10 days)@. You can rotate a secret as often as every four
    -- hours. If you use a @rate()@ expression, the rotation window starts at
    -- midnight. For a rate in hours, the default rotation window closes after
    -- one hour. For a rate in days, the default rotation window closes at the
    -- end of the day. You can set the @Duration@ to change the rotation
    -- window. The rotation window must not extend into the next UTC day or
    -- into the next rotation window.
    --
    -- You can use a @cron()@ expression to create a rotation schedule that is
    -- more detailed than a rotation interval. For more information, including
    -- examples, see
    -- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/rotate-secrets_schedule.html Schedule expressions in Secrets Manager rotation>
    -- in the /Secrets Manager Users Guide/. For a cron expression that
    -- represents a schedule in hours, the default rotation window closes after
    -- one hour. For a cron expression that represents a schedule in days, the
    -- default rotation window closes at the end of the day. You can set the
    -- @Duration@ to change the rotation window. The rotation window must not
    -- extend into the next UTC day or into the next rotation window.
    scheduleExpression :: Prelude.Maybe Prelude.Text,
    -- | The length of the rotation window in hours, for example @3h@ for a three
    -- hour window. Secrets Manager rotates your secret at any time during this
    -- window. The window must not extend into the next rotation window or the
    -- next UTC day. The window starts according to the @ScheduleExpression@.
    -- If you don\'t specify a @Duration@, for a @ScheduleExpression@ in hours,
    -- the window automatically closes after one hour. For a
    -- @ScheduleExpression@ in days, the window automatically closes at the end
    -- of the UTC day. For more information, including examples, see
    -- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/rotate-secrets_schedule.html Schedule expressions in Secrets Manager rotation>
    -- in the /Secrets Manager Users Guide/.
    duration :: Prelude.Maybe Prelude.Text,
    -- | The number of days between automatic scheduled rotations of the secret.
    -- You can use this value to check that your secret meets your compliance
    -- guidelines for how often secrets must be rotated.
    --
    -- In @DescribeSecret@ and @ListSecrets@, this value is calculated from the
    -- rotation schedule after every successful rotation. In @RotateSecret@,
    -- you can set the rotation schedule in @RotationRules@ with
    -- @AutomaticallyAfterDays@ or @ScheduleExpression@, but not both. To set a
    -- rotation schedule in hours, use @ScheduleExpression@.
    automaticallyAfterDays :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RotationRulesType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scheduleExpression', 'rotationRulesType_scheduleExpression' - A @cron()@ or @rate()@ expression that defines the schedule for rotating
-- your secret. Secrets Manager rotation schedules use UTC time zone.
-- Secrets Manager rotates your secret any time during a rotation window.
--
-- Secrets Manager @rate()@ expressions represent the interval in hours or
-- days that you want to rotate your secret, for example @rate(12 hours)@
-- or @rate(10 days)@. You can rotate a secret as often as every four
-- hours. If you use a @rate()@ expression, the rotation window starts at
-- midnight. For a rate in hours, the default rotation window closes after
-- one hour. For a rate in days, the default rotation window closes at the
-- end of the day. You can set the @Duration@ to change the rotation
-- window. The rotation window must not extend into the next UTC day or
-- into the next rotation window.
--
-- You can use a @cron()@ expression to create a rotation schedule that is
-- more detailed than a rotation interval. For more information, including
-- examples, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/rotate-secrets_schedule.html Schedule expressions in Secrets Manager rotation>
-- in the /Secrets Manager Users Guide/. For a cron expression that
-- represents a schedule in hours, the default rotation window closes after
-- one hour. For a cron expression that represents a schedule in days, the
-- default rotation window closes at the end of the day. You can set the
-- @Duration@ to change the rotation window. The rotation window must not
-- extend into the next UTC day or into the next rotation window.
--
-- 'duration', 'rotationRulesType_duration' - The length of the rotation window in hours, for example @3h@ for a three
-- hour window. Secrets Manager rotates your secret at any time during this
-- window. The window must not extend into the next rotation window or the
-- next UTC day. The window starts according to the @ScheduleExpression@.
-- If you don\'t specify a @Duration@, for a @ScheduleExpression@ in hours,
-- the window automatically closes after one hour. For a
-- @ScheduleExpression@ in days, the window automatically closes at the end
-- of the UTC day. For more information, including examples, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/rotate-secrets_schedule.html Schedule expressions in Secrets Manager rotation>
-- in the /Secrets Manager Users Guide/.
--
-- 'automaticallyAfterDays', 'rotationRulesType_automaticallyAfterDays' - The number of days between automatic scheduled rotations of the secret.
-- You can use this value to check that your secret meets your compliance
-- guidelines for how often secrets must be rotated.
--
-- In @DescribeSecret@ and @ListSecrets@, this value is calculated from the
-- rotation schedule after every successful rotation. In @RotateSecret@,
-- you can set the rotation schedule in @RotationRules@ with
-- @AutomaticallyAfterDays@ or @ScheduleExpression@, but not both. To set a
-- rotation schedule in hours, use @ScheduleExpression@.
newRotationRulesType ::
  RotationRulesType
newRotationRulesType =
  RotationRulesType'
    { scheduleExpression =
        Prelude.Nothing,
      duration = Prelude.Nothing,
      automaticallyAfterDays = Prelude.Nothing
    }

-- | A @cron()@ or @rate()@ expression that defines the schedule for rotating
-- your secret. Secrets Manager rotation schedules use UTC time zone.
-- Secrets Manager rotates your secret any time during a rotation window.
--
-- Secrets Manager @rate()@ expressions represent the interval in hours or
-- days that you want to rotate your secret, for example @rate(12 hours)@
-- or @rate(10 days)@. You can rotate a secret as often as every four
-- hours. If you use a @rate()@ expression, the rotation window starts at
-- midnight. For a rate in hours, the default rotation window closes after
-- one hour. For a rate in days, the default rotation window closes at the
-- end of the day. You can set the @Duration@ to change the rotation
-- window. The rotation window must not extend into the next UTC day or
-- into the next rotation window.
--
-- You can use a @cron()@ expression to create a rotation schedule that is
-- more detailed than a rotation interval. For more information, including
-- examples, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/rotate-secrets_schedule.html Schedule expressions in Secrets Manager rotation>
-- in the /Secrets Manager Users Guide/. For a cron expression that
-- represents a schedule in hours, the default rotation window closes after
-- one hour. For a cron expression that represents a schedule in days, the
-- default rotation window closes at the end of the day. You can set the
-- @Duration@ to change the rotation window. The rotation window must not
-- extend into the next UTC day or into the next rotation window.
rotationRulesType_scheduleExpression :: Lens.Lens' RotationRulesType (Prelude.Maybe Prelude.Text)
rotationRulesType_scheduleExpression = Lens.lens (\RotationRulesType' {scheduleExpression} -> scheduleExpression) (\s@RotationRulesType' {} a -> s {scheduleExpression = a} :: RotationRulesType)

-- | The length of the rotation window in hours, for example @3h@ for a three
-- hour window. Secrets Manager rotates your secret at any time during this
-- window. The window must not extend into the next rotation window or the
-- next UTC day. The window starts according to the @ScheduleExpression@.
-- If you don\'t specify a @Duration@, for a @ScheduleExpression@ in hours,
-- the window automatically closes after one hour. For a
-- @ScheduleExpression@ in days, the window automatically closes at the end
-- of the UTC day. For more information, including examples, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/rotate-secrets_schedule.html Schedule expressions in Secrets Manager rotation>
-- in the /Secrets Manager Users Guide/.
rotationRulesType_duration :: Lens.Lens' RotationRulesType (Prelude.Maybe Prelude.Text)
rotationRulesType_duration = Lens.lens (\RotationRulesType' {duration} -> duration) (\s@RotationRulesType' {} a -> s {duration = a} :: RotationRulesType)

-- | The number of days between automatic scheduled rotations of the secret.
-- You can use this value to check that your secret meets your compliance
-- guidelines for how often secrets must be rotated.
--
-- In @DescribeSecret@ and @ListSecrets@, this value is calculated from the
-- rotation schedule after every successful rotation. In @RotateSecret@,
-- you can set the rotation schedule in @RotationRules@ with
-- @AutomaticallyAfterDays@ or @ScheduleExpression@, but not both. To set a
-- rotation schedule in hours, use @ScheduleExpression@.
rotationRulesType_automaticallyAfterDays :: Lens.Lens' RotationRulesType (Prelude.Maybe Prelude.Natural)
rotationRulesType_automaticallyAfterDays = Lens.lens (\RotationRulesType' {automaticallyAfterDays} -> automaticallyAfterDays) (\s@RotationRulesType' {} a -> s {automaticallyAfterDays = a} :: RotationRulesType)

instance Data.FromJSON RotationRulesType where
  parseJSON =
    Data.withObject
      "RotationRulesType"
      ( \x ->
          RotationRulesType'
            Prelude.<$> (x Data..:? "ScheduleExpression")
            Prelude.<*> (x Data..:? "Duration")
            Prelude.<*> (x Data..:? "AutomaticallyAfterDays")
      )

instance Prelude.Hashable RotationRulesType where
  hashWithSalt _salt RotationRulesType' {..} =
    _salt `Prelude.hashWithSalt` scheduleExpression
      `Prelude.hashWithSalt` duration
      `Prelude.hashWithSalt` automaticallyAfterDays

instance Prelude.NFData RotationRulesType where
  rnf RotationRulesType' {..} =
    Prelude.rnf scheduleExpression
      `Prelude.seq` Prelude.rnf duration
      `Prelude.seq` Prelude.rnf automaticallyAfterDays

instance Data.ToJSON RotationRulesType where
  toJSON RotationRulesType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ScheduleExpression" Data..=)
              Prelude.<$> scheduleExpression,
            ("Duration" Data..=) Prelude.<$> duration,
            ("AutomaticallyAfterDays" Data..=)
              Prelude.<$> automaticallyAfterDays
          ]
      )
