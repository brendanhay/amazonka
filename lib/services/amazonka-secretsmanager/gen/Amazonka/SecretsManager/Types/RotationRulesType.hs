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
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A structure that defines the rotation configuration for the secret.
--
-- /See:/ 'newRotationRulesType' smart constructor.
data RotationRulesType = RotationRulesType'
  { -- | A @cron()@ or @rate()@ expression that defines the schedule for rotating
    -- your secret. Secrets Manager rotation schedules use UTC time zone.
    --
    -- Secrets Manager @rate()@ expressions represent the interval in days that
    -- you want to rotate your secret, for example @rate(10 days)@. If you use
    -- a @rate()@ expression, the rotation window opens at midnight, and
    -- Secrets Manager rotates your secret any time that day after midnight.
    -- You can set a @Duration@ to shorten the rotation window.
    --
    -- You can use a @cron()@ expression to create rotation schedules that are
    -- more detailed than a rotation interval. For more information, including
    -- examples, see
    -- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/rotate-secrets_schedule.html Schedule expressions in Secrets Manager rotation>.
    -- If you use a @cron()@ expression, Secrets Manager rotates your secret
    -- any time during that day after the window opens. For example,
    -- @cron(0 8 1 * ? *)@ represents a rotation window that occurs on the
    -- first day of every month beginning at 8:00 AM UTC. Secrets Manager
    -- rotates the secret any time that day after 8:00 AM. You can set a
    -- @Duration@ to shorten the rotation window.
    scheduleExpression :: Prelude.Maybe Prelude.Text,
    -- | The length of the rotation window in hours, for example @3h@ for a three
    -- hour window. Secrets Manager rotates your secret at any time during this
    -- window. The window must not go into the next UTC day. If you don\'t
    -- specify this value, the window automatically ends at the end of the UTC
    -- day. The window begins according to the @ScheduleExpression@. For more
    -- information, including examples, see
    -- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/rotate-secrets_schedule.html Schedule expressions in Secrets Manager rotation>.
    duration :: Prelude.Maybe Prelude.Text,
    -- | The number of days between automatic scheduled rotations of the secret.
    -- You can use this value to check that your secret meets your compliance
    -- guidelines for how often secrets must be rotated.
    --
    -- In @DescribeSecret@ and @ListSecrets@, this value is calculated from the
    -- rotation schedule after every successful rotation. In @RotateSecret@,
    -- you can set the rotation schedule in @RotationRules@ with
    -- @AutomaticallyAfterDays@ or @ScheduleExpression@, but not both.
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
--
-- Secrets Manager @rate()@ expressions represent the interval in days that
-- you want to rotate your secret, for example @rate(10 days)@. If you use
-- a @rate()@ expression, the rotation window opens at midnight, and
-- Secrets Manager rotates your secret any time that day after midnight.
-- You can set a @Duration@ to shorten the rotation window.
--
-- You can use a @cron()@ expression to create rotation schedules that are
-- more detailed than a rotation interval. For more information, including
-- examples, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/rotate-secrets_schedule.html Schedule expressions in Secrets Manager rotation>.
-- If you use a @cron()@ expression, Secrets Manager rotates your secret
-- any time during that day after the window opens. For example,
-- @cron(0 8 1 * ? *)@ represents a rotation window that occurs on the
-- first day of every month beginning at 8:00 AM UTC. Secrets Manager
-- rotates the secret any time that day after 8:00 AM. You can set a
-- @Duration@ to shorten the rotation window.
--
-- 'duration', 'rotationRulesType_duration' - The length of the rotation window in hours, for example @3h@ for a three
-- hour window. Secrets Manager rotates your secret at any time during this
-- window. The window must not go into the next UTC day. If you don\'t
-- specify this value, the window automatically ends at the end of the UTC
-- day. The window begins according to the @ScheduleExpression@. For more
-- information, including examples, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/rotate-secrets_schedule.html Schedule expressions in Secrets Manager rotation>.
--
-- 'automaticallyAfterDays', 'rotationRulesType_automaticallyAfterDays' - The number of days between automatic scheduled rotations of the secret.
-- You can use this value to check that your secret meets your compliance
-- guidelines for how often secrets must be rotated.
--
-- In @DescribeSecret@ and @ListSecrets@, this value is calculated from the
-- rotation schedule after every successful rotation. In @RotateSecret@,
-- you can set the rotation schedule in @RotationRules@ with
-- @AutomaticallyAfterDays@ or @ScheduleExpression@, but not both.
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
--
-- Secrets Manager @rate()@ expressions represent the interval in days that
-- you want to rotate your secret, for example @rate(10 days)@. If you use
-- a @rate()@ expression, the rotation window opens at midnight, and
-- Secrets Manager rotates your secret any time that day after midnight.
-- You can set a @Duration@ to shorten the rotation window.
--
-- You can use a @cron()@ expression to create rotation schedules that are
-- more detailed than a rotation interval. For more information, including
-- examples, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/rotate-secrets_schedule.html Schedule expressions in Secrets Manager rotation>.
-- If you use a @cron()@ expression, Secrets Manager rotates your secret
-- any time during that day after the window opens. For example,
-- @cron(0 8 1 * ? *)@ represents a rotation window that occurs on the
-- first day of every month beginning at 8:00 AM UTC. Secrets Manager
-- rotates the secret any time that day after 8:00 AM. You can set a
-- @Duration@ to shorten the rotation window.
rotationRulesType_scheduleExpression :: Lens.Lens' RotationRulesType (Prelude.Maybe Prelude.Text)
rotationRulesType_scheduleExpression = Lens.lens (\RotationRulesType' {scheduleExpression} -> scheduleExpression) (\s@RotationRulesType' {} a -> s {scheduleExpression = a} :: RotationRulesType)

-- | The length of the rotation window in hours, for example @3h@ for a three
-- hour window. Secrets Manager rotates your secret at any time during this
-- window. The window must not go into the next UTC day. If you don\'t
-- specify this value, the window automatically ends at the end of the UTC
-- day. The window begins according to the @ScheduleExpression@. For more
-- information, including examples, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/rotate-secrets_schedule.html Schedule expressions in Secrets Manager rotation>.
rotationRulesType_duration :: Lens.Lens' RotationRulesType (Prelude.Maybe Prelude.Text)
rotationRulesType_duration = Lens.lens (\RotationRulesType' {duration} -> duration) (\s@RotationRulesType' {} a -> s {duration = a} :: RotationRulesType)

-- | The number of days between automatic scheduled rotations of the secret.
-- You can use this value to check that your secret meets your compliance
-- guidelines for how often secrets must be rotated.
--
-- In @DescribeSecret@ and @ListSecrets@, this value is calculated from the
-- rotation schedule after every successful rotation. In @RotateSecret@,
-- you can set the rotation schedule in @RotationRules@ with
-- @AutomaticallyAfterDays@ or @ScheduleExpression@, but not both.
rotationRulesType_automaticallyAfterDays :: Lens.Lens' RotationRulesType (Prelude.Maybe Prelude.Natural)
rotationRulesType_automaticallyAfterDays = Lens.lens (\RotationRulesType' {automaticallyAfterDays} -> automaticallyAfterDays) (\s@RotationRulesType' {} a -> s {automaticallyAfterDays = a} :: RotationRulesType)

instance Core.FromJSON RotationRulesType where
  parseJSON =
    Core.withObject
      "RotationRulesType"
      ( \x ->
          RotationRulesType'
            Prelude.<$> (x Core..:? "ScheduleExpression")
            Prelude.<*> (x Core..:? "Duration")
            Prelude.<*> (x Core..:? "AutomaticallyAfterDays")
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

instance Core.ToJSON RotationRulesType where
  toJSON RotationRulesType' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ScheduleExpression" Core..=)
              Prelude.<$> scheduleExpression,
            ("Duration" Core..=) Prelude.<$> duration,
            ("AutomaticallyAfterDays" Core..=)
              Prelude.<$> automaticallyAfterDays
          ]
      )
