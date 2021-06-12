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
-- Module      : Network.AWS.Shield.Types.AttackSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.AttackSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Shield.Types.AttackVectorDescription

-- | Summarizes all DDoS attacks for a specified time period.
--
-- /See:/ 'newAttackSummary' smart constructor.
data AttackSummary = AttackSummary'
  { -- | The ARN (Amazon Resource Name) of the resource that was attacked.
    resourceArn :: Core.Maybe Core.Text,
    -- | The start time of the attack, in Unix time in seconds. For more
    -- information see
    -- <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp>.
    startTime :: Core.Maybe Core.POSIX,
    -- | The end time of the attack, in Unix time in seconds. For more
    -- information see
    -- <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp>.
    endTime :: Core.Maybe Core.POSIX,
    -- | The unique identifier (ID) of the attack.
    attackId :: Core.Maybe Core.Text,
    -- | The list of attacks for a specified time period.
    attackVectors :: Core.Maybe [AttackVectorDescription]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AttackSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'attackSummary_resourceArn' - The ARN (Amazon Resource Name) of the resource that was attacked.
--
-- 'startTime', 'attackSummary_startTime' - The start time of the attack, in Unix time in seconds. For more
-- information see
-- <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp>.
--
-- 'endTime', 'attackSummary_endTime' - The end time of the attack, in Unix time in seconds. For more
-- information see
-- <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp>.
--
-- 'attackId', 'attackSummary_attackId' - The unique identifier (ID) of the attack.
--
-- 'attackVectors', 'attackSummary_attackVectors' - The list of attacks for a specified time period.
newAttackSummary ::
  AttackSummary
newAttackSummary =
  AttackSummary'
    { resourceArn = Core.Nothing,
      startTime = Core.Nothing,
      endTime = Core.Nothing,
      attackId = Core.Nothing,
      attackVectors = Core.Nothing
    }

-- | The ARN (Amazon Resource Name) of the resource that was attacked.
attackSummary_resourceArn :: Lens.Lens' AttackSummary (Core.Maybe Core.Text)
attackSummary_resourceArn = Lens.lens (\AttackSummary' {resourceArn} -> resourceArn) (\s@AttackSummary' {} a -> s {resourceArn = a} :: AttackSummary)

-- | The start time of the attack, in Unix time in seconds. For more
-- information see
-- <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp>.
attackSummary_startTime :: Lens.Lens' AttackSummary (Core.Maybe Core.UTCTime)
attackSummary_startTime = Lens.lens (\AttackSummary' {startTime} -> startTime) (\s@AttackSummary' {} a -> s {startTime = a} :: AttackSummary) Core.. Lens.mapping Core._Time

-- | The end time of the attack, in Unix time in seconds. For more
-- information see
-- <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp>.
attackSummary_endTime :: Lens.Lens' AttackSummary (Core.Maybe Core.UTCTime)
attackSummary_endTime = Lens.lens (\AttackSummary' {endTime} -> endTime) (\s@AttackSummary' {} a -> s {endTime = a} :: AttackSummary) Core.. Lens.mapping Core._Time

-- | The unique identifier (ID) of the attack.
attackSummary_attackId :: Lens.Lens' AttackSummary (Core.Maybe Core.Text)
attackSummary_attackId = Lens.lens (\AttackSummary' {attackId} -> attackId) (\s@AttackSummary' {} a -> s {attackId = a} :: AttackSummary)

-- | The list of attacks for a specified time period.
attackSummary_attackVectors :: Lens.Lens' AttackSummary (Core.Maybe [AttackVectorDescription])
attackSummary_attackVectors = Lens.lens (\AttackSummary' {attackVectors} -> attackVectors) (\s@AttackSummary' {} a -> s {attackVectors = a} :: AttackSummary) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON AttackSummary where
  parseJSON =
    Core.withObject
      "AttackSummary"
      ( \x ->
          AttackSummary'
            Core.<$> (x Core..:? "ResourceArn")
            Core.<*> (x Core..:? "StartTime")
            Core.<*> (x Core..:? "EndTime")
            Core.<*> (x Core..:? "AttackId")
            Core.<*> (x Core..:? "AttackVectors" Core..!= Core.mempty)
      )

instance Core.Hashable AttackSummary

instance Core.NFData AttackSummary
