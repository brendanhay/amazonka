{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Shield.Types.AttackVectorDescription

-- | Summarizes all DDoS attacks for a specified time period.
--
-- /See:/ 'newAttackSummary' smart constructor.
data AttackSummary = AttackSummary'
  { -- | The ARN (Amazon Resource Name) of the resource that was attacked.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | The start time of the attack, in Unix time in seconds. For more
    -- information see
    -- <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp>.
    startTime :: Prelude.Maybe Prelude.POSIX,
    -- | The end time of the attack, in Unix time in seconds. For more
    -- information see
    -- <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp>.
    endTime :: Prelude.Maybe Prelude.POSIX,
    -- | The unique identifier (ID) of the attack.
    attackId :: Prelude.Maybe Prelude.Text,
    -- | The list of attacks for a specified time period.
    attackVectors :: Prelude.Maybe [AttackVectorDescription]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { resourceArn = Prelude.Nothing,
      startTime = Prelude.Nothing,
      endTime = Prelude.Nothing,
      attackId = Prelude.Nothing,
      attackVectors = Prelude.Nothing
    }

-- | The ARN (Amazon Resource Name) of the resource that was attacked.
attackSummary_resourceArn :: Lens.Lens' AttackSummary (Prelude.Maybe Prelude.Text)
attackSummary_resourceArn = Lens.lens (\AttackSummary' {resourceArn} -> resourceArn) (\s@AttackSummary' {} a -> s {resourceArn = a} :: AttackSummary)

-- | The start time of the attack, in Unix time in seconds. For more
-- information see
-- <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp>.
attackSummary_startTime :: Lens.Lens' AttackSummary (Prelude.Maybe Prelude.UTCTime)
attackSummary_startTime = Lens.lens (\AttackSummary' {startTime} -> startTime) (\s@AttackSummary' {} a -> s {startTime = a} :: AttackSummary) Prelude.. Lens.mapping Prelude._Time

-- | The end time of the attack, in Unix time in seconds. For more
-- information see
-- <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp>.
attackSummary_endTime :: Lens.Lens' AttackSummary (Prelude.Maybe Prelude.UTCTime)
attackSummary_endTime = Lens.lens (\AttackSummary' {endTime} -> endTime) (\s@AttackSummary' {} a -> s {endTime = a} :: AttackSummary) Prelude.. Lens.mapping Prelude._Time

-- | The unique identifier (ID) of the attack.
attackSummary_attackId :: Lens.Lens' AttackSummary (Prelude.Maybe Prelude.Text)
attackSummary_attackId = Lens.lens (\AttackSummary' {attackId} -> attackId) (\s@AttackSummary' {} a -> s {attackId = a} :: AttackSummary)

-- | The list of attacks for a specified time period.
attackSummary_attackVectors :: Lens.Lens' AttackSummary (Prelude.Maybe [AttackVectorDescription])
attackSummary_attackVectors = Lens.lens (\AttackSummary' {attackVectors} -> attackVectors) (\s@AttackSummary' {} a -> s {attackVectors = a} :: AttackSummary) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON AttackSummary where
  parseJSON =
    Prelude.withObject
      "AttackSummary"
      ( \x ->
          AttackSummary'
            Prelude.<$> (x Prelude..:? "ResourceArn")
            Prelude.<*> (x Prelude..:? "StartTime")
            Prelude.<*> (x Prelude..:? "EndTime")
            Prelude.<*> (x Prelude..:? "AttackId")
            Prelude.<*> ( x Prelude..:? "AttackVectors"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable AttackSummary

instance Prelude.NFData AttackSummary
