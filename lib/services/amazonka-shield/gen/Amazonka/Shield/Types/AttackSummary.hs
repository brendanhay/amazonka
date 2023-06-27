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
-- Module      : Amazonka.Shield.Types.AttackSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Shield.Types.AttackSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Shield.Types.AttackVectorDescription

-- | Summarizes all DDoS attacks for a specified time period.
--
-- /See:/ 'newAttackSummary' smart constructor.
data AttackSummary = AttackSummary'
  { -- | The unique identifier (ID) of the attack.
    attackId :: Prelude.Maybe Prelude.Text,
    -- | The list of attacks for a specified time period.
    attackVectors :: Prelude.Maybe [AttackVectorDescription],
    -- | The end time of the attack, in Unix time in seconds.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | The ARN (Amazon Resource Name) of the resource that was attacked.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | The start time of the attack, in Unix time in seconds.
    startTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttackSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attackId', 'attackSummary_attackId' - The unique identifier (ID) of the attack.
--
-- 'attackVectors', 'attackSummary_attackVectors' - The list of attacks for a specified time period.
--
-- 'endTime', 'attackSummary_endTime' - The end time of the attack, in Unix time in seconds.
--
-- 'resourceArn', 'attackSummary_resourceArn' - The ARN (Amazon Resource Name) of the resource that was attacked.
--
-- 'startTime', 'attackSummary_startTime' - The start time of the attack, in Unix time in seconds.
newAttackSummary ::
  AttackSummary
newAttackSummary =
  AttackSummary'
    { attackId = Prelude.Nothing,
      attackVectors = Prelude.Nothing,
      endTime = Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      startTime = Prelude.Nothing
    }

-- | The unique identifier (ID) of the attack.
attackSummary_attackId :: Lens.Lens' AttackSummary (Prelude.Maybe Prelude.Text)
attackSummary_attackId = Lens.lens (\AttackSummary' {attackId} -> attackId) (\s@AttackSummary' {} a -> s {attackId = a} :: AttackSummary)

-- | The list of attacks for a specified time period.
attackSummary_attackVectors :: Lens.Lens' AttackSummary (Prelude.Maybe [AttackVectorDescription])
attackSummary_attackVectors = Lens.lens (\AttackSummary' {attackVectors} -> attackVectors) (\s@AttackSummary' {} a -> s {attackVectors = a} :: AttackSummary) Prelude.. Lens.mapping Lens.coerced

-- | The end time of the attack, in Unix time in seconds.
attackSummary_endTime :: Lens.Lens' AttackSummary (Prelude.Maybe Prelude.UTCTime)
attackSummary_endTime = Lens.lens (\AttackSummary' {endTime} -> endTime) (\s@AttackSummary' {} a -> s {endTime = a} :: AttackSummary) Prelude.. Lens.mapping Data._Time

-- | The ARN (Amazon Resource Name) of the resource that was attacked.
attackSummary_resourceArn :: Lens.Lens' AttackSummary (Prelude.Maybe Prelude.Text)
attackSummary_resourceArn = Lens.lens (\AttackSummary' {resourceArn} -> resourceArn) (\s@AttackSummary' {} a -> s {resourceArn = a} :: AttackSummary)

-- | The start time of the attack, in Unix time in seconds.
attackSummary_startTime :: Lens.Lens' AttackSummary (Prelude.Maybe Prelude.UTCTime)
attackSummary_startTime = Lens.lens (\AttackSummary' {startTime} -> startTime) (\s@AttackSummary' {} a -> s {startTime = a} :: AttackSummary) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON AttackSummary where
  parseJSON =
    Data.withObject
      "AttackSummary"
      ( \x ->
          AttackSummary'
            Prelude.<$> (x Data..:? "AttackId")
            Prelude.<*> (x Data..:? "AttackVectors" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "EndTime")
            Prelude.<*> (x Data..:? "ResourceArn")
            Prelude.<*> (x Data..:? "StartTime")
      )

instance Prelude.Hashable AttackSummary where
  hashWithSalt _salt AttackSummary' {..} =
    _salt
      `Prelude.hashWithSalt` attackId
      `Prelude.hashWithSalt` attackVectors
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData AttackSummary where
  rnf AttackSummary' {..} =
    Prelude.rnf attackId
      `Prelude.seq` Prelude.rnf attackVectors
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf startTime
