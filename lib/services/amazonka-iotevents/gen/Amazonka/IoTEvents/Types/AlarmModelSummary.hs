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
-- Module      : Amazonka.IoTEvents.Types.AlarmModelSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEvents.Types.AlarmModelSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains a summary of an alarm model.
--
-- /See:/ 'newAlarmModelSummary' smart constructor.
data AlarmModelSummary = AlarmModelSummary'
  { -- | The time the alarm model was created, in the Unix epoch format.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the alarm model.
    alarmModelName :: Prelude.Maybe Prelude.Text,
    -- | The description of the alarm model.
    alarmModelDescription :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AlarmModelSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'alarmModelSummary_creationTime' - The time the alarm model was created, in the Unix epoch format.
--
-- 'alarmModelName', 'alarmModelSummary_alarmModelName' - The name of the alarm model.
--
-- 'alarmModelDescription', 'alarmModelSummary_alarmModelDescription' - The description of the alarm model.
newAlarmModelSummary ::
  AlarmModelSummary
newAlarmModelSummary =
  AlarmModelSummary'
    { creationTime = Prelude.Nothing,
      alarmModelName = Prelude.Nothing,
      alarmModelDescription = Prelude.Nothing
    }

-- | The time the alarm model was created, in the Unix epoch format.
alarmModelSummary_creationTime :: Lens.Lens' AlarmModelSummary (Prelude.Maybe Prelude.UTCTime)
alarmModelSummary_creationTime = Lens.lens (\AlarmModelSummary' {creationTime} -> creationTime) (\s@AlarmModelSummary' {} a -> s {creationTime = a} :: AlarmModelSummary) Prelude.. Lens.mapping Core._Time

-- | The name of the alarm model.
alarmModelSummary_alarmModelName :: Lens.Lens' AlarmModelSummary (Prelude.Maybe Prelude.Text)
alarmModelSummary_alarmModelName = Lens.lens (\AlarmModelSummary' {alarmModelName} -> alarmModelName) (\s@AlarmModelSummary' {} a -> s {alarmModelName = a} :: AlarmModelSummary)

-- | The description of the alarm model.
alarmModelSummary_alarmModelDescription :: Lens.Lens' AlarmModelSummary (Prelude.Maybe Prelude.Text)
alarmModelSummary_alarmModelDescription = Lens.lens (\AlarmModelSummary' {alarmModelDescription} -> alarmModelDescription) (\s@AlarmModelSummary' {} a -> s {alarmModelDescription = a} :: AlarmModelSummary)

instance Core.FromJSON AlarmModelSummary where
  parseJSON =
    Core.withObject
      "AlarmModelSummary"
      ( \x ->
          AlarmModelSummary'
            Prelude.<$> (x Core..:? "creationTime")
            Prelude.<*> (x Core..:? "alarmModelName")
            Prelude.<*> (x Core..:? "alarmModelDescription")
      )

instance Prelude.Hashable AlarmModelSummary

instance Prelude.NFData AlarmModelSummary
