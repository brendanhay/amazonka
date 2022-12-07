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
-- Module      : Amazonka.IoTEventsData.Types.SnoozeActionConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEventsData.Types.SnoozeActionConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the configuration information of a snooze action.
--
-- /See:/ 'newSnoozeActionConfiguration' smart constructor.
data SnoozeActionConfiguration = SnoozeActionConfiguration'
  { -- | The note that you can leave when you snooze the alarm.
    note :: Prelude.Maybe Prelude.Text,
    -- | The snooze time in seconds. The alarm automatically changes to the
    -- @NORMAL@ state after this duration.
    snoozeDuration :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SnoozeActionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'note', 'snoozeActionConfiguration_note' - The note that you can leave when you snooze the alarm.
--
-- 'snoozeDuration', 'snoozeActionConfiguration_snoozeDuration' - The snooze time in seconds. The alarm automatically changes to the
-- @NORMAL@ state after this duration.
newSnoozeActionConfiguration ::
  SnoozeActionConfiguration
newSnoozeActionConfiguration =
  SnoozeActionConfiguration'
    { note = Prelude.Nothing,
      snoozeDuration = Prelude.Nothing
    }

-- | The note that you can leave when you snooze the alarm.
snoozeActionConfiguration_note :: Lens.Lens' SnoozeActionConfiguration (Prelude.Maybe Prelude.Text)
snoozeActionConfiguration_note = Lens.lens (\SnoozeActionConfiguration' {note} -> note) (\s@SnoozeActionConfiguration' {} a -> s {note = a} :: SnoozeActionConfiguration)

-- | The snooze time in seconds. The alarm automatically changes to the
-- @NORMAL@ state after this duration.
snoozeActionConfiguration_snoozeDuration :: Lens.Lens' SnoozeActionConfiguration (Prelude.Maybe Prelude.Int)
snoozeActionConfiguration_snoozeDuration = Lens.lens (\SnoozeActionConfiguration' {snoozeDuration} -> snoozeDuration) (\s@SnoozeActionConfiguration' {} a -> s {snoozeDuration = a} :: SnoozeActionConfiguration)

instance Data.FromJSON SnoozeActionConfiguration where
  parseJSON =
    Data.withObject
      "SnoozeActionConfiguration"
      ( \x ->
          SnoozeActionConfiguration'
            Prelude.<$> (x Data..:? "note")
            Prelude.<*> (x Data..:? "snoozeDuration")
      )

instance Prelude.Hashable SnoozeActionConfiguration where
  hashWithSalt _salt SnoozeActionConfiguration' {..} =
    _salt `Prelude.hashWithSalt` note
      `Prelude.hashWithSalt` snoozeDuration

instance Prelude.NFData SnoozeActionConfiguration where
  rnf SnoozeActionConfiguration' {..} =
    Prelude.rnf note
      `Prelude.seq` Prelude.rnf snoozeDuration
