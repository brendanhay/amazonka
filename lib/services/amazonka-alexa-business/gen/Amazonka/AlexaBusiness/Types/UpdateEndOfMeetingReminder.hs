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
-- Module      : Amazonka.AlexaBusiness.Types.UpdateEndOfMeetingReminder
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.UpdateEndOfMeetingReminder where

import Amazonka.AlexaBusiness.Types.EndOfMeetingReminderType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Settings for the end of meeting reminder feature that are applied to a
-- room profile. The end of meeting reminder enables Alexa to remind users
-- when a meeting is ending.
--
-- /See:/ 'newUpdateEndOfMeetingReminder' smart constructor.
data UpdateEndOfMeetingReminder = UpdateEndOfMeetingReminder'
  { -- | Whether an end of meeting reminder is enabled or not.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | Updates settings for the end of meeting reminder feature that are
    -- applied to a room profile. The end of meeting reminder enables Alexa to
    -- remind users when a meeting is ending.
    reminderAtMinutes :: Prelude.Maybe (Prelude.NonEmpty Prelude.Int),
    -- | The type of sound that users hear during the end of meeting reminder.
    reminderType :: Prelude.Maybe EndOfMeetingReminderType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEndOfMeetingReminder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'updateEndOfMeetingReminder_enabled' - Whether an end of meeting reminder is enabled or not.
--
-- 'reminderAtMinutes', 'updateEndOfMeetingReminder_reminderAtMinutes' - Updates settings for the end of meeting reminder feature that are
-- applied to a room profile. The end of meeting reminder enables Alexa to
-- remind users when a meeting is ending.
--
-- 'reminderType', 'updateEndOfMeetingReminder_reminderType' - The type of sound that users hear during the end of meeting reminder.
newUpdateEndOfMeetingReminder ::
  UpdateEndOfMeetingReminder
newUpdateEndOfMeetingReminder =
  UpdateEndOfMeetingReminder'
    { enabled =
        Prelude.Nothing,
      reminderAtMinutes = Prelude.Nothing,
      reminderType = Prelude.Nothing
    }

-- | Whether an end of meeting reminder is enabled or not.
updateEndOfMeetingReminder_enabled :: Lens.Lens' UpdateEndOfMeetingReminder (Prelude.Maybe Prelude.Bool)
updateEndOfMeetingReminder_enabled = Lens.lens (\UpdateEndOfMeetingReminder' {enabled} -> enabled) (\s@UpdateEndOfMeetingReminder' {} a -> s {enabled = a} :: UpdateEndOfMeetingReminder)

-- | Updates settings for the end of meeting reminder feature that are
-- applied to a room profile. The end of meeting reminder enables Alexa to
-- remind users when a meeting is ending.
updateEndOfMeetingReminder_reminderAtMinutes :: Lens.Lens' UpdateEndOfMeetingReminder (Prelude.Maybe (Prelude.NonEmpty Prelude.Int))
updateEndOfMeetingReminder_reminderAtMinutes = Lens.lens (\UpdateEndOfMeetingReminder' {reminderAtMinutes} -> reminderAtMinutes) (\s@UpdateEndOfMeetingReminder' {} a -> s {reminderAtMinutes = a} :: UpdateEndOfMeetingReminder) Prelude.. Lens.mapping Lens.coerced

-- | The type of sound that users hear during the end of meeting reminder.
updateEndOfMeetingReminder_reminderType :: Lens.Lens' UpdateEndOfMeetingReminder (Prelude.Maybe EndOfMeetingReminderType)
updateEndOfMeetingReminder_reminderType = Lens.lens (\UpdateEndOfMeetingReminder' {reminderType} -> reminderType) (\s@UpdateEndOfMeetingReminder' {} a -> s {reminderType = a} :: UpdateEndOfMeetingReminder)

instance Prelude.Hashable UpdateEndOfMeetingReminder where
  hashWithSalt _salt UpdateEndOfMeetingReminder' {..} =
    _salt `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` reminderAtMinutes
      `Prelude.hashWithSalt` reminderType

instance Prelude.NFData UpdateEndOfMeetingReminder where
  rnf UpdateEndOfMeetingReminder' {..} =
    Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf reminderAtMinutes
      `Prelude.seq` Prelude.rnf reminderType

instance Data.ToJSON UpdateEndOfMeetingReminder where
  toJSON UpdateEndOfMeetingReminder' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Enabled" Data..=) Prelude.<$> enabled,
            ("ReminderAtMinutes" Data..=)
              Prelude.<$> reminderAtMinutes,
            ("ReminderType" Data..=) Prelude.<$> reminderType
          ]
      )
