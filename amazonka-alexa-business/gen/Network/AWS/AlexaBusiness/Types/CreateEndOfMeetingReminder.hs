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
-- Module      : Network.AWS.AlexaBusiness.Types.CreateEndOfMeetingReminder
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.CreateEndOfMeetingReminder where

import Network.AWS.AlexaBusiness.Types.EndOfMeetingReminderType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Creates settings for the end of meeting reminder feature that are
-- applied to a room profile. The end of meeting reminder enables Alexa to
-- remind users when a meeting is ending.
--
-- /See:/ 'newCreateEndOfMeetingReminder' smart constructor.
data CreateEndOfMeetingReminder = CreateEndOfMeetingReminder'
  { -- | A range of 3 to 15 minutes that determines when the reminder begins.
    reminderAtMinutes :: Prelude.NonEmpty Prelude.Int,
    -- | The type of sound that users hear during the end of meeting reminder.
    reminderType :: EndOfMeetingReminderType,
    -- | Whether an end of meeting reminder is enabled or not.
    enabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEndOfMeetingReminder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reminderAtMinutes', 'createEndOfMeetingReminder_reminderAtMinutes' - A range of 3 to 15 minutes that determines when the reminder begins.
--
-- 'reminderType', 'createEndOfMeetingReminder_reminderType' - The type of sound that users hear during the end of meeting reminder.
--
-- 'enabled', 'createEndOfMeetingReminder_enabled' - Whether an end of meeting reminder is enabled or not.
newCreateEndOfMeetingReminder ::
  -- | 'reminderAtMinutes'
  Prelude.NonEmpty Prelude.Int ->
  -- | 'reminderType'
  EndOfMeetingReminderType ->
  -- | 'enabled'
  Prelude.Bool ->
  CreateEndOfMeetingReminder
newCreateEndOfMeetingReminder
  pReminderAtMinutes_
  pReminderType_
  pEnabled_ =
    CreateEndOfMeetingReminder'
      { reminderAtMinutes =
          Lens._Coerce Lens.# pReminderAtMinutes_,
        reminderType = pReminderType_,
        enabled = pEnabled_
      }

-- | A range of 3 to 15 minutes that determines when the reminder begins.
createEndOfMeetingReminder_reminderAtMinutes :: Lens.Lens' CreateEndOfMeetingReminder (Prelude.NonEmpty Prelude.Int)
createEndOfMeetingReminder_reminderAtMinutes = Lens.lens (\CreateEndOfMeetingReminder' {reminderAtMinutes} -> reminderAtMinutes) (\s@CreateEndOfMeetingReminder' {} a -> s {reminderAtMinutes = a} :: CreateEndOfMeetingReminder) Prelude.. Lens._Coerce

-- | The type of sound that users hear during the end of meeting reminder.
createEndOfMeetingReminder_reminderType :: Lens.Lens' CreateEndOfMeetingReminder EndOfMeetingReminderType
createEndOfMeetingReminder_reminderType = Lens.lens (\CreateEndOfMeetingReminder' {reminderType} -> reminderType) (\s@CreateEndOfMeetingReminder' {} a -> s {reminderType = a} :: CreateEndOfMeetingReminder)

-- | Whether an end of meeting reminder is enabled or not.
createEndOfMeetingReminder_enabled :: Lens.Lens' CreateEndOfMeetingReminder Prelude.Bool
createEndOfMeetingReminder_enabled = Lens.lens (\CreateEndOfMeetingReminder' {enabled} -> enabled) (\s@CreateEndOfMeetingReminder' {} a -> s {enabled = a} :: CreateEndOfMeetingReminder)

instance Prelude.Hashable CreateEndOfMeetingReminder

instance Prelude.NFData CreateEndOfMeetingReminder

instance Core.ToJSON CreateEndOfMeetingReminder where
  toJSON CreateEndOfMeetingReminder' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ReminderAtMinutes" Core..= reminderAtMinutes),
            Prelude.Just ("ReminderType" Core..= reminderType),
            Prelude.Just ("Enabled" Core..= enabled)
          ]
      )
