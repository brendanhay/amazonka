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

-- | Creates settings for the end of meeting reminder feature that are
-- applied to a room profile. The end of meeting reminder enables Alexa to
-- remind users when a meeting is ending.
--
-- /See:/ 'newCreateEndOfMeetingReminder' smart constructor.
data CreateEndOfMeetingReminder = CreateEndOfMeetingReminder'
  { -- | A range of 3 to 15 minutes that determines when the reminder begins.
    reminderAtMinutes :: Core.NonEmpty Core.Int,
    -- | The type of sound that users hear during the end of meeting reminder.
    reminderType :: EndOfMeetingReminderType,
    -- | Whether an end of meeting reminder is enabled or not.
    enabled :: Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.NonEmpty Core.Int ->
  -- | 'reminderType'
  EndOfMeetingReminderType ->
  -- | 'enabled'
  Core.Bool ->
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
createEndOfMeetingReminder_reminderAtMinutes :: Lens.Lens' CreateEndOfMeetingReminder (Core.NonEmpty Core.Int)
createEndOfMeetingReminder_reminderAtMinutes = Lens.lens (\CreateEndOfMeetingReminder' {reminderAtMinutes} -> reminderAtMinutes) (\s@CreateEndOfMeetingReminder' {} a -> s {reminderAtMinutes = a} :: CreateEndOfMeetingReminder) Core.. Lens._Coerce

-- | The type of sound that users hear during the end of meeting reminder.
createEndOfMeetingReminder_reminderType :: Lens.Lens' CreateEndOfMeetingReminder EndOfMeetingReminderType
createEndOfMeetingReminder_reminderType = Lens.lens (\CreateEndOfMeetingReminder' {reminderType} -> reminderType) (\s@CreateEndOfMeetingReminder' {} a -> s {reminderType = a} :: CreateEndOfMeetingReminder)

-- | Whether an end of meeting reminder is enabled or not.
createEndOfMeetingReminder_enabled :: Lens.Lens' CreateEndOfMeetingReminder Core.Bool
createEndOfMeetingReminder_enabled = Lens.lens (\CreateEndOfMeetingReminder' {enabled} -> enabled) (\s@CreateEndOfMeetingReminder' {} a -> s {enabled = a} :: CreateEndOfMeetingReminder)

instance Core.Hashable CreateEndOfMeetingReminder

instance Core.NFData CreateEndOfMeetingReminder

instance Core.ToJSON CreateEndOfMeetingReminder where
  toJSON CreateEndOfMeetingReminder' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("ReminderAtMinutes" Core..= reminderAtMinutes),
            Core.Just ("ReminderType" Core..= reminderType),
            Core.Just ("Enabled" Core..= enabled)
          ]
      )
