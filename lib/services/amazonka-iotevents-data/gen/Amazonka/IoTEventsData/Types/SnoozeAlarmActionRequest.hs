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
-- Module      : Amazonka.IoTEventsData.Types.SnoozeAlarmActionRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEventsData.Types.SnoozeAlarmActionRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information needed to snooze the alarm.
--
-- /See:/ 'newSnoozeAlarmActionRequest' smart constructor.
data SnoozeAlarmActionRequest = SnoozeAlarmActionRequest'
  { -- | The value of the key used as a filter to select only the alarms
    -- associated with the
    -- <https://docs.aws.amazon.com/iotevents/latest/apireference/API_CreateAlarmModel.html#iotevents-CreateAlarmModel-request-key key>.
    keyValue :: Prelude.Maybe Prelude.Text,
    -- | The note that you can leave when you snooze the alarm.
    note :: Prelude.Maybe Prelude.Text,
    -- | The request ID. Each ID must be unique within each batch.
    requestId :: Prelude.Text,
    -- | The name of the alarm model.
    alarmModelName :: Prelude.Text,
    -- | The snooze time in seconds. The alarm automatically changes to the
    -- @NORMAL@ state after this duration.
    snoozeDuration :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SnoozeAlarmActionRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyValue', 'snoozeAlarmActionRequest_keyValue' - The value of the key used as a filter to select only the alarms
-- associated with the
-- <https://docs.aws.amazon.com/iotevents/latest/apireference/API_CreateAlarmModel.html#iotevents-CreateAlarmModel-request-key key>.
--
-- 'note', 'snoozeAlarmActionRequest_note' - The note that you can leave when you snooze the alarm.
--
-- 'requestId', 'snoozeAlarmActionRequest_requestId' - The request ID. Each ID must be unique within each batch.
--
-- 'alarmModelName', 'snoozeAlarmActionRequest_alarmModelName' - The name of the alarm model.
--
-- 'snoozeDuration', 'snoozeAlarmActionRequest_snoozeDuration' - The snooze time in seconds. The alarm automatically changes to the
-- @NORMAL@ state after this duration.
newSnoozeAlarmActionRequest ::
  -- | 'requestId'
  Prelude.Text ->
  -- | 'alarmModelName'
  Prelude.Text ->
  -- | 'snoozeDuration'
  Prelude.Int ->
  SnoozeAlarmActionRequest
newSnoozeAlarmActionRequest
  pRequestId_
  pAlarmModelName_
  pSnoozeDuration_ =
    SnoozeAlarmActionRequest'
      { keyValue =
          Prelude.Nothing,
        note = Prelude.Nothing,
        requestId = pRequestId_,
        alarmModelName = pAlarmModelName_,
        snoozeDuration = pSnoozeDuration_
      }

-- | The value of the key used as a filter to select only the alarms
-- associated with the
-- <https://docs.aws.amazon.com/iotevents/latest/apireference/API_CreateAlarmModel.html#iotevents-CreateAlarmModel-request-key key>.
snoozeAlarmActionRequest_keyValue :: Lens.Lens' SnoozeAlarmActionRequest (Prelude.Maybe Prelude.Text)
snoozeAlarmActionRequest_keyValue = Lens.lens (\SnoozeAlarmActionRequest' {keyValue} -> keyValue) (\s@SnoozeAlarmActionRequest' {} a -> s {keyValue = a} :: SnoozeAlarmActionRequest)

-- | The note that you can leave when you snooze the alarm.
snoozeAlarmActionRequest_note :: Lens.Lens' SnoozeAlarmActionRequest (Prelude.Maybe Prelude.Text)
snoozeAlarmActionRequest_note = Lens.lens (\SnoozeAlarmActionRequest' {note} -> note) (\s@SnoozeAlarmActionRequest' {} a -> s {note = a} :: SnoozeAlarmActionRequest)

-- | The request ID. Each ID must be unique within each batch.
snoozeAlarmActionRequest_requestId :: Lens.Lens' SnoozeAlarmActionRequest Prelude.Text
snoozeAlarmActionRequest_requestId = Lens.lens (\SnoozeAlarmActionRequest' {requestId} -> requestId) (\s@SnoozeAlarmActionRequest' {} a -> s {requestId = a} :: SnoozeAlarmActionRequest)

-- | The name of the alarm model.
snoozeAlarmActionRequest_alarmModelName :: Lens.Lens' SnoozeAlarmActionRequest Prelude.Text
snoozeAlarmActionRequest_alarmModelName = Lens.lens (\SnoozeAlarmActionRequest' {alarmModelName} -> alarmModelName) (\s@SnoozeAlarmActionRequest' {} a -> s {alarmModelName = a} :: SnoozeAlarmActionRequest)

-- | The snooze time in seconds. The alarm automatically changes to the
-- @NORMAL@ state after this duration.
snoozeAlarmActionRequest_snoozeDuration :: Lens.Lens' SnoozeAlarmActionRequest Prelude.Int
snoozeAlarmActionRequest_snoozeDuration = Lens.lens (\SnoozeAlarmActionRequest' {snoozeDuration} -> snoozeDuration) (\s@SnoozeAlarmActionRequest' {} a -> s {snoozeDuration = a} :: SnoozeAlarmActionRequest)

instance Prelude.Hashable SnoozeAlarmActionRequest where
  hashWithSalt _salt SnoozeAlarmActionRequest' {..} =
    _salt `Prelude.hashWithSalt` keyValue
      `Prelude.hashWithSalt` note
      `Prelude.hashWithSalt` requestId
      `Prelude.hashWithSalt` alarmModelName
      `Prelude.hashWithSalt` snoozeDuration

instance Prelude.NFData SnoozeAlarmActionRequest where
  rnf SnoozeAlarmActionRequest' {..} =
    Prelude.rnf keyValue
      `Prelude.seq` Prelude.rnf note
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf alarmModelName
      `Prelude.seq` Prelude.rnf snoozeDuration

instance Data.ToJSON SnoozeAlarmActionRequest where
  toJSON SnoozeAlarmActionRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("keyValue" Data..=) Prelude.<$> keyValue,
            ("note" Data..=) Prelude.<$> note,
            Prelude.Just ("requestId" Data..= requestId),
            Prelude.Just
              ("alarmModelName" Data..= alarmModelName),
            Prelude.Just
              ("snoozeDuration" Data..= snoozeDuration)
          ]
      )
