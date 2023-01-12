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
-- Module      : Amazonka.IoTEventsData.Types.EnableAlarmActionRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEventsData.Types.EnableAlarmActionRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information needed to enable the alarm.
--
-- /See:/ 'newEnableAlarmActionRequest' smart constructor.
data EnableAlarmActionRequest = EnableAlarmActionRequest'
  { -- | The value of the key used as a filter to select only the alarms
    -- associated with the
    -- <https://docs.aws.amazon.com/iotevents/latest/apireference/API_CreateAlarmModel.html#iotevents-CreateAlarmModel-request-key key>.
    keyValue :: Prelude.Maybe Prelude.Text,
    -- | The note that you can leave when you enable the alarm.
    note :: Prelude.Maybe Prelude.Text,
    -- | The request ID. Each ID must be unique within each batch.
    requestId :: Prelude.Text,
    -- | The name of the alarm model.
    alarmModelName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableAlarmActionRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyValue', 'enableAlarmActionRequest_keyValue' - The value of the key used as a filter to select only the alarms
-- associated with the
-- <https://docs.aws.amazon.com/iotevents/latest/apireference/API_CreateAlarmModel.html#iotevents-CreateAlarmModel-request-key key>.
--
-- 'note', 'enableAlarmActionRequest_note' - The note that you can leave when you enable the alarm.
--
-- 'requestId', 'enableAlarmActionRequest_requestId' - The request ID. Each ID must be unique within each batch.
--
-- 'alarmModelName', 'enableAlarmActionRequest_alarmModelName' - The name of the alarm model.
newEnableAlarmActionRequest ::
  -- | 'requestId'
  Prelude.Text ->
  -- | 'alarmModelName'
  Prelude.Text ->
  EnableAlarmActionRequest
newEnableAlarmActionRequest
  pRequestId_
  pAlarmModelName_ =
    EnableAlarmActionRequest'
      { keyValue =
          Prelude.Nothing,
        note = Prelude.Nothing,
        requestId = pRequestId_,
        alarmModelName = pAlarmModelName_
      }

-- | The value of the key used as a filter to select only the alarms
-- associated with the
-- <https://docs.aws.amazon.com/iotevents/latest/apireference/API_CreateAlarmModel.html#iotevents-CreateAlarmModel-request-key key>.
enableAlarmActionRequest_keyValue :: Lens.Lens' EnableAlarmActionRequest (Prelude.Maybe Prelude.Text)
enableAlarmActionRequest_keyValue = Lens.lens (\EnableAlarmActionRequest' {keyValue} -> keyValue) (\s@EnableAlarmActionRequest' {} a -> s {keyValue = a} :: EnableAlarmActionRequest)

-- | The note that you can leave when you enable the alarm.
enableAlarmActionRequest_note :: Lens.Lens' EnableAlarmActionRequest (Prelude.Maybe Prelude.Text)
enableAlarmActionRequest_note = Lens.lens (\EnableAlarmActionRequest' {note} -> note) (\s@EnableAlarmActionRequest' {} a -> s {note = a} :: EnableAlarmActionRequest)

-- | The request ID. Each ID must be unique within each batch.
enableAlarmActionRequest_requestId :: Lens.Lens' EnableAlarmActionRequest Prelude.Text
enableAlarmActionRequest_requestId = Lens.lens (\EnableAlarmActionRequest' {requestId} -> requestId) (\s@EnableAlarmActionRequest' {} a -> s {requestId = a} :: EnableAlarmActionRequest)

-- | The name of the alarm model.
enableAlarmActionRequest_alarmModelName :: Lens.Lens' EnableAlarmActionRequest Prelude.Text
enableAlarmActionRequest_alarmModelName = Lens.lens (\EnableAlarmActionRequest' {alarmModelName} -> alarmModelName) (\s@EnableAlarmActionRequest' {} a -> s {alarmModelName = a} :: EnableAlarmActionRequest)

instance Prelude.Hashable EnableAlarmActionRequest where
  hashWithSalt _salt EnableAlarmActionRequest' {..} =
    _salt `Prelude.hashWithSalt` keyValue
      `Prelude.hashWithSalt` note
      `Prelude.hashWithSalt` requestId
      `Prelude.hashWithSalt` alarmModelName

instance Prelude.NFData EnableAlarmActionRequest where
  rnf EnableAlarmActionRequest' {..} =
    Prelude.rnf keyValue
      `Prelude.seq` Prelude.rnf note
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf alarmModelName

instance Data.ToJSON EnableAlarmActionRequest where
  toJSON EnableAlarmActionRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("keyValue" Data..=) Prelude.<$> keyValue,
            ("note" Data..=) Prelude.<$> note,
            Prelude.Just ("requestId" Data..= requestId),
            Prelude.Just
              ("alarmModelName" Data..= alarmModelName)
          ]
      )
