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
-- Module      : Network.AWS.CloudWatch.Types.AlarmHistoryItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.AlarmHistoryItem where

import Network.AWS.CloudWatch.Types.AlarmType
import Network.AWS.CloudWatch.Types.HistoryItemType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents the history of a specific alarm.
--
-- /See:/ 'newAlarmHistoryItem' smart constructor.
data AlarmHistoryItem = AlarmHistoryItem'
  { -- | A summary of the alarm history, in text format.
    historySummary :: Core.Maybe Core.Text,
    -- | The type of alarm history item.
    historyItemType :: Core.Maybe HistoryItemType,
    -- | The descriptive name for the alarm.
    alarmName :: Core.Maybe Core.Text,
    -- | The time stamp for the alarm history item.
    timestamp :: Core.Maybe Core.ISO8601,
    -- | The type of alarm, either metric alarm or composite alarm.
    alarmType :: Core.Maybe AlarmType,
    -- | Data about the alarm, in JSON format.
    historyData :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AlarmHistoryItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'historySummary', 'alarmHistoryItem_historySummary' - A summary of the alarm history, in text format.
--
-- 'historyItemType', 'alarmHistoryItem_historyItemType' - The type of alarm history item.
--
-- 'alarmName', 'alarmHistoryItem_alarmName' - The descriptive name for the alarm.
--
-- 'timestamp', 'alarmHistoryItem_timestamp' - The time stamp for the alarm history item.
--
-- 'alarmType', 'alarmHistoryItem_alarmType' - The type of alarm, either metric alarm or composite alarm.
--
-- 'historyData', 'alarmHistoryItem_historyData' - Data about the alarm, in JSON format.
newAlarmHistoryItem ::
  AlarmHistoryItem
newAlarmHistoryItem =
  AlarmHistoryItem'
    { historySummary = Core.Nothing,
      historyItemType = Core.Nothing,
      alarmName = Core.Nothing,
      timestamp = Core.Nothing,
      alarmType = Core.Nothing,
      historyData = Core.Nothing
    }

-- | A summary of the alarm history, in text format.
alarmHistoryItem_historySummary :: Lens.Lens' AlarmHistoryItem (Core.Maybe Core.Text)
alarmHistoryItem_historySummary = Lens.lens (\AlarmHistoryItem' {historySummary} -> historySummary) (\s@AlarmHistoryItem' {} a -> s {historySummary = a} :: AlarmHistoryItem)

-- | The type of alarm history item.
alarmHistoryItem_historyItemType :: Lens.Lens' AlarmHistoryItem (Core.Maybe HistoryItemType)
alarmHistoryItem_historyItemType = Lens.lens (\AlarmHistoryItem' {historyItemType} -> historyItemType) (\s@AlarmHistoryItem' {} a -> s {historyItemType = a} :: AlarmHistoryItem)

-- | The descriptive name for the alarm.
alarmHistoryItem_alarmName :: Lens.Lens' AlarmHistoryItem (Core.Maybe Core.Text)
alarmHistoryItem_alarmName = Lens.lens (\AlarmHistoryItem' {alarmName} -> alarmName) (\s@AlarmHistoryItem' {} a -> s {alarmName = a} :: AlarmHistoryItem)

-- | The time stamp for the alarm history item.
alarmHistoryItem_timestamp :: Lens.Lens' AlarmHistoryItem (Core.Maybe Core.UTCTime)
alarmHistoryItem_timestamp = Lens.lens (\AlarmHistoryItem' {timestamp} -> timestamp) (\s@AlarmHistoryItem' {} a -> s {timestamp = a} :: AlarmHistoryItem) Core.. Lens.mapping Core._Time

-- | The type of alarm, either metric alarm or composite alarm.
alarmHistoryItem_alarmType :: Lens.Lens' AlarmHistoryItem (Core.Maybe AlarmType)
alarmHistoryItem_alarmType = Lens.lens (\AlarmHistoryItem' {alarmType} -> alarmType) (\s@AlarmHistoryItem' {} a -> s {alarmType = a} :: AlarmHistoryItem)

-- | Data about the alarm, in JSON format.
alarmHistoryItem_historyData :: Lens.Lens' AlarmHistoryItem (Core.Maybe Core.Text)
alarmHistoryItem_historyData = Lens.lens (\AlarmHistoryItem' {historyData} -> historyData) (\s@AlarmHistoryItem' {} a -> s {historyData = a} :: AlarmHistoryItem)

instance Core.FromXML AlarmHistoryItem where
  parseXML x =
    AlarmHistoryItem'
      Core.<$> (x Core..@? "HistorySummary")
      Core.<*> (x Core..@? "HistoryItemType")
      Core.<*> (x Core..@? "AlarmName")
      Core.<*> (x Core..@? "Timestamp")
      Core.<*> (x Core..@? "AlarmType")
      Core.<*> (x Core..@? "HistoryData")

instance Core.Hashable AlarmHistoryItem

instance Core.NFData AlarmHistoryItem
