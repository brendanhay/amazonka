{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.AlarmHistoryItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.AlarmHistoryItem
  ( AlarmHistoryItem (..),

    -- * Smart constructor
    mkAlarmHistoryItem,

    -- * Lenses
    ahiAlarmName,
    ahiHistoryItemType,
    ahiHistoryData,
    ahiAlarmType,
    ahiHistorySummary,
    ahiTimestamp,
  )
where

import Network.AWS.CloudWatch.Types.AlarmType
import Network.AWS.CloudWatch.Types.HistoryItemType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the history of a specific alarm.
--
-- /See:/ 'mkAlarmHistoryItem' smart constructor.
data AlarmHistoryItem = AlarmHistoryItem'
  { alarmName ::
      Lude.Maybe Lude.Text,
    historyItemType :: Lude.Maybe HistoryItemType,
    historyData :: Lude.Maybe Lude.Text,
    alarmType :: Lude.Maybe AlarmType,
    historySummary :: Lude.Maybe Lude.Text,
    timestamp :: Lude.Maybe Lude.DateTime
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AlarmHistoryItem' with the minimum fields required to make a request.
--
-- * 'alarmName' - The descriptive name for the alarm.
-- * 'alarmType' - The type of alarm, either metric alarm or composite alarm.
-- * 'historyData' - Data about the alarm, in JSON format.
-- * 'historyItemType' - The type of alarm history item.
-- * 'historySummary' - A summary of the alarm history, in text format.
-- * 'timestamp' - The time stamp for the alarm history item.
mkAlarmHistoryItem ::
  AlarmHistoryItem
mkAlarmHistoryItem =
  AlarmHistoryItem'
    { alarmName = Lude.Nothing,
      historyItemType = Lude.Nothing,
      historyData = Lude.Nothing,
      alarmType = Lude.Nothing,
      historySummary = Lude.Nothing,
      timestamp = Lude.Nothing
    }

-- | The descriptive name for the alarm.
--
-- /Note:/ Consider using 'alarmName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahiAlarmName :: Lens.Lens' AlarmHistoryItem (Lude.Maybe Lude.Text)
ahiAlarmName = Lens.lens (alarmName :: AlarmHistoryItem -> Lude.Maybe Lude.Text) (\s a -> s {alarmName = a} :: AlarmHistoryItem)
{-# DEPRECATED ahiAlarmName "Use generic-lens or generic-optics with 'alarmName' instead." #-}

-- | The type of alarm history item.
--
-- /Note:/ Consider using 'historyItemType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahiHistoryItemType :: Lens.Lens' AlarmHistoryItem (Lude.Maybe HistoryItemType)
ahiHistoryItemType = Lens.lens (historyItemType :: AlarmHistoryItem -> Lude.Maybe HistoryItemType) (\s a -> s {historyItemType = a} :: AlarmHistoryItem)
{-# DEPRECATED ahiHistoryItemType "Use generic-lens or generic-optics with 'historyItemType' instead." #-}

-- | Data about the alarm, in JSON format.
--
-- /Note:/ Consider using 'historyData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahiHistoryData :: Lens.Lens' AlarmHistoryItem (Lude.Maybe Lude.Text)
ahiHistoryData = Lens.lens (historyData :: AlarmHistoryItem -> Lude.Maybe Lude.Text) (\s a -> s {historyData = a} :: AlarmHistoryItem)
{-# DEPRECATED ahiHistoryData "Use generic-lens or generic-optics with 'historyData' instead." #-}

-- | The type of alarm, either metric alarm or composite alarm.
--
-- /Note:/ Consider using 'alarmType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahiAlarmType :: Lens.Lens' AlarmHistoryItem (Lude.Maybe AlarmType)
ahiAlarmType = Lens.lens (alarmType :: AlarmHistoryItem -> Lude.Maybe AlarmType) (\s a -> s {alarmType = a} :: AlarmHistoryItem)
{-# DEPRECATED ahiAlarmType "Use generic-lens or generic-optics with 'alarmType' instead." #-}

-- | A summary of the alarm history, in text format.
--
-- /Note:/ Consider using 'historySummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahiHistorySummary :: Lens.Lens' AlarmHistoryItem (Lude.Maybe Lude.Text)
ahiHistorySummary = Lens.lens (historySummary :: AlarmHistoryItem -> Lude.Maybe Lude.Text) (\s a -> s {historySummary = a} :: AlarmHistoryItem)
{-# DEPRECATED ahiHistorySummary "Use generic-lens or generic-optics with 'historySummary' instead." #-}

-- | The time stamp for the alarm history item.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahiTimestamp :: Lens.Lens' AlarmHistoryItem (Lude.Maybe Lude.DateTime)
ahiTimestamp = Lens.lens (timestamp :: AlarmHistoryItem -> Lude.Maybe Lude.DateTime) (\s a -> s {timestamp = a} :: AlarmHistoryItem)
{-# DEPRECATED ahiTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

instance Lude.FromXML AlarmHistoryItem where
  parseXML x =
    AlarmHistoryItem'
      Lude.<$> (x Lude..@? "AlarmName")
      Lude.<*> (x Lude..@? "HistoryItemType")
      Lude.<*> (x Lude..@? "HistoryData")
      Lude.<*> (x Lude..@? "AlarmType")
      Lude.<*> (x Lude..@? "HistorySummary")
      Lude.<*> (x Lude..@? "Timestamp")
