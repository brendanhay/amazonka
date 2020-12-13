{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ScheduledAuditMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ScheduledAuditMetadata
  ( ScheduledAuditMetadata (..),

    -- * Smart constructor
    mkScheduledAuditMetadata,

    -- * Lenses
    samFrequency,
    samScheduledAuditName,
    samDayOfMonth,
    samDayOfWeek,
    samScheduledAuditARN,
  )
where

import Network.AWS.IoT.Types.AuditFrequency
import Network.AWS.IoT.Types.DayOfWeek
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the scheduled audit.
--
-- /See:/ 'mkScheduledAuditMetadata' smart constructor.
data ScheduledAuditMetadata = ScheduledAuditMetadata'
  { -- | How often the scheduled audit occurs.
    frequency :: Lude.Maybe AuditFrequency,
    -- | The name of the scheduled audit.
    scheduledAuditName :: Lude.Maybe Lude.Text,
    -- | The day of the month on which the scheduled audit is run (if the @frequency@ is "MONTHLY"). If days 29-31 are specified, and the month does not have that many days, the audit takes place on the "LAST" day of the month.
    dayOfMonth :: Lude.Maybe Lude.Text,
    -- | The day of the week on which the scheduled audit is run (if the @frequency@ is "WEEKLY" or "BIWEEKLY").
    dayOfWeek :: Lude.Maybe DayOfWeek,
    -- | The ARN of the scheduled audit.
    scheduledAuditARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ScheduledAuditMetadata' with the minimum fields required to make a request.
--
-- * 'frequency' - How often the scheduled audit occurs.
-- * 'scheduledAuditName' - The name of the scheduled audit.
-- * 'dayOfMonth' - The day of the month on which the scheduled audit is run (if the @frequency@ is "MONTHLY"). If days 29-31 are specified, and the month does not have that many days, the audit takes place on the "LAST" day of the month.
-- * 'dayOfWeek' - The day of the week on which the scheduled audit is run (if the @frequency@ is "WEEKLY" or "BIWEEKLY").
-- * 'scheduledAuditARN' - The ARN of the scheduled audit.
mkScheduledAuditMetadata ::
  ScheduledAuditMetadata
mkScheduledAuditMetadata =
  ScheduledAuditMetadata'
    { frequency = Lude.Nothing,
      scheduledAuditName = Lude.Nothing,
      dayOfMonth = Lude.Nothing,
      dayOfWeek = Lude.Nothing,
      scheduledAuditARN = Lude.Nothing
    }

-- | How often the scheduled audit occurs.
--
-- /Note:/ Consider using 'frequency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samFrequency :: Lens.Lens' ScheduledAuditMetadata (Lude.Maybe AuditFrequency)
samFrequency = Lens.lens (frequency :: ScheduledAuditMetadata -> Lude.Maybe AuditFrequency) (\s a -> s {frequency = a} :: ScheduledAuditMetadata)
{-# DEPRECATED samFrequency "Use generic-lens or generic-optics with 'frequency' instead." #-}

-- | The name of the scheduled audit.
--
-- /Note:/ Consider using 'scheduledAuditName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samScheduledAuditName :: Lens.Lens' ScheduledAuditMetadata (Lude.Maybe Lude.Text)
samScheduledAuditName = Lens.lens (scheduledAuditName :: ScheduledAuditMetadata -> Lude.Maybe Lude.Text) (\s a -> s {scheduledAuditName = a} :: ScheduledAuditMetadata)
{-# DEPRECATED samScheduledAuditName "Use generic-lens or generic-optics with 'scheduledAuditName' instead." #-}

-- | The day of the month on which the scheduled audit is run (if the @frequency@ is "MONTHLY"). If days 29-31 are specified, and the month does not have that many days, the audit takes place on the "LAST" day of the month.
--
-- /Note:/ Consider using 'dayOfMonth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samDayOfMonth :: Lens.Lens' ScheduledAuditMetadata (Lude.Maybe Lude.Text)
samDayOfMonth = Lens.lens (dayOfMonth :: ScheduledAuditMetadata -> Lude.Maybe Lude.Text) (\s a -> s {dayOfMonth = a} :: ScheduledAuditMetadata)
{-# DEPRECATED samDayOfMonth "Use generic-lens or generic-optics with 'dayOfMonth' instead." #-}

-- | The day of the week on which the scheduled audit is run (if the @frequency@ is "WEEKLY" or "BIWEEKLY").
--
-- /Note:/ Consider using 'dayOfWeek' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samDayOfWeek :: Lens.Lens' ScheduledAuditMetadata (Lude.Maybe DayOfWeek)
samDayOfWeek = Lens.lens (dayOfWeek :: ScheduledAuditMetadata -> Lude.Maybe DayOfWeek) (\s a -> s {dayOfWeek = a} :: ScheduledAuditMetadata)
{-# DEPRECATED samDayOfWeek "Use generic-lens or generic-optics with 'dayOfWeek' instead." #-}

-- | The ARN of the scheduled audit.
--
-- /Note:/ Consider using 'scheduledAuditARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samScheduledAuditARN :: Lens.Lens' ScheduledAuditMetadata (Lude.Maybe Lude.Text)
samScheduledAuditARN = Lens.lens (scheduledAuditARN :: ScheduledAuditMetadata -> Lude.Maybe Lude.Text) (\s a -> s {scheduledAuditARN = a} :: ScheduledAuditMetadata)
{-# DEPRECATED samScheduledAuditARN "Use generic-lens or generic-optics with 'scheduledAuditARN' instead." #-}

instance Lude.FromJSON ScheduledAuditMetadata where
  parseJSON =
    Lude.withObject
      "ScheduledAuditMetadata"
      ( \x ->
          ScheduledAuditMetadata'
            Lude.<$> (x Lude..:? "frequency")
            Lude.<*> (x Lude..:? "scheduledAuditName")
            Lude.<*> (x Lude..:? "dayOfMonth")
            Lude.<*> (x Lude..:? "dayOfWeek")
            Lude.<*> (x Lude..:? "scheduledAuditArn")
      )
