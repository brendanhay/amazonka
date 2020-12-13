{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DescribeScheduledAudit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a scheduled audit.
module Network.AWS.IoT.DescribeScheduledAudit
  ( -- * Creating a request
    DescribeScheduledAudit (..),
    mkDescribeScheduledAudit,

    -- ** Request lenses
    dScheduledAuditName,

    -- * Destructuring the response
    DescribeScheduledAuditResponse (..),
    mkDescribeScheduledAuditResponse,

    -- ** Response lenses
    dsarsFrequency,
    dsarsScheduledAuditName,
    dsarsDayOfMonth,
    dsarsTargetCheckNames,
    dsarsDayOfWeek,
    dsarsScheduledAuditARN,
    dsarsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeScheduledAudit' smart constructor.
newtype DescribeScheduledAudit = DescribeScheduledAudit'
  { -- | The name of the scheduled audit whose information you want to get.
    scheduledAuditName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeScheduledAudit' with the minimum fields required to make a request.
--
-- * 'scheduledAuditName' - The name of the scheduled audit whose information you want to get.
mkDescribeScheduledAudit ::
  -- | 'scheduledAuditName'
  Lude.Text ->
  DescribeScheduledAudit
mkDescribeScheduledAudit pScheduledAuditName_ =
  DescribeScheduledAudit'
    { scheduledAuditName =
        pScheduledAuditName_
    }

-- | The name of the scheduled audit whose information you want to get.
--
-- /Note:/ Consider using 'scheduledAuditName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dScheduledAuditName :: Lens.Lens' DescribeScheduledAudit Lude.Text
dScheduledAuditName = Lens.lens (scheduledAuditName :: DescribeScheduledAudit -> Lude.Text) (\s a -> s {scheduledAuditName = a} :: DescribeScheduledAudit)
{-# DEPRECATED dScheduledAuditName "Use generic-lens or generic-optics with 'scheduledAuditName' instead." #-}

instance Lude.AWSRequest DescribeScheduledAudit where
  type Rs DescribeScheduledAudit = DescribeScheduledAuditResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeScheduledAuditResponse'
            Lude.<$> (x Lude..?> "frequency")
            Lude.<*> (x Lude..?> "scheduledAuditName")
            Lude.<*> (x Lude..?> "dayOfMonth")
            Lude.<*> (x Lude..?> "targetCheckNames" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "dayOfWeek")
            Lude.<*> (x Lude..?> "scheduledAuditArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeScheduledAudit where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeScheduledAudit where
  toPath DescribeScheduledAudit' {..} =
    Lude.mconcat
      ["/audit/scheduledaudits/", Lude.toBS scheduledAuditName]

instance Lude.ToQuery DescribeScheduledAudit where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeScheduledAuditResponse' smart constructor.
data DescribeScheduledAuditResponse = DescribeScheduledAuditResponse'
  { -- | How often the scheduled audit takes place. One of "DAILY", "WEEKLY", "BIWEEKLY", or "MONTHLY". The start time of each audit is determined by the system.
    frequency :: Lude.Maybe AuditFrequency,
    -- | The name of the scheduled audit.
    scheduledAuditName :: Lude.Maybe Lude.Text,
    -- | The day of the month on which the scheduled audit takes place. Will be "1" through "31" or "LAST". If days 29-31 are specified, and the month does not have that many days, the audit takes place on the "LAST" day of the month.
    dayOfMonth :: Lude.Maybe Lude.Text,
    -- | Which checks are performed during the scheduled audit. Checks must be enabled for your account. (Use @DescribeAccountAuditConfiguration@ to see the list of all checks, including those that are enabled or use @UpdateAccountAuditConfiguration@ to select which checks are enabled.)
    targetCheckNames :: Lude.Maybe [Lude.Text],
    -- | The day of the week on which the scheduled audit takes place. One of "SUN", "MON", "TUE", "WED", "THU", "FRI", or "SAT".
    dayOfWeek :: Lude.Maybe DayOfWeek,
    -- | The ARN of the scheduled audit.
    scheduledAuditARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeScheduledAuditResponse' with the minimum fields required to make a request.
--
-- * 'frequency' - How often the scheduled audit takes place. One of "DAILY", "WEEKLY", "BIWEEKLY", or "MONTHLY". The start time of each audit is determined by the system.
-- * 'scheduledAuditName' - The name of the scheduled audit.
-- * 'dayOfMonth' - The day of the month on which the scheduled audit takes place. Will be "1" through "31" or "LAST". If days 29-31 are specified, and the month does not have that many days, the audit takes place on the "LAST" day of the month.
-- * 'targetCheckNames' - Which checks are performed during the scheduled audit. Checks must be enabled for your account. (Use @DescribeAccountAuditConfiguration@ to see the list of all checks, including those that are enabled or use @UpdateAccountAuditConfiguration@ to select which checks are enabled.)
-- * 'dayOfWeek' - The day of the week on which the scheduled audit takes place. One of "SUN", "MON", "TUE", "WED", "THU", "FRI", or "SAT".
-- * 'scheduledAuditARN' - The ARN of the scheduled audit.
-- * 'responseStatus' - The response status code.
mkDescribeScheduledAuditResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeScheduledAuditResponse
mkDescribeScheduledAuditResponse pResponseStatus_ =
  DescribeScheduledAuditResponse'
    { frequency = Lude.Nothing,
      scheduledAuditName = Lude.Nothing,
      dayOfMonth = Lude.Nothing,
      targetCheckNames = Lude.Nothing,
      dayOfWeek = Lude.Nothing,
      scheduledAuditARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | How often the scheduled audit takes place. One of "DAILY", "WEEKLY", "BIWEEKLY", or "MONTHLY". The start time of each audit is determined by the system.
--
-- /Note:/ Consider using 'frequency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsarsFrequency :: Lens.Lens' DescribeScheduledAuditResponse (Lude.Maybe AuditFrequency)
dsarsFrequency = Lens.lens (frequency :: DescribeScheduledAuditResponse -> Lude.Maybe AuditFrequency) (\s a -> s {frequency = a} :: DescribeScheduledAuditResponse)
{-# DEPRECATED dsarsFrequency "Use generic-lens or generic-optics with 'frequency' instead." #-}

-- | The name of the scheduled audit.
--
-- /Note:/ Consider using 'scheduledAuditName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsarsScheduledAuditName :: Lens.Lens' DescribeScheduledAuditResponse (Lude.Maybe Lude.Text)
dsarsScheduledAuditName = Lens.lens (scheduledAuditName :: DescribeScheduledAuditResponse -> Lude.Maybe Lude.Text) (\s a -> s {scheduledAuditName = a} :: DescribeScheduledAuditResponse)
{-# DEPRECATED dsarsScheduledAuditName "Use generic-lens or generic-optics with 'scheduledAuditName' instead." #-}

-- | The day of the month on which the scheduled audit takes place. Will be "1" through "31" or "LAST". If days 29-31 are specified, and the month does not have that many days, the audit takes place on the "LAST" day of the month.
--
-- /Note:/ Consider using 'dayOfMonth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsarsDayOfMonth :: Lens.Lens' DescribeScheduledAuditResponse (Lude.Maybe Lude.Text)
dsarsDayOfMonth = Lens.lens (dayOfMonth :: DescribeScheduledAuditResponse -> Lude.Maybe Lude.Text) (\s a -> s {dayOfMonth = a} :: DescribeScheduledAuditResponse)
{-# DEPRECATED dsarsDayOfMonth "Use generic-lens or generic-optics with 'dayOfMonth' instead." #-}

-- | Which checks are performed during the scheduled audit. Checks must be enabled for your account. (Use @DescribeAccountAuditConfiguration@ to see the list of all checks, including those that are enabled or use @UpdateAccountAuditConfiguration@ to select which checks are enabled.)
--
-- /Note:/ Consider using 'targetCheckNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsarsTargetCheckNames :: Lens.Lens' DescribeScheduledAuditResponse (Lude.Maybe [Lude.Text])
dsarsTargetCheckNames = Lens.lens (targetCheckNames :: DescribeScheduledAuditResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {targetCheckNames = a} :: DescribeScheduledAuditResponse)
{-# DEPRECATED dsarsTargetCheckNames "Use generic-lens or generic-optics with 'targetCheckNames' instead." #-}

-- | The day of the week on which the scheduled audit takes place. One of "SUN", "MON", "TUE", "WED", "THU", "FRI", or "SAT".
--
-- /Note:/ Consider using 'dayOfWeek' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsarsDayOfWeek :: Lens.Lens' DescribeScheduledAuditResponse (Lude.Maybe DayOfWeek)
dsarsDayOfWeek = Lens.lens (dayOfWeek :: DescribeScheduledAuditResponse -> Lude.Maybe DayOfWeek) (\s a -> s {dayOfWeek = a} :: DescribeScheduledAuditResponse)
{-# DEPRECATED dsarsDayOfWeek "Use generic-lens or generic-optics with 'dayOfWeek' instead." #-}

-- | The ARN of the scheduled audit.
--
-- /Note:/ Consider using 'scheduledAuditARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsarsScheduledAuditARN :: Lens.Lens' DescribeScheduledAuditResponse (Lude.Maybe Lude.Text)
dsarsScheduledAuditARN = Lens.lens (scheduledAuditARN :: DescribeScheduledAuditResponse -> Lude.Maybe Lude.Text) (\s a -> s {scheduledAuditARN = a} :: DescribeScheduledAuditResponse)
{-# DEPRECATED dsarsScheduledAuditARN "Use generic-lens or generic-optics with 'scheduledAuditARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsarsResponseStatus :: Lens.Lens' DescribeScheduledAuditResponse Lude.Int
dsarsResponseStatus = Lens.lens (responseStatus :: DescribeScheduledAuditResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeScheduledAuditResponse)
{-# DEPRECATED dsarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
