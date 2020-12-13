{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.UpdateScheduledAudit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a scheduled audit, including which checks are performed and how often the audit takes place.
module Network.AWS.IoT.UpdateScheduledAudit
  ( -- * Creating a request
    UpdateScheduledAudit (..),
    mkUpdateScheduledAudit,

    -- ** Request lenses
    usaFrequency,
    usaScheduledAuditName,
    usaDayOfMonth,
    usaTargetCheckNames,
    usaDayOfWeek,

    -- * Destructuring the response
    UpdateScheduledAuditResponse (..),
    mkUpdateScheduledAuditResponse,

    -- ** Response lenses
    usarsScheduledAuditARN,
    usarsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateScheduledAudit' smart constructor.
data UpdateScheduledAudit = UpdateScheduledAudit'
  { -- | How often the scheduled audit takes place. Can be one of "DAILY", "WEEKLY", "BIWEEKLY", or "MONTHLY". The start time of each audit is determined by the system.
    frequency :: Lude.Maybe AuditFrequency,
    -- | The name of the scheduled audit. (Max. 128 chars)
    scheduledAuditName :: Lude.Text,
    -- | The day of the month on which the scheduled audit takes place. Can be "1" through "31" or "LAST". This field is required if the "frequency" parameter is set to "MONTHLY". If days 29-31 are specified, and the month does not have that many days, the audit takes place on the "LAST" day of the month.
    dayOfMonth :: Lude.Maybe Lude.Text,
    -- | Which checks are performed during the scheduled audit. Checks must be enabled for your account. (Use @DescribeAccountAuditConfiguration@ to see the list of all checks, including those that are enabled or use @UpdateAccountAuditConfiguration@ to select which checks are enabled.)
    targetCheckNames :: Lude.Maybe [Lude.Text],
    -- | The day of the week on which the scheduled audit takes place. Can be one of "SUN", "MON", "TUE", "WED", "THU", "FRI", or "SAT". This field is required if the "frequency" parameter is set to "WEEKLY" or "BIWEEKLY".
    dayOfWeek :: Lude.Maybe DayOfWeek
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateScheduledAudit' with the minimum fields required to make a request.
--
-- * 'frequency' - How often the scheduled audit takes place. Can be one of "DAILY", "WEEKLY", "BIWEEKLY", or "MONTHLY". The start time of each audit is determined by the system.
-- * 'scheduledAuditName' - The name of the scheduled audit. (Max. 128 chars)
-- * 'dayOfMonth' - The day of the month on which the scheduled audit takes place. Can be "1" through "31" or "LAST". This field is required if the "frequency" parameter is set to "MONTHLY". If days 29-31 are specified, and the month does not have that many days, the audit takes place on the "LAST" day of the month.
-- * 'targetCheckNames' - Which checks are performed during the scheduled audit. Checks must be enabled for your account. (Use @DescribeAccountAuditConfiguration@ to see the list of all checks, including those that are enabled or use @UpdateAccountAuditConfiguration@ to select which checks are enabled.)
-- * 'dayOfWeek' - The day of the week on which the scheduled audit takes place. Can be one of "SUN", "MON", "TUE", "WED", "THU", "FRI", or "SAT". This field is required if the "frequency" parameter is set to "WEEKLY" or "BIWEEKLY".
mkUpdateScheduledAudit ::
  -- | 'scheduledAuditName'
  Lude.Text ->
  UpdateScheduledAudit
mkUpdateScheduledAudit pScheduledAuditName_ =
  UpdateScheduledAudit'
    { frequency = Lude.Nothing,
      scheduledAuditName = pScheduledAuditName_,
      dayOfMonth = Lude.Nothing,
      targetCheckNames = Lude.Nothing,
      dayOfWeek = Lude.Nothing
    }

-- | How often the scheduled audit takes place. Can be one of "DAILY", "WEEKLY", "BIWEEKLY", or "MONTHLY". The start time of each audit is determined by the system.
--
-- /Note:/ Consider using 'frequency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usaFrequency :: Lens.Lens' UpdateScheduledAudit (Lude.Maybe AuditFrequency)
usaFrequency = Lens.lens (frequency :: UpdateScheduledAudit -> Lude.Maybe AuditFrequency) (\s a -> s {frequency = a} :: UpdateScheduledAudit)
{-# DEPRECATED usaFrequency "Use generic-lens or generic-optics with 'frequency' instead." #-}

-- | The name of the scheduled audit. (Max. 128 chars)
--
-- /Note:/ Consider using 'scheduledAuditName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usaScheduledAuditName :: Lens.Lens' UpdateScheduledAudit Lude.Text
usaScheduledAuditName = Lens.lens (scheduledAuditName :: UpdateScheduledAudit -> Lude.Text) (\s a -> s {scheduledAuditName = a} :: UpdateScheduledAudit)
{-# DEPRECATED usaScheduledAuditName "Use generic-lens or generic-optics with 'scheduledAuditName' instead." #-}

-- | The day of the month on which the scheduled audit takes place. Can be "1" through "31" or "LAST". This field is required if the "frequency" parameter is set to "MONTHLY". If days 29-31 are specified, and the month does not have that many days, the audit takes place on the "LAST" day of the month.
--
-- /Note:/ Consider using 'dayOfMonth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usaDayOfMonth :: Lens.Lens' UpdateScheduledAudit (Lude.Maybe Lude.Text)
usaDayOfMonth = Lens.lens (dayOfMonth :: UpdateScheduledAudit -> Lude.Maybe Lude.Text) (\s a -> s {dayOfMonth = a} :: UpdateScheduledAudit)
{-# DEPRECATED usaDayOfMonth "Use generic-lens or generic-optics with 'dayOfMonth' instead." #-}

-- | Which checks are performed during the scheduled audit. Checks must be enabled for your account. (Use @DescribeAccountAuditConfiguration@ to see the list of all checks, including those that are enabled or use @UpdateAccountAuditConfiguration@ to select which checks are enabled.)
--
-- /Note:/ Consider using 'targetCheckNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usaTargetCheckNames :: Lens.Lens' UpdateScheduledAudit (Lude.Maybe [Lude.Text])
usaTargetCheckNames = Lens.lens (targetCheckNames :: UpdateScheduledAudit -> Lude.Maybe [Lude.Text]) (\s a -> s {targetCheckNames = a} :: UpdateScheduledAudit)
{-# DEPRECATED usaTargetCheckNames "Use generic-lens or generic-optics with 'targetCheckNames' instead." #-}

-- | The day of the week on which the scheduled audit takes place. Can be one of "SUN", "MON", "TUE", "WED", "THU", "FRI", or "SAT". This field is required if the "frequency" parameter is set to "WEEKLY" or "BIWEEKLY".
--
-- /Note:/ Consider using 'dayOfWeek' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usaDayOfWeek :: Lens.Lens' UpdateScheduledAudit (Lude.Maybe DayOfWeek)
usaDayOfWeek = Lens.lens (dayOfWeek :: UpdateScheduledAudit -> Lude.Maybe DayOfWeek) (\s a -> s {dayOfWeek = a} :: UpdateScheduledAudit)
{-# DEPRECATED usaDayOfWeek "Use generic-lens or generic-optics with 'dayOfWeek' instead." #-}

instance Lude.AWSRequest UpdateScheduledAudit where
  type Rs UpdateScheduledAudit = UpdateScheduledAuditResponse
  request = Req.patchJSON ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateScheduledAuditResponse'
            Lude.<$> (x Lude..?> "scheduledAuditArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateScheduledAudit where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON UpdateScheduledAudit where
  toJSON UpdateScheduledAudit' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("frequency" Lude..=) Lude.<$> frequency,
            ("dayOfMonth" Lude..=) Lude.<$> dayOfMonth,
            ("targetCheckNames" Lude..=) Lude.<$> targetCheckNames,
            ("dayOfWeek" Lude..=) Lude.<$> dayOfWeek
          ]
      )

instance Lude.ToPath UpdateScheduledAudit where
  toPath UpdateScheduledAudit' {..} =
    Lude.mconcat
      ["/audit/scheduledaudits/", Lude.toBS scheduledAuditName]

instance Lude.ToQuery UpdateScheduledAudit where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateScheduledAuditResponse' smart constructor.
data UpdateScheduledAuditResponse = UpdateScheduledAuditResponse'
  { -- | The ARN of the scheduled audit.
    scheduledAuditARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateScheduledAuditResponse' with the minimum fields required to make a request.
--
-- * 'scheduledAuditARN' - The ARN of the scheduled audit.
-- * 'responseStatus' - The response status code.
mkUpdateScheduledAuditResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateScheduledAuditResponse
mkUpdateScheduledAuditResponse pResponseStatus_ =
  UpdateScheduledAuditResponse'
    { scheduledAuditARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the scheduled audit.
--
-- /Note:/ Consider using 'scheduledAuditARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usarsScheduledAuditARN :: Lens.Lens' UpdateScheduledAuditResponse (Lude.Maybe Lude.Text)
usarsScheduledAuditARN = Lens.lens (scheduledAuditARN :: UpdateScheduledAuditResponse -> Lude.Maybe Lude.Text) (\s a -> s {scheduledAuditARN = a} :: UpdateScheduledAuditResponse)
{-# DEPRECATED usarsScheduledAuditARN "Use generic-lens or generic-optics with 'scheduledAuditARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usarsResponseStatus :: Lens.Lens' UpdateScheduledAuditResponse Lude.Int
usarsResponseStatus = Lens.lens (responseStatus :: UpdateScheduledAuditResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateScheduledAuditResponse)
{-# DEPRECATED usarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
