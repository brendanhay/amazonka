{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CreateScheduledAudit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a scheduled audit that is run at a specified time interval.
module Network.AWS.IoT.CreateScheduledAudit
  ( -- * Creating a request
    CreateScheduledAudit (..),
    mkCreateScheduledAudit,

    -- ** Request lenses
    csaFrequency,
    csaScheduledAuditName,
    csaDayOfMonth,
    csaTargetCheckNames,
    csaDayOfWeek,
    csaTags,

    -- * Destructuring the response
    CreateScheduledAuditResponse (..),
    mkCreateScheduledAuditResponse,

    -- ** Response lenses
    csarsScheduledAuditARN,
    csarsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateScheduledAudit' smart constructor.
data CreateScheduledAudit = CreateScheduledAudit'
  { -- | How often the scheduled audit takes place. Can be one of "DAILY", "WEEKLY", "BIWEEKLY" or "MONTHLY". The start time of each audit is determined by the system.
    frequency :: AuditFrequency,
    -- | The name you want to give to the scheduled audit. (Max. 128 chars)
    scheduledAuditName :: Lude.Text,
    -- | The day of the month on which the scheduled audit takes place. Can be "1" through "31" or "LAST". This field is required if the "frequency" parameter is set to "MONTHLY". If days 29-31 are specified, and the month does not have that many days, the audit takes place on the "LAST" day of the month.
    dayOfMonth :: Lude.Maybe Lude.Text,
    -- | Which checks are performed during the scheduled audit. Checks must be enabled for your account. (Use @DescribeAccountAuditConfiguration@ to see the list of all checks, including those that are enabled or use @UpdateAccountAuditConfiguration@ to select which checks are enabled.)
    targetCheckNames :: [Lude.Text],
    -- | The day of the week on which the scheduled audit takes place. Can be one of "SUN", "MON", "TUE", "WED", "THU", "FRI", or "SAT". This field is required if the "frequency" parameter is set to "WEEKLY" or "BIWEEKLY".
    dayOfWeek :: Lude.Maybe DayOfWeek,
    -- | Metadata that can be used to manage the scheduled audit.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateScheduledAudit' with the minimum fields required to make a request.
--
-- * 'frequency' - How often the scheduled audit takes place. Can be one of "DAILY", "WEEKLY", "BIWEEKLY" or "MONTHLY". The start time of each audit is determined by the system.
-- * 'scheduledAuditName' - The name you want to give to the scheduled audit. (Max. 128 chars)
-- * 'dayOfMonth' - The day of the month on which the scheduled audit takes place. Can be "1" through "31" or "LAST". This field is required if the "frequency" parameter is set to "MONTHLY". If days 29-31 are specified, and the month does not have that many days, the audit takes place on the "LAST" day of the month.
-- * 'targetCheckNames' - Which checks are performed during the scheduled audit. Checks must be enabled for your account. (Use @DescribeAccountAuditConfiguration@ to see the list of all checks, including those that are enabled or use @UpdateAccountAuditConfiguration@ to select which checks are enabled.)
-- * 'dayOfWeek' - The day of the week on which the scheduled audit takes place. Can be one of "SUN", "MON", "TUE", "WED", "THU", "FRI", or "SAT". This field is required if the "frequency" parameter is set to "WEEKLY" or "BIWEEKLY".
-- * 'tags' - Metadata that can be used to manage the scheduled audit.
mkCreateScheduledAudit ::
  -- | 'frequency'
  AuditFrequency ->
  -- | 'scheduledAuditName'
  Lude.Text ->
  CreateScheduledAudit
mkCreateScheduledAudit pFrequency_ pScheduledAuditName_ =
  CreateScheduledAudit'
    { frequency = pFrequency_,
      scheduledAuditName = pScheduledAuditName_,
      dayOfMonth = Lude.Nothing,
      targetCheckNames = Lude.mempty,
      dayOfWeek = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | How often the scheduled audit takes place. Can be one of "DAILY", "WEEKLY", "BIWEEKLY" or "MONTHLY". The start time of each audit is determined by the system.
--
-- /Note:/ Consider using 'frequency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csaFrequency :: Lens.Lens' CreateScheduledAudit AuditFrequency
csaFrequency = Lens.lens (frequency :: CreateScheduledAudit -> AuditFrequency) (\s a -> s {frequency = a} :: CreateScheduledAudit)
{-# DEPRECATED csaFrequency "Use generic-lens or generic-optics with 'frequency' instead." #-}

-- | The name you want to give to the scheduled audit. (Max. 128 chars)
--
-- /Note:/ Consider using 'scheduledAuditName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csaScheduledAuditName :: Lens.Lens' CreateScheduledAudit Lude.Text
csaScheduledAuditName = Lens.lens (scheduledAuditName :: CreateScheduledAudit -> Lude.Text) (\s a -> s {scheduledAuditName = a} :: CreateScheduledAudit)
{-# DEPRECATED csaScheduledAuditName "Use generic-lens or generic-optics with 'scheduledAuditName' instead." #-}

-- | The day of the month on which the scheduled audit takes place. Can be "1" through "31" or "LAST". This field is required if the "frequency" parameter is set to "MONTHLY". If days 29-31 are specified, and the month does not have that many days, the audit takes place on the "LAST" day of the month.
--
-- /Note:/ Consider using 'dayOfMonth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csaDayOfMonth :: Lens.Lens' CreateScheduledAudit (Lude.Maybe Lude.Text)
csaDayOfMonth = Lens.lens (dayOfMonth :: CreateScheduledAudit -> Lude.Maybe Lude.Text) (\s a -> s {dayOfMonth = a} :: CreateScheduledAudit)
{-# DEPRECATED csaDayOfMonth "Use generic-lens or generic-optics with 'dayOfMonth' instead." #-}

-- | Which checks are performed during the scheduled audit. Checks must be enabled for your account. (Use @DescribeAccountAuditConfiguration@ to see the list of all checks, including those that are enabled or use @UpdateAccountAuditConfiguration@ to select which checks are enabled.)
--
-- /Note:/ Consider using 'targetCheckNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csaTargetCheckNames :: Lens.Lens' CreateScheduledAudit [Lude.Text]
csaTargetCheckNames = Lens.lens (targetCheckNames :: CreateScheduledAudit -> [Lude.Text]) (\s a -> s {targetCheckNames = a} :: CreateScheduledAudit)
{-# DEPRECATED csaTargetCheckNames "Use generic-lens or generic-optics with 'targetCheckNames' instead." #-}

-- | The day of the week on which the scheduled audit takes place. Can be one of "SUN", "MON", "TUE", "WED", "THU", "FRI", or "SAT". This field is required if the "frequency" parameter is set to "WEEKLY" or "BIWEEKLY".
--
-- /Note:/ Consider using 'dayOfWeek' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csaDayOfWeek :: Lens.Lens' CreateScheduledAudit (Lude.Maybe DayOfWeek)
csaDayOfWeek = Lens.lens (dayOfWeek :: CreateScheduledAudit -> Lude.Maybe DayOfWeek) (\s a -> s {dayOfWeek = a} :: CreateScheduledAudit)
{-# DEPRECATED csaDayOfWeek "Use generic-lens or generic-optics with 'dayOfWeek' instead." #-}

-- | Metadata that can be used to manage the scheduled audit.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csaTags :: Lens.Lens' CreateScheduledAudit (Lude.Maybe [Tag])
csaTags = Lens.lens (tags :: CreateScheduledAudit -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateScheduledAudit)
{-# DEPRECATED csaTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateScheduledAudit where
  type Rs CreateScheduledAudit = CreateScheduledAuditResponse
  request = Req.postJSON ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateScheduledAuditResponse'
            Lude.<$> (x Lude..?> "scheduledAuditArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateScheduledAudit where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON CreateScheduledAudit where
  toJSON CreateScheduledAudit' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("frequency" Lude..= frequency),
            ("dayOfMonth" Lude..=) Lude.<$> dayOfMonth,
            Lude.Just ("targetCheckNames" Lude..= targetCheckNames),
            ("dayOfWeek" Lude..=) Lude.<$> dayOfWeek,
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateScheduledAudit where
  toPath CreateScheduledAudit' {..} =
    Lude.mconcat
      ["/audit/scheduledaudits/", Lude.toBS scheduledAuditName]

instance Lude.ToQuery CreateScheduledAudit where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateScheduledAuditResponse' smart constructor.
data CreateScheduledAuditResponse = CreateScheduledAuditResponse'
  { -- | The ARN of the scheduled audit.
    scheduledAuditARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateScheduledAuditResponse' with the minimum fields required to make a request.
--
-- * 'scheduledAuditARN' - The ARN of the scheduled audit.
-- * 'responseStatus' - The response status code.
mkCreateScheduledAuditResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateScheduledAuditResponse
mkCreateScheduledAuditResponse pResponseStatus_ =
  CreateScheduledAuditResponse'
    { scheduledAuditARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the scheduled audit.
--
-- /Note:/ Consider using 'scheduledAuditARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csarsScheduledAuditARN :: Lens.Lens' CreateScheduledAuditResponse (Lude.Maybe Lude.Text)
csarsScheduledAuditARN = Lens.lens (scheduledAuditARN :: CreateScheduledAuditResponse -> Lude.Maybe Lude.Text) (\s a -> s {scheduledAuditARN = a} :: CreateScheduledAuditResponse)
{-# DEPRECATED csarsScheduledAuditARN "Use generic-lens or generic-optics with 'scheduledAuditARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csarsResponseStatus :: Lens.Lens' CreateScheduledAuditResponse Lude.Int
csarsResponseStatus = Lens.lens (responseStatus :: CreateScheduledAuditResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateScheduledAuditResponse)
{-# DEPRECATED csarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
