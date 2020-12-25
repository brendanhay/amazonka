{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.Notification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.Notification
  ( Notification (..),

    -- * Smart constructor
    mkNotification,

    -- * Lenses
    nJobStatesToNotify,
    nNotifyAll,
    nSnsTopicARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Snowball.Types.JobState as Types
import qualified Network.AWS.Snowball.Types.SnsTopicARN as Types

-- | The Amazon Simple Notification Service (Amazon SNS) notification settings associated with a specific job. The @Notification@ object is returned as a part of the response syntax of the @DescribeJob@ action in the @JobMetadata@ data type.
--
-- When the notification settings are defined during job creation, you can choose to notify based on a specific set of job states using the @JobStatesToNotify@ array of strings, or you can specify that you want to have Amazon SNS notifications sent out for all job states with @NotifyAll@ set to true.
--
-- /See:/ 'mkNotification' smart constructor.
data Notification = Notification'
  { -- | The list of job states that will trigger a notification for this job.
    jobStatesToNotify :: Core.Maybe [Types.JobState],
    -- | Any change in job state will trigger a notification for this job.
    notifyAll :: Core.Maybe Core.Bool,
    -- | The new SNS @TopicArn@ that you want to associate with this job. You can create Amazon Resource Names (ARNs) for topics by using the <https://docs.aws.amazon.com/sns/latest/api/API_CreateTopic.html CreateTopic> Amazon SNS API action.
    --
    -- You can subscribe email addresses to an Amazon SNS topic through the AWS Management Console, or by using the <https://docs.aws.amazon.com/sns/latest/api/API_Subscribe.html Subscribe> AWS Simple Notification Service (SNS) API action.
    snsTopicARN :: Core.Maybe Types.SnsTopicARN
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Notification' value with any optional fields omitted.
mkNotification ::
  Notification
mkNotification =
  Notification'
    { jobStatesToNotify = Core.Nothing,
      notifyAll = Core.Nothing,
      snsTopicARN = Core.Nothing
    }

-- | The list of job states that will trigger a notification for this job.
--
-- /Note:/ Consider using 'jobStatesToNotify' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nJobStatesToNotify :: Lens.Lens' Notification (Core.Maybe [Types.JobState])
nJobStatesToNotify = Lens.field @"jobStatesToNotify"
{-# DEPRECATED nJobStatesToNotify "Use generic-lens or generic-optics with 'jobStatesToNotify' instead." #-}

-- | Any change in job state will trigger a notification for this job.
--
-- /Note:/ Consider using 'notifyAll' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nNotifyAll :: Lens.Lens' Notification (Core.Maybe Core.Bool)
nNotifyAll = Lens.field @"notifyAll"
{-# DEPRECATED nNotifyAll "Use generic-lens or generic-optics with 'notifyAll' instead." #-}

-- | The new SNS @TopicArn@ that you want to associate with this job. You can create Amazon Resource Names (ARNs) for topics by using the <https://docs.aws.amazon.com/sns/latest/api/API_CreateTopic.html CreateTopic> Amazon SNS API action.
--
-- You can subscribe email addresses to an Amazon SNS topic through the AWS Management Console, or by using the <https://docs.aws.amazon.com/sns/latest/api/API_Subscribe.html Subscribe> AWS Simple Notification Service (SNS) API action.
--
-- /Note:/ Consider using 'snsTopicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nSnsTopicARN :: Lens.Lens' Notification (Core.Maybe Types.SnsTopicARN)
nSnsTopicARN = Lens.field @"snsTopicARN"
{-# DEPRECATED nSnsTopicARN "Use generic-lens or generic-optics with 'snsTopicARN' instead." #-}

instance Core.FromJSON Notification where
  toJSON Notification {..} =
    Core.object
      ( Core.catMaybes
          [ ("JobStatesToNotify" Core..=) Core.<$> jobStatesToNotify,
            ("NotifyAll" Core..=) Core.<$> notifyAll,
            ("SnsTopicARN" Core..=) Core.<$> snsTopicARN
          ]
      )

instance Core.FromJSON Notification where
  parseJSON =
    Core.withObject "Notification" Core.$
      \x ->
        Notification'
          Core.<$> (x Core..:? "JobStatesToNotify")
          Core.<*> (x Core..:? "NotifyAll")
          Core.<*> (x Core..:? "SnsTopicARN")
