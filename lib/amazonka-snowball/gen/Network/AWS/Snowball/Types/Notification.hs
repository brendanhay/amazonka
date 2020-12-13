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
    nNotifyAll,
    nSNSTopicARN,
    nJobStatesToNotify,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Snowball.Types.JobState

-- | The Amazon Simple Notification Service (Amazon SNS) notification settings associated with a specific job. The @Notification@ object is returned as a part of the response syntax of the @DescribeJob@ action in the @JobMetadata@ data type.
--
-- When the notification settings are defined during job creation, you can choose to notify based on a specific set of job states using the @JobStatesToNotify@ array of strings, or you can specify that you want to have Amazon SNS notifications sent out for all job states with @NotifyAll@ set to true.
--
-- /See:/ 'mkNotification' smart constructor.
data Notification = Notification'
  { -- | Any change in job state will trigger a notification for this job.
    notifyAll :: Lude.Maybe Lude.Bool,
    -- | The new SNS @TopicArn@ that you want to associate with this job. You can create Amazon Resource Names (ARNs) for topics by using the <https://docs.aws.amazon.com/sns/latest/api/API_CreateTopic.html CreateTopic> Amazon SNS API action.
    --
    -- You can subscribe email addresses to an Amazon SNS topic through the AWS Management Console, or by using the <https://docs.aws.amazon.com/sns/latest/api/API_Subscribe.html Subscribe> AWS Simple Notification Service (SNS) API action.
    snsTopicARN :: Lude.Maybe Lude.Text,
    -- | The list of job states that will trigger a notification for this job.
    jobStatesToNotify :: Lude.Maybe [JobState]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Notification' with the minimum fields required to make a request.
--
-- * 'notifyAll' - Any change in job state will trigger a notification for this job.
-- * 'snsTopicARN' - The new SNS @TopicArn@ that you want to associate with this job. You can create Amazon Resource Names (ARNs) for topics by using the <https://docs.aws.amazon.com/sns/latest/api/API_CreateTopic.html CreateTopic> Amazon SNS API action.
--
-- You can subscribe email addresses to an Amazon SNS topic through the AWS Management Console, or by using the <https://docs.aws.amazon.com/sns/latest/api/API_Subscribe.html Subscribe> AWS Simple Notification Service (SNS) API action.
-- * 'jobStatesToNotify' - The list of job states that will trigger a notification for this job.
mkNotification ::
  Notification
mkNotification =
  Notification'
    { notifyAll = Lude.Nothing,
      snsTopicARN = Lude.Nothing,
      jobStatesToNotify = Lude.Nothing
    }

-- | Any change in job state will trigger a notification for this job.
--
-- /Note:/ Consider using 'notifyAll' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nNotifyAll :: Lens.Lens' Notification (Lude.Maybe Lude.Bool)
nNotifyAll = Lens.lens (notifyAll :: Notification -> Lude.Maybe Lude.Bool) (\s a -> s {notifyAll = a} :: Notification)
{-# DEPRECATED nNotifyAll "Use generic-lens or generic-optics with 'notifyAll' instead." #-}

-- | The new SNS @TopicArn@ that you want to associate with this job. You can create Amazon Resource Names (ARNs) for topics by using the <https://docs.aws.amazon.com/sns/latest/api/API_CreateTopic.html CreateTopic> Amazon SNS API action.
--
-- You can subscribe email addresses to an Amazon SNS topic through the AWS Management Console, or by using the <https://docs.aws.amazon.com/sns/latest/api/API_Subscribe.html Subscribe> AWS Simple Notification Service (SNS) API action.
--
-- /Note:/ Consider using 'snsTopicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nSNSTopicARN :: Lens.Lens' Notification (Lude.Maybe Lude.Text)
nSNSTopicARN = Lens.lens (snsTopicARN :: Notification -> Lude.Maybe Lude.Text) (\s a -> s {snsTopicARN = a} :: Notification)
{-# DEPRECATED nSNSTopicARN "Use generic-lens or generic-optics with 'snsTopicARN' instead." #-}

-- | The list of job states that will trigger a notification for this job.
--
-- /Note:/ Consider using 'jobStatesToNotify' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nJobStatesToNotify :: Lens.Lens' Notification (Lude.Maybe [JobState])
nJobStatesToNotify = Lens.lens (jobStatesToNotify :: Notification -> Lude.Maybe [JobState]) (\s a -> s {jobStatesToNotify = a} :: Notification)
{-# DEPRECATED nJobStatesToNotify "Use generic-lens or generic-optics with 'jobStatesToNotify' instead." #-}

instance Lude.FromJSON Notification where
  parseJSON =
    Lude.withObject
      "Notification"
      ( \x ->
          Notification'
            Lude.<$> (x Lude..:? "NotifyAll")
            Lude.<*> (x Lude..:? "SnsTopicARN")
            Lude.<*> (x Lude..:? "JobStatesToNotify" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON Notification where
  toJSON Notification' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NotifyAll" Lude..=) Lude.<$> notifyAll,
            ("SnsTopicARN" Lude..=) Lude.<$> snsTopicARN,
            ("JobStatesToNotify" Lude..=) Lude.<$> jobStatesToNotify
          ]
      )
