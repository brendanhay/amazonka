{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Snowball.Types.Notification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.Notification where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Snowball.Types.JobState

-- | The Amazon Simple Notification Service (Amazon SNS) notification
-- settings associated with a specific job. The @Notification@ object is
-- returned as a part of the response syntax of the @DescribeJob@ action in
-- the @JobMetadata@ data type.
--
-- When the notification settings are defined during job creation, you can
-- choose to notify based on a specific set of job states using the
-- @JobStatesToNotify@ array of strings, or you can specify that you want
-- to have Amazon SNS notifications sent out for all job states with
-- @NotifyAll@ set to true.
--
-- /See:/ 'newNotification' smart constructor.
data Notification = Notification'
  { -- | The list of job states that will trigger a notification for this job.
    jobStatesToNotify :: Prelude.Maybe [JobState],
    -- | Any change in job state will trigger a notification for this job.
    notifyAll :: Prelude.Maybe Prelude.Bool,
    -- | The new SNS @TopicArn@ that you want to associate with this job. You can
    -- create Amazon Resource Names (ARNs) for topics by using the
    -- <https://docs.aws.amazon.com/sns/latest/api/API_CreateTopic.html CreateTopic>
    -- Amazon SNS API action.
    --
    -- You can subscribe email addresses to an Amazon SNS topic through the AWS
    -- Management Console, or by using the
    -- <https://docs.aws.amazon.com/sns/latest/api/API_Subscribe.html Subscribe>
    -- AWS Simple Notification Service (SNS) API action.
    snsTopicARN :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Notification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobStatesToNotify', 'notification_jobStatesToNotify' - The list of job states that will trigger a notification for this job.
--
-- 'notifyAll', 'notification_notifyAll' - Any change in job state will trigger a notification for this job.
--
-- 'snsTopicARN', 'notification_snsTopicARN' - The new SNS @TopicArn@ that you want to associate with this job. You can
-- create Amazon Resource Names (ARNs) for topics by using the
-- <https://docs.aws.amazon.com/sns/latest/api/API_CreateTopic.html CreateTopic>
-- Amazon SNS API action.
--
-- You can subscribe email addresses to an Amazon SNS topic through the AWS
-- Management Console, or by using the
-- <https://docs.aws.amazon.com/sns/latest/api/API_Subscribe.html Subscribe>
-- AWS Simple Notification Service (SNS) API action.
newNotification ::
  Notification
newNotification =
  Notification'
    { jobStatesToNotify = Prelude.Nothing,
      notifyAll = Prelude.Nothing,
      snsTopicARN = Prelude.Nothing
    }

-- | The list of job states that will trigger a notification for this job.
notification_jobStatesToNotify :: Lens.Lens' Notification (Prelude.Maybe [JobState])
notification_jobStatesToNotify = Lens.lens (\Notification' {jobStatesToNotify} -> jobStatesToNotify) (\s@Notification' {} a -> s {jobStatesToNotify = a} :: Notification) Prelude.. Lens.mapping Prelude._Coerce

-- | Any change in job state will trigger a notification for this job.
notification_notifyAll :: Lens.Lens' Notification (Prelude.Maybe Prelude.Bool)
notification_notifyAll = Lens.lens (\Notification' {notifyAll} -> notifyAll) (\s@Notification' {} a -> s {notifyAll = a} :: Notification)

-- | The new SNS @TopicArn@ that you want to associate with this job. You can
-- create Amazon Resource Names (ARNs) for topics by using the
-- <https://docs.aws.amazon.com/sns/latest/api/API_CreateTopic.html CreateTopic>
-- Amazon SNS API action.
--
-- You can subscribe email addresses to an Amazon SNS topic through the AWS
-- Management Console, or by using the
-- <https://docs.aws.amazon.com/sns/latest/api/API_Subscribe.html Subscribe>
-- AWS Simple Notification Service (SNS) API action.
notification_snsTopicARN :: Lens.Lens' Notification (Prelude.Maybe Prelude.Text)
notification_snsTopicARN = Lens.lens (\Notification' {snsTopicARN} -> snsTopicARN) (\s@Notification' {} a -> s {snsTopicARN = a} :: Notification)

instance Prelude.FromJSON Notification where
  parseJSON =
    Prelude.withObject
      "Notification"
      ( \x ->
          Notification'
            Prelude.<$> ( x Prelude..:? "JobStatesToNotify"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "NotifyAll")
            Prelude.<*> (x Prelude..:? "SnsTopicARN")
      )

instance Prelude.Hashable Notification

instance Prelude.NFData Notification

instance Prelude.ToJSON Notification where
  toJSON Notification' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("JobStatesToNotify" Prelude..=)
              Prelude.<$> jobStatesToNotify,
            ("NotifyAll" Prelude..=) Prelude.<$> notifyAll,
            ("SnsTopicARN" Prelude..=) Prelude.<$> snsTopicARN
          ]
      )
