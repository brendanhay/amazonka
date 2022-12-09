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
-- Module      : Amazonka.Snowball.Types.Notification
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Snowball.Types.Notification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Snowball.Types.JobState

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
    -- You can subscribe email addresses to an Amazon SNS topic through the
    -- Amazon Web Services Management Console, or by using the
    -- <https://docs.aws.amazon.com/sns/latest/api/API_Subscribe.html Subscribe>
    -- Amazon Simple Notification Service (Amazon SNS) API action.
    snsTopicARN :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- You can subscribe email addresses to an Amazon SNS topic through the
-- Amazon Web Services Management Console, or by using the
-- <https://docs.aws.amazon.com/sns/latest/api/API_Subscribe.html Subscribe>
-- Amazon Simple Notification Service (Amazon SNS) API action.
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
notification_jobStatesToNotify = Lens.lens (\Notification' {jobStatesToNotify} -> jobStatesToNotify) (\s@Notification' {} a -> s {jobStatesToNotify = a} :: Notification) Prelude.. Lens.mapping Lens.coerced

-- | Any change in job state will trigger a notification for this job.
notification_notifyAll :: Lens.Lens' Notification (Prelude.Maybe Prelude.Bool)
notification_notifyAll = Lens.lens (\Notification' {notifyAll} -> notifyAll) (\s@Notification' {} a -> s {notifyAll = a} :: Notification)

-- | The new SNS @TopicArn@ that you want to associate with this job. You can
-- create Amazon Resource Names (ARNs) for topics by using the
-- <https://docs.aws.amazon.com/sns/latest/api/API_CreateTopic.html CreateTopic>
-- Amazon SNS API action.
--
-- You can subscribe email addresses to an Amazon SNS topic through the
-- Amazon Web Services Management Console, or by using the
-- <https://docs.aws.amazon.com/sns/latest/api/API_Subscribe.html Subscribe>
-- Amazon Simple Notification Service (Amazon SNS) API action.
notification_snsTopicARN :: Lens.Lens' Notification (Prelude.Maybe Prelude.Text)
notification_snsTopicARN = Lens.lens (\Notification' {snsTopicARN} -> snsTopicARN) (\s@Notification' {} a -> s {snsTopicARN = a} :: Notification)

instance Data.FromJSON Notification where
  parseJSON =
    Data.withObject
      "Notification"
      ( \x ->
          Notification'
            Prelude.<$> ( x Data..:? "JobStatesToNotify"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "NotifyAll")
            Prelude.<*> (x Data..:? "SnsTopicARN")
      )

instance Prelude.Hashable Notification where
  hashWithSalt _salt Notification' {..} =
    _salt `Prelude.hashWithSalt` jobStatesToNotify
      `Prelude.hashWithSalt` notifyAll
      `Prelude.hashWithSalt` snsTopicARN

instance Prelude.NFData Notification where
  rnf Notification' {..} =
    Prelude.rnf jobStatesToNotify
      `Prelude.seq` Prelude.rnf notifyAll
      `Prelude.seq` Prelude.rnf snsTopicARN

instance Data.ToJSON Notification where
  toJSON Notification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("JobStatesToNotify" Data..=)
              Prelude.<$> jobStatesToNotify,
            ("NotifyAll" Data..=) Prelude.<$> notifyAll,
            ("SnsTopicARN" Data..=) Prelude.<$> snsTopicARN
          ]
      )
