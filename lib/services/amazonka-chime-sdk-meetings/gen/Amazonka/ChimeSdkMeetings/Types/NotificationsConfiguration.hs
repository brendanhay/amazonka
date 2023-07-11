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
-- Module      : Amazonka.ChimeSdkMeetings.Types.NotificationsConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMeetings.Types.NotificationsConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration for resource targets to receive notifications when
-- meeting and attendee events occur.
--
-- /See:/ 'newNotificationsConfiguration' smart constructor.
data NotificationsConfiguration = NotificationsConfiguration'
  { -- | The ARN of the AWS Lambda function in the notifications configuration.
    lambdaFunctionArn :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The ARN of the SNS topic.
    snsTopicArn :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The ARN of the SQS queue.
    sqsQueueArn :: Prelude.Maybe (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NotificationsConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lambdaFunctionArn', 'notificationsConfiguration_lambdaFunctionArn' - The ARN of the AWS Lambda function in the notifications configuration.
--
-- 'snsTopicArn', 'notificationsConfiguration_snsTopicArn' - The ARN of the SNS topic.
--
-- 'sqsQueueArn', 'notificationsConfiguration_sqsQueueArn' - The ARN of the SQS queue.
newNotificationsConfiguration ::
  NotificationsConfiguration
newNotificationsConfiguration =
  NotificationsConfiguration'
    { lambdaFunctionArn =
        Prelude.Nothing,
      snsTopicArn = Prelude.Nothing,
      sqsQueueArn = Prelude.Nothing
    }

-- | The ARN of the AWS Lambda function in the notifications configuration.
notificationsConfiguration_lambdaFunctionArn :: Lens.Lens' NotificationsConfiguration (Prelude.Maybe Prelude.Text)
notificationsConfiguration_lambdaFunctionArn = Lens.lens (\NotificationsConfiguration' {lambdaFunctionArn} -> lambdaFunctionArn) (\s@NotificationsConfiguration' {} a -> s {lambdaFunctionArn = a} :: NotificationsConfiguration) Prelude.. Lens.mapping Data._Sensitive

-- | The ARN of the SNS topic.
notificationsConfiguration_snsTopicArn :: Lens.Lens' NotificationsConfiguration (Prelude.Maybe Prelude.Text)
notificationsConfiguration_snsTopicArn = Lens.lens (\NotificationsConfiguration' {snsTopicArn} -> snsTopicArn) (\s@NotificationsConfiguration' {} a -> s {snsTopicArn = a} :: NotificationsConfiguration) Prelude.. Lens.mapping Data._Sensitive

-- | The ARN of the SQS queue.
notificationsConfiguration_sqsQueueArn :: Lens.Lens' NotificationsConfiguration (Prelude.Maybe Prelude.Text)
notificationsConfiguration_sqsQueueArn = Lens.lens (\NotificationsConfiguration' {sqsQueueArn} -> sqsQueueArn) (\s@NotificationsConfiguration' {} a -> s {sqsQueueArn = a} :: NotificationsConfiguration) Prelude.. Lens.mapping Data._Sensitive

instance Prelude.Hashable NotificationsConfiguration where
  hashWithSalt _salt NotificationsConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` lambdaFunctionArn
      `Prelude.hashWithSalt` snsTopicArn
      `Prelude.hashWithSalt` sqsQueueArn

instance Prelude.NFData NotificationsConfiguration where
  rnf NotificationsConfiguration' {..} =
    Prelude.rnf lambdaFunctionArn
      `Prelude.seq` Prelude.rnf snsTopicArn
      `Prelude.seq` Prelude.rnf sqsQueueArn

instance Data.ToJSON NotificationsConfiguration where
  toJSON NotificationsConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LambdaFunctionArn" Data..=)
              Prelude.<$> lambdaFunctionArn,
            ("SnsTopicArn" Data..=) Prelude.<$> snsTopicArn,
            ("SqsQueueArn" Data..=) Prelude.<$> sqsQueueArn
          ]
      )
