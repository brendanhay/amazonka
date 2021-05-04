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
-- Module      : Network.AWS.Rekognition.Types.NotificationChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.NotificationChannel where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The Amazon Simple Notification Service topic to which Amazon Rekognition
-- publishes the completion status of a video analysis operation. For more
-- information, see api-video.
--
-- /See:/ 'newNotificationChannel' smart constructor.
data NotificationChannel = NotificationChannel'
  { -- | The Amazon SNS topic to which Amazon Rekognition to posts the completion
    -- status.
    sNSTopicArn :: Prelude.Text,
    -- | The ARN of an IAM role that gives Amazon Rekognition publishing
    -- permissions to the Amazon SNS topic.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'NotificationChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sNSTopicArn', 'notificationChannel_sNSTopicArn' - The Amazon SNS topic to which Amazon Rekognition to posts the completion
-- status.
--
-- 'roleArn', 'notificationChannel_roleArn' - The ARN of an IAM role that gives Amazon Rekognition publishing
-- permissions to the Amazon SNS topic.
newNotificationChannel ::
  -- | 'sNSTopicArn'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  NotificationChannel
newNotificationChannel pSNSTopicArn_ pRoleArn_ =
  NotificationChannel'
    { sNSTopicArn = pSNSTopicArn_,
      roleArn = pRoleArn_
    }

-- | The Amazon SNS topic to which Amazon Rekognition to posts the completion
-- status.
notificationChannel_sNSTopicArn :: Lens.Lens' NotificationChannel Prelude.Text
notificationChannel_sNSTopicArn = Lens.lens (\NotificationChannel' {sNSTopicArn} -> sNSTopicArn) (\s@NotificationChannel' {} a -> s {sNSTopicArn = a} :: NotificationChannel)

-- | The ARN of an IAM role that gives Amazon Rekognition publishing
-- permissions to the Amazon SNS topic.
notificationChannel_roleArn :: Lens.Lens' NotificationChannel Prelude.Text
notificationChannel_roleArn = Lens.lens (\NotificationChannel' {roleArn} -> roleArn) (\s@NotificationChannel' {} a -> s {roleArn = a} :: NotificationChannel)

instance Prelude.Hashable NotificationChannel

instance Prelude.NFData NotificationChannel

instance Prelude.ToJSON NotificationChannel where
  toJSON NotificationChannel' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("SNSTopicArn" Prelude..= sNSTopicArn),
            Prelude.Just ("RoleArn" Prelude..= roleArn)
          ]
      )
