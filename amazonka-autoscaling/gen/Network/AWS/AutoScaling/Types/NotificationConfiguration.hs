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
-- Module      : Network.AWS.AutoScaling.Types.NotificationConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.NotificationConfiguration where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a notification.
--
-- /See:/ 'newNotificationConfiguration' smart constructor.
data NotificationConfiguration = NotificationConfiguration'
  { -- | One of the following event notification types:
    --
    -- -   @autoscaling:EC2_INSTANCE_LAUNCH@
    --
    -- -   @autoscaling:EC2_INSTANCE_LAUNCH_ERROR@
    --
    -- -   @autoscaling:EC2_INSTANCE_TERMINATE@
    --
    -- -   @autoscaling:EC2_INSTANCE_TERMINATE_ERROR@
    --
    -- -   @autoscaling:TEST_NOTIFICATION@
    notificationType :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service
    -- (Amazon SNS) topic.
    topicARN :: Prelude.Maybe Prelude.Text,
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'NotificationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notificationType', 'notificationConfiguration_notificationType' - One of the following event notification types:
--
-- -   @autoscaling:EC2_INSTANCE_LAUNCH@
--
-- -   @autoscaling:EC2_INSTANCE_LAUNCH_ERROR@
--
-- -   @autoscaling:EC2_INSTANCE_TERMINATE@
--
-- -   @autoscaling:EC2_INSTANCE_TERMINATE_ERROR@
--
-- -   @autoscaling:TEST_NOTIFICATION@
--
-- 'topicARN', 'notificationConfiguration_topicARN' - The Amazon Resource Name (ARN) of the Amazon Simple Notification Service
-- (Amazon SNS) topic.
--
-- 'autoScalingGroupName', 'notificationConfiguration_autoScalingGroupName' - The name of the Auto Scaling group.
newNotificationConfiguration ::
  NotificationConfiguration
newNotificationConfiguration =
  NotificationConfiguration'
    { notificationType =
        Prelude.Nothing,
      topicARN = Prelude.Nothing,
      autoScalingGroupName = Prelude.Nothing
    }

-- | One of the following event notification types:
--
-- -   @autoscaling:EC2_INSTANCE_LAUNCH@
--
-- -   @autoscaling:EC2_INSTANCE_LAUNCH_ERROR@
--
-- -   @autoscaling:EC2_INSTANCE_TERMINATE@
--
-- -   @autoscaling:EC2_INSTANCE_TERMINATE_ERROR@
--
-- -   @autoscaling:TEST_NOTIFICATION@
notificationConfiguration_notificationType :: Lens.Lens' NotificationConfiguration (Prelude.Maybe Prelude.Text)
notificationConfiguration_notificationType = Lens.lens (\NotificationConfiguration' {notificationType} -> notificationType) (\s@NotificationConfiguration' {} a -> s {notificationType = a} :: NotificationConfiguration)

-- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service
-- (Amazon SNS) topic.
notificationConfiguration_topicARN :: Lens.Lens' NotificationConfiguration (Prelude.Maybe Prelude.Text)
notificationConfiguration_topicARN = Lens.lens (\NotificationConfiguration' {topicARN} -> topicARN) (\s@NotificationConfiguration' {} a -> s {topicARN = a} :: NotificationConfiguration)

-- | The name of the Auto Scaling group.
notificationConfiguration_autoScalingGroupName :: Lens.Lens' NotificationConfiguration (Prelude.Maybe Prelude.Text)
notificationConfiguration_autoScalingGroupName = Lens.lens (\NotificationConfiguration' {autoScalingGroupName} -> autoScalingGroupName) (\s@NotificationConfiguration' {} a -> s {autoScalingGroupName = a} :: NotificationConfiguration)

instance Prelude.FromXML NotificationConfiguration where
  parseXML x =
    NotificationConfiguration'
      Prelude.<$> (x Prelude..@? "NotificationType")
      Prelude.<*> (x Prelude..@? "TopicARN")
      Prelude.<*> (x Prelude..@? "AutoScalingGroupName")

instance Prelude.Hashable NotificationConfiguration

instance Prelude.NFData NotificationConfiguration
