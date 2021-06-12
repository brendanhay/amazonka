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
-- Module      : Network.AWS.ElastiCache.Types.NotificationConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.NotificationConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes a notification topic and its status. Notification topics are
-- used for publishing ElastiCache events to subscribers using Amazon
-- Simple Notification Service (SNS).
--
-- /See:/ 'newNotificationConfiguration' smart constructor.
data NotificationConfiguration = NotificationConfiguration'
  { -- | The current state of the topic.
    topicStatus :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) that identifies the topic.
    topicArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'NotificationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'topicStatus', 'notificationConfiguration_topicStatus' - The current state of the topic.
--
-- 'topicArn', 'notificationConfiguration_topicArn' - The Amazon Resource Name (ARN) that identifies the topic.
newNotificationConfiguration ::
  NotificationConfiguration
newNotificationConfiguration =
  NotificationConfiguration'
    { topicStatus =
        Core.Nothing,
      topicArn = Core.Nothing
    }

-- | The current state of the topic.
notificationConfiguration_topicStatus :: Lens.Lens' NotificationConfiguration (Core.Maybe Core.Text)
notificationConfiguration_topicStatus = Lens.lens (\NotificationConfiguration' {topicStatus} -> topicStatus) (\s@NotificationConfiguration' {} a -> s {topicStatus = a} :: NotificationConfiguration)

-- | The Amazon Resource Name (ARN) that identifies the topic.
notificationConfiguration_topicArn :: Lens.Lens' NotificationConfiguration (Core.Maybe Core.Text)
notificationConfiguration_topicArn = Lens.lens (\NotificationConfiguration' {topicArn} -> topicArn) (\s@NotificationConfiguration' {} a -> s {topicArn = a} :: NotificationConfiguration)

instance Core.FromXML NotificationConfiguration where
  parseXML x =
    NotificationConfiguration'
      Core.<$> (x Core..@? "TopicStatus")
      Core.<*> (x Core..@? "TopicArn")

instance Core.Hashable NotificationConfiguration

instance Core.NFData NotificationConfiguration
