{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.Types.NotificationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DAX.Types.NotificationConfiguration
  ( NotificationConfiguration (..)
  -- * Smart constructor
  , mkNotificationConfiguration
  -- * Lenses
  , ncTopicArn
  , ncTopicStatus
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a notification topic and its status. Notification topics are used for publishing DAX events to subscribers using Amazon Simple Notification Service (SNS).
--
-- /See:/ 'mkNotificationConfiguration' smart constructor.
data NotificationConfiguration = NotificationConfiguration'
  { topicArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) that identifies the topic. 
  , topicStatus :: Core.Maybe Core.Text
    -- ^ The current state of the topic.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NotificationConfiguration' value with any optional fields omitted.
mkNotificationConfiguration
    :: NotificationConfiguration
mkNotificationConfiguration
  = NotificationConfiguration'{topicArn = Core.Nothing,
                               topicStatus = Core.Nothing}

-- | The Amazon Resource Name (ARN) that identifies the topic. 
--
-- /Note:/ Consider using 'topicArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncTopicArn :: Lens.Lens' NotificationConfiguration (Core.Maybe Core.Text)
ncTopicArn = Lens.field @"topicArn"
{-# INLINEABLE ncTopicArn #-}
{-# DEPRECATED topicArn "Use generic-lens or generic-optics with 'topicArn' instead"  #-}

-- | The current state of the topic.
--
-- /Note:/ Consider using 'topicStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncTopicStatus :: Lens.Lens' NotificationConfiguration (Core.Maybe Core.Text)
ncTopicStatus = Lens.field @"topicStatus"
{-# INLINEABLE ncTopicStatus #-}
{-# DEPRECATED topicStatus "Use generic-lens or generic-optics with 'topicStatus' instead"  #-}

instance Core.FromJSON NotificationConfiguration where
        parseJSON
          = Core.withObject "NotificationConfiguration" Core.$
              \ x ->
                NotificationConfiguration' Core.<$>
                  (x Core..:? "TopicArn") Core.<*> x Core..:? "TopicStatus"
