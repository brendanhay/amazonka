{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.TopicConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.TopicConfiguration
  ( TopicConfiguration (..),

    -- * Smart constructor
    mkTopicConfiguration,

    -- * Lenses
    tcTopicArn,
    tcEvents,
    tcFilter,
    tcId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.Event as Types
import qualified Network.AWS.S3.Types.Id as Types
import qualified Network.AWS.S3.Types.NotificationConfigurationFilter as Types
import qualified Network.AWS.S3.Types.TopicArn as Types

-- | A container for specifying the configuration for publication of messages to an Amazon Simple Notification Service (Amazon SNS) topic when Amazon S3 detects specified events.
--
-- /See:/ 'mkTopicConfiguration' smart constructor.
data TopicConfiguration = TopicConfiguration'
  { -- | The Amazon Resource Name (ARN) of the Amazon SNS topic to which Amazon S3 publishes a message when it detects events of the specified type.
    topicArn :: Types.TopicArn,
    -- | The Amazon S3 bucket event about which to send notifications. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Supported Event Types> in the /Amazon Simple Storage Service Developer Guide/ .
    events :: [Types.Event],
    filter :: Core.Maybe Types.NotificationConfigurationFilter,
    id :: Core.Maybe Types.Id
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TopicConfiguration' value with any optional fields omitted.
mkTopicConfiguration ::
  -- | 'topicArn'
  Types.TopicArn ->
  TopicConfiguration
mkTopicConfiguration topicArn =
  TopicConfiguration'
    { topicArn,
      events = Core.mempty,
      filter = Core.Nothing,
      id = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic to which Amazon S3 publishes a message when it detects events of the specified type.
--
-- /Note:/ Consider using 'topicArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcTopicArn :: Lens.Lens' TopicConfiguration Types.TopicArn
tcTopicArn = Lens.field @"topicArn"
{-# DEPRECATED tcTopicArn "Use generic-lens or generic-optics with 'topicArn' instead." #-}

-- | The Amazon S3 bucket event about which to send notifications. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Supported Event Types> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'events' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcEvents :: Lens.Lens' TopicConfiguration [Types.Event]
tcEvents = Lens.field @"events"
{-# DEPRECATED tcEvents "Use generic-lens or generic-optics with 'events' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcFilter :: Lens.Lens' TopicConfiguration (Core.Maybe Types.NotificationConfigurationFilter)
tcFilter = Lens.field @"filter"
{-# DEPRECATED tcFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcId :: Lens.Lens' TopicConfiguration (Core.Maybe Types.Id)
tcId = Lens.field @"id"
{-# DEPRECATED tcId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Core.ToXML TopicConfiguration where
  toXML TopicConfiguration {..} =
    Core.toXMLNode "Topic" topicArn
      Core.<> Core.toXMLList "Event" events
      Core.<> Core.toXMLNode "Filter" Core.<$> filter
      Core.<> Core.toXMLNode "Id" Core.<$> id

instance Core.FromXML TopicConfiguration where
  parseXML x =
    TopicConfiguration'
      Core.<$> (x Core..@ "Topic")
      Core.<*> (x Core..@? "Event" Core..@! Core.mempty)
      Core.<*> (x Core..@? "Filter")
      Core.<*> (x Core..@? "Id")
