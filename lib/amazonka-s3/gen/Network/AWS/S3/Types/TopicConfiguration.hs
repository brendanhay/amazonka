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
    tcTopicARN,
    tcEvents,
    tcId,
    tcFilter,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Event
import Network.AWS.S3.Types.NotificationConfigurationFilter

-- | A container for specifying the configuration for publication of messages to an Amazon Simple Notification Service (Amazon SNS) topic when Amazon S3 detects specified events.
--
-- /See:/ 'mkTopicConfiguration' smart constructor.
data TopicConfiguration = TopicConfiguration'
  { -- | The Amazon Resource Name (ARN) of the Amazon SNS topic to which Amazon S3 publishes a message when it detects events of the specified type.
    topicARN :: Lude.Text,
    -- | The Amazon S3 bucket event about which to send notifications. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Supported Event Types> in the /Amazon Simple Storage Service Developer Guide/ .
    events :: [Event],
    id :: Lude.Maybe Lude.Text,
    filter :: Lude.Maybe NotificationConfigurationFilter
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TopicConfiguration' with the minimum fields required to make a request.
--
-- * 'topicARN' - The Amazon Resource Name (ARN) of the Amazon SNS topic to which Amazon S3 publishes a message when it detects events of the specified type.
-- * 'events' - The Amazon S3 bucket event about which to send notifications. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Supported Event Types> in the /Amazon Simple Storage Service Developer Guide/ .
-- * 'id' -
-- * 'filter' -
mkTopicConfiguration ::
  -- | 'topicARN'
  Lude.Text ->
  TopicConfiguration
mkTopicConfiguration pTopicARN_ =
  TopicConfiguration'
    { topicARN = pTopicARN_,
      events = Lude.mempty,
      id = Lude.Nothing,
      filter = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic to which Amazon S3 publishes a message when it detects events of the specified type.
--
-- /Note:/ Consider using 'topicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcTopicARN :: Lens.Lens' TopicConfiguration Lude.Text
tcTopicARN = Lens.lens (topicARN :: TopicConfiguration -> Lude.Text) (\s a -> s {topicARN = a} :: TopicConfiguration)
{-# DEPRECATED tcTopicARN "Use generic-lens or generic-optics with 'topicARN' instead." #-}

-- | The Amazon S3 bucket event about which to send notifications. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Supported Event Types> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'events' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcEvents :: Lens.Lens' TopicConfiguration [Event]
tcEvents = Lens.lens (events :: TopicConfiguration -> [Event]) (\s a -> s {events = a} :: TopicConfiguration)
{-# DEPRECATED tcEvents "Use generic-lens or generic-optics with 'events' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcId :: Lens.Lens' TopicConfiguration (Lude.Maybe Lude.Text)
tcId = Lens.lens (id :: TopicConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: TopicConfiguration)
{-# DEPRECATED tcId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcFilter :: Lens.Lens' TopicConfiguration (Lude.Maybe NotificationConfigurationFilter)
tcFilter = Lens.lens (filter :: TopicConfiguration -> Lude.Maybe NotificationConfigurationFilter) (\s a -> s {filter = a} :: TopicConfiguration)
{-# DEPRECATED tcFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

instance Lude.FromXML TopicConfiguration where
  parseXML x =
    TopicConfiguration'
      Lude.<$> (x Lude..@ "Topic")
      Lude.<*> (Lude.parseXMLList "Event" x)
      Lude.<*> (x Lude..@? "Id")
      Lude.<*> (x Lude..@? "Filter")

instance Lude.ToXML TopicConfiguration where
  toXML TopicConfiguration' {..} =
    Lude.mconcat
      [ "Topic" Lude.@= topicARN,
        Lude.toXMLList "Event" events,
        "Id" Lude.@= id,
        "Filter" Lude.@= filter
      ]
