{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.EventDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.EventDestination
  ( EventDestination (..),

    -- * Smart constructor
    mkEventDestination,

    -- * Lenses
    edMatchingEventTypes,
    edEnabled,
    edKinesisFirehoseDestination,
    edCloudWatchDestination,
    edName,
    edSNSDestination,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SES.Types.CloudWatchDestination
import Network.AWS.SES.Types.EventType
import Network.AWS.SES.Types.KinesisFirehoseDestination
import Network.AWS.SES.Types.SNSDestination

-- | Contains information about the event destination that the specified email sending events will be published to.
--
-- Event destinations are associated with configuration sets, which enable you to publish email sending events to Amazon CloudWatch, Amazon Kinesis Firehose, or Amazon Simple Notification Service (Amazon SNS). For information about using configuration sets, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkEventDestination' smart constructor.
data EventDestination = EventDestination'
  { -- | The type of email sending events to publish to the event destination.
    matchingEventTypes :: [EventType],
    -- | Sets whether Amazon SES publishes events to this destination when you send an email with the associated configuration set. Set to @true@ to enable publishing to this destination; set to @false@ to prevent publishing to this destination. The default value is @false@ .
    enabled :: Lude.Maybe Lude.Bool,
    -- | An object that contains the delivery stream ARN and the IAM role ARN associated with an Amazon Kinesis Firehose event destination.
    kinesisFirehoseDestination :: Lude.Maybe KinesisFirehoseDestination,
    -- | An object that contains the names, default values, and sources of the dimensions associated with an Amazon CloudWatch event destination.
    cloudWatchDestination :: Lude.Maybe CloudWatchDestination,
    -- | The name of the event destination. The name must:
    --
    --
    --     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).
    --
    --
    --     * Contain less than 64 characters.
    name :: Lude.Text,
    -- | An object that contains the topic ARN associated with an Amazon Simple Notification Service (Amazon SNS) event destination.
    snsDestination :: Lude.Maybe SNSDestination
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EventDestination' with the minimum fields required to make a request.
--
-- * 'matchingEventTypes' - The type of email sending events to publish to the event destination.
-- * 'enabled' - Sets whether Amazon SES publishes events to this destination when you send an email with the associated configuration set. Set to @true@ to enable publishing to this destination; set to @false@ to prevent publishing to this destination. The default value is @false@ .
-- * 'kinesisFirehoseDestination' - An object that contains the delivery stream ARN and the IAM role ARN associated with an Amazon Kinesis Firehose event destination.
-- * 'cloudWatchDestination' - An object that contains the names, default values, and sources of the dimensions associated with an Amazon CloudWatch event destination.
-- * 'name' - The name of the event destination. The name must:
--
--
--     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).
--
--
--     * Contain less than 64 characters.
--
--
-- * 'snsDestination' - An object that contains the topic ARN associated with an Amazon Simple Notification Service (Amazon SNS) event destination.
mkEventDestination ::
  -- | 'name'
  Lude.Text ->
  EventDestination
mkEventDestination pName_ =
  EventDestination'
    { matchingEventTypes = Lude.mempty,
      enabled = Lude.Nothing,
      kinesisFirehoseDestination = Lude.Nothing,
      cloudWatchDestination = Lude.Nothing,
      name = pName_,
      snsDestination = Lude.Nothing
    }

-- | The type of email sending events to publish to the event destination.
--
-- /Note:/ Consider using 'matchingEventTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edMatchingEventTypes :: Lens.Lens' EventDestination [EventType]
edMatchingEventTypes = Lens.lens (matchingEventTypes :: EventDestination -> [EventType]) (\s a -> s {matchingEventTypes = a} :: EventDestination)
{-# DEPRECATED edMatchingEventTypes "Use generic-lens or generic-optics with 'matchingEventTypes' instead." #-}

-- | Sets whether Amazon SES publishes events to this destination when you send an email with the associated configuration set. Set to @true@ to enable publishing to this destination; set to @false@ to prevent publishing to this destination. The default value is @false@ .
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edEnabled :: Lens.Lens' EventDestination (Lude.Maybe Lude.Bool)
edEnabled = Lens.lens (enabled :: EventDestination -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: EventDestination)
{-# DEPRECATED edEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | An object that contains the delivery stream ARN and the IAM role ARN associated with an Amazon Kinesis Firehose event destination.
--
-- /Note:/ Consider using 'kinesisFirehoseDestination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edKinesisFirehoseDestination :: Lens.Lens' EventDestination (Lude.Maybe KinesisFirehoseDestination)
edKinesisFirehoseDestination = Lens.lens (kinesisFirehoseDestination :: EventDestination -> Lude.Maybe KinesisFirehoseDestination) (\s a -> s {kinesisFirehoseDestination = a} :: EventDestination)
{-# DEPRECATED edKinesisFirehoseDestination "Use generic-lens or generic-optics with 'kinesisFirehoseDestination' instead." #-}

-- | An object that contains the names, default values, and sources of the dimensions associated with an Amazon CloudWatch event destination.
--
-- /Note:/ Consider using 'cloudWatchDestination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edCloudWatchDestination :: Lens.Lens' EventDestination (Lude.Maybe CloudWatchDestination)
edCloudWatchDestination = Lens.lens (cloudWatchDestination :: EventDestination -> Lude.Maybe CloudWatchDestination) (\s a -> s {cloudWatchDestination = a} :: EventDestination)
{-# DEPRECATED edCloudWatchDestination "Use generic-lens or generic-optics with 'cloudWatchDestination' instead." #-}

-- | The name of the event destination. The name must:
--
--
--     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).
--
--
--     * Contain less than 64 characters.
--
--
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edName :: Lens.Lens' EventDestination Lude.Text
edName = Lens.lens (name :: EventDestination -> Lude.Text) (\s a -> s {name = a} :: EventDestination)
{-# DEPRECATED edName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | An object that contains the topic ARN associated with an Amazon Simple Notification Service (Amazon SNS) event destination.
--
-- /Note:/ Consider using 'snsDestination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edSNSDestination :: Lens.Lens' EventDestination (Lude.Maybe SNSDestination)
edSNSDestination = Lens.lens (snsDestination :: EventDestination -> Lude.Maybe SNSDestination) (\s a -> s {snsDestination = a} :: EventDestination)
{-# DEPRECATED edSNSDestination "Use generic-lens or generic-optics with 'snsDestination' instead." #-}

instance Lude.FromXML EventDestination where
  parseXML x =
    EventDestination'
      Lude.<$> ( x Lude..@? "MatchingEventTypes" Lude..!@ Lude.mempty
                   Lude.>>= Lude.parseXMLList "member"
               )
      Lude.<*> (x Lude..@? "Enabled")
      Lude.<*> (x Lude..@? "KinesisFirehoseDestination")
      Lude.<*> (x Lude..@? "CloudWatchDestination")
      Lude.<*> (x Lude..@ "Name")
      Lude.<*> (x Lude..@? "SNSDestination")

instance Lude.ToQuery EventDestination where
  toQuery EventDestination' {..} =
    Lude.mconcat
      [ "MatchingEventTypes"
          Lude.=: Lude.toQueryList "member" matchingEventTypes,
        "Enabled" Lude.=: enabled,
        "KinesisFirehoseDestination" Lude.=: kinesisFirehoseDestination,
        "CloudWatchDestination" Lude.=: cloudWatchDestination,
        "Name" Lude.=: name,
        "SNSDestination" Lude.=: snsDestination
      ]
