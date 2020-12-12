{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.EventSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.EventSubscription
  ( EventSubscription (..),

    -- * Smart constructor
    mkEventSubscription,

    -- * Lenses
    esStatus,
    esCustomerAWSId,
    esCustSubscriptionId,
    esSNSTopicARN,
    esEnabled,
    esSourceType,
    esSeverity,
    esSubscriptionCreationTime,
    esEventCategoriesList,
    esTags,
    esSourceIdsList,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.Tag

-- | Describes event subscriptions.
--
-- /See:/ 'mkEventSubscription' smart constructor.
data EventSubscription = EventSubscription'
  { status ::
      Lude.Maybe Lude.Text,
    customerAWSId :: Lude.Maybe Lude.Text,
    custSubscriptionId :: Lude.Maybe Lude.Text,
    snsTopicARN :: Lude.Maybe Lude.Text,
    enabled :: Lude.Maybe Lude.Bool,
    sourceType :: Lude.Maybe Lude.Text,
    severity :: Lude.Maybe Lude.Text,
    subscriptionCreationTime :: Lude.Maybe Lude.DateTime,
    eventCategoriesList :: Lude.Maybe [Lude.Text],
    tags :: Lude.Maybe [Tag],
    sourceIdsList :: Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EventSubscription' with the minimum fields required to make a request.
--
-- * 'custSubscriptionId' - The name of the Amazon Redshift event notification subscription.
-- * 'customerAWSId' - The AWS customer account associated with the Amazon Redshift event notification subscription.
-- * 'enabled' - A boolean value indicating whether the subscription is enabled; @true@ indicates that the subscription is enabled.
-- * 'eventCategoriesList' - The list of Amazon Redshift event categories specified in the event notification subscription.
--
-- Values: Configuration, Management, Monitoring, Security
-- * 'severity' - The event severity specified in the Amazon Redshift event notification subscription.
--
-- Values: ERROR, INFO
-- * 'snsTopicARN' - The Amazon Resource Name (ARN) of the Amazon SNS topic used by the event notification subscription.
-- * 'sourceIdsList' - A list of the sources that publish events to the Amazon Redshift event notification subscription.
-- * 'sourceType' - The source type of the events returned by the Amazon Redshift event notification, such as cluster, cluster-snapshot, cluster-parameter-group, cluster-security-group, or scheduled-action.
-- * 'status' - The status of the Amazon Redshift event notification subscription.
--
-- Constraints:
--
--     * Can be one of the following: active | no-permission | topic-not-exist
--
--
--     * The status "no-permission" indicates that Amazon Redshift no longer has permission to post to the Amazon SNS topic. The status "topic-not-exist" indicates that the topic was deleted after the subscription was created.
--
--
-- * 'subscriptionCreationTime' - The date and time the Amazon Redshift event notification subscription was created.
-- * 'tags' - The list of tags for the event subscription.
mkEventSubscription ::
  EventSubscription
mkEventSubscription =
  EventSubscription'
    { status = Lude.Nothing,
      customerAWSId = Lude.Nothing,
      custSubscriptionId = Lude.Nothing,
      snsTopicARN = Lude.Nothing,
      enabled = Lude.Nothing,
      sourceType = Lude.Nothing,
      severity = Lude.Nothing,
      subscriptionCreationTime = Lude.Nothing,
      eventCategoriesList = Lude.Nothing,
      tags = Lude.Nothing,
      sourceIdsList = Lude.Nothing
    }

-- | The status of the Amazon Redshift event notification subscription.
--
-- Constraints:
--
--     * Can be one of the following: active | no-permission | topic-not-exist
--
--
--     * The status "no-permission" indicates that Amazon Redshift no longer has permission to post to the Amazon SNS topic. The status "topic-not-exist" indicates that the topic was deleted after the subscription was created.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esStatus :: Lens.Lens' EventSubscription (Lude.Maybe Lude.Text)
esStatus = Lens.lens (status :: EventSubscription -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: EventSubscription)
{-# DEPRECATED esStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The AWS customer account associated with the Amazon Redshift event notification subscription.
--
-- /Note:/ Consider using 'customerAWSId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esCustomerAWSId :: Lens.Lens' EventSubscription (Lude.Maybe Lude.Text)
esCustomerAWSId = Lens.lens (customerAWSId :: EventSubscription -> Lude.Maybe Lude.Text) (\s a -> s {customerAWSId = a} :: EventSubscription)
{-# DEPRECATED esCustomerAWSId "Use generic-lens or generic-optics with 'customerAWSId' instead." #-}

-- | The name of the Amazon Redshift event notification subscription.
--
-- /Note:/ Consider using 'custSubscriptionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esCustSubscriptionId :: Lens.Lens' EventSubscription (Lude.Maybe Lude.Text)
esCustSubscriptionId = Lens.lens (custSubscriptionId :: EventSubscription -> Lude.Maybe Lude.Text) (\s a -> s {custSubscriptionId = a} :: EventSubscription)
{-# DEPRECATED esCustSubscriptionId "Use generic-lens or generic-optics with 'custSubscriptionId' instead." #-}

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic used by the event notification subscription.
--
-- /Note:/ Consider using 'snsTopicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esSNSTopicARN :: Lens.Lens' EventSubscription (Lude.Maybe Lude.Text)
esSNSTopicARN = Lens.lens (snsTopicARN :: EventSubscription -> Lude.Maybe Lude.Text) (\s a -> s {snsTopicARN = a} :: EventSubscription)
{-# DEPRECATED esSNSTopicARN "Use generic-lens or generic-optics with 'snsTopicARN' instead." #-}

-- | A boolean value indicating whether the subscription is enabled; @true@ indicates that the subscription is enabled.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esEnabled :: Lens.Lens' EventSubscription (Lude.Maybe Lude.Bool)
esEnabled = Lens.lens (enabled :: EventSubscription -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: EventSubscription)
{-# DEPRECATED esEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The source type of the events returned by the Amazon Redshift event notification, such as cluster, cluster-snapshot, cluster-parameter-group, cluster-security-group, or scheduled-action.
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esSourceType :: Lens.Lens' EventSubscription (Lude.Maybe Lude.Text)
esSourceType = Lens.lens (sourceType :: EventSubscription -> Lude.Maybe Lude.Text) (\s a -> s {sourceType = a} :: EventSubscription)
{-# DEPRECATED esSourceType "Use generic-lens or generic-optics with 'sourceType' instead." #-}

-- | The event severity specified in the Amazon Redshift event notification subscription.
--
-- Values: ERROR, INFO
--
-- /Note:/ Consider using 'severity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esSeverity :: Lens.Lens' EventSubscription (Lude.Maybe Lude.Text)
esSeverity = Lens.lens (severity :: EventSubscription -> Lude.Maybe Lude.Text) (\s a -> s {severity = a} :: EventSubscription)
{-# DEPRECATED esSeverity "Use generic-lens or generic-optics with 'severity' instead." #-}

-- | The date and time the Amazon Redshift event notification subscription was created.
--
-- /Note:/ Consider using 'subscriptionCreationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esSubscriptionCreationTime :: Lens.Lens' EventSubscription (Lude.Maybe Lude.DateTime)
esSubscriptionCreationTime = Lens.lens (subscriptionCreationTime :: EventSubscription -> Lude.Maybe Lude.DateTime) (\s a -> s {subscriptionCreationTime = a} :: EventSubscription)
{-# DEPRECATED esSubscriptionCreationTime "Use generic-lens or generic-optics with 'subscriptionCreationTime' instead." #-}

-- | The list of Amazon Redshift event categories specified in the event notification subscription.
--
-- Values: Configuration, Management, Monitoring, Security
--
-- /Note:/ Consider using 'eventCategoriesList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esEventCategoriesList :: Lens.Lens' EventSubscription (Lude.Maybe [Lude.Text])
esEventCategoriesList = Lens.lens (eventCategoriesList :: EventSubscription -> Lude.Maybe [Lude.Text]) (\s a -> s {eventCategoriesList = a} :: EventSubscription)
{-# DEPRECATED esEventCategoriesList "Use generic-lens or generic-optics with 'eventCategoriesList' instead." #-}

-- | The list of tags for the event subscription.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esTags :: Lens.Lens' EventSubscription (Lude.Maybe [Tag])
esTags = Lens.lens (tags :: EventSubscription -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: EventSubscription)
{-# DEPRECATED esTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | A list of the sources that publish events to the Amazon Redshift event notification subscription.
--
-- /Note:/ Consider using 'sourceIdsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esSourceIdsList :: Lens.Lens' EventSubscription (Lude.Maybe [Lude.Text])
esSourceIdsList = Lens.lens (sourceIdsList :: EventSubscription -> Lude.Maybe [Lude.Text]) (\s a -> s {sourceIdsList = a} :: EventSubscription)
{-# DEPRECATED esSourceIdsList "Use generic-lens or generic-optics with 'sourceIdsList' instead." #-}

instance Lude.FromXML EventSubscription where
  parseXML x =
    EventSubscription'
      Lude.<$> (x Lude..@? "Status")
      Lude.<*> (x Lude..@? "CustomerAwsId")
      Lude.<*> (x Lude..@? "CustSubscriptionId")
      Lude.<*> (x Lude..@? "SnsTopicArn")
      Lude.<*> (x Lude..@? "Enabled")
      Lude.<*> (x Lude..@? "SourceType")
      Lude.<*> (x Lude..@? "Severity")
      Lude.<*> (x Lude..@? "SubscriptionCreationTime")
      Lude.<*> ( x Lude..@? "EventCategoriesList" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "EventCategory")
               )
      Lude.<*> ( x Lude..@? "Tags" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "Tag")
               )
      Lude.<*> ( x Lude..@? "SourceIdsList" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "SourceId")
               )
