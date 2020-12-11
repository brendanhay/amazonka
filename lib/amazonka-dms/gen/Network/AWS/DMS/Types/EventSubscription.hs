-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.EventSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.EventSubscription
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
    esSubscriptionCreationTime,
    esEventCategoriesList,
    esSourceIdsList,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an event notification subscription created by the @CreateEventSubscription@ operation.
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
    subscriptionCreationTime :: Lude.Maybe Lude.Text,
    eventCategoriesList :: Lude.Maybe [Lude.Text],
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
-- * 'custSubscriptionId' - The AWS DMS event notification subscription Id.
-- * 'customerAWSId' - The AWS customer account associated with the AWS DMS event notification subscription.
-- * 'enabled' - Boolean value that indicates if the event subscription is enabled.
-- * 'eventCategoriesList' - A lists of event categories.
-- * 'snsTopicARN' - The topic ARN of the AWS DMS event notification subscription.
-- * 'sourceIdsList' - A list of source Ids for the event subscription.
-- * 'sourceType' - The type of AWS DMS resource that generates events.
--
-- Valid values: replication-instance | replication-server | security-group | replication-task
-- * 'status' - The status of the AWS DMS event notification subscription.
--
-- Constraints:
-- Can be one of the following: creating | modifying | deleting | active | no-permission | topic-not-exist
-- The status "no-permission" indicates that AWS DMS no longer has permission to post to the SNS topic. The status "topic-not-exist" indicates that the topic was deleted after the subscription was created.
-- * 'subscriptionCreationTime' - The time the AWS DMS event notification subscription was created.
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
      subscriptionCreationTime = Lude.Nothing,
      eventCategoriesList = Lude.Nothing,
      sourceIdsList = Lude.Nothing
    }

-- | The status of the AWS DMS event notification subscription.
--
-- Constraints:
-- Can be one of the following: creating | modifying | deleting | active | no-permission | topic-not-exist
-- The status "no-permission" indicates that AWS DMS no longer has permission to post to the SNS topic. The status "topic-not-exist" indicates that the topic was deleted after the subscription was created.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esStatus :: Lens.Lens' EventSubscription (Lude.Maybe Lude.Text)
esStatus = Lens.lens (status :: EventSubscription -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: EventSubscription)
{-# DEPRECATED esStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The AWS customer account associated with the AWS DMS event notification subscription.
--
-- /Note:/ Consider using 'customerAWSId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esCustomerAWSId :: Lens.Lens' EventSubscription (Lude.Maybe Lude.Text)
esCustomerAWSId = Lens.lens (customerAWSId :: EventSubscription -> Lude.Maybe Lude.Text) (\s a -> s {customerAWSId = a} :: EventSubscription)
{-# DEPRECATED esCustomerAWSId "Use generic-lens or generic-optics with 'customerAWSId' instead." #-}

-- | The AWS DMS event notification subscription Id.
--
-- /Note:/ Consider using 'custSubscriptionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esCustSubscriptionId :: Lens.Lens' EventSubscription (Lude.Maybe Lude.Text)
esCustSubscriptionId = Lens.lens (custSubscriptionId :: EventSubscription -> Lude.Maybe Lude.Text) (\s a -> s {custSubscriptionId = a} :: EventSubscription)
{-# DEPRECATED esCustSubscriptionId "Use generic-lens or generic-optics with 'custSubscriptionId' instead." #-}

-- | The topic ARN of the AWS DMS event notification subscription.
--
-- /Note:/ Consider using 'snsTopicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esSNSTopicARN :: Lens.Lens' EventSubscription (Lude.Maybe Lude.Text)
esSNSTopicARN = Lens.lens (snsTopicARN :: EventSubscription -> Lude.Maybe Lude.Text) (\s a -> s {snsTopicARN = a} :: EventSubscription)
{-# DEPRECATED esSNSTopicARN "Use generic-lens or generic-optics with 'snsTopicARN' instead." #-}

-- | Boolean value that indicates if the event subscription is enabled.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esEnabled :: Lens.Lens' EventSubscription (Lude.Maybe Lude.Bool)
esEnabled = Lens.lens (enabled :: EventSubscription -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: EventSubscription)
{-# DEPRECATED esEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The type of AWS DMS resource that generates events.
--
-- Valid values: replication-instance | replication-server | security-group | replication-task
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esSourceType :: Lens.Lens' EventSubscription (Lude.Maybe Lude.Text)
esSourceType = Lens.lens (sourceType :: EventSubscription -> Lude.Maybe Lude.Text) (\s a -> s {sourceType = a} :: EventSubscription)
{-# DEPRECATED esSourceType "Use generic-lens or generic-optics with 'sourceType' instead." #-}

-- | The time the AWS DMS event notification subscription was created.
--
-- /Note:/ Consider using 'subscriptionCreationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esSubscriptionCreationTime :: Lens.Lens' EventSubscription (Lude.Maybe Lude.Text)
esSubscriptionCreationTime = Lens.lens (subscriptionCreationTime :: EventSubscription -> Lude.Maybe Lude.Text) (\s a -> s {subscriptionCreationTime = a} :: EventSubscription)
{-# DEPRECATED esSubscriptionCreationTime "Use generic-lens or generic-optics with 'subscriptionCreationTime' instead." #-}

-- | A lists of event categories.
--
-- /Note:/ Consider using 'eventCategoriesList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esEventCategoriesList :: Lens.Lens' EventSubscription (Lude.Maybe [Lude.Text])
esEventCategoriesList = Lens.lens (eventCategoriesList :: EventSubscription -> Lude.Maybe [Lude.Text]) (\s a -> s {eventCategoriesList = a} :: EventSubscription)
{-# DEPRECATED esEventCategoriesList "Use generic-lens or generic-optics with 'eventCategoriesList' instead." #-}

-- | A list of source Ids for the event subscription.
--
-- /Note:/ Consider using 'sourceIdsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esSourceIdsList :: Lens.Lens' EventSubscription (Lude.Maybe [Lude.Text])
esSourceIdsList = Lens.lens (sourceIdsList :: EventSubscription -> Lude.Maybe [Lude.Text]) (\s a -> s {sourceIdsList = a} :: EventSubscription)
{-# DEPRECATED esSourceIdsList "Use generic-lens or generic-optics with 'sourceIdsList' instead." #-}

instance Lude.FromJSON EventSubscription where
  parseJSON =
    Lude.withObject
      "EventSubscription"
      ( \x ->
          EventSubscription'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "CustomerAwsId")
            Lude.<*> (x Lude..:? "CustSubscriptionId")
            Lude.<*> (x Lude..:? "SnsTopicArn")
            Lude.<*> (x Lude..:? "Enabled")
            Lude.<*> (x Lude..:? "SourceType")
            Lude.<*> (x Lude..:? "SubscriptionCreationTime")
            Lude.<*> (x Lude..:? "EventCategoriesList" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "SourceIdsList" Lude..!= Lude.mempty)
      )
