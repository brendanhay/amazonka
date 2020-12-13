{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.EventSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.EventSubscription
  ( EventSubscription (..),

    -- * Smart constructor
    mkEventSubscription,

    -- * Lenses
    esStatus,
    esCustomerAWSId,
    esCustSubscriptionId,
    esSNSTopicARN,
    esEventSubscriptionARN,
    esEnabled,
    esSourceType,
    esSubscriptionCreationTime,
    esEventCategoriesList,
    esSourceIdsList,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the results of a successful invocation of the @DescribeEventSubscriptions@ action.
--
-- /See:/ 'mkEventSubscription' smart constructor.
data EventSubscription = EventSubscription'
  { -- | The status of the RDS event notification subscription.
    --
    -- Constraints:
    -- Can be one of the following: creating | modifying | deleting | active | no-permission | topic-not-exist
    -- The status "no-permission" indicates that RDS no longer has permission to post to the SNS topic. The status "topic-not-exist" indicates that the topic was deleted after the subscription was created.
    status :: Lude.Maybe Lude.Text,
    -- | The AWS customer account associated with the RDS event notification subscription.
    customerAWSId :: Lude.Maybe Lude.Text,
    -- | The RDS event notification subscription Id.
    custSubscriptionId :: Lude.Maybe Lude.Text,
    -- | The topic ARN of the RDS event notification subscription.
    snsTopicARN :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) for the event subscription.
    eventSubscriptionARN :: Lude.Maybe Lude.Text,
    -- | A Boolean value indicating if the subscription is enabled. True indicates the subscription is enabled.
    enabled :: Lude.Maybe Lude.Bool,
    -- | The source type for the RDS event notification subscription.
    sourceType :: Lude.Maybe Lude.Text,
    -- | The time the RDS event notification subscription was created.
    subscriptionCreationTime :: Lude.Maybe Lude.Text,
    -- | A list of event categories for the RDS event notification subscription.
    eventCategoriesList :: Lude.Maybe [Lude.Text],
    -- | A list of source IDs for the RDS event notification subscription.
    sourceIdsList :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EventSubscription' with the minimum fields required to make a request.
--
-- * 'status' - The status of the RDS event notification subscription.
--
-- Constraints:
-- Can be one of the following: creating | modifying | deleting | active | no-permission | topic-not-exist
-- The status "no-permission" indicates that RDS no longer has permission to post to the SNS topic. The status "topic-not-exist" indicates that the topic was deleted after the subscription was created.
-- * 'customerAWSId' - The AWS customer account associated with the RDS event notification subscription.
-- * 'custSubscriptionId' - The RDS event notification subscription Id.
-- * 'snsTopicARN' - The topic ARN of the RDS event notification subscription.
-- * 'eventSubscriptionARN' - The Amazon Resource Name (ARN) for the event subscription.
-- * 'enabled' - A Boolean value indicating if the subscription is enabled. True indicates the subscription is enabled.
-- * 'sourceType' - The source type for the RDS event notification subscription.
-- * 'subscriptionCreationTime' - The time the RDS event notification subscription was created.
-- * 'eventCategoriesList' - A list of event categories for the RDS event notification subscription.
-- * 'sourceIdsList' - A list of source IDs for the RDS event notification subscription.
mkEventSubscription ::
  EventSubscription
mkEventSubscription =
  EventSubscription'
    { status = Lude.Nothing,
      customerAWSId = Lude.Nothing,
      custSubscriptionId = Lude.Nothing,
      snsTopicARN = Lude.Nothing,
      eventSubscriptionARN = Lude.Nothing,
      enabled = Lude.Nothing,
      sourceType = Lude.Nothing,
      subscriptionCreationTime = Lude.Nothing,
      eventCategoriesList = Lude.Nothing,
      sourceIdsList = Lude.Nothing
    }

-- | The status of the RDS event notification subscription.
--
-- Constraints:
-- Can be one of the following: creating | modifying | deleting | active | no-permission | topic-not-exist
-- The status "no-permission" indicates that RDS no longer has permission to post to the SNS topic. The status "topic-not-exist" indicates that the topic was deleted after the subscription was created.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esStatus :: Lens.Lens' EventSubscription (Lude.Maybe Lude.Text)
esStatus = Lens.lens (status :: EventSubscription -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: EventSubscription)
{-# DEPRECATED esStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The AWS customer account associated with the RDS event notification subscription.
--
-- /Note:/ Consider using 'customerAWSId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esCustomerAWSId :: Lens.Lens' EventSubscription (Lude.Maybe Lude.Text)
esCustomerAWSId = Lens.lens (customerAWSId :: EventSubscription -> Lude.Maybe Lude.Text) (\s a -> s {customerAWSId = a} :: EventSubscription)
{-# DEPRECATED esCustomerAWSId "Use generic-lens or generic-optics with 'customerAWSId' instead." #-}

-- | The RDS event notification subscription Id.
--
-- /Note:/ Consider using 'custSubscriptionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esCustSubscriptionId :: Lens.Lens' EventSubscription (Lude.Maybe Lude.Text)
esCustSubscriptionId = Lens.lens (custSubscriptionId :: EventSubscription -> Lude.Maybe Lude.Text) (\s a -> s {custSubscriptionId = a} :: EventSubscription)
{-# DEPRECATED esCustSubscriptionId "Use generic-lens or generic-optics with 'custSubscriptionId' instead." #-}

-- | The topic ARN of the RDS event notification subscription.
--
-- /Note:/ Consider using 'snsTopicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esSNSTopicARN :: Lens.Lens' EventSubscription (Lude.Maybe Lude.Text)
esSNSTopicARN = Lens.lens (snsTopicARN :: EventSubscription -> Lude.Maybe Lude.Text) (\s a -> s {snsTopicARN = a} :: EventSubscription)
{-# DEPRECATED esSNSTopicARN "Use generic-lens or generic-optics with 'snsTopicARN' instead." #-}

-- | The Amazon Resource Name (ARN) for the event subscription.
--
-- /Note:/ Consider using 'eventSubscriptionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esEventSubscriptionARN :: Lens.Lens' EventSubscription (Lude.Maybe Lude.Text)
esEventSubscriptionARN = Lens.lens (eventSubscriptionARN :: EventSubscription -> Lude.Maybe Lude.Text) (\s a -> s {eventSubscriptionARN = a} :: EventSubscription)
{-# DEPRECATED esEventSubscriptionARN "Use generic-lens or generic-optics with 'eventSubscriptionARN' instead." #-}

-- | A Boolean value indicating if the subscription is enabled. True indicates the subscription is enabled.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esEnabled :: Lens.Lens' EventSubscription (Lude.Maybe Lude.Bool)
esEnabled = Lens.lens (enabled :: EventSubscription -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: EventSubscription)
{-# DEPRECATED esEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The source type for the RDS event notification subscription.
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esSourceType :: Lens.Lens' EventSubscription (Lude.Maybe Lude.Text)
esSourceType = Lens.lens (sourceType :: EventSubscription -> Lude.Maybe Lude.Text) (\s a -> s {sourceType = a} :: EventSubscription)
{-# DEPRECATED esSourceType "Use generic-lens or generic-optics with 'sourceType' instead." #-}

-- | The time the RDS event notification subscription was created.
--
-- /Note:/ Consider using 'subscriptionCreationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esSubscriptionCreationTime :: Lens.Lens' EventSubscription (Lude.Maybe Lude.Text)
esSubscriptionCreationTime = Lens.lens (subscriptionCreationTime :: EventSubscription -> Lude.Maybe Lude.Text) (\s a -> s {subscriptionCreationTime = a} :: EventSubscription)
{-# DEPRECATED esSubscriptionCreationTime "Use generic-lens or generic-optics with 'subscriptionCreationTime' instead." #-}

-- | A list of event categories for the RDS event notification subscription.
--
-- /Note:/ Consider using 'eventCategoriesList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esEventCategoriesList :: Lens.Lens' EventSubscription (Lude.Maybe [Lude.Text])
esEventCategoriesList = Lens.lens (eventCategoriesList :: EventSubscription -> Lude.Maybe [Lude.Text]) (\s a -> s {eventCategoriesList = a} :: EventSubscription)
{-# DEPRECATED esEventCategoriesList "Use generic-lens or generic-optics with 'eventCategoriesList' instead." #-}

-- | A list of source IDs for the RDS event notification subscription.
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
      Lude.<*> (x Lude..@? "EventSubscriptionArn")
      Lude.<*> (x Lude..@? "Enabled")
      Lude.<*> (x Lude..@? "SourceType")
      Lude.<*> (x Lude..@? "SubscriptionCreationTime")
      Lude.<*> ( x Lude..@? "EventCategoriesList" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "EventCategory")
               )
      Lude.<*> ( x Lude..@? "SourceIdsList" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "SourceId")
               )
