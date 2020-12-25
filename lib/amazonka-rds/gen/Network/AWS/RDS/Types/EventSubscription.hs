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
    esCustSubscriptionId,
    esCustomerAwsId,
    esEnabled,
    esEventCategoriesList,
    esEventSubscriptionArn,
    esSnsTopicArn,
    esSourceIdsList,
    esSourceType,
    esStatus,
    esSubscriptionCreationTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.String as Types

-- | Contains the results of a successful invocation of the @DescribeEventSubscriptions@ action.
--
-- /See:/ 'mkEventSubscription' smart constructor.
data EventSubscription = EventSubscription'
  { -- | The RDS event notification subscription Id.
    custSubscriptionId :: Core.Maybe Types.String,
    -- | The AWS customer account associated with the RDS event notification subscription.
    customerAwsId :: Core.Maybe Types.String,
    -- | A Boolean value indicating if the subscription is enabled. True indicates the subscription is enabled.
    enabled :: Core.Maybe Core.Bool,
    -- | A list of event categories for the RDS event notification subscription.
    eventCategoriesList :: Core.Maybe [Types.String],
    -- | The Amazon Resource Name (ARN) for the event subscription.
    eventSubscriptionArn :: Core.Maybe Types.String,
    -- | The topic ARN of the RDS event notification subscription.
    snsTopicArn :: Core.Maybe Types.String,
    -- | A list of source IDs for the RDS event notification subscription.
    sourceIdsList :: Core.Maybe [Types.String],
    -- | The source type for the RDS event notification subscription.
    sourceType :: Core.Maybe Types.String,
    -- | The status of the RDS event notification subscription.
    --
    -- Constraints:
    -- Can be one of the following: creating | modifying | deleting | active | no-permission | topic-not-exist
    -- The status "no-permission" indicates that RDS no longer has permission to post to the SNS topic. The status "topic-not-exist" indicates that the topic was deleted after the subscription was created.
    status :: Core.Maybe Types.String,
    -- | The time the RDS event notification subscription was created.
    subscriptionCreationTime :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EventSubscription' value with any optional fields omitted.
mkEventSubscription ::
  EventSubscription
mkEventSubscription =
  EventSubscription'
    { custSubscriptionId = Core.Nothing,
      customerAwsId = Core.Nothing,
      enabled = Core.Nothing,
      eventCategoriesList = Core.Nothing,
      eventSubscriptionArn = Core.Nothing,
      snsTopicArn = Core.Nothing,
      sourceIdsList = Core.Nothing,
      sourceType = Core.Nothing,
      status = Core.Nothing,
      subscriptionCreationTime = Core.Nothing
    }

-- | The RDS event notification subscription Id.
--
-- /Note:/ Consider using 'custSubscriptionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esCustSubscriptionId :: Lens.Lens' EventSubscription (Core.Maybe Types.String)
esCustSubscriptionId = Lens.field @"custSubscriptionId"
{-# DEPRECATED esCustSubscriptionId "Use generic-lens or generic-optics with 'custSubscriptionId' instead." #-}

-- | The AWS customer account associated with the RDS event notification subscription.
--
-- /Note:/ Consider using 'customerAwsId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esCustomerAwsId :: Lens.Lens' EventSubscription (Core.Maybe Types.String)
esCustomerAwsId = Lens.field @"customerAwsId"
{-# DEPRECATED esCustomerAwsId "Use generic-lens or generic-optics with 'customerAwsId' instead." #-}

-- | A Boolean value indicating if the subscription is enabled. True indicates the subscription is enabled.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esEnabled :: Lens.Lens' EventSubscription (Core.Maybe Core.Bool)
esEnabled = Lens.field @"enabled"
{-# DEPRECATED esEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | A list of event categories for the RDS event notification subscription.
--
-- /Note:/ Consider using 'eventCategoriesList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esEventCategoriesList :: Lens.Lens' EventSubscription (Core.Maybe [Types.String])
esEventCategoriesList = Lens.field @"eventCategoriesList"
{-# DEPRECATED esEventCategoriesList "Use generic-lens or generic-optics with 'eventCategoriesList' instead." #-}

-- | The Amazon Resource Name (ARN) for the event subscription.
--
-- /Note:/ Consider using 'eventSubscriptionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esEventSubscriptionArn :: Lens.Lens' EventSubscription (Core.Maybe Types.String)
esEventSubscriptionArn = Lens.field @"eventSubscriptionArn"
{-# DEPRECATED esEventSubscriptionArn "Use generic-lens or generic-optics with 'eventSubscriptionArn' instead." #-}

-- | The topic ARN of the RDS event notification subscription.
--
-- /Note:/ Consider using 'snsTopicArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esSnsTopicArn :: Lens.Lens' EventSubscription (Core.Maybe Types.String)
esSnsTopicArn = Lens.field @"snsTopicArn"
{-# DEPRECATED esSnsTopicArn "Use generic-lens or generic-optics with 'snsTopicArn' instead." #-}

-- | A list of source IDs for the RDS event notification subscription.
--
-- /Note:/ Consider using 'sourceIdsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esSourceIdsList :: Lens.Lens' EventSubscription (Core.Maybe [Types.String])
esSourceIdsList = Lens.field @"sourceIdsList"
{-# DEPRECATED esSourceIdsList "Use generic-lens or generic-optics with 'sourceIdsList' instead." #-}

-- | The source type for the RDS event notification subscription.
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esSourceType :: Lens.Lens' EventSubscription (Core.Maybe Types.String)
esSourceType = Lens.field @"sourceType"
{-# DEPRECATED esSourceType "Use generic-lens or generic-optics with 'sourceType' instead." #-}

-- | The status of the RDS event notification subscription.
--
-- Constraints:
-- Can be one of the following: creating | modifying | deleting | active | no-permission | topic-not-exist
-- The status "no-permission" indicates that RDS no longer has permission to post to the SNS topic. The status "topic-not-exist" indicates that the topic was deleted after the subscription was created.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esStatus :: Lens.Lens' EventSubscription (Core.Maybe Types.String)
esStatus = Lens.field @"status"
{-# DEPRECATED esStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The time the RDS event notification subscription was created.
--
-- /Note:/ Consider using 'subscriptionCreationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esSubscriptionCreationTime :: Lens.Lens' EventSubscription (Core.Maybe Types.String)
esSubscriptionCreationTime = Lens.field @"subscriptionCreationTime"
{-# DEPRECATED esSubscriptionCreationTime "Use generic-lens or generic-optics with 'subscriptionCreationTime' instead." #-}

instance Core.FromXML EventSubscription where
  parseXML x =
    EventSubscription'
      Core.<$> (x Core..@? "CustSubscriptionId")
      Core.<*> (x Core..@? "CustomerAwsId")
      Core.<*> (x Core..@? "Enabled")
      Core.<*> ( x Core..@? "EventCategoriesList"
                   Core..<@> Core.parseXMLList "EventCategory"
               )
      Core.<*> (x Core..@? "EventSubscriptionArn")
      Core.<*> (x Core..@? "SnsTopicArn")
      Core.<*> (x Core..@? "SourceIdsList" Core..<@> Core.parseXMLList "SourceId")
      Core.<*> (x Core..@? "SourceType")
      Core.<*> (x Core..@? "Status")
      Core.<*> (x Core..@? "SubscriptionCreationTime")
