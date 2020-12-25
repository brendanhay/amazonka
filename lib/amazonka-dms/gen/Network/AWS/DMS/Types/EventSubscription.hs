{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    esCustSubscriptionId,
    esCustomerAwsId,
    esEnabled,
    esEventCategoriesList,
    esSnsTopicArn,
    esSourceIdsList,
    esSourceType,
    esStatus,
    esSubscriptionCreationTime,
  )
where

import qualified Network.AWS.DMS.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an event notification subscription created by the @CreateEventSubscription@ operation.
--
-- /See:/ 'mkEventSubscription' smart constructor.
data EventSubscription = EventSubscription'
  { -- | The AWS DMS event notification subscription Id.
    custSubscriptionId :: Core.Maybe Types.String,
    -- | The AWS customer account associated with the AWS DMS event notification subscription.
    customerAwsId :: Core.Maybe Types.String,
    -- | Boolean value that indicates if the event subscription is enabled.
    enabled :: Core.Maybe Core.Bool,
    -- | A lists of event categories.
    eventCategoriesList :: Core.Maybe [Types.String],
    -- | The topic ARN of the AWS DMS event notification subscription.
    snsTopicArn :: Core.Maybe Types.String,
    -- | A list of source Ids for the event subscription.
    sourceIdsList :: Core.Maybe [Types.String],
    -- | The type of AWS DMS resource that generates events.
    --
    -- Valid values: replication-instance | replication-server | security-group | replication-task
    sourceType :: Core.Maybe Types.String,
    -- | The status of the AWS DMS event notification subscription.
    --
    -- Constraints:
    -- Can be one of the following: creating | modifying | deleting | active | no-permission | topic-not-exist
    -- The status "no-permission" indicates that AWS DMS no longer has permission to post to the SNS topic. The status "topic-not-exist" indicates that the topic was deleted after the subscription was created.
    status :: Core.Maybe Types.String,
    -- | The time the AWS DMS event notification subscription was created.
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
      snsTopicArn = Core.Nothing,
      sourceIdsList = Core.Nothing,
      sourceType = Core.Nothing,
      status = Core.Nothing,
      subscriptionCreationTime = Core.Nothing
    }

-- | The AWS DMS event notification subscription Id.
--
-- /Note:/ Consider using 'custSubscriptionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esCustSubscriptionId :: Lens.Lens' EventSubscription (Core.Maybe Types.String)
esCustSubscriptionId = Lens.field @"custSubscriptionId"
{-# DEPRECATED esCustSubscriptionId "Use generic-lens or generic-optics with 'custSubscriptionId' instead." #-}

-- | The AWS customer account associated with the AWS DMS event notification subscription.
--
-- /Note:/ Consider using 'customerAwsId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esCustomerAwsId :: Lens.Lens' EventSubscription (Core.Maybe Types.String)
esCustomerAwsId = Lens.field @"customerAwsId"
{-# DEPRECATED esCustomerAwsId "Use generic-lens or generic-optics with 'customerAwsId' instead." #-}

-- | Boolean value that indicates if the event subscription is enabled.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esEnabled :: Lens.Lens' EventSubscription (Core.Maybe Core.Bool)
esEnabled = Lens.field @"enabled"
{-# DEPRECATED esEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | A lists of event categories.
--
-- /Note:/ Consider using 'eventCategoriesList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esEventCategoriesList :: Lens.Lens' EventSubscription (Core.Maybe [Types.String])
esEventCategoriesList = Lens.field @"eventCategoriesList"
{-# DEPRECATED esEventCategoriesList "Use generic-lens or generic-optics with 'eventCategoriesList' instead." #-}

-- | The topic ARN of the AWS DMS event notification subscription.
--
-- /Note:/ Consider using 'snsTopicArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esSnsTopicArn :: Lens.Lens' EventSubscription (Core.Maybe Types.String)
esSnsTopicArn = Lens.field @"snsTopicArn"
{-# DEPRECATED esSnsTopicArn "Use generic-lens or generic-optics with 'snsTopicArn' instead." #-}

-- | A list of source Ids for the event subscription.
--
-- /Note:/ Consider using 'sourceIdsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esSourceIdsList :: Lens.Lens' EventSubscription (Core.Maybe [Types.String])
esSourceIdsList = Lens.field @"sourceIdsList"
{-# DEPRECATED esSourceIdsList "Use generic-lens or generic-optics with 'sourceIdsList' instead." #-}

-- | The type of AWS DMS resource that generates events.
--
-- Valid values: replication-instance | replication-server | security-group | replication-task
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esSourceType :: Lens.Lens' EventSubscription (Core.Maybe Types.String)
esSourceType = Lens.field @"sourceType"
{-# DEPRECATED esSourceType "Use generic-lens or generic-optics with 'sourceType' instead." #-}

-- | The status of the AWS DMS event notification subscription.
--
-- Constraints:
-- Can be one of the following: creating | modifying | deleting | active | no-permission | topic-not-exist
-- The status "no-permission" indicates that AWS DMS no longer has permission to post to the SNS topic. The status "topic-not-exist" indicates that the topic was deleted after the subscription was created.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esStatus :: Lens.Lens' EventSubscription (Core.Maybe Types.String)
esStatus = Lens.field @"status"
{-# DEPRECATED esStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The time the AWS DMS event notification subscription was created.
--
-- /Note:/ Consider using 'subscriptionCreationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esSubscriptionCreationTime :: Lens.Lens' EventSubscription (Core.Maybe Types.String)
esSubscriptionCreationTime = Lens.field @"subscriptionCreationTime"
{-# DEPRECATED esSubscriptionCreationTime "Use generic-lens or generic-optics with 'subscriptionCreationTime' instead." #-}

instance Core.FromJSON EventSubscription where
  parseJSON =
    Core.withObject "EventSubscription" Core.$
      \x ->
        EventSubscription'
          Core.<$> (x Core..:? "CustSubscriptionId")
          Core.<*> (x Core..:? "CustomerAwsId")
          Core.<*> (x Core..:? "Enabled")
          Core.<*> (x Core..:? "EventCategoriesList")
          Core.<*> (x Core..:? "SnsTopicArn")
          Core.<*> (x Core..:? "SourceIdsList")
          Core.<*> (x Core..:? "SourceType")
          Core.<*> (x Core..:? "Status")
          Core.<*> (x Core..:? "SubscriptionCreationTime")
