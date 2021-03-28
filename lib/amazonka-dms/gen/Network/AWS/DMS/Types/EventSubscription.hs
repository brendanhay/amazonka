{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.EventSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DMS.Types.EventSubscription
  ( EventSubscription (..)
  -- * Smart constructor
  , mkEventSubscription
  -- * Lenses
  , esCustSubscriptionId
  , esCustomerAwsId
  , esEnabled
  , esEventCategoriesList
  , esSnsTopicArn
  , esSourceIdsList
  , esSourceType
  , esStatus
  , esSubscriptionCreationTime
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an event notification subscription created by the @CreateEventSubscription@ operation.
--
-- /See:/ 'mkEventSubscription' smart constructor.
data EventSubscription = EventSubscription'
  { custSubscriptionId :: Core.Maybe Core.Text
    -- ^ The AWS DMS event notification subscription Id.
  , customerAwsId :: Core.Maybe Core.Text
    -- ^ The AWS customer account associated with the AWS DMS event notification subscription.
  , enabled :: Core.Maybe Core.Bool
    -- ^ Boolean value that indicates if the event subscription is enabled.
  , eventCategoriesList :: Core.Maybe [Core.Text]
    -- ^ A lists of event categories.
  , snsTopicArn :: Core.Maybe Core.Text
    -- ^ The topic ARN of the AWS DMS event notification subscription.
  , sourceIdsList :: Core.Maybe [Core.Text]
    -- ^ A list of source Ids for the event subscription.
  , sourceType :: Core.Maybe Core.Text
    -- ^ The type of AWS DMS resource that generates events. 
--
-- Valid values: replication-instance | replication-server | security-group | replication-task
  , status :: Core.Maybe Core.Text
    -- ^ The status of the AWS DMS event notification subscription.
--
-- Constraints:
-- Can be one of the following: creating | modifying | deleting | active | no-permission | topic-not-exist
-- The status "no-permission" indicates that AWS DMS no longer has permission to post to the SNS topic. The status "topic-not-exist" indicates that the topic was deleted after the subscription was created.
  , subscriptionCreationTime :: Core.Maybe Core.Text
    -- ^ The time the AWS DMS event notification subscription was created.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EventSubscription' value with any optional fields omitted.
mkEventSubscription
    :: EventSubscription
mkEventSubscription
  = EventSubscription'{custSubscriptionId = Core.Nothing,
                       customerAwsId = Core.Nothing, enabled = Core.Nothing,
                       eventCategoriesList = Core.Nothing, snsTopicArn = Core.Nothing,
                       sourceIdsList = Core.Nothing, sourceType = Core.Nothing,
                       status = Core.Nothing, subscriptionCreationTime = Core.Nothing}

-- | The AWS DMS event notification subscription Id.
--
-- /Note:/ Consider using 'custSubscriptionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esCustSubscriptionId :: Lens.Lens' EventSubscription (Core.Maybe Core.Text)
esCustSubscriptionId = Lens.field @"custSubscriptionId"
{-# INLINEABLE esCustSubscriptionId #-}
{-# DEPRECATED custSubscriptionId "Use generic-lens or generic-optics with 'custSubscriptionId' instead"  #-}

-- | The AWS customer account associated with the AWS DMS event notification subscription.
--
-- /Note:/ Consider using 'customerAwsId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esCustomerAwsId :: Lens.Lens' EventSubscription (Core.Maybe Core.Text)
esCustomerAwsId = Lens.field @"customerAwsId"
{-# INLINEABLE esCustomerAwsId #-}
{-# DEPRECATED customerAwsId "Use generic-lens or generic-optics with 'customerAwsId' instead"  #-}

-- | Boolean value that indicates if the event subscription is enabled.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esEnabled :: Lens.Lens' EventSubscription (Core.Maybe Core.Bool)
esEnabled = Lens.field @"enabled"
{-# INLINEABLE esEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

-- | A lists of event categories.
--
-- /Note:/ Consider using 'eventCategoriesList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esEventCategoriesList :: Lens.Lens' EventSubscription (Core.Maybe [Core.Text])
esEventCategoriesList = Lens.field @"eventCategoriesList"
{-# INLINEABLE esEventCategoriesList #-}
{-# DEPRECATED eventCategoriesList "Use generic-lens or generic-optics with 'eventCategoriesList' instead"  #-}

-- | The topic ARN of the AWS DMS event notification subscription.
--
-- /Note:/ Consider using 'snsTopicArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esSnsTopicArn :: Lens.Lens' EventSubscription (Core.Maybe Core.Text)
esSnsTopicArn = Lens.field @"snsTopicArn"
{-# INLINEABLE esSnsTopicArn #-}
{-# DEPRECATED snsTopicArn "Use generic-lens or generic-optics with 'snsTopicArn' instead"  #-}

-- | A list of source Ids for the event subscription.
--
-- /Note:/ Consider using 'sourceIdsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esSourceIdsList :: Lens.Lens' EventSubscription (Core.Maybe [Core.Text])
esSourceIdsList = Lens.field @"sourceIdsList"
{-# INLINEABLE esSourceIdsList #-}
{-# DEPRECATED sourceIdsList "Use generic-lens or generic-optics with 'sourceIdsList' instead"  #-}

-- | The type of AWS DMS resource that generates events. 
--
-- Valid values: replication-instance | replication-server | security-group | replication-task
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esSourceType :: Lens.Lens' EventSubscription (Core.Maybe Core.Text)
esSourceType = Lens.field @"sourceType"
{-# INLINEABLE esSourceType #-}
{-# DEPRECATED sourceType "Use generic-lens or generic-optics with 'sourceType' instead"  #-}

-- | The status of the AWS DMS event notification subscription.
--
-- Constraints:
-- Can be one of the following: creating | modifying | deleting | active | no-permission | topic-not-exist
-- The status "no-permission" indicates that AWS DMS no longer has permission to post to the SNS topic. The status "topic-not-exist" indicates that the topic was deleted after the subscription was created.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esStatus :: Lens.Lens' EventSubscription (Core.Maybe Core.Text)
esStatus = Lens.field @"status"
{-# INLINEABLE esStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The time the AWS DMS event notification subscription was created.
--
-- /Note:/ Consider using 'subscriptionCreationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esSubscriptionCreationTime :: Lens.Lens' EventSubscription (Core.Maybe Core.Text)
esSubscriptionCreationTime = Lens.field @"subscriptionCreationTime"
{-# INLINEABLE esSubscriptionCreationTime #-}
{-# DEPRECATED subscriptionCreationTime "Use generic-lens or generic-optics with 'subscriptionCreationTime' instead"  #-}

instance Core.FromJSON EventSubscription where
        parseJSON
          = Core.withObject "EventSubscription" Core.$
              \ x ->
                EventSubscription' Core.<$>
                  (x Core..:? "CustSubscriptionId") Core.<*>
                    x Core..:? "CustomerAwsId"
                    Core.<*> x Core..:? "Enabled"
                    Core.<*> x Core..:? "EventCategoriesList"
                    Core.<*> x Core..:? "SnsTopicArn"
                    Core.<*> x Core..:? "SourceIdsList"
                    Core.<*> x Core..:? "SourceType"
                    Core.<*> x Core..:? "Status"
                    Core.<*> x Core..:? "SubscriptionCreationTime"
