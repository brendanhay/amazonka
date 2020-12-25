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
    esCustSubscriptionId,
    esCustomerAwsId,
    esEnabled,
    esEventCategoriesList,
    esSeverity,
    esSnsTopicArn,
    esSourceIdsList,
    esSourceType,
    esStatus,
    esSubscriptionCreationTime,
    esTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types
import qualified Network.AWS.Redshift.Types.String as Types
import qualified Network.AWS.Redshift.Types.Tag as Types

-- | Describes event subscriptions.
--
-- /See:/ 'mkEventSubscription' smart constructor.
data EventSubscription = EventSubscription'
  { -- | The name of the Amazon Redshift event notification subscription.
    custSubscriptionId :: Core.Maybe Types.String,
    -- | The AWS customer account associated with the Amazon Redshift event notification subscription.
    customerAwsId :: Core.Maybe Types.String,
    -- | A boolean value indicating whether the subscription is enabled; @true@ indicates that the subscription is enabled.
    enabled :: Core.Maybe Core.Bool,
    -- | The list of Amazon Redshift event categories specified in the event notification subscription.
    --
    -- Values: Configuration, Management, Monitoring, Security
    eventCategoriesList :: Core.Maybe [Types.String],
    -- | The event severity specified in the Amazon Redshift event notification subscription.
    --
    -- Values: ERROR, INFO
    severity :: Core.Maybe Types.String,
    -- | The Amazon Resource Name (ARN) of the Amazon SNS topic used by the event notification subscription.
    snsTopicArn :: Core.Maybe Types.String,
    -- | A list of the sources that publish events to the Amazon Redshift event notification subscription.
    sourceIdsList :: Core.Maybe [Types.String],
    -- | The source type of the events returned by the Amazon Redshift event notification, such as cluster, cluster-snapshot, cluster-parameter-group, cluster-security-group, or scheduled-action.
    sourceType :: Core.Maybe Types.String,
    -- | The status of the Amazon Redshift event notification subscription.
    --
    -- Constraints:
    --
    --     * Can be one of the following: active | no-permission | topic-not-exist
    --
    --
    --     * The status "no-permission" indicates that Amazon Redshift no longer has permission to post to the Amazon SNS topic. The status "topic-not-exist" indicates that the topic was deleted after the subscription was created.
    status :: Core.Maybe Types.String,
    -- | The date and time the Amazon Redshift event notification subscription was created.
    subscriptionCreationTime :: Core.Maybe Core.UTCTime,
    -- | The list of tags for the event subscription.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'EventSubscription' value with any optional fields omitted.
mkEventSubscription ::
  EventSubscription
mkEventSubscription =
  EventSubscription'
    { custSubscriptionId = Core.Nothing,
      customerAwsId = Core.Nothing,
      enabled = Core.Nothing,
      eventCategoriesList = Core.Nothing,
      severity = Core.Nothing,
      snsTopicArn = Core.Nothing,
      sourceIdsList = Core.Nothing,
      sourceType = Core.Nothing,
      status = Core.Nothing,
      subscriptionCreationTime = Core.Nothing,
      tags = Core.Nothing
    }

-- | The name of the Amazon Redshift event notification subscription.
--
-- /Note:/ Consider using 'custSubscriptionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esCustSubscriptionId :: Lens.Lens' EventSubscription (Core.Maybe Types.String)
esCustSubscriptionId = Lens.field @"custSubscriptionId"
{-# DEPRECATED esCustSubscriptionId "Use generic-lens or generic-optics with 'custSubscriptionId' instead." #-}

-- | The AWS customer account associated with the Amazon Redshift event notification subscription.
--
-- /Note:/ Consider using 'customerAwsId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esCustomerAwsId :: Lens.Lens' EventSubscription (Core.Maybe Types.String)
esCustomerAwsId = Lens.field @"customerAwsId"
{-# DEPRECATED esCustomerAwsId "Use generic-lens or generic-optics with 'customerAwsId' instead." #-}

-- | A boolean value indicating whether the subscription is enabled; @true@ indicates that the subscription is enabled.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esEnabled :: Lens.Lens' EventSubscription (Core.Maybe Core.Bool)
esEnabled = Lens.field @"enabled"
{-# DEPRECATED esEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The list of Amazon Redshift event categories specified in the event notification subscription.
--
-- Values: Configuration, Management, Monitoring, Security
--
-- /Note:/ Consider using 'eventCategoriesList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esEventCategoriesList :: Lens.Lens' EventSubscription (Core.Maybe [Types.String])
esEventCategoriesList = Lens.field @"eventCategoriesList"
{-# DEPRECATED esEventCategoriesList "Use generic-lens or generic-optics with 'eventCategoriesList' instead." #-}

-- | The event severity specified in the Amazon Redshift event notification subscription.
--
-- Values: ERROR, INFO
--
-- /Note:/ Consider using 'severity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esSeverity :: Lens.Lens' EventSubscription (Core.Maybe Types.String)
esSeverity = Lens.field @"severity"
{-# DEPRECATED esSeverity "Use generic-lens or generic-optics with 'severity' instead." #-}

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic used by the event notification subscription.
--
-- /Note:/ Consider using 'snsTopicArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esSnsTopicArn :: Lens.Lens' EventSubscription (Core.Maybe Types.String)
esSnsTopicArn = Lens.field @"snsTopicArn"
{-# DEPRECATED esSnsTopicArn "Use generic-lens or generic-optics with 'snsTopicArn' instead." #-}

-- | A list of the sources that publish events to the Amazon Redshift event notification subscription.
--
-- /Note:/ Consider using 'sourceIdsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esSourceIdsList :: Lens.Lens' EventSubscription (Core.Maybe [Types.String])
esSourceIdsList = Lens.field @"sourceIdsList"
{-# DEPRECATED esSourceIdsList "Use generic-lens or generic-optics with 'sourceIdsList' instead." #-}

-- | The source type of the events returned by the Amazon Redshift event notification, such as cluster, cluster-snapshot, cluster-parameter-group, cluster-security-group, or scheduled-action.
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esSourceType :: Lens.Lens' EventSubscription (Core.Maybe Types.String)
esSourceType = Lens.field @"sourceType"
{-# DEPRECATED esSourceType "Use generic-lens or generic-optics with 'sourceType' instead." #-}

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
esStatus :: Lens.Lens' EventSubscription (Core.Maybe Types.String)
esStatus = Lens.field @"status"
{-# DEPRECATED esStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The date and time the Amazon Redshift event notification subscription was created.
--
-- /Note:/ Consider using 'subscriptionCreationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esSubscriptionCreationTime :: Lens.Lens' EventSubscription (Core.Maybe Core.UTCTime)
esSubscriptionCreationTime = Lens.field @"subscriptionCreationTime"
{-# DEPRECATED esSubscriptionCreationTime "Use generic-lens or generic-optics with 'subscriptionCreationTime' instead." #-}

-- | The list of tags for the event subscription.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esTags :: Lens.Lens' EventSubscription (Core.Maybe [Types.Tag])
esTags = Lens.field @"tags"
{-# DEPRECATED esTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromXML EventSubscription where
  parseXML x =
    EventSubscription'
      Core.<$> (x Core..@? "CustSubscriptionId")
      Core.<*> (x Core..@? "CustomerAwsId")
      Core.<*> (x Core..@? "Enabled")
      Core.<*> ( x Core..@? "EventCategoriesList"
                   Core..<@> Core.parseXMLList "EventCategory"
               )
      Core.<*> (x Core..@? "Severity")
      Core.<*> (x Core..@? "SnsTopicArn")
      Core.<*> (x Core..@? "SourceIdsList" Core..<@> Core.parseXMLList "SourceId")
      Core.<*> (x Core..@? "SourceType")
      Core.<*> (x Core..@? "Status")
      Core.<*> (x Core..@? "SubscriptionCreationTime")
      Core.<*> (x Core..@? "Tags" Core..<@> Core.parseXMLList "Tag")
