{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.EventSubscription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.EventSubscription where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains the results of a successful invocation of the
-- @DescribeEventSubscriptions@ action.
--
-- /See:/ 'newEventSubscription' smart constructor.
data EventSubscription = EventSubscription'
  { -- | The RDS event notification subscription Id.
    custSubscriptionId :: Core.Maybe Core.Text,
    -- | The status of the RDS event notification subscription.
    --
    -- Constraints:
    --
    -- Can be one of the following: creating | modifying | deleting | active |
    -- no-permission | topic-not-exist
    --
    -- The status \"no-permission\" indicates that RDS no longer has permission
    -- to post to the SNS topic. The status \"topic-not-exist\" indicates that
    -- the topic was deleted after the subscription was created.
    status :: Core.Maybe Core.Text,
    -- | A list of source IDs for the RDS event notification subscription.
    sourceIdsList :: Core.Maybe [Core.Text],
    -- | A list of event categories for the RDS event notification subscription.
    eventCategoriesList :: Core.Maybe [Core.Text],
    -- | A Boolean value indicating if the subscription is enabled. True
    -- indicates the subscription is enabled.
    enabled :: Core.Maybe Core.Bool,
    -- | The Amazon Resource Name (ARN) for the event subscription.
    eventSubscriptionArn :: Core.Maybe Core.Text,
    -- | The time the RDS event notification subscription was created.
    subscriptionCreationTime :: Core.Maybe Core.Text,
    -- | The AWS customer account associated with the RDS event notification
    -- subscription.
    customerAwsId :: Core.Maybe Core.Text,
    -- | The source type for the RDS event notification subscription.
    sourceType :: Core.Maybe Core.Text,
    -- | The topic ARN of the RDS event notification subscription.
    snsTopicArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EventSubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'custSubscriptionId', 'eventSubscription_custSubscriptionId' - The RDS event notification subscription Id.
--
-- 'status', 'eventSubscription_status' - The status of the RDS event notification subscription.
--
-- Constraints:
--
-- Can be one of the following: creating | modifying | deleting | active |
-- no-permission | topic-not-exist
--
-- The status \"no-permission\" indicates that RDS no longer has permission
-- to post to the SNS topic. The status \"topic-not-exist\" indicates that
-- the topic was deleted after the subscription was created.
--
-- 'sourceIdsList', 'eventSubscription_sourceIdsList' - A list of source IDs for the RDS event notification subscription.
--
-- 'eventCategoriesList', 'eventSubscription_eventCategoriesList' - A list of event categories for the RDS event notification subscription.
--
-- 'enabled', 'eventSubscription_enabled' - A Boolean value indicating if the subscription is enabled. True
-- indicates the subscription is enabled.
--
-- 'eventSubscriptionArn', 'eventSubscription_eventSubscriptionArn' - The Amazon Resource Name (ARN) for the event subscription.
--
-- 'subscriptionCreationTime', 'eventSubscription_subscriptionCreationTime' - The time the RDS event notification subscription was created.
--
-- 'customerAwsId', 'eventSubscription_customerAwsId' - The AWS customer account associated with the RDS event notification
-- subscription.
--
-- 'sourceType', 'eventSubscription_sourceType' - The source type for the RDS event notification subscription.
--
-- 'snsTopicArn', 'eventSubscription_snsTopicArn' - The topic ARN of the RDS event notification subscription.
newEventSubscription ::
  EventSubscription
newEventSubscription =
  EventSubscription'
    { custSubscriptionId =
        Core.Nothing,
      status = Core.Nothing,
      sourceIdsList = Core.Nothing,
      eventCategoriesList = Core.Nothing,
      enabled = Core.Nothing,
      eventSubscriptionArn = Core.Nothing,
      subscriptionCreationTime = Core.Nothing,
      customerAwsId = Core.Nothing,
      sourceType = Core.Nothing,
      snsTopicArn = Core.Nothing
    }

-- | The RDS event notification subscription Id.
eventSubscription_custSubscriptionId :: Lens.Lens' EventSubscription (Core.Maybe Core.Text)
eventSubscription_custSubscriptionId = Lens.lens (\EventSubscription' {custSubscriptionId} -> custSubscriptionId) (\s@EventSubscription' {} a -> s {custSubscriptionId = a} :: EventSubscription)

-- | The status of the RDS event notification subscription.
--
-- Constraints:
--
-- Can be one of the following: creating | modifying | deleting | active |
-- no-permission | topic-not-exist
--
-- The status \"no-permission\" indicates that RDS no longer has permission
-- to post to the SNS topic. The status \"topic-not-exist\" indicates that
-- the topic was deleted after the subscription was created.
eventSubscription_status :: Lens.Lens' EventSubscription (Core.Maybe Core.Text)
eventSubscription_status = Lens.lens (\EventSubscription' {status} -> status) (\s@EventSubscription' {} a -> s {status = a} :: EventSubscription)

-- | A list of source IDs for the RDS event notification subscription.
eventSubscription_sourceIdsList :: Lens.Lens' EventSubscription (Core.Maybe [Core.Text])
eventSubscription_sourceIdsList = Lens.lens (\EventSubscription' {sourceIdsList} -> sourceIdsList) (\s@EventSubscription' {} a -> s {sourceIdsList = a} :: EventSubscription) Core.. Lens.mapping Lens._Coerce

-- | A list of event categories for the RDS event notification subscription.
eventSubscription_eventCategoriesList :: Lens.Lens' EventSubscription (Core.Maybe [Core.Text])
eventSubscription_eventCategoriesList = Lens.lens (\EventSubscription' {eventCategoriesList} -> eventCategoriesList) (\s@EventSubscription' {} a -> s {eventCategoriesList = a} :: EventSubscription) Core.. Lens.mapping Lens._Coerce

-- | A Boolean value indicating if the subscription is enabled. True
-- indicates the subscription is enabled.
eventSubscription_enabled :: Lens.Lens' EventSubscription (Core.Maybe Core.Bool)
eventSubscription_enabled = Lens.lens (\EventSubscription' {enabled} -> enabled) (\s@EventSubscription' {} a -> s {enabled = a} :: EventSubscription)

-- | The Amazon Resource Name (ARN) for the event subscription.
eventSubscription_eventSubscriptionArn :: Lens.Lens' EventSubscription (Core.Maybe Core.Text)
eventSubscription_eventSubscriptionArn = Lens.lens (\EventSubscription' {eventSubscriptionArn} -> eventSubscriptionArn) (\s@EventSubscription' {} a -> s {eventSubscriptionArn = a} :: EventSubscription)

-- | The time the RDS event notification subscription was created.
eventSubscription_subscriptionCreationTime :: Lens.Lens' EventSubscription (Core.Maybe Core.Text)
eventSubscription_subscriptionCreationTime = Lens.lens (\EventSubscription' {subscriptionCreationTime} -> subscriptionCreationTime) (\s@EventSubscription' {} a -> s {subscriptionCreationTime = a} :: EventSubscription)

-- | The AWS customer account associated with the RDS event notification
-- subscription.
eventSubscription_customerAwsId :: Lens.Lens' EventSubscription (Core.Maybe Core.Text)
eventSubscription_customerAwsId = Lens.lens (\EventSubscription' {customerAwsId} -> customerAwsId) (\s@EventSubscription' {} a -> s {customerAwsId = a} :: EventSubscription)

-- | The source type for the RDS event notification subscription.
eventSubscription_sourceType :: Lens.Lens' EventSubscription (Core.Maybe Core.Text)
eventSubscription_sourceType = Lens.lens (\EventSubscription' {sourceType} -> sourceType) (\s@EventSubscription' {} a -> s {sourceType = a} :: EventSubscription)

-- | The topic ARN of the RDS event notification subscription.
eventSubscription_snsTopicArn :: Lens.Lens' EventSubscription (Core.Maybe Core.Text)
eventSubscription_snsTopicArn = Lens.lens (\EventSubscription' {snsTopicArn} -> snsTopicArn) (\s@EventSubscription' {} a -> s {snsTopicArn = a} :: EventSubscription)

instance Core.FromXML EventSubscription where
  parseXML x =
    EventSubscription'
      Core.<$> (x Core..@? "CustSubscriptionId")
      Core.<*> (x Core..@? "Status")
      Core.<*> ( x Core..@? "SourceIdsList" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "SourceId")
               )
      Core.<*> ( x Core..@? "EventCategoriesList"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "EventCategory")
               )
      Core.<*> (x Core..@? "Enabled")
      Core.<*> (x Core..@? "EventSubscriptionArn")
      Core.<*> (x Core..@? "SubscriptionCreationTime")
      Core.<*> (x Core..@? "CustomerAwsId")
      Core.<*> (x Core..@? "SourceType")
      Core.<*> (x Core..@? "SnsTopicArn")

instance Core.Hashable EventSubscription

instance Core.NFData EventSubscription
