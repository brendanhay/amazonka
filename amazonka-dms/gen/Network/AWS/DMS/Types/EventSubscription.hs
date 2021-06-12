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
-- Module      : Network.AWS.DMS.Types.EventSubscription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.EventSubscription where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes an event notification subscription created by the
-- @CreateEventSubscription@ operation.
--
-- /See:/ 'newEventSubscription' smart constructor.
data EventSubscription = EventSubscription'
  { -- | The AWS DMS event notification subscription Id.
    custSubscriptionId :: Core.Maybe Core.Text,
    -- | The status of the AWS DMS event notification subscription.
    --
    -- Constraints:
    --
    -- Can be one of the following: creating | modifying | deleting | active |
    -- no-permission | topic-not-exist
    --
    -- The status \"no-permission\" indicates that AWS DMS no longer has
    -- permission to post to the SNS topic. The status \"topic-not-exist\"
    -- indicates that the topic was deleted after the subscription was created.
    status :: Core.Maybe Core.Text,
    -- | A list of source Ids for the event subscription.
    sourceIdsList :: Core.Maybe [Core.Text],
    -- | A lists of event categories.
    eventCategoriesList :: Core.Maybe [Core.Text],
    -- | Boolean value that indicates if the event subscription is enabled.
    enabled :: Core.Maybe Core.Bool,
    -- | The time the AWS DMS event notification subscription was created.
    subscriptionCreationTime :: Core.Maybe Core.Text,
    -- | The AWS customer account associated with the AWS DMS event notification
    -- subscription.
    customerAwsId :: Core.Maybe Core.Text,
    -- | The type of AWS DMS resource that generates events.
    --
    -- Valid values: replication-instance | replication-server | security-group
    -- | replication-task
    sourceType :: Core.Maybe Core.Text,
    -- | The topic ARN of the AWS DMS event notification subscription.
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
-- 'custSubscriptionId', 'eventSubscription_custSubscriptionId' - The AWS DMS event notification subscription Id.
--
-- 'status', 'eventSubscription_status' - The status of the AWS DMS event notification subscription.
--
-- Constraints:
--
-- Can be one of the following: creating | modifying | deleting | active |
-- no-permission | topic-not-exist
--
-- The status \"no-permission\" indicates that AWS DMS no longer has
-- permission to post to the SNS topic. The status \"topic-not-exist\"
-- indicates that the topic was deleted after the subscription was created.
--
-- 'sourceIdsList', 'eventSubscription_sourceIdsList' - A list of source Ids for the event subscription.
--
-- 'eventCategoriesList', 'eventSubscription_eventCategoriesList' - A lists of event categories.
--
-- 'enabled', 'eventSubscription_enabled' - Boolean value that indicates if the event subscription is enabled.
--
-- 'subscriptionCreationTime', 'eventSubscription_subscriptionCreationTime' - The time the AWS DMS event notification subscription was created.
--
-- 'customerAwsId', 'eventSubscription_customerAwsId' - The AWS customer account associated with the AWS DMS event notification
-- subscription.
--
-- 'sourceType', 'eventSubscription_sourceType' - The type of AWS DMS resource that generates events.
--
-- Valid values: replication-instance | replication-server | security-group
-- | replication-task
--
-- 'snsTopicArn', 'eventSubscription_snsTopicArn' - The topic ARN of the AWS DMS event notification subscription.
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
      subscriptionCreationTime = Core.Nothing,
      customerAwsId = Core.Nothing,
      sourceType = Core.Nothing,
      snsTopicArn = Core.Nothing
    }

-- | The AWS DMS event notification subscription Id.
eventSubscription_custSubscriptionId :: Lens.Lens' EventSubscription (Core.Maybe Core.Text)
eventSubscription_custSubscriptionId = Lens.lens (\EventSubscription' {custSubscriptionId} -> custSubscriptionId) (\s@EventSubscription' {} a -> s {custSubscriptionId = a} :: EventSubscription)

-- | The status of the AWS DMS event notification subscription.
--
-- Constraints:
--
-- Can be one of the following: creating | modifying | deleting | active |
-- no-permission | topic-not-exist
--
-- The status \"no-permission\" indicates that AWS DMS no longer has
-- permission to post to the SNS topic. The status \"topic-not-exist\"
-- indicates that the topic was deleted after the subscription was created.
eventSubscription_status :: Lens.Lens' EventSubscription (Core.Maybe Core.Text)
eventSubscription_status = Lens.lens (\EventSubscription' {status} -> status) (\s@EventSubscription' {} a -> s {status = a} :: EventSubscription)

-- | A list of source Ids for the event subscription.
eventSubscription_sourceIdsList :: Lens.Lens' EventSubscription (Core.Maybe [Core.Text])
eventSubscription_sourceIdsList = Lens.lens (\EventSubscription' {sourceIdsList} -> sourceIdsList) (\s@EventSubscription' {} a -> s {sourceIdsList = a} :: EventSubscription) Core.. Lens.mapping Lens._Coerce

-- | A lists of event categories.
eventSubscription_eventCategoriesList :: Lens.Lens' EventSubscription (Core.Maybe [Core.Text])
eventSubscription_eventCategoriesList = Lens.lens (\EventSubscription' {eventCategoriesList} -> eventCategoriesList) (\s@EventSubscription' {} a -> s {eventCategoriesList = a} :: EventSubscription) Core.. Lens.mapping Lens._Coerce

-- | Boolean value that indicates if the event subscription is enabled.
eventSubscription_enabled :: Lens.Lens' EventSubscription (Core.Maybe Core.Bool)
eventSubscription_enabled = Lens.lens (\EventSubscription' {enabled} -> enabled) (\s@EventSubscription' {} a -> s {enabled = a} :: EventSubscription)

-- | The time the AWS DMS event notification subscription was created.
eventSubscription_subscriptionCreationTime :: Lens.Lens' EventSubscription (Core.Maybe Core.Text)
eventSubscription_subscriptionCreationTime = Lens.lens (\EventSubscription' {subscriptionCreationTime} -> subscriptionCreationTime) (\s@EventSubscription' {} a -> s {subscriptionCreationTime = a} :: EventSubscription)

-- | The AWS customer account associated with the AWS DMS event notification
-- subscription.
eventSubscription_customerAwsId :: Lens.Lens' EventSubscription (Core.Maybe Core.Text)
eventSubscription_customerAwsId = Lens.lens (\EventSubscription' {customerAwsId} -> customerAwsId) (\s@EventSubscription' {} a -> s {customerAwsId = a} :: EventSubscription)

-- | The type of AWS DMS resource that generates events.
--
-- Valid values: replication-instance | replication-server | security-group
-- | replication-task
eventSubscription_sourceType :: Lens.Lens' EventSubscription (Core.Maybe Core.Text)
eventSubscription_sourceType = Lens.lens (\EventSubscription' {sourceType} -> sourceType) (\s@EventSubscription' {} a -> s {sourceType = a} :: EventSubscription)

-- | The topic ARN of the AWS DMS event notification subscription.
eventSubscription_snsTopicArn :: Lens.Lens' EventSubscription (Core.Maybe Core.Text)
eventSubscription_snsTopicArn = Lens.lens (\EventSubscription' {snsTopicArn} -> snsTopicArn) (\s@EventSubscription' {} a -> s {snsTopicArn = a} :: EventSubscription)

instance Core.FromJSON EventSubscription where
  parseJSON =
    Core.withObject
      "EventSubscription"
      ( \x ->
          EventSubscription'
            Core.<$> (x Core..:? "CustSubscriptionId")
            Core.<*> (x Core..:? "Status")
            Core.<*> (x Core..:? "SourceIdsList" Core..!= Core.mempty)
            Core.<*> ( x Core..:? "EventCategoriesList"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "Enabled")
            Core.<*> (x Core..:? "SubscriptionCreationTime")
            Core.<*> (x Core..:? "CustomerAwsId")
            Core.<*> (x Core..:? "SourceType")
            Core.<*> (x Core..:? "SnsTopicArn")
      )

instance Core.Hashable EventSubscription

instance Core.NFData EventSubscription
