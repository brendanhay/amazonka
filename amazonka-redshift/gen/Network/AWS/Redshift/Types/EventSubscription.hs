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
-- Module      : Network.AWS.Redshift.Types.EventSubscription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.EventSubscription where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.Tag

-- | Describes event subscriptions.
--
-- /See:/ 'newEventSubscription' smart constructor.
data EventSubscription = EventSubscription'
  { -- | The name of the Amazon Redshift event notification subscription.
    custSubscriptionId :: Core.Maybe Core.Text,
    -- | The status of the Amazon Redshift event notification subscription.
    --
    -- Constraints:
    --
    -- -   Can be one of the following: active | no-permission |
    --     topic-not-exist
    --
    -- -   The status \"no-permission\" indicates that Amazon Redshift no
    --     longer has permission to post to the Amazon SNS topic. The status
    --     \"topic-not-exist\" indicates that the topic was deleted after the
    --     subscription was created.
    status :: Core.Maybe Core.Text,
    -- | A list of the sources that publish events to the Amazon Redshift event
    -- notification subscription.
    sourceIdsList :: Core.Maybe [Core.Text],
    -- | The event severity specified in the Amazon Redshift event notification
    -- subscription.
    --
    -- Values: ERROR, INFO
    severity :: Core.Maybe Core.Text,
    -- | The list of Amazon Redshift event categories specified in the event
    -- notification subscription.
    --
    -- Values: Configuration, Management, Monitoring, Security
    eventCategoriesList :: Core.Maybe [Core.Text],
    -- | A boolean value indicating whether the subscription is enabled; @true@
    -- indicates that the subscription is enabled.
    enabled :: Core.Maybe Core.Bool,
    -- | The date and time the Amazon Redshift event notification subscription
    -- was created.
    subscriptionCreationTime :: Core.Maybe Core.ISO8601,
    -- | The AWS customer account associated with the Amazon Redshift event
    -- notification subscription.
    customerAwsId :: Core.Maybe Core.Text,
    -- | The list of tags for the event subscription.
    tags :: Core.Maybe [Tag],
    -- | The source type of the events returned by the Amazon Redshift event
    -- notification, such as cluster, cluster-snapshot,
    -- cluster-parameter-group, cluster-security-group, or scheduled-action.
    sourceType :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the Amazon SNS topic used by the event
    -- notification subscription.
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
-- 'custSubscriptionId', 'eventSubscription_custSubscriptionId' - The name of the Amazon Redshift event notification subscription.
--
-- 'status', 'eventSubscription_status' - The status of the Amazon Redshift event notification subscription.
--
-- Constraints:
--
-- -   Can be one of the following: active | no-permission |
--     topic-not-exist
--
-- -   The status \"no-permission\" indicates that Amazon Redshift no
--     longer has permission to post to the Amazon SNS topic. The status
--     \"topic-not-exist\" indicates that the topic was deleted after the
--     subscription was created.
--
-- 'sourceIdsList', 'eventSubscription_sourceIdsList' - A list of the sources that publish events to the Amazon Redshift event
-- notification subscription.
--
-- 'severity', 'eventSubscription_severity' - The event severity specified in the Amazon Redshift event notification
-- subscription.
--
-- Values: ERROR, INFO
--
-- 'eventCategoriesList', 'eventSubscription_eventCategoriesList' - The list of Amazon Redshift event categories specified in the event
-- notification subscription.
--
-- Values: Configuration, Management, Monitoring, Security
--
-- 'enabled', 'eventSubscription_enabled' - A boolean value indicating whether the subscription is enabled; @true@
-- indicates that the subscription is enabled.
--
-- 'subscriptionCreationTime', 'eventSubscription_subscriptionCreationTime' - The date and time the Amazon Redshift event notification subscription
-- was created.
--
-- 'customerAwsId', 'eventSubscription_customerAwsId' - The AWS customer account associated with the Amazon Redshift event
-- notification subscription.
--
-- 'tags', 'eventSubscription_tags' - The list of tags for the event subscription.
--
-- 'sourceType', 'eventSubscription_sourceType' - The source type of the events returned by the Amazon Redshift event
-- notification, such as cluster, cluster-snapshot,
-- cluster-parameter-group, cluster-security-group, or scheduled-action.
--
-- 'snsTopicArn', 'eventSubscription_snsTopicArn' - The Amazon Resource Name (ARN) of the Amazon SNS topic used by the event
-- notification subscription.
newEventSubscription ::
  EventSubscription
newEventSubscription =
  EventSubscription'
    { custSubscriptionId =
        Core.Nothing,
      status = Core.Nothing,
      sourceIdsList = Core.Nothing,
      severity = Core.Nothing,
      eventCategoriesList = Core.Nothing,
      enabled = Core.Nothing,
      subscriptionCreationTime = Core.Nothing,
      customerAwsId = Core.Nothing,
      tags = Core.Nothing,
      sourceType = Core.Nothing,
      snsTopicArn = Core.Nothing
    }

-- | The name of the Amazon Redshift event notification subscription.
eventSubscription_custSubscriptionId :: Lens.Lens' EventSubscription (Core.Maybe Core.Text)
eventSubscription_custSubscriptionId = Lens.lens (\EventSubscription' {custSubscriptionId} -> custSubscriptionId) (\s@EventSubscription' {} a -> s {custSubscriptionId = a} :: EventSubscription)

-- | The status of the Amazon Redshift event notification subscription.
--
-- Constraints:
--
-- -   Can be one of the following: active | no-permission |
--     topic-not-exist
--
-- -   The status \"no-permission\" indicates that Amazon Redshift no
--     longer has permission to post to the Amazon SNS topic. The status
--     \"topic-not-exist\" indicates that the topic was deleted after the
--     subscription was created.
eventSubscription_status :: Lens.Lens' EventSubscription (Core.Maybe Core.Text)
eventSubscription_status = Lens.lens (\EventSubscription' {status} -> status) (\s@EventSubscription' {} a -> s {status = a} :: EventSubscription)

-- | A list of the sources that publish events to the Amazon Redshift event
-- notification subscription.
eventSubscription_sourceIdsList :: Lens.Lens' EventSubscription (Core.Maybe [Core.Text])
eventSubscription_sourceIdsList = Lens.lens (\EventSubscription' {sourceIdsList} -> sourceIdsList) (\s@EventSubscription' {} a -> s {sourceIdsList = a} :: EventSubscription) Core.. Lens.mapping Lens._Coerce

-- | The event severity specified in the Amazon Redshift event notification
-- subscription.
--
-- Values: ERROR, INFO
eventSubscription_severity :: Lens.Lens' EventSubscription (Core.Maybe Core.Text)
eventSubscription_severity = Lens.lens (\EventSubscription' {severity} -> severity) (\s@EventSubscription' {} a -> s {severity = a} :: EventSubscription)

-- | The list of Amazon Redshift event categories specified in the event
-- notification subscription.
--
-- Values: Configuration, Management, Monitoring, Security
eventSubscription_eventCategoriesList :: Lens.Lens' EventSubscription (Core.Maybe [Core.Text])
eventSubscription_eventCategoriesList = Lens.lens (\EventSubscription' {eventCategoriesList} -> eventCategoriesList) (\s@EventSubscription' {} a -> s {eventCategoriesList = a} :: EventSubscription) Core.. Lens.mapping Lens._Coerce

-- | A boolean value indicating whether the subscription is enabled; @true@
-- indicates that the subscription is enabled.
eventSubscription_enabled :: Lens.Lens' EventSubscription (Core.Maybe Core.Bool)
eventSubscription_enabled = Lens.lens (\EventSubscription' {enabled} -> enabled) (\s@EventSubscription' {} a -> s {enabled = a} :: EventSubscription)

-- | The date and time the Amazon Redshift event notification subscription
-- was created.
eventSubscription_subscriptionCreationTime :: Lens.Lens' EventSubscription (Core.Maybe Core.UTCTime)
eventSubscription_subscriptionCreationTime = Lens.lens (\EventSubscription' {subscriptionCreationTime} -> subscriptionCreationTime) (\s@EventSubscription' {} a -> s {subscriptionCreationTime = a} :: EventSubscription) Core.. Lens.mapping Core._Time

-- | The AWS customer account associated with the Amazon Redshift event
-- notification subscription.
eventSubscription_customerAwsId :: Lens.Lens' EventSubscription (Core.Maybe Core.Text)
eventSubscription_customerAwsId = Lens.lens (\EventSubscription' {customerAwsId} -> customerAwsId) (\s@EventSubscription' {} a -> s {customerAwsId = a} :: EventSubscription)

-- | The list of tags for the event subscription.
eventSubscription_tags :: Lens.Lens' EventSubscription (Core.Maybe [Tag])
eventSubscription_tags = Lens.lens (\EventSubscription' {tags} -> tags) (\s@EventSubscription' {} a -> s {tags = a} :: EventSubscription) Core.. Lens.mapping Lens._Coerce

-- | The source type of the events returned by the Amazon Redshift event
-- notification, such as cluster, cluster-snapshot,
-- cluster-parameter-group, cluster-security-group, or scheduled-action.
eventSubscription_sourceType :: Lens.Lens' EventSubscription (Core.Maybe Core.Text)
eventSubscription_sourceType = Lens.lens (\EventSubscription' {sourceType} -> sourceType) (\s@EventSubscription' {} a -> s {sourceType = a} :: EventSubscription)

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic used by the event
-- notification subscription.
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
      Core.<*> (x Core..@? "Severity")
      Core.<*> ( x Core..@? "EventCategoriesList"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "EventCategory")
               )
      Core.<*> (x Core..@? "Enabled")
      Core.<*> (x Core..@? "SubscriptionCreationTime")
      Core.<*> (x Core..@? "CustomerAwsId")
      Core.<*> ( x Core..@? "Tags" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "Tag")
               )
      Core.<*> (x Core..@? "SourceType")
      Core.<*> (x Core..@? "SnsTopicArn")

instance Core.Hashable EventSubscription

instance Core.NFData EventSubscription
