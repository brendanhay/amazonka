{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.Tag

-- | Describes event subscriptions.
--
-- /See:/ 'newEventSubscription' smart constructor.
data EventSubscription = EventSubscription'
  { -- | The name of the Amazon Redshift event notification subscription.
    custSubscriptionId :: Prelude.Maybe Prelude.Text,
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
    status :: Prelude.Maybe Prelude.Text,
    -- | A list of the sources that publish events to the Amazon Redshift event
    -- notification subscription.
    sourceIdsList :: Prelude.Maybe [Prelude.Text],
    -- | The event severity specified in the Amazon Redshift event notification
    -- subscription.
    --
    -- Values: ERROR, INFO
    severity :: Prelude.Maybe Prelude.Text,
    -- | The list of Amazon Redshift event categories specified in the event
    -- notification subscription.
    --
    -- Values: Configuration, Management, Monitoring, Security
    eventCategoriesList :: Prelude.Maybe [Prelude.Text],
    -- | A boolean value indicating whether the subscription is enabled; @true@
    -- indicates that the subscription is enabled.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The date and time the Amazon Redshift event notification subscription
    -- was created.
    subscriptionCreationTime :: Prelude.Maybe Prelude.ISO8601,
    -- | The AWS customer account associated with the Amazon Redshift event
    -- notification subscription.
    customerAwsId :: Prelude.Maybe Prelude.Text,
    -- | The list of tags for the event subscription.
    tags :: Prelude.Maybe [Tag],
    -- | The source type of the events returned by the Amazon Redshift event
    -- notification, such as cluster, cluster-snapshot,
    -- cluster-parameter-group, cluster-security-group, or scheduled-action.
    sourceType :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Amazon SNS topic used by the event
    -- notification subscription.
    snsTopicArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      status = Prelude.Nothing,
      sourceIdsList = Prelude.Nothing,
      severity = Prelude.Nothing,
      eventCategoriesList = Prelude.Nothing,
      enabled = Prelude.Nothing,
      subscriptionCreationTime = Prelude.Nothing,
      customerAwsId = Prelude.Nothing,
      tags = Prelude.Nothing,
      sourceType = Prelude.Nothing,
      snsTopicArn = Prelude.Nothing
    }

-- | The name of the Amazon Redshift event notification subscription.
eventSubscription_custSubscriptionId :: Lens.Lens' EventSubscription (Prelude.Maybe Prelude.Text)
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
eventSubscription_status :: Lens.Lens' EventSubscription (Prelude.Maybe Prelude.Text)
eventSubscription_status = Lens.lens (\EventSubscription' {status} -> status) (\s@EventSubscription' {} a -> s {status = a} :: EventSubscription)

-- | A list of the sources that publish events to the Amazon Redshift event
-- notification subscription.
eventSubscription_sourceIdsList :: Lens.Lens' EventSubscription (Prelude.Maybe [Prelude.Text])
eventSubscription_sourceIdsList = Lens.lens (\EventSubscription' {sourceIdsList} -> sourceIdsList) (\s@EventSubscription' {} a -> s {sourceIdsList = a} :: EventSubscription) Prelude.. Lens.mapping Prelude._Coerce

-- | The event severity specified in the Amazon Redshift event notification
-- subscription.
--
-- Values: ERROR, INFO
eventSubscription_severity :: Lens.Lens' EventSubscription (Prelude.Maybe Prelude.Text)
eventSubscription_severity = Lens.lens (\EventSubscription' {severity} -> severity) (\s@EventSubscription' {} a -> s {severity = a} :: EventSubscription)

-- | The list of Amazon Redshift event categories specified in the event
-- notification subscription.
--
-- Values: Configuration, Management, Monitoring, Security
eventSubscription_eventCategoriesList :: Lens.Lens' EventSubscription (Prelude.Maybe [Prelude.Text])
eventSubscription_eventCategoriesList = Lens.lens (\EventSubscription' {eventCategoriesList} -> eventCategoriesList) (\s@EventSubscription' {} a -> s {eventCategoriesList = a} :: EventSubscription) Prelude.. Lens.mapping Prelude._Coerce

-- | A boolean value indicating whether the subscription is enabled; @true@
-- indicates that the subscription is enabled.
eventSubscription_enabled :: Lens.Lens' EventSubscription (Prelude.Maybe Prelude.Bool)
eventSubscription_enabled = Lens.lens (\EventSubscription' {enabled} -> enabled) (\s@EventSubscription' {} a -> s {enabled = a} :: EventSubscription)

-- | The date and time the Amazon Redshift event notification subscription
-- was created.
eventSubscription_subscriptionCreationTime :: Lens.Lens' EventSubscription (Prelude.Maybe Prelude.UTCTime)
eventSubscription_subscriptionCreationTime = Lens.lens (\EventSubscription' {subscriptionCreationTime} -> subscriptionCreationTime) (\s@EventSubscription' {} a -> s {subscriptionCreationTime = a} :: EventSubscription) Prelude.. Lens.mapping Prelude._Time

-- | The AWS customer account associated with the Amazon Redshift event
-- notification subscription.
eventSubscription_customerAwsId :: Lens.Lens' EventSubscription (Prelude.Maybe Prelude.Text)
eventSubscription_customerAwsId = Lens.lens (\EventSubscription' {customerAwsId} -> customerAwsId) (\s@EventSubscription' {} a -> s {customerAwsId = a} :: EventSubscription)

-- | The list of tags for the event subscription.
eventSubscription_tags :: Lens.Lens' EventSubscription (Prelude.Maybe [Tag])
eventSubscription_tags = Lens.lens (\EventSubscription' {tags} -> tags) (\s@EventSubscription' {} a -> s {tags = a} :: EventSubscription) Prelude.. Lens.mapping Prelude._Coerce

-- | The source type of the events returned by the Amazon Redshift event
-- notification, such as cluster, cluster-snapshot,
-- cluster-parameter-group, cluster-security-group, or scheduled-action.
eventSubscription_sourceType :: Lens.Lens' EventSubscription (Prelude.Maybe Prelude.Text)
eventSubscription_sourceType = Lens.lens (\EventSubscription' {sourceType} -> sourceType) (\s@EventSubscription' {} a -> s {sourceType = a} :: EventSubscription)

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic used by the event
-- notification subscription.
eventSubscription_snsTopicArn :: Lens.Lens' EventSubscription (Prelude.Maybe Prelude.Text)
eventSubscription_snsTopicArn = Lens.lens (\EventSubscription' {snsTopicArn} -> snsTopicArn) (\s@EventSubscription' {} a -> s {snsTopicArn = a} :: EventSubscription)

instance Prelude.FromXML EventSubscription where
  parseXML x =
    EventSubscription'
      Prelude.<$> (x Prelude..@? "CustSubscriptionId")
      Prelude.<*> (x Prelude..@? "Status")
      Prelude.<*> ( x Prelude..@? "SourceIdsList"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "SourceId")
                  )
      Prelude.<*> (x Prelude..@? "Severity")
      Prelude.<*> ( x Prelude..@? "EventCategoriesList"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "EventCategory")
                  )
      Prelude.<*> (x Prelude..@? "Enabled")
      Prelude.<*> (x Prelude..@? "SubscriptionCreationTime")
      Prelude.<*> (x Prelude..@? "CustomerAwsId")
      Prelude.<*> ( x Prelude..@? "Tags" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "Tag")
                  )
      Prelude.<*> (x Prelude..@? "SourceType")
      Prelude.<*> (x Prelude..@? "SnsTopicArn")

instance Prelude.Hashable EventSubscription

instance Prelude.NFData EventSubscription
