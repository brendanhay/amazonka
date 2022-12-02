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
-- Module      : Amazonka.Redshift.Types.EventSubscription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.EventSubscription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal
import Amazonka.Redshift.Types.Tag

-- | Describes event subscriptions.
--
-- /See:/ 'newEventSubscription' smart constructor.
data EventSubscription = EventSubscription'
  { -- | The list of tags for the event subscription.
    tags :: Prelude.Maybe [Tag],
    -- | The event severity specified in the Amazon Redshift event notification
    -- subscription.
    --
    -- Values: ERROR, INFO
    severity :: Prelude.Maybe Prelude.Text,
    -- | The date and time the Amazon Redshift event notification subscription
    -- was created.
    subscriptionCreationTime :: Prelude.Maybe Data.ISO8601,
    -- | The name of the Amazon Redshift event notification subscription.
    custSubscriptionId :: Prelude.Maybe Prelude.Text,
    -- | A list of the sources that publish events to the Amazon Redshift event
    -- notification subscription.
    sourceIdsList :: Prelude.Maybe [Prelude.Text],
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
    -- | The source type of the events returned by the Amazon Redshift event
    -- notification, such as cluster, cluster-snapshot,
    -- cluster-parameter-group, cluster-security-group, or scheduled-action.
    sourceType :: Prelude.Maybe Prelude.Text,
    -- | A boolean value indicating whether the subscription is enabled; @true@
    -- indicates that the subscription is enabled.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the Amazon SNS topic used by the event
    -- notification subscription.
    snsTopicArn :: Prelude.Maybe Prelude.Text,
    -- | The list of Amazon Redshift event categories specified in the event
    -- notification subscription.
    --
    -- Values: Configuration, Management, Monitoring, Security, Pending
    eventCategoriesList :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Web Services account associated with the Amazon Redshift
    -- event notification subscription.
    customerAwsId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EventSubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'eventSubscription_tags' - The list of tags for the event subscription.
--
-- 'severity', 'eventSubscription_severity' - The event severity specified in the Amazon Redshift event notification
-- subscription.
--
-- Values: ERROR, INFO
--
-- 'subscriptionCreationTime', 'eventSubscription_subscriptionCreationTime' - The date and time the Amazon Redshift event notification subscription
-- was created.
--
-- 'custSubscriptionId', 'eventSubscription_custSubscriptionId' - The name of the Amazon Redshift event notification subscription.
--
-- 'sourceIdsList', 'eventSubscription_sourceIdsList' - A list of the sources that publish events to the Amazon Redshift event
-- notification subscription.
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
-- 'sourceType', 'eventSubscription_sourceType' - The source type of the events returned by the Amazon Redshift event
-- notification, such as cluster, cluster-snapshot,
-- cluster-parameter-group, cluster-security-group, or scheduled-action.
--
-- 'enabled', 'eventSubscription_enabled' - A boolean value indicating whether the subscription is enabled; @true@
-- indicates that the subscription is enabled.
--
-- 'snsTopicArn', 'eventSubscription_snsTopicArn' - The Amazon Resource Name (ARN) of the Amazon SNS topic used by the event
-- notification subscription.
--
-- 'eventCategoriesList', 'eventSubscription_eventCategoriesList' - The list of Amazon Redshift event categories specified in the event
-- notification subscription.
--
-- Values: Configuration, Management, Monitoring, Security, Pending
--
-- 'customerAwsId', 'eventSubscription_customerAwsId' - The Amazon Web Services account associated with the Amazon Redshift
-- event notification subscription.
newEventSubscription ::
  EventSubscription
newEventSubscription =
  EventSubscription'
    { tags = Prelude.Nothing,
      severity = Prelude.Nothing,
      subscriptionCreationTime = Prelude.Nothing,
      custSubscriptionId = Prelude.Nothing,
      sourceIdsList = Prelude.Nothing,
      status = Prelude.Nothing,
      sourceType = Prelude.Nothing,
      enabled = Prelude.Nothing,
      snsTopicArn = Prelude.Nothing,
      eventCategoriesList = Prelude.Nothing,
      customerAwsId = Prelude.Nothing
    }

-- | The list of tags for the event subscription.
eventSubscription_tags :: Lens.Lens' EventSubscription (Prelude.Maybe [Tag])
eventSubscription_tags = Lens.lens (\EventSubscription' {tags} -> tags) (\s@EventSubscription' {} a -> s {tags = a} :: EventSubscription) Prelude.. Lens.mapping Lens.coerced

-- | The event severity specified in the Amazon Redshift event notification
-- subscription.
--
-- Values: ERROR, INFO
eventSubscription_severity :: Lens.Lens' EventSubscription (Prelude.Maybe Prelude.Text)
eventSubscription_severity = Lens.lens (\EventSubscription' {severity} -> severity) (\s@EventSubscription' {} a -> s {severity = a} :: EventSubscription)

-- | The date and time the Amazon Redshift event notification subscription
-- was created.
eventSubscription_subscriptionCreationTime :: Lens.Lens' EventSubscription (Prelude.Maybe Prelude.UTCTime)
eventSubscription_subscriptionCreationTime = Lens.lens (\EventSubscription' {subscriptionCreationTime} -> subscriptionCreationTime) (\s@EventSubscription' {} a -> s {subscriptionCreationTime = a} :: EventSubscription) Prelude.. Lens.mapping Data._Time

-- | The name of the Amazon Redshift event notification subscription.
eventSubscription_custSubscriptionId :: Lens.Lens' EventSubscription (Prelude.Maybe Prelude.Text)
eventSubscription_custSubscriptionId = Lens.lens (\EventSubscription' {custSubscriptionId} -> custSubscriptionId) (\s@EventSubscription' {} a -> s {custSubscriptionId = a} :: EventSubscription)

-- | A list of the sources that publish events to the Amazon Redshift event
-- notification subscription.
eventSubscription_sourceIdsList :: Lens.Lens' EventSubscription (Prelude.Maybe [Prelude.Text])
eventSubscription_sourceIdsList = Lens.lens (\EventSubscription' {sourceIdsList} -> sourceIdsList) (\s@EventSubscription' {} a -> s {sourceIdsList = a} :: EventSubscription) Prelude.. Lens.mapping Lens.coerced

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

-- | The source type of the events returned by the Amazon Redshift event
-- notification, such as cluster, cluster-snapshot,
-- cluster-parameter-group, cluster-security-group, or scheduled-action.
eventSubscription_sourceType :: Lens.Lens' EventSubscription (Prelude.Maybe Prelude.Text)
eventSubscription_sourceType = Lens.lens (\EventSubscription' {sourceType} -> sourceType) (\s@EventSubscription' {} a -> s {sourceType = a} :: EventSubscription)

-- | A boolean value indicating whether the subscription is enabled; @true@
-- indicates that the subscription is enabled.
eventSubscription_enabled :: Lens.Lens' EventSubscription (Prelude.Maybe Prelude.Bool)
eventSubscription_enabled = Lens.lens (\EventSubscription' {enabled} -> enabled) (\s@EventSubscription' {} a -> s {enabled = a} :: EventSubscription)

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic used by the event
-- notification subscription.
eventSubscription_snsTopicArn :: Lens.Lens' EventSubscription (Prelude.Maybe Prelude.Text)
eventSubscription_snsTopicArn = Lens.lens (\EventSubscription' {snsTopicArn} -> snsTopicArn) (\s@EventSubscription' {} a -> s {snsTopicArn = a} :: EventSubscription)

-- | The list of Amazon Redshift event categories specified in the event
-- notification subscription.
--
-- Values: Configuration, Management, Monitoring, Security, Pending
eventSubscription_eventCategoriesList :: Lens.Lens' EventSubscription (Prelude.Maybe [Prelude.Text])
eventSubscription_eventCategoriesList = Lens.lens (\EventSubscription' {eventCategoriesList} -> eventCategoriesList) (\s@EventSubscription' {} a -> s {eventCategoriesList = a} :: EventSubscription) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Web Services account associated with the Amazon Redshift
-- event notification subscription.
eventSubscription_customerAwsId :: Lens.Lens' EventSubscription (Prelude.Maybe Prelude.Text)
eventSubscription_customerAwsId = Lens.lens (\EventSubscription' {customerAwsId} -> customerAwsId) (\s@EventSubscription' {} a -> s {customerAwsId = a} :: EventSubscription)

instance Data.FromXML EventSubscription where
  parseXML x =
    EventSubscription'
      Prelude.<$> ( x Data..@? "Tags" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "Tag")
                  )
      Prelude.<*> (x Data..@? "Severity")
      Prelude.<*> (x Data..@? "SubscriptionCreationTime")
      Prelude.<*> (x Data..@? "CustSubscriptionId")
      Prelude.<*> ( x Data..@? "SourceIdsList" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "SourceId")
                  )
      Prelude.<*> (x Data..@? "Status")
      Prelude.<*> (x Data..@? "SourceType")
      Prelude.<*> (x Data..@? "Enabled")
      Prelude.<*> (x Data..@? "SnsTopicArn")
      Prelude.<*> ( x Data..@? "EventCategoriesList"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "EventCategory")
                  )
      Prelude.<*> (x Data..@? "CustomerAwsId")

instance Prelude.Hashable EventSubscription where
  hashWithSalt _salt EventSubscription' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` severity
      `Prelude.hashWithSalt` subscriptionCreationTime
      `Prelude.hashWithSalt` custSubscriptionId
      `Prelude.hashWithSalt` sourceIdsList
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` sourceType
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` snsTopicArn
      `Prelude.hashWithSalt` eventCategoriesList
      `Prelude.hashWithSalt` customerAwsId

instance Prelude.NFData EventSubscription where
  rnf EventSubscription' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf severity
      `Prelude.seq` Prelude.rnf subscriptionCreationTime
      `Prelude.seq` Prelude.rnf custSubscriptionId
      `Prelude.seq` Prelude.rnf sourceIdsList
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf sourceType
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf snsTopicArn
      `Prelude.seq` Prelude.rnf eventCategoriesList
      `Prelude.seq` Prelude.rnf customerAwsId
