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
-- Module      : Network.AWS.RDS.Types.EventSubscription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.EventSubscription where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains the results of a successful invocation of the
-- @DescribeEventSubscriptions@ action.
--
-- /See:/ 'newEventSubscription' smart constructor.
data EventSubscription = EventSubscription'
  { -- | The RDS event notification subscription Id.
    custSubscriptionId :: Prelude.Maybe Prelude.Text,
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
    status :: Prelude.Maybe Prelude.Text,
    -- | A list of source IDs for the RDS event notification subscription.
    sourceIdsList :: Prelude.Maybe [Prelude.Text],
    -- | A list of event categories for the RDS event notification subscription.
    eventCategoriesList :: Prelude.Maybe [Prelude.Text],
    -- | A Boolean value indicating if the subscription is enabled. True
    -- indicates the subscription is enabled.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) for the event subscription.
    eventSubscriptionArn :: Prelude.Maybe Prelude.Text,
    -- | The time the RDS event notification subscription was created.
    subscriptionCreationTime :: Prelude.Maybe Prelude.Text,
    -- | The AWS customer account associated with the RDS event notification
    -- subscription.
    customerAwsId :: Prelude.Maybe Prelude.Text,
    -- | The source type for the RDS event notification subscription.
    sourceType :: Prelude.Maybe Prelude.Text,
    -- | The topic ARN of the RDS event notification subscription.
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
        Prelude.Nothing,
      status = Prelude.Nothing,
      sourceIdsList = Prelude.Nothing,
      eventCategoriesList = Prelude.Nothing,
      enabled = Prelude.Nothing,
      eventSubscriptionArn = Prelude.Nothing,
      subscriptionCreationTime = Prelude.Nothing,
      customerAwsId = Prelude.Nothing,
      sourceType = Prelude.Nothing,
      snsTopicArn = Prelude.Nothing
    }

-- | The RDS event notification subscription Id.
eventSubscription_custSubscriptionId :: Lens.Lens' EventSubscription (Prelude.Maybe Prelude.Text)
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
eventSubscription_status :: Lens.Lens' EventSubscription (Prelude.Maybe Prelude.Text)
eventSubscription_status = Lens.lens (\EventSubscription' {status} -> status) (\s@EventSubscription' {} a -> s {status = a} :: EventSubscription)

-- | A list of source IDs for the RDS event notification subscription.
eventSubscription_sourceIdsList :: Lens.Lens' EventSubscription (Prelude.Maybe [Prelude.Text])
eventSubscription_sourceIdsList = Lens.lens (\EventSubscription' {sourceIdsList} -> sourceIdsList) (\s@EventSubscription' {} a -> s {sourceIdsList = a} :: EventSubscription) Prelude.. Lens.mapping Prelude._Coerce

-- | A list of event categories for the RDS event notification subscription.
eventSubscription_eventCategoriesList :: Lens.Lens' EventSubscription (Prelude.Maybe [Prelude.Text])
eventSubscription_eventCategoriesList = Lens.lens (\EventSubscription' {eventCategoriesList} -> eventCategoriesList) (\s@EventSubscription' {} a -> s {eventCategoriesList = a} :: EventSubscription) Prelude.. Lens.mapping Prelude._Coerce

-- | A Boolean value indicating if the subscription is enabled. True
-- indicates the subscription is enabled.
eventSubscription_enabled :: Lens.Lens' EventSubscription (Prelude.Maybe Prelude.Bool)
eventSubscription_enabled = Lens.lens (\EventSubscription' {enabled} -> enabled) (\s@EventSubscription' {} a -> s {enabled = a} :: EventSubscription)

-- | The Amazon Resource Name (ARN) for the event subscription.
eventSubscription_eventSubscriptionArn :: Lens.Lens' EventSubscription (Prelude.Maybe Prelude.Text)
eventSubscription_eventSubscriptionArn = Lens.lens (\EventSubscription' {eventSubscriptionArn} -> eventSubscriptionArn) (\s@EventSubscription' {} a -> s {eventSubscriptionArn = a} :: EventSubscription)

-- | The time the RDS event notification subscription was created.
eventSubscription_subscriptionCreationTime :: Lens.Lens' EventSubscription (Prelude.Maybe Prelude.Text)
eventSubscription_subscriptionCreationTime = Lens.lens (\EventSubscription' {subscriptionCreationTime} -> subscriptionCreationTime) (\s@EventSubscription' {} a -> s {subscriptionCreationTime = a} :: EventSubscription)

-- | The AWS customer account associated with the RDS event notification
-- subscription.
eventSubscription_customerAwsId :: Lens.Lens' EventSubscription (Prelude.Maybe Prelude.Text)
eventSubscription_customerAwsId = Lens.lens (\EventSubscription' {customerAwsId} -> customerAwsId) (\s@EventSubscription' {} a -> s {customerAwsId = a} :: EventSubscription)

-- | The source type for the RDS event notification subscription.
eventSubscription_sourceType :: Lens.Lens' EventSubscription (Prelude.Maybe Prelude.Text)
eventSubscription_sourceType = Lens.lens (\EventSubscription' {sourceType} -> sourceType) (\s@EventSubscription' {} a -> s {sourceType = a} :: EventSubscription)

-- | The topic ARN of the RDS event notification subscription.
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
      Prelude.<*> ( x Prelude..@? "EventCategoriesList"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "EventCategory")
                  )
      Prelude.<*> (x Prelude..@? "Enabled")
      Prelude.<*> (x Prelude..@? "EventSubscriptionArn")
      Prelude.<*> (x Prelude..@? "SubscriptionCreationTime")
      Prelude.<*> (x Prelude..@? "CustomerAwsId")
      Prelude.<*> (x Prelude..@? "SourceType")
      Prelude.<*> (x Prelude..@? "SnsTopicArn")

instance Prelude.Hashable EventSubscription

instance Prelude.NFData EventSubscription
