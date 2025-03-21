{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DMS.CreateEventSubscription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an DMS event notification subscription.
--
-- You can specify the type of source (@SourceType@) you want to be
-- notified of, provide a list of DMS source IDs (@SourceIds@) that
-- triggers the events, and provide a list of event categories
-- (@EventCategories@) for events you want to be notified of. If you
-- specify both the @SourceType@ and @SourceIds@, such as
-- @SourceType = replication-instance@ and
-- @SourceIdentifier = my-replinstance@, you will be notified of all the
-- replication instance events for the specified source. If you specify a
-- @SourceType@ but don\'t specify a @SourceIdentifier@, you receive notice
-- of the events for that source type for all your DMS sources. If you
-- don\'t specify either @SourceType@ nor @SourceIdentifier@, you will be
-- notified of events generated from all DMS sources belonging to your
-- customer account.
--
-- For more information about DMS events, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Events.html Working with Events and Notifications>
-- in the /Database Migration Service User Guide./
module Amazonka.DMS.CreateEventSubscription
  ( -- * Creating a Request
    CreateEventSubscription (..),
    newCreateEventSubscription,

    -- * Request Lenses
    createEventSubscription_enabled,
    createEventSubscription_eventCategories,
    createEventSubscription_sourceIds,
    createEventSubscription_sourceType,
    createEventSubscription_tags,
    createEventSubscription_subscriptionName,
    createEventSubscription_snsTopicArn,

    -- * Destructuring the Response
    CreateEventSubscriptionResponse (..),
    newCreateEventSubscriptionResponse,

    -- * Response Lenses
    createEventSubscriptionResponse_eventSubscription,
    createEventSubscriptionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newCreateEventSubscription' smart constructor.
data CreateEventSubscription = CreateEventSubscription'
  { -- | A Boolean value; set to @true@ to activate the subscription, or set to
    -- @false@ to create the subscription but not activate it.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | A list of event categories for a source type that you want to subscribe
    -- to. For more information, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Events.html Working with Events and Notifications>
    -- in the /Database Migration Service User Guide./
    eventCategories :: Prelude.Maybe [Prelude.Text],
    -- | A list of identifiers for which DMS provides notification events.
    --
    -- If you don\'t specify a value, notifications are provided for all
    -- sources.
    --
    -- If you specify multiple values, they must be of the same type. For
    -- example, if you specify a database instance ID, then all of the other
    -- values must be database instance IDs.
    sourceIds :: Prelude.Maybe [Prelude.Text],
    -- | The type of DMS resource that generates the events. For example, if you
    -- want to be notified of events generated by a replication instance, you
    -- set this parameter to @replication-instance@. If this value isn\'t
    -- specified, all events are returned.
    --
    -- Valid values: @replication-instance@ | @replication-task@
    sourceType :: Prelude.Maybe Prelude.Text,
    -- | One or more tags to be assigned to the event subscription.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the DMS event notification subscription. This name must be
    -- less than 255 characters.
    subscriptionName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Amazon SNS topic created for event
    -- notification. The ARN is created by Amazon SNS when you create a topic
    -- and subscribe to it.
    snsTopicArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEventSubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'createEventSubscription_enabled' - A Boolean value; set to @true@ to activate the subscription, or set to
-- @false@ to create the subscription but not activate it.
--
-- 'eventCategories', 'createEventSubscription_eventCategories' - A list of event categories for a source type that you want to subscribe
-- to. For more information, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Events.html Working with Events and Notifications>
-- in the /Database Migration Service User Guide./
--
-- 'sourceIds', 'createEventSubscription_sourceIds' - A list of identifiers for which DMS provides notification events.
--
-- If you don\'t specify a value, notifications are provided for all
-- sources.
--
-- If you specify multiple values, they must be of the same type. For
-- example, if you specify a database instance ID, then all of the other
-- values must be database instance IDs.
--
-- 'sourceType', 'createEventSubscription_sourceType' - The type of DMS resource that generates the events. For example, if you
-- want to be notified of events generated by a replication instance, you
-- set this parameter to @replication-instance@. If this value isn\'t
-- specified, all events are returned.
--
-- Valid values: @replication-instance@ | @replication-task@
--
-- 'tags', 'createEventSubscription_tags' - One or more tags to be assigned to the event subscription.
--
-- 'subscriptionName', 'createEventSubscription_subscriptionName' - The name of the DMS event notification subscription. This name must be
-- less than 255 characters.
--
-- 'snsTopicArn', 'createEventSubscription_snsTopicArn' - The Amazon Resource Name (ARN) of the Amazon SNS topic created for event
-- notification. The ARN is created by Amazon SNS when you create a topic
-- and subscribe to it.
newCreateEventSubscription ::
  -- | 'subscriptionName'
  Prelude.Text ->
  -- | 'snsTopicArn'
  Prelude.Text ->
  CreateEventSubscription
newCreateEventSubscription
  pSubscriptionName_
  pSnsTopicArn_ =
    CreateEventSubscription'
      { enabled = Prelude.Nothing,
        eventCategories = Prelude.Nothing,
        sourceIds = Prelude.Nothing,
        sourceType = Prelude.Nothing,
        tags = Prelude.Nothing,
        subscriptionName = pSubscriptionName_,
        snsTopicArn = pSnsTopicArn_
      }

-- | A Boolean value; set to @true@ to activate the subscription, or set to
-- @false@ to create the subscription but not activate it.
createEventSubscription_enabled :: Lens.Lens' CreateEventSubscription (Prelude.Maybe Prelude.Bool)
createEventSubscription_enabled = Lens.lens (\CreateEventSubscription' {enabled} -> enabled) (\s@CreateEventSubscription' {} a -> s {enabled = a} :: CreateEventSubscription)

-- | A list of event categories for a source type that you want to subscribe
-- to. For more information, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Events.html Working with Events and Notifications>
-- in the /Database Migration Service User Guide./
createEventSubscription_eventCategories :: Lens.Lens' CreateEventSubscription (Prelude.Maybe [Prelude.Text])
createEventSubscription_eventCategories = Lens.lens (\CreateEventSubscription' {eventCategories} -> eventCategories) (\s@CreateEventSubscription' {} a -> s {eventCategories = a} :: CreateEventSubscription) Prelude.. Lens.mapping Lens.coerced

-- | A list of identifiers for which DMS provides notification events.
--
-- If you don\'t specify a value, notifications are provided for all
-- sources.
--
-- If you specify multiple values, they must be of the same type. For
-- example, if you specify a database instance ID, then all of the other
-- values must be database instance IDs.
createEventSubscription_sourceIds :: Lens.Lens' CreateEventSubscription (Prelude.Maybe [Prelude.Text])
createEventSubscription_sourceIds = Lens.lens (\CreateEventSubscription' {sourceIds} -> sourceIds) (\s@CreateEventSubscription' {} a -> s {sourceIds = a} :: CreateEventSubscription) Prelude.. Lens.mapping Lens.coerced

-- | The type of DMS resource that generates the events. For example, if you
-- want to be notified of events generated by a replication instance, you
-- set this parameter to @replication-instance@. If this value isn\'t
-- specified, all events are returned.
--
-- Valid values: @replication-instance@ | @replication-task@
createEventSubscription_sourceType :: Lens.Lens' CreateEventSubscription (Prelude.Maybe Prelude.Text)
createEventSubscription_sourceType = Lens.lens (\CreateEventSubscription' {sourceType} -> sourceType) (\s@CreateEventSubscription' {} a -> s {sourceType = a} :: CreateEventSubscription)

-- | One or more tags to be assigned to the event subscription.
createEventSubscription_tags :: Lens.Lens' CreateEventSubscription (Prelude.Maybe [Tag])
createEventSubscription_tags = Lens.lens (\CreateEventSubscription' {tags} -> tags) (\s@CreateEventSubscription' {} a -> s {tags = a} :: CreateEventSubscription) Prelude.. Lens.mapping Lens.coerced

-- | The name of the DMS event notification subscription. This name must be
-- less than 255 characters.
createEventSubscription_subscriptionName :: Lens.Lens' CreateEventSubscription Prelude.Text
createEventSubscription_subscriptionName = Lens.lens (\CreateEventSubscription' {subscriptionName} -> subscriptionName) (\s@CreateEventSubscription' {} a -> s {subscriptionName = a} :: CreateEventSubscription)

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic created for event
-- notification. The ARN is created by Amazon SNS when you create a topic
-- and subscribe to it.
createEventSubscription_snsTopicArn :: Lens.Lens' CreateEventSubscription Prelude.Text
createEventSubscription_snsTopicArn = Lens.lens (\CreateEventSubscription' {snsTopicArn} -> snsTopicArn) (\s@CreateEventSubscription' {} a -> s {snsTopicArn = a} :: CreateEventSubscription)

instance Core.AWSRequest CreateEventSubscription where
  type
    AWSResponse CreateEventSubscription =
      CreateEventSubscriptionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateEventSubscriptionResponse'
            Prelude.<$> (x Data..?> "EventSubscription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateEventSubscription where
  hashWithSalt _salt CreateEventSubscription' {..} =
    _salt
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` eventCategories
      `Prelude.hashWithSalt` sourceIds
      `Prelude.hashWithSalt` sourceType
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` subscriptionName
      `Prelude.hashWithSalt` snsTopicArn

instance Prelude.NFData CreateEventSubscription where
  rnf CreateEventSubscription' {..} =
    Prelude.rnf enabled `Prelude.seq`
      Prelude.rnf eventCategories `Prelude.seq`
        Prelude.rnf sourceIds `Prelude.seq`
          Prelude.rnf sourceType `Prelude.seq`
            Prelude.rnf tags `Prelude.seq`
              Prelude.rnf subscriptionName `Prelude.seq`
                Prelude.rnf snsTopicArn

instance Data.ToHeaders CreateEventSubscription where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonDMSv20160101.CreateEventSubscription" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateEventSubscription where
  toJSON CreateEventSubscription' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Enabled" Data..=) Prelude.<$> enabled,
            ("EventCategories" Data..=)
              Prelude.<$> eventCategories,
            ("SourceIds" Data..=) Prelude.<$> sourceIds,
            ("SourceType" Data..=) Prelude.<$> sourceType,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("SubscriptionName" Data..= subscriptionName),
            Prelude.Just ("SnsTopicArn" Data..= snsTopicArn)
          ]
      )

instance Data.ToPath CreateEventSubscription where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateEventSubscription where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newCreateEventSubscriptionResponse' smart constructor.
data CreateEventSubscriptionResponse = CreateEventSubscriptionResponse'
  { -- | The event subscription that was created.
    eventSubscription :: Prelude.Maybe EventSubscription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEventSubscriptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventSubscription', 'createEventSubscriptionResponse_eventSubscription' - The event subscription that was created.
--
-- 'httpStatus', 'createEventSubscriptionResponse_httpStatus' - The response's http status code.
newCreateEventSubscriptionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateEventSubscriptionResponse
newCreateEventSubscriptionResponse pHttpStatus_ =
  CreateEventSubscriptionResponse'
    { eventSubscription =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The event subscription that was created.
createEventSubscriptionResponse_eventSubscription :: Lens.Lens' CreateEventSubscriptionResponse (Prelude.Maybe EventSubscription)
createEventSubscriptionResponse_eventSubscription = Lens.lens (\CreateEventSubscriptionResponse' {eventSubscription} -> eventSubscription) (\s@CreateEventSubscriptionResponse' {} a -> s {eventSubscription = a} :: CreateEventSubscriptionResponse)

-- | The response's http status code.
createEventSubscriptionResponse_httpStatus :: Lens.Lens' CreateEventSubscriptionResponse Prelude.Int
createEventSubscriptionResponse_httpStatus = Lens.lens (\CreateEventSubscriptionResponse' {httpStatus} -> httpStatus) (\s@CreateEventSubscriptionResponse' {} a -> s {httpStatus = a} :: CreateEventSubscriptionResponse)

instance
  Prelude.NFData
    CreateEventSubscriptionResponse
  where
  rnf CreateEventSubscriptionResponse' {..} =
    Prelude.rnf eventSubscription `Prelude.seq`
      Prelude.rnf httpStatus
