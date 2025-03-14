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
-- Module      : Amazonka.DocumentDB.CreateEventSubscription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon DocumentDB event notification subscription. This
-- action requires a topic Amazon Resource Name (ARN) created by using the
-- Amazon DocumentDB console, the Amazon SNS console, or the Amazon SNS
-- API. To obtain an ARN with Amazon SNS, you must create a topic in Amazon
-- SNS and subscribe to the topic. The ARN is displayed in the Amazon SNS
-- console.
--
-- You can specify the type of source (@SourceType@) that you want to be
-- notified of. You can also provide a list of Amazon DocumentDB sources
-- (@SourceIds@) that trigger the events, and you can provide a list of
-- event categories (@EventCategories@) for events that you want to be
-- notified of. For example, you can specify @SourceType = db-instance@,
-- @SourceIds = mydbinstance1, mydbinstance2@ and
-- @EventCategories = Availability, Backup@.
--
-- If you specify both the @SourceType@ and @SourceIds@ (such as
-- @SourceType = db-instance@ and @SourceIdentifier = myDBInstance1@), you
-- are notified of all the @db-instance@ events for the specified source.
-- If you specify a @SourceType@ but do not specify a @SourceIdentifier@,
-- you receive notice of the events for that source type for all your
-- Amazon DocumentDB sources. If you do not specify either the @SourceType@
-- or the @SourceIdentifier@, you are notified of events generated from all
-- Amazon DocumentDB sources belonging to your customer account.
module Amazonka.DocumentDB.CreateEventSubscription
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
import qualified Amazonka.Data as Data
import Amazonka.DocumentDB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input to CreateEventSubscription.
--
-- /See:/ 'newCreateEventSubscription' smart constructor.
data CreateEventSubscription = CreateEventSubscription'
  { -- | A Boolean value; set to @true@ to activate the subscription, set to
    -- @false@ to create the subscription but not active it.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | A list of event categories for a @SourceType@ that you want to subscribe
    -- to.
    eventCategories :: Prelude.Maybe [Prelude.Text],
    -- | The list of identifiers of the event sources for which events are
    -- returned. If not specified, then all sources are included in the
    -- response. An identifier must begin with a letter and must contain only
    -- ASCII letters, digits, and hyphens; it can\'t end with a hyphen or
    -- contain two consecutive hyphens.
    --
    -- Constraints:
    --
    -- -   If @SourceIds@ are provided, @SourceType@ must also be provided.
    --
    -- -   If the source type is an instance, a @DBInstanceIdentifier@ must be
    --     provided.
    --
    -- -   If the source type is a security group, a @DBSecurityGroupName@ must
    --     be provided.
    --
    -- -   If the source type is a parameter group, a @DBParameterGroupName@
    --     must be provided.
    --
    -- -   If the source type is a snapshot, a @DBSnapshotIdentifier@ must be
    --     provided.
    sourceIds :: Prelude.Maybe [Prelude.Text],
    -- | The type of source that is generating the events. For example, if you
    -- want to be notified of events generated by an instance, you would set
    -- this parameter to @db-instance@. If this value is not specified, all
    -- events are returned.
    --
    -- Valid values: @db-instance@, @db-cluster@, @db-parameter-group@,
    -- @db-security-group@, @db-cluster-snapshot@
    sourceType :: Prelude.Maybe Prelude.Text,
    -- | The tags to be assigned to the event subscription.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the subscription.
    --
    -- Constraints: The name must be fewer than 255 characters.
    subscriptionName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the SNS topic created for event
    -- notification. Amazon SNS creates the ARN when you create a topic and
    -- subscribe to it.
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
-- 'enabled', 'createEventSubscription_enabled' - A Boolean value; set to @true@ to activate the subscription, set to
-- @false@ to create the subscription but not active it.
--
-- 'eventCategories', 'createEventSubscription_eventCategories' - A list of event categories for a @SourceType@ that you want to subscribe
-- to.
--
-- 'sourceIds', 'createEventSubscription_sourceIds' - The list of identifiers of the event sources for which events are
-- returned. If not specified, then all sources are included in the
-- response. An identifier must begin with a letter and must contain only
-- ASCII letters, digits, and hyphens; it can\'t end with a hyphen or
-- contain two consecutive hyphens.
--
-- Constraints:
--
-- -   If @SourceIds@ are provided, @SourceType@ must also be provided.
--
-- -   If the source type is an instance, a @DBInstanceIdentifier@ must be
--     provided.
--
-- -   If the source type is a security group, a @DBSecurityGroupName@ must
--     be provided.
--
-- -   If the source type is a parameter group, a @DBParameterGroupName@
--     must be provided.
--
-- -   If the source type is a snapshot, a @DBSnapshotIdentifier@ must be
--     provided.
--
-- 'sourceType', 'createEventSubscription_sourceType' - The type of source that is generating the events. For example, if you
-- want to be notified of events generated by an instance, you would set
-- this parameter to @db-instance@. If this value is not specified, all
-- events are returned.
--
-- Valid values: @db-instance@, @db-cluster@, @db-parameter-group@,
-- @db-security-group@, @db-cluster-snapshot@
--
-- 'tags', 'createEventSubscription_tags' - The tags to be assigned to the event subscription.
--
-- 'subscriptionName', 'createEventSubscription_subscriptionName' - The name of the subscription.
--
-- Constraints: The name must be fewer than 255 characters.
--
-- 'snsTopicArn', 'createEventSubscription_snsTopicArn' - The Amazon Resource Name (ARN) of the SNS topic created for event
-- notification. Amazon SNS creates the ARN when you create a topic and
-- subscribe to it.
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

-- | A Boolean value; set to @true@ to activate the subscription, set to
-- @false@ to create the subscription but not active it.
createEventSubscription_enabled :: Lens.Lens' CreateEventSubscription (Prelude.Maybe Prelude.Bool)
createEventSubscription_enabled = Lens.lens (\CreateEventSubscription' {enabled} -> enabled) (\s@CreateEventSubscription' {} a -> s {enabled = a} :: CreateEventSubscription)

-- | A list of event categories for a @SourceType@ that you want to subscribe
-- to.
createEventSubscription_eventCategories :: Lens.Lens' CreateEventSubscription (Prelude.Maybe [Prelude.Text])
createEventSubscription_eventCategories = Lens.lens (\CreateEventSubscription' {eventCategories} -> eventCategories) (\s@CreateEventSubscription' {} a -> s {eventCategories = a} :: CreateEventSubscription) Prelude.. Lens.mapping Lens.coerced

-- | The list of identifiers of the event sources for which events are
-- returned. If not specified, then all sources are included in the
-- response. An identifier must begin with a letter and must contain only
-- ASCII letters, digits, and hyphens; it can\'t end with a hyphen or
-- contain two consecutive hyphens.
--
-- Constraints:
--
-- -   If @SourceIds@ are provided, @SourceType@ must also be provided.
--
-- -   If the source type is an instance, a @DBInstanceIdentifier@ must be
--     provided.
--
-- -   If the source type is a security group, a @DBSecurityGroupName@ must
--     be provided.
--
-- -   If the source type is a parameter group, a @DBParameterGroupName@
--     must be provided.
--
-- -   If the source type is a snapshot, a @DBSnapshotIdentifier@ must be
--     provided.
createEventSubscription_sourceIds :: Lens.Lens' CreateEventSubscription (Prelude.Maybe [Prelude.Text])
createEventSubscription_sourceIds = Lens.lens (\CreateEventSubscription' {sourceIds} -> sourceIds) (\s@CreateEventSubscription' {} a -> s {sourceIds = a} :: CreateEventSubscription) Prelude.. Lens.mapping Lens.coerced

-- | The type of source that is generating the events. For example, if you
-- want to be notified of events generated by an instance, you would set
-- this parameter to @db-instance@. If this value is not specified, all
-- events are returned.
--
-- Valid values: @db-instance@, @db-cluster@, @db-parameter-group@,
-- @db-security-group@, @db-cluster-snapshot@
createEventSubscription_sourceType :: Lens.Lens' CreateEventSubscription (Prelude.Maybe Prelude.Text)
createEventSubscription_sourceType = Lens.lens (\CreateEventSubscription' {sourceType} -> sourceType) (\s@CreateEventSubscription' {} a -> s {sourceType = a} :: CreateEventSubscription)

-- | The tags to be assigned to the event subscription.
createEventSubscription_tags :: Lens.Lens' CreateEventSubscription (Prelude.Maybe [Tag])
createEventSubscription_tags = Lens.lens (\CreateEventSubscription' {tags} -> tags) (\s@CreateEventSubscription' {} a -> s {tags = a} :: CreateEventSubscription) Prelude.. Lens.mapping Lens.coerced

-- | The name of the subscription.
--
-- Constraints: The name must be fewer than 255 characters.
createEventSubscription_subscriptionName :: Lens.Lens' CreateEventSubscription Prelude.Text
createEventSubscription_subscriptionName = Lens.lens (\CreateEventSubscription' {subscriptionName} -> subscriptionName) (\s@CreateEventSubscription' {} a -> s {subscriptionName = a} :: CreateEventSubscription)

-- | The Amazon Resource Name (ARN) of the SNS topic created for event
-- notification. Amazon SNS creates the ARN when you create a topic and
-- subscribe to it.
createEventSubscription_snsTopicArn :: Lens.Lens' CreateEventSubscription Prelude.Text
createEventSubscription_snsTopicArn = Lens.lens (\CreateEventSubscription' {snsTopicArn} -> snsTopicArn) (\s@CreateEventSubscription' {} a -> s {snsTopicArn = a} :: CreateEventSubscription)

instance Core.AWSRequest CreateEventSubscription where
  type
    AWSResponse CreateEventSubscription =
      CreateEventSubscriptionResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CreateEventSubscriptionResult"
      ( \s h x ->
          CreateEventSubscriptionResponse'
            Prelude.<$> (x Data..@? "EventSubscription")
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
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateEventSubscription where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateEventSubscription where
  toQuery CreateEventSubscription' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateEventSubscription" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "Enabled" Data.=: enabled,
        "EventCategories"
          Data.=: Data.toQuery
            ( Data.toQueryList "EventCategory"
                Prelude.<$> eventCategories
            ),
        "SourceIds"
          Data.=: Data.toQuery
            (Data.toQueryList "SourceId" Prelude.<$> sourceIds),
        "SourceType" Data.=: sourceType,
        "Tags"
          Data.=: Data.toQuery
            (Data.toQueryList "Tag" Prelude.<$> tags),
        "SubscriptionName" Data.=: subscriptionName,
        "SnsTopicArn" Data.=: snsTopicArn
      ]

-- | /See:/ 'newCreateEventSubscriptionResponse' smart constructor.
data CreateEventSubscriptionResponse = CreateEventSubscriptionResponse'
  { eventSubscription :: Prelude.Maybe EventSubscription,
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
-- 'eventSubscription', 'createEventSubscriptionResponse_eventSubscription' - Undocumented member.
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

-- | Undocumented member.
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
