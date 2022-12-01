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
-- Module      : Amazonka.RDS.CreateEventSubscription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an RDS event notification subscription. This operation requires
-- a topic Amazon Resource Name (ARN) created by either the RDS console,
-- the SNS console, or the SNS API. To obtain an ARN with SNS, you must
-- create a topic in Amazon SNS and subscribe to the topic. The ARN is
-- displayed in the SNS console.
--
-- You can specify the type of source (@SourceType@) that you want to be
-- notified of and provide a list of RDS sources (@SourceIds@) that
-- triggers the events. You can also provide a list of event categories
-- (@EventCategories@) for events that you want to be notified of. For
-- example, you can specify @SourceType@ = @db-instance@, @SourceIds@ =
-- @mydbinstance1@, @mydbinstance2@ and @EventCategories@ = @Availability@,
-- @Backup@.
--
-- If you specify both the @SourceType@ and @SourceIds@, such as
-- @SourceType@ = @db-instance@ and @SourceIds@ = @myDBInstance1@, you are
-- notified of all the @db-instance@ events for the specified source. If
-- you specify a @SourceType@ but do not specify @SourceIds@, you receive
-- notice of the events for that source type for all your RDS sources. If
-- you don\'t specify either the SourceType or the @SourceIds@, you are
-- notified of events generated from all RDS sources belonging to your
-- customer account.
--
-- For more information about subscribing to an event for RDS DB engines,
-- see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Events.Subscribing.html Subscribing to Amazon RDS event notification>
-- in the /Amazon RDS User Guide/.
--
-- For more information about subscribing to an event for Aurora DB
-- engines, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_Events.Subscribing.html Subscribing to Amazon RDS event notification>
-- in the /Amazon Aurora User Guide/.
module Amazonka.RDS.CreateEventSubscription
  ( -- * Creating a Request
    CreateEventSubscription (..),
    newCreateEventSubscription,

    -- * Request Lenses
    createEventSubscription_tags,
    createEventSubscription_sourceIds,
    createEventSubscription_sourceType,
    createEventSubscription_enabled,
    createEventSubscription_eventCategories,
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
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newCreateEventSubscription' smart constructor.
data CreateEventSubscription = CreateEventSubscription'
  { tags :: Prelude.Maybe [Tag],
    -- | The list of identifiers of the event sources for which events are
    -- returned. If not specified, then all sources are included in the
    -- response. An identifier must begin with a letter and must contain only
    -- ASCII letters, digits, and hyphens. It can\'t end with a hyphen or
    -- contain two consecutive hyphens.
    --
    -- Constraints:
    --
    -- -   If @SourceIds@ are supplied, @SourceType@ must also be provided.
    --
    -- -   If the source type is a DB instance, a @DBInstanceIdentifier@ value
    --     must be supplied.
    --
    -- -   If the source type is a DB cluster, a @DBClusterIdentifier@ value
    --     must be supplied.
    --
    -- -   If the source type is a DB parameter group, a @DBParameterGroupName@
    --     value must be supplied.
    --
    -- -   If the source type is a DB security group, a @DBSecurityGroupName@
    --     value must be supplied.
    --
    -- -   If the source type is a DB snapshot, a @DBSnapshotIdentifier@ value
    --     must be supplied.
    --
    -- -   If the source type is a DB cluster snapshot, a
    --     @DBClusterSnapshotIdentifier@ value must be supplied.
    --
    -- -   If the source type is an RDS Proxy, a @DBProxyName@ value must be
    --     supplied.
    sourceIds :: Prelude.Maybe [Prelude.Text],
    -- | The type of source that is generating the events. For example, if you
    -- want to be notified of events generated by a DB instance, you set this
    -- parameter to @db-instance@. For RDS Proxy events, specify @db-proxy@. If
    -- this value isn\'t specified, all events are returned.
    --
    -- Valid values: @db-instance@ | @db-cluster@ | @db-parameter-group@ |
    -- @db-security-group@ | @db-snapshot@ | @db-cluster-snapshot@ | @db-proxy@
    sourceType :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether to activate the subscription. If the
    -- event notification subscription isn\'t activated, the subscription is
    -- created but not active.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | A list of event categories for a particular source type (@SourceType@)
    -- that you want to subscribe to. You can see a list of the categories for
    -- a given source type in the \"Amazon RDS event categories and event
    -- messages\" section of the
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Events.Messages.html Amazon RDS User Guide>
    -- or the
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_Events.Messages.html Amazon Aurora User Guide>
    -- . You can also see this list by using the @DescribeEventCategories@
    -- operation.
    eventCategories :: Prelude.Maybe [Prelude.Text],
    -- | The name of the subscription.
    --
    -- Constraints: The name must be less than 255 characters.
    subscriptionName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the SNS topic created for event
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
-- 'tags', 'createEventSubscription_tags' - Undocumented member.
--
-- 'sourceIds', 'createEventSubscription_sourceIds' - The list of identifiers of the event sources for which events are
-- returned. If not specified, then all sources are included in the
-- response. An identifier must begin with a letter and must contain only
-- ASCII letters, digits, and hyphens. It can\'t end with a hyphen or
-- contain two consecutive hyphens.
--
-- Constraints:
--
-- -   If @SourceIds@ are supplied, @SourceType@ must also be provided.
--
-- -   If the source type is a DB instance, a @DBInstanceIdentifier@ value
--     must be supplied.
--
-- -   If the source type is a DB cluster, a @DBClusterIdentifier@ value
--     must be supplied.
--
-- -   If the source type is a DB parameter group, a @DBParameterGroupName@
--     value must be supplied.
--
-- -   If the source type is a DB security group, a @DBSecurityGroupName@
--     value must be supplied.
--
-- -   If the source type is a DB snapshot, a @DBSnapshotIdentifier@ value
--     must be supplied.
--
-- -   If the source type is a DB cluster snapshot, a
--     @DBClusterSnapshotIdentifier@ value must be supplied.
--
-- -   If the source type is an RDS Proxy, a @DBProxyName@ value must be
--     supplied.
--
-- 'sourceType', 'createEventSubscription_sourceType' - The type of source that is generating the events. For example, if you
-- want to be notified of events generated by a DB instance, you set this
-- parameter to @db-instance@. For RDS Proxy events, specify @db-proxy@. If
-- this value isn\'t specified, all events are returned.
--
-- Valid values: @db-instance@ | @db-cluster@ | @db-parameter-group@ |
-- @db-security-group@ | @db-snapshot@ | @db-cluster-snapshot@ | @db-proxy@
--
-- 'enabled', 'createEventSubscription_enabled' - A value that indicates whether to activate the subscription. If the
-- event notification subscription isn\'t activated, the subscription is
-- created but not active.
--
-- 'eventCategories', 'createEventSubscription_eventCategories' - A list of event categories for a particular source type (@SourceType@)
-- that you want to subscribe to. You can see a list of the categories for
-- a given source type in the \"Amazon RDS event categories and event
-- messages\" section of the
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Events.Messages.html Amazon RDS User Guide>
-- or the
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_Events.Messages.html Amazon Aurora User Guide>
-- . You can also see this list by using the @DescribeEventCategories@
-- operation.
--
-- 'subscriptionName', 'createEventSubscription_subscriptionName' - The name of the subscription.
--
-- Constraints: The name must be less than 255 characters.
--
-- 'snsTopicArn', 'createEventSubscription_snsTopicArn' - The Amazon Resource Name (ARN) of the SNS topic created for event
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
      { tags = Prelude.Nothing,
        sourceIds = Prelude.Nothing,
        sourceType = Prelude.Nothing,
        enabled = Prelude.Nothing,
        eventCategories = Prelude.Nothing,
        subscriptionName = pSubscriptionName_,
        snsTopicArn = pSnsTopicArn_
      }

-- | Undocumented member.
createEventSubscription_tags :: Lens.Lens' CreateEventSubscription (Prelude.Maybe [Tag])
createEventSubscription_tags = Lens.lens (\CreateEventSubscription' {tags} -> tags) (\s@CreateEventSubscription' {} a -> s {tags = a} :: CreateEventSubscription) Prelude.. Lens.mapping Lens.coerced

-- | The list of identifiers of the event sources for which events are
-- returned. If not specified, then all sources are included in the
-- response. An identifier must begin with a letter and must contain only
-- ASCII letters, digits, and hyphens. It can\'t end with a hyphen or
-- contain two consecutive hyphens.
--
-- Constraints:
--
-- -   If @SourceIds@ are supplied, @SourceType@ must also be provided.
--
-- -   If the source type is a DB instance, a @DBInstanceIdentifier@ value
--     must be supplied.
--
-- -   If the source type is a DB cluster, a @DBClusterIdentifier@ value
--     must be supplied.
--
-- -   If the source type is a DB parameter group, a @DBParameterGroupName@
--     value must be supplied.
--
-- -   If the source type is a DB security group, a @DBSecurityGroupName@
--     value must be supplied.
--
-- -   If the source type is a DB snapshot, a @DBSnapshotIdentifier@ value
--     must be supplied.
--
-- -   If the source type is a DB cluster snapshot, a
--     @DBClusterSnapshotIdentifier@ value must be supplied.
--
-- -   If the source type is an RDS Proxy, a @DBProxyName@ value must be
--     supplied.
createEventSubscription_sourceIds :: Lens.Lens' CreateEventSubscription (Prelude.Maybe [Prelude.Text])
createEventSubscription_sourceIds = Lens.lens (\CreateEventSubscription' {sourceIds} -> sourceIds) (\s@CreateEventSubscription' {} a -> s {sourceIds = a} :: CreateEventSubscription) Prelude.. Lens.mapping Lens.coerced

-- | The type of source that is generating the events. For example, if you
-- want to be notified of events generated by a DB instance, you set this
-- parameter to @db-instance@. For RDS Proxy events, specify @db-proxy@. If
-- this value isn\'t specified, all events are returned.
--
-- Valid values: @db-instance@ | @db-cluster@ | @db-parameter-group@ |
-- @db-security-group@ | @db-snapshot@ | @db-cluster-snapshot@ | @db-proxy@
createEventSubscription_sourceType :: Lens.Lens' CreateEventSubscription (Prelude.Maybe Prelude.Text)
createEventSubscription_sourceType = Lens.lens (\CreateEventSubscription' {sourceType} -> sourceType) (\s@CreateEventSubscription' {} a -> s {sourceType = a} :: CreateEventSubscription)

-- | A value that indicates whether to activate the subscription. If the
-- event notification subscription isn\'t activated, the subscription is
-- created but not active.
createEventSubscription_enabled :: Lens.Lens' CreateEventSubscription (Prelude.Maybe Prelude.Bool)
createEventSubscription_enabled = Lens.lens (\CreateEventSubscription' {enabled} -> enabled) (\s@CreateEventSubscription' {} a -> s {enabled = a} :: CreateEventSubscription)

-- | A list of event categories for a particular source type (@SourceType@)
-- that you want to subscribe to. You can see a list of the categories for
-- a given source type in the \"Amazon RDS event categories and event
-- messages\" section of the
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Events.Messages.html Amazon RDS User Guide>
-- or the
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_Events.Messages.html Amazon Aurora User Guide>
-- . You can also see this list by using the @DescribeEventCategories@
-- operation.
createEventSubscription_eventCategories :: Lens.Lens' CreateEventSubscription (Prelude.Maybe [Prelude.Text])
createEventSubscription_eventCategories = Lens.lens (\CreateEventSubscription' {eventCategories} -> eventCategories) (\s@CreateEventSubscription' {} a -> s {eventCategories = a} :: CreateEventSubscription) Prelude.. Lens.mapping Lens.coerced

-- | The name of the subscription.
--
-- Constraints: The name must be less than 255 characters.
createEventSubscription_subscriptionName :: Lens.Lens' CreateEventSubscription Prelude.Text
createEventSubscription_subscriptionName = Lens.lens (\CreateEventSubscription' {subscriptionName} -> subscriptionName) (\s@CreateEventSubscription' {} a -> s {subscriptionName = a} :: CreateEventSubscription)

-- | The Amazon Resource Name (ARN) of the SNS topic created for event
-- notification. The ARN is created by Amazon SNS when you create a topic
-- and subscribe to it.
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
            Prelude.<$> (x Core..@? "EventSubscription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateEventSubscription where
  hashWithSalt _salt CreateEventSubscription' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` sourceIds
      `Prelude.hashWithSalt` sourceType
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` eventCategories
      `Prelude.hashWithSalt` subscriptionName
      `Prelude.hashWithSalt` snsTopicArn

instance Prelude.NFData CreateEventSubscription where
  rnf CreateEventSubscription' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf sourceIds
      `Prelude.seq` Prelude.rnf sourceType
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf eventCategories
      `Prelude.seq` Prelude.rnf subscriptionName
      `Prelude.seq` Prelude.rnf snsTopicArn

instance Core.ToHeaders CreateEventSubscription where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateEventSubscription where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateEventSubscription where
  toQuery CreateEventSubscription' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("CreateEventSubscription" :: Prelude.ByteString),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "Tags"
          Core.=: Core.toQuery
            (Core.toQueryList "Tag" Prelude.<$> tags),
        "SourceIds"
          Core.=: Core.toQuery
            (Core.toQueryList "SourceId" Prelude.<$> sourceIds),
        "SourceType" Core.=: sourceType,
        "Enabled" Core.=: enabled,
        "EventCategories"
          Core.=: Core.toQuery
            ( Core.toQueryList "EventCategory"
                Prelude.<$> eventCategories
            ),
        "SubscriptionName" Core.=: subscriptionName,
        "SnsTopicArn" Core.=: snsTopicArn
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
    Prelude.rnf eventSubscription
      `Prelude.seq` Prelude.rnf httpStatus
