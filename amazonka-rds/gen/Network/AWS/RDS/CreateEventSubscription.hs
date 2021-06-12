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
-- Module      : Network.AWS.RDS.CreateEventSubscription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an RDS event notification subscription. This action requires a
-- topic Amazon Resource Name (ARN) created by either the RDS console, the
-- SNS console, or the SNS API. To obtain an ARN with SNS, you must create
-- a topic in Amazon SNS and subscribe to the topic. The ARN is displayed
-- in the SNS console.
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
-- @SourceType@ = @db-instance@ and @SourceIdentifier@ = @myDBInstance1@,
-- you are notified of all the @db-instance@ events for the specified
-- source. If you specify a @SourceType@ but do not specify a
-- @SourceIdentifier@, you receive notice of the events for that source
-- type for all your RDS sources. If you don\'t specify either the
-- SourceType or the @SourceIdentifier@, you are notified of events
-- generated from all RDS sources belonging to your customer account.
--
-- RDS event notification is only available for unencrypted SNS topics. If
-- you specify an encrypted SNS topic, event notifications aren\'t sent for
-- the topic.
module Network.AWS.RDS.CreateEventSubscription
  ( -- * Creating a Request
    CreateEventSubscription (..),
    newCreateEventSubscription,

    -- * Request Lenses
    createEventSubscription_sourceIds,
    createEventSubscription_enabled,
    createEventSubscription_eventCategories,
    createEventSubscription_tags,
    createEventSubscription_sourceType,
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newCreateEventSubscription' smart constructor.
data CreateEventSubscription = CreateEventSubscription'
  { -- | The list of identifiers of the event sources for which events are
    -- returned. If not specified, then all sources are included in the
    -- response. An identifier must begin with a letter and must contain only
    -- ASCII letters, digits, and hyphens. It can\'t end with a hyphen or
    -- contain two consecutive hyphens.
    --
    -- Constraints:
    --
    -- -   If a @SourceIds@ value is supplied, @SourceType@ must also be
    --     provided.
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
    sourceIds :: Core.Maybe [Core.Text],
    -- | A value that indicates whether to activate the subscription. If the
    -- event notification subscription isn\'t activated, the subscription is
    -- created but not active.
    enabled :: Core.Maybe Core.Bool,
    -- | A list of event categories for a particular source type (@SourceType@)
    -- that you want to subscribe to. You can see a list of the categories for
    -- a given source type in
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Events.html Events>
    -- in the /Amazon RDS User Guide/ or by using the @DescribeEventCategories@
    -- operation.
    eventCategories :: Core.Maybe [Core.Text],
    tags :: Core.Maybe [Tag],
    -- | The type of source that is generating the events. For example, if you
    -- want to be notified of events generated by a DB instance, you set this
    -- parameter to @db-instance@. If this value isn\'t specified, all events
    -- are returned.
    --
    -- Valid values: @db-instance@ | @db-cluster@ | @db-parameter-group@ |
    -- @db-security-group@ | @db-snapshot@ | @db-cluster-snapshot@
    sourceType :: Core.Maybe Core.Text,
    -- | The name of the subscription.
    --
    -- Constraints: The name must be less than 255 characters.
    subscriptionName :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the SNS topic created for event
    -- notification. The ARN is created by Amazon SNS when you create a topic
    -- and subscribe to it.
    snsTopicArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateEventSubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceIds', 'createEventSubscription_sourceIds' - The list of identifiers of the event sources for which events are
-- returned. If not specified, then all sources are included in the
-- response. An identifier must begin with a letter and must contain only
-- ASCII letters, digits, and hyphens. It can\'t end with a hyphen or
-- contain two consecutive hyphens.
--
-- Constraints:
--
-- -   If a @SourceIds@ value is supplied, @SourceType@ must also be
--     provided.
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
-- 'enabled', 'createEventSubscription_enabled' - A value that indicates whether to activate the subscription. If the
-- event notification subscription isn\'t activated, the subscription is
-- created but not active.
--
-- 'eventCategories', 'createEventSubscription_eventCategories' - A list of event categories for a particular source type (@SourceType@)
-- that you want to subscribe to. You can see a list of the categories for
-- a given source type in
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Events.html Events>
-- in the /Amazon RDS User Guide/ or by using the @DescribeEventCategories@
-- operation.
--
-- 'tags', 'createEventSubscription_tags' - Undocumented member.
--
-- 'sourceType', 'createEventSubscription_sourceType' - The type of source that is generating the events. For example, if you
-- want to be notified of events generated by a DB instance, you set this
-- parameter to @db-instance@. If this value isn\'t specified, all events
-- are returned.
--
-- Valid values: @db-instance@ | @db-cluster@ | @db-parameter-group@ |
-- @db-security-group@ | @db-snapshot@ | @db-cluster-snapshot@
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
  Core.Text ->
  -- | 'snsTopicArn'
  Core.Text ->
  CreateEventSubscription
newCreateEventSubscription
  pSubscriptionName_
  pSnsTopicArn_ =
    CreateEventSubscription'
      { sourceIds = Core.Nothing,
        enabled = Core.Nothing,
        eventCategories = Core.Nothing,
        tags = Core.Nothing,
        sourceType = Core.Nothing,
        subscriptionName = pSubscriptionName_,
        snsTopicArn = pSnsTopicArn_
      }

-- | The list of identifiers of the event sources for which events are
-- returned. If not specified, then all sources are included in the
-- response. An identifier must begin with a letter and must contain only
-- ASCII letters, digits, and hyphens. It can\'t end with a hyphen or
-- contain two consecutive hyphens.
--
-- Constraints:
--
-- -   If a @SourceIds@ value is supplied, @SourceType@ must also be
--     provided.
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
createEventSubscription_sourceIds :: Lens.Lens' CreateEventSubscription (Core.Maybe [Core.Text])
createEventSubscription_sourceIds = Lens.lens (\CreateEventSubscription' {sourceIds} -> sourceIds) (\s@CreateEventSubscription' {} a -> s {sourceIds = a} :: CreateEventSubscription) Core.. Lens.mapping Lens._Coerce

-- | A value that indicates whether to activate the subscription. If the
-- event notification subscription isn\'t activated, the subscription is
-- created but not active.
createEventSubscription_enabled :: Lens.Lens' CreateEventSubscription (Core.Maybe Core.Bool)
createEventSubscription_enabled = Lens.lens (\CreateEventSubscription' {enabled} -> enabled) (\s@CreateEventSubscription' {} a -> s {enabled = a} :: CreateEventSubscription)

-- | A list of event categories for a particular source type (@SourceType@)
-- that you want to subscribe to. You can see a list of the categories for
-- a given source type in
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Events.html Events>
-- in the /Amazon RDS User Guide/ or by using the @DescribeEventCategories@
-- operation.
createEventSubscription_eventCategories :: Lens.Lens' CreateEventSubscription (Core.Maybe [Core.Text])
createEventSubscription_eventCategories = Lens.lens (\CreateEventSubscription' {eventCategories} -> eventCategories) (\s@CreateEventSubscription' {} a -> s {eventCategories = a} :: CreateEventSubscription) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
createEventSubscription_tags :: Lens.Lens' CreateEventSubscription (Core.Maybe [Tag])
createEventSubscription_tags = Lens.lens (\CreateEventSubscription' {tags} -> tags) (\s@CreateEventSubscription' {} a -> s {tags = a} :: CreateEventSubscription) Core.. Lens.mapping Lens._Coerce

-- | The type of source that is generating the events. For example, if you
-- want to be notified of events generated by a DB instance, you set this
-- parameter to @db-instance@. If this value isn\'t specified, all events
-- are returned.
--
-- Valid values: @db-instance@ | @db-cluster@ | @db-parameter-group@ |
-- @db-security-group@ | @db-snapshot@ | @db-cluster-snapshot@
createEventSubscription_sourceType :: Lens.Lens' CreateEventSubscription (Core.Maybe Core.Text)
createEventSubscription_sourceType = Lens.lens (\CreateEventSubscription' {sourceType} -> sourceType) (\s@CreateEventSubscription' {} a -> s {sourceType = a} :: CreateEventSubscription)

-- | The name of the subscription.
--
-- Constraints: The name must be less than 255 characters.
createEventSubscription_subscriptionName :: Lens.Lens' CreateEventSubscription Core.Text
createEventSubscription_subscriptionName = Lens.lens (\CreateEventSubscription' {subscriptionName} -> subscriptionName) (\s@CreateEventSubscription' {} a -> s {subscriptionName = a} :: CreateEventSubscription)

-- | The Amazon Resource Name (ARN) of the SNS topic created for event
-- notification. The ARN is created by Amazon SNS when you create a topic
-- and subscribe to it.
createEventSubscription_snsTopicArn :: Lens.Lens' CreateEventSubscription Core.Text
createEventSubscription_snsTopicArn = Lens.lens (\CreateEventSubscription' {snsTopicArn} -> snsTopicArn) (\s@CreateEventSubscription' {} a -> s {snsTopicArn = a} :: CreateEventSubscription)

instance Core.AWSRequest CreateEventSubscription where
  type
    AWSResponse CreateEventSubscription =
      CreateEventSubscriptionResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CreateEventSubscriptionResult"
      ( \s h x ->
          CreateEventSubscriptionResponse'
            Core.<$> (x Core..@? "EventSubscription")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateEventSubscription

instance Core.NFData CreateEventSubscription

instance Core.ToHeaders CreateEventSubscription where
  toHeaders = Core.const Core.mempty

instance Core.ToPath CreateEventSubscription where
  toPath = Core.const "/"

instance Core.ToQuery CreateEventSubscription where
  toQuery CreateEventSubscription' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("CreateEventSubscription" :: Core.ByteString),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "SourceIds"
          Core.=: Core.toQuery
            (Core.toQueryList "SourceId" Core.<$> sourceIds),
        "Enabled" Core.=: enabled,
        "EventCategories"
          Core.=: Core.toQuery
            ( Core.toQueryList "EventCategory"
                Core.<$> eventCategories
            ),
        "Tags"
          Core.=: Core.toQuery (Core.toQueryList "Tag" Core.<$> tags),
        "SourceType" Core.=: sourceType,
        "SubscriptionName" Core.=: subscriptionName,
        "SnsTopicArn" Core.=: snsTopicArn
      ]

-- | /See:/ 'newCreateEventSubscriptionResponse' smart constructor.
data CreateEventSubscriptionResponse = CreateEventSubscriptionResponse'
  { eventSubscription :: Core.Maybe EventSubscription,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  CreateEventSubscriptionResponse
newCreateEventSubscriptionResponse pHttpStatus_ =
  CreateEventSubscriptionResponse'
    { eventSubscription =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createEventSubscriptionResponse_eventSubscription :: Lens.Lens' CreateEventSubscriptionResponse (Core.Maybe EventSubscription)
createEventSubscriptionResponse_eventSubscription = Lens.lens (\CreateEventSubscriptionResponse' {eventSubscription} -> eventSubscription) (\s@CreateEventSubscriptionResponse' {} a -> s {eventSubscription = a} :: CreateEventSubscriptionResponse)

-- | The response's http status code.
createEventSubscriptionResponse_httpStatus :: Lens.Lens' CreateEventSubscriptionResponse Core.Int
createEventSubscriptionResponse_httpStatus = Lens.lens (\CreateEventSubscriptionResponse' {httpStatus} -> httpStatus) (\s@CreateEventSubscriptionResponse' {} a -> s {httpStatus = a} :: CreateEventSubscriptionResponse)

instance Core.NFData CreateEventSubscriptionResponse
