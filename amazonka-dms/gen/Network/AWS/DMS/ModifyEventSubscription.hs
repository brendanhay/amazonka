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
-- Module      : Network.AWS.DMS.ModifyEventSubscription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies an existing AWS DMS event notification subscription.
module Network.AWS.DMS.ModifyEventSubscription
  ( -- * Creating a Request
    ModifyEventSubscription (..),
    newModifyEventSubscription,

    -- * Request Lenses
    modifyEventSubscription_enabled,
    modifyEventSubscription_eventCategories,
    modifyEventSubscription_sourceType,
    modifyEventSubscription_snsTopicArn,
    modifyEventSubscription_subscriptionName,

    -- * Destructuring the Response
    ModifyEventSubscriptionResponse (..),
    newModifyEventSubscriptionResponse,

    -- * Response Lenses
    modifyEventSubscriptionResponse_eventSubscription,
    modifyEventSubscriptionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newModifyEventSubscription' smart constructor.
data ModifyEventSubscription = ModifyEventSubscription'
  { -- | A Boolean value; set to __true__ to activate the subscription.
    enabled :: Core.Maybe Core.Bool,
    -- | A list of event categories for a source type that you want to subscribe
    -- to. Use the @DescribeEventCategories@ action to see a list of event
    -- categories.
    eventCategories :: Core.Maybe [Core.Text],
    -- | The type of AWS DMS resource that generates the events you want to
    -- subscribe to.
    --
    -- Valid values: replication-instance | replication-task
    sourceType :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the Amazon SNS topic created for event
    -- notification. The ARN is created by Amazon SNS when you create a topic
    -- and subscribe to it.
    snsTopicArn :: Core.Maybe Core.Text,
    -- | The name of the AWS DMS event notification subscription to be modified.
    subscriptionName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyEventSubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'modifyEventSubscription_enabled' - A Boolean value; set to __true__ to activate the subscription.
--
-- 'eventCategories', 'modifyEventSubscription_eventCategories' - A list of event categories for a source type that you want to subscribe
-- to. Use the @DescribeEventCategories@ action to see a list of event
-- categories.
--
-- 'sourceType', 'modifyEventSubscription_sourceType' - The type of AWS DMS resource that generates the events you want to
-- subscribe to.
--
-- Valid values: replication-instance | replication-task
--
-- 'snsTopicArn', 'modifyEventSubscription_snsTopicArn' - The Amazon Resource Name (ARN) of the Amazon SNS topic created for event
-- notification. The ARN is created by Amazon SNS when you create a topic
-- and subscribe to it.
--
-- 'subscriptionName', 'modifyEventSubscription_subscriptionName' - The name of the AWS DMS event notification subscription to be modified.
newModifyEventSubscription ::
  -- | 'subscriptionName'
  Core.Text ->
  ModifyEventSubscription
newModifyEventSubscription pSubscriptionName_ =
  ModifyEventSubscription'
    { enabled = Core.Nothing,
      eventCategories = Core.Nothing,
      sourceType = Core.Nothing,
      snsTopicArn = Core.Nothing,
      subscriptionName = pSubscriptionName_
    }

-- | A Boolean value; set to __true__ to activate the subscription.
modifyEventSubscription_enabled :: Lens.Lens' ModifyEventSubscription (Core.Maybe Core.Bool)
modifyEventSubscription_enabled = Lens.lens (\ModifyEventSubscription' {enabled} -> enabled) (\s@ModifyEventSubscription' {} a -> s {enabled = a} :: ModifyEventSubscription)

-- | A list of event categories for a source type that you want to subscribe
-- to. Use the @DescribeEventCategories@ action to see a list of event
-- categories.
modifyEventSubscription_eventCategories :: Lens.Lens' ModifyEventSubscription (Core.Maybe [Core.Text])
modifyEventSubscription_eventCategories = Lens.lens (\ModifyEventSubscription' {eventCategories} -> eventCategories) (\s@ModifyEventSubscription' {} a -> s {eventCategories = a} :: ModifyEventSubscription) Core.. Lens.mapping Lens._Coerce

-- | The type of AWS DMS resource that generates the events you want to
-- subscribe to.
--
-- Valid values: replication-instance | replication-task
modifyEventSubscription_sourceType :: Lens.Lens' ModifyEventSubscription (Core.Maybe Core.Text)
modifyEventSubscription_sourceType = Lens.lens (\ModifyEventSubscription' {sourceType} -> sourceType) (\s@ModifyEventSubscription' {} a -> s {sourceType = a} :: ModifyEventSubscription)

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic created for event
-- notification. The ARN is created by Amazon SNS when you create a topic
-- and subscribe to it.
modifyEventSubscription_snsTopicArn :: Lens.Lens' ModifyEventSubscription (Core.Maybe Core.Text)
modifyEventSubscription_snsTopicArn = Lens.lens (\ModifyEventSubscription' {snsTopicArn} -> snsTopicArn) (\s@ModifyEventSubscription' {} a -> s {snsTopicArn = a} :: ModifyEventSubscription)

-- | The name of the AWS DMS event notification subscription to be modified.
modifyEventSubscription_subscriptionName :: Lens.Lens' ModifyEventSubscription Core.Text
modifyEventSubscription_subscriptionName = Lens.lens (\ModifyEventSubscription' {subscriptionName} -> subscriptionName) (\s@ModifyEventSubscription' {} a -> s {subscriptionName = a} :: ModifyEventSubscription)

instance Core.AWSRequest ModifyEventSubscription where
  type
    AWSResponse ModifyEventSubscription =
      ModifyEventSubscriptionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ModifyEventSubscriptionResponse'
            Core.<$> (x Core..?> "EventSubscription")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ModifyEventSubscription

instance Core.NFData ModifyEventSubscription

instance Core.ToHeaders ModifyEventSubscription where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDMSv20160101.ModifyEventSubscription" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ModifyEventSubscription where
  toJSON ModifyEventSubscription' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Enabled" Core..=) Core.<$> enabled,
            ("EventCategories" Core..=) Core.<$> eventCategories,
            ("SourceType" Core..=) Core.<$> sourceType,
            ("SnsTopicArn" Core..=) Core.<$> snsTopicArn,
            Core.Just
              ("SubscriptionName" Core..= subscriptionName)
          ]
      )

instance Core.ToPath ModifyEventSubscription where
  toPath = Core.const "/"

instance Core.ToQuery ModifyEventSubscription where
  toQuery = Core.const Core.mempty

-- |
--
-- /See:/ 'newModifyEventSubscriptionResponse' smart constructor.
data ModifyEventSubscriptionResponse = ModifyEventSubscriptionResponse'
  { -- | The modified event subscription.
    eventSubscription :: Core.Maybe EventSubscription,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyEventSubscriptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventSubscription', 'modifyEventSubscriptionResponse_eventSubscription' - The modified event subscription.
--
-- 'httpStatus', 'modifyEventSubscriptionResponse_httpStatus' - The response's http status code.
newModifyEventSubscriptionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ModifyEventSubscriptionResponse
newModifyEventSubscriptionResponse pHttpStatus_ =
  ModifyEventSubscriptionResponse'
    { eventSubscription =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The modified event subscription.
modifyEventSubscriptionResponse_eventSubscription :: Lens.Lens' ModifyEventSubscriptionResponse (Core.Maybe EventSubscription)
modifyEventSubscriptionResponse_eventSubscription = Lens.lens (\ModifyEventSubscriptionResponse' {eventSubscription} -> eventSubscription) (\s@ModifyEventSubscriptionResponse' {} a -> s {eventSubscription = a} :: ModifyEventSubscriptionResponse)

-- | The response's http status code.
modifyEventSubscriptionResponse_httpStatus :: Lens.Lens' ModifyEventSubscriptionResponse Core.Int
modifyEventSubscriptionResponse_httpStatus = Lens.lens (\ModifyEventSubscriptionResponse' {httpStatus} -> httpStatus) (\s@ModifyEventSubscriptionResponse' {} a -> s {httpStatus = a} :: ModifyEventSubscriptionResponse)

instance Core.NFData ModifyEventSubscriptionResponse
