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
-- Module      : Amazonka.DMS.ModifyEventSubscription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies an existing DMS event notification subscription.
module Amazonka.DMS.ModifyEventSubscription
  ( -- * Creating a Request
    ModifyEventSubscription (..),
    newModifyEventSubscription,

    -- * Request Lenses
    modifyEventSubscription_enabled,
    modifyEventSubscription_eventCategories,
    modifyEventSubscription_snsTopicArn,
    modifyEventSubscription_sourceType,
    modifyEventSubscription_subscriptionName,

    -- * Destructuring the Response
    ModifyEventSubscriptionResponse (..),
    newModifyEventSubscriptionResponse,

    -- * Response Lenses
    modifyEventSubscriptionResponse_eventSubscription,
    modifyEventSubscriptionResponse_httpStatus,
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
-- /See:/ 'newModifyEventSubscription' smart constructor.
data ModifyEventSubscription = ModifyEventSubscription'
  { -- | A Boolean value; set to __true__ to activate the subscription.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | A list of event categories for a source type that you want to subscribe
    -- to. Use the @DescribeEventCategories@ action to see a list of event
    -- categories.
    eventCategories :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Resource Name (ARN) of the Amazon SNS topic created for event
    -- notification. The ARN is created by Amazon SNS when you create a topic
    -- and subscribe to it.
    snsTopicArn :: Prelude.Maybe Prelude.Text,
    -- | The type of DMS resource that generates the events you want to subscribe
    -- to.
    --
    -- Valid values: replication-instance | replication-task
    sourceType :: Prelude.Maybe Prelude.Text,
    -- | The name of the DMS event notification subscription to be modified.
    subscriptionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'snsTopicArn', 'modifyEventSubscription_snsTopicArn' - The Amazon Resource Name (ARN) of the Amazon SNS topic created for event
-- notification. The ARN is created by Amazon SNS when you create a topic
-- and subscribe to it.
--
-- 'sourceType', 'modifyEventSubscription_sourceType' - The type of DMS resource that generates the events you want to subscribe
-- to.
--
-- Valid values: replication-instance | replication-task
--
-- 'subscriptionName', 'modifyEventSubscription_subscriptionName' - The name of the DMS event notification subscription to be modified.
newModifyEventSubscription ::
  -- | 'subscriptionName'
  Prelude.Text ->
  ModifyEventSubscription
newModifyEventSubscription pSubscriptionName_ =
  ModifyEventSubscription'
    { enabled = Prelude.Nothing,
      eventCategories = Prelude.Nothing,
      snsTopicArn = Prelude.Nothing,
      sourceType = Prelude.Nothing,
      subscriptionName = pSubscriptionName_
    }

-- | A Boolean value; set to __true__ to activate the subscription.
modifyEventSubscription_enabled :: Lens.Lens' ModifyEventSubscription (Prelude.Maybe Prelude.Bool)
modifyEventSubscription_enabled = Lens.lens (\ModifyEventSubscription' {enabled} -> enabled) (\s@ModifyEventSubscription' {} a -> s {enabled = a} :: ModifyEventSubscription)

-- | A list of event categories for a source type that you want to subscribe
-- to. Use the @DescribeEventCategories@ action to see a list of event
-- categories.
modifyEventSubscription_eventCategories :: Lens.Lens' ModifyEventSubscription (Prelude.Maybe [Prelude.Text])
modifyEventSubscription_eventCategories = Lens.lens (\ModifyEventSubscription' {eventCategories} -> eventCategories) (\s@ModifyEventSubscription' {} a -> s {eventCategories = a} :: ModifyEventSubscription) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic created for event
-- notification. The ARN is created by Amazon SNS when you create a topic
-- and subscribe to it.
modifyEventSubscription_snsTopicArn :: Lens.Lens' ModifyEventSubscription (Prelude.Maybe Prelude.Text)
modifyEventSubscription_snsTopicArn = Lens.lens (\ModifyEventSubscription' {snsTopicArn} -> snsTopicArn) (\s@ModifyEventSubscription' {} a -> s {snsTopicArn = a} :: ModifyEventSubscription)

-- | The type of DMS resource that generates the events you want to subscribe
-- to.
--
-- Valid values: replication-instance | replication-task
modifyEventSubscription_sourceType :: Lens.Lens' ModifyEventSubscription (Prelude.Maybe Prelude.Text)
modifyEventSubscription_sourceType = Lens.lens (\ModifyEventSubscription' {sourceType} -> sourceType) (\s@ModifyEventSubscription' {} a -> s {sourceType = a} :: ModifyEventSubscription)

-- | The name of the DMS event notification subscription to be modified.
modifyEventSubscription_subscriptionName :: Lens.Lens' ModifyEventSubscription Prelude.Text
modifyEventSubscription_subscriptionName = Lens.lens (\ModifyEventSubscription' {subscriptionName} -> subscriptionName) (\s@ModifyEventSubscription' {} a -> s {subscriptionName = a} :: ModifyEventSubscription)

instance Core.AWSRequest ModifyEventSubscription where
  type
    AWSResponse ModifyEventSubscription =
      ModifyEventSubscriptionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ModifyEventSubscriptionResponse'
            Prelude.<$> (x Data..?> "EventSubscription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyEventSubscription where
  hashWithSalt _salt ModifyEventSubscription' {..} =
    _salt
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` eventCategories
      `Prelude.hashWithSalt` snsTopicArn
      `Prelude.hashWithSalt` sourceType
      `Prelude.hashWithSalt` subscriptionName

instance Prelude.NFData ModifyEventSubscription where
  rnf ModifyEventSubscription' {..} =
    Prelude.rnf enabled `Prelude.seq`
      Prelude.rnf eventCategories `Prelude.seq`
        Prelude.rnf snsTopicArn `Prelude.seq`
          Prelude.rnf sourceType `Prelude.seq`
            Prelude.rnf subscriptionName

instance Data.ToHeaders ModifyEventSubscription where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonDMSv20160101.ModifyEventSubscription" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ModifyEventSubscription where
  toJSON ModifyEventSubscription' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Enabled" Data..=) Prelude.<$> enabled,
            ("EventCategories" Data..=)
              Prelude.<$> eventCategories,
            ("SnsTopicArn" Data..=) Prelude.<$> snsTopicArn,
            ("SourceType" Data..=) Prelude.<$> sourceType,
            Prelude.Just
              ("SubscriptionName" Data..= subscriptionName)
          ]
      )

instance Data.ToPath ModifyEventSubscription where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyEventSubscription where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newModifyEventSubscriptionResponse' smart constructor.
data ModifyEventSubscriptionResponse = ModifyEventSubscriptionResponse'
  { -- | The modified event subscription.
    eventSubscription :: Prelude.Maybe EventSubscription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ModifyEventSubscriptionResponse
newModifyEventSubscriptionResponse pHttpStatus_ =
  ModifyEventSubscriptionResponse'
    { eventSubscription =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The modified event subscription.
modifyEventSubscriptionResponse_eventSubscription :: Lens.Lens' ModifyEventSubscriptionResponse (Prelude.Maybe EventSubscription)
modifyEventSubscriptionResponse_eventSubscription = Lens.lens (\ModifyEventSubscriptionResponse' {eventSubscription} -> eventSubscription) (\s@ModifyEventSubscriptionResponse' {} a -> s {eventSubscription = a} :: ModifyEventSubscriptionResponse)

-- | The response's http status code.
modifyEventSubscriptionResponse_httpStatus :: Lens.Lens' ModifyEventSubscriptionResponse Prelude.Int
modifyEventSubscriptionResponse_httpStatus = Lens.lens (\ModifyEventSubscriptionResponse' {httpStatus} -> httpStatus) (\s@ModifyEventSubscriptionResponse' {} a -> s {httpStatus = a} :: ModifyEventSubscriptionResponse)

instance
  Prelude.NFData
    ModifyEventSubscriptionResponse
  where
  rnf ModifyEventSubscriptionResponse' {..} =
    Prelude.rnf eventSubscription `Prelude.seq`
      Prelude.rnf httpStatus
