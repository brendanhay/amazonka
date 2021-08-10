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
-- Module      : Network.AWS.WorkDocs.CreateNotificationSubscription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configure Amazon WorkDocs to use Amazon SNS notifications. The endpoint
-- receives a confirmation message, and must confirm the subscription.
--
-- For more information, see
-- <https://docs.aws.amazon.com/workdocs/latest/developerguide/subscribe-notifications.html Subscribe to Notifications>
-- in the /Amazon WorkDocs Developer Guide/.
module Network.AWS.WorkDocs.CreateNotificationSubscription
  ( -- * Creating a Request
    CreateNotificationSubscription (..),
    newCreateNotificationSubscription,

    -- * Request Lenses
    createNotificationSubscription_organizationId,
    createNotificationSubscription_endpoint,
    createNotificationSubscription_protocol,
    createNotificationSubscription_subscriptionType,

    -- * Destructuring the Response
    CreateNotificationSubscriptionResponse (..),
    newCreateNotificationSubscriptionResponse,

    -- * Response Lenses
    createNotificationSubscriptionResponse_subscription,
    createNotificationSubscriptionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkDocs.Types

-- | /See:/ 'newCreateNotificationSubscription' smart constructor.
data CreateNotificationSubscription = CreateNotificationSubscription'
  { -- | The ID of the organization.
    organizationId :: Prelude.Text,
    -- | The endpoint to receive the notifications. If the protocol is HTTPS, the
    -- endpoint is a URL that begins with @https@.
    endpoint :: Prelude.Text,
    -- | The protocol to use. The supported value is https, which delivers
    -- JSON-encoded messages using HTTPS POST.
    protocol :: SubscriptionProtocolType,
    -- | The notification type.
    subscriptionType :: SubscriptionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateNotificationSubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationId', 'createNotificationSubscription_organizationId' - The ID of the organization.
--
-- 'endpoint', 'createNotificationSubscription_endpoint' - The endpoint to receive the notifications. If the protocol is HTTPS, the
-- endpoint is a URL that begins with @https@.
--
-- 'protocol', 'createNotificationSubscription_protocol' - The protocol to use. The supported value is https, which delivers
-- JSON-encoded messages using HTTPS POST.
--
-- 'subscriptionType', 'createNotificationSubscription_subscriptionType' - The notification type.
newCreateNotificationSubscription ::
  -- | 'organizationId'
  Prelude.Text ->
  -- | 'endpoint'
  Prelude.Text ->
  -- | 'protocol'
  SubscriptionProtocolType ->
  -- | 'subscriptionType'
  SubscriptionType ->
  CreateNotificationSubscription
newCreateNotificationSubscription
  pOrganizationId_
  pEndpoint_
  pProtocol_
  pSubscriptionType_ =
    CreateNotificationSubscription'
      { organizationId =
          pOrganizationId_,
        endpoint = pEndpoint_,
        protocol = pProtocol_,
        subscriptionType = pSubscriptionType_
      }

-- | The ID of the organization.
createNotificationSubscription_organizationId :: Lens.Lens' CreateNotificationSubscription Prelude.Text
createNotificationSubscription_organizationId = Lens.lens (\CreateNotificationSubscription' {organizationId} -> organizationId) (\s@CreateNotificationSubscription' {} a -> s {organizationId = a} :: CreateNotificationSubscription)

-- | The endpoint to receive the notifications. If the protocol is HTTPS, the
-- endpoint is a URL that begins with @https@.
createNotificationSubscription_endpoint :: Lens.Lens' CreateNotificationSubscription Prelude.Text
createNotificationSubscription_endpoint = Lens.lens (\CreateNotificationSubscription' {endpoint} -> endpoint) (\s@CreateNotificationSubscription' {} a -> s {endpoint = a} :: CreateNotificationSubscription)

-- | The protocol to use. The supported value is https, which delivers
-- JSON-encoded messages using HTTPS POST.
createNotificationSubscription_protocol :: Lens.Lens' CreateNotificationSubscription SubscriptionProtocolType
createNotificationSubscription_protocol = Lens.lens (\CreateNotificationSubscription' {protocol} -> protocol) (\s@CreateNotificationSubscription' {} a -> s {protocol = a} :: CreateNotificationSubscription)

-- | The notification type.
createNotificationSubscription_subscriptionType :: Lens.Lens' CreateNotificationSubscription SubscriptionType
createNotificationSubscription_subscriptionType = Lens.lens (\CreateNotificationSubscription' {subscriptionType} -> subscriptionType) (\s@CreateNotificationSubscription' {} a -> s {subscriptionType = a} :: CreateNotificationSubscription)

instance
  Core.AWSRequest
    CreateNotificationSubscription
  where
  type
    AWSResponse CreateNotificationSubscription =
      CreateNotificationSubscriptionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateNotificationSubscriptionResponse'
            Prelude.<$> (x Core..?> "Subscription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateNotificationSubscription

instance
  Prelude.NFData
    CreateNotificationSubscription

instance
  Core.ToHeaders
    CreateNotificationSubscription
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateNotificationSubscription where
  toJSON CreateNotificationSubscription' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Endpoint" Core..= endpoint),
            Prelude.Just ("Protocol" Core..= protocol),
            Prelude.Just
              ("SubscriptionType" Core..= subscriptionType)
          ]
      )

instance Core.ToPath CreateNotificationSubscription where
  toPath CreateNotificationSubscription' {..} =
    Prelude.mconcat
      [ "/api/v1/organizations/",
        Core.toBS organizationId,
        "/subscriptions"
      ]

instance Core.ToQuery CreateNotificationSubscription where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateNotificationSubscriptionResponse' smart constructor.
data CreateNotificationSubscriptionResponse = CreateNotificationSubscriptionResponse'
  { -- | The subscription.
    subscription :: Prelude.Maybe Subscription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateNotificationSubscriptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subscription', 'createNotificationSubscriptionResponse_subscription' - The subscription.
--
-- 'httpStatus', 'createNotificationSubscriptionResponse_httpStatus' - The response's http status code.
newCreateNotificationSubscriptionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateNotificationSubscriptionResponse
newCreateNotificationSubscriptionResponse
  pHttpStatus_ =
    CreateNotificationSubscriptionResponse'
      { subscription =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The subscription.
createNotificationSubscriptionResponse_subscription :: Lens.Lens' CreateNotificationSubscriptionResponse (Prelude.Maybe Subscription)
createNotificationSubscriptionResponse_subscription = Lens.lens (\CreateNotificationSubscriptionResponse' {subscription} -> subscription) (\s@CreateNotificationSubscriptionResponse' {} a -> s {subscription = a} :: CreateNotificationSubscriptionResponse)

-- | The response's http status code.
createNotificationSubscriptionResponse_httpStatus :: Lens.Lens' CreateNotificationSubscriptionResponse Prelude.Int
createNotificationSubscriptionResponse_httpStatus = Lens.lens (\CreateNotificationSubscriptionResponse' {httpStatus} -> httpStatus) (\s@CreateNotificationSubscriptionResponse' {} a -> s {httpStatus = a} :: CreateNotificationSubscriptionResponse)

instance
  Prelude.NFData
    CreateNotificationSubscriptionResponse
