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
-- Module      : Amazonka.SecurityLake.UpdateDatalakeExceptionsSubscription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified notification subscription in Amazon Security Lake
-- for the organization you specify.
module Amazonka.SecurityLake.UpdateDatalakeExceptionsSubscription
  ( -- * Creating a Request
    UpdateDatalakeExceptionsSubscription (..),
    newUpdateDatalakeExceptionsSubscription,

    -- * Request Lenses
    updateDatalakeExceptionsSubscription_notificationEndpoint,
    updateDatalakeExceptionsSubscription_subscriptionProtocol,

    -- * Destructuring the Response
    UpdateDatalakeExceptionsSubscriptionResponse (..),
    newUpdateDatalakeExceptionsSubscriptionResponse,

    -- * Response Lenses
    updateDatalakeExceptionsSubscriptionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityLake.Types

-- | /See:/ 'newUpdateDatalakeExceptionsSubscription' smart constructor.
data UpdateDatalakeExceptionsSubscription = UpdateDatalakeExceptionsSubscription'
  { -- | The account that is subscribed to receive exception notifications.
    notificationEndpoint :: Prelude.Text,
    -- | The subscription protocol to which exception messages are posted.
    subscriptionProtocol :: SubscriptionProtocolType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDatalakeExceptionsSubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notificationEndpoint', 'updateDatalakeExceptionsSubscription_notificationEndpoint' - The account that is subscribed to receive exception notifications.
--
-- 'subscriptionProtocol', 'updateDatalakeExceptionsSubscription_subscriptionProtocol' - The subscription protocol to which exception messages are posted.
newUpdateDatalakeExceptionsSubscription ::
  -- | 'notificationEndpoint'
  Prelude.Text ->
  -- | 'subscriptionProtocol'
  SubscriptionProtocolType ->
  UpdateDatalakeExceptionsSubscription
newUpdateDatalakeExceptionsSubscription
  pNotificationEndpoint_
  pSubscriptionProtocol_ =
    UpdateDatalakeExceptionsSubscription'
      { notificationEndpoint =
          pNotificationEndpoint_,
        subscriptionProtocol =
          pSubscriptionProtocol_
      }

-- | The account that is subscribed to receive exception notifications.
updateDatalakeExceptionsSubscription_notificationEndpoint :: Lens.Lens' UpdateDatalakeExceptionsSubscription Prelude.Text
updateDatalakeExceptionsSubscription_notificationEndpoint = Lens.lens (\UpdateDatalakeExceptionsSubscription' {notificationEndpoint} -> notificationEndpoint) (\s@UpdateDatalakeExceptionsSubscription' {} a -> s {notificationEndpoint = a} :: UpdateDatalakeExceptionsSubscription)

-- | The subscription protocol to which exception messages are posted.
updateDatalakeExceptionsSubscription_subscriptionProtocol :: Lens.Lens' UpdateDatalakeExceptionsSubscription SubscriptionProtocolType
updateDatalakeExceptionsSubscription_subscriptionProtocol = Lens.lens (\UpdateDatalakeExceptionsSubscription' {subscriptionProtocol} -> subscriptionProtocol) (\s@UpdateDatalakeExceptionsSubscription' {} a -> s {subscriptionProtocol = a} :: UpdateDatalakeExceptionsSubscription)

instance
  Core.AWSRequest
    UpdateDatalakeExceptionsSubscription
  where
  type
    AWSResponse UpdateDatalakeExceptionsSubscription =
      UpdateDatalakeExceptionsSubscriptionResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateDatalakeExceptionsSubscriptionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateDatalakeExceptionsSubscription
  where
  hashWithSalt
    _salt
    UpdateDatalakeExceptionsSubscription' {..} =
      _salt
        `Prelude.hashWithSalt` notificationEndpoint
        `Prelude.hashWithSalt` subscriptionProtocol

instance
  Prelude.NFData
    UpdateDatalakeExceptionsSubscription
  where
  rnf UpdateDatalakeExceptionsSubscription' {..} =
    Prelude.rnf notificationEndpoint `Prelude.seq`
      Prelude.rnf subscriptionProtocol

instance
  Data.ToHeaders
    UpdateDatalakeExceptionsSubscription
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    UpdateDatalakeExceptionsSubscription
  where
  toJSON UpdateDatalakeExceptionsSubscription' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "notificationEndpoint"
                  Data..= notificationEndpoint
              ),
            Prelude.Just
              ( "subscriptionProtocol"
                  Data..= subscriptionProtocol
              )
          ]
      )

instance
  Data.ToPath
    UpdateDatalakeExceptionsSubscription
  where
  toPath =
    Prelude.const
      "/v1/datalake/exceptions/subscription"

instance
  Data.ToQuery
    UpdateDatalakeExceptionsSubscription
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDatalakeExceptionsSubscriptionResponse' smart constructor.
data UpdateDatalakeExceptionsSubscriptionResponse = UpdateDatalakeExceptionsSubscriptionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDatalakeExceptionsSubscriptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateDatalakeExceptionsSubscriptionResponse_httpStatus' - The response's http status code.
newUpdateDatalakeExceptionsSubscriptionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateDatalakeExceptionsSubscriptionResponse
newUpdateDatalakeExceptionsSubscriptionResponse
  pHttpStatus_ =
    UpdateDatalakeExceptionsSubscriptionResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
updateDatalakeExceptionsSubscriptionResponse_httpStatus :: Lens.Lens' UpdateDatalakeExceptionsSubscriptionResponse Prelude.Int
updateDatalakeExceptionsSubscriptionResponse_httpStatus = Lens.lens (\UpdateDatalakeExceptionsSubscriptionResponse' {httpStatus} -> httpStatus) (\s@UpdateDatalakeExceptionsSubscriptionResponse' {} a -> s {httpStatus = a} :: UpdateDatalakeExceptionsSubscriptionResponse)

instance
  Prelude.NFData
    UpdateDatalakeExceptionsSubscriptionResponse
  where
  rnf UpdateDatalakeExceptionsSubscriptionResponse' {..} =
    Prelude.rnf httpStatus
