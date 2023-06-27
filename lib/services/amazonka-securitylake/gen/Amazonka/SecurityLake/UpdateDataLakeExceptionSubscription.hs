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
-- Module      : Amazonka.SecurityLake.UpdateDataLakeExceptionSubscription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified notification subscription in Amazon Security Lake
-- for the organization you specify.
module Amazonka.SecurityLake.UpdateDataLakeExceptionSubscription
  ( -- * Creating a Request
    UpdateDataLakeExceptionSubscription (..),
    newUpdateDataLakeExceptionSubscription,

    -- * Request Lenses
    updateDataLakeExceptionSubscription_exceptionTimeToLive,
    updateDataLakeExceptionSubscription_notificationEndpoint,
    updateDataLakeExceptionSubscription_subscriptionProtocol,

    -- * Destructuring the Response
    UpdateDataLakeExceptionSubscriptionResponse (..),
    newUpdateDataLakeExceptionSubscriptionResponse,

    -- * Response Lenses
    updateDataLakeExceptionSubscriptionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityLake.Types

-- | /See:/ 'newUpdateDataLakeExceptionSubscription' smart constructor.
data UpdateDataLakeExceptionSubscription = UpdateDataLakeExceptionSubscription'
  { -- | The time-to-live (TTL) for the exception message to remain.
    exceptionTimeToLive :: Prelude.Maybe Prelude.Natural,
    -- | The account that is subscribed to receive exception notifications.
    notificationEndpoint :: Prelude.Text,
    -- | The subscription protocol to which exception messages are posted.
    subscriptionProtocol :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDataLakeExceptionSubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exceptionTimeToLive', 'updateDataLakeExceptionSubscription_exceptionTimeToLive' - The time-to-live (TTL) for the exception message to remain.
--
-- 'notificationEndpoint', 'updateDataLakeExceptionSubscription_notificationEndpoint' - The account that is subscribed to receive exception notifications.
--
-- 'subscriptionProtocol', 'updateDataLakeExceptionSubscription_subscriptionProtocol' - The subscription protocol to which exception messages are posted.
newUpdateDataLakeExceptionSubscription ::
  -- | 'notificationEndpoint'
  Prelude.Text ->
  -- | 'subscriptionProtocol'
  Prelude.Text ->
  UpdateDataLakeExceptionSubscription
newUpdateDataLakeExceptionSubscription
  pNotificationEndpoint_
  pSubscriptionProtocol_ =
    UpdateDataLakeExceptionSubscription'
      { exceptionTimeToLive =
          Prelude.Nothing,
        notificationEndpoint =
          pNotificationEndpoint_,
        subscriptionProtocol =
          pSubscriptionProtocol_
      }

-- | The time-to-live (TTL) for the exception message to remain.
updateDataLakeExceptionSubscription_exceptionTimeToLive :: Lens.Lens' UpdateDataLakeExceptionSubscription (Prelude.Maybe Prelude.Natural)
updateDataLakeExceptionSubscription_exceptionTimeToLive = Lens.lens (\UpdateDataLakeExceptionSubscription' {exceptionTimeToLive} -> exceptionTimeToLive) (\s@UpdateDataLakeExceptionSubscription' {} a -> s {exceptionTimeToLive = a} :: UpdateDataLakeExceptionSubscription)

-- | The account that is subscribed to receive exception notifications.
updateDataLakeExceptionSubscription_notificationEndpoint :: Lens.Lens' UpdateDataLakeExceptionSubscription Prelude.Text
updateDataLakeExceptionSubscription_notificationEndpoint = Lens.lens (\UpdateDataLakeExceptionSubscription' {notificationEndpoint} -> notificationEndpoint) (\s@UpdateDataLakeExceptionSubscription' {} a -> s {notificationEndpoint = a} :: UpdateDataLakeExceptionSubscription)

-- | The subscription protocol to which exception messages are posted.
updateDataLakeExceptionSubscription_subscriptionProtocol :: Lens.Lens' UpdateDataLakeExceptionSubscription Prelude.Text
updateDataLakeExceptionSubscription_subscriptionProtocol = Lens.lens (\UpdateDataLakeExceptionSubscription' {subscriptionProtocol} -> subscriptionProtocol) (\s@UpdateDataLakeExceptionSubscription' {} a -> s {subscriptionProtocol = a} :: UpdateDataLakeExceptionSubscription)

instance
  Core.AWSRequest
    UpdateDataLakeExceptionSubscription
  where
  type
    AWSResponse UpdateDataLakeExceptionSubscription =
      UpdateDataLakeExceptionSubscriptionResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateDataLakeExceptionSubscriptionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateDataLakeExceptionSubscription
  where
  hashWithSalt
    _salt
    UpdateDataLakeExceptionSubscription' {..} =
      _salt
        `Prelude.hashWithSalt` exceptionTimeToLive
        `Prelude.hashWithSalt` notificationEndpoint
        `Prelude.hashWithSalt` subscriptionProtocol

instance
  Prelude.NFData
    UpdateDataLakeExceptionSubscription
  where
  rnf UpdateDataLakeExceptionSubscription' {..} =
    Prelude.rnf exceptionTimeToLive
      `Prelude.seq` Prelude.rnf notificationEndpoint
      `Prelude.seq` Prelude.rnf subscriptionProtocol

instance
  Data.ToHeaders
    UpdateDataLakeExceptionSubscription
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
    UpdateDataLakeExceptionSubscription
  where
  toJSON UpdateDataLakeExceptionSubscription' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("exceptionTimeToLive" Data..=)
              Prelude.<$> exceptionTimeToLive,
            Prelude.Just
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
    UpdateDataLakeExceptionSubscription
  where
  toPath =
    Prelude.const
      "/v1/datalake/exceptions/subscription"

instance
  Data.ToQuery
    UpdateDataLakeExceptionSubscription
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDataLakeExceptionSubscriptionResponse' smart constructor.
data UpdateDataLakeExceptionSubscriptionResponse = UpdateDataLakeExceptionSubscriptionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDataLakeExceptionSubscriptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateDataLakeExceptionSubscriptionResponse_httpStatus' - The response's http status code.
newUpdateDataLakeExceptionSubscriptionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateDataLakeExceptionSubscriptionResponse
newUpdateDataLakeExceptionSubscriptionResponse
  pHttpStatus_ =
    UpdateDataLakeExceptionSubscriptionResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
updateDataLakeExceptionSubscriptionResponse_httpStatus :: Lens.Lens' UpdateDataLakeExceptionSubscriptionResponse Prelude.Int
updateDataLakeExceptionSubscriptionResponse_httpStatus = Lens.lens (\UpdateDataLakeExceptionSubscriptionResponse' {httpStatus} -> httpStatus) (\s@UpdateDataLakeExceptionSubscriptionResponse' {} a -> s {httpStatus = a} :: UpdateDataLakeExceptionSubscriptionResponse)

instance
  Prelude.NFData
    UpdateDataLakeExceptionSubscriptionResponse
  where
  rnf UpdateDataLakeExceptionSubscriptionResponse' {..} =
    Prelude.rnf httpStatus
