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
-- Module      : Amazonka.SecurityLake.GetDataLakeExceptionSubscription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the details of exception notifications for the account in
-- Amazon Security Lake.
module Amazonka.SecurityLake.GetDataLakeExceptionSubscription
  ( -- * Creating a Request
    GetDataLakeExceptionSubscription (..),
    newGetDataLakeExceptionSubscription,

    -- * Destructuring the Response
    GetDataLakeExceptionSubscriptionResponse (..),
    newGetDataLakeExceptionSubscriptionResponse,

    -- * Response Lenses
    getDataLakeExceptionSubscriptionResponse_exceptionTimeToLive,
    getDataLakeExceptionSubscriptionResponse_notificationEndpoint,
    getDataLakeExceptionSubscriptionResponse_subscriptionProtocol,
    getDataLakeExceptionSubscriptionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityLake.Types

-- | /See:/ 'newGetDataLakeExceptionSubscription' smart constructor.
data GetDataLakeExceptionSubscription = GetDataLakeExceptionSubscription'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDataLakeExceptionSubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetDataLakeExceptionSubscription ::
  GetDataLakeExceptionSubscription
newGetDataLakeExceptionSubscription =
  GetDataLakeExceptionSubscription'

instance
  Core.AWSRequest
    GetDataLakeExceptionSubscription
  where
  type
    AWSResponse GetDataLakeExceptionSubscription =
      GetDataLakeExceptionSubscriptionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDataLakeExceptionSubscriptionResponse'
            Prelude.<$> (x Data..?> "exceptionTimeToLive")
            Prelude.<*> (x Data..?> "notificationEndpoint")
            Prelude.<*> (x Data..?> "subscriptionProtocol")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetDataLakeExceptionSubscription
  where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance
  Prelude.NFData
    GetDataLakeExceptionSubscription
  where
  rnf _ = ()

instance
  Data.ToHeaders
    GetDataLakeExceptionSubscription
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

instance Data.ToPath GetDataLakeExceptionSubscription where
  toPath =
    Prelude.const
      "/v1/datalake/exceptions/subscription"

instance
  Data.ToQuery
    GetDataLakeExceptionSubscription
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDataLakeExceptionSubscriptionResponse' smart constructor.
data GetDataLakeExceptionSubscriptionResponse = GetDataLakeExceptionSubscriptionResponse'
  { -- | The expiration period and time-to-live (TTL).
    exceptionTimeToLive :: Prelude.Maybe Prelude.Integer,
    -- | The Amazon Web Services account where you receive exception
    -- notifications.
    notificationEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The subscription protocol to which exception notifications are posted.
    subscriptionProtocol :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDataLakeExceptionSubscriptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exceptionTimeToLive', 'getDataLakeExceptionSubscriptionResponse_exceptionTimeToLive' - The expiration period and time-to-live (TTL).
--
-- 'notificationEndpoint', 'getDataLakeExceptionSubscriptionResponse_notificationEndpoint' - The Amazon Web Services account where you receive exception
-- notifications.
--
-- 'subscriptionProtocol', 'getDataLakeExceptionSubscriptionResponse_subscriptionProtocol' - The subscription protocol to which exception notifications are posted.
--
-- 'httpStatus', 'getDataLakeExceptionSubscriptionResponse_httpStatus' - The response's http status code.
newGetDataLakeExceptionSubscriptionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDataLakeExceptionSubscriptionResponse
newGetDataLakeExceptionSubscriptionResponse
  pHttpStatus_ =
    GetDataLakeExceptionSubscriptionResponse'
      { exceptionTimeToLive =
          Prelude.Nothing,
        notificationEndpoint =
          Prelude.Nothing,
        subscriptionProtocol =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The expiration period and time-to-live (TTL).
getDataLakeExceptionSubscriptionResponse_exceptionTimeToLive :: Lens.Lens' GetDataLakeExceptionSubscriptionResponse (Prelude.Maybe Prelude.Integer)
getDataLakeExceptionSubscriptionResponse_exceptionTimeToLive = Lens.lens (\GetDataLakeExceptionSubscriptionResponse' {exceptionTimeToLive} -> exceptionTimeToLive) (\s@GetDataLakeExceptionSubscriptionResponse' {} a -> s {exceptionTimeToLive = a} :: GetDataLakeExceptionSubscriptionResponse)

-- | The Amazon Web Services account where you receive exception
-- notifications.
getDataLakeExceptionSubscriptionResponse_notificationEndpoint :: Lens.Lens' GetDataLakeExceptionSubscriptionResponse (Prelude.Maybe Prelude.Text)
getDataLakeExceptionSubscriptionResponse_notificationEndpoint = Lens.lens (\GetDataLakeExceptionSubscriptionResponse' {notificationEndpoint} -> notificationEndpoint) (\s@GetDataLakeExceptionSubscriptionResponse' {} a -> s {notificationEndpoint = a} :: GetDataLakeExceptionSubscriptionResponse)

-- | The subscription protocol to which exception notifications are posted.
getDataLakeExceptionSubscriptionResponse_subscriptionProtocol :: Lens.Lens' GetDataLakeExceptionSubscriptionResponse (Prelude.Maybe Prelude.Text)
getDataLakeExceptionSubscriptionResponse_subscriptionProtocol = Lens.lens (\GetDataLakeExceptionSubscriptionResponse' {subscriptionProtocol} -> subscriptionProtocol) (\s@GetDataLakeExceptionSubscriptionResponse' {} a -> s {subscriptionProtocol = a} :: GetDataLakeExceptionSubscriptionResponse)

-- | The response's http status code.
getDataLakeExceptionSubscriptionResponse_httpStatus :: Lens.Lens' GetDataLakeExceptionSubscriptionResponse Prelude.Int
getDataLakeExceptionSubscriptionResponse_httpStatus = Lens.lens (\GetDataLakeExceptionSubscriptionResponse' {httpStatus} -> httpStatus) (\s@GetDataLakeExceptionSubscriptionResponse' {} a -> s {httpStatus = a} :: GetDataLakeExceptionSubscriptionResponse)

instance
  Prelude.NFData
    GetDataLakeExceptionSubscriptionResponse
  where
  rnf GetDataLakeExceptionSubscriptionResponse' {..} =
    Prelude.rnf exceptionTimeToLive
      `Prelude.seq` Prelude.rnf notificationEndpoint
      `Prelude.seq` Prelude.rnf subscriptionProtocol
      `Prelude.seq` Prelude.rnf httpStatus
