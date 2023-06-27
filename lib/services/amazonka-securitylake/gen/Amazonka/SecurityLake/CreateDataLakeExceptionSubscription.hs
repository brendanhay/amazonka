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
-- Module      : Amazonka.SecurityLake.CreateDataLakeExceptionSubscription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates the specified notification subscription in Amazon Security Lake
-- for the organization you specify.
module Amazonka.SecurityLake.CreateDataLakeExceptionSubscription
  ( -- * Creating a Request
    CreateDataLakeExceptionSubscription (..),
    newCreateDataLakeExceptionSubscription,

    -- * Request Lenses
    createDataLakeExceptionSubscription_exceptionTimeToLive,
    createDataLakeExceptionSubscription_notificationEndpoint,
    createDataLakeExceptionSubscription_subscriptionProtocol,

    -- * Destructuring the Response
    CreateDataLakeExceptionSubscriptionResponse (..),
    newCreateDataLakeExceptionSubscriptionResponse,

    -- * Response Lenses
    createDataLakeExceptionSubscriptionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityLake.Types

-- | /See:/ 'newCreateDataLakeExceptionSubscription' smart constructor.
data CreateDataLakeExceptionSubscription = CreateDataLakeExceptionSubscription'
  { -- | The expiration period and time-to-live (TTL).
    exceptionTimeToLive :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Web Services account where you want to receive exception
    -- notifications.
    notificationEndpoint :: Prelude.Text,
    -- | The subscription protocol to which exception notifications are posted.
    subscriptionProtocol :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDataLakeExceptionSubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exceptionTimeToLive', 'createDataLakeExceptionSubscription_exceptionTimeToLive' - The expiration period and time-to-live (TTL).
--
-- 'notificationEndpoint', 'createDataLakeExceptionSubscription_notificationEndpoint' - The Amazon Web Services account where you want to receive exception
-- notifications.
--
-- 'subscriptionProtocol', 'createDataLakeExceptionSubscription_subscriptionProtocol' - The subscription protocol to which exception notifications are posted.
newCreateDataLakeExceptionSubscription ::
  -- | 'notificationEndpoint'
  Prelude.Text ->
  -- | 'subscriptionProtocol'
  Prelude.Text ->
  CreateDataLakeExceptionSubscription
newCreateDataLakeExceptionSubscription
  pNotificationEndpoint_
  pSubscriptionProtocol_ =
    CreateDataLakeExceptionSubscription'
      { exceptionTimeToLive =
          Prelude.Nothing,
        notificationEndpoint =
          pNotificationEndpoint_,
        subscriptionProtocol =
          pSubscriptionProtocol_
      }

-- | The expiration period and time-to-live (TTL).
createDataLakeExceptionSubscription_exceptionTimeToLive :: Lens.Lens' CreateDataLakeExceptionSubscription (Prelude.Maybe Prelude.Natural)
createDataLakeExceptionSubscription_exceptionTimeToLive = Lens.lens (\CreateDataLakeExceptionSubscription' {exceptionTimeToLive} -> exceptionTimeToLive) (\s@CreateDataLakeExceptionSubscription' {} a -> s {exceptionTimeToLive = a} :: CreateDataLakeExceptionSubscription)

-- | The Amazon Web Services account where you want to receive exception
-- notifications.
createDataLakeExceptionSubscription_notificationEndpoint :: Lens.Lens' CreateDataLakeExceptionSubscription Prelude.Text
createDataLakeExceptionSubscription_notificationEndpoint = Lens.lens (\CreateDataLakeExceptionSubscription' {notificationEndpoint} -> notificationEndpoint) (\s@CreateDataLakeExceptionSubscription' {} a -> s {notificationEndpoint = a} :: CreateDataLakeExceptionSubscription)

-- | The subscription protocol to which exception notifications are posted.
createDataLakeExceptionSubscription_subscriptionProtocol :: Lens.Lens' CreateDataLakeExceptionSubscription Prelude.Text
createDataLakeExceptionSubscription_subscriptionProtocol = Lens.lens (\CreateDataLakeExceptionSubscription' {subscriptionProtocol} -> subscriptionProtocol) (\s@CreateDataLakeExceptionSubscription' {} a -> s {subscriptionProtocol = a} :: CreateDataLakeExceptionSubscription)

instance
  Core.AWSRequest
    CreateDataLakeExceptionSubscription
  where
  type
    AWSResponse CreateDataLakeExceptionSubscription =
      CreateDataLakeExceptionSubscriptionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateDataLakeExceptionSubscriptionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateDataLakeExceptionSubscription
  where
  hashWithSalt
    _salt
    CreateDataLakeExceptionSubscription' {..} =
      _salt
        `Prelude.hashWithSalt` exceptionTimeToLive
        `Prelude.hashWithSalt` notificationEndpoint
        `Prelude.hashWithSalt` subscriptionProtocol

instance
  Prelude.NFData
    CreateDataLakeExceptionSubscription
  where
  rnf CreateDataLakeExceptionSubscription' {..} =
    Prelude.rnf exceptionTimeToLive
      `Prelude.seq` Prelude.rnf notificationEndpoint
      `Prelude.seq` Prelude.rnf subscriptionProtocol

instance
  Data.ToHeaders
    CreateDataLakeExceptionSubscription
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
    CreateDataLakeExceptionSubscription
  where
  toJSON CreateDataLakeExceptionSubscription' {..} =
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
    CreateDataLakeExceptionSubscription
  where
  toPath =
    Prelude.const
      "/v1/datalake/exceptions/subscription"

instance
  Data.ToQuery
    CreateDataLakeExceptionSubscription
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDataLakeExceptionSubscriptionResponse' smart constructor.
data CreateDataLakeExceptionSubscriptionResponse = CreateDataLakeExceptionSubscriptionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDataLakeExceptionSubscriptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createDataLakeExceptionSubscriptionResponse_httpStatus' - The response's http status code.
newCreateDataLakeExceptionSubscriptionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDataLakeExceptionSubscriptionResponse
newCreateDataLakeExceptionSubscriptionResponse
  pHttpStatus_ =
    CreateDataLakeExceptionSubscriptionResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
createDataLakeExceptionSubscriptionResponse_httpStatus :: Lens.Lens' CreateDataLakeExceptionSubscriptionResponse Prelude.Int
createDataLakeExceptionSubscriptionResponse_httpStatus = Lens.lens (\CreateDataLakeExceptionSubscriptionResponse' {httpStatus} -> httpStatus) (\s@CreateDataLakeExceptionSubscriptionResponse' {} a -> s {httpStatus = a} :: CreateDataLakeExceptionSubscriptionResponse)

instance
  Prelude.NFData
    CreateDataLakeExceptionSubscriptionResponse
  where
  rnf CreateDataLakeExceptionSubscriptionResponse' {..} =
    Prelude.rnf httpStatus
