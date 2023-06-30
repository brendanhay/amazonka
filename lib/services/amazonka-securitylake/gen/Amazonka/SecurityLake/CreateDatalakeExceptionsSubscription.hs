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
-- Module      : Amazonka.SecurityLake.CreateDatalakeExceptionsSubscription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates the specified notification subscription in Amazon Security Lake
-- for the organization you specify.
module Amazonka.SecurityLake.CreateDatalakeExceptionsSubscription
  ( -- * Creating a Request
    CreateDatalakeExceptionsSubscription (..),
    newCreateDatalakeExceptionsSubscription,

    -- * Request Lenses
    createDatalakeExceptionsSubscription_notificationEndpoint,
    createDatalakeExceptionsSubscription_subscriptionProtocol,

    -- * Destructuring the Response
    CreateDatalakeExceptionsSubscriptionResponse (..),
    newCreateDatalakeExceptionsSubscriptionResponse,

    -- * Response Lenses
    createDatalakeExceptionsSubscriptionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityLake.Types

-- | /See:/ 'newCreateDatalakeExceptionsSubscription' smart constructor.
data CreateDatalakeExceptionsSubscription = CreateDatalakeExceptionsSubscription'
  { -- | The Amazon Web Services account where you want to receive exception
    -- notifications.
    notificationEndpoint :: Prelude.Text,
    -- | The subscription protocol to which exception notifications are posted.
    subscriptionProtocol :: SubscriptionProtocolType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDatalakeExceptionsSubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notificationEndpoint', 'createDatalakeExceptionsSubscription_notificationEndpoint' - The Amazon Web Services account where you want to receive exception
-- notifications.
--
-- 'subscriptionProtocol', 'createDatalakeExceptionsSubscription_subscriptionProtocol' - The subscription protocol to which exception notifications are posted.
newCreateDatalakeExceptionsSubscription ::
  -- | 'notificationEndpoint'
  Prelude.Text ->
  -- | 'subscriptionProtocol'
  SubscriptionProtocolType ->
  CreateDatalakeExceptionsSubscription
newCreateDatalakeExceptionsSubscription
  pNotificationEndpoint_
  pSubscriptionProtocol_ =
    CreateDatalakeExceptionsSubscription'
      { notificationEndpoint =
          pNotificationEndpoint_,
        subscriptionProtocol =
          pSubscriptionProtocol_
      }

-- | The Amazon Web Services account where you want to receive exception
-- notifications.
createDatalakeExceptionsSubscription_notificationEndpoint :: Lens.Lens' CreateDatalakeExceptionsSubscription Prelude.Text
createDatalakeExceptionsSubscription_notificationEndpoint = Lens.lens (\CreateDatalakeExceptionsSubscription' {notificationEndpoint} -> notificationEndpoint) (\s@CreateDatalakeExceptionsSubscription' {} a -> s {notificationEndpoint = a} :: CreateDatalakeExceptionsSubscription)

-- | The subscription protocol to which exception notifications are posted.
createDatalakeExceptionsSubscription_subscriptionProtocol :: Lens.Lens' CreateDatalakeExceptionsSubscription SubscriptionProtocolType
createDatalakeExceptionsSubscription_subscriptionProtocol = Lens.lens (\CreateDatalakeExceptionsSubscription' {subscriptionProtocol} -> subscriptionProtocol) (\s@CreateDatalakeExceptionsSubscription' {} a -> s {subscriptionProtocol = a} :: CreateDatalakeExceptionsSubscription)

instance
  Core.AWSRequest
    CreateDatalakeExceptionsSubscription
  where
  type
    AWSResponse CreateDatalakeExceptionsSubscription =
      CreateDatalakeExceptionsSubscriptionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateDatalakeExceptionsSubscriptionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateDatalakeExceptionsSubscription
  where
  hashWithSalt
    _salt
    CreateDatalakeExceptionsSubscription' {..} =
      _salt
        `Prelude.hashWithSalt` notificationEndpoint
        `Prelude.hashWithSalt` subscriptionProtocol

instance
  Prelude.NFData
    CreateDatalakeExceptionsSubscription
  where
  rnf CreateDatalakeExceptionsSubscription' {..} =
    Prelude.rnf notificationEndpoint
      `Prelude.seq` Prelude.rnf subscriptionProtocol

instance
  Data.ToHeaders
    CreateDatalakeExceptionsSubscription
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
    CreateDatalakeExceptionsSubscription
  where
  toJSON CreateDatalakeExceptionsSubscription' {..} =
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
    CreateDatalakeExceptionsSubscription
  where
  toPath =
    Prelude.const
      "/v1/datalake/exceptions/subscription"

instance
  Data.ToQuery
    CreateDatalakeExceptionsSubscription
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDatalakeExceptionsSubscriptionResponse' smart constructor.
data CreateDatalakeExceptionsSubscriptionResponse = CreateDatalakeExceptionsSubscriptionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDatalakeExceptionsSubscriptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createDatalakeExceptionsSubscriptionResponse_httpStatus' - The response's http status code.
newCreateDatalakeExceptionsSubscriptionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDatalakeExceptionsSubscriptionResponse
newCreateDatalakeExceptionsSubscriptionResponse
  pHttpStatus_ =
    CreateDatalakeExceptionsSubscriptionResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
createDatalakeExceptionsSubscriptionResponse_httpStatus :: Lens.Lens' CreateDatalakeExceptionsSubscriptionResponse Prelude.Int
createDatalakeExceptionsSubscriptionResponse_httpStatus = Lens.lens (\CreateDatalakeExceptionsSubscriptionResponse' {httpStatus} -> httpStatus) (\s@CreateDatalakeExceptionsSubscriptionResponse' {} a -> s {httpStatus = a} :: CreateDatalakeExceptionsSubscriptionResponse)

instance
  Prelude.NFData
    CreateDatalakeExceptionsSubscriptionResponse
  where
  rnf CreateDatalakeExceptionsSubscriptionResponse' {..} =
    Prelude.rnf httpStatus
