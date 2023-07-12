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
-- Module      : Amazonka.SecurityLake.GetDatalakeExceptionsSubscription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the details of exception notifications for the account in
-- Amazon Security Lake.
module Amazonka.SecurityLake.GetDatalakeExceptionsSubscription
  ( -- * Creating a Request
    GetDatalakeExceptionsSubscription (..),
    newGetDatalakeExceptionsSubscription,

    -- * Destructuring the Response
    GetDatalakeExceptionsSubscriptionResponse (..),
    newGetDatalakeExceptionsSubscriptionResponse,

    -- * Response Lenses
    getDatalakeExceptionsSubscriptionResponse_httpStatus,
    getDatalakeExceptionsSubscriptionResponse_protocolAndNotificationEndpoint,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityLake.Types

-- | /See:/ 'newGetDatalakeExceptionsSubscription' smart constructor.
data GetDatalakeExceptionsSubscription = GetDatalakeExceptionsSubscription'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDatalakeExceptionsSubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetDatalakeExceptionsSubscription ::
  GetDatalakeExceptionsSubscription
newGetDatalakeExceptionsSubscription =
  GetDatalakeExceptionsSubscription'

instance
  Core.AWSRequest
    GetDatalakeExceptionsSubscription
  where
  type
    AWSResponse GetDatalakeExceptionsSubscription =
      GetDatalakeExceptionsSubscriptionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDatalakeExceptionsSubscriptionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "protocolAndNotificationEndpoint")
      )

instance
  Prelude.Hashable
    GetDatalakeExceptionsSubscription
  where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance
  Prelude.NFData
    GetDatalakeExceptionsSubscription
  where
  rnf _ = ()

instance
  Data.ToHeaders
    GetDatalakeExceptionsSubscription
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
  Data.ToPath
    GetDatalakeExceptionsSubscription
  where
  toPath =
    Prelude.const
      "/v1/datalake/exceptions/subscription"

instance
  Data.ToQuery
    GetDatalakeExceptionsSubscription
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDatalakeExceptionsSubscriptionResponse' smart constructor.
data GetDatalakeExceptionsSubscriptionResponse = GetDatalakeExceptionsSubscriptionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Retrieves the exception notification subscription information.
    protocolAndNotificationEndpoint :: ProtocolAndNotificationEndpoint
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDatalakeExceptionsSubscriptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getDatalakeExceptionsSubscriptionResponse_httpStatus' - The response's http status code.
--
-- 'protocolAndNotificationEndpoint', 'getDatalakeExceptionsSubscriptionResponse_protocolAndNotificationEndpoint' - Retrieves the exception notification subscription information.
newGetDatalakeExceptionsSubscriptionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'protocolAndNotificationEndpoint'
  ProtocolAndNotificationEndpoint ->
  GetDatalakeExceptionsSubscriptionResponse
newGetDatalakeExceptionsSubscriptionResponse
  pHttpStatus_
  pProtocolAndNotificationEndpoint_ =
    GetDatalakeExceptionsSubscriptionResponse'
      { httpStatus =
          pHttpStatus_,
        protocolAndNotificationEndpoint =
          pProtocolAndNotificationEndpoint_
      }

-- | The response's http status code.
getDatalakeExceptionsSubscriptionResponse_httpStatus :: Lens.Lens' GetDatalakeExceptionsSubscriptionResponse Prelude.Int
getDatalakeExceptionsSubscriptionResponse_httpStatus = Lens.lens (\GetDatalakeExceptionsSubscriptionResponse' {httpStatus} -> httpStatus) (\s@GetDatalakeExceptionsSubscriptionResponse' {} a -> s {httpStatus = a} :: GetDatalakeExceptionsSubscriptionResponse)

-- | Retrieves the exception notification subscription information.
getDatalakeExceptionsSubscriptionResponse_protocolAndNotificationEndpoint :: Lens.Lens' GetDatalakeExceptionsSubscriptionResponse ProtocolAndNotificationEndpoint
getDatalakeExceptionsSubscriptionResponse_protocolAndNotificationEndpoint = Lens.lens (\GetDatalakeExceptionsSubscriptionResponse' {protocolAndNotificationEndpoint} -> protocolAndNotificationEndpoint) (\s@GetDatalakeExceptionsSubscriptionResponse' {} a -> s {protocolAndNotificationEndpoint = a} :: GetDatalakeExceptionsSubscriptionResponse)

instance
  Prelude.NFData
    GetDatalakeExceptionsSubscriptionResponse
  where
  rnf GetDatalakeExceptionsSubscriptionResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf protocolAndNotificationEndpoint
