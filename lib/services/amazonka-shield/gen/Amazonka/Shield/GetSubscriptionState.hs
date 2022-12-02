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
-- Module      : Amazonka.Shield.GetSubscriptionState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the @SubscriptionState@, either @Active@ or @Inactive@.
module Amazonka.Shield.GetSubscriptionState
  ( -- * Creating a Request
    GetSubscriptionState (..),
    newGetSubscriptionState,

    -- * Destructuring the Response
    GetSubscriptionStateResponse (..),
    newGetSubscriptionStateResponse,

    -- * Response Lenses
    getSubscriptionStateResponse_httpStatus,
    getSubscriptionStateResponse_subscriptionState,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Shield.Types

-- | /See:/ 'newGetSubscriptionState' smart constructor.
data GetSubscriptionState = GetSubscriptionState'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSubscriptionState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetSubscriptionState ::
  GetSubscriptionState
newGetSubscriptionState = GetSubscriptionState'

instance Core.AWSRequest GetSubscriptionState where
  type
    AWSResponse GetSubscriptionState =
      GetSubscriptionStateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSubscriptionStateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "SubscriptionState")
      )

instance Prelude.Hashable GetSubscriptionState where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetSubscriptionState where
  rnf _ = ()

instance Data.ToHeaders GetSubscriptionState where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSShield_20160616.GetSubscriptionState" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetSubscriptionState where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath GetSubscriptionState where
  toPath = Prelude.const "/"

instance Data.ToQuery GetSubscriptionState where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSubscriptionStateResponse' smart constructor.
data GetSubscriptionStateResponse = GetSubscriptionStateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The status of the subscription.
    subscriptionState :: SubscriptionState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSubscriptionStateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getSubscriptionStateResponse_httpStatus' - The response's http status code.
--
-- 'subscriptionState', 'getSubscriptionStateResponse_subscriptionState' - The status of the subscription.
newGetSubscriptionStateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'subscriptionState'
  SubscriptionState ->
  GetSubscriptionStateResponse
newGetSubscriptionStateResponse
  pHttpStatus_
  pSubscriptionState_ =
    GetSubscriptionStateResponse'
      { httpStatus =
          pHttpStatus_,
        subscriptionState = pSubscriptionState_
      }

-- | The response's http status code.
getSubscriptionStateResponse_httpStatus :: Lens.Lens' GetSubscriptionStateResponse Prelude.Int
getSubscriptionStateResponse_httpStatus = Lens.lens (\GetSubscriptionStateResponse' {httpStatus} -> httpStatus) (\s@GetSubscriptionStateResponse' {} a -> s {httpStatus = a} :: GetSubscriptionStateResponse)

-- | The status of the subscription.
getSubscriptionStateResponse_subscriptionState :: Lens.Lens' GetSubscriptionStateResponse SubscriptionState
getSubscriptionStateResponse_subscriptionState = Lens.lens (\GetSubscriptionStateResponse' {subscriptionState} -> subscriptionState) (\s@GetSubscriptionStateResponse' {} a -> s {subscriptionState = a} :: GetSubscriptionStateResponse)

instance Prelude.NFData GetSubscriptionStateResponse where
  rnf GetSubscriptionStateResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf subscriptionState
