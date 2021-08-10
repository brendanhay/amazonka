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
-- Module      : Network.AWS.Shield.GetSubscriptionState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the @SubscriptionState@, either @Active@ or @Inactive@.
module Network.AWS.Shield.GetSubscriptionState
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Shield.Types

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSubscriptionStateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "SubscriptionState")
      )

instance Prelude.Hashable GetSubscriptionState

instance Prelude.NFData GetSubscriptionState

instance Core.ToHeaders GetSubscriptionState where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSShield_20160616.GetSubscriptionState" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetSubscriptionState where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath GetSubscriptionState where
  toPath = Prelude.const "/"

instance Core.ToQuery GetSubscriptionState where
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

instance Prelude.NFData GetSubscriptionStateResponse
