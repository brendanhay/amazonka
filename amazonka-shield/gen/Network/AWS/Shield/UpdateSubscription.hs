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
-- Module      : Network.AWS.Shield.UpdateSubscription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the details of an existing subscription. Only enter values for
-- parameters you want to change. Empty parameters are not updated.
module Network.AWS.Shield.UpdateSubscription
  ( -- * Creating a Request
    UpdateSubscription (..),
    newUpdateSubscription,

    -- * Request Lenses
    updateSubscription_autoRenew,

    -- * Destructuring the Response
    UpdateSubscriptionResponse (..),
    newUpdateSubscriptionResponse,

    -- * Response Lenses
    updateSubscriptionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Shield.Types

-- | /See:/ 'newUpdateSubscription' smart constructor.
data UpdateSubscription = UpdateSubscription'
  { -- | When you initally create a subscription, @AutoRenew@ is set to
    -- @ENABLED@. If @ENABLED@, the subscription will be automatically renewed
    -- at the end of the existing subscription period. You can change this by
    -- submitting an @UpdateSubscription@ request. If the @UpdateSubscription@
    -- request does not included a value for @AutoRenew@, the existing value
    -- for @AutoRenew@ remains unchanged.
    autoRenew :: Core.Maybe AutoRenew
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateSubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoRenew', 'updateSubscription_autoRenew' - When you initally create a subscription, @AutoRenew@ is set to
-- @ENABLED@. If @ENABLED@, the subscription will be automatically renewed
-- at the end of the existing subscription period. You can change this by
-- submitting an @UpdateSubscription@ request. If the @UpdateSubscription@
-- request does not included a value for @AutoRenew@, the existing value
-- for @AutoRenew@ remains unchanged.
newUpdateSubscription ::
  UpdateSubscription
newUpdateSubscription =
  UpdateSubscription' {autoRenew = Core.Nothing}

-- | When you initally create a subscription, @AutoRenew@ is set to
-- @ENABLED@. If @ENABLED@, the subscription will be automatically renewed
-- at the end of the existing subscription period. You can change this by
-- submitting an @UpdateSubscription@ request. If the @UpdateSubscription@
-- request does not included a value for @AutoRenew@, the existing value
-- for @AutoRenew@ remains unchanged.
updateSubscription_autoRenew :: Lens.Lens' UpdateSubscription (Core.Maybe AutoRenew)
updateSubscription_autoRenew = Lens.lens (\UpdateSubscription' {autoRenew} -> autoRenew) (\s@UpdateSubscription' {} a -> s {autoRenew = a} :: UpdateSubscription)

instance Core.AWSRequest UpdateSubscription where
  type
    AWSResponse UpdateSubscription =
      UpdateSubscriptionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateSubscriptionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateSubscription

instance Core.NFData UpdateSubscription

instance Core.ToHeaders UpdateSubscription where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSShield_20160616.UpdateSubscription" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateSubscription where
  toJSON UpdateSubscription' {..} =
    Core.object
      ( Core.catMaybes
          [("AutoRenew" Core..=) Core.<$> autoRenew]
      )

instance Core.ToPath UpdateSubscription where
  toPath = Core.const "/"

instance Core.ToQuery UpdateSubscription where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateSubscriptionResponse' smart constructor.
data UpdateSubscriptionResponse = UpdateSubscriptionResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateSubscriptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateSubscriptionResponse_httpStatus' - The response's http status code.
newUpdateSubscriptionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateSubscriptionResponse
newUpdateSubscriptionResponse pHttpStatus_ =
  UpdateSubscriptionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateSubscriptionResponse_httpStatus :: Lens.Lens' UpdateSubscriptionResponse Core.Int
updateSubscriptionResponse_httpStatus = Lens.lens (\UpdateSubscriptionResponse' {httpStatus} -> httpStatus) (\s@UpdateSubscriptionResponse' {} a -> s {httpStatus = a} :: UpdateSubscriptionResponse)

instance Core.NFData UpdateSubscriptionResponse
