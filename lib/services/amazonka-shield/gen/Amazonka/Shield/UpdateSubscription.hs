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
-- Module      : Amazonka.Shield.UpdateSubscription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the details of an existing subscription. Only enter values for
-- parameters you want to change. Empty parameters are not updated.
--
-- For accounts that are members of an Organizations organization, Shield
-- Advanced subscriptions are billed against the organization\'s payer
-- account, regardless of whether the payer account itself is subscribed.
module Amazonka.Shield.UpdateSubscription
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Shield.Types

-- | /See:/ 'newUpdateSubscription' smart constructor.
data UpdateSubscription = UpdateSubscription'
  { -- | When you initally create a subscription, @AutoRenew@ is set to
    -- @ENABLED@. If @ENABLED@, the subscription will be automatically renewed
    -- at the end of the existing subscription period. You can change this by
    -- submitting an @UpdateSubscription@ request. If the @UpdateSubscription@
    -- request does not included a value for @AutoRenew@, the existing value
    -- for @AutoRenew@ remains unchanged.
    autoRenew :: Prelude.Maybe AutoRenew
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  UpdateSubscription' {autoRenew = Prelude.Nothing}

-- | When you initally create a subscription, @AutoRenew@ is set to
-- @ENABLED@. If @ENABLED@, the subscription will be automatically renewed
-- at the end of the existing subscription period. You can change this by
-- submitting an @UpdateSubscription@ request. If the @UpdateSubscription@
-- request does not included a value for @AutoRenew@, the existing value
-- for @AutoRenew@ remains unchanged.
updateSubscription_autoRenew :: Lens.Lens' UpdateSubscription (Prelude.Maybe AutoRenew)
updateSubscription_autoRenew = Lens.lens (\UpdateSubscription' {autoRenew} -> autoRenew) (\s@UpdateSubscription' {} a -> s {autoRenew = a} :: UpdateSubscription)

instance Core.AWSRequest UpdateSubscription where
  type
    AWSResponse UpdateSubscription =
      UpdateSubscriptionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateSubscriptionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateSubscription where
  hashWithSalt _salt UpdateSubscription' {..} =
    _salt `Prelude.hashWithSalt` autoRenew

instance Prelude.NFData UpdateSubscription where
  rnf UpdateSubscription' {..} = Prelude.rnf autoRenew

instance Data.ToHeaders UpdateSubscription where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSShield_20160616.UpdateSubscription" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateSubscription where
  toJSON UpdateSubscription' {..} =
    Data.object
      ( Prelude.catMaybes
          [("AutoRenew" Data..=) Prelude.<$> autoRenew]
      )

instance Data.ToPath UpdateSubscription where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateSubscription where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSubscriptionResponse' smart constructor.
data UpdateSubscriptionResponse = UpdateSubscriptionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  UpdateSubscriptionResponse
newUpdateSubscriptionResponse pHttpStatus_ =
  UpdateSubscriptionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateSubscriptionResponse_httpStatus :: Lens.Lens' UpdateSubscriptionResponse Prelude.Int
updateSubscriptionResponse_httpStatus = Lens.lens (\UpdateSubscriptionResponse' {httpStatus} -> httpStatus) (\s@UpdateSubscriptionResponse' {} a -> s {httpStatus = a} :: UpdateSubscriptionResponse)

instance Prelude.NFData UpdateSubscriptionResponse where
  rnf UpdateSubscriptionResponse' {..} =
    Prelude.rnf httpStatus
