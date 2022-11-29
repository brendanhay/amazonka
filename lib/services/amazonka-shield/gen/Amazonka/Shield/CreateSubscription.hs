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
-- Module      : Amazonka.Shield.CreateSubscription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Activates Shield Advanced for an account.
--
-- For accounts that are members of an Organizations organization, Shield
-- Advanced subscriptions are billed against the organization\'s payer
-- account, regardless of whether the payer account itself is subscribed.
--
-- When you initially create a subscription, your subscription is set to be
-- automatically renewed at the end of the existing subscription period.
-- You can change this by submitting an @UpdateSubscription@ request.
module Amazonka.Shield.CreateSubscription
  ( -- * Creating a Request
    CreateSubscription (..),
    newCreateSubscription,

    -- * Destructuring the Response
    CreateSubscriptionResponse (..),
    newCreateSubscriptionResponse,

    -- * Response Lenses
    createSubscriptionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Shield.Types

-- | /See:/ 'newCreateSubscription' smart constructor.
data CreateSubscription = CreateSubscription'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newCreateSubscription ::
  CreateSubscription
newCreateSubscription = CreateSubscription'

instance Core.AWSRequest CreateSubscription where
  type
    AWSResponse CreateSubscription =
      CreateSubscriptionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateSubscriptionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSubscription where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData CreateSubscription where
  rnf _ = ()

instance Core.ToHeaders CreateSubscription where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSShield_20160616.CreateSubscription" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateSubscription where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath CreateSubscription where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateSubscription where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSubscriptionResponse' smart constructor.
data CreateSubscriptionResponse = CreateSubscriptionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSubscriptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createSubscriptionResponse_httpStatus' - The response's http status code.
newCreateSubscriptionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSubscriptionResponse
newCreateSubscriptionResponse pHttpStatus_ =
  CreateSubscriptionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
createSubscriptionResponse_httpStatus :: Lens.Lens' CreateSubscriptionResponse Prelude.Int
createSubscriptionResponse_httpStatus = Lens.lens (\CreateSubscriptionResponse' {httpStatus} -> httpStatus) (\s@CreateSubscriptionResponse' {} a -> s {httpStatus = a} :: CreateSubscriptionResponse)

instance Prelude.NFData CreateSubscriptionResponse where
  rnf CreateSubscriptionResponse' {..} =
    Prelude.rnf httpStatus
