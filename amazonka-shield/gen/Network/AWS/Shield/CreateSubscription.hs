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
-- Module      : Network.AWS.Shield.CreateSubscription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Activates AWS Shield Advanced for an account.
--
-- When you initally create a subscription, your subscription is set to be
-- automatically renewed at the end of the existing subscription period.
-- You can change this by submitting an @UpdateSubscription@ request.
module Network.AWS.Shield.CreateSubscription
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Shield.Types

-- | /See:/ 'newCreateSubscription' smart constructor.
data CreateSubscription = CreateSubscription'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateSubscriptionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateSubscription

instance Core.NFData CreateSubscription

instance Core.ToHeaders CreateSubscription where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSShield_20160616.CreateSubscription" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateSubscription where
  toJSON = Core.const (Core.Object Core.mempty)

instance Core.ToPath CreateSubscription where
  toPath = Core.const "/"

instance Core.ToQuery CreateSubscription where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateSubscriptionResponse' smart constructor.
data CreateSubscriptionResponse = CreateSubscriptionResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  CreateSubscriptionResponse
newCreateSubscriptionResponse pHttpStatus_ =
  CreateSubscriptionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
createSubscriptionResponse_httpStatus :: Lens.Lens' CreateSubscriptionResponse Core.Int
createSubscriptionResponse_httpStatus = Lens.lens (\CreateSubscriptionResponse' {httpStatus} -> httpStatus) (\s@CreateSubscriptionResponse' {} a -> s {httpStatus = a} :: CreateSubscriptionResponse)

instance Core.NFData CreateSubscriptionResponse
