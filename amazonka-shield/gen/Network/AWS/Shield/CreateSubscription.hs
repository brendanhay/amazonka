{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Shield.Types

-- | /See:/ 'newCreateSubscription' smart constructor.
data CreateSubscription = CreateSubscription'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateSubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newCreateSubscription ::
  CreateSubscription
newCreateSubscription = CreateSubscription'

instance Prelude.AWSRequest CreateSubscription where
  type
    Rs CreateSubscription =
      CreateSubscriptionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateSubscriptionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSubscription

instance Prelude.NFData CreateSubscription

instance Prelude.ToHeaders CreateSubscription where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSShield_20160616.CreateSubscription" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateSubscription where
  toJSON =
    Prelude.const (Prelude.Object Prelude.mempty)

instance Prelude.ToPath CreateSubscription where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateSubscription where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSubscriptionResponse' smart constructor.
data CreateSubscriptionResponse = CreateSubscriptionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData CreateSubscriptionResponse
