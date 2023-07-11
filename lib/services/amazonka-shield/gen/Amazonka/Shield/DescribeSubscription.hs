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
-- Module      : Amazonka.Shield.DescribeSubscription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides details about the Shield Advanced subscription for an account.
module Amazonka.Shield.DescribeSubscription
  ( -- * Creating a Request
    DescribeSubscription (..),
    newDescribeSubscription,

    -- * Destructuring the Response
    DescribeSubscriptionResponse (..),
    newDescribeSubscriptionResponse,

    -- * Response Lenses
    describeSubscriptionResponse_subscription,
    describeSubscriptionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Shield.Types

-- | /See:/ 'newDescribeSubscription' smart constructor.
data DescribeSubscription = DescribeSubscription'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDescribeSubscription ::
  DescribeSubscription
newDescribeSubscription = DescribeSubscription'

instance Core.AWSRequest DescribeSubscription where
  type
    AWSResponse DescribeSubscription =
      DescribeSubscriptionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSubscriptionResponse'
            Prelude.<$> (x Data..?> "Subscription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSubscription where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData DescribeSubscription where
  rnf _ = ()

instance Data.ToHeaders DescribeSubscription where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSShield_20160616.DescribeSubscription" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeSubscription where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath DescribeSubscription where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeSubscription where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeSubscriptionResponse' smart constructor.
data DescribeSubscriptionResponse = DescribeSubscriptionResponse'
  { -- | The Shield Advanced subscription details for an account.
    subscription :: Prelude.Maybe Subscription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSubscriptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subscription', 'describeSubscriptionResponse_subscription' - The Shield Advanced subscription details for an account.
--
-- 'httpStatus', 'describeSubscriptionResponse_httpStatus' - The response's http status code.
newDescribeSubscriptionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeSubscriptionResponse
newDescribeSubscriptionResponse pHttpStatus_ =
  DescribeSubscriptionResponse'
    { subscription =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Shield Advanced subscription details for an account.
describeSubscriptionResponse_subscription :: Lens.Lens' DescribeSubscriptionResponse (Prelude.Maybe Subscription)
describeSubscriptionResponse_subscription = Lens.lens (\DescribeSubscriptionResponse' {subscription} -> subscription) (\s@DescribeSubscriptionResponse' {} a -> s {subscription = a} :: DescribeSubscriptionResponse)

-- | The response's http status code.
describeSubscriptionResponse_httpStatus :: Lens.Lens' DescribeSubscriptionResponse Prelude.Int
describeSubscriptionResponse_httpStatus = Lens.lens (\DescribeSubscriptionResponse' {httpStatus} -> httpStatus) (\s@DescribeSubscriptionResponse' {} a -> s {httpStatus = a} :: DescribeSubscriptionResponse)

instance Prelude.NFData DescribeSubscriptionResponse where
  rnf DescribeSubscriptionResponse' {..} =
    Prelude.rnf subscription
      `Prelude.seq` Prelude.rnf httpStatus
