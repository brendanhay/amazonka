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
-- Module      : Network.AWS.MechanicalTurk.GetAccountBalance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @GetAccountBalance@ operation retrieves the amount of money in your
-- Amazon Mechanical Turk account.
module Network.AWS.MechanicalTurk.GetAccountBalance
  ( -- * Creating a Request
    GetAccountBalance (..),
    newGetAccountBalance,

    -- * Destructuring the Response
    GetAccountBalanceResponse (..),
    newGetAccountBalanceResponse,

    -- * Response Lenses
    getAccountBalanceResponse_onHoldBalance,
    getAccountBalanceResponse_availableBalance,
    getAccountBalanceResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetAccountBalance' smart constructor.
data GetAccountBalance = GetAccountBalance'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetAccountBalance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetAccountBalance ::
  GetAccountBalance
newGetAccountBalance = GetAccountBalance'

instance Prelude.AWSRequest GetAccountBalance where
  type Rs GetAccountBalance = GetAccountBalanceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAccountBalanceResponse'
            Prelude.<$> (x Prelude..?> "OnHoldBalance")
            Prelude.<*> (x Prelude..?> "AvailableBalance")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAccountBalance

instance Prelude.NFData GetAccountBalance

instance Prelude.ToHeaders GetAccountBalance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "MTurkRequesterServiceV20170117.GetAccountBalance" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetAccountBalance where
  toJSON =
    Prelude.const (Prelude.Object Prelude.mempty)

instance Prelude.ToPath GetAccountBalance where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetAccountBalance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAccountBalanceResponse' smart constructor.
data GetAccountBalanceResponse = GetAccountBalanceResponse'
  { onHoldBalance :: Prelude.Maybe Prelude.Text,
    availableBalance :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetAccountBalanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'onHoldBalance', 'getAccountBalanceResponse_onHoldBalance' - Undocumented member.
--
-- 'availableBalance', 'getAccountBalanceResponse_availableBalance' - Undocumented member.
--
-- 'httpStatus', 'getAccountBalanceResponse_httpStatus' - The response's http status code.
newGetAccountBalanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAccountBalanceResponse
newGetAccountBalanceResponse pHttpStatus_ =
  GetAccountBalanceResponse'
    { onHoldBalance =
        Prelude.Nothing,
      availableBalance = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
getAccountBalanceResponse_onHoldBalance :: Lens.Lens' GetAccountBalanceResponse (Prelude.Maybe Prelude.Text)
getAccountBalanceResponse_onHoldBalance = Lens.lens (\GetAccountBalanceResponse' {onHoldBalance} -> onHoldBalance) (\s@GetAccountBalanceResponse' {} a -> s {onHoldBalance = a} :: GetAccountBalanceResponse)

-- | Undocumented member.
getAccountBalanceResponse_availableBalance :: Lens.Lens' GetAccountBalanceResponse (Prelude.Maybe Prelude.Text)
getAccountBalanceResponse_availableBalance = Lens.lens (\GetAccountBalanceResponse' {availableBalance} -> availableBalance) (\s@GetAccountBalanceResponse' {} a -> s {availableBalance = a} :: GetAccountBalanceResponse)

-- | The response's http status code.
getAccountBalanceResponse_httpStatus :: Lens.Lens' GetAccountBalanceResponse Prelude.Int
getAccountBalanceResponse_httpStatus = Lens.lens (\GetAccountBalanceResponse' {httpStatus} -> httpStatus) (\s@GetAccountBalanceResponse' {} a -> s {httpStatus = a} :: GetAccountBalanceResponse)

instance Prelude.NFData GetAccountBalanceResponse
