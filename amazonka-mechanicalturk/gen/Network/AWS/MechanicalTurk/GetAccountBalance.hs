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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetAccountBalance' smart constructor.
data GetAccountBalance = GetAccountBalance'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetAccountBalance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetAccountBalance ::
  GetAccountBalance
newGetAccountBalance = GetAccountBalance'

instance Core.AWSRequest GetAccountBalance where
  type
    AWSResponse GetAccountBalance =
      GetAccountBalanceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAccountBalanceResponse'
            Core.<$> (x Core..?> "OnHoldBalance")
            Core.<*> (x Core..?> "AvailableBalance")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetAccountBalance

instance Core.NFData GetAccountBalance

instance Core.ToHeaders GetAccountBalance where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "MTurkRequesterServiceV20170117.GetAccountBalance" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetAccountBalance where
  toJSON = Core.const (Core.Object Core.mempty)

instance Core.ToPath GetAccountBalance where
  toPath = Core.const "/"

instance Core.ToQuery GetAccountBalance where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetAccountBalanceResponse' smart constructor.
data GetAccountBalanceResponse = GetAccountBalanceResponse'
  { onHoldBalance :: Core.Maybe Core.Text,
    availableBalance :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetAccountBalanceResponse
newGetAccountBalanceResponse pHttpStatus_ =
  GetAccountBalanceResponse'
    { onHoldBalance =
        Core.Nothing,
      availableBalance = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
getAccountBalanceResponse_onHoldBalance :: Lens.Lens' GetAccountBalanceResponse (Core.Maybe Core.Text)
getAccountBalanceResponse_onHoldBalance = Lens.lens (\GetAccountBalanceResponse' {onHoldBalance} -> onHoldBalance) (\s@GetAccountBalanceResponse' {} a -> s {onHoldBalance = a} :: GetAccountBalanceResponse)

-- | Undocumented member.
getAccountBalanceResponse_availableBalance :: Lens.Lens' GetAccountBalanceResponse (Core.Maybe Core.Text)
getAccountBalanceResponse_availableBalance = Lens.lens (\GetAccountBalanceResponse' {availableBalance} -> availableBalance) (\s@GetAccountBalanceResponse' {} a -> s {availableBalance = a} :: GetAccountBalanceResponse)

-- | The response's http status code.
getAccountBalanceResponse_httpStatus :: Lens.Lens' GetAccountBalanceResponse Core.Int
getAccountBalanceResponse_httpStatus = Lens.lens (\GetAccountBalanceResponse' {httpStatus} -> httpStatus) (\s@GetAccountBalanceResponse' {} a -> s {httpStatus = a} :: GetAccountBalanceResponse)

instance Core.NFData GetAccountBalanceResponse
