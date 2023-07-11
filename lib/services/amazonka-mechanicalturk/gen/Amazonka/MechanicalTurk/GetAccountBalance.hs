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
-- Module      : Amazonka.MechanicalTurk.GetAccountBalance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @GetAccountBalance@ operation retrieves the Prepaid HITs balance in
-- your Amazon Mechanical Turk account if you are a Prepaid Requester.
-- Alternatively, this operation will retrieve the remaining available AWS
-- Billing usage if you have enabled AWS Billing. Note: If you have enabled
-- AWS Billing and still have a remaining Prepaid HITs balance, this
-- balance can be viewed on the My Account page in the Requester console.
module Amazonka.MechanicalTurk.GetAccountBalance
  ( -- * Creating a Request
    GetAccountBalance (..),
    newGetAccountBalance,

    -- * Destructuring the Response
    GetAccountBalanceResponse (..),
    newGetAccountBalanceResponse,

    -- * Response Lenses
    getAccountBalanceResponse_availableBalance,
    getAccountBalanceResponse_onHoldBalance,
    getAccountBalanceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MechanicalTurk.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetAccountBalance' smart constructor.
data GetAccountBalance = GetAccountBalance'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAccountBalanceResponse'
            Prelude.<$> (x Data..?> "AvailableBalance")
            Prelude.<*> (x Data..?> "OnHoldBalance")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAccountBalance where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetAccountBalance where
  rnf _ = ()

instance Data.ToHeaders GetAccountBalance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "MTurkRequesterServiceV20170117.GetAccountBalance" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetAccountBalance where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath GetAccountBalance where
  toPath = Prelude.const "/"

instance Data.ToQuery GetAccountBalance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAccountBalanceResponse' smart constructor.
data GetAccountBalanceResponse = GetAccountBalanceResponse'
  { availableBalance :: Prelude.Maybe Prelude.Text,
    onHoldBalance :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAccountBalanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availableBalance', 'getAccountBalanceResponse_availableBalance' - Undocumented member.
--
-- 'onHoldBalance', 'getAccountBalanceResponse_onHoldBalance' - Undocumented member.
--
-- 'httpStatus', 'getAccountBalanceResponse_httpStatus' - The response's http status code.
newGetAccountBalanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAccountBalanceResponse
newGetAccountBalanceResponse pHttpStatus_ =
  GetAccountBalanceResponse'
    { availableBalance =
        Prelude.Nothing,
      onHoldBalance = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
getAccountBalanceResponse_availableBalance :: Lens.Lens' GetAccountBalanceResponse (Prelude.Maybe Prelude.Text)
getAccountBalanceResponse_availableBalance = Lens.lens (\GetAccountBalanceResponse' {availableBalance} -> availableBalance) (\s@GetAccountBalanceResponse' {} a -> s {availableBalance = a} :: GetAccountBalanceResponse)

-- | Undocumented member.
getAccountBalanceResponse_onHoldBalance :: Lens.Lens' GetAccountBalanceResponse (Prelude.Maybe Prelude.Text)
getAccountBalanceResponse_onHoldBalance = Lens.lens (\GetAccountBalanceResponse' {onHoldBalance} -> onHoldBalance) (\s@GetAccountBalanceResponse' {} a -> s {onHoldBalance = a} :: GetAccountBalanceResponse)

-- | The response's http status code.
getAccountBalanceResponse_httpStatus :: Lens.Lens' GetAccountBalanceResponse Prelude.Int
getAccountBalanceResponse_httpStatus = Lens.lens (\GetAccountBalanceResponse' {httpStatus} -> httpStatus) (\s@GetAccountBalanceResponse' {} a -> s {httpStatus = a} :: GetAccountBalanceResponse)

instance Prelude.NFData GetAccountBalanceResponse where
  rnf GetAccountBalanceResponse' {..} =
    Prelude.rnf availableBalance
      `Prelude.seq` Prelude.rnf onHoldBalance
      `Prelude.seq` Prelude.rnf httpStatus
