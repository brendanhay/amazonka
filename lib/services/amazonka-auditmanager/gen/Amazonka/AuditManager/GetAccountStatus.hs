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
-- Module      : Amazonka.AuditManager.GetAccountStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the registration status of an account in Audit Manager.
module Amazonka.AuditManager.GetAccountStatus
  ( -- * Creating a Request
    GetAccountStatus (..),
    newGetAccountStatus,

    -- * Destructuring the Response
    GetAccountStatusResponse (..),
    newGetAccountStatusResponse,

    -- * Response Lenses
    getAccountStatusResponse_status,
    getAccountStatusResponse_httpStatus,
  )
where

import Amazonka.AuditManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetAccountStatus' smart constructor.
data GetAccountStatus = GetAccountStatus'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAccountStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetAccountStatus ::
  GetAccountStatus
newGetAccountStatus = GetAccountStatus'

instance Core.AWSRequest GetAccountStatus where
  type
    AWSResponse GetAccountStatus =
      GetAccountStatusResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAccountStatusResponse'
            Prelude.<$> (x Data..?> "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAccountStatus where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetAccountStatus where
  rnf _ = ()

instance Data.ToHeaders GetAccountStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetAccountStatus where
  toPath = Prelude.const "/account/status"

instance Data.ToQuery GetAccountStatus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAccountStatusResponse' smart constructor.
data GetAccountStatusResponse = GetAccountStatusResponse'
  { -- | The status of the Amazon Web Services account.
    status :: Prelude.Maybe AccountStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAccountStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'getAccountStatusResponse_status' - The status of the Amazon Web Services account.
--
-- 'httpStatus', 'getAccountStatusResponse_httpStatus' - The response's http status code.
newGetAccountStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAccountStatusResponse
newGetAccountStatusResponse pHttpStatus_ =
  GetAccountStatusResponse'
    { status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the Amazon Web Services account.
getAccountStatusResponse_status :: Lens.Lens' GetAccountStatusResponse (Prelude.Maybe AccountStatus)
getAccountStatusResponse_status = Lens.lens (\GetAccountStatusResponse' {status} -> status) (\s@GetAccountStatusResponse' {} a -> s {status = a} :: GetAccountStatusResponse)

-- | The response's http status code.
getAccountStatusResponse_httpStatus :: Lens.Lens' GetAccountStatusResponse Prelude.Int
getAccountStatusResponse_httpStatus = Lens.lens (\GetAccountStatusResponse' {httpStatus} -> httpStatus) (\s@GetAccountStatusResponse' {} a -> s {httpStatus = a} :: GetAccountStatusResponse)

instance Prelude.NFData GetAccountStatusResponse where
  rnf GetAccountStatusResponse' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
