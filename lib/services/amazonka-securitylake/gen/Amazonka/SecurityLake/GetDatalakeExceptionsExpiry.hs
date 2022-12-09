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
-- Module      : Amazonka.SecurityLake.GetDatalakeExceptionsExpiry
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the expiration period and time-to-live (TTL) for which the
-- exception message will remain. Exceptions are stored by default, for a 2
-- week period of time from when a record was created in Security Lake.
-- This API does not take input parameters. This API does not take input
-- parameters.
module Amazonka.SecurityLake.GetDatalakeExceptionsExpiry
  ( -- * Creating a Request
    GetDatalakeExceptionsExpiry (..),
    newGetDatalakeExceptionsExpiry,

    -- * Destructuring the Response
    GetDatalakeExceptionsExpiryResponse (..),
    newGetDatalakeExceptionsExpiryResponse,

    -- * Response Lenses
    getDatalakeExceptionsExpiryResponse_httpStatus,
    getDatalakeExceptionsExpiryResponse_exceptionMessageExpiry,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityLake.Types

-- | /See:/ 'newGetDatalakeExceptionsExpiry' smart constructor.
data GetDatalakeExceptionsExpiry = GetDatalakeExceptionsExpiry'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDatalakeExceptionsExpiry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetDatalakeExceptionsExpiry ::
  GetDatalakeExceptionsExpiry
newGetDatalakeExceptionsExpiry =
  GetDatalakeExceptionsExpiry'

instance Core.AWSRequest GetDatalakeExceptionsExpiry where
  type
    AWSResponse GetDatalakeExceptionsExpiry =
      GetDatalakeExceptionsExpiryResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDatalakeExceptionsExpiryResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "exceptionMessageExpiry")
      )

instance Prelude.Hashable GetDatalakeExceptionsExpiry where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetDatalakeExceptionsExpiry where
  rnf _ = ()

instance Data.ToHeaders GetDatalakeExceptionsExpiry where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetDatalakeExceptionsExpiry where
  toPath =
    Prelude.const "/v1/datalake/exceptions/expiry"

instance Data.ToQuery GetDatalakeExceptionsExpiry where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDatalakeExceptionsExpiryResponse' smart constructor.
data GetDatalakeExceptionsExpiryResponse = GetDatalakeExceptionsExpiryResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The expiration period and time-to-live (TTL).
    exceptionMessageExpiry :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDatalakeExceptionsExpiryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getDatalakeExceptionsExpiryResponse_httpStatus' - The response's http status code.
--
-- 'exceptionMessageExpiry', 'getDatalakeExceptionsExpiryResponse_exceptionMessageExpiry' - The expiration period and time-to-live (TTL).
newGetDatalakeExceptionsExpiryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'exceptionMessageExpiry'
  Prelude.Integer ->
  GetDatalakeExceptionsExpiryResponse
newGetDatalakeExceptionsExpiryResponse
  pHttpStatus_
  pExceptionMessageExpiry_ =
    GetDatalakeExceptionsExpiryResponse'
      { httpStatus =
          pHttpStatus_,
        exceptionMessageExpiry =
          pExceptionMessageExpiry_
      }

-- | The response's http status code.
getDatalakeExceptionsExpiryResponse_httpStatus :: Lens.Lens' GetDatalakeExceptionsExpiryResponse Prelude.Int
getDatalakeExceptionsExpiryResponse_httpStatus = Lens.lens (\GetDatalakeExceptionsExpiryResponse' {httpStatus} -> httpStatus) (\s@GetDatalakeExceptionsExpiryResponse' {} a -> s {httpStatus = a} :: GetDatalakeExceptionsExpiryResponse)

-- | The expiration period and time-to-live (TTL).
getDatalakeExceptionsExpiryResponse_exceptionMessageExpiry :: Lens.Lens' GetDatalakeExceptionsExpiryResponse Prelude.Integer
getDatalakeExceptionsExpiryResponse_exceptionMessageExpiry = Lens.lens (\GetDatalakeExceptionsExpiryResponse' {exceptionMessageExpiry} -> exceptionMessageExpiry) (\s@GetDatalakeExceptionsExpiryResponse' {} a -> s {exceptionMessageExpiry = a} :: GetDatalakeExceptionsExpiryResponse)

instance
  Prelude.NFData
    GetDatalakeExceptionsExpiryResponse
  where
  rnf GetDatalakeExceptionsExpiryResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf exceptionMessageExpiry
