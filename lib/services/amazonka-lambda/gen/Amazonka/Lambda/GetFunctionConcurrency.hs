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
-- Module      : Amazonka.Lambda.GetFunctionConcurrency
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns details about the reserved concurrency configuration for a
-- function. To set a concurrency limit for a function, use
-- PutFunctionConcurrency.
module Amazonka.Lambda.GetFunctionConcurrency
  ( -- * Creating a Request
    GetFunctionConcurrency (..),
    newGetFunctionConcurrency,

    -- * Request Lenses
    getFunctionConcurrency_functionName,

    -- * Destructuring the Response
    GetFunctionConcurrencyResponse (..),
    newGetFunctionConcurrencyResponse,

    -- * Response Lenses
    getFunctionConcurrencyResponse_reservedConcurrentExecutions,
    getFunctionConcurrencyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lambda.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetFunctionConcurrency' smart constructor.
data GetFunctionConcurrency = GetFunctionConcurrency'
  { -- | The name of the Lambda function.
    --
    -- __Name formats__
    --
    -- -   __Function name__ – @my-function@.
    --
    -- -   __Function ARN__ –
    --     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
    --
    -- -   __Partial ARN__ – @123456789012:function:my-function@.
    --
    -- The length constraint applies only to the full ARN. If you specify only
    -- the function name, it is limited to 64 characters in length.
    functionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFunctionConcurrency' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'functionName', 'getFunctionConcurrency_functionName' - The name of the Lambda function.
--
-- __Name formats__
--
-- -   __Function name__ – @my-function@.
--
-- -   __Function ARN__ –
--     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
--
-- -   __Partial ARN__ – @123456789012:function:my-function@.
--
-- The length constraint applies only to the full ARN. If you specify only
-- the function name, it is limited to 64 characters in length.
newGetFunctionConcurrency ::
  -- | 'functionName'
  Prelude.Text ->
  GetFunctionConcurrency
newGetFunctionConcurrency pFunctionName_ =
  GetFunctionConcurrency'
    { functionName =
        pFunctionName_
    }

-- | The name of the Lambda function.
--
-- __Name formats__
--
-- -   __Function name__ – @my-function@.
--
-- -   __Function ARN__ –
--     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
--
-- -   __Partial ARN__ – @123456789012:function:my-function@.
--
-- The length constraint applies only to the full ARN. If you specify only
-- the function name, it is limited to 64 characters in length.
getFunctionConcurrency_functionName :: Lens.Lens' GetFunctionConcurrency Prelude.Text
getFunctionConcurrency_functionName = Lens.lens (\GetFunctionConcurrency' {functionName} -> functionName) (\s@GetFunctionConcurrency' {} a -> s {functionName = a} :: GetFunctionConcurrency)

instance Core.AWSRequest GetFunctionConcurrency where
  type
    AWSResponse GetFunctionConcurrency =
      GetFunctionConcurrencyResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetFunctionConcurrencyResponse'
            Prelude.<$> (x Data..?> "ReservedConcurrentExecutions")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetFunctionConcurrency where
  hashWithSalt _salt GetFunctionConcurrency' {..} =
    _salt `Prelude.hashWithSalt` functionName

instance Prelude.NFData GetFunctionConcurrency where
  rnf GetFunctionConcurrency' {..} =
    Prelude.rnf functionName

instance Data.ToHeaders GetFunctionConcurrency where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetFunctionConcurrency where
  toPath GetFunctionConcurrency' {..} =
    Prelude.mconcat
      [ "/2019-09-30/functions/",
        Data.toBS functionName,
        "/concurrency"
      ]

instance Data.ToQuery GetFunctionConcurrency where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetFunctionConcurrencyResponse' smart constructor.
data GetFunctionConcurrencyResponse = GetFunctionConcurrencyResponse'
  { -- | The number of simultaneous executions that are reserved for the
    -- function.
    reservedConcurrentExecutions :: Prelude.Maybe Prelude.Natural,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFunctionConcurrencyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reservedConcurrentExecutions', 'getFunctionConcurrencyResponse_reservedConcurrentExecutions' - The number of simultaneous executions that are reserved for the
-- function.
--
-- 'httpStatus', 'getFunctionConcurrencyResponse_httpStatus' - The response's http status code.
newGetFunctionConcurrencyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetFunctionConcurrencyResponse
newGetFunctionConcurrencyResponse pHttpStatus_ =
  GetFunctionConcurrencyResponse'
    { reservedConcurrentExecutions =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The number of simultaneous executions that are reserved for the
-- function.
getFunctionConcurrencyResponse_reservedConcurrentExecutions :: Lens.Lens' GetFunctionConcurrencyResponse (Prelude.Maybe Prelude.Natural)
getFunctionConcurrencyResponse_reservedConcurrentExecutions = Lens.lens (\GetFunctionConcurrencyResponse' {reservedConcurrentExecutions} -> reservedConcurrentExecutions) (\s@GetFunctionConcurrencyResponse' {} a -> s {reservedConcurrentExecutions = a} :: GetFunctionConcurrencyResponse)

-- | The response's http status code.
getFunctionConcurrencyResponse_httpStatus :: Lens.Lens' GetFunctionConcurrencyResponse Prelude.Int
getFunctionConcurrencyResponse_httpStatus = Lens.lens (\GetFunctionConcurrencyResponse' {httpStatus} -> httpStatus) (\s@GetFunctionConcurrencyResponse' {} a -> s {httpStatus = a} :: GetFunctionConcurrencyResponse)

instance
  Prelude.NFData
    GetFunctionConcurrencyResponse
  where
  rnf GetFunctionConcurrencyResponse' {..} =
    Prelude.rnf reservedConcurrentExecutions `Prelude.seq`
      Prelude.rnf httpStatus
