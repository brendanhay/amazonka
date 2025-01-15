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
-- Module      : Amazonka.SSMSAP.GetOperation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the details of an operation by specifying the operation ID.
module Amazonka.SSMSAP.GetOperation
  ( -- * Creating a Request
    GetOperation (..),
    newGetOperation,

    -- * Request Lenses
    getOperation_operationId,

    -- * Destructuring the Response
    GetOperationResponse (..),
    newGetOperationResponse,

    -- * Response Lenses
    getOperationResponse_operation,
    getOperationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMSAP.Types

-- | /See:/ 'newGetOperation' smart constructor.
data GetOperation = GetOperation'
  { operationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetOperation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operationId', 'getOperation_operationId' -
newGetOperation ::
  -- | 'operationId'
  Prelude.Text ->
  GetOperation
newGetOperation pOperationId_ =
  GetOperation' {operationId = pOperationId_}

getOperation_operationId :: Lens.Lens' GetOperation Prelude.Text
getOperation_operationId = Lens.lens (\GetOperation' {operationId} -> operationId) (\s@GetOperation' {} a -> s {operationId = a} :: GetOperation)

instance Core.AWSRequest GetOperation where
  type AWSResponse GetOperation = GetOperationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetOperationResponse'
            Prelude.<$> (x Data..?> "Operation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetOperation where
  hashWithSalt _salt GetOperation' {..} =
    _salt `Prelude.hashWithSalt` operationId

instance Prelude.NFData GetOperation where
  rnf GetOperation' {..} = Prelude.rnf operationId

instance Data.ToHeaders GetOperation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetOperation where
  toJSON GetOperation' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("OperationId" Data..= operationId)]
      )

instance Data.ToPath GetOperation where
  toPath = Prelude.const "/get-operation"

instance Data.ToQuery GetOperation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetOperationResponse' smart constructor.
data GetOperationResponse = GetOperationResponse'
  { operation :: Prelude.Maybe Operation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetOperationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operation', 'getOperationResponse_operation' -
--
-- 'httpStatus', 'getOperationResponse_httpStatus' - The response's http status code.
newGetOperationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetOperationResponse
newGetOperationResponse pHttpStatus_ =
  GetOperationResponse'
    { operation = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

getOperationResponse_operation :: Lens.Lens' GetOperationResponse (Prelude.Maybe Operation)
getOperationResponse_operation = Lens.lens (\GetOperationResponse' {operation} -> operation) (\s@GetOperationResponse' {} a -> s {operation = a} :: GetOperationResponse)

-- | The response's http status code.
getOperationResponse_httpStatus :: Lens.Lens' GetOperationResponse Prelude.Int
getOperationResponse_httpStatus = Lens.lens (\GetOperationResponse' {httpStatus} -> httpStatus) (\s@GetOperationResponse' {} a -> s {httpStatus = a} :: GetOperationResponse)

instance Prelude.NFData GetOperationResponse where
  rnf GetOperationResponse' {..} =
    Prelude.rnf operation `Prelude.seq`
      Prelude.rnf httpStatus
