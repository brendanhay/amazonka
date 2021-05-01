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
-- Module      : Network.AWS.Route53AutoNaming.GetOperation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about any operation that returns an operation ID in the
-- response, such as a @CreateService@ request.
--
-- To get a list of operations that match specified criteria, see
-- <https://docs.aws.amazon.com/cloud-map/latest/api/API_ListOperations.html ListOperations>.
module Network.AWS.Route53AutoNaming.GetOperation
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53AutoNaming.Types

-- | /See:/ 'newGetOperation' smart constructor.
data GetOperation = GetOperation'
  { -- | The ID of the operation that you want to get more information about.
    operationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetOperation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operationId', 'getOperation_operationId' - The ID of the operation that you want to get more information about.
newGetOperation ::
  -- | 'operationId'
  Prelude.Text ->
  GetOperation
newGetOperation pOperationId_ =
  GetOperation' {operationId = pOperationId_}

-- | The ID of the operation that you want to get more information about.
getOperation_operationId :: Lens.Lens' GetOperation Prelude.Text
getOperation_operationId = Lens.lens (\GetOperation' {operationId} -> operationId) (\s@GetOperation' {} a -> s {operationId = a} :: GetOperation)

instance Prelude.AWSRequest GetOperation where
  type Rs GetOperation = GetOperationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetOperationResponse'
            Prelude.<$> (x Prelude..?> "Operation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetOperation

instance Prelude.NFData GetOperation

instance Prelude.ToHeaders GetOperation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Route53AutoNaming_v20170314.GetOperation" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetOperation where
  toJSON GetOperation' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("OperationId" Prelude..= operationId)
          ]
      )

instance Prelude.ToPath GetOperation where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetOperation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetOperationResponse' smart constructor.
data GetOperationResponse = GetOperationResponse'
  { -- | A complex type that contains information about the operation.
    operation :: Prelude.Maybe Operation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetOperationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operation', 'getOperationResponse_operation' - A complex type that contains information about the operation.
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

-- | A complex type that contains information about the operation.
getOperationResponse_operation :: Lens.Lens' GetOperationResponse (Prelude.Maybe Operation)
getOperationResponse_operation = Lens.lens (\GetOperationResponse' {operation} -> operation) (\s@GetOperationResponse' {} a -> s {operation = a} :: GetOperationResponse)

-- | The response's http status code.
getOperationResponse_httpStatus :: Lens.Lens' GetOperationResponse Prelude.Int
getOperationResponse_httpStatus = Lens.lens (\GetOperationResponse' {httpStatus} -> httpStatus) (\s@GetOperationResponse' {} a -> s {httpStatus = a} :: GetOperationResponse)

instance Prelude.NFData GetOperationResponse
