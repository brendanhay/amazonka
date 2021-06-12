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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53AutoNaming.Types

-- | /See:/ 'newGetOperation' smart constructor.
data GetOperation = GetOperation'
  { -- | The ID of the operation that you want to get more information about.
    operationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  GetOperation
newGetOperation pOperationId_ =
  GetOperation' {operationId = pOperationId_}

-- | The ID of the operation that you want to get more information about.
getOperation_operationId :: Lens.Lens' GetOperation Core.Text
getOperation_operationId = Lens.lens (\GetOperation' {operationId} -> operationId) (\s@GetOperation' {} a -> s {operationId = a} :: GetOperation)

instance Core.AWSRequest GetOperation where
  type AWSResponse GetOperation = GetOperationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetOperationResponse'
            Core.<$> (x Core..?> "Operation")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetOperation

instance Core.NFData GetOperation

instance Core.ToHeaders GetOperation where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Route53AutoNaming_v20170314.GetOperation" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetOperation where
  toJSON GetOperation' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("OperationId" Core..= operationId)]
      )

instance Core.ToPath GetOperation where
  toPath = Core.const "/"

instance Core.ToQuery GetOperation where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetOperationResponse' smart constructor.
data GetOperationResponse = GetOperationResponse'
  { -- | A complex type that contains information about the operation.
    operation :: Core.Maybe Operation,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetOperationResponse
newGetOperationResponse pHttpStatus_ =
  GetOperationResponse'
    { operation = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A complex type that contains information about the operation.
getOperationResponse_operation :: Lens.Lens' GetOperationResponse (Core.Maybe Operation)
getOperationResponse_operation = Lens.lens (\GetOperationResponse' {operation} -> operation) (\s@GetOperationResponse' {} a -> s {operation = a} :: GetOperationResponse)

-- | The response's http status code.
getOperationResponse_httpStatus :: Lens.Lens' GetOperationResponse Core.Int
getOperationResponse_httpStatus = Lens.lens (\GetOperationResponse' {httpStatus} -> httpStatus) (\s@GetOperationResponse' {} a -> s {httpStatus = a} :: GetOperationResponse)

instance Core.NFData GetOperationResponse
