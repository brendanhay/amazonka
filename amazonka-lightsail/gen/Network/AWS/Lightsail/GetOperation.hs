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
-- Module      : Network.AWS.Lightsail.GetOperation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific operation. Operations include
-- events such as when you create an instance, allocate a static IP, attach
-- a static IP, and so on.
module Network.AWS.Lightsail.GetOperation
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
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetOperation' smart constructor.
data GetOperation = GetOperation'
  { -- | A GUID used to identify the operation.
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
-- 'operationId', 'getOperation_operationId' - A GUID used to identify the operation.
newGetOperation ::
  -- | 'operationId'
  Core.Text ->
  GetOperation
newGetOperation pOperationId_ =
  GetOperation' {operationId = pOperationId_}

-- | A GUID used to identify the operation.
getOperation_operationId :: Lens.Lens' GetOperation Core.Text
getOperation_operationId = Lens.lens (\GetOperation' {operationId} -> operationId) (\s@GetOperation' {} a -> s {operationId = a} :: GetOperation)

instance Core.AWSRequest GetOperation where
  type AWSResponse GetOperation = GetOperationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetOperationResponse'
            Core.<$> (x Core..?> "operation")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetOperation

instance Core.NFData GetOperation

instance Core.ToHeaders GetOperation where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.GetOperation" ::
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
          [Core.Just ("operationId" Core..= operationId)]
      )

instance Core.ToPath GetOperation where
  toPath = Core.const "/"

instance Core.ToQuery GetOperation where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetOperationResponse' smart constructor.
data GetOperationResponse = GetOperationResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
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
-- 'operation', 'getOperationResponse_operation' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
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

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
getOperationResponse_operation :: Lens.Lens' GetOperationResponse (Core.Maybe Operation)
getOperationResponse_operation = Lens.lens (\GetOperationResponse' {operation} -> operation) (\s@GetOperationResponse' {} a -> s {operation = a} :: GetOperationResponse)

-- | The response's http status code.
getOperationResponse_httpStatus :: Lens.Lens' GetOperationResponse Core.Int
getOperationResponse_httpStatus = Lens.lens (\GetOperationResponse' {httpStatus} -> httpStatus) (\s@GetOperationResponse' {} a -> s {httpStatus = a} :: GetOperationResponse)

instance Core.NFData GetOperationResponse
