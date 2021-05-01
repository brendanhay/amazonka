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

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetOperation' smart constructor.
data GetOperation = GetOperation'
  { -- | A GUID used to identify the operation.
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
-- 'operationId', 'getOperation_operationId' - A GUID used to identify the operation.
newGetOperation ::
  -- | 'operationId'
  Prelude.Text ->
  GetOperation
newGetOperation pOperationId_ =
  GetOperation' {operationId = pOperationId_}

-- | A GUID used to identify the operation.
getOperation_operationId :: Lens.Lens' GetOperation Prelude.Text
getOperation_operationId = Lens.lens (\GetOperation' {operationId} -> operationId) (\s@GetOperation' {} a -> s {operationId = a} :: GetOperation)

instance Prelude.AWSRequest GetOperation where
  type Rs GetOperation = GetOperationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetOperationResponse'
            Prelude.<$> (x Prelude..?> "operation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetOperation

instance Prelude.NFData GetOperation

instance Prelude.ToHeaders GetOperation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Lightsail_20161128.GetOperation" ::
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
              ("operationId" Prelude..= operationId)
          ]
      )

instance Prelude.ToPath GetOperation where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetOperation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetOperationResponse' smart constructor.
data GetOperationResponse = GetOperationResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
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
-- 'operation', 'getOperationResponse_operation' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
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

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
getOperationResponse_operation :: Lens.Lens' GetOperationResponse (Prelude.Maybe Operation)
getOperationResponse_operation = Lens.lens (\GetOperationResponse' {operation} -> operation) (\s@GetOperationResponse' {} a -> s {operation = a} :: GetOperationResponse)

-- | The response's http status code.
getOperationResponse_httpStatus :: Lens.Lens' GetOperationResponse Prelude.Int
getOperationResponse_httpStatus = Lens.lens (\GetOperationResponse' {httpStatus} -> httpStatus) (\s@GetOperationResponse' {} a -> s {httpStatus = a} :: GetOperationResponse)

instance Prelude.NFData GetOperationResponse
