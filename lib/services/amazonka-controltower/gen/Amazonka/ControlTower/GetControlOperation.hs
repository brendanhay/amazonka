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
-- Module      : Amazonka.ControlTower.GetControlOperation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the status of a particular @EnableControl@ or @DisableControl@
-- operation. Displays a message in case of error. Details for an operation
-- are available for 90 days.
module Amazonka.ControlTower.GetControlOperation
  ( -- * Creating a Request
    GetControlOperation (..),
    newGetControlOperation,

    -- * Request Lenses
    getControlOperation_operationIdentifier,

    -- * Destructuring the Response
    GetControlOperationResponse (..),
    newGetControlOperationResponse,

    -- * Response Lenses
    getControlOperationResponse_httpStatus,
    getControlOperationResponse_controlOperation,
  )
where

import Amazonka.ControlTower.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetControlOperation' smart constructor.
data GetControlOperation = GetControlOperation'
  { -- | The ID of the asynchronous operation, which is used to track status. The
    -- operation is available for 90 days.
    operationIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetControlOperation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operationIdentifier', 'getControlOperation_operationIdentifier' - The ID of the asynchronous operation, which is used to track status. The
-- operation is available for 90 days.
newGetControlOperation ::
  -- | 'operationIdentifier'
  Prelude.Text ->
  GetControlOperation
newGetControlOperation pOperationIdentifier_ =
  GetControlOperation'
    { operationIdentifier =
        pOperationIdentifier_
    }

-- | The ID of the asynchronous operation, which is used to track status. The
-- operation is available for 90 days.
getControlOperation_operationIdentifier :: Lens.Lens' GetControlOperation Prelude.Text
getControlOperation_operationIdentifier = Lens.lens (\GetControlOperation' {operationIdentifier} -> operationIdentifier) (\s@GetControlOperation' {} a -> s {operationIdentifier = a} :: GetControlOperation)

instance Core.AWSRequest GetControlOperation where
  type
    AWSResponse GetControlOperation =
      GetControlOperationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetControlOperationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "controlOperation")
      )

instance Prelude.Hashable GetControlOperation where
  hashWithSalt _salt GetControlOperation' {..} =
    _salt `Prelude.hashWithSalt` operationIdentifier

instance Prelude.NFData GetControlOperation where
  rnf GetControlOperation' {..} =
    Prelude.rnf operationIdentifier

instance Core.ToHeaders GetControlOperation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetControlOperation where
  toJSON GetControlOperation' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("operationIdentifier" Core..= operationIdentifier)
          ]
      )

instance Core.ToPath GetControlOperation where
  toPath = Prelude.const "/get-control-operation"

instance Core.ToQuery GetControlOperation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetControlOperationResponse' smart constructor.
data GetControlOperationResponse = GetControlOperationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    controlOperation :: ControlOperation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetControlOperationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getControlOperationResponse_httpStatus' - The response's http status code.
--
-- 'controlOperation', 'getControlOperationResponse_controlOperation' -
newGetControlOperationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'controlOperation'
  ControlOperation ->
  GetControlOperationResponse
newGetControlOperationResponse
  pHttpStatus_
  pControlOperation_ =
    GetControlOperationResponse'
      { httpStatus =
          pHttpStatus_,
        controlOperation = pControlOperation_
      }

-- | The response's http status code.
getControlOperationResponse_httpStatus :: Lens.Lens' GetControlOperationResponse Prelude.Int
getControlOperationResponse_httpStatus = Lens.lens (\GetControlOperationResponse' {httpStatus} -> httpStatus) (\s@GetControlOperationResponse' {} a -> s {httpStatus = a} :: GetControlOperationResponse)

-- |
getControlOperationResponse_controlOperation :: Lens.Lens' GetControlOperationResponse ControlOperation
getControlOperationResponse_controlOperation = Lens.lens (\GetControlOperationResponse' {controlOperation} -> controlOperation) (\s@GetControlOperationResponse' {} a -> s {controlOperation = a} :: GetControlOperationResponse)

instance Prelude.NFData GetControlOperationResponse where
  rnf GetControlOperationResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf controlOperation
