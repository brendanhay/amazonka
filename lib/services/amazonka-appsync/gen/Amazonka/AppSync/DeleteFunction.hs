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
-- Module      : Amazonka.AppSync.DeleteFunction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a @Function@.
module Amazonka.AppSync.DeleteFunction
  ( -- * Creating a Request
    DeleteFunction (..),
    newDeleteFunction,

    -- * Request Lenses
    deleteFunction_apiId,
    deleteFunction_functionId,

    -- * Destructuring the Response
    DeleteFunctionResponse (..),
    newDeleteFunctionResponse,

    -- * Response Lenses
    deleteFunctionResponse_httpStatus,
  )
where

import Amazonka.AppSync.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteFunction' smart constructor.
data DeleteFunction = DeleteFunction'
  { -- | The GraphQL API ID.
    apiId :: Prelude.Text,
    -- | The @Function@ ID.
    functionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFunction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiId', 'deleteFunction_apiId' - The GraphQL API ID.
--
-- 'functionId', 'deleteFunction_functionId' - The @Function@ ID.
newDeleteFunction ::
  -- | 'apiId'
  Prelude.Text ->
  -- | 'functionId'
  Prelude.Text ->
  DeleteFunction
newDeleteFunction pApiId_ pFunctionId_ =
  DeleteFunction'
    { apiId = pApiId_,
      functionId = pFunctionId_
    }

-- | The GraphQL API ID.
deleteFunction_apiId :: Lens.Lens' DeleteFunction Prelude.Text
deleteFunction_apiId = Lens.lens (\DeleteFunction' {apiId} -> apiId) (\s@DeleteFunction' {} a -> s {apiId = a} :: DeleteFunction)

-- | The @Function@ ID.
deleteFunction_functionId :: Lens.Lens' DeleteFunction Prelude.Text
deleteFunction_functionId = Lens.lens (\DeleteFunction' {functionId} -> functionId) (\s@DeleteFunction' {} a -> s {functionId = a} :: DeleteFunction)

instance Core.AWSRequest DeleteFunction where
  type
    AWSResponse DeleteFunction =
      DeleteFunctionResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteFunctionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteFunction where
  hashWithSalt _salt DeleteFunction' {..} =
    _salt
      `Prelude.hashWithSalt` apiId
      `Prelude.hashWithSalt` functionId

instance Prelude.NFData DeleteFunction where
  rnf DeleteFunction' {..} =
    Prelude.rnf apiId
      `Prelude.seq` Prelude.rnf functionId

instance Data.ToHeaders DeleteFunction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteFunction where
  toPath DeleteFunction' {..} =
    Prelude.mconcat
      [ "/v1/apis/",
        Data.toBS apiId,
        "/functions/",
        Data.toBS functionId
      ]

instance Data.ToQuery DeleteFunction where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteFunctionResponse' smart constructor.
data DeleteFunctionResponse = DeleteFunctionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFunctionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteFunctionResponse_httpStatus' - The response's http status code.
newDeleteFunctionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteFunctionResponse
newDeleteFunctionResponse pHttpStatus_ =
  DeleteFunctionResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteFunctionResponse_httpStatus :: Lens.Lens' DeleteFunctionResponse Prelude.Int
deleteFunctionResponse_httpStatus = Lens.lens (\DeleteFunctionResponse' {httpStatus} -> httpStatus) (\s@DeleteFunctionResponse' {} a -> s {httpStatus = a} :: DeleteFunctionResponse)

instance Prelude.NFData DeleteFunctionResponse where
  rnf DeleteFunctionResponse' {..} =
    Prelude.rnf httpStatus
