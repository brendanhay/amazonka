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
-- Module      : Network.AWS.AppSync.DeleteFunction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a @Function@.
module Network.AWS.AppSync.DeleteFunction
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

import Network.AWS.AppSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteFunction' smart constructor.
data DeleteFunction = DeleteFunction'
  { -- | The GraphQL API ID.
    apiId :: Prelude.Text,
    -- | The @Function@ ID.
    functionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DeleteFunction where
  type Rs DeleteFunction = DeleteFunctionResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteFunctionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteFunction

instance Prelude.NFData DeleteFunction

instance Prelude.ToHeaders DeleteFunction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath DeleteFunction where
  toPath DeleteFunction' {..} =
    Prelude.mconcat
      [ "/v1/apis/",
        Prelude.toBS apiId,
        "/functions/",
        Prelude.toBS functionId
      ]

instance Prelude.ToQuery DeleteFunction where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteFunctionResponse' smart constructor.
data DeleteFunctionResponse = DeleteFunctionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData DeleteFunctionResponse
