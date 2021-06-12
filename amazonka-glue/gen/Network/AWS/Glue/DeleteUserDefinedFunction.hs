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
-- Module      : Network.AWS.Glue.DeleteUserDefinedFunction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing function definition from the Data Catalog.
module Network.AWS.Glue.DeleteUserDefinedFunction
  ( -- * Creating a Request
    DeleteUserDefinedFunction (..),
    newDeleteUserDefinedFunction,

    -- * Request Lenses
    deleteUserDefinedFunction_catalogId,
    deleteUserDefinedFunction_databaseName,
    deleteUserDefinedFunction_functionName,

    -- * Destructuring the Response
    DeleteUserDefinedFunctionResponse (..),
    newDeleteUserDefinedFunctionResponse,

    -- * Response Lenses
    deleteUserDefinedFunctionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteUserDefinedFunction' smart constructor.
data DeleteUserDefinedFunction = DeleteUserDefinedFunction'
  { -- | The ID of the Data Catalog where the function to be deleted is located.
    -- If none is supplied, the AWS account ID is used by default.
    catalogId :: Core.Maybe Core.Text,
    -- | The name of the catalog database where the function is located.
    databaseName :: Core.Text,
    -- | The name of the function definition to be deleted.
    functionName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteUserDefinedFunction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'deleteUserDefinedFunction_catalogId' - The ID of the Data Catalog where the function to be deleted is located.
-- If none is supplied, the AWS account ID is used by default.
--
-- 'databaseName', 'deleteUserDefinedFunction_databaseName' - The name of the catalog database where the function is located.
--
-- 'functionName', 'deleteUserDefinedFunction_functionName' - The name of the function definition to be deleted.
newDeleteUserDefinedFunction ::
  -- | 'databaseName'
  Core.Text ->
  -- | 'functionName'
  Core.Text ->
  DeleteUserDefinedFunction
newDeleteUserDefinedFunction
  pDatabaseName_
  pFunctionName_ =
    DeleteUserDefinedFunction'
      { catalogId =
          Core.Nothing,
        databaseName = pDatabaseName_,
        functionName = pFunctionName_
      }

-- | The ID of the Data Catalog where the function to be deleted is located.
-- If none is supplied, the AWS account ID is used by default.
deleteUserDefinedFunction_catalogId :: Lens.Lens' DeleteUserDefinedFunction (Core.Maybe Core.Text)
deleteUserDefinedFunction_catalogId = Lens.lens (\DeleteUserDefinedFunction' {catalogId} -> catalogId) (\s@DeleteUserDefinedFunction' {} a -> s {catalogId = a} :: DeleteUserDefinedFunction)

-- | The name of the catalog database where the function is located.
deleteUserDefinedFunction_databaseName :: Lens.Lens' DeleteUserDefinedFunction Core.Text
deleteUserDefinedFunction_databaseName = Lens.lens (\DeleteUserDefinedFunction' {databaseName} -> databaseName) (\s@DeleteUserDefinedFunction' {} a -> s {databaseName = a} :: DeleteUserDefinedFunction)

-- | The name of the function definition to be deleted.
deleteUserDefinedFunction_functionName :: Lens.Lens' DeleteUserDefinedFunction Core.Text
deleteUserDefinedFunction_functionName = Lens.lens (\DeleteUserDefinedFunction' {functionName} -> functionName) (\s@DeleteUserDefinedFunction' {} a -> s {functionName = a} :: DeleteUserDefinedFunction)

instance Core.AWSRequest DeleteUserDefinedFunction where
  type
    AWSResponse DeleteUserDefinedFunction =
      DeleteUserDefinedFunctionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteUserDefinedFunctionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteUserDefinedFunction

instance Core.NFData DeleteUserDefinedFunction

instance Core.ToHeaders DeleteUserDefinedFunction where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSGlue.DeleteUserDefinedFunction" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteUserDefinedFunction where
  toJSON DeleteUserDefinedFunction' {..} =
    Core.object
      ( Core.catMaybes
          [ ("CatalogId" Core..=) Core.<$> catalogId,
            Core.Just ("DatabaseName" Core..= databaseName),
            Core.Just ("FunctionName" Core..= functionName)
          ]
      )

instance Core.ToPath DeleteUserDefinedFunction where
  toPath = Core.const "/"

instance Core.ToQuery DeleteUserDefinedFunction where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteUserDefinedFunctionResponse' smart constructor.
data DeleteUserDefinedFunctionResponse = DeleteUserDefinedFunctionResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteUserDefinedFunctionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteUserDefinedFunctionResponse_httpStatus' - The response's http status code.
newDeleteUserDefinedFunctionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteUserDefinedFunctionResponse
newDeleteUserDefinedFunctionResponse pHttpStatus_ =
  DeleteUserDefinedFunctionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteUserDefinedFunctionResponse_httpStatus :: Lens.Lens' DeleteUserDefinedFunctionResponse Core.Int
deleteUserDefinedFunctionResponse_httpStatus = Lens.lens (\DeleteUserDefinedFunctionResponse' {httpStatus} -> httpStatus) (\s@DeleteUserDefinedFunctionResponse' {} a -> s {httpStatus = a} :: DeleteUserDefinedFunctionResponse)

instance
  Core.NFData
    DeleteUserDefinedFunctionResponse
