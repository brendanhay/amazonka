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

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteUserDefinedFunction' smart constructor.
data DeleteUserDefinedFunction = DeleteUserDefinedFunction'
  { -- | The ID of the Data Catalog where the function to be deleted is located.
    -- If none is supplied, the AWS account ID is used by default.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The name of the catalog database where the function is located.
    databaseName :: Prelude.Text,
    -- | The name of the function definition to be deleted.
    functionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'functionName'
  Prelude.Text ->
  DeleteUserDefinedFunction
newDeleteUserDefinedFunction
  pDatabaseName_
  pFunctionName_ =
    DeleteUserDefinedFunction'
      { catalogId =
          Prelude.Nothing,
        databaseName = pDatabaseName_,
        functionName = pFunctionName_
      }

-- | The ID of the Data Catalog where the function to be deleted is located.
-- If none is supplied, the AWS account ID is used by default.
deleteUserDefinedFunction_catalogId :: Lens.Lens' DeleteUserDefinedFunction (Prelude.Maybe Prelude.Text)
deleteUserDefinedFunction_catalogId = Lens.lens (\DeleteUserDefinedFunction' {catalogId} -> catalogId) (\s@DeleteUserDefinedFunction' {} a -> s {catalogId = a} :: DeleteUserDefinedFunction)

-- | The name of the catalog database where the function is located.
deleteUserDefinedFunction_databaseName :: Lens.Lens' DeleteUserDefinedFunction Prelude.Text
deleteUserDefinedFunction_databaseName = Lens.lens (\DeleteUserDefinedFunction' {databaseName} -> databaseName) (\s@DeleteUserDefinedFunction' {} a -> s {databaseName = a} :: DeleteUserDefinedFunction)

-- | The name of the function definition to be deleted.
deleteUserDefinedFunction_functionName :: Lens.Lens' DeleteUserDefinedFunction Prelude.Text
deleteUserDefinedFunction_functionName = Lens.lens (\DeleteUserDefinedFunction' {functionName} -> functionName) (\s@DeleteUserDefinedFunction' {} a -> s {functionName = a} :: DeleteUserDefinedFunction)

instance Prelude.AWSRequest DeleteUserDefinedFunction where
  type
    Rs DeleteUserDefinedFunction =
      DeleteUserDefinedFunctionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteUserDefinedFunctionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteUserDefinedFunction

instance Prelude.NFData DeleteUserDefinedFunction

instance Prelude.ToHeaders DeleteUserDefinedFunction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSGlue.DeleteUserDefinedFunction" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteUserDefinedFunction where
  toJSON DeleteUserDefinedFunction' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("CatalogId" Prelude..=) Prelude.<$> catalogId,
            Prelude.Just
              ("DatabaseName" Prelude..= databaseName),
            Prelude.Just
              ("FunctionName" Prelude..= functionName)
          ]
      )

instance Prelude.ToPath DeleteUserDefinedFunction where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteUserDefinedFunction where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteUserDefinedFunctionResponse' smart constructor.
data DeleteUserDefinedFunctionResponse = DeleteUserDefinedFunctionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DeleteUserDefinedFunctionResponse
newDeleteUserDefinedFunctionResponse pHttpStatus_ =
  DeleteUserDefinedFunctionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteUserDefinedFunctionResponse_httpStatus :: Lens.Lens' DeleteUserDefinedFunctionResponse Prelude.Int
deleteUserDefinedFunctionResponse_httpStatus = Lens.lens (\DeleteUserDefinedFunctionResponse' {httpStatus} -> httpStatus) (\s@DeleteUserDefinedFunctionResponse' {} a -> s {httpStatus = a} :: DeleteUserDefinedFunctionResponse)

instance
  Prelude.NFData
    DeleteUserDefinedFunctionResponse
