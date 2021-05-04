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
-- Module      : Network.AWS.Glue.UpdateUserDefinedFunction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing function definition in the Data Catalog.
module Network.AWS.Glue.UpdateUserDefinedFunction
  ( -- * Creating a Request
    UpdateUserDefinedFunction (..),
    newUpdateUserDefinedFunction,

    -- * Request Lenses
    updateUserDefinedFunction_catalogId,
    updateUserDefinedFunction_databaseName,
    updateUserDefinedFunction_functionName,
    updateUserDefinedFunction_functionInput,

    -- * Destructuring the Response
    UpdateUserDefinedFunctionResponse (..),
    newUpdateUserDefinedFunctionResponse,

    -- * Response Lenses
    updateUserDefinedFunctionResponse_httpStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateUserDefinedFunction' smart constructor.
data UpdateUserDefinedFunction = UpdateUserDefinedFunction'
  { -- | The ID of the Data Catalog where the function to be updated is located.
    -- If none is provided, the AWS account ID is used by default.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The name of the catalog database where the function to be updated is
    -- located.
    databaseName :: Prelude.Text,
    -- | The name of the function.
    functionName :: Prelude.Text,
    -- | A @FunctionInput@ object that redefines the function in the Data
    -- Catalog.
    functionInput :: UserDefinedFunctionInput
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateUserDefinedFunction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'updateUserDefinedFunction_catalogId' - The ID of the Data Catalog where the function to be updated is located.
-- If none is provided, the AWS account ID is used by default.
--
-- 'databaseName', 'updateUserDefinedFunction_databaseName' - The name of the catalog database where the function to be updated is
-- located.
--
-- 'functionName', 'updateUserDefinedFunction_functionName' - The name of the function.
--
-- 'functionInput', 'updateUserDefinedFunction_functionInput' - A @FunctionInput@ object that redefines the function in the Data
-- Catalog.
newUpdateUserDefinedFunction ::
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'functionName'
  Prelude.Text ->
  -- | 'functionInput'
  UserDefinedFunctionInput ->
  UpdateUserDefinedFunction
newUpdateUserDefinedFunction
  pDatabaseName_
  pFunctionName_
  pFunctionInput_ =
    UpdateUserDefinedFunction'
      { catalogId =
          Prelude.Nothing,
        databaseName = pDatabaseName_,
        functionName = pFunctionName_,
        functionInput = pFunctionInput_
      }

-- | The ID of the Data Catalog where the function to be updated is located.
-- If none is provided, the AWS account ID is used by default.
updateUserDefinedFunction_catalogId :: Lens.Lens' UpdateUserDefinedFunction (Prelude.Maybe Prelude.Text)
updateUserDefinedFunction_catalogId = Lens.lens (\UpdateUserDefinedFunction' {catalogId} -> catalogId) (\s@UpdateUserDefinedFunction' {} a -> s {catalogId = a} :: UpdateUserDefinedFunction)

-- | The name of the catalog database where the function to be updated is
-- located.
updateUserDefinedFunction_databaseName :: Lens.Lens' UpdateUserDefinedFunction Prelude.Text
updateUserDefinedFunction_databaseName = Lens.lens (\UpdateUserDefinedFunction' {databaseName} -> databaseName) (\s@UpdateUserDefinedFunction' {} a -> s {databaseName = a} :: UpdateUserDefinedFunction)

-- | The name of the function.
updateUserDefinedFunction_functionName :: Lens.Lens' UpdateUserDefinedFunction Prelude.Text
updateUserDefinedFunction_functionName = Lens.lens (\UpdateUserDefinedFunction' {functionName} -> functionName) (\s@UpdateUserDefinedFunction' {} a -> s {functionName = a} :: UpdateUserDefinedFunction)

-- | A @FunctionInput@ object that redefines the function in the Data
-- Catalog.
updateUserDefinedFunction_functionInput :: Lens.Lens' UpdateUserDefinedFunction UserDefinedFunctionInput
updateUserDefinedFunction_functionInput = Lens.lens (\UpdateUserDefinedFunction' {functionInput} -> functionInput) (\s@UpdateUserDefinedFunction' {} a -> s {functionInput = a} :: UpdateUserDefinedFunction)

instance Prelude.AWSRequest UpdateUserDefinedFunction where
  type
    Rs UpdateUserDefinedFunction =
      UpdateUserDefinedFunctionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateUserDefinedFunctionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateUserDefinedFunction

instance Prelude.NFData UpdateUserDefinedFunction

instance Prelude.ToHeaders UpdateUserDefinedFunction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSGlue.UpdateUserDefinedFunction" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateUserDefinedFunction where
  toJSON UpdateUserDefinedFunction' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("CatalogId" Prelude..=) Prelude.<$> catalogId,
            Prelude.Just
              ("DatabaseName" Prelude..= databaseName),
            Prelude.Just
              ("FunctionName" Prelude..= functionName),
            Prelude.Just
              ("FunctionInput" Prelude..= functionInput)
          ]
      )

instance Prelude.ToPath UpdateUserDefinedFunction where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateUserDefinedFunction where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateUserDefinedFunctionResponse' smart constructor.
data UpdateUserDefinedFunctionResponse = UpdateUserDefinedFunctionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateUserDefinedFunctionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateUserDefinedFunctionResponse_httpStatus' - The response's http status code.
newUpdateUserDefinedFunctionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateUserDefinedFunctionResponse
newUpdateUserDefinedFunctionResponse pHttpStatus_ =
  UpdateUserDefinedFunctionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateUserDefinedFunctionResponse_httpStatus :: Lens.Lens' UpdateUserDefinedFunctionResponse Prelude.Int
updateUserDefinedFunctionResponse_httpStatus = Lens.lens (\UpdateUserDefinedFunctionResponse' {httpStatus} -> httpStatus) (\s@UpdateUserDefinedFunctionResponse' {} a -> s {httpStatus = a} :: UpdateUserDefinedFunctionResponse)

instance
  Prelude.NFData
    UpdateUserDefinedFunctionResponse
