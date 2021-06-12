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
-- Module      : Network.AWS.Glue.CreateUserDefinedFunction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new function definition in the Data Catalog.
module Network.AWS.Glue.CreateUserDefinedFunction
  ( -- * Creating a Request
    CreateUserDefinedFunction (..),
    newCreateUserDefinedFunction,

    -- * Request Lenses
    createUserDefinedFunction_catalogId,
    createUserDefinedFunction_databaseName,
    createUserDefinedFunction_functionInput,

    -- * Destructuring the Response
    CreateUserDefinedFunctionResponse (..),
    newCreateUserDefinedFunctionResponse,

    -- * Response Lenses
    createUserDefinedFunctionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateUserDefinedFunction' smart constructor.
data CreateUserDefinedFunction = CreateUserDefinedFunction'
  { -- | The ID of the Data Catalog in which to create the function. If none is
    -- provided, the AWS account ID is used by default.
    catalogId :: Core.Maybe Core.Text,
    -- | The name of the catalog database in which to create the function.
    databaseName :: Core.Text,
    -- | A @FunctionInput@ object that defines the function to create in the Data
    -- Catalog.
    functionInput :: UserDefinedFunctionInput
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateUserDefinedFunction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'createUserDefinedFunction_catalogId' - The ID of the Data Catalog in which to create the function. If none is
-- provided, the AWS account ID is used by default.
--
-- 'databaseName', 'createUserDefinedFunction_databaseName' - The name of the catalog database in which to create the function.
--
-- 'functionInput', 'createUserDefinedFunction_functionInput' - A @FunctionInput@ object that defines the function to create in the Data
-- Catalog.
newCreateUserDefinedFunction ::
  -- | 'databaseName'
  Core.Text ->
  -- | 'functionInput'
  UserDefinedFunctionInput ->
  CreateUserDefinedFunction
newCreateUserDefinedFunction
  pDatabaseName_
  pFunctionInput_ =
    CreateUserDefinedFunction'
      { catalogId =
          Core.Nothing,
        databaseName = pDatabaseName_,
        functionInput = pFunctionInput_
      }

-- | The ID of the Data Catalog in which to create the function. If none is
-- provided, the AWS account ID is used by default.
createUserDefinedFunction_catalogId :: Lens.Lens' CreateUserDefinedFunction (Core.Maybe Core.Text)
createUserDefinedFunction_catalogId = Lens.lens (\CreateUserDefinedFunction' {catalogId} -> catalogId) (\s@CreateUserDefinedFunction' {} a -> s {catalogId = a} :: CreateUserDefinedFunction)

-- | The name of the catalog database in which to create the function.
createUserDefinedFunction_databaseName :: Lens.Lens' CreateUserDefinedFunction Core.Text
createUserDefinedFunction_databaseName = Lens.lens (\CreateUserDefinedFunction' {databaseName} -> databaseName) (\s@CreateUserDefinedFunction' {} a -> s {databaseName = a} :: CreateUserDefinedFunction)

-- | A @FunctionInput@ object that defines the function to create in the Data
-- Catalog.
createUserDefinedFunction_functionInput :: Lens.Lens' CreateUserDefinedFunction UserDefinedFunctionInput
createUserDefinedFunction_functionInput = Lens.lens (\CreateUserDefinedFunction' {functionInput} -> functionInput) (\s@CreateUserDefinedFunction' {} a -> s {functionInput = a} :: CreateUserDefinedFunction)

instance Core.AWSRequest CreateUserDefinedFunction where
  type
    AWSResponse CreateUserDefinedFunction =
      CreateUserDefinedFunctionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateUserDefinedFunctionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateUserDefinedFunction

instance Core.NFData CreateUserDefinedFunction

instance Core.ToHeaders CreateUserDefinedFunction where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSGlue.CreateUserDefinedFunction" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateUserDefinedFunction where
  toJSON CreateUserDefinedFunction' {..} =
    Core.object
      ( Core.catMaybes
          [ ("CatalogId" Core..=) Core.<$> catalogId,
            Core.Just ("DatabaseName" Core..= databaseName),
            Core.Just ("FunctionInput" Core..= functionInput)
          ]
      )

instance Core.ToPath CreateUserDefinedFunction where
  toPath = Core.const "/"

instance Core.ToQuery CreateUserDefinedFunction where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateUserDefinedFunctionResponse' smart constructor.
data CreateUserDefinedFunctionResponse = CreateUserDefinedFunctionResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateUserDefinedFunctionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createUserDefinedFunctionResponse_httpStatus' - The response's http status code.
newCreateUserDefinedFunctionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateUserDefinedFunctionResponse
newCreateUserDefinedFunctionResponse pHttpStatus_ =
  CreateUserDefinedFunctionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
createUserDefinedFunctionResponse_httpStatus :: Lens.Lens' CreateUserDefinedFunctionResponse Core.Int
createUserDefinedFunctionResponse_httpStatus = Lens.lens (\CreateUserDefinedFunctionResponse' {httpStatus} -> httpStatus) (\s@CreateUserDefinedFunctionResponse' {} a -> s {httpStatus = a} :: CreateUserDefinedFunctionResponse)

instance
  Core.NFData
    CreateUserDefinedFunctionResponse
