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
-- Module      : Amazonka.Glue.CreateUserDefinedFunction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new function definition in the Data Catalog.
module Amazonka.Glue.CreateUserDefinedFunction
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateUserDefinedFunction' smart constructor.
data CreateUserDefinedFunction = CreateUserDefinedFunction'
  { -- | The ID of the Data Catalog in which to create the function. If none is
    -- provided, the Amazon Web Services account ID is used by default.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The name of the catalog database in which to create the function.
    databaseName :: Prelude.Text,
    -- | A @FunctionInput@ object that defines the function to create in the Data
    -- Catalog.
    functionInput :: UserDefinedFunctionInput
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateUserDefinedFunction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'createUserDefinedFunction_catalogId' - The ID of the Data Catalog in which to create the function. If none is
-- provided, the Amazon Web Services account ID is used by default.
--
-- 'databaseName', 'createUserDefinedFunction_databaseName' - The name of the catalog database in which to create the function.
--
-- 'functionInput', 'createUserDefinedFunction_functionInput' - A @FunctionInput@ object that defines the function to create in the Data
-- Catalog.
newCreateUserDefinedFunction ::
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'functionInput'
  UserDefinedFunctionInput ->
  CreateUserDefinedFunction
newCreateUserDefinedFunction
  pDatabaseName_
  pFunctionInput_ =
    CreateUserDefinedFunction'
      { catalogId =
          Prelude.Nothing,
        databaseName = pDatabaseName_,
        functionInput = pFunctionInput_
      }

-- | The ID of the Data Catalog in which to create the function. If none is
-- provided, the Amazon Web Services account ID is used by default.
createUserDefinedFunction_catalogId :: Lens.Lens' CreateUserDefinedFunction (Prelude.Maybe Prelude.Text)
createUserDefinedFunction_catalogId = Lens.lens (\CreateUserDefinedFunction' {catalogId} -> catalogId) (\s@CreateUserDefinedFunction' {} a -> s {catalogId = a} :: CreateUserDefinedFunction)

-- | The name of the catalog database in which to create the function.
createUserDefinedFunction_databaseName :: Lens.Lens' CreateUserDefinedFunction Prelude.Text
createUserDefinedFunction_databaseName = Lens.lens (\CreateUserDefinedFunction' {databaseName} -> databaseName) (\s@CreateUserDefinedFunction' {} a -> s {databaseName = a} :: CreateUserDefinedFunction)

-- | A @FunctionInput@ object that defines the function to create in the Data
-- Catalog.
createUserDefinedFunction_functionInput :: Lens.Lens' CreateUserDefinedFunction UserDefinedFunctionInput
createUserDefinedFunction_functionInput = Lens.lens (\CreateUserDefinedFunction' {functionInput} -> functionInput) (\s@CreateUserDefinedFunction' {} a -> s {functionInput = a} :: CreateUserDefinedFunction)

instance Core.AWSRequest CreateUserDefinedFunction where
  type
    AWSResponse CreateUserDefinedFunction =
      CreateUserDefinedFunctionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateUserDefinedFunctionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateUserDefinedFunction where
  hashWithSalt _salt CreateUserDefinedFunction' {..} =
    _salt `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` functionInput

instance Prelude.NFData CreateUserDefinedFunction where
  rnf CreateUserDefinedFunction' {..} =
    Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf functionInput

instance Data.ToHeaders CreateUserDefinedFunction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSGlue.CreateUserDefinedFunction" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateUserDefinedFunction where
  toJSON CreateUserDefinedFunction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CatalogId" Data..=) Prelude.<$> catalogId,
            Prelude.Just ("DatabaseName" Data..= databaseName),
            Prelude.Just
              ("FunctionInput" Data..= functionInput)
          ]
      )

instance Data.ToPath CreateUserDefinedFunction where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateUserDefinedFunction where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateUserDefinedFunctionResponse' smart constructor.
data CreateUserDefinedFunctionResponse = CreateUserDefinedFunctionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateUserDefinedFunctionResponse
newCreateUserDefinedFunctionResponse pHttpStatus_ =
  CreateUserDefinedFunctionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
createUserDefinedFunctionResponse_httpStatus :: Lens.Lens' CreateUserDefinedFunctionResponse Prelude.Int
createUserDefinedFunctionResponse_httpStatus = Lens.lens (\CreateUserDefinedFunctionResponse' {httpStatus} -> httpStatus) (\s@CreateUserDefinedFunctionResponse' {} a -> s {httpStatus = a} :: CreateUserDefinedFunctionResponse)

instance
  Prelude.NFData
    CreateUserDefinedFunctionResponse
  where
  rnf CreateUserDefinedFunctionResponse' {..} =
    Prelude.rnf httpStatus
