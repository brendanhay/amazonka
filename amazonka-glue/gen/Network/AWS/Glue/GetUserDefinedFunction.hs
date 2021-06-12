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
-- Module      : Network.AWS.Glue.GetUserDefinedFunction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a specified function definition from the Data Catalog.
module Network.AWS.Glue.GetUserDefinedFunction
  ( -- * Creating a Request
    GetUserDefinedFunction (..),
    newGetUserDefinedFunction,

    -- * Request Lenses
    getUserDefinedFunction_catalogId,
    getUserDefinedFunction_databaseName,
    getUserDefinedFunction_functionName,

    -- * Destructuring the Response
    GetUserDefinedFunctionResponse (..),
    newGetUserDefinedFunctionResponse,

    -- * Response Lenses
    getUserDefinedFunctionResponse_userDefinedFunction,
    getUserDefinedFunctionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetUserDefinedFunction' smart constructor.
data GetUserDefinedFunction = GetUserDefinedFunction'
  { -- | The ID of the Data Catalog where the function to be retrieved is
    -- located. If none is provided, the AWS account ID is used by default.
    catalogId :: Core.Maybe Core.Text,
    -- | The name of the catalog database where the function is located.
    databaseName :: Core.Text,
    -- | The name of the function.
    functionName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetUserDefinedFunction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'getUserDefinedFunction_catalogId' - The ID of the Data Catalog where the function to be retrieved is
-- located. If none is provided, the AWS account ID is used by default.
--
-- 'databaseName', 'getUserDefinedFunction_databaseName' - The name of the catalog database where the function is located.
--
-- 'functionName', 'getUserDefinedFunction_functionName' - The name of the function.
newGetUserDefinedFunction ::
  -- | 'databaseName'
  Core.Text ->
  -- | 'functionName'
  Core.Text ->
  GetUserDefinedFunction
newGetUserDefinedFunction
  pDatabaseName_
  pFunctionName_ =
    GetUserDefinedFunction'
      { catalogId = Core.Nothing,
        databaseName = pDatabaseName_,
        functionName = pFunctionName_
      }

-- | The ID of the Data Catalog where the function to be retrieved is
-- located. If none is provided, the AWS account ID is used by default.
getUserDefinedFunction_catalogId :: Lens.Lens' GetUserDefinedFunction (Core.Maybe Core.Text)
getUserDefinedFunction_catalogId = Lens.lens (\GetUserDefinedFunction' {catalogId} -> catalogId) (\s@GetUserDefinedFunction' {} a -> s {catalogId = a} :: GetUserDefinedFunction)

-- | The name of the catalog database where the function is located.
getUserDefinedFunction_databaseName :: Lens.Lens' GetUserDefinedFunction Core.Text
getUserDefinedFunction_databaseName = Lens.lens (\GetUserDefinedFunction' {databaseName} -> databaseName) (\s@GetUserDefinedFunction' {} a -> s {databaseName = a} :: GetUserDefinedFunction)

-- | The name of the function.
getUserDefinedFunction_functionName :: Lens.Lens' GetUserDefinedFunction Core.Text
getUserDefinedFunction_functionName = Lens.lens (\GetUserDefinedFunction' {functionName} -> functionName) (\s@GetUserDefinedFunction' {} a -> s {functionName = a} :: GetUserDefinedFunction)

instance Core.AWSRequest GetUserDefinedFunction where
  type
    AWSResponse GetUserDefinedFunction =
      GetUserDefinedFunctionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetUserDefinedFunctionResponse'
            Core.<$> (x Core..?> "UserDefinedFunction")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetUserDefinedFunction

instance Core.NFData GetUserDefinedFunction

instance Core.ToHeaders GetUserDefinedFunction where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSGlue.GetUserDefinedFunction" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetUserDefinedFunction where
  toJSON GetUserDefinedFunction' {..} =
    Core.object
      ( Core.catMaybes
          [ ("CatalogId" Core..=) Core.<$> catalogId,
            Core.Just ("DatabaseName" Core..= databaseName),
            Core.Just ("FunctionName" Core..= functionName)
          ]
      )

instance Core.ToPath GetUserDefinedFunction where
  toPath = Core.const "/"

instance Core.ToQuery GetUserDefinedFunction where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetUserDefinedFunctionResponse' smart constructor.
data GetUserDefinedFunctionResponse = GetUserDefinedFunctionResponse'
  { -- | The requested function definition.
    userDefinedFunction :: Core.Maybe UserDefinedFunction,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetUserDefinedFunctionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userDefinedFunction', 'getUserDefinedFunctionResponse_userDefinedFunction' - The requested function definition.
--
-- 'httpStatus', 'getUserDefinedFunctionResponse_httpStatus' - The response's http status code.
newGetUserDefinedFunctionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetUserDefinedFunctionResponse
newGetUserDefinedFunctionResponse pHttpStatus_ =
  GetUserDefinedFunctionResponse'
    { userDefinedFunction =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The requested function definition.
getUserDefinedFunctionResponse_userDefinedFunction :: Lens.Lens' GetUserDefinedFunctionResponse (Core.Maybe UserDefinedFunction)
getUserDefinedFunctionResponse_userDefinedFunction = Lens.lens (\GetUserDefinedFunctionResponse' {userDefinedFunction} -> userDefinedFunction) (\s@GetUserDefinedFunctionResponse' {} a -> s {userDefinedFunction = a} :: GetUserDefinedFunctionResponse)

-- | The response's http status code.
getUserDefinedFunctionResponse_httpStatus :: Lens.Lens' GetUserDefinedFunctionResponse Core.Int
getUserDefinedFunctionResponse_httpStatus = Lens.lens (\GetUserDefinedFunctionResponse' {httpStatus} -> httpStatus) (\s@GetUserDefinedFunctionResponse' {} a -> s {httpStatus = a} :: GetUserDefinedFunctionResponse)

instance Core.NFData GetUserDefinedFunctionResponse
