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
-- Module      : Network.AWS.Glue.GetUserDefinedFunctions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves multiple function definitions from the Data Catalog.
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetUserDefinedFunctions
  ( -- * Creating a Request
    GetUserDefinedFunctions (..),
    newGetUserDefinedFunctions,

    -- * Request Lenses
    getUserDefinedFunctions_nextToken,
    getUserDefinedFunctions_catalogId,
    getUserDefinedFunctions_maxResults,
    getUserDefinedFunctions_databaseName,
    getUserDefinedFunctions_pattern,

    -- * Destructuring the Response
    GetUserDefinedFunctionsResponse (..),
    newGetUserDefinedFunctionsResponse,

    -- * Response Lenses
    getUserDefinedFunctionsResponse_nextToken,
    getUserDefinedFunctionsResponse_userDefinedFunctions,
    getUserDefinedFunctionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetUserDefinedFunctions' smart constructor.
data GetUserDefinedFunctions = GetUserDefinedFunctions'
  { -- | A continuation token, if this is a continuation call.
    nextToken :: Core.Maybe Core.Text,
    -- | The ID of the Data Catalog where the functions to be retrieved are
    -- located. If none is provided, the AWS account ID is used by default.
    catalogId :: Core.Maybe Core.Text,
    -- | The maximum number of functions to return in one response.
    maxResults :: Core.Maybe Core.Natural,
    -- | The name of the catalog database where the functions are located. If
    -- none is provided, functions from all the databases across the catalog
    -- will be returned.
    databaseName :: Core.Maybe Core.Text,
    -- | An optional function-name pattern string that filters the function
    -- definitions returned.
    pattern' :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetUserDefinedFunctions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getUserDefinedFunctions_nextToken' - A continuation token, if this is a continuation call.
--
-- 'catalogId', 'getUserDefinedFunctions_catalogId' - The ID of the Data Catalog where the functions to be retrieved are
-- located. If none is provided, the AWS account ID is used by default.
--
-- 'maxResults', 'getUserDefinedFunctions_maxResults' - The maximum number of functions to return in one response.
--
-- 'databaseName', 'getUserDefinedFunctions_databaseName' - The name of the catalog database where the functions are located. If
-- none is provided, functions from all the databases across the catalog
-- will be returned.
--
-- 'pattern'', 'getUserDefinedFunctions_pattern' - An optional function-name pattern string that filters the function
-- definitions returned.
newGetUserDefinedFunctions ::
  -- | 'pattern''
  Core.Text ->
  GetUserDefinedFunctions
newGetUserDefinedFunctions pPattern_ =
  GetUserDefinedFunctions'
    { nextToken = Core.Nothing,
      catalogId = Core.Nothing,
      maxResults = Core.Nothing,
      databaseName = Core.Nothing,
      pattern' = pPattern_
    }

-- | A continuation token, if this is a continuation call.
getUserDefinedFunctions_nextToken :: Lens.Lens' GetUserDefinedFunctions (Core.Maybe Core.Text)
getUserDefinedFunctions_nextToken = Lens.lens (\GetUserDefinedFunctions' {nextToken} -> nextToken) (\s@GetUserDefinedFunctions' {} a -> s {nextToken = a} :: GetUserDefinedFunctions)

-- | The ID of the Data Catalog where the functions to be retrieved are
-- located. If none is provided, the AWS account ID is used by default.
getUserDefinedFunctions_catalogId :: Lens.Lens' GetUserDefinedFunctions (Core.Maybe Core.Text)
getUserDefinedFunctions_catalogId = Lens.lens (\GetUserDefinedFunctions' {catalogId} -> catalogId) (\s@GetUserDefinedFunctions' {} a -> s {catalogId = a} :: GetUserDefinedFunctions)

-- | The maximum number of functions to return in one response.
getUserDefinedFunctions_maxResults :: Lens.Lens' GetUserDefinedFunctions (Core.Maybe Core.Natural)
getUserDefinedFunctions_maxResults = Lens.lens (\GetUserDefinedFunctions' {maxResults} -> maxResults) (\s@GetUserDefinedFunctions' {} a -> s {maxResults = a} :: GetUserDefinedFunctions)

-- | The name of the catalog database where the functions are located. If
-- none is provided, functions from all the databases across the catalog
-- will be returned.
getUserDefinedFunctions_databaseName :: Lens.Lens' GetUserDefinedFunctions (Core.Maybe Core.Text)
getUserDefinedFunctions_databaseName = Lens.lens (\GetUserDefinedFunctions' {databaseName} -> databaseName) (\s@GetUserDefinedFunctions' {} a -> s {databaseName = a} :: GetUserDefinedFunctions)

-- | An optional function-name pattern string that filters the function
-- definitions returned.
getUserDefinedFunctions_pattern :: Lens.Lens' GetUserDefinedFunctions Core.Text
getUserDefinedFunctions_pattern = Lens.lens (\GetUserDefinedFunctions' {pattern'} -> pattern') (\s@GetUserDefinedFunctions' {} a -> s {pattern' = a} :: GetUserDefinedFunctions)

instance Core.AWSPager GetUserDefinedFunctions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getUserDefinedFunctionsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getUserDefinedFunctionsResponse_userDefinedFunctions
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getUserDefinedFunctions_nextToken
          Lens..~ rs
          Lens.^? getUserDefinedFunctionsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest GetUserDefinedFunctions where
  type
    AWSResponse GetUserDefinedFunctions =
      GetUserDefinedFunctionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetUserDefinedFunctionsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "UserDefinedFunctions"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetUserDefinedFunctions

instance Core.NFData GetUserDefinedFunctions

instance Core.ToHeaders GetUserDefinedFunctions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSGlue.GetUserDefinedFunctions" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetUserDefinedFunctions where
  toJSON GetUserDefinedFunctions' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("CatalogId" Core..=) Core.<$> catalogId,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("DatabaseName" Core..=) Core.<$> databaseName,
            Core.Just ("Pattern" Core..= pattern')
          ]
      )

instance Core.ToPath GetUserDefinedFunctions where
  toPath = Core.const "/"

instance Core.ToQuery GetUserDefinedFunctions where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetUserDefinedFunctionsResponse' smart constructor.
data GetUserDefinedFunctionsResponse = GetUserDefinedFunctionsResponse'
  { -- | A continuation token, if the list of functions returned does not include
    -- the last requested function.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of requested function definitions.
    userDefinedFunctions :: Core.Maybe [UserDefinedFunction],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetUserDefinedFunctionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getUserDefinedFunctionsResponse_nextToken' - A continuation token, if the list of functions returned does not include
-- the last requested function.
--
-- 'userDefinedFunctions', 'getUserDefinedFunctionsResponse_userDefinedFunctions' - A list of requested function definitions.
--
-- 'httpStatus', 'getUserDefinedFunctionsResponse_httpStatus' - The response's http status code.
newGetUserDefinedFunctionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetUserDefinedFunctionsResponse
newGetUserDefinedFunctionsResponse pHttpStatus_ =
  GetUserDefinedFunctionsResponse'
    { nextToken =
        Core.Nothing,
      userDefinedFunctions = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A continuation token, if the list of functions returned does not include
-- the last requested function.
getUserDefinedFunctionsResponse_nextToken :: Lens.Lens' GetUserDefinedFunctionsResponse (Core.Maybe Core.Text)
getUserDefinedFunctionsResponse_nextToken = Lens.lens (\GetUserDefinedFunctionsResponse' {nextToken} -> nextToken) (\s@GetUserDefinedFunctionsResponse' {} a -> s {nextToken = a} :: GetUserDefinedFunctionsResponse)

-- | A list of requested function definitions.
getUserDefinedFunctionsResponse_userDefinedFunctions :: Lens.Lens' GetUserDefinedFunctionsResponse (Core.Maybe [UserDefinedFunction])
getUserDefinedFunctionsResponse_userDefinedFunctions = Lens.lens (\GetUserDefinedFunctionsResponse' {userDefinedFunctions} -> userDefinedFunctions) (\s@GetUserDefinedFunctionsResponse' {} a -> s {userDefinedFunctions = a} :: GetUserDefinedFunctionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getUserDefinedFunctionsResponse_httpStatus :: Lens.Lens' GetUserDefinedFunctionsResponse Core.Int
getUserDefinedFunctionsResponse_httpStatus = Lens.lens (\GetUserDefinedFunctionsResponse' {httpStatus} -> httpStatus) (\s@GetUserDefinedFunctionsResponse' {} a -> s {httpStatus = a} :: GetUserDefinedFunctionsResponse)

instance Core.NFData GetUserDefinedFunctionsResponse
