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
-- Module      : Amazonka.HoneyCode.GetScreenData
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The GetScreenData API allows retrieval of data from a screen in a
-- Honeycode app. The API allows setting local variables in the screen to
-- filter, sort or otherwise affect what will be displayed on the screen.
module Amazonka.HoneyCode.GetScreenData
  ( -- * Creating a Request
    GetScreenData (..),
    newGetScreenData,

    -- * Request Lenses
    getScreenData_nextToken,
    getScreenData_maxResults,
    getScreenData_variables,
    getScreenData_workbookId,
    getScreenData_appId,
    getScreenData_screenId,

    -- * Destructuring the Response
    GetScreenDataResponse (..),
    newGetScreenDataResponse,

    -- * Response Lenses
    getScreenDataResponse_nextToken,
    getScreenDataResponse_httpStatus,
    getScreenDataResponse_results,
    getScreenDataResponse_workbookCursor,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.HoneyCode.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetScreenData' smart constructor.
data GetScreenData = GetScreenData'
  { -- | This parameter is optional. If a nextToken is not specified, the API
    -- returns the first page of data.
    --
    -- Pagination tokens expire after 1 hour. If you use a token that was
    -- returned more than an hour back, the API will throw ValidationException.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The number of results to be returned on a single page. Specify a number
    -- between 1 and 100. The maximum value is 100.
    --
    -- This parameter is optional. If you don\'t specify this parameter, the
    -- default page size is 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Variables are optional and are needed only if the screen requires them
    -- to render correctly. Variables are specified as a map where the key is
    -- the name of the variable as defined on the screen. The value is an
    -- object which currently has only one property, rawValue, which holds the
    -- value of the variable to be passed to the screen.
    variables :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text (Data.Sensitive VariableValue))),
    -- | The ID of the workbook that contains the screen.
    workbookId :: Prelude.Text,
    -- | The ID of the app that contains the screen.
    appId :: Prelude.Text,
    -- | The ID of the screen.
    screenId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetScreenData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getScreenData_nextToken' - This parameter is optional. If a nextToken is not specified, the API
-- returns the first page of data.
--
-- Pagination tokens expire after 1 hour. If you use a token that was
-- returned more than an hour back, the API will throw ValidationException.
--
-- 'maxResults', 'getScreenData_maxResults' - The number of results to be returned on a single page. Specify a number
-- between 1 and 100. The maximum value is 100.
--
-- This parameter is optional. If you don\'t specify this parameter, the
-- default page size is 100.
--
-- 'variables', 'getScreenData_variables' - Variables are optional and are needed only if the screen requires them
-- to render correctly. Variables are specified as a map where the key is
-- the name of the variable as defined on the screen. The value is an
-- object which currently has only one property, rawValue, which holds the
-- value of the variable to be passed to the screen.
--
-- 'workbookId', 'getScreenData_workbookId' - The ID of the workbook that contains the screen.
--
-- 'appId', 'getScreenData_appId' - The ID of the app that contains the screen.
--
-- 'screenId', 'getScreenData_screenId' - The ID of the screen.
newGetScreenData ::
  -- | 'workbookId'
  Prelude.Text ->
  -- | 'appId'
  Prelude.Text ->
  -- | 'screenId'
  Prelude.Text ->
  GetScreenData
newGetScreenData pWorkbookId_ pAppId_ pScreenId_ =
  GetScreenData'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      variables = Prelude.Nothing,
      workbookId = pWorkbookId_,
      appId = pAppId_,
      screenId = pScreenId_
    }

-- | This parameter is optional. If a nextToken is not specified, the API
-- returns the first page of data.
--
-- Pagination tokens expire after 1 hour. If you use a token that was
-- returned more than an hour back, the API will throw ValidationException.
getScreenData_nextToken :: Lens.Lens' GetScreenData (Prelude.Maybe Prelude.Text)
getScreenData_nextToken = Lens.lens (\GetScreenData' {nextToken} -> nextToken) (\s@GetScreenData' {} a -> s {nextToken = a} :: GetScreenData)

-- | The number of results to be returned on a single page. Specify a number
-- between 1 and 100. The maximum value is 100.
--
-- This parameter is optional. If you don\'t specify this parameter, the
-- default page size is 100.
getScreenData_maxResults :: Lens.Lens' GetScreenData (Prelude.Maybe Prelude.Natural)
getScreenData_maxResults = Lens.lens (\GetScreenData' {maxResults} -> maxResults) (\s@GetScreenData' {} a -> s {maxResults = a} :: GetScreenData)

-- | Variables are optional and are needed only if the screen requires them
-- to render correctly. Variables are specified as a map where the key is
-- the name of the variable as defined on the screen. The value is an
-- object which currently has only one property, rawValue, which holds the
-- value of the variable to be passed to the screen.
getScreenData_variables :: Lens.Lens' GetScreenData (Prelude.Maybe (Prelude.HashMap Prelude.Text VariableValue))
getScreenData_variables = Lens.lens (\GetScreenData' {variables} -> variables) (\s@GetScreenData' {} a -> s {variables = a} :: GetScreenData) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The ID of the workbook that contains the screen.
getScreenData_workbookId :: Lens.Lens' GetScreenData Prelude.Text
getScreenData_workbookId = Lens.lens (\GetScreenData' {workbookId} -> workbookId) (\s@GetScreenData' {} a -> s {workbookId = a} :: GetScreenData)

-- | The ID of the app that contains the screen.
getScreenData_appId :: Lens.Lens' GetScreenData Prelude.Text
getScreenData_appId = Lens.lens (\GetScreenData' {appId} -> appId) (\s@GetScreenData' {} a -> s {appId = a} :: GetScreenData)

-- | The ID of the screen.
getScreenData_screenId :: Lens.Lens' GetScreenData Prelude.Text
getScreenData_screenId = Lens.lens (\GetScreenData' {screenId} -> screenId) (\s@GetScreenData' {} a -> s {screenId = a} :: GetScreenData)

instance Core.AWSRequest GetScreenData where
  type
    AWSResponse GetScreenData =
      GetScreenDataResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetScreenDataResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "results" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..:> "workbookCursor")
      )

instance Prelude.Hashable GetScreenData where
  hashWithSalt _salt GetScreenData' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` variables
      `Prelude.hashWithSalt` workbookId
      `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` screenId

instance Prelude.NFData GetScreenData where
  rnf GetScreenData' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf variables
      `Prelude.seq` Prelude.rnf workbookId
      `Prelude.seq` Prelude.rnf appId
      `Prelude.seq` Prelude.rnf screenId

instance Data.ToHeaders GetScreenData where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetScreenData where
  toJSON GetScreenData' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("nextToken" Data..=) Prelude.<$> nextToken,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("variables" Data..=) Prelude.<$> variables,
            Prelude.Just ("workbookId" Data..= workbookId),
            Prelude.Just ("appId" Data..= appId),
            Prelude.Just ("screenId" Data..= screenId)
          ]
      )

instance Data.ToPath GetScreenData where
  toPath = Prelude.const "/screendata"

instance Data.ToQuery GetScreenData where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetScreenDataResponse' smart constructor.
data GetScreenDataResponse = GetScreenDataResponse'
  { -- | Provides the pagination token to load the next page if there are more
    -- results matching the request. If a pagination token is not present in
    -- the response, it means that all data matching the query has been loaded.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A map of all the rows on the screen keyed by block name.
    results :: Prelude.HashMap Prelude.Text ResultSet,
    -- | Indicates the cursor of the workbook at which the data returned by this
    -- workbook is read. Workbook cursor keeps increasing with every update and
    -- the increments are not sequential.
    workbookCursor :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetScreenDataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getScreenDataResponse_nextToken' - Provides the pagination token to load the next page if there are more
-- results matching the request. If a pagination token is not present in
-- the response, it means that all data matching the query has been loaded.
--
-- 'httpStatus', 'getScreenDataResponse_httpStatus' - The response's http status code.
--
-- 'results', 'getScreenDataResponse_results' - A map of all the rows on the screen keyed by block name.
--
-- 'workbookCursor', 'getScreenDataResponse_workbookCursor' - Indicates the cursor of the workbook at which the data returned by this
-- workbook is read. Workbook cursor keeps increasing with every update and
-- the increments are not sequential.
newGetScreenDataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'workbookCursor'
  Prelude.Integer ->
  GetScreenDataResponse
newGetScreenDataResponse
  pHttpStatus_
  pWorkbookCursor_ =
    GetScreenDataResponse'
      { nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        results = Prelude.mempty,
        workbookCursor = pWorkbookCursor_
      }

-- | Provides the pagination token to load the next page if there are more
-- results matching the request. If a pagination token is not present in
-- the response, it means that all data matching the query has been loaded.
getScreenDataResponse_nextToken :: Lens.Lens' GetScreenDataResponse (Prelude.Maybe Prelude.Text)
getScreenDataResponse_nextToken = Lens.lens (\GetScreenDataResponse' {nextToken} -> nextToken) (\s@GetScreenDataResponse' {} a -> s {nextToken = a} :: GetScreenDataResponse)

-- | The response's http status code.
getScreenDataResponse_httpStatus :: Lens.Lens' GetScreenDataResponse Prelude.Int
getScreenDataResponse_httpStatus = Lens.lens (\GetScreenDataResponse' {httpStatus} -> httpStatus) (\s@GetScreenDataResponse' {} a -> s {httpStatus = a} :: GetScreenDataResponse)

-- | A map of all the rows on the screen keyed by block name.
getScreenDataResponse_results :: Lens.Lens' GetScreenDataResponse (Prelude.HashMap Prelude.Text ResultSet)
getScreenDataResponse_results = Lens.lens (\GetScreenDataResponse' {results} -> results) (\s@GetScreenDataResponse' {} a -> s {results = a} :: GetScreenDataResponse) Prelude.. Lens.coerced

-- | Indicates the cursor of the workbook at which the data returned by this
-- workbook is read. Workbook cursor keeps increasing with every update and
-- the increments are not sequential.
getScreenDataResponse_workbookCursor :: Lens.Lens' GetScreenDataResponse Prelude.Integer
getScreenDataResponse_workbookCursor = Lens.lens (\GetScreenDataResponse' {workbookCursor} -> workbookCursor) (\s@GetScreenDataResponse' {} a -> s {workbookCursor = a} :: GetScreenDataResponse)

instance Prelude.NFData GetScreenDataResponse where
  rnf GetScreenDataResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf results
      `Prelude.seq` Prelude.rnf workbookCursor
