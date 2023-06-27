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
-- Module      : Amazonka.Lightsail.GetRelationalDatabaseParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all of the runtime parameters offered by the underlying database
-- software, or engine, for a specific database in Amazon Lightsail.
--
-- In addition to the parameter names and values, this operation returns
-- other information about each parameter. This information includes
-- whether changes require a reboot, whether the parameter is modifiable,
-- the allowed values, and the data types.
--
-- This operation returns paginated results.
module Amazonka.Lightsail.GetRelationalDatabaseParameters
  ( -- * Creating a Request
    GetRelationalDatabaseParameters (..),
    newGetRelationalDatabaseParameters,

    -- * Request Lenses
    getRelationalDatabaseParameters_pageToken,
    getRelationalDatabaseParameters_relationalDatabaseName,

    -- * Destructuring the Response
    GetRelationalDatabaseParametersResponse (..),
    newGetRelationalDatabaseParametersResponse,

    -- * Response Lenses
    getRelationalDatabaseParametersResponse_nextPageToken,
    getRelationalDatabaseParametersResponse_parameters,
    getRelationalDatabaseParametersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetRelationalDatabaseParameters' smart constructor.
data GetRelationalDatabaseParameters = GetRelationalDatabaseParameters'
  { -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial
    -- @GetRelationalDatabaseParameters@ request. If your results are
    -- paginated, the response will return a next page token that you can
    -- specify as the page token in a subsequent request.
    pageToken :: Prelude.Maybe Prelude.Text,
    -- | The name of your database for which to get parameters.
    relationalDatabaseName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRelationalDatabaseParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageToken', 'getRelationalDatabaseParameters_pageToken' - The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial
-- @GetRelationalDatabaseParameters@ request. If your results are
-- paginated, the response will return a next page token that you can
-- specify as the page token in a subsequent request.
--
-- 'relationalDatabaseName', 'getRelationalDatabaseParameters_relationalDatabaseName' - The name of your database for which to get parameters.
newGetRelationalDatabaseParameters ::
  -- | 'relationalDatabaseName'
  Prelude.Text ->
  GetRelationalDatabaseParameters
newGetRelationalDatabaseParameters
  pRelationalDatabaseName_ =
    GetRelationalDatabaseParameters'
      { pageToken =
          Prelude.Nothing,
        relationalDatabaseName =
          pRelationalDatabaseName_
      }

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial
-- @GetRelationalDatabaseParameters@ request. If your results are
-- paginated, the response will return a next page token that you can
-- specify as the page token in a subsequent request.
getRelationalDatabaseParameters_pageToken :: Lens.Lens' GetRelationalDatabaseParameters (Prelude.Maybe Prelude.Text)
getRelationalDatabaseParameters_pageToken = Lens.lens (\GetRelationalDatabaseParameters' {pageToken} -> pageToken) (\s@GetRelationalDatabaseParameters' {} a -> s {pageToken = a} :: GetRelationalDatabaseParameters)

-- | The name of your database for which to get parameters.
getRelationalDatabaseParameters_relationalDatabaseName :: Lens.Lens' GetRelationalDatabaseParameters Prelude.Text
getRelationalDatabaseParameters_relationalDatabaseName = Lens.lens (\GetRelationalDatabaseParameters' {relationalDatabaseName} -> relationalDatabaseName) (\s@GetRelationalDatabaseParameters' {} a -> s {relationalDatabaseName = a} :: GetRelationalDatabaseParameters)

instance
  Core.AWSPager
    GetRelationalDatabaseParameters
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getRelationalDatabaseParametersResponse_nextPageToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getRelationalDatabaseParametersResponse_parameters
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& getRelationalDatabaseParameters_pageToken
          Lens..~ rs
          Lens.^? getRelationalDatabaseParametersResponse_nextPageToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    GetRelationalDatabaseParameters
  where
  type
    AWSResponse GetRelationalDatabaseParameters =
      GetRelationalDatabaseParametersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRelationalDatabaseParametersResponse'
            Prelude.<$> (x Data..?> "nextPageToken")
            Prelude.<*> (x Data..?> "parameters" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetRelationalDatabaseParameters
  where
  hashWithSalt
    _salt
    GetRelationalDatabaseParameters' {..} =
      _salt
        `Prelude.hashWithSalt` pageToken
        `Prelude.hashWithSalt` relationalDatabaseName

instance
  Prelude.NFData
    GetRelationalDatabaseParameters
  where
  rnf GetRelationalDatabaseParameters' {..} =
    Prelude.rnf pageToken
      `Prelude.seq` Prelude.rnf relationalDatabaseName

instance
  Data.ToHeaders
    GetRelationalDatabaseParameters
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.GetRelationalDatabaseParameters" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetRelationalDatabaseParameters where
  toJSON GetRelationalDatabaseParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("pageToken" Data..=) Prelude.<$> pageToken,
            Prelude.Just
              ( "relationalDatabaseName"
                  Data..= relationalDatabaseName
              )
          ]
      )

instance Data.ToPath GetRelationalDatabaseParameters where
  toPath = Prelude.const "/"

instance Data.ToQuery GetRelationalDatabaseParameters where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRelationalDatabaseParametersResponse' smart constructor.
data GetRelationalDatabaseParametersResponse = GetRelationalDatabaseParametersResponse'
  { -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to
    -- display.
    --
    -- To get the next page of results, perform another
    -- @GetRelationalDatabaseParameters@ request and specify the next page
    -- token using the @pageToken@ parameter.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | An object describing the result of your get relational database
    -- parameters request.
    parameters :: Prelude.Maybe [RelationalDatabaseParameter],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRelationalDatabaseParametersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageToken', 'getRelationalDatabaseParametersResponse_nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another
-- @GetRelationalDatabaseParameters@ request and specify the next page
-- token using the @pageToken@ parameter.
--
-- 'parameters', 'getRelationalDatabaseParametersResponse_parameters' - An object describing the result of your get relational database
-- parameters request.
--
-- 'httpStatus', 'getRelationalDatabaseParametersResponse_httpStatus' - The response's http status code.
newGetRelationalDatabaseParametersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRelationalDatabaseParametersResponse
newGetRelationalDatabaseParametersResponse
  pHttpStatus_ =
    GetRelationalDatabaseParametersResponse'
      { nextPageToken =
          Prelude.Nothing,
        parameters = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another
-- @GetRelationalDatabaseParameters@ request and specify the next page
-- token using the @pageToken@ parameter.
getRelationalDatabaseParametersResponse_nextPageToken :: Lens.Lens' GetRelationalDatabaseParametersResponse (Prelude.Maybe Prelude.Text)
getRelationalDatabaseParametersResponse_nextPageToken = Lens.lens (\GetRelationalDatabaseParametersResponse' {nextPageToken} -> nextPageToken) (\s@GetRelationalDatabaseParametersResponse' {} a -> s {nextPageToken = a} :: GetRelationalDatabaseParametersResponse)

-- | An object describing the result of your get relational database
-- parameters request.
getRelationalDatabaseParametersResponse_parameters :: Lens.Lens' GetRelationalDatabaseParametersResponse (Prelude.Maybe [RelationalDatabaseParameter])
getRelationalDatabaseParametersResponse_parameters = Lens.lens (\GetRelationalDatabaseParametersResponse' {parameters} -> parameters) (\s@GetRelationalDatabaseParametersResponse' {} a -> s {parameters = a} :: GetRelationalDatabaseParametersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getRelationalDatabaseParametersResponse_httpStatus :: Lens.Lens' GetRelationalDatabaseParametersResponse Prelude.Int
getRelationalDatabaseParametersResponse_httpStatus = Lens.lens (\GetRelationalDatabaseParametersResponse' {httpStatus} -> httpStatus) (\s@GetRelationalDatabaseParametersResponse' {} a -> s {httpStatus = a} :: GetRelationalDatabaseParametersResponse)

instance
  Prelude.NFData
    GetRelationalDatabaseParametersResponse
  where
  rnf GetRelationalDatabaseParametersResponse' {..} =
    Prelude.rnf nextPageToken
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf httpStatus
