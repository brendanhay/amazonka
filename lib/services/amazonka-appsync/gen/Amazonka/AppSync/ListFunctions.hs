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
-- Module      : Amazonka.AppSync.ListFunctions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List multiple functions.
--
-- This operation returns paginated results.
module Amazonka.AppSync.ListFunctions
  ( -- * Creating a Request
    ListFunctions (..),
    newListFunctions,

    -- * Request Lenses
    listFunctions_nextToken,
    listFunctions_maxResults,
    listFunctions_apiId,

    -- * Destructuring the Response
    ListFunctionsResponse (..),
    newListFunctionsResponse,

    -- * Response Lenses
    listFunctionsResponse_functions,
    listFunctionsResponse_nextToken,
    listFunctionsResponse_httpStatus,
  )
where

import Amazonka.AppSync.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListFunctions' smart constructor.
data ListFunctions = ListFunctions'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which you can use to return the next set of items in the
    -- list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results that you want the request to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The GraphQL API ID.
    apiId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFunctions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listFunctions_nextToken' - An identifier that was returned from the previous call to this
-- operation, which you can use to return the next set of items in the
-- list.
--
-- 'maxResults', 'listFunctions_maxResults' - The maximum number of results that you want the request to return.
--
-- 'apiId', 'listFunctions_apiId' - The GraphQL API ID.
newListFunctions ::
  -- | 'apiId'
  Prelude.Text ->
  ListFunctions
newListFunctions pApiId_ =
  ListFunctions'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      apiId = pApiId_
    }

-- | An identifier that was returned from the previous call to this
-- operation, which you can use to return the next set of items in the
-- list.
listFunctions_nextToken :: Lens.Lens' ListFunctions (Prelude.Maybe Prelude.Text)
listFunctions_nextToken = Lens.lens (\ListFunctions' {nextToken} -> nextToken) (\s@ListFunctions' {} a -> s {nextToken = a} :: ListFunctions)

-- | The maximum number of results that you want the request to return.
listFunctions_maxResults :: Lens.Lens' ListFunctions (Prelude.Maybe Prelude.Natural)
listFunctions_maxResults = Lens.lens (\ListFunctions' {maxResults} -> maxResults) (\s@ListFunctions' {} a -> s {maxResults = a} :: ListFunctions)

-- | The GraphQL API ID.
listFunctions_apiId :: Lens.Lens' ListFunctions Prelude.Text
listFunctions_apiId = Lens.lens (\ListFunctions' {apiId} -> apiId) (\s@ListFunctions' {} a -> s {apiId = a} :: ListFunctions)

instance Core.AWSPager ListFunctions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listFunctionsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listFunctionsResponse_functions Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listFunctions_nextToken
          Lens..~ rs
          Lens.^? listFunctionsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListFunctions where
  type
    AWSResponse ListFunctions =
      ListFunctionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFunctionsResponse'
            Prelude.<$> (x Data..?> "functions" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListFunctions where
  hashWithSalt _salt ListFunctions' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` apiId

instance Prelude.NFData ListFunctions where
  rnf ListFunctions' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf apiId

instance Data.ToHeaders ListFunctions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListFunctions where
  toPath ListFunctions' {..} =
    Prelude.mconcat
      ["/v1/apis/", Data.toBS apiId, "/functions"]

instance Data.ToQuery ListFunctions where
  toQuery ListFunctions' {..} =
    Prelude.mconcat
      [ "nextToken" Data.=: nextToken,
        "maxResults" Data.=: maxResults
      ]

-- | /See:/ 'newListFunctionsResponse' smart constructor.
data ListFunctionsResponse = ListFunctionsResponse'
  { -- | A list of @Function@ objects.
    functions :: Prelude.Maybe [FunctionConfiguration],
    -- | An identifier that was returned from the previous call to this
    -- operation, which you can use to return the next set of items in the
    -- list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFunctionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'functions', 'listFunctionsResponse_functions' - A list of @Function@ objects.
--
-- 'nextToken', 'listFunctionsResponse_nextToken' - An identifier that was returned from the previous call to this
-- operation, which you can use to return the next set of items in the
-- list.
--
-- 'httpStatus', 'listFunctionsResponse_httpStatus' - The response's http status code.
newListFunctionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListFunctionsResponse
newListFunctionsResponse pHttpStatus_ =
  ListFunctionsResponse'
    { functions = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of @Function@ objects.
listFunctionsResponse_functions :: Lens.Lens' ListFunctionsResponse (Prelude.Maybe [FunctionConfiguration])
listFunctionsResponse_functions = Lens.lens (\ListFunctionsResponse' {functions} -> functions) (\s@ListFunctionsResponse' {} a -> s {functions = a} :: ListFunctionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | An identifier that was returned from the previous call to this
-- operation, which you can use to return the next set of items in the
-- list.
listFunctionsResponse_nextToken :: Lens.Lens' ListFunctionsResponse (Prelude.Maybe Prelude.Text)
listFunctionsResponse_nextToken = Lens.lens (\ListFunctionsResponse' {nextToken} -> nextToken) (\s@ListFunctionsResponse' {} a -> s {nextToken = a} :: ListFunctionsResponse)

-- | The response's http status code.
listFunctionsResponse_httpStatus :: Lens.Lens' ListFunctionsResponse Prelude.Int
listFunctionsResponse_httpStatus = Lens.lens (\ListFunctionsResponse' {httpStatus} -> httpStatus) (\s@ListFunctionsResponse' {} a -> s {httpStatus = a} :: ListFunctionsResponse)

instance Prelude.NFData ListFunctionsResponse where
  rnf ListFunctionsResponse' {..} =
    Prelude.rnf functions
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
