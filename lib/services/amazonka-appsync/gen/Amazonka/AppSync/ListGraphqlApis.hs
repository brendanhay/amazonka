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
-- Module      : Amazonka.AppSync.ListGraphqlApis
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists your GraphQL APIs.
--
-- This operation returns paginated results.
module Amazonka.AppSync.ListGraphqlApis
  ( -- * Creating a Request
    ListGraphqlApis (..),
    newListGraphqlApis,

    -- * Request Lenses
    listGraphqlApis_maxResults,
    listGraphqlApis_nextToken,

    -- * Destructuring the Response
    ListGraphqlApisResponse (..),
    newListGraphqlApisResponse,

    -- * Response Lenses
    listGraphqlApisResponse_graphqlApis,
    listGraphqlApisResponse_nextToken,
    listGraphqlApisResponse_httpStatus,
  )
where

import Amazonka.AppSync.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListGraphqlApis' smart constructor.
data ListGraphqlApis = ListGraphqlApis'
  { -- | The maximum number of results that you want the request to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | An identifier that was returned from the previous call to this
    -- operation, which you can use to return the next set of items in the
    -- list.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListGraphqlApis' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listGraphqlApis_maxResults' - The maximum number of results that you want the request to return.
--
-- 'nextToken', 'listGraphqlApis_nextToken' - An identifier that was returned from the previous call to this
-- operation, which you can use to return the next set of items in the
-- list.
newListGraphqlApis ::
  ListGraphqlApis
newListGraphqlApis =
  ListGraphqlApis'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results that you want the request to return.
listGraphqlApis_maxResults :: Lens.Lens' ListGraphqlApis (Prelude.Maybe Prelude.Natural)
listGraphqlApis_maxResults = Lens.lens (\ListGraphqlApis' {maxResults} -> maxResults) (\s@ListGraphqlApis' {} a -> s {maxResults = a} :: ListGraphqlApis)

-- | An identifier that was returned from the previous call to this
-- operation, which you can use to return the next set of items in the
-- list.
listGraphqlApis_nextToken :: Lens.Lens' ListGraphqlApis (Prelude.Maybe Prelude.Text)
listGraphqlApis_nextToken = Lens.lens (\ListGraphqlApis' {nextToken} -> nextToken) (\s@ListGraphqlApis' {} a -> s {nextToken = a} :: ListGraphqlApis)

instance Core.AWSPager ListGraphqlApis where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listGraphqlApisResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listGraphqlApisResponse_graphqlApis
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listGraphqlApis_nextToken
          Lens..~ rs
          Lens.^? listGraphqlApisResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListGraphqlApis where
  type
    AWSResponse ListGraphqlApis =
      ListGraphqlApisResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListGraphqlApisResponse'
            Prelude.<$> (x Data..?> "graphqlApis" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListGraphqlApis where
  hashWithSalt _salt ListGraphqlApis' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListGraphqlApis where
  rnf ListGraphqlApis' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListGraphqlApis where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListGraphqlApis where
  toPath = Prelude.const "/v1/apis"

instance Data.ToQuery ListGraphqlApis where
  toQuery ListGraphqlApis' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListGraphqlApisResponse' smart constructor.
data ListGraphqlApisResponse = ListGraphqlApisResponse'
  { -- | The @GraphqlApi@ objects.
    graphqlApis :: Prelude.Maybe [GraphqlApi],
    -- | An identifier to pass in the next request to this operation to return
    -- the next set of items in the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListGraphqlApisResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'graphqlApis', 'listGraphqlApisResponse_graphqlApis' - The @GraphqlApi@ objects.
--
-- 'nextToken', 'listGraphqlApisResponse_nextToken' - An identifier to pass in the next request to this operation to return
-- the next set of items in the list.
--
-- 'httpStatus', 'listGraphqlApisResponse_httpStatus' - The response's http status code.
newListGraphqlApisResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListGraphqlApisResponse
newListGraphqlApisResponse pHttpStatus_ =
  ListGraphqlApisResponse'
    { graphqlApis =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @GraphqlApi@ objects.
listGraphqlApisResponse_graphqlApis :: Lens.Lens' ListGraphqlApisResponse (Prelude.Maybe [GraphqlApi])
listGraphqlApisResponse_graphqlApis = Lens.lens (\ListGraphqlApisResponse' {graphqlApis} -> graphqlApis) (\s@ListGraphqlApisResponse' {} a -> s {graphqlApis = a} :: ListGraphqlApisResponse) Prelude.. Lens.mapping Lens.coerced

-- | An identifier to pass in the next request to this operation to return
-- the next set of items in the list.
listGraphqlApisResponse_nextToken :: Lens.Lens' ListGraphqlApisResponse (Prelude.Maybe Prelude.Text)
listGraphqlApisResponse_nextToken = Lens.lens (\ListGraphqlApisResponse' {nextToken} -> nextToken) (\s@ListGraphqlApisResponse' {} a -> s {nextToken = a} :: ListGraphqlApisResponse)

-- | The response's http status code.
listGraphqlApisResponse_httpStatus :: Lens.Lens' ListGraphqlApisResponse Prelude.Int
listGraphqlApisResponse_httpStatus = Lens.lens (\ListGraphqlApisResponse' {httpStatus} -> httpStatus) (\s@ListGraphqlApisResponse' {} a -> s {httpStatus = a} :: ListGraphqlApisResponse)

instance Prelude.NFData ListGraphqlApisResponse where
  rnf ListGraphqlApisResponse' {..} =
    Prelude.rnf graphqlApis
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
