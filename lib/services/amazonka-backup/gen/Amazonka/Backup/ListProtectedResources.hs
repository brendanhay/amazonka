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
-- Module      : Amazonka.Backup.ListProtectedResources
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of resources successfully backed up by Backup,
-- including the time the resource was saved, an Amazon Resource Name (ARN)
-- of the resource, and a resource type.
--
-- This operation returns paginated results.
module Amazonka.Backup.ListProtectedResources
  ( -- * Creating a Request
    ListProtectedResources (..),
    newListProtectedResources,

    -- * Request Lenses
    listProtectedResources_maxResults,
    listProtectedResources_nextToken,

    -- * Destructuring the Response
    ListProtectedResourcesResponse (..),
    newListProtectedResourcesResponse,

    -- * Response Lenses
    listProtectedResourcesResponse_nextToken,
    listProtectedResourcesResponse_results,
    listProtectedResourcesResponse_httpStatus,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListProtectedResources' smart constructor.
data ListProtectedResources = ListProtectedResources'
  { -- | The maximum number of items to be returned.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The next item following a partial list of returned items. For example,
    -- if a request is made to return @maxResults@ number of items, @NextToken@
    -- allows you to return more items in your list starting at the location
    -- pointed to by the next token.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProtectedResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listProtectedResources_maxResults' - The maximum number of items to be returned.
--
-- 'nextToken', 'listProtectedResources_nextToken' - The next item following a partial list of returned items. For example,
-- if a request is made to return @maxResults@ number of items, @NextToken@
-- allows you to return more items in your list starting at the location
-- pointed to by the next token.
newListProtectedResources ::
  ListProtectedResources
newListProtectedResources =
  ListProtectedResources'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of items to be returned.
listProtectedResources_maxResults :: Lens.Lens' ListProtectedResources (Prelude.Maybe Prelude.Natural)
listProtectedResources_maxResults = Lens.lens (\ListProtectedResources' {maxResults} -> maxResults) (\s@ListProtectedResources' {} a -> s {maxResults = a} :: ListProtectedResources)

-- | The next item following a partial list of returned items. For example,
-- if a request is made to return @maxResults@ number of items, @NextToken@
-- allows you to return more items in your list starting at the location
-- pointed to by the next token.
listProtectedResources_nextToken :: Lens.Lens' ListProtectedResources (Prelude.Maybe Prelude.Text)
listProtectedResources_nextToken = Lens.lens (\ListProtectedResources' {nextToken} -> nextToken) (\s@ListProtectedResources' {} a -> s {nextToken = a} :: ListProtectedResources)

instance Core.AWSPager ListProtectedResources where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listProtectedResourcesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listProtectedResourcesResponse_results
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listProtectedResources_nextToken
          Lens..~ rs
          Lens.^? listProtectedResourcesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListProtectedResources where
  type
    AWSResponse ListProtectedResources =
      ListProtectedResourcesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProtectedResourcesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Results" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListProtectedResources where
  hashWithSalt _salt ListProtectedResources' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListProtectedResources where
  rnf ListProtectedResources' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListProtectedResources where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListProtectedResources where
  toPath = Prelude.const "/resources/"

instance Data.ToQuery ListProtectedResources where
  toQuery ListProtectedResources' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListProtectedResourcesResponse' smart constructor.
data ListProtectedResourcesResponse = ListProtectedResourcesResponse'
  { -- | The next item following a partial list of returned items. For example,
    -- if a request is made to return @maxResults@ number of items, @NextToken@
    -- allows you to return more items in your list starting at the location
    -- pointed to by the next token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of resources successfully backed up by Backup including the
    -- time the resource was saved, an Amazon Resource Name (ARN) of the
    -- resource, and a resource type.
    results :: Prelude.Maybe [ProtectedResource],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProtectedResourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listProtectedResourcesResponse_nextToken' - The next item following a partial list of returned items. For example,
-- if a request is made to return @maxResults@ number of items, @NextToken@
-- allows you to return more items in your list starting at the location
-- pointed to by the next token.
--
-- 'results', 'listProtectedResourcesResponse_results' - An array of resources successfully backed up by Backup including the
-- time the resource was saved, an Amazon Resource Name (ARN) of the
-- resource, and a resource type.
--
-- 'httpStatus', 'listProtectedResourcesResponse_httpStatus' - The response's http status code.
newListProtectedResourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListProtectedResourcesResponse
newListProtectedResourcesResponse pHttpStatus_ =
  ListProtectedResourcesResponse'
    { nextToken =
        Prelude.Nothing,
      results = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The next item following a partial list of returned items. For example,
-- if a request is made to return @maxResults@ number of items, @NextToken@
-- allows you to return more items in your list starting at the location
-- pointed to by the next token.
listProtectedResourcesResponse_nextToken :: Lens.Lens' ListProtectedResourcesResponse (Prelude.Maybe Prelude.Text)
listProtectedResourcesResponse_nextToken = Lens.lens (\ListProtectedResourcesResponse' {nextToken} -> nextToken) (\s@ListProtectedResourcesResponse' {} a -> s {nextToken = a} :: ListProtectedResourcesResponse)

-- | An array of resources successfully backed up by Backup including the
-- time the resource was saved, an Amazon Resource Name (ARN) of the
-- resource, and a resource type.
listProtectedResourcesResponse_results :: Lens.Lens' ListProtectedResourcesResponse (Prelude.Maybe [ProtectedResource])
listProtectedResourcesResponse_results = Lens.lens (\ListProtectedResourcesResponse' {results} -> results) (\s@ListProtectedResourcesResponse' {} a -> s {results = a} :: ListProtectedResourcesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listProtectedResourcesResponse_httpStatus :: Lens.Lens' ListProtectedResourcesResponse Prelude.Int
listProtectedResourcesResponse_httpStatus = Lens.lens (\ListProtectedResourcesResponse' {httpStatus} -> httpStatus) (\s@ListProtectedResourcesResponse' {} a -> s {httpStatus = a} :: ListProtectedResourcesResponse)

instance
  Prelude.NFData
    ListProtectedResourcesResponse
  where
  rnf ListProtectedResourcesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf results
      `Prelude.seq` Prelude.rnf httpStatus
