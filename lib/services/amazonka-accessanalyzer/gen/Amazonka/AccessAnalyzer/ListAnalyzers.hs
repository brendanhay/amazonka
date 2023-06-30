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
-- Module      : Amazonka.AccessAnalyzer.ListAnalyzers
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of analyzers.
--
-- This operation returns paginated results.
module Amazonka.AccessAnalyzer.ListAnalyzers
  ( -- * Creating a Request
    ListAnalyzers (..),
    newListAnalyzers,

    -- * Request Lenses
    listAnalyzers_maxResults,
    listAnalyzers_nextToken,
    listAnalyzers_type,

    -- * Destructuring the Response
    ListAnalyzersResponse (..),
    newListAnalyzersResponse,

    -- * Response Lenses
    listAnalyzersResponse_nextToken,
    listAnalyzersResponse_httpStatus,
    listAnalyzersResponse_analyzers,
  )
where

import Amazonka.AccessAnalyzer.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Retrieves a list of analyzers.
--
-- /See:/ 'newListAnalyzers' smart constructor.
data ListAnalyzers = ListAnalyzers'
  { -- | The maximum number of results to return in the response.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | A token used for pagination of results returned.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The type of analyzer.
    type' :: Prelude.Maybe Type
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAnalyzers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listAnalyzers_maxResults' - The maximum number of results to return in the response.
--
-- 'nextToken', 'listAnalyzers_nextToken' - A token used for pagination of results returned.
--
-- 'type'', 'listAnalyzers_type' - The type of analyzer.
newListAnalyzers ::
  ListAnalyzers
newListAnalyzers =
  ListAnalyzers'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The maximum number of results to return in the response.
listAnalyzers_maxResults :: Lens.Lens' ListAnalyzers (Prelude.Maybe Prelude.Int)
listAnalyzers_maxResults = Lens.lens (\ListAnalyzers' {maxResults} -> maxResults) (\s@ListAnalyzers' {} a -> s {maxResults = a} :: ListAnalyzers)

-- | A token used for pagination of results returned.
listAnalyzers_nextToken :: Lens.Lens' ListAnalyzers (Prelude.Maybe Prelude.Text)
listAnalyzers_nextToken = Lens.lens (\ListAnalyzers' {nextToken} -> nextToken) (\s@ListAnalyzers' {} a -> s {nextToken = a} :: ListAnalyzers)

-- | The type of analyzer.
listAnalyzers_type :: Lens.Lens' ListAnalyzers (Prelude.Maybe Type)
listAnalyzers_type = Lens.lens (\ListAnalyzers' {type'} -> type') (\s@ListAnalyzers' {} a -> s {type' = a} :: ListAnalyzers)

instance Core.AWSPager ListAnalyzers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAnalyzersResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        (rs Lens.^. listAnalyzersResponse_analyzers) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listAnalyzers_nextToken
          Lens..~ rs
          Lens.^? listAnalyzersResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListAnalyzers where
  type
    AWSResponse ListAnalyzers =
      ListAnalyzersResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAnalyzersResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "analyzers" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListAnalyzers where
  hashWithSalt _salt ListAnalyzers' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` type'

instance Prelude.NFData ListAnalyzers where
  rnf ListAnalyzers' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf type'

instance Data.ToHeaders ListAnalyzers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListAnalyzers where
  toPath = Prelude.const "/analyzer"

instance Data.ToQuery ListAnalyzers where
  toQuery ListAnalyzers' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "type" Data.=: type'
      ]

-- | The response to the request.
--
-- /See:/ 'newListAnalyzersResponse' smart constructor.
data ListAnalyzersResponse = ListAnalyzersResponse'
  { -- | A token used for pagination of results returned.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The analyzers retrieved.
    analyzers :: [AnalyzerSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAnalyzersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAnalyzersResponse_nextToken' - A token used for pagination of results returned.
--
-- 'httpStatus', 'listAnalyzersResponse_httpStatus' - The response's http status code.
--
-- 'analyzers', 'listAnalyzersResponse_analyzers' - The analyzers retrieved.
newListAnalyzersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAnalyzersResponse
newListAnalyzersResponse pHttpStatus_ =
  ListAnalyzersResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      analyzers = Prelude.mempty
    }

-- | A token used for pagination of results returned.
listAnalyzersResponse_nextToken :: Lens.Lens' ListAnalyzersResponse (Prelude.Maybe Prelude.Text)
listAnalyzersResponse_nextToken = Lens.lens (\ListAnalyzersResponse' {nextToken} -> nextToken) (\s@ListAnalyzersResponse' {} a -> s {nextToken = a} :: ListAnalyzersResponse)

-- | The response's http status code.
listAnalyzersResponse_httpStatus :: Lens.Lens' ListAnalyzersResponse Prelude.Int
listAnalyzersResponse_httpStatus = Lens.lens (\ListAnalyzersResponse' {httpStatus} -> httpStatus) (\s@ListAnalyzersResponse' {} a -> s {httpStatus = a} :: ListAnalyzersResponse)

-- | The analyzers retrieved.
listAnalyzersResponse_analyzers :: Lens.Lens' ListAnalyzersResponse [AnalyzerSummary]
listAnalyzersResponse_analyzers = Lens.lens (\ListAnalyzersResponse' {analyzers} -> analyzers) (\s@ListAnalyzersResponse' {} a -> s {analyzers = a} :: ListAnalyzersResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListAnalyzersResponse where
  rnf ListAnalyzersResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf analyzers
