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
-- Module      : Amazonka.Personalize.ListSolutionVersions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of solution versions for the given solution. When a
-- solution is not specified, all the solution versions associated with the
-- account are listed. The response provides the properties for each
-- solution version, including the Amazon Resource Name (ARN).
--
-- This operation returns paginated results.
module Amazonka.Personalize.ListSolutionVersions
  ( -- * Creating a Request
    ListSolutionVersions (..),
    newListSolutionVersions,

    -- * Request Lenses
    listSolutionVersions_maxResults,
    listSolutionVersions_nextToken,
    listSolutionVersions_solutionArn,

    -- * Destructuring the Response
    ListSolutionVersionsResponse (..),
    newListSolutionVersionsResponse,

    -- * Response Lenses
    listSolutionVersionsResponse_nextToken,
    listSolutionVersionsResponse_solutionVersions,
    listSolutionVersionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListSolutionVersions' smart constructor.
data ListSolutionVersions = ListSolutionVersions'
  { -- | The maximum number of solution versions to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token returned from the previous call to @ListSolutionVersions@ for
    -- getting the next set of solution versions (if they exist).
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the solution.
    solutionArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSolutionVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listSolutionVersions_maxResults' - The maximum number of solution versions to return.
--
-- 'nextToken', 'listSolutionVersions_nextToken' - A token returned from the previous call to @ListSolutionVersions@ for
-- getting the next set of solution versions (if they exist).
--
-- 'solutionArn', 'listSolutionVersions_solutionArn' - The Amazon Resource Name (ARN) of the solution.
newListSolutionVersions ::
  ListSolutionVersions
newListSolutionVersions =
  ListSolutionVersions'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      solutionArn = Prelude.Nothing
    }

-- | The maximum number of solution versions to return.
listSolutionVersions_maxResults :: Lens.Lens' ListSolutionVersions (Prelude.Maybe Prelude.Natural)
listSolutionVersions_maxResults = Lens.lens (\ListSolutionVersions' {maxResults} -> maxResults) (\s@ListSolutionVersions' {} a -> s {maxResults = a} :: ListSolutionVersions)

-- | A token returned from the previous call to @ListSolutionVersions@ for
-- getting the next set of solution versions (if they exist).
listSolutionVersions_nextToken :: Lens.Lens' ListSolutionVersions (Prelude.Maybe Prelude.Text)
listSolutionVersions_nextToken = Lens.lens (\ListSolutionVersions' {nextToken} -> nextToken) (\s@ListSolutionVersions' {} a -> s {nextToken = a} :: ListSolutionVersions)

-- | The Amazon Resource Name (ARN) of the solution.
listSolutionVersions_solutionArn :: Lens.Lens' ListSolutionVersions (Prelude.Maybe Prelude.Text)
listSolutionVersions_solutionArn = Lens.lens (\ListSolutionVersions' {solutionArn} -> solutionArn) (\s@ListSolutionVersions' {} a -> s {solutionArn = a} :: ListSolutionVersions)

instance Core.AWSPager ListSolutionVersions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSolutionVersionsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listSolutionVersionsResponse_solutionVersions
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listSolutionVersions_nextToken
          Lens..~ rs
          Lens.^? listSolutionVersionsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListSolutionVersions where
  type
    AWSResponse ListSolutionVersions =
      ListSolutionVersionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSolutionVersionsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> ( x
                            Data..?> "solutionVersions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSolutionVersions where
  hashWithSalt _salt ListSolutionVersions' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` solutionArn

instance Prelude.NFData ListSolutionVersions where
  rnf ListSolutionVersions' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf solutionArn

instance Data.ToHeaders ListSolutionVersions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonPersonalize.ListSolutionVersions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListSolutionVersions where
  toJSON ListSolutionVersions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("solutionArn" Data..=) Prelude.<$> solutionArn
          ]
      )

instance Data.ToPath ListSolutionVersions where
  toPath = Prelude.const "/"

instance Data.ToQuery ListSolutionVersions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListSolutionVersionsResponse' smart constructor.
data ListSolutionVersionsResponse = ListSolutionVersionsResponse'
  { -- | A token for getting the next set of solution versions (if they exist).
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of solution versions describing the version properties.
    solutionVersions :: Prelude.Maybe [SolutionVersionSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSolutionVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSolutionVersionsResponse_nextToken' - A token for getting the next set of solution versions (if they exist).
--
-- 'solutionVersions', 'listSolutionVersionsResponse_solutionVersions' - A list of solution versions describing the version properties.
--
-- 'httpStatus', 'listSolutionVersionsResponse_httpStatus' - The response's http status code.
newListSolutionVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSolutionVersionsResponse
newListSolutionVersionsResponse pHttpStatus_ =
  ListSolutionVersionsResponse'
    { nextToken =
        Prelude.Nothing,
      solutionVersions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token for getting the next set of solution versions (if they exist).
listSolutionVersionsResponse_nextToken :: Lens.Lens' ListSolutionVersionsResponse (Prelude.Maybe Prelude.Text)
listSolutionVersionsResponse_nextToken = Lens.lens (\ListSolutionVersionsResponse' {nextToken} -> nextToken) (\s@ListSolutionVersionsResponse' {} a -> s {nextToken = a} :: ListSolutionVersionsResponse)

-- | A list of solution versions describing the version properties.
listSolutionVersionsResponse_solutionVersions :: Lens.Lens' ListSolutionVersionsResponse (Prelude.Maybe [SolutionVersionSummary])
listSolutionVersionsResponse_solutionVersions = Lens.lens (\ListSolutionVersionsResponse' {solutionVersions} -> solutionVersions) (\s@ListSolutionVersionsResponse' {} a -> s {solutionVersions = a} :: ListSolutionVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSolutionVersionsResponse_httpStatus :: Lens.Lens' ListSolutionVersionsResponse Prelude.Int
listSolutionVersionsResponse_httpStatus = Lens.lens (\ListSolutionVersionsResponse' {httpStatus} -> httpStatus) (\s@ListSolutionVersionsResponse' {} a -> s {httpStatus = a} :: ListSolutionVersionsResponse)

instance Prelude.NFData ListSolutionVersionsResponse where
  rnf ListSolutionVersionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf solutionVersions
      `Prelude.seq` Prelude.rnf httpStatus
