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
-- Module      : Amazonka.AccessAnalyzer.ListAccessPreviews
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of access previews for the specified analyzer.
--
-- This operation returns paginated results.
module Amazonka.AccessAnalyzer.ListAccessPreviews
  ( -- * Creating a Request
    ListAccessPreviews (..),
    newListAccessPreviews,

    -- * Request Lenses
    listAccessPreviews_maxResults,
    listAccessPreviews_nextToken,
    listAccessPreviews_analyzerArn,

    -- * Destructuring the Response
    ListAccessPreviewsResponse (..),
    newListAccessPreviewsResponse,

    -- * Response Lenses
    listAccessPreviewsResponse_nextToken,
    listAccessPreviewsResponse_httpStatus,
    listAccessPreviewsResponse_accessPreviews,
  )
where

import Amazonka.AccessAnalyzer.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAccessPreviews' smart constructor.
data ListAccessPreviews = ListAccessPreviews'
  { -- | The maximum number of results to return in the response.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | A token used for pagination of results returned.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access-analyzer-getting-started.html#permission-resources ARN of the analyzer>
    -- used to generate the access preview.
    analyzerArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAccessPreviews' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listAccessPreviews_maxResults' - The maximum number of results to return in the response.
--
-- 'nextToken', 'listAccessPreviews_nextToken' - A token used for pagination of results returned.
--
-- 'analyzerArn', 'listAccessPreviews_analyzerArn' - The
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access-analyzer-getting-started.html#permission-resources ARN of the analyzer>
-- used to generate the access preview.
newListAccessPreviews ::
  -- | 'analyzerArn'
  Prelude.Text ->
  ListAccessPreviews
newListAccessPreviews pAnalyzerArn_ =
  ListAccessPreviews'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      analyzerArn = pAnalyzerArn_
    }

-- | The maximum number of results to return in the response.
listAccessPreviews_maxResults :: Lens.Lens' ListAccessPreviews (Prelude.Maybe Prelude.Int)
listAccessPreviews_maxResults = Lens.lens (\ListAccessPreviews' {maxResults} -> maxResults) (\s@ListAccessPreviews' {} a -> s {maxResults = a} :: ListAccessPreviews)

-- | A token used for pagination of results returned.
listAccessPreviews_nextToken :: Lens.Lens' ListAccessPreviews (Prelude.Maybe Prelude.Text)
listAccessPreviews_nextToken = Lens.lens (\ListAccessPreviews' {nextToken} -> nextToken) (\s@ListAccessPreviews' {} a -> s {nextToken = a} :: ListAccessPreviews)

-- | The
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access-analyzer-getting-started.html#permission-resources ARN of the analyzer>
-- used to generate the access preview.
listAccessPreviews_analyzerArn :: Lens.Lens' ListAccessPreviews Prelude.Text
listAccessPreviews_analyzerArn = Lens.lens (\ListAccessPreviews' {analyzerArn} -> analyzerArn) (\s@ListAccessPreviews' {} a -> s {analyzerArn = a} :: ListAccessPreviews)

instance Core.AWSPager ListAccessPreviews where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAccessPreviewsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listAccessPreviewsResponse_accessPreviews
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listAccessPreviews_nextToken
              Lens..~ rs
              Lens.^? listAccessPreviewsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListAccessPreviews where
  type
    AWSResponse ListAccessPreviews =
      ListAccessPreviewsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAccessPreviewsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "accessPreviews"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListAccessPreviews where
  hashWithSalt _salt ListAccessPreviews' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` analyzerArn

instance Prelude.NFData ListAccessPreviews where
  rnf ListAccessPreviews' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf analyzerArn

instance Data.ToHeaders ListAccessPreviews where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListAccessPreviews where
  toPath = Prelude.const "/access-preview"

instance Data.ToQuery ListAccessPreviews where
  toQuery ListAccessPreviews' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "analyzerArn" Data.=: analyzerArn
      ]

-- | /See:/ 'newListAccessPreviewsResponse' smart constructor.
data ListAccessPreviewsResponse = ListAccessPreviewsResponse'
  { -- | A token used for pagination of results returned.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of access previews retrieved for the analyzer.
    accessPreviews :: [AccessPreviewSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAccessPreviewsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAccessPreviewsResponse_nextToken' - A token used for pagination of results returned.
--
-- 'httpStatus', 'listAccessPreviewsResponse_httpStatus' - The response's http status code.
--
-- 'accessPreviews', 'listAccessPreviewsResponse_accessPreviews' - A list of access previews retrieved for the analyzer.
newListAccessPreviewsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAccessPreviewsResponse
newListAccessPreviewsResponse pHttpStatus_ =
  ListAccessPreviewsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      accessPreviews = Prelude.mempty
    }

-- | A token used for pagination of results returned.
listAccessPreviewsResponse_nextToken :: Lens.Lens' ListAccessPreviewsResponse (Prelude.Maybe Prelude.Text)
listAccessPreviewsResponse_nextToken = Lens.lens (\ListAccessPreviewsResponse' {nextToken} -> nextToken) (\s@ListAccessPreviewsResponse' {} a -> s {nextToken = a} :: ListAccessPreviewsResponse)

-- | The response's http status code.
listAccessPreviewsResponse_httpStatus :: Lens.Lens' ListAccessPreviewsResponse Prelude.Int
listAccessPreviewsResponse_httpStatus = Lens.lens (\ListAccessPreviewsResponse' {httpStatus} -> httpStatus) (\s@ListAccessPreviewsResponse' {} a -> s {httpStatus = a} :: ListAccessPreviewsResponse)

-- | A list of access previews retrieved for the analyzer.
listAccessPreviewsResponse_accessPreviews :: Lens.Lens' ListAccessPreviewsResponse [AccessPreviewSummary]
listAccessPreviewsResponse_accessPreviews = Lens.lens (\ListAccessPreviewsResponse' {accessPreviews} -> accessPreviews) (\s@ListAccessPreviewsResponse' {} a -> s {accessPreviews = a} :: ListAccessPreviewsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListAccessPreviewsResponse where
  rnf ListAccessPreviewsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf accessPreviews
