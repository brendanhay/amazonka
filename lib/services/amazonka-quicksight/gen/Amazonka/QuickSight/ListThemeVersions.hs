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
-- Module      : Amazonka.QuickSight.ListThemeVersions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the versions of the themes in the current Amazon Web Services
-- account.
--
-- This operation returns paginated results.
module Amazonka.QuickSight.ListThemeVersions
  ( -- * Creating a Request
    ListThemeVersions (..),
    newListThemeVersions,

    -- * Request Lenses
    listThemeVersions_maxResults,
    listThemeVersions_nextToken,
    listThemeVersions_awsAccountId,
    listThemeVersions_themeId,

    -- * Destructuring the Response
    ListThemeVersionsResponse (..),
    newListThemeVersionsResponse,

    -- * Response Lenses
    listThemeVersionsResponse_nextToken,
    listThemeVersionsResponse_requestId,
    listThemeVersionsResponse_themeVersionSummaryList,
    listThemeVersionsResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListThemeVersions' smart constructor.
data ListThemeVersions = ListThemeVersions'
  { -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account that contains the themes that
    -- you\'re listing.
    awsAccountId :: Prelude.Text,
    -- | The ID for the theme.
    themeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListThemeVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listThemeVersions_maxResults' - The maximum number of results to be returned per request.
--
-- 'nextToken', 'listThemeVersions_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'awsAccountId', 'listThemeVersions_awsAccountId' - The ID of the Amazon Web Services account that contains the themes that
-- you\'re listing.
--
-- 'themeId', 'listThemeVersions_themeId' - The ID for the theme.
newListThemeVersions ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'themeId'
  Prelude.Text ->
  ListThemeVersions
newListThemeVersions pAwsAccountId_ pThemeId_ =
  ListThemeVersions'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      awsAccountId = pAwsAccountId_,
      themeId = pThemeId_
    }

-- | The maximum number of results to be returned per request.
listThemeVersions_maxResults :: Lens.Lens' ListThemeVersions (Prelude.Maybe Prelude.Natural)
listThemeVersions_maxResults = Lens.lens (\ListThemeVersions' {maxResults} -> maxResults) (\s@ListThemeVersions' {} a -> s {maxResults = a} :: ListThemeVersions)

-- | The token for the next set of results, or null if there are no more
-- results.
listThemeVersions_nextToken :: Lens.Lens' ListThemeVersions (Prelude.Maybe Prelude.Text)
listThemeVersions_nextToken = Lens.lens (\ListThemeVersions' {nextToken} -> nextToken) (\s@ListThemeVersions' {} a -> s {nextToken = a} :: ListThemeVersions)

-- | The ID of the Amazon Web Services account that contains the themes that
-- you\'re listing.
listThemeVersions_awsAccountId :: Lens.Lens' ListThemeVersions Prelude.Text
listThemeVersions_awsAccountId = Lens.lens (\ListThemeVersions' {awsAccountId} -> awsAccountId) (\s@ListThemeVersions' {} a -> s {awsAccountId = a} :: ListThemeVersions)

-- | The ID for the theme.
listThemeVersions_themeId :: Lens.Lens' ListThemeVersions Prelude.Text
listThemeVersions_themeId = Lens.lens (\ListThemeVersions' {themeId} -> themeId) (\s@ListThemeVersions' {} a -> s {themeId = a} :: ListThemeVersions)

instance Core.AWSPager ListThemeVersions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listThemeVersionsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listThemeVersionsResponse_themeVersionSummaryList
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listThemeVersions_nextToken
              Lens..~ rs
              Lens.^? listThemeVersionsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListThemeVersions where
  type
    AWSResponse ListThemeVersions =
      ListThemeVersionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListThemeVersionsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> ( x
                            Data..?> "ThemeVersionSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListThemeVersions where
  hashWithSalt _salt ListThemeVersions' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` themeId

instance Prelude.NFData ListThemeVersions where
  rnf ListThemeVersions' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf awsAccountId `Prelude.seq`
          Prelude.rnf themeId

instance Data.ToHeaders ListThemeVersions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListThemeVersions where
  toPath ListThemeVersions' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/themes/",
        Data.toBS themeId,
        "/versions"
      ]

instance Data.ToQuery ListThemeVersions where
  toQuery ListThemeVersions' {..} =
    Prelude.mconcat
      [ "max-results" Data.=: maxResults,
        "next-token" Data.=: nextToken
      ]

-- | /See:/ 'newListThemeVersionsResponse' smart constructor.
data ListThemeVersionsResponse = ListThemeVersionsResponse'
  { -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | A structure containing a list of all the versions of the specified
    -- theme.
    themeVersionSummaryList :: Prelude.Maybe [ThemeVersionSummary],
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListThemeVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listThemeVersionsResponse_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'requestId', 'listThemeVersionsResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'themeVersionSummaryList', 'listThemeVersionsResponse_themeVersionSummaryList' - A structure containing a list of all the versions of the specified
-- theme.
--
-- 'status', 'listThemeVersionsResponse_status' - The HTTP status of the request.
newListThemeVersionsResponse ::
  -- | 'status'
  Prelude.Int ->
  ListThemeVersionsResponse
newListThemeVersionsResponse pStatus_ =
  ListThemeVersionsResponse'
    { nextToken =
        Prelude.Nothing,
      requestId = Prelude.Nothing,
      themeVersionSummaryList = Prelude.Nothing,
      status = pStatus_
    }

-- | The token for the next set of results, or null if there are no more
-- results.
listThemeVersionsResponse_nextToken :: Lens.Lens' ListThemeVersionsResponse (Prelude.Maybe Prelude.Text)
listThemeVersionsResponse_nextToken = Lens.lens (\ListThemeVersionsResponse' {nextToken} -> nextToken) (\s@ListThemeVersionsResponse' {} a -> s {nextToken = a} :: ListThemeVersionsResponse)

-- | The Amazon Web Services request ID for this operation.
listThemeVersionsResponse_requestId :: Lens.Lens' ListThemeVersionsResponse (Prelude.Maybe Prelude.Text)
listThemeVersionsResponse_requestId = Lens.lens (\ListThemeVersionsResponse' {requestId} -> requestId) (\s@ListThemeVersionsResponse' {} a -> s {requestId = a} :: ListThemeVersionsResponse)

-- | A structure containing a list of all the versions of the specified
-- theme.
listThemeVersionsResponse_themeVersionSummaryList :: Lens.Lens' ListThemeVersionsResponse (Prelude.Maybe [ThemeVersionSummary])
listThemeVersionsResponse_themeVersionSummaryList = Lens.lens (\ListThemeVersionsResponse' {themeVersionSummaryList} -> themeVersionSummaryList) (\s@ListThemeVersionsResponse' {} a -> s {themeVersionSummaryList = a} :: ListThemeVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The HTTP status of the request.
listThemeVersionsResponse_status :: Lens.Lens' ListThemeVersionsResponse Prelude.Int
listThemeVersionsResponse_status = Lens.lens (\ListThemeVersionsResponse' {status} -> status) (\s@ListThemeVersionsResponse' {} a -> s {status = a} :: ListThemeVersionsResponse)

instance Prelude.NFData ListThemeVersionsResponse where
  rnf ListThemeVersionsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf requestId `Prelude.seq`
        Prelude.rnf themeVersionSummaryList `Prelude.seq`
          Prelude.rnf status
