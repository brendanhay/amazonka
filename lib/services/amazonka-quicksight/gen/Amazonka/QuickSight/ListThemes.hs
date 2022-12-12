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
-- Module      : Amazonka.QuickSight.ListThemes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the themes in the current Amazon Web Services account.
--
-- This operation returns paginated results.
module Amazonka.QuickSight.ListThemes
  ( -- * Creating a Request
    ListThemes (..),
    newListThemes,

    -- * Request Lenses
    listThemes_maxResults,
    listThemes_nextToken,
    listThemes_type,
    listThemes_awsAccountId,

    -- * Destructuring the Response
    ListThemesResponse (..),
    newListThemesResponse,

    -- * Response Lenses
    listThemesResponse_nextToken,
    listThemesResponse_requestId,
    listThemesResponse_themeSummaryList,
    listThemesResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListThemes' smart constructor.
data ListThemes = ListThemes'
  { -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The type of themes that you want to list. Valid options include the
    -- following:
    --
    -- -   @ALL (default)@- Display all existing themes.
    --
    -- -   @CUSTOM@ - Display only the themes created by people using Amazon
    --     QuickSight.
    --
    -- -   @QUICKSIGHT@ - Display only the starting themes defined by Amazon
    --     QuickSight.
    type' :: Prelude.Maybe ThemeType,
    -- | The ID of the Amazon Web Services account that contains the themes that
    -- you\'re listing.
    awsAccountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListThemes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listThemes_maxResults' - The maximum number of results to be returned per request.
--
-- 'nextToken', 'listThemes_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'type'', 'listThemes_type' - The type of themes that you want to list. Valid options include the
-- following:
--
-- -   @ALL (default)@- Display all existing themes.
--
-- -   @CUSTOM@ - Display only the themes created by people using Amazon
--     QuickSight.
--
-- -   @QUICKSIGHT@ - Display only the starting themes defined by Amazon
--     QuickSight.
--
-- 'awsAccountId', 'listThemes_awsAccountId' - The ID of the Amazon Web Services account that contains the themes that
-- you\'re listing.
newListThemes ::
  -- | 'awsAccountId'
  Prelude.Text ->
  ListThemes
newListThemes pAwsAccountId_ =
  ListThemes'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      type' = Prelude.Nothing,
      awsAccountId = pAwsAccountId_
    }

-- | The maximum number of results to be returned per request.
listThemes_maxResults :: Lens.Lens' ListThemes (Prelude.Maybe Prelude.Natural)
listThemes_maxResults = Lens.lens (\ListThemes' {maxResults} -> maxResults) (\s@ListThemes' {} a -> s {maxResults = a} :: ListThemes)

-- | The token for the next set of results, or null if there are no more
-- results.
listThemes_nextToken :: Lens.Lens' ListThemes (Prelude.Maybe Prelude.Text)
listThemes_nextToken = Lens.lens (\ListThemes' {nextToken} -> nextToken) (\s@ListThemes' {} a -> s {nextToken = a} :: ListThemes)

-- | The type of themes that you want to list. Valid options include the
-- following:
--
-- -   @ALL (default)@- Display all existing themes.
--
-- -   @CUSTOM@ - Display only the themes created by people using Amazon
--     QuickSight.
--
-- -   @QUICKSIGHT@ - Display only the starting themes defined by Amazon
--     QuickSight.
listThemes_type :: Lens.Lens' ListThemes (Prelude.Maybe ThemeType)
listThemes_type = Lens.lens (\ListThemes' {type'} -> type') (\s@ListThemes' {} a -> s {type' = a} :: ListThemes)

-- | The ID of the Amazon Web Services account that contains the themes that
-- you\'re listing.
listThemes_awsAccountId :: Lens.Lens' ListThemes Prelude.Text
listThemes_awsAccountId = Lens.lens (\ListThemes' {awsAccountId} -> awsAccountId) (\s@ListThemes' {} a -> s {awsAccountId = a} :: ListThemes)

instance Core.AWSPager ListThemes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listThemesResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listThemesResponse_themeSummaryList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listThemes_nextToken
          Lens..~ rs
          Lens.^? listThemesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListThemes where
  type AWSResponse ListThemes = ListThemesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListThemesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> ( x Data..?> "ThemeSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListThemes where
  hashWithSalt _salt ListThemes' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` awsAccountId

instance Prelude.NFData ListThemes where
  rnf ListThemes' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf awsAccountId

instance Data.ToHeaders ListThemes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListThemes where
  toPath ListThemes' {..} =
    Prelude.mconcat
      ["/accounts/", Data.toBS awsAccountId, "/themes"]

instance Data.ToQuery ListThemes where
  toQuery ListThemes' {..} =
    Prelude.mconcat
      [ "max-results" Data.=: maxResults,
        "next-token" Data.=: nextToken,
        "type" Data.=: type'
      ]

-- | /See:/ 'newListThemesResponse' smart constructor.
data ListThemesResponse = ListThemesResponse'
  { -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | Information about the themes in the list.
    themeSummaryList :: Prelude.Maybe [ThemeSummary],
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListThemesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listThemesResponse_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'requestId', 'listThemesResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'themeSummaryList', 'listThemesResponse_themeSummaryList' - Information about the themes in the list.
--
-- 'status', 'listThemesResponse_status' - The HTTP status of the request.
newListThemesResponse ::
  -- | 'status'
  Prelude.Int ->
  ListThemesResponse
newListThemesResponse pStatus_ =
  ListThemesResponse'
    { nextToken = Prelude.Nothing,
      requestId = Prelude.Nothing,
      themeSummaryList = Prelude.Nothing,
      status = pStatus_
    }

-- | The token for the next set of results, or null if there are no more
-- results.
listThemesResponse_nextToken :: Lens.Lens' ListThemesResponse (Prelude.Maybe Prelude.Text)
listThemesResponse_nextToken = Lens.lens (\ListThemesResponse' {nextToken} -> nextToken) (\s@ListThemesResponse' {} a -> s {nextToken = a} :: ListThemesResponse)

-- | The Amazon Web Services request ID for this operation.
listThemesResponse_requestId :: Lens.Lens' ListThemesResponse (Prelude.Maybe Prelude.Text)
listThemesResponse_requestId = Lens.lens (\ListThemesResponse' {requestId} -> requestId) (\s@ListThemesResponse' {} a -> s {requestId = a} :: ListThemesResponse)

-- | Information about the themes in the list.
listThemesResponse_themeSummaryList :: Lens.Lens' ListThemesResponse (Prelude.Maybe [ThemeSummary])
listThemesResponse_themeSummaryList = Lens.lens (\ListThemesResponse' {themeSummaryList} -> themeSummaryList) (\s@ListThemesResponse' {} a -> s {themeSummaryList = a} :: ListThemesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The HTTP status of the request.
listThemesResponse_status :: Lens.Lens' ListThemesResponse Prelude.Int
listThemesResponse_status = Lens.lens (\ListThemesResponse' {status} -> status) (\s@ListThemesResponse' {} a -> s {status = a} :: ListThemesResponse)

instance Prelude.NFData ListThemesResponse where
  rnf ListThemesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf themeSummaryList
      `Prelude.seq` Prelude.rnf status
