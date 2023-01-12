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
-- Module      : Amazonka.ServiceCatalog.ListRecordHistory
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the specified requests or all performed requests.
--
-- This operation returns paginated results.
module Amazonka.ServiceCatalog.ListRecordHistory
  ( -- * Creating a Request
    ListRecordHistory (..),
    newListRecordHistory,

    -- * Request Lenses
    listRecordHistory_acceptLanguage,
    listRecordHistory_accessLevelFilter,
    listRecordHistory_pageSize,
    listRecordHistory_pageToken,
    listRecordHistory_searchFilter,

    -- * Destructuring the Response
    ListRecordHistoryResponse (..),
    newListRecordHistoryResponse,

    -- * Response Lenses
    listRecordHistoryResponse_nextPageToken,
    listRecordHistoryResponse_recordDetails,
    listRecordHistoryResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newListRecordHistory' smart constructor.
data ListRecordHistory = ListRecordHistory'
  { -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | The access level to use to obtain results. The default is @User@.
    accessLevelFilter :: Prelude.Maybe AccessLevelFilter,
    -- | The maximum number of items to return with this call.
    pageSize :: Prelude.Maybe Prelude.Natural,
    -- | The page token for the next set of results. To retrieve the first set of
    -- results, use null.
    pageToken :: Prelude.Maybe Prelude.Text,
    -- | The search filter to scope the results.
    searchFilter :: Prelude.Maybe ListRecordHistorySearchFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRecordHistory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceptLanguage', 'listRecordHistory_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'accessLevelFilter', 'listRecordHistory_accessLevelFilter' - The access level to use to obtain results. The default is @User@.
--
-- 'pageSize', 'listRecordHistory_pageSize' - The maximum number of items to return with this call.
--
-- 'pageToken', 'listRecordHistory_pageToken' - The page token for the next set of results. To retrieve the first set of
-- results, use null.
--
-- 'searchFilter', 'listRecordHistory_searchFilter' - The search filter to scope the results.
newListRecordHistory ::
  ListRecordHistory
newListRecordHistory =
  ListRecordHistory'
    { acceptLanguage =
        Prelude.Nothing,
      accessLevelFilter = Prelude.Nothing,
      pageSize = Prelude.Nothing,
      pageToken = Prelude.Nothing,
      searchFilter = Prelude.Nothing
    }

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
listRecordHistory_acceptLanguage :: Lens.Lens' ListRecordHistory (Prelude.Maybe Prelude.Text)
listRecordHistory_acceptLanguage = Lens.lens (\ListRecordHistory' {acceptLanguage} -> acceptLanguage) (\s@ListRecordHistory' {} a -> s {acceptLanguage = a} :: ListRecordHistory)

-- | The access level to use to obtain results. The default is @User@.
listRecordHistory_accessLevelFilter :: Lens.Lens' ListRecordHistory (Prelude.Maybe AccessLevelFilter)
listRecordHistory_accessLevelFilter = Lens.lens (\ListRecordHistory' {accessLevelFilter} -> accessLevelFilter) (\s@ListRecordHistory' {} a -> s {accessLevelFilter = a} :: ListRecordHistory)

-- | The maximum number of items to return with this call.
listRecordHistory_pageSize :: Lens.Lens' ListRecordHistory (Prelude.Maybe Prelude.Natural)
listRecordHistory_pageSize = Lens.lens (\ListRecordHistory' {pageSize} -> pageSize) (\s@ListRecordHistory' {} a -> s {pageSize = a} :: ListRecordHistory)

-- | The page token for the next set of results. To retrieve the first set of
-- results, use null.
listRecordHistory_pageToken :: Lens.Lens' ListRecordHistory (Prelude.Maybe Prelude.Text)
listRecordHistory_pageToken = Lens.lens (\ListRecordHistory' {pageToken} -> pageToken) (\s@ListRecordHistory' {} a -> s {pageToken = a} :: ListRecordHistory)

-- | The search filter to scope the results.
listRecordHistory_searchFilter :: Lens.Lens' ListRecordHistory (Prelude.Maybe ListRecordHistorySearchFilter)
listRecordHistory_searchFilter = Lens.lens (\ListRecordHistory' {searchFilter} -> searchFilter) (\s@ListRecordHistory' {} a -> s {searchFilter = a} :: ListRecordHistory)

instance Core.AWSPager ListRecordHistory where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRecordHistoryResponse_nextPageToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listRecordHistoryResponse_recordDetails
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listRecordHistory_pageToken
          Lens..~ rs
          Lens.^? listRecordHistoryResponse_nextPageToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListRecordHistory where
  type
    AWSResponse ListRecordHistory =
      ListRecordHistoryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRecordHistoryResponse'
            Prelude.<$> (x Data..?> "NextPageToken")
            Prelude.<*> (x Data..?> "RecordDetails" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRecordHistory where
  hashWithSalt _salt ListRecordHistory' {..} =
    _salt `Prelude.hashWithSalt` acceptLanguage
      `Prelude.hashWithSalt` accessLevelFilter
      `Prelude.hashWithSalt` pageSize
      `Prelude.hashWithSalt` pageToken
      `Prelude.hashWithSalt` searchFilter

instance Prelude.NFData ListRecordHistory where
  rnf ListRecordHistory' {..} =
    Prelude.rnf acceptLanguage
      `Prelude.seq` Prelude.rnf accessLevelFilter
      `Prelude.seq` Prelude.rnf pageSize
      `Prelude.seq` Prelude.rnf pageToken
      `Prelude.seq` Prelude.rnf searchFilter

instance Data.ToHeaders ListRecordHistory where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWS242ServiceCatalogService.ListRecordHistory" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListRecordHistory where
  toJSON ListRecordHistory' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AcceptLanguage" Data..=)
              Prelude.<$> acceptLanguage,
            ("AccessLevelFilter" Data..=)
              Prelude.<$> accessLevelFilter,
            ("PageSize" Data..=) Prelude.<$> pageSize,
            ("PageToken" Data..=) Prelude.<$> pageToken,
            ("SearchFilter" Data..=) Prelude.<$> searchFilter
          ]
      )

instance Data.ToPath ListRecordHistory where
  toPath = Prelude.const "/"

instance Data.ToQuery ListRecordHistory where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListRecordHistoryResponse' smart constructor.
data ListRecordHistoryResponse = ListRecordHistoryResponse'
  { -- | The page token to use to retrieve the next set of results. If there are
    -- no additional results, this value is null.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | The records, in reverse chronological order.
    recordDetails :: Prelude.Maybe [RecordDetail],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRecordHistoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageToken', 'listRecordHistoryResponse_nextPageToken' - The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
--
-- 'recordDetails', 'listRecordHistoryResponse_recordDetails' - The records, in reverse chronological order.
--
-- 'httpStatus', 'listRecordHistoryResponse_httpStatus' - The response's http status code.
newListRecordHistoryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRecordHistoryResponse
newListRecordHistoryResponse pHttpStatus_ =
  ListRecordHistoryResponse'
    { nextPageToken =
        Prelude.Nothing,
      recordDetails = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
listRecordHistoryResponse_nextPageToken :: Lens.Lens' ListRecordHistoryResponse (Prelude.Maybe Prelude.Text)
listRecordHistoryResponse_nextPageToken = Lens.lens (\ListRecordHistoryResponse' {nextPageToken} -> nextPageToken) (\s@ListRecordHistoryResponse' {} a -> s {nextPageToken = a} :: ListRecordHistoryResponse)

-- | The records, in reverse chronological order.
listRecordHistoryResponse_recordDetails :: Lens.Lens' ListRecordHistoryResponse (Prelude.Maybe [RecordDetail])
listRecordHistoryResponse_recordDetails = Lens.lens (\ListRecordHistoryResponse' {recordDetails} -> recordDetails) (\s@ListRecordHistoryResponse' {} a -> s {recordDetails = a} :: ListRecordHistoryResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listRecordHistoryResponse_httpStatus :: Lens.Lens' ListRecordHistoryResponse Prelude.Int
listRecordHistoryResponse_httpStatus = Lens.lens (\ListRecordHistoryResponse' {httpStatus} -> httpStatus) (\s@ListRecordHistoryResponse' {} a -> s {httpStatus = a} :: ListRecordHistoryResponse)

instance Prelude.NFData ListRecordHistoryResponse where
  rnf ListRecordHistoryResponse' {..} =
    Prelude.rnf nextPageToken
      `Prelude.seq` Prelude.rnf recordDetails
      `Prelude.seq` Prelude.rnf httpStatus
