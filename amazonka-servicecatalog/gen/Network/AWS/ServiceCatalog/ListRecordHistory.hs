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
-- Module      : Network.AWS.ServiceCatalog.ListRecordHistory
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the specified requests or all performed requests.
--
-- This operation returns paginated results.
module Network.AWS.ServiceCatalog.ListRecordHistory
  ( -- * Creating a Request
    ListRecordHistory (..),
    newListRecordHistory,

    -- * Request Lenses
    listRecordHistory_pageSize,
    listRecordHistory_pageToken,
    listRecordHistory_accessLevelFilter,
    listRecordHistory_searchFilter,
    listRecordHistory_acceptLanguage,

    -- * Destructuring the Response
    ListRecordHistoryResponse (..),
    newListRecordHistoryResponse,

    -- * Response Lenses
    listRecordHistoryResponse_recordDetails,
    listRecordHistoryResponse_nextPageToken,
    listRecordHistoryResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newListRecordHistory' smart constructor.
data ListRecordHistory = ListRecordHistory'
  { -- | The maximum number of items to return with this call.
    pageSize :: Prelude.Maybe Prelude.Natural,
    -- | The page token for the next set of results. To retrieve the first set of
    -- results, use null.
    pageToken :: Prelude.Maybe Prelude.Text,
    -- | The access level to use to obtain results. The default is @User@.
    accessLevelFilter :: Prelude.Maybe AccessLevelFilter,
    -- | The search filter to scope the results.
    searchFilter :: Prelude.Maybe ListRecordHistorySearchFilter,
    -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text
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
-- 'pageSize', 'listRecordHistory_pageSize' - The maximum number of items to return with this call.
--
-- 'pageToken', 'listRecordHistory_pageToken' - The page token for the next set of results. To retrieve the first set of
-- results, use null.
--
-- 'accessLevelFilter', 'listRecordHistory_accessLevelFilter' - The access level to use to obtain results. The default is @User@.
--
-- 'searchFilter', 'listRecordHistory_searchFilter' - The search filter to scope the results.
--
-- 'acceptLanguage', 'listRecordHistory_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
newListRecordHistory ::
  ListRecordHistory
newListRecordHistory =
  ListRecordHistory'
    { pageSize = Prelude.Nothing,
      pageToken = Prelude.Nothing,
      accessLevelFilter = Prelude.Nothing,
      searchFilter = Prelude.Nothing,
      acceptLanguage = Prelude.Nothing
    }

-- | The maximum number of items to return with this call.
listRecordHistory_pageSize :: Lens.Lens' ListRecordHistory (Prelude.Maybe Prelude.Natural)
listRecordHistory_pageSize = Lens.lens (\ListRecordHistory' {pageSize} -> pageSize) (\s@ListRecordHistory' {} a -> s {pageSize = a} :: ListRecordHistory)

-- | The page token for the next set of results. To retrieve the first set of
-- results, use null.
listRecordHistory_pageToken :: Lens.Lens' ListRecordHistory (Prelude.Maybe Prelude.Text)
listRecordHistory_pageToken = Lens.lens (\ListRecordHistory' {pageToken} -> pageToken) (\s@ListRecordHistory' {} a -> s {pageToken = a} :: ListRecordHistory)

-- | The access level to use to obtain results. The default is @User@.
listRecordHistory_accessLevelFilter :: Lens.Lens' ListRecordHistory (Prelude.Maybe AccessLevelFilter)
listRecordHistory_accessLevelFilter = Lens.lens (\ListRecordHistory' {accessLevelFilter} -> accessLevelFilter) (\s@ListRecordHistory' {} a -> s {accessLevelFilter = a} :: ListRecordHistory)

-- | The search filter to scope the results.
listRecordHistory_searchFilter :: Lens.Lens' ListRecordHistory (Prelude.Maybe ListRecordHistorySearchFilter)
listRecordHistory_searchFilter = Lens.lens (\ListRecordHistory' {searchFilter} -> searchFilter) (\s@ListRecordHistory' {} a -> s {searchFilter = a} :: ListRecordHistory)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
listRecordHistory_acceptLanguage :: Lens.Lens' ListRecordHistory (Prelude.Maybe Prelude.Text)
listRecordHistory_acceptLanguage = Lens.lens (\ListRecordHistory' {acceptLanguage} -> acceptLanguage) (\s@ListRecordHistory' {} a -> s {acceptLanguage = a} :: ListRecordHistory)

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRecordHistoryResponse'
            Prelude.<$> (x Core..?> "RecordDetails" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextPageToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRecordHistory

instance Prelude.NFData ListRecordHistory

instance Core.ToHeaders ListRecordHistory where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.ListRecordHistory" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListRecordHistory where
  toJSON ListRecordHistory' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PageSize" Core..=) Prelude.<$> pageSize,
            ("PageToken" Core..=) Prelude.<$> pageToken,
            ("AccessLevelFilter" Core..=)
              Prelude.<$> accessLevelFilter,
            ("SearchFilter" Core..=) Prelude.<$> searchFilter,
            ("AcceptLanguage" Core..=)
              Prelude.<$> acceptLanguage
          ]
      )

instance Core.ToPath ListRecordHistory where
  toPath = Prelude.const "/"

instance Core.ToQuery ListRecordHistory where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListRecordHistoryResponse' smart constructor.
data ListRecordHistoryResponse = ListRecordHistoryResponse'
  { -- | The records, in reverse chronological order.
    recordDetails :: Prelude.Maybe [RecordDetail],
    -- | The page token to use to retrieve the next set of results. If there are
    -- no additional results, this value is null.
    nextPageToken :: Prelude.Maybe Prelude.Text,
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
-- 'recordDetails', 'listRecordHistoryResponse_recordDetails' - The records, in reverse chronological order.
--
-- 'nextPageToken', 'listRecordHistoryResponse_nextPageToken' - The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
--
-- 'httpStatus', 'listRecordHistoryResponse_httpStatus' - The response's http status code.
newListRecordHistoryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRecordHistoryResponse
newListRecordHistoryResponse pHttpStatus_ =
  ListRecordHistoryResponse'
    { recordDetails =
        Prelude.Nothing,
      nextPageToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The records, in reverse chronological order.
listRecordHistoryResponse_recordDetails :: Lens.Lens' ListRecordHistoryResponse (Prelude.Maybe [RecordDetail])
listRecordHistoryResponse_recordDetails = Lens.lens (\ListRecordHistoryResponse' {recordDetails} -> recordDetails) (\s@ListRecordHistoryResponse' {} a -> s {recordDetails = a} :: ListRecordHistoryResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
listRecordHistoryResponse_nextPageToken :: Lens.Lens' ListRecordHistoryResponse (Prelude.Maybe Prelude.Text)
listRecordHistoryResponse_nextPageToken = Lens.lens (\ListRecordHistoryResponse' {nextPageToken} -> nextPageToken) (\s@ListRecordHistoryResponse' {} a -> s {nextPageToken = a} :: ListRecordHistoryResponse)

-- | The response's http status code.
listRecordHistoryResponse_httpStatus :: Lens.Lens' ListRecordHistoryResponse Prelude.Int
listRecordHistoryResponse_httpStatus = Lens.lens (\ListRecordHistoryResponse' {httpStatus} -> httpStatus) (\s@ListRecordHistoryResponse' {} a -> s {httpStatus = a} :: ListRecordHistoryResponse)

instance Prelude.NFData ListRecordHistoryResponse
