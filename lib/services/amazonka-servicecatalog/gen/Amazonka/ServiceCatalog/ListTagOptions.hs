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
-- Module      : Amazonka.ServiceCatalog.ListTagOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the specified TagOptions or all TagOptions.
--
-- This operation returns paginated results.
module Amazonka.ServiceCatalog.ListTagOptions
  ( -- * Creating a Request
    ListTagOptions (..),
    newListTagOptions,

    -- * Request Lenses
    listTagOptions_filters,
    listTagOptions_pageSize,
    listTagOptions_pageToken,

    -- * Destructuring the Response
    ListTagOptionsResponse (..),
    newListTagOptionsResponse,

    -- * Response Lenses
    listTagOptionsResponse_pageToken,
    listTagOptionsResponse_tagOptionDetails,
    listTagOptionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newListTagOptions' smart constructor.
data ListTagOptions = ListTagOptions'
  { -- | The search filters. If no search filters are specified, the output
    -- includes all TagOptions.
    filters :: Prelude.Maybe ListTagOptionsFilters,
    -- | The maximum number of items to return with this call.
    pageSize :: Prelude.Maybe Prelude.Natural,
    -- | The page token for the next set of results. To retrieve the first set of
    -- results, use null.
    pageToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTagOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listTagOptions_filters' - The search filters. If no search filters are specified, the output
-- includes all TagOptions.
--
-- 'pageSize', 'listTagOptions_pageSize' - The maximum number of items to return with this call.
--
-- 'pageToken', 'listTagOptions_pageToken' - The page token for the next set of results. To retrieve the first set of
-- results, use null.
newListTagOptions ::
  ListTagOptions
newListTagOptions =
  ListTagOptions'
    { filters = Prelude.Nothing,
      pageSize = Prelude.Nothing,
      pageToken = Prelude.Nothing
    }

-- | The search filters. If no search filters are specified, the output
-- includes all TagOptions.
listTagOptions_filters :: Lens.Lens' ListTagOptions (Prelude.Maybe ListTagOptionsFilters)
listTagOptions_filters = Lens.lens (\ListTagOptions' {filters} -> filters) (\s@ListTagOptions' {} a -> s {filters = a} :: ListTagOptions)

-- | The maximum number of items to return with this call.
listTagOptions_pageSize :: Lens.Lens' ListTagOptions (Prelude.Maybe Prelude.Natural)
listTagOptions_pageSize = Lens.lens (\ListTagOptions' {pageSize} -> pageSize) (\s@ListTagOptions' {} a -> s {pageSize = a} :: ListTagOptions)

-- | The page token for the next set of results. To retrieve the first set of
-- results, use null.
listTagOptions_pageToken :: Lens.Lens' ListTagOptions (Prelude.Maybe Prelude.Text)
listTagOptions_pageToken = Lens.lens (\ListTagOptions' {pageToken} -> pageToken) (\s@ListTagOptions' {} a -> s {pageToken = a} :: ListTagOptions)

instance Core.AWSPager ListTagOptions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTagOptionsResponse_pageToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listTagOptionsResponse_tagOptionDetails
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listTagOptions_pageToken
          Lens..~ rs
          Lens.^? listTagOptionsResponse_pageToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListTagOptions where
  type
    AWSResponse ListTagOptions =
      ListTagOptionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTagOptionsResponse'
            Prelude.<$> (x Data..?> "PageToken")
            Prelude.<*> ( x
                            Data..?> "TagOptionDetails"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTagOptions where
  hashWithSalt _salt ListTagOptions' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` pageSize
      `Prelude.hashWithSalt` pageToken

instance Prelude.NFData ListTagOptions where
  rnf ListTagOptions' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf pageSize
      `Prelude.seq` Prelude.rnf pageToken

instance Data.ToHeaders ListTagOptions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWS242ServiceCatalogService.ListTagOptions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListTagOptions where
  toJSON ListTagOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("PageSize" Data..=) Prelude.<$> pageSize,
            ("PageToken" Data..=) Prelude.<$> pageToken
          ]
      )

instance Data.ToPath ListTagOptions where
  toPath = Prelude.const "/"

instance Data.ToQuery ListTagOptions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTagOptionsResponse' smart constructor.
data ListTagOptionsResponse = ListTagOptionsResponse'
  { -- | The page token for the next set of results. To retrieve the first set of
    -- results, use null.
    pageToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the TagOptions.
    tagOptionDetails :: Prelude.Maybe [TagOptionDetail],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTagOptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageToken', 'listTagOptionsResponse_pageToken' - The page token for the next set of results. To retrieve the first set of
-- results, use null.
--
-- 'tagOptionDetails', 'listTagOptionsResponse_tagOptionDetails' - Information about the TagOptions.
--
-- 'httpStatus', 'listTagOptionsResponse_httpStatus' - The response's http status code.
newListTagOptionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTagOptionsResponse
newListTagOptionsResponse pHttpStatus_ =
  ListTagOptionsResponse'
    { pageToken =
        Prelude.Nothing,
      tagOptionDetails = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The page token for the next set of results. To retrieve the first set of
-- results, use null.
listTagOptionsResponse_pageToken :: Lens.Lens' ListTagOptionsResponse (Prelude.Maybe Prelude.Text)
listTagOptionsResponse_pageToken = Lens.lens (\ListTagOptionsResponse' {pageToken} -> pageToken) (\s@ListTagOptionsResponse' {} a -> s {pageToken = a} :: ListTagOptionsResponse)

-- | Information about the TagOptions.
listTagOptionsResponse_tagOptionDetails :: Lens.Lens' ListTagOptionsResponse (Prelude.Maybe [TagOptionDetail])
listTagOptionsResponse_tagOptionDetails = Lens.lens (\ListTagOptionsResponse' {tagOptionDetails} -> tagOptionDetails) (\s@ListTagOptionsResponse' {} a -> s {tagOptionDetails = a} :: ListTagOptionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listTagOptionsResponse_httpStatus :: Lens.Lens' ListTagOptionsResponse Prelude.Int
listTagOptionsResponse_httpStatus = Lens.lens (\ListTagOptionsResponse' {httpStatus} -> httpStatus) (\s@ListTagOptionsResponse' {} a -> s {httpStatus = a} :: ListTagOptionsResponse)

instance Prelude.NFData ListTagOptionsResponse where
  rnf ListTagOptionsResponse' {..} =
    Prelude.rnf pageToken
      `Prelude.seq` Prelude.rnf tagOptionDetails
      `Prelude.seq` Prelude.rnf httpStatus
