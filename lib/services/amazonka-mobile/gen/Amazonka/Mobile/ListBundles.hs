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
-- Module      : Amazonka.Mobile.ListBundles
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all available bundles.
--
-- This operation returns paginated results.
module Amazonka.Mobile.ListBundles
  ( -- * Creating a Request
    ListBundles (..),
    newListBundles,

    -- * Request Lenses
    listBundles_maxResults,
    listBundles_nextToken,

    -- * Destructuring the Response
    ListBundlesResponse (..),
    newListBundlesResponse,

    -- * Response Lenses
    listBundlesResponse_bundleList,
    listBundlesResponse_nextToken,
    listBundlesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Mobile.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request structure to request all available bundles.
--
-- /See:/ 'newListBundles' smart constructor.
data ListBundles = ListBundles'
  { -- | Maximum number of records to list in a single response.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | Pagination token. Set to null to start listing bundles from start. If
    -- non-null pagination token is returned in a result, then pass its value
    -- in here in another request to list more bundles.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBundles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listBundles_maxResults' - Maximum number of records to list in a single response.
--
-- 'nextToken', 'listBundles_nextToken' - Pagination token. Set to null to start listing bundles from start. If
-- non-null pagination token is returned in a result, then pass its value
-- in here in another request to list more bundles.
newListBundles ::
  ListBundles
newListBundles =
  ListBundles'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Maximum number of records to list in a single response.
listBundles_maxResults :: Lens.Lens' ListBundles (Prelude.Maybe Prelude.Int)
listBundles_maxResults = Lens.lens (\ListBundles' {maxResults} -> maxResults) (\s@ListBundles' {} a -> s {maxResults = a} :: ListBundles)

-- | Pagination token. Set to null to start listing bundles from start. If
-- non-null pagination token is returned in a result, then pass its value
-- in here in another request to list more bundles.
listBundles_nextToken :: Lens.Lens' ListBundles (Prelude.Maybe Prelude.Text)
listBundles_nextToken = Lens.lens (\ListBundles' {nextToken} -> nextToken) (\s@ListBundles' {} a -> s {nextToken = a} :: ListBundles)

instance Core.AWSPager ListBundles where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listBundlesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listBundlesResponse_bundleList
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listBundles_nextToken
              Lens..~ rs
              Lens.^? listBundlesResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListBundles where
  type AWSResponse ListBundles = ListBundlesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBundlesResponse'
            Prelude.<$> (x Data..?> "bundleList" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListBundles where
  hashWithSalt _salt ListBundles' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListBundles where
  rnf ListBundles' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken

instance Data.ToHeaders ListBundles where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListBundles where
  toPath = Prelude.const "/bundles"

instance Data.ToQuery ListBundles where
  toQuery ListBundles' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | Result structure contains a list of all available bundles with details.
--
-- /See:/ 'newListBundlesResponse' smart constructor.
data ListBundlesResponse = ListBundlesResponse'
  { -- | A list of bundles.
    bundleList :: Prelude.Maybe [BundleDetails],
    -- | Pagination token. If non-null pagination token is returned in a result,
    -- then pass its value in another request to fetch more entries.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBundlesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bundleList', 'listBundlesResponse_bundleList' - A list of bundles.
--
-- 'nextToken', 'listBundlesResponse_nextToken' - Pagination token. If non-null pagination token is returned in a result,
-- then pass its value in another request to fetch more entries.
--
-- 'httpStatus', 'listBundlesResponse_httpStatus' - The response's http status code.
newListBundlesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListBundlesResponse
newListBundlesResponse pHttpStatus_ =
  ListBundlesResponse'
    { bundleList = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of bundles.
listBundlesResponse_bundleList :: Lens.Lens' ListBundlesResponse (Prelude.Maybe [BundleDetails])
listBundlesResponse_bundleList = Lens.lens (\ListBundlesResponse' {bundleList} -> bundleList) (\s@ListBundlesResponse' {} a -> s {bundleList = a} :: ListBundlesResponse) Prelude.. Lens.mapping Lens.coerced

-- | Pagination token. If non-null pagination token is returned in a result,
-- then pass its value in another request to fetch more entries.
listBundlesResponse_nextToken :: Lens.Lens' ListBundlesResponse (Prelude.Maybe Prelude.Text)
listBundlesResponse_nextToken = Lens.lens (\ListBundlesResponse' {nextToken} -> nextToken) (\s@ListBundlesResponse' {} a -> s {nextToken = a} :: ListBundlesResponse)

-- | The response's http status code.
listBundlesResponse_httpStatus :: Lens.Lens' ListBundlesResponse Prelude.Int
listBundlesResponse_httpStatus = Lens.lens (\ListBundlesResponse' {httpStatus} -> httpStatus) (\s@ListBundlesResponse' {} a -> s {httpStatus = a} :: ListBundlesResponse)

instance Prelude.NFData ListBundlesResponse where
  rnf ListBundlesResponse' {..} =
    Prelude.rnf bundleList `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
