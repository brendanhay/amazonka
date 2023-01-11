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
-- Module      : Amazonka.Outposts.ListAssets
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the hardware assets for the specified Outpost.
--
-- Use filters to return specific results. If you specify multiple filters,
-- the results include only the resources that match all of the specified
-- filters. For a filter where you can specify multiple values, the results
-- include items that match any of the values that you specify for the
-- filter.
module Amazonka.Outposts.ListAssets
  ( -- * Creating a Request
    ListAssets (..),
    newListAssets,

    -- * Request Lenses
    listAssets_hostIdFilter,
    listAssets_maxResults,
    listAssets_nextToken,
    listAssets_statusFilter,
    listAssets_outpostIdentifier,

    -- * Destructuring the Response
    ListAssetsResponse (..),
    newListAssetsResponse,

    -- * Response Lenses
    listAssetsResponse_assets,
    listAssetsResponse_nextToken,
    listAssetsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Outposts.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAssets' smart constructor.
data ListAssets = ListAssets'
  { -- | Filters the results by the host ID of a Dedicated Host.
    hostIdFilter :: Prelude.Maybe [Prelude.Text],
    maxResults :: Prelude.Maybe Prelude.Natural,
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Filters the results by state.
    statusFilter :: Prelude.Maybe (Prelude.NonEmpty AssetState),
    -- | The ID or the Amazon Resource Name (ARN) of the Outpost.
    outpostIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAssets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hostIdFilter', 'listAssets_hostIdFilter' - Filters the results by the host ID of a Dedicated Host.
--
-- 'maxResults', 'listAssets_maxResults' - Undocumented member.
--
-- 'nextToken', 'listAssets_nextToken' - Undocumented member.
--
-- 'statusFilter', 'listAssets_statusFilter' - Filters the results by state.
--
-- 'outpostIdentifier', 'listAssets_outpostIdentifier' - The ID or the Amazon Resource Name (ARN) of the Outpost.
newListAssets ::
  -- | 'outpostIdentifier'
  Prelude.Text ->
  ListAssets
newListAssets pOutpostIdentifier_ =
  ListAssets'
    { hostIdFilter = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      statusFilter = Prelude.Nothing,
      outpostIdentifier = pOutpostIdentifier_
    }

-- | Filters the results by the host ID of a Dedicated Host.
listAssets_hostIdFilter :: Lens.Lens' ListAssets (Prelude.Maybe [Prelude.Text])
listAssets_hostIdFilter = Lens.lens (\ListAssets' {hostIdFilter} -> hostIdFilter) (\s@ListAssets' {} a -> s {hostIdFilter = a} :: ListAssets) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
listAssets_maxResults :: Lens.Lens' ListAssets (Prelude.Maybe Prelude.Natural)
listAssets_maxResults = Lens.lens (\ListAssets' {maxResults} -> maxResults) (\s@ListAssets' {} a -> s {maxResults = a} :: ListAssets)

-- | Undocumented member.
listAssets_nextToken :: Lens.Lens' ListAssets (Prelude.Maybe Prelude.Text)
listAssets_nextToken = Lens.lens (\ListAssets' {nextToken} -> nextToken) (\s@ListAssets' {} a -> s {nextToken = a} :: ListAssets)

-- | Filters the results by state.
listAssets_statusFilter :: Lens.Lens' ListAssets (Prelude.Maybe (Prelude.NonEmpty AssetState))
listAssets_statusFilter = Lens.lens (\ListAssets' {statusFilter} -> statusFilter) (\s@ListAssets' {} a -> s {statusFilter = a} :: ListAssets) Prelude.. Lens.mapping Lens.coerced

-- | The ID or the Amazon Resource Name (ARN) of the Outpost.
listAssets_outpostIdentifier :: Lens.Lens' ListAssets Prelude.Text
listAssets_outpostIdentifier = Lens.lens (\ListAssets' {outpostIdentifier} -> outpostIdentifier) (\s@ListAssets' {} a -> s {outpostIdentifier = a} :: ListAssets)

instance Core.AWSRequest ListAssets where
  type AWSResponse ListAssets = ListAssetsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAssetsResponse'
            Prelude.<$> (x Data..?> "Assets" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAssets where
  hashWithSalt _salt ListAssets' {..} =
    _salt `Prelude.hashWithSalt` hostIdFilter
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` statusFilter
      `Prelude.hashWithSalt` outpostIdentifier

instance Prelude.NFData ListAssets where
  rnf ListAssets' {..} =
    Prelude.rnf hostIdFilter
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf statusFilter
      `Prelude.seq` Prelude.rnf outpostIdentifier

instance Data.ToHeaders ListAssets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListAssets where
  toPath ListAssets' {..} =
    Prelude.mconcat
      [ "/outposts/",
        Data.toBS outpostIdentifier,
        "/assets"
      ]

instance Data.ToQuery ListAssets where
  toQuery ListAssets' {..} =
    Prelude.mconcat
      [ "HostIdFilter"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> hostIdFilter),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken,
        "StatusFilter"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> statusFilter)
      ]

-- | /See:/ 'newListAssetsResponse' smart constructor.
data ListAssetsResponse = ListAssetsResponse'
  { -- | Information about the hardware assets.
    assets :: Prelude.Maybe [AssetInfo],
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAssetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assets', 'listAssetsResponse_assets' - Information about the hardware assets.
--
-- 'nextToken', 'listAssetsResponse_nextToken' - Undocumented member.
--
-- 'httpStatus', 'listAssetsResponse_httpStatus' - The response's http status code.
newListAssetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAssetsResponse
newListAssetsResponse pHttpStatus_ =
  ListAssetsResponse'
    { assets = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the hardware assets.
listAssetsResponse_assets :: Lens.Lens' ListAssetsResponse (Prelude.Maybe [AssetInfo])
listAssetsResponse_assets = Lens.lens (\ListAssetsResponse' {assets} -> assets) (\s@ListAssetsResponse' {} a -> s {assets = a} :: ListAssetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
listAssetsResponse_nextToken :: Lens.Lens' ListAssetsResponse (Prelude.Maybe Prelude.Text)
listAssetsResponse_nextToken = Lens.lens (\ListAssetsResponse' {nextToken} -> nextToken) (\s@ListAssetsResponse' {} a -> s {nextToken = a} :: ListAssetsResponse)

-- | The response's http status code.
listAssetsResponse_httpStatus :: Lens.Lens' ListAssetsResponse Prelude.Int
listAssetsResponse_httpStatus = Lens.lens (\ListAssetsResponse' {httpStatus} -> httpStatus) (\s@ListAssetsResponse' {} a -> s {httpStatus = a} :: ListAssetsResponse)

instance Prelude.NFData ListAssetsResponse where
  rnf ListAssetsResponse' {..} =
    Prelude.rnf assets
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
