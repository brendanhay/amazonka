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
-- Module      : Amazonka.ConnectCases.ListLayouts
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all layouts in the given cases domain. Each list item is a
-- condensed summary object of the layout.
module Amazonka.ConnectCases.ListLayouts
  ( -- * Creating a Request
    ListLayouts (..),
    newListLayouts,

    -- * Request Lenses
    listLayouts_nextToken,
    listLayouts_maxResults,
    listLayouts_domainId,

    -- * Destructuring the Response
    ListLayoutsResponse (..),
    newListLayoutsResponse,

    -- * Response Lenses
    listLayoutsResponse_nextToken,
    listLayoutsResponse_httpStatus,
    listLayoutsResponse_layouts,
  )
where

import Amazonka.ConnectCases.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListLayouts' smart constructor.
data ListLayouts = ListLayouts'
  { -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The unique identifier of the Cases domain.
    domainId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLayouts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listLayouts_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'maxResults', 'listLayouts_maxResults' - The maximum number of results to return per page.
--
-- 'domainId', 'listLayouts_domainId' - The unique identifier of the Cases domain.
newListLayouts ::
  -- | 'domainId'
  Prelude.Text ->
  ListLayouts
newListLayouts pDomainId_ =
  ListLayouts'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      domainId = pDomainId_
    }

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listLayouts_nextToken :: Lens.Lens' ListLayouts (Prelude.Maybe Prelude.Text)
listLayouts_nextToken = Lens.lens (\ListLayouts' {nextToken} -> nextToken) (\s@ListLayouts' {} a -> s {nextToken = a} :: ListLayouts)

-- | The maximum number of results to return per page.
listLayouts_maxResults :: Lens.Lens' ListLayouts (Prelude.Maybe Prelude.Natural)
listLayouts_maxResults = Lens.lens (\ListLayouts' {maxResults} -> maxResults) (\s@ListLayouts' {} a -> s {maxResults = a} :: ListLayouts)

-- | The unique identifier of the Cases domain.
listLayouts_domainId :: Lens.Lens' ListLayouts Prelude.Text
listLayouts_domainId = Lens.lens (\ListLayouts' {domainId} -> domainId) (\s@ListLayouts' {} a -> s {domainId = a} :: ListLayouts)

instance Core.AWSRequest ListLayouts where
  type AWSResponse ListLayouts = ListLayoutsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLayoutsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..?> "layouts" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListLayouts where
  hashWithSalt _salt ListLayouts' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` domainId

instance Prelude.NFData ListLayouts where
  rnf ListLayouts' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf domainId

instance Core.ToHeaders ListLayouts where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListLayouts where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath ListLayouts where
  toPath ListLayouts' {..} =
    Prelude.mconcat
      ["/domains/", Core.toBS domainId, "/layouts-list"]

instance Core.ToQuery ListLayouts where
  toQuery ListLayouts' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListLayoutsResponse' smart constructor.
data ListLayoutsResponse = ListLayoutsResponse'
  { -- | The token for the next set of results. This is null if there are no more
    -- results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The layouts for the domain.
    layouts :: [LayoutSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLayoutsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listLayoutsResponse_nextToken' - The token for the next set of results. This is null if there are no more
-- results to return.
--
-- 'httpStatus', 'listLayoutsResponse_httpStatus' - The response's http status code.
--
-- 'layouts', 'listLayoutsResponse_layouts' - The layouts for the domain.
newListLayoutsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListLayoutsResponse
newListLayoutsResponse pHttpStatus_ =
  ListLayoutsResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      layouts = Prelude.mempty
    }

-- | The token for the next set of results. This is null if there are no more
-- results to return.
listLayoutsResponse_nextToken :: Lens.Lens' ListLayoutsResponse (Prelude.Maybe Prelude.Text)
listLayoutsResponse_nextToken = Lens.lens (\ListLayoutsResponse' {nextToken} -> nextToken) (\s@ListLayoutsResponse' {} a -> s {nextToken = a} :: ListLayoutsResponse)

-- | The response's http status code.
listLayoutsResponse_httpStatus :: Lens.Lens' ListLayoutsResponse Prelude.Int
listLayoutsResponse_httpStatus = Lens.lens (\ListLayoutsResponse' {httpStatus} -> httpStatus) (\s@ListLayoutsResponse' {} a -> s {httpStatus = a} :: ListLayoutsResponse)

-- | The layouts for the domain.
listLayoutsResponse_layouts :: Lens.Lens' ListLayoutsResponse [LayoutSummary]
listLayoutsResponse_layouts = Lens.lens (\ListLayoutsResponse' {layouts} -> layouts) (\s@ListLayoutsResponse' {} a -> s {layouts = a} :: ListLayoutsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListLayoutsResponse where
  rnf ListLayoutsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf layouts
