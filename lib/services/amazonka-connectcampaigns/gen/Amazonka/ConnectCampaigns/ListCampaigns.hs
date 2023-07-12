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
-- Module      : Amazonka.ConnectCampaigns.ListCampaigns
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides summary information about the campaigns under the specified
-- Amazon Connect account.
--
-- This operation returns paginated results.
module Amazonka.ConnectCampaigns.ListCampaigns
  ( -- * Creating a Request
    ListCampaigns (..),
    newListCampaigns,

    -- * Request Lenses
    listCampaigns_filters,
    listCampaigns_maxResults,
    listCampaigns_nextToken,

    -- * Destructuring the Response
    ListCampaignsResponse (..),
    newListCampaignsResponse,

    -- * Response Lenses
    listCampaignsResponse_campaignSummaryList,
    listCampaignsResponse_nextToken,
    listCampaignsResponse_httpStatus,
  )
where

import Amazonka.ConnectCampaigns.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | ListCampaignsRequest
--
-- /See:/ 'newListCampaigns' smart constructor.
data ListCampaigns = ListCampaigns'
  { filters :: Prelude.Maybe CampaignFilters,
    maxResults :: Prelude.Maybe Prelude.Natural,
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCampaigns' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listCampaigns_filters' - Undocumented member.
--
-- 'maxResults', 'listCampaigns_maxResults' - Undocumented member.
--
-- 'nextToken', 'listCampaigns_nextToken' - Undocumented member.
newListCampaigns ::
  ListCampaigns
newListCampaigns =
  ListCampaigns'
    { filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Undocumented member.
listCampaigns_filters :: Lens.Lens' ListCampaigns (Prelude.Maybe CampaignFilters)
listCampaigns_filters = Lens.lens (\ListCampaigns' {filters} -> filters) (\s@ListCampaigns' {} a -> s {filters = a} :: ListCampaigns)

-- | Undocumented member.
listCampaigns_maxResults :: Lens.Lens' ListCampaigns (Prelude.Maybe Prelude.Natural)
listCampaigns_maxResults = Lens.lens (\ListCampaigns' {maxResults} -> maxResults) (\s@ListCampaigns' {} a -> s {maxResults = a} :: ListCampaigns)

-- | Undocumented member.
listCampaigns_nextToken :: Lens.Lens' ListCampaigns (Prelude.Maybe Prelude.Text)
listCampaigns_nextToken = Lens.lens (\ListCampaigns' {nextToken} -> nextToken) (\s@ListCampaigns' {} a -> s {nextToken = a} :: ListCampaigns)

instance Core.AWSPager ListCampaigns where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listCampaignsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listCampaignsResponse_campaignSummaryList
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listCampaigns_nextToken
          Lens..~ rs
          Lens.^? listCampaignsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListCampaigns where
  type
    AWSResponse ListCampaigns =
      ListCampaignsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCampaignsResponse'
            Prelude.<$> ( x
                            Data..?> "campaignSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCampaigns where
  hashWithSalt _salt ListCampaigns' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListCampaigns where
  rnf ListCampaigns' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListCampaigns where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListCampaigns where
  toJSON ListCampaigns' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("filters" Data..=) Prelude.<$> filters,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListCampaigns where
  toPath = Prelude.const "/campaigns-summary"

instance Data.ToQuery ListCampaigns where
  toQuery = Prelude.const Prelude.mempty

-- | ListCampaignsResponse
--
-- /See:/ 'newListCampaignsResponse' smart constructor.
data ListCampaignsResponse = ListCampaignsResponse'
  { campaignSummaryList :: Prelude.Maybe [CampaignSummary],
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCampaignsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'campaignSummaryList', 'listCampaignsResponse_campaignSummaryList' - Undocumented member.
--
-- 'nextToken', 'listCampaignsResponse_nextToken' - Undocumented member.
--
-- 'httpStatus', 'listCampaignsResponse_httpStatus' - The response's http status code.
newListCampaignsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCampaignsResponse
newListCampaignsResponse pHttpStatus_ =
  ListCampaignsResponse'
    { campaignSummaryList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
listCampaignsResponse_campaignSummaryList :: Lens.Lens' ListCampaignsResponse (Prelude.Maybe [CampaignSummary])
listCampaignsResponse_campaignSummaryList = Lens.lens (\ListCampaignsResponse' {campaignSummaryList} -> campaignSummaryList) (\s@ListCampaignsResponse' {} a -> s {campaignSummaryList = a} :: ListCampaignsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
listCampaignsResponse_nextToken :: Lens.Lens' ListCampaignsResponse (Prelude.Maybe Prelude.Text)
listCampaignsResponse_nextToken = Lens.lens (\ListCampaignsResponse' {nextToken} -> nextToken) (\s@ListCampaignsResponse' {} a -> s {nextToken = a} :: ListCampaignsResponse)

-- | The response's http status code.
listCampaignsResponse_httpStatus :: Lens.Lens' ListCampaignsResponse Prelude.Int
listCampaignsResponse_httpStatus = Lens.lens (\ListCampaignsResponse' {httpStatus} -> httpStatus) (\s@ListCampaignsResponse' {} a -> s {httpStatus = a} :: ListCampaignsResponse)

instance Prelude.NFData ListCampaignsResponse where
  rnf ListCampaignsResponse' {..} =
    Prelude.rnf campaignSummaryList
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
