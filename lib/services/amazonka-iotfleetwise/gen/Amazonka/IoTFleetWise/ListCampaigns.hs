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
-- Module      : Amazonka.IoTFleetWise.ListCampaigns
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists information about created campaigns.
--
-- This API operation uses pagination. Specify the @nextToken@ parameter in
-- the request to return more results.
--
-- This operation returns paginated results.
module Amazonka.IoTFleetWise.ListCampaigns
  ( -- * Creating a Request
    ListCampaigns (..),
    newListCampaigns,

    -- * Request Lenses
    listCampaigns_maxResults,
    listCampaigns_nextToken,
    listCampaigns_status,

    -- * Destructuring the Response
    ListCampaignsResponse (..),
    newListCampaignsResponse,

    -- * Response Lenses
    listCampaignsResponse_campaignSummaries,
    listCampaignsResponse_nextToken,
    listCampaignsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTFleetWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListCampaigns' smart constructor.
data ListCampaigns = ListCampaigns'
  { -- | The maximum number of items to return, between 1 and 100, inclusive.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A pagination token for the next set of results.
    --
    -- If the results of a search are large, only a portion of the results are
    -- returned, and a @nextToken@ pagination token is returned in the
    -- response. To retrieve the next set of results, reissue the search
    -- request and include the returned token. When all results have been
    -- returned, the response does not contain a pagination token value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Optional parameter to filter the results by the status of each created
    -- campaign in your account. The status can be one of: @CREATING@,
    -- @WAITING_FOR_APPROVAL@, @RUNNING@, or @SUSPENDED@.
    status :: Prelude.Maybe Prelude.Text
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
-- 'maxResults', 'listCampaigns_maxResults' - The maximum number of items to return, between 1 and 100, inclusive.
--
-- 'nextToken', 'listCampaigns_nextToken' - A pagination token for the next set of results.
--
-- If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next set of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
--
-- 'status', 'listCampaigns_status' - Optional parameter to filter the results by the status of each created
-- campaign in your account. The status can be one of: @CREATING@,
-- @WAITING_FOR_APPROVAL@, @RUNNING@, or @SUSPENDED@.
newListCampaigns ::
  ListCampaigns
newListCampaigns =
  ListCampaigns'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The maximum number of items to return, between 1 and 100, inclusive.
listCampaigns_maxResults :: Lens.Lens' ListCampaigns (Prelude.Maybe Prelude.Natural)
listCampaigns_maxResults = Lens.lens (\ListCampaigns' {maxResults} -> maxResults) (\s@ListCampaigns' {} a -> s {maxResults = a} :: ListCampaigns)

-- | A pagination token for the next set of results.
--
-- If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next set of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
listCampaigns_nextToken :: Lens.Lens' ListCampaigns (Prelude.Maybe Prelude.Text)
listCampaigns_nextToken = Lens.lens (\ListCampaigns' {nextToken} -> nextToken) (\s@ListCampaigns' {} a -> s {nextToken = a} :: ListCampaigns)

-- | Optional parameter to filter the results by the status of each created
-- campaign in your account. The status can be one of: @CREATING@,
-- @WAITING_FOR_APPROVAL@, @RUNNING@, or @SUSPENDED@.
listCampaigns_status :: Lens.Lens' ListCampaigns (Prelude.Maybe Prelude.Text)
listCampaigns_status = Lens.lens (\ListCampaigns' {status} -> status) (\s@ListCampaigns' {} a -> s {status = a} :: ListCampaigns)

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
            Lens.^? listCampaignsResponse_campaignSummaries
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
                            Data..?> "campaignSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCampaigns where
  hashWithSalt _salt ListCampaigns' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` status

instance Prelude.NFData ListCampaigns where
  rnf ListCampaigns' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf status

instance Data.ToHeaders ListCampaigns where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "IoTAutobahnControlPlane.ListCampaigns" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListCampaigns where
  toJSON ListCampaigns' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("status" Data..=) Prelude.<$> status
          ]
      )

instance Data.ToPath ListCampaigns where
  toPath = Prelude.const "/"

instance Data.ToQuery ListCampaigns where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListCampaignsResponse' smart constructor.
data ListCampaignsResponse = ListCampaignsResponse'
  { -- | A summary of information about each campaign.
    campaignSummaries :: Prelude.Maybe [CampaignSummary],
    -- | The token to retrieve the next set of results, or @null@ if there are no
    -- more results.
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
-- 'campaignSummaries', 'listCampaignsResponse_campaignSummaries' - A summary of information about each campaign.
--
-- 'nextToken', 'listCampaignsResponse_nextToken' - The token to retrieve the next set of results, or @null@ if there are no
-- more results.
--
-- 'httpStatus', 'listCampaignsResponse_httpStatus' - The response's http status code.
newListCampaignsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCampaignsResponse
newListCampaignsResponse pHttpStatus_ =
  ListCampaignsResponse'
    { campaignSummaries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A summary of information about each campaign.
listCampaignsResponse_campaignSummaries :: Lens.Lens' ListCampaignsResponse (Prelude.Maybe [CampaignSummary])
listCampaignsResponse_campaignSummaries = Lens.lens (\ListCampaignsResponse' {campaignSummaries} -> campaignSummaries) (\s@ListCampaignsResponse' {} a -> s {campaignSummaries = a} :: ListCampaignsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to retrieve the next set of results, or @null@ if there are no
-- more results.
listCampaignsResponse_nextToken :: Lens.Lens' ListCampaignsResponse (Prelude.Maybe Prelude.Text)
listCampaignsResponse_nextToken = Lens.lens (\ListCampaignsResponse' {nextToken} -> nextToken) (\s@ListCampaignsResponse' {} a -> s {nextToken = a} :: ListCampaignsResponse)

-- | The response's http status code.
listCampaignsResponse_httpStatus :: Lens.Lens' ListCampaignsResponse Prelude.Int
listCampaignsResponse_httpStatus = Lens.lens (\ListCampaignsResponse' {httpStatus} -> httpStatus) (\s@ListCampaignsResponse' {} a -> s {httpStatus = a} :: ListCampaignsResponse)

instance Prelude.NFData ListCampaignsResponse where
  rnf ListCampaignsResponse' {..} =
    Prelude.rnf campaignSummaries
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
