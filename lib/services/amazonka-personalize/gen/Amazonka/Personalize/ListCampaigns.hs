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
-- Module      : Amazonka.Personalize.ListCampaigns
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of campaigns that use the given solution. When a solution
-- is not specified, all the campaigns associated with the account are
-- listed. The response provides the properties for each campaign,
-- including the Amazon Resource Name (ARN). For more information on
-- campaigns, see
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_CreateCampaign.html CreateCampaign>.
--
-- This operation returns paginated results.
module Amazonka.Personalize.ListCampaigns
  ( -- * Creating a Request
    ListCampaigns (..),
    newListCampaigns,

    -- * Request Lenses
    listCampaigns_solutionArn,
    listCampaigns_nextToken,
    listCampaigns_maxResults,

    -- * Destructuring the Response
    ListCampaignsResponse (..),
    newListCampaignsResponse,

    -- * Response Lenses
    listCampaignsResponse_nextToken,
    listCampaignsResponse_campaigns,
    listCampaignsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListCampaigns' smart constructor.
data ListCampaigns = ListCampaigns'
  { -- | The Amazon Resource Name (ARN) of the solution to list the campaigns
    -- for. When a solution is not specified, all the campaigns associated with
    -- the account are listed.
    solutionArn :: Prelude.Maybe Prelude.Text,
    -- | A token returned from the previous call to
    -- <https://docs.aws.amazon.com/personalize/latest/dg/API_ListCampaigns.html ListCampaigns>
    -- for getting the next set of campaigns (if they exist).
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of campaigns to return.
    maxResults :: Prelude.Maybe Prelude.Natural
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
-- 'solutionArn', 'listCampaigns_solutionArn' - The Amazon Resource Name (ARN) of the solution to list the campaigns
-- for. When a solution is not specified, all the campaigns associated with
-- the account are listed.
--
-- 'nextToken', 'listCampaigns_nextToken' - A token returned from the previous call to
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_ListCampaigns.html ListCampaigns>
-- for getting the next set of campaigns (if they exist).
--
-- 'maxResults', 'listCampaigns_maxResults' - The maximum number of campaigns to return.
newListCampaigns ::
  ListCampaigns
newListCampaigns =
  ListCampaigns'
    { solutionArn = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the solution to list the campaigns
-- for. When a solution is not specified, all the campaigns associated with
-- the account are listed.
listCampaigns_solutionArn :: Lens.Lens' ListCampaigns (Prelude.Maybe Prelude.Text)
listCampaigns_solutionArn = Lens.lens (\ListCampaigns' {solutionArn} -> solutionArn) (\s@ListCampaigns' {} a -> s {solutionArn = a} :: ListCampaigns)

-- | A token returned from the previous call to
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_ListCampaigns.html ListCampaigns>
-- for getting the next set of campaigns (if they exist).
listCampaigns_nextToken :: Lens.Lens' ListCampaigns (Prelude.Maybe Prelude.Text)
listCampaigns_nextToken = Lens.lens (\ListCampaigns' {nextToken} -> nextToken) (\s@ListCampaigns' {} a -> s {nextToken = a} :: ListCampaigns)

-- | The maximum number of campaigns to return.
listCampaigns_maxResults :: Lens.Lens' ListCampaigns (Prelude.Maybe Prelude.Natural)
listCampaigns_maxResults = Lens.lens (\ListCampaigns' {maxResults} -> maxResults) (\s@ListCampaigns' {} a -> s {maxResults = a} :: ListCampaigns)

instance Core.AWSPager ListCampaigns where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listCampaignsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listCampaignsResponse_campaigns Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listCampaigns_nextToken
          Lens..~ rs
          Lens.^? listCampaignsResponse_nextToken Prelude.. Lens._Just

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
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "campaigns" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCampaigns where
  hashWithSalt _salt ListCampaigns' {..} =
    _salt `Prelude.hashWithSalt` solutionArn
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListCampaigns where
  rnf ListCampaigns' {..} =
    Prelude.rnf solutionArn
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListCampaigns where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonPersonalize.ListCampaigns" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListCampaigns where
  toJSON ListCampaigns' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("solutionArn" Data..=) Prelude.<$> solutionArn,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("maxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance Data.ToPath ListCampaigns where
  toPath = Prelude.const "/"

instance Data.ToQuery ListCampaigns where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListCampaignsResponse' smart constructor.
data ListCampaignsResponse = ListCampaignsResponse'
  { -- | A token for getting the next set of campaigns (if they exist).
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of the campaigns.
    campaigns :: Prelude.Maybe [CampaignSummary],
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
-- 'nextToken', 'listCampaignsResponse_nextToken' - A token for getting the next set of campaigns (if they exist).
--
-- 'campaigns', 'listCampaignsResponse_campaigns' - A list of the campaigns.
--
-- 'httpStatus', 'listCampaignsResponse_httpStatus' - The response's http status code.
newListCampaignsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCampaignsResponse
newListCampaignsResponse pHttpStatus_ =
  ListCampaignsResponse'
    { nextToken = Prelude.Nothing,
      campaigns = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token for getting the next set of campaigns (if they exist).
listCampaignsResponse_nextToken :: Lens.Lens' ListCampaignsResponse (Prelude.Maybe Prelude.Text)
listCampaignsResponse_nextToken = Lens.lens (\ListCampaignsResponse' {nextToken} -> nextToken) (\s@ListCampaignsResponse' {} a -> s {nextToken = a} :: ListCampaignsResponse)

-- | A list of the campaigns.
listCampaignsResponse_campaigns :: Lens.Lens' ListCampaignsResponse (Prelude.Maybe [CampaignSummary])
listCampaignsResponse_campaigns = Lens.lens (\ListCampaignsResponse' {campaigns} -> campaigns) (\s@ListCampaignsResponse' {} a -> s {campaigns = a} :: ListCampaignsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listCampaignsResponse_httpStatus :: Lens.Lens' ListCampaignsResponse Prelude.Int
listCampaignsResponse_httpStatus = Lens.lens (\ListCampaignsResponse' {httpStatus} -> httpStatus) (\s@ListCampaignsResponse' {} a -> s {httpStatus = a} :: ListCampaignsResponse)

instance Prelude.NFData ListCampaignsResponse where
  rnf ListCampaignsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf campaigns
      `Prelude.seq` Prelude.rnf httpStatus
