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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
    listCampaigns_maxResults,
    listCampaigns_nextToken,
    listCampaigns_solutionArn,

    -- * Destructuring the Response
    ListCampaignsResponse (..),
    newListCampaignsResponse,

    -- * Response Lenses
    listCampaignsResponse_campaigns,
    listCampaignsResponse_nextToken,
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
  { -- | The maximum number of campaigns to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token returned from the previous call to
    -- <https://docs.aws.amazon.com/personalize/latest/dg/API_ListCampaigns.html ListCampaigns>
    -- for getting the next set of campaigns (if they exist).
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the solution to list the campaigns
    -- for. When a solution is not specified, all the campaigns associated with
    -- the account are listed.
    solutionArn :: Prelude.Maybe Prelude.Text
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
-- 'maxResults', 'listCampaigns_maxResults' - The maximum number of campaigns to return.
--
-- 'nextToken', 'listCampaigns_nextToken' - A token returned from the previous call to
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_ListCampaigns.html ListCampaigns>
-- for getting the next set of campaigns (if they exist).
--
-- 'solutionArn', 'listCampaigns_solutionArn' - The Amazon Resource Name (ARN) of the solution to list the campaigns
-- for. When a solution is not specified, all the campaigns associated with
-- the account are listed.
newListCampaigns ::
  ListCampaigns
newListCampaigns =
  ListCampaigns'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      solutionArn = Prelude.Nothing
    }

-- | The maximum number of campaigns to return.
listCampaigns_maxResults :: Lens.Lens' ListCampaigns (Prelude.Maybe Prelude.Natural)
listCampaigns_maxResults = Lens.lens (\ListCampaigns' {maxResults} -> maxResults) (\s@ListCampaigns' {} a -> s {maxResults = a} :: ListCampaigns)

-- | A token returned from the previous call to
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_ListCampaigns.html ListCampaigns>
-- for getting the next set of campaigns (if they exist).
listCampaigns_nextToken :: Lens.Lens' ListCampaigns (Prelude.Maybe Prelude.Text)
listCampaigns_nextToken = Lens.lens (\ListCampaigns' {nextToken} -> nextToken) (\s@ListCampaigns' {} a -> s {nextToken = a} :: ListCampaigns)

-- | The Amazon Resource Name (ARN) of the solution to list the campaigns
-- for. When a solution is not specified, all the campaigns associated with
-- the account are listed.
listCampaigns_solutionArn :: Lens.Lens' ListCampaigns (Prelude.Maybe Prelude.Text)
listCampaigns_solutionArn = Lens.lens (\ListCampaigns' {solutionArn} -> solutionArn) (\s@ListCampaigns' {} a -> s {solutionArn = a} :: ListCampaigns)

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
            Lens.^? listCampaignsResponse_campaigns
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
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
            Prelude.<$> (x Data..?> "campaigns" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCampaigns where
  hashWithSalt _salt ListCampaigns' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` solutionArn

instance Prelude.NFData ListCampaigns where
  rnf ListCampaigns' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf solutionArn

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
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("solutionArn" Data..=) Prelude.<$> solutionArn
          ]
      )

instance Data.ToPath ListCampaigns where
  toPath = Prelude.const "/"

instance Data.ToQuery ListCampaigns where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListCampaignsResponse' smart constructor.
data ListCampaignsResponse = ListCampaignsResponse'
  { -- | A list of the campaigns.
    campaigns :: Prelude.Maybe [CampaignSummary],
    -- | A token for getting the next set of campaigns (if they exist).
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
-- 'campaigns', 'listCampaignsResponse_campaigns' - A list of the campaigns.
--
-- 'nextToken', 'listCampaignsResponse_nextToken' - A token for getting the next set of campaigns (if they exist).
--
-- 'httpStatus', 'listCampaignsResponse_httpStatus' - The response's http status code.
newListCampaignsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCampaignsResponse
newListCampaignsResponse pHttpStatus_ =
  ListCampaignsResponse'
    { campaigns = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of the campaigns.
listCampaignsResponse_campaigns :: Lens.Lens' ListCampaignsResponse (Prelude.Maybe [CampaignSummary])
listCampaignsResponse_campaigns = Lens.lens (\ListCampaignsResponse' {campaigns} -> campaigns) (\s@ListCampaignsResponse' {} a -> s {campaigns = a} :: ListCampaignsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token for getting the next set of campaigns (if they exist).
listCampaignsResponse_nextToken :: Lens.Lens' ListCampaignsResponse (Prelude.Maybe Prelude.Text)
listCampaignsResponse_nextToken = Lens.lens (\ListCampaignsResponse' {nextToken} -> nextToken) (\s@ListCampaignsResponse' {} a -> s {nextToken = a} :: ListCampaignsResponse)

-- | The response's http status code.
listCampaignsResponse_httpStatus :: Lens.Lens' ListCampaignsResponse Prelude.Int
listCampaignsResponse_httpStatus = Lens.lens (\ListCampaignsResponse' {httpStatus} -> httpStatus) (\s@ListCampaignsResponse' {} a -> s {httpStatus = a} :: ListCampaignsResponse)

instance Prelude.NFData ListCampaignsResponse where
  rnf ListCampaignsResponse' {..} =
    Prelude.rnf campaigns `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
