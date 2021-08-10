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
-- Module      : Network.AWS.WorkMail.ListOrganizations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns summaries of the customer\'s organizations.
--
-- This operation returns paginated results.
module Network.AWS.WorkMail.ListOrganizations
  ( -- * Creating a Request
    ListOrganizations (..),
    newListOrganizations,

    -- * Request Lenses
    listOrganizations_nextToken,
    listOrganizations_maxResults,

    -- * Destructuring the Response
    ListOrganizationsResponse (..),
    newListOrganizationsResponse,

    -- * Response Lenses
    listOrganizationsResponse_nextToken,
    listOrganizationsResponse_organizationSummaries,
    listOrganizationsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'newListOrganizations' smart constructor.
data ListOrganizations = ListOrganizations'
  { -- | The token to use to retrieve the next page of results. The first call
    -- does not contain any tokens.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return in a single call.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListOrganizations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listOrganizations_nextToken' - The token to use to retrieve the next page of results. The first call
-- does not contain any tokens.
--
-- 'maxResults', 'listOrganizations_maxResults' - The maximum number of results to return in a single call.
newListOrganizations ::
  ListOrganizations
newListOrganizations =
  ListOrganizations'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token to use to retrieve the next page of results. The first call
-- does not contain any tokens.
listOrganizations_nextToken :: Lens.Lens' ListOrganizations (Prelude.Maybe Prelude.Text)
listOrganizations_nextToken = Lens.lens (\ListOrganizations' {nextToken} -> nextToken) (\s@ListOrganizations' {} a -> s {nextToken = a} :: ListOrganizations)

-- | The maximum number of results to return in a single call.
listOrganizations_maxResults :: Lens.Lens' ListOrganizations (Prelude.Maybe Prelude.Natural)
listOrganizations_maxResults = Lens.lens (\ListOrganizations' {maxResults} -> maxResults) (\s@ListOrganizations' {} a -> s {maxResults = a} :: ListOrganizations)

instance Core.AWSPager ListOrganizations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listOrganizationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listOrganizationsResponse_organizationSummaries
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listOrganizations_nextToken
          Lens..~ rs
          Lens.^? listOrganizationsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListOrganizations where
  type
    AWSResponse ListOrganizations =
      ListOrganizationsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListOrganizationsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "OrganizationSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListOrganizations

instance Prelude.NFData ListOrganizations

instance Core.ToHeaders ListOrganizations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkMailService.ListOrganizations" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListOrganizations where
  toJSON ListOrganizations' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListOrganizations where
  toPath = Prelude.const "/"

instance Core.ToQuery ListOrganizations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListOrganizationsResponse' smart constructor.
data ListOrganizationsResponse = ListOrganizationsResponse'
  { -- | The token to use to retrieve the next page of results. The value is
    -- \"null\" when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The overview of owned organizations presented as a list of organization
    -- summaries.
    organizationSummaries :: Prelude.Maybe [OrganizationSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListOrganizationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listOrganizationsResponse_nextToken' - The token to use to retrieve the next page of results. The value is
-- \"null\" when there are no more results to return.
--
-- 'organizationSummaries', 'listOrganizationsResponse_organizationSummaries' - The overview of owned organizations presented as a list of organization
-- summaries.
--
-- 'httpStatus', 'listOrganizationsResponse_httpStatus' - The response's http status code.
newListOrganizationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListOrganizationsResponse
newListOrganizationsResponse pHttpStatus_ =
  ListOrganizationsResponse'
    { nextToken =
        Prelude.Nothing,
      organizationSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. The value is
-- \"null\" when there are no more results to return.
listOrganizationsResponse_nextToken :: Lens.Lens' ListOrganizationsResponse (Prelude.Maybe Prelude.Text)
listOrganizationsResponse_nextToken = Lens.lens (\ListOrganizationsResponse' {nextToken} -> nextToken) (\s@ListOrganizationsResponse' {} a -> s {nextToken = a} :: ListOrganizationsResponse)

-- | The overview of owned organizations presented as a list of organization
-- summaries.
listOrganizationsResponse_organizationSummaries :: Lens.Lens' ListOrganizationsResponse (Prelude.Maybe [OrganizationSummary])
listOrganizationsResponse_organizationSummaries = Lens.lens (\ListOrganizationsResponse' {organizationSummaries} -> organizationSummaries) (\s@ListOrganizationsResponse' {} a -> s {organizationSummaries = a} :: ListOrganizationsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listOrganizationsResponse_httpStatus :: Lens.Lens' ListOrganizationsResponse Prelude.Int
listOrganizationsResponse_httpStatus = Lens.lens (\ListOrganizationsResponse' {httpStatus} -> httpStatus) (\s@ListOrganizationsResponse' {} a -> s {httpStatus = a} :: ListOrganizationsResponse)

instance Prelude.NFData ListOrganizationsResponse
