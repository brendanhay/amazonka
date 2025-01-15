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
-- Module      : Amazonka.WorkMail.ListOrganizations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns summaries of the customer\'s organizations.
--
-- This operation returns paginated results.
module Amazonka.WorkMail.ListOrganizations
  ( -- * Creating a Request
    ListOrganizations (..),
    newListOrganizations,

    -- * Request Lenses
    listOrganizations_maxResults,
    listOrganizations_nextToken,

    -- * Destructuring the Response
    ListOrganizationsResponse (..),
    newListOrganizationsResponse,

    -- * Response Lenses
    listOrganizationsResponse_nextToken,
    listOrganizationsResponse_organizationSummaries,
    listOrganizationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkMail.Types

-- | /See:/ 'newListOrganizations' smart constructor.
data ListOrganizations = ListOrganizations'
  { -- | The maximum number of results to return in a single call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token to use to retrieve the next page of results. The first call
    -- does not contain any tokens.
    nextToken :: Prelude.Maybe Prelude.Text
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
-- 'maxResults', 'listOrganizations_maxResults' - The maximum number of results to return in a single call.
--
-- 'nextToken', 'listOrganizations_nextToken' - The token to use to retrieve the next page of results. The first call
-- does not contain any tokens.
newListOrganizations ::
  ListOrganizations
newListOrganizations =
  ListOrganizations'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to return in a single call.
listOrganizations_maxResults :: Lens.Lens' ListOrganizations (Prelude.Maybe Prelude.Natural)
listOrganizations_maxResults = Lens.lens (\ListOrganizations' {maxResults} -> maxResults) (\s@ListOrganizations' {} a -> s {maxResults = a} :: ListOrganizations)

-- | The token to use to retrieve the next page of results. The first call
-- does not contain any tokens.
listOrganizations_nextToken :: Lens.Lens' ListOrganizations (Prelude.Maybe Prelude.Text)
listOrganizations_nextToken = Lens.lens (\ListOrganizations' {nextToken} -> nextToken) (\s@ListOrganizations' {} a -> s {nextToken = a} :: ListOrganizations)

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListOrganizationsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "OrganizationSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListOrganizations where
  hashWithSalt _salt ListOrganizations' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListOrganizations where
  rnf ListOrganizations' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken

instance Data.ToHeaders ListOrganizations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkMailService.ListOrganizations" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListOrganizations where
  toJSON ListOrganizations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListOrganizations where
  toPath = Prelude.const "/"

instance Data.ToQuery ListOrganizations where
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
listOrganizationsResponse_organizationSummaries = Lens.lens (\ListOrganizationsResponse' {organizationSummaries} -> organizationSummaries) (\s@ListOrganizationsResponse' {} a -> s {organizationSummaries = a} :: ListOrganizationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listOrganizationsResponse_httpStatus :: Lens.Lens' ListOrganizationsResponse Prelude.Int
listOrganizationsResponse_httpStatus = Lens.lens (\ListOrganizationsResponse' {httpStatus} -> httpStatus) (\s@ListOrganizationsResponse' {} a -> s {httpStatus = a} :: ListOrganizationsResponse)

instance Prelude.NFData ListOrganizationsResponse where
  rnf ListOrganizationsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf organizationSummaries `Prelude.seq`
        Prelude.rnf httpStatus
