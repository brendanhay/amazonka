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
-- Module      : Amazonka.CustomerProfiles.ListIdentityResolutionJobs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of the Identity Resolution Jobs in your domain. The response
-- sorts the list by @JobStartTime@.
module Amazonka.CustomerProfiles.ListIdentityResolutionJobs
  ( -- * Creating a Request
    ListIdentityResolutionJobs (..),
    newListIdentityResolutionJobs,

    -- * Request Lenses
    listIdentityResolutionJobs_nextToken,
    listIdentityResolutionJobs_maxResults,
    listIdentityResolutionJobs_domainName,

    -- * Destructuring the Response
    ListIdentityResolutionJobsResponse (..),
    newListIdentityResolutionJobsResponse,

    -- * Response Lenses
    listIdentityResolutionJobsResponse_nextToken,
    listIdentityResolutionJobsResponse_identityResolutionJobsList,
    listIdentityResolutionJobsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListIdentityResolutionJobs' smart constructor.
data ListIdentityResolutionJobs = ListIdentityResolutionJobs'
  { -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The unique name of the domain.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListIdentityResolutionJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listIdentityResolutionJobs_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'maxResults', 'listIdentityResolutionJobs_maxResults' - The maximum number of results to return per page.
--
-- 'domainName', 'listIdentityResolutionJobs_domainName' - The unique name of the domain.
newListIdentityResolutionJobs ::
  -- | 'domainName'
  Prelude.Text ->
  ListIdentityResolutionJobs
newListIdentityResolutionJobs pDomainName_ =
  ListIdentityResolutionJobs'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      domainName = pDomainName_
    }

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listIdentityResolutionJobs_nextToken :: Lens.Lens' ListIdentityResolutionJobs (Prelude.Maybe Prelude.Text)
listIdentityResolutionJobs_nextToken = Lens.lens (\ListIdentityResolutionJobs' {nextToken} -> nextToken) (\s@ListIdentityResolutionJobs' {} a -> s {nextToken = a} :: ListIdentityResolutionJobs)

-- | The maximum number of results to return per page.
listIdentityResolutionJobs_maxResults :: Lens.Lens' ListIdentityResolutionJobs (Prelude.Maybe Prelude.Natural)
listIdentityResolutionJobs_maxResults = Lens.lens (\ListIdentityResolutionJobs' {maxResults} -> maxResults) (\s@ListIdentityResolutionJobs' {} a -> s {maxResults = a} :: ListIdentityResolutionJobs)

-- | The unique name of the domain.
listIdentityResolutionJobs_domainName :: Lens.Lens' ListIdentityResolutionJobs Prelude.Text
listIdentityResolutionJobs_domainName = Lens.lens (\ListIdentityResolutionJobs' {domainName} -> domainName) (\s@ListIdentityResolutionJobs' {} a -> s {domainName = a} :: ListIdentityResolutionJobs)

instance Core.AWSRequest ListIdentityResolutionJobs where
  type
    AWSResponse ListIdentityResolutionJobs =
      ListIdentityResolutionJobsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListIdentityResolutionJobsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "IdentityResolutionJobsList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListIdentityResolutionJobs where
  hashWithSalt _salt ListIdentityResolutionJobs' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData ListIdentityResolutionJobs where
  rnf ListIdentityResolutionJobs' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf domainName

instance Data.ToHeaders ListIdentityResolutionJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListIdentityResolutionJobs where
  toPath ListIdentityResolutionJobs' {..} =
    Prelude.mconcat
      [ "/domains/",
        Data.toBS domainName,
        "/identity-resolution-jobs"
      ]

instance Data.ToQuery ListIdentityResolutionJobs where
  toQuery ListIdentityResolutionJobs' {..} =
    Prelude.mconcat
      [ "next-token" Data.=: nextToken,
        "max-results" Data.=: maxResults
      ]

-- | /See:/ 'newListIdentityResolutionJobsResponse' smart constructor.
data ListIdentityResolutionJobsResponse = ListIdentityResolutionJobsResponse'
  { -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of Identity Resolution Jobs.
    identityResolutionJobsList :: Prelude.Maybe [IdentityResolutionJob],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListIdentityResolutionJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listIdentityResolutionJobsResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'identityResolutionJobsList', 'listIdentityResolutionJobsResponse_identityResolutionJobsList' - A list of Identity Resolution Jobs.
--
-- 'httpStatus', 'listIdentityResolutionJobsResponse_httpStatus' - The response's http status code.
newListIdentityResolutionJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListIdentityResolutionJobsResponse
newListIdentityResolutionJobsResponse pHttpStatus_ =
  ListIdentityResolutionJobsResponse'
    { nextToken =
        Prelude.Nothing,
      identityResolutionJobsList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are additional results, this is the token for the next set of
-- results.
listIdentityResolutionJobsResponse_nextToken :: Lens.Lens' ListIdentityResolutionJobsResponse (Prelude.Maybe Prelude.Text)
listIdentityResolutionJobsResponse_nextToken = Lens.lens (\ListIdentityResolutionJobsResponse' {nextToken} -> nextToken) (\s@ListIdentityResolutionJobsResponse' {} a -> s {nextToken = a} :: ListIdentityResolutionJobsResponse)

-- | A list of Identity Resolution Jobs.
listIdentityResolutionJobsResponse_identityResolutionJobsList :: Lens.Lens' ListIdentityResolutionJobsResponse (Prelude.Maybe [IdentityResolutionJob])
listIdentityResolutionJobsResponse_identityResolutionJobsList = Lens.lens (\ListIdentityResolutionJobsResponse' {identityResolutionJobsList} -> identityResolutionJobsList) (\s@ListIdentityResolutionJobsResponse' {} a -> s {identityResolutionJobsList = a} :: ListIdentityResolutionJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listIdentityResolutionJobsResponse_httpStatus :: Lens.Lens' ListIdentityResolutionJobsResponse Prelude.Int
listIdentityResolutionJobsResponse_httpStatus = Lens.lens (\ListIdentityResolutionJobsResponse' {httpStatus} -> httpStatus) (\s@ListIdentityResolutionJobsResponse' {} a -> s {httpStatus = a} :: ListIdentityResolutionJobsResponse)

instance
  Prelude.NFData
    ListIdentityResolutionJobsResponse
  where
  rnf ListIdentityResolutionJobsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf identityResolutionJobsList
      `Prelude.seq` Prelude.rnf httpStatus
