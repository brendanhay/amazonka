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
-- Module      : Amazonka.M2.ListApplications
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the applications associated with a specific Amazon Web Services
-- account. You can provide the unique identifier of a specific runtime
-- environment in a query parameter to see all applications associated with
-- that environment.
--
-- This operation returns paginated results.
module Amazonka.M2.ListApplications
  ( -- * Creating a Request
    ListApplications (..),
    newListApplications,

    -- * Request Lenses
    listApplications_environmentId,
    listApplications_maxResults,
    listApplications_names,
    listApplications_nextToken,

    -- * Destructuring the Response
    ListApplicationsResponse (..),
    newListApplicationsResponse,

    -- * Response Lenses
    listApplicationsResponse_nextToken,
    listApplicationsResponse_httpStatus,
    listApplicationsResponse_applications,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.M2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListApplications' smart constructor.
data ListApplications = ListApplications'
  { -- | The unique identifier of the runtime environment where the applications
    -- are deployed.
    environmentId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of applications to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The names of the applications.
    names :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A pagination token to control the number of applications displayed in
    -- the list.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListApplications' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environmentId', 'listApplications_environmentId' - The unique identifier of the runtime environment where the applications
-- are deployed.
--
-- 'maxResults', 'listApplications_maxResults' - The maximum number of applications to return.
--
-- 'names', 'listApplications_names' - The names of the applications.
--
-- 'nextToken', 'listApplications_nextToken' - A pagination token to control the number of applications displayed in
-- the list.
newListApplications ::
  ListApplications
newListApplications =
  ListApplications'
    { environmentId = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      names = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The unique identifier of the runtime environment where the applications
-- are deployed.
listApplications_environmentId :: Lens.Lens' ListApplications (Prelude.Maybe Prelude.Text)
listApplications_environmentId = Lens.lens (\ListApplications' {environmentId} -> environmentId) (\s@ListApplications' {} a -> s {environmentId = a} :: ListApplications)

-- | The maximum number of applications to return.
listApplications_maxResults :: Lens.Lens' ListApplications (Prelude.Maybe Prelude.Natural)
listApplications_maxResults = Lens.lens (\ListApplications' {maxResults} -> maxResults) (\s@ListApplications' {} a -> s {maxResults = a} :: ListApplications)

-- | The names of the applications.
listApplications_names :: Lens.Lens' ListApplications (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
listApplications_names = Lens.lens (\ListApplications' {names} -> names) (\s@ListApplications' {} a -> s {names = a} :: ListApplications) Prelude.. Lens.mapping Lens.coerced

-- | A pagination token to control the number of applications displayed in
-- the list.
listApplications_nextToken :: Lens.Lens' ListApplications (Prelude.Maybe Prelude.Text)
listApplications_nextToken = Lens.lens (\ListApplications' {nextToken} -> nextToken) (\s@ListApplications' {} a -> s {nextToken = a} :: ListApplications)

instance Core.AWSPager ListApplications where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listApplicationsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        (rs Lens.^. listApplicationsResponse_applications) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listApplications_nextToken
          Lens..~ rs
          Lens.^? listApplicationsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListApplications where
  type
    AWSResponse ListApplications =
      ListApplicationsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListApplicationsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "applications" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListApplications where
  hashWithSalt _salt ListApplications' {..} =
    _salt
      `Prelude.hashWithSalt` environmentId
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` names
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListApplications where
  rnf ListApplications' {..} =
    Prelude.rnf environmentId
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf names
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListApplications where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListApplications where
  toPath = Prelude.const "/applications"

instance Data.ToQuery ListApplications where
  toQuery ListApplications' {..} =
    Prelude.mconcat
      [ "environmentId" Data.=: environmentId,
        "maxResults" Data.=: maxResults,
        "names"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> names),
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListApplicationsResponse' smart constructor.
data ListApplicationsResponse = ListApplicationsResponse'
  { -- | A pagination token that\'s returned when the response doesn\'t contain
    -- all applications.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Returns a list of summary details for all the applications in a runtime
    -- environment.
    applications :: [ApplicationSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListApplicationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listApplicationsResponse_nextToken' - A pagination token that\'s returned when the response doesn\'t contain
-- all applications.
--
-- 'httpStatus', 'listApplicationsResponse_httpStatus' - The response's http status code.
--
-- 'applications', 'listApplicationsResponse_applications' - Returns a list of summary details for all the applications in a runtime
-- environment.
newListApplicationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListApplicationsResponse
newListApplicationsResponse pHttpStatus_ =
  ListApplicationsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      applications = Prelude.mempty
    }

-- | A pagination token that\'s returned when the response doesn\'t contain
-- all applications.
listApplicationsResponse_nextToken :: Lens.Lens' ListApplicationsResponse (Prelude.Maybe Prelude.Text)
listApplicationsResponse_nextToken = Lens.lens (\ListApplicationsResponse' {nextToken} -> nextToken) (\s@ListApplicationsResponse' {} a -> s {nextToken = a} :: ListApplicationsResponse)

-- | The response's http status code.
listApplicationsResponse_httpStatus :: Lens.Lens' ListApplicationsResponse Prelude.Int
listApplicationsResponse_httpStatus = Lens.lens (\ListApplicationsResponse' {httpStatus} -> httpStatus) (\s@ListApplicationsResponse' {} a -> s {httpStatus = a} :: ListApplicationsResponse)

-- | Returns a list of summary details for all the applications in a runtime
-- environment.
listApplicationsResponse_applications :: Lens.Lens' ListApplicationsResponse [ApplicationSummary]
listApplicationsResponse_applications = Lens.lens (\ListApplicationsResponse' {applications} -> applications) (\s@ListApplicationsResponse' {} a -> s {applications = a} :: ListApplicationsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListApplicationsResponse where
  rnf ListApplicationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf applications
