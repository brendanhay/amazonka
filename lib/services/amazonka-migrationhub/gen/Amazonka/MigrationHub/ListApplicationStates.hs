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
-- Module      : Amazonka.MigrationHub.ListApplicationStates
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the migration statuses for your applications. If you use the
-- optional @ApplicationIds@ parameter, only the migration statuses for
-- those applications will be returned.
--
-- This operation returns paginated results.
module Amazonka.MigrationHub.ListApplicationStates
  ( -- * Creating a Request
    ListApplicationStates (..),
    newListApplicationStates,

    -- * Request Lenses
    listApplicationStates_nextToken,
    listApplicationStates_applicationIds,
    listApplicationStates_maxResults,

    -- * Destructuring the Response
    ListApplicationStatesResponse (..),
    newListApplicationStatesResponse,

    -- * Response Lenses
    listApplicationStatesResponse_nextToken,
    listApplicationStatesResponse_applicationStateList,
    listApplicationStatesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MigrationHub.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListApplicationStates' smart constructor.
data ListApplicationStates = ListApplicationStates'
  { -- | If a @NextToken@ was returned by a previous call, there are more results
    -- available. To retrieve the next page of results, make the call again
    -- using the returned token in @NextToken@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The configurationIds from the Application Discovery Service that
    -- uniquely identifies your applications.
    applicationIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Maximum number of results to be returned per page.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListApplicationStates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listApplicationStates_nextToken' - If a @NextToken@ was returned by a previous call, there are more results
-- available. To retrieve the next page of results, make the call again
-- using the returned token in @NextToken@.
--
-- 'applicationIds', 'listApplicationStates_applicationIds' - The configurationIds from the Application Discovery Service that
-- uniquely identifies your applications.
--
-- 'maxResults', 'listApplicationStates_maxResults' - Maximum number of results to be returned per page.
newListApplicationStates ::
  ListApplicationStates
newListApplicationStates =
  ListApplicationStates'
    { nextToken = Prelude.Nothing,
      applicationIds = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | If a @NextToken@ was returned by a previous call, there are more results
-- available. To retrieve the next page of results, make the call again
-- using the returned token in @NextToken@.
listApplicationStates_nextToken :: Lens.Lens' ListApplicationStates (Prelude.Maybe Prelude.Text)
listApplicationStates_nextToken = Lens.lens (\ListApplicationStates' {nextToken} -> nextToken) (\s@ListApplicationStates' {} a -> s {nextToken = a} :: ListApplicationStates)

-- | The configurationIds from the Application Discovery Service that
-- uniquely identifies your applications.
listApplicationStates_applicationIds :: Lens.Lens' ListApplicationStates (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
listApplicationStates_applicationIds = Lens.lens (\ListApplicationStates' {applicationIds} -> applicationIds) (\s@ListApplicationStates' {} a -> s {applicationIds = a} :: ListApplicationStates) Prelude.. Lens.mapping Lens.coerced

-- | Maximum number of results to be returned per page.
listApplicationStates_maxResults :: Lens.Lens' ListApplicationStates (Prelude.Maybe Prelude.Natural)
listApplicationStates_maxResults = Lens.lens (\ListApplicationStates' {maxResults} -> maxResults) (\s@ListApplicationStates' {} a -> s {maxResults = a} :: ListApplicationStates)

instance Core.AWSPager ListApplicationStates where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listApplicationStatesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listApplicationStatesResponse_applicationStateList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listApplicationStates_nextToken
          Lens..~ rs
          Lens.^? listApplicationStatesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListApplicationStates where
  type
    AWSResponse ListApplicationStates =
      ListApplicationStatesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListApplicationStatesResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "ApplicationStateList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListApplicationStates where
  hashWithSalt _salt ListApplicationStates' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` applicationIds
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListApplicationStates where
  rnf ListApplicationStates' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf applicationIds
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListApplicationStates where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSMigrationHub.ListApplicationStates" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListApplicationStates where
  toJSON ListApplicationStates' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("ApplicationIds" Core..=)
              Prelude.<$> applicationIds,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListApplicationStates where
  toPath = Prelude.const "/"

instance Core.ToQuery ListApplicationStates where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListApplicationStatesResponse' smart constructor.
data ListApplicationStatesResponse = ListApplicationStatesResponse'
  { -- | If a @NextToken@ was returned by a previous call, there are more results
    -- available. To retrieve the next page of results, make the call again
    -- using the returned token in @NextToken@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of Applications that exist in Application Discovery Service.
    applicationStateList :: Prelude.Maybe [ApplicationState],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListApplicationStatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listApplicationStatesResponse_nextToken' - If a @NextToken@ was returned by a previous call, there are more results
-- available. To retrieve the next page of results, make the call again
-- using the returned token in @NextToken@.
--
-- 'applicationStateList', 'listApplicationStatesResponse_applicationStateList' - A list of Applications that exist in Application Discovery Service.
--
-- 'httpStatus', 'listApplicationStatesResponse_httpStatus' - The response's http status code.
newListApplicationStatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListApplicationStatesResponse
newListApplicationStatesResponse pHttpStatus_ =
  ListApplicationStatesResponse'
    { nextToken =
        Prelude.Nothing,
      applicationStateList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If a @NextToken@ was returned by a previous call, there are more results
-- available. To retrieve the next page of results, make the call again
-- using the returned token in @NextToken@.
listApplicationStatesResponse_nextToken :: Lens.Lens' ListApplicationStatesResponse (Prelude.Maybe Prelude.Text)
listApplicationStatesResponse_nextToken = Lens.lens (\ListApplicationStatesResponse' {nextToken} -> nextToken) (\s@ListApplicationStatesResponse' {} a -> s {nextToken = a} :: ListApplicationStatesResponse)

-- | A list of Applications that exist in Application Discovery Service.
listApplicationStatesResponse_applicationStateList :: Lens.Lens' ListApplicationStatesResponse (Prelude.Maybe [ApplicationState])
listApplicationStatesResponse_applicationStateList = Lens.lens (\ListApplicationStatesResponse' {applicationStateList} -> applicationStateList) (\s@ListApplicationStatesResponse' {} a -> s {applicationStateList = a} :: ListApplicationStatesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listApplicationStatesResponse_httpStatus :: Lens.Lens' ListApplicationStatesResponse Prelude.Int
listApplicationStatesResponse_httpStatus = Lens.lens (\ListApplicationStatesResponse' {httpStatus} -> httpStatus) (\s@ListApplicationStatesResponse' {} a -> s {httpStatus = a} :: ListApplicationStatesResponse)

instance Prelude.NFData ListApplicationStatesResponse where
  rnf ListApplicationStatesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf applicationStateList
      `Prelude.seq` Prelude.rnf httpStatus
