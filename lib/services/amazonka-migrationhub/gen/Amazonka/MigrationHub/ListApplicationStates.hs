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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
    listApplicationStates_applicationIds,
    listApplicationStates_maxResults,
    listApplicationStates_nextToken,

    -- * Destructuring the Response
    ListApplicationStatesResponse (..),
    newListApplicationStatesResponse,

    -- * Response Lenses
    listApplicationStatesResponse_applicationStateList,
    listApplicationStatesResponse_nextToken,
    listApplicationStatesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHub.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListApplicationStates' smart constructor.
data ListApplicationStates = ListApplicationStates'
  { -- | The configurationIds from the Application Discovery Service that
    -- uniquely identifies your applications.
    applicationIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Maximum number of results to be returned per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If a @NextToken@ was returned by a previous call, there are more results
    -- available. To retrieve the next page of results, make the call again
    -- using the returned token in @NextToken@.
    nextToken :: Prelude.Maybe Prelude.Text
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
-- 'applicationIds', 'listApplicationStates_applicationIds' - The configurationIds from the Application Discovery Service that
-- uniquely identifies your applications.
--
-- 'maxResults', 'listApplicationStates_maxResults' - Maximum number of results to be returned per page.
--
-- 'nextToken', 'listApplicationStates_nextToken' - If a @NextToken@ was returned by a previous call, there are more results
-- available. To retrieve the next page of results, make the call again
-- using the returned token in @NextToken@.
newListApplicationStates ::
  ListApplicationStates
newListApplicationStates =
  ListApplicationStates'
    { applicationIds =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The configurationIds from the Application Discovery Service that
-- uniquely identifies your applications.
listApplicationStates_applicationIds :: Lens.Lens' ListApplicationStates (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
listApplicationStates_applicationIds = Lens.lens (\ListApplicationStates' {applicationIds} -> applicationIds) (\s@ListApplicationStates' {} a -> s {applicationIds = a} :: ListApplicationStates) Prelude.. Lens.mapping Lens.coerced

-- | Maximum number of results to be returned per page.
listApplicationStates_maxResults :: Lens.Lens' ListApplicationStates (Prelude.Maybe Prelude.Natural)
listApplicationStates_maxResults = Lens.lens (\ListApplicationStates' {maxResults} -> maxResults) (\s@ListApplicationStates' {} a -> s {maxResults = a} :: ListApplicationStates)

-- | If a @NextToken@ was returned by a previous call, there are more results
-- available. To retrieve the next page of results, make the call again
-- using the returned token in @NextToken@.
listApplicationStates_nextToken :: Lens.Lens' ListApplicationStates (Prelude.Maybe Prelude.Text)
listApplicationStates_nextToken = Lens.lens (\ListApplicationStates' {nextToken} -> nextToken) (\s@ListApplicationStates' {} a -> s {nextToken = a} :: ListApplicationStates)

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
        Prelude.Just
          Prelude.$ rq
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
            Prelude.<$> ( x
                            Data..?> "ApplicationStateList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListApplicationStates where
  hashWithSalt _salt ListApplicationStates' {..} =
    _salt
      `Prelude.hashWithSalt` applicationIds
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListApplicationStates where
  rnf ListApplicationStates' {..} =
    Prelude.rnf applicationIds
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListApplicationStates where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSMigrationHub.ListApplicationStates" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListApplicationStates where
  toJSON ListApplicationStates' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ApplicationIds" Data..=)
              Prelude.<$> applicationIds,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListApplicationStates where
  toPath = Prelude.const "/"

instance Data.ToQuery ListApplicationStates where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListApplicationStatesResponse' smart constructor.
data ListApplicationStatesResponse = ListApplicationStatesResponse'
  { -- | A list of Applications that exist in Application Discovery Service.
    applicationStateList :: Prelude.Maybe [ApplicationState],
    -- | If a @NextToken@ was returned by a previous call, there are more results
    -- available. To retrieve the next page of results, make the call again
    -- using the returned token in @NextToken@.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'applicationStateList', 'listApplicationStatesResponse_applicationStateList' - A list of Applications that exist in Application Discovery Service.
--
-- 'nextToken', 'listApplicationStatesResponse_nextToken' - If a @NextToken@ was returned by a previous call, there are more results
-- available. To retrieve the next page of results, make the call again
-- using the returned token in @NextToken@.
--
-- 'httpStatus', 'listApplicationStatesResponse_httpStatus' - The response's http status code.
newListApplicationStatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListApplicationStatesResponse
newListApplicationStatesResponse pHttpStatus_ =
  ListApplicationStatesResponse'
    { applicationStateList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of Applications that exist in Application Discovery Service.
listApplicationStatesResponse_applicationStateList :: Lens.Lens' ListApplicationStatesResponse (Prelude.Maybe [ApplicationState])
listApplicationStatesResponse_applicationStateList = Lens.lens (\ListApplicationStatesResponse' {applicationStateList} -> applicationStateList) (\s@ListApplicationStatesResponse' {} a -> s {applicationStateList = a} :: ListApplicationStatesResponse) Prelude.. Lens.mapping Lens.coerced

-- | If a @NextToken@ was returned by a previous call, there are more results
-- available. To retrieve the next page of results, make the call again
-- using the returned token in @NextToken@.
listApplicationStatesResponse_nextToken :: Lens.Lens' ListApplicationStatesResponse (Prelude.Maybe Prelude.Text)
listApplicationStatesResponse_nextToken = Lens.lens (\ListApplicationStatesResponse' {nextToken} -> nextToken) (\s@ListApplicationStatesResponse' {} a -> s {nextToken = a} :: ListApplicationStatesResponse)

-- | The response's http status code.
listApplicationStatesResponse_httpStatus :: Lens.Lens' ListApplicationStatesResponse Prelude.Int
listApplicationStatesResponse_httpStatus = Lens.lens (\ListApplicationStatesResponse' {httpStatus} -> httpStatus) (\s@ListApplicationStatesResponse' {} a -> s {httpStatus = a} :: ListApplicationStatesResponse)

instance Prelude.NFData ListApplicationStatesResponse where
  rnf ListApplicationStatesResponse' {..} =
    Prelude.rnf applicationStateList
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
