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
-- Module      : Amazonka.CodeGuruReviewer.ListCodeReviews
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the code reviews that the customer has created in the past 90
-- days.
module Amazonka.CodeGuruReviewer.ListCodeReviews
  ( -- * Creating a Request
    ListCodeReviews (..),
    newListCodeReviews,

    -- * Request Lenses
    listCodeReviews_maxResults,
    listCodeReviews_nextToken,
    listCodeReviews_providerTypes,
    listCodeReviews_repositoryNames,
    listCodeReviews_states,
    listCodeReviews_type,

    -- * Destructuring the Response
    ListCodeReviewsResponse (..),
    newListCodeReviewsResponse,

    -- * Response Lenses
    listCodeReviewsResponse_codeReviewSummaries,
    listCodeReviewsResponse_nextToken,
    listCodeReviewsResponse_httpStatus,
  )
where

import Amazonka.CodeGuruReviewer.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListCodeReviews' smart constructor.
data ListCodeReviews = ListCodeReviews'
  { -- | The maximum number of results that are returned per call. The default is
    -- 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If @nextToken@ is returned, there are more results available. The value
    -- of @nextToken@ is a unique pagination token for each page. Make the call
    -- again using the returned token to retrieve the next page. Keep all other
    -- arguments unchanged.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | List of provider types for filtering that needs to be applied before
    -- displaying the result. For example, @providerTypes=[GitHub]@ lists code
    -- reviews from GitHub.
    providerTypes :: Prelude.Maybe (Prelude.NonEmpty ProviderType),
    -- | List of repository names for filtering that needs to be applied before
    -- displaying the result.
    repositoryNames :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | List of states for filtering that needs to be applied before displaying
    -- the result. For example, @states=[Pending]@ lists code reviews in the
    -- Pending state.
    --
    -- The valid code review states are:
    --
    -- -   @Completed@: The code review is complete.
    --
    -- -   @Pending@: The code review started and has not completed or failed.
    --
    -- -   @Failed@: The code review failed.
    --
    -- -   @Deleting@: The code review is being deleted.
    states :: Prelude.Maybe (Prelude.NonEmpty JobState),
    -- | The type of code reviews to list in the response.
    type' :: Type
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCodeReviews' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listCodeReviews_maxResults' - The maximum number of results that are returned per call. The default is
-- 100.
--
-- 'nextToken', 'listCodeReviews_nextToken' - If @nextToken@ is returned, there are more results available. The value
-- of @nextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged.
--
-- 'providerTypes', 'listCodeReviews_providerTypes' - List of provider types for filtering that needs to be applied before
-- displaying the result. For example, @providerTypes=[GitHub]@ lists code
-- reviews from GitHub.
--
-- 'repositoryNames', 'listCodeReviews_repositoryNames' - List of repository names for filtering that needs to be applied before
-- displaying the result.
--
-- 'states', 'listCodeReviews_states' - List of states for filtering that needs to be applied before displaying
-- the result. For example, @states=[Pending]@ lists code reviews in the
-- Pending state.
--
-- The valid code review states are:
--
-- -   @Completed@: The code review is complete.
--
-- -   @Pending@: The code review started and has not completed or failed.
--
-- -   @Failed@: The code review failed.
--
-- -   @Deleting@: The code review is being deleted.
--
-- 'type'', 'listCodeReviews_type' - The type of code reviews to list in the response.
newListCodeReviews ::
  -- | 'type''
  Type ->
  ListCodeReviews
newListCodeReviews pType_ =
  ListCodeReviews'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      providerTypes = Prelude.Nothing,
      repositoryNames = Prelude.Nothing,
      states = Prelude.Nothing,
      type' = pType_
    }

-- | The maximum number of results that are returned per call. The default is
-- 100.
listCodeReviews_maxResults :: Lens.Lens' ListCodeReviews (Prelude.Maybe Prelude.Natural)
listCodeReviews_maxResults = Lens.lens (\ListCodeReviews' {maxResults} -> maxResults) (\s@ListCodeReviews' {} a -> s {maxResults = a} :: ListCodeReviews)

-- | If @nextToken@ is returned, there are more results available. The value
-- of @nextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged.
listCodeReviews_nextToken :: Lens.Lens' ListCodeReviews (Prelude.Maybe Prelude.Text)
listCodeReviews_nextToken = Lens.lens (\ListCodeReviews' {nextToken} -> nextToken) (\s@ListCodeReviews' {} a -> s {nextToken = a} :: ListCodeReviews)

-- | List of provider types for filtering that needs to be applied before
-- displaying the result. For example, @providerTypes=[GitHub]@ lists code
-- reviews from GitHub.
listCodeReviews_providerTypes :: Lens.Lens' ListCodeReviews (Prelude.Maybe (Prelude.NonEmpty ProviderType))
listCodeReviews_providerTypes = Lens.lens (\ListCodeReviews' {providerTypes} -> providerTypes) (\s@ListCodeReviews' {} a -> s {providerTypes = a} :: ListCodeReviews) Prelude.. Lens.mapping Lens.coerced

-- | List of repository names for filtering that needs to be applied before
-- displaying the result.
listCodeReviews_repositoryNames :: Lens.Lens' ListCodeReviews (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
listCodeReviews_repositoryNames = Lens.lens (\ListCodeReviews' {repositoryNames} -> repositoryNames) (\s@ListCodeReviews' {} a -> s {repositoryNames = a} :: ListCodeReviews) Prelude.. Lens.mapping Lens.coerced

-- | List of states for filtering that needs to be applied before displaying
-- the result. For example, @states=[Pending]@ lists code reviews in the
-- Pending state.
--
-- The valid code review states are:
--
-- -   @Completed@: The code review is complete.
--
-- -   @Pending@: The code review started and has not completed or failed.
--
-- -   @Failed@: The code review failed.
--
-- -   @Deleting@: The code review is being deleted.
listCodeReviews_states :: Lens.Lens' ListCodeReviews (Prelude.Maybe (Prelude.NonEmpty JobState))
listCodeReviews_states = Lens.lens (\ListCodeReviews' {states} -> states) (\s@ListCodeReviews' {} a -> s {states = a} :: ListCodeReviews) Prelude.. Lens.mapping Lens.coerced

-- | The type of code reviews to list in the response.
listCodeReviews_type :: Lens.Lens' ListCodeReviews Type
listCodeReviews_type = Lens.lens (\ListCodeReviews' {type'} -> type') (\s@ListCodeReviews' {} a -> s {type' = a} :: ListCodeReviews)

instance Core.AWSRequest ListCodeReviews where
  type
    AWSResponse ListCodeReviews =
      ListCodeReviewsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCodeReviewsResponse'
            Prelude.<$> ( x
                            Data..?> "CodeReviewSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCodeReviews where
  hashWithSalt _salt ListCodeReviews' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` providerTypes
      `Prelude.hashWithSalt` repositoryNames
      `Prelude.hashWithSalt` states
      `Prelude.hashWithSalt` type'

instance Prelude.NFData ListCodeReviews where
  rnf ListCodeReviews' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf providerTypes
      `Prelude.seq` Prelude.rnf repositoryNames
      `Prelude.seq` Prelude.rnf states
      `Prelude.seq` Prelude.rnf type'

instance Data.ToHeaders ListCodeReviews where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListCodeReviews where
  toPath = Prelude.const "/codereviews"

instance Data.ToQuery ListCodeReviews where
  toQuery ListCodeReviews' {..} =
    Prelude.mconcat
      [ "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken,
        "ProviderTypes"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> providerTypes
            ),
        "RepositoryNames"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> repositoryNames
            ),
        "States"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> states),
        "Type" Data.=: type'
      ]

-- | /See:/ 'newListCodeReviewsResponse' smart constructor.
data ListCodeReviewsResponse = ListCodeReviewsResponse'
  { -- | A list of code reviews that meet the criteria of the request.
    codeReviewSummaries :: Prelude.Maybe [CodeReviewSummary],
    -- | Pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCodeReviewsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'codeReviewSummaries', 'listCodeReviewsResponse_codeReviewSummaries' - A list of code reviews that meet the criteria of the request.
--
-- 'nextToken', 'listCodeReviewsResponse_nextToken' - Pagination token.
--
-- 'httpStatus', 'listCodeReviewsResponse_httpStatus' - The response's http status code.
newListCodeReviewsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCodeReviewsResponse
newListCodeReviewsResponse pHttpStatus_ =
  ListCodeReviewsResponse'
    { codeReviewSummaries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of code reviews that meet the criteria of the request.
listCodeReviewsResponse_codeReviewSummaries :: Lens.Lens' ListCodeReviewsResponse (Prelude.Maybe [CodeReviewSummary])
listCodeReviewsResponse_codeReviewSummaries = Lens.lens (\ListCodeReviewsResponse' {codeReviewSummaries} -> codeReviewSummaries) (\s@ListCodeReviewsResponse' {} a -> s {codeReviewSummaries = a} :: ListCodeReviewsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Pagination token.
listCodeReviewsResponse_nextToken :: Lens.Lens' ListCodeReviewsResponse (Prelude.Maybe Prelude.Text)
listCodeReviewsResponse_nextToken = Lens.lens (\ListCodeReviewsResponse' {nextToken} -> nextToken) (\s@ListCodeReviewsResponse' {} a -> s {nextToken = a} :: ListCodeReviewsResponse)

-- | The response's http status code.
listCodeReviewsResponse_httpStatus :: Lens.Lens' ListCodeReviewsResponse Prelude.Int
listCodeReviewsResponse_httpStatus = Lens.lens (\ListCodeReviewsResponse' {httpStatus} -> httpStatus) (\s@ListCodeReviewsResponse' {} a -> s {httpStatus = a} :: ListCodeReviewsResponse)

instance Prelude.NFData ListCodeReviewsResponse where
  rnf ListCodeReviewsResponse' {..} =
    Prelude.rnf codeReviewSummaries
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
