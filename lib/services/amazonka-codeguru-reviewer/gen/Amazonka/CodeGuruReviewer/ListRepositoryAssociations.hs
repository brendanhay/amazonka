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
-- Module      : Amazonka.CodeGuruReviewer.ListRepositoryAssociations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_RepositoryAssociationSummary.html RepositoryAssociationSummary>
-- objects that contain summary information about a repository association.
-- You can filter the returned list by
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_RepositoryAssociationSummary.html#reviewer-Type-RepositoryAssociationSummary-ProviderType ProviderType>,
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_RepositoryAssociationSummary.html#reviewer-Type-RepositoryAssociationSummary-Name Name>,
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_RepositoryAssociationSummary.html#reviewer-Type-RepositoryAssociationSummary-State State>,
-- and
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_RepositoryAssociationSummary.html#reviewer-Type-RepositoryAssociationSummary-Owner Owner>.
--
-- This operation returns paginated results.
module Amazonka.CodeGuruReviewer.ListRepositoryAssociations
  ( -- * Creating a Request
    ListRepositoryAssociations (..),
    newListRepositoryAssociations,

    -- * Request Lenses
    listRepositoryAssociations_maxResults,
    listRepositoryAssociations_names,
    listRepositoryAssociations_nextToken,
    listRepositoryAssociations_owners,
    listRepositoryAssociations_providerTypes,
    listRepositoryAssociations_states,

    -- * Destructuring the Response
    ListRepositoryAssociationsResponse (..),
    newListRepositoryAssociationsResponse,

    -- * Response Lenses
    listRepositoryAssociationsResponse_nextToken,
    listRepositoryAssociationsResponse_repositoryAssociationSummaries,
    listRepositoryAssociationsResponse_httpStatus,
  )
where

import Amazonka.CodeGuruReviewer.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListRepositoryAssociations' smart constructor.
data ListRepositoryAssociations = ListRepositoryAssociations'
  { -- | The maximum number of repository association results returned by
    -- @ListRepositoryAssociations@ in paginated output. When this parameter is
    -- used, @ListRepositoryAssociations@ only returns @maxResults@ results in
    -- a single page with a @nextToken@ response element. The remaining results
    -- of the initial request can be seen by sending another
    -- @ListRepositoryAssociations@ request with the returned @nextToken@
    -- value. This value can be between 1 and 100. If this parameter is not
    -- used, @ListRepositoryAssociations@ returns up to 100 results and a
    -- @nextToken@ value if applicable.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | List of repository names to use as a filter.
    names :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The @nextToken@ value returned from a previous paginated
    -- @ListRepositoryAssociations@ request where @maxResults@ was used and the
    -- results exceeded the value of that parameter. Pagination continues from
    -- the end of the previous results that returned the @nextToken@ value.
    --
    -- Treat this token as an opaque identifier that is only used to retrieve
    -- the next items in a list and not for other programmatic purposes.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | List of owners to use as a filter. For Amazon Web Services CodeCommit,
    -- it is the name of the CodeCommit account that was used to associate the
    -- repository. For other repository source providers, such as Bitbucket and
    -- GitHub Enterprise Server, this is name of the account that was used to
    -- associate the repository.
    owners :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | List of provider types to use as a filter.
    providerTypes :: Prelude.Maybe (Prelude.NonEmpty ProviderType),
    -- | List of repository association states to use as a filter.
    --
    -- The valid repository association states are:
    --
    -- -   __Associated__: The repository association is complete.
    --
    -- -   __Associating__: CodeGuru Reviewer is:
    --
    --     -   Setting up pull request notifications. This is required for pull
    --         requests to trigger a CodeGuru Reviewer review.
    --
    --         If your repository @ProviderType@ is @GitHub@,
    --         @GitHub Enterprise Server@, or @Bitbucket@, CodeGuru Reviewer
    --         creates webhooks in your repository to trigger CodeGuru Reviewer
    --         reviews. If you delete these webhooks, reviews of code in your
    --         repository cannot be triggered.
    --
    --     -   Setting up source code access. This is required for CodeGuru
    --         Reviewer to securely clone code in your repository.
    --
    -- -   __Failed__: The repository failed to associate or disassociate.
    --
    -- -   __Disassociating__: CodeGuru Reviewer is removing the repository\'s
    --     pull request notifications and source code access.
    --
    -- -   __Disassociated__: CodeGuru Reviewer successfully disassociated the
    --     repository. You can create a new association with this repository if
    --     you want to review source code in it later. You can control access
    --     to code reviews created in anassociated repository with tags after
    --     it has been disassociated. For more information, see
    --     <https://docs.aws.amazon.com/codeguru/latest/reviewer-ug/auth-and-access-control-using-tags.html Using tags to control access to associated repositories>
    --     in the /Amazon CodeGuru Reviewer User Guide/.
    states :: Prelude.Maybe (Prelude.NonEmpty RepositoryAssociationState)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRepositoryAssociations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listRepositoryAssociations_maxResults' - The maximum number of repository association results returned by
-- @ListRepositoryAssociations@ in paginated output. When this parameter is
-- used, @ListRepositoryAssociations@ only returns @maxResults@ results in
-- a single page with a @nextToken@ response element. The remaining results
-- of the initial request can be seen by sending another
-- @ListRepositoryAssociations@ request with the returned @nextToken@
-- value. This value can be between 1 and 100. If this parameter is not
-- used, @ListRepositoryAssociations@ returns up to 100 results and a
-- @nextToken@ value if applicable.
--
-- 'names', 'listRepositoryAssociations_names' - List of repository names to use as a filter.
--
-- 'nextToken', 'listRepositoryAssociations_nextToken' - The @nextToken@ value returned from a previous paginated
-- @ListRepositoryAssociations@ request where @maxResults@ was used and the
-- results exceeded the value of that parameter. Pagination continues from
-- the end of the previous results that returned the @nextToken@ value.
--
-- Treat this token as an opaque identifier that is only used to retrieve
-- the next items in a list and not for other programmatic purposes.
--
-- 'owners', 'listRepositoryAssociations_owners' - List of owners to use as a filter. For Amazon Web Services CodeCommit,
-- it is the name of the CodeCommit account that was used to associate the
-- repository. For other repository source providers, such as Bitbucket and
-- GitHub Enterprise Server, this is name of the account that was used to
-- associate the repository.
--
-- 'providerTypes', 'listRepositoryAssociations_providerTypes' - List of provider types to use as a filter.
--
-- 'states', 'listRepositoryAssociations_states' - List of repository association states to use as a filter.
--
-- The valid repository association states are:
--
-- -   __Associated__: The repository association is complete.
--
-- -   __Associating__: CodeGuru Reviewer is:
--
--     -   Setting up pull request notifications. This is required for pull
--         requests to trigger a CodeGuru Reviewer review.
--
--         If your repository @ProviderType@ is @GitHub@,
--         @GitHub Enterprise Server@, or @Bitbucket@, CodeGuru Reviewer
--         creates webhooks in your repository to trigger CodeGuru Reviewer
--         reviews. If you delete these webhooks, reviews of code in your
--         repository cannot be triggered.
--
--     -   Setting up source code access. This is required for CodeGuru
--         Reviewer to securely clone code in your repository.
--
-- -   __Failed__: The repository failed to associate or disassociate.
--
-- -   __Disassociating__: CodeGuru Reviewer is removing the repository\'s
--     pull request notifications and source code access.
--
-- -   __Disassociated__: CodeGuru Reviewer successfully disassociated the
--     repository. You can create a new association with this repository if
--     you want to review source code in it later. You can control access
--     to code reviews created in anassociated repository with tags after
--     it has been disassociated. For more information, see
--     <https://docs.aws.amazon.com/codeguru/latest/reviewer-ug/auth-and-access-control-using-tags.html Using tags to control access to associated repositories>
--     in the /Amazon CodeGuru Reviewer User Guide/.
newListRepositoryAssociations ::
  ListRepositoryAssociations
newListRepositoryAssociations =
  ListRepositoryAssociations'
    { maxResults =
        Prelude.Nothing,
      names = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      owners = Prelude.Nothing,
      providerTypes = Prelude.Nothing,
      states = Prelude.Nothing
    }

-- | The maximum number of repository association results returned by
-- @ListRepositoryAssociations@ in paginated output. When this parameter is
-- used, @ListRepositoryAssociations@ only returns @maxResults@ results in
-- a single page with a @nextToken@ response element. The remaining results
-- of the initial request can be seen by sending another
-- @ListRepositoryAssociations@ request with the returned @nextToken@
-- value. This value can be between 1 and 100. If this parameter is not
-- used, @ListRepositoryAssociations@ returns up to 100 results and a
-- @nextToken@ value if applicable.
listRepositoryAssociations_maxResults :: Lens.Lens' ListRepositoryAssociations (Prelude.Maybe Prelude.Natural)
listRepositoryAssociations_maxResults = Lens.lens (\ListRepositoryAssociations' {maxResults} -> maxResults) (\s@ListRepositoryAssociations' {} a -> s {maxResults = a} :: ListRepositoryAssociations)

-- | List of repository names to use as a filter.
listRepositoryAssociations_names :: Lens.Lens' ListRepositoryAssociations (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
listRepositoryAssociations_names = Lens.lens (\ListRepositoryAssociations' {names} -> names) (\s@ListRepositoryAssociations' {} a -> s {names = a} :: ListRepositoryAssociations) Prelude.. Lens.mapping Lens.coerced

-- | The @nextToken@ value returned from a previous paginated
-- @ListRepositoryAssociations@ request where @maxResults@ was used and the
-- results exceeded the value of that parameter. Pagination continues from
-- the end of the previous results that returned the @nextToken@ value.
--
-- Treat this token as an opaque identifier that is only used to retrieve
-- the next items in a list and not for other programmatic purposes.
listRepositoryAssociations_nextToken :: Lens.Lens' ListRepositoryAssociations (Prelude.Maybe Prelude.Text)
listRepositoryAssociations_nextToken = Lens.lens (\ListRepositoryAssociations' {nextToken} -> nextToken) (\s@ListRepositoryAssociations' {} a -> s {nextToken = a} :: ListRepositoryAssociations)

-- | List of owners to use as a filter. For Amazon Web Services CodeCommit,
-- it is the name of the CodeCommit account that was used to associate the
-- repository. For other repository source providers, such as Bitbucket and
-- GitHub Enterprise Server, this is name of the account that was used to
-- associate the repository.
listRepositoryAssociations_owners :: Lens.Lens' ListRepositoryAssociations (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
listRepositoryAssociations_owners = Lens.lens (\ListRepositoryAssociations' {owners} -> owners) (\s@ListRepositoryAssociations' {} a -> s {owners = a} :: ListRepositoryAssociations) Prelude.. Lens.mapping Lens.coerced

-- | List of provider types to use as a filter.
listRepositoryAssociations_providerTypes :: Lens.Lens' ListRepositoryAssociations (Prelude.Maybe (Prelude.NonEmpty ProviderType))
listRepositoryAssociations_providerTypes = Lens.lens (\ListRepositoryAssociations' {providerTypes} -> providerTypes) (\s@ListRepositoryAssociations' {} a -> s {providerTypes = a} :: ListRepositoryAssociations) Prelude.. Lens.mapping Lens.coerced

-- | List of repository association states to use as a filter.
--
-- The valid repository association states are:
--
-- -   __Associated__: The repository association is complete.
--
-- -   __Associating__: CodeGuru Reviewer is:
--
--     -   Setting up pull request notifications. This is required for pull
--         requests to trigger a CodeGuru Reviewer review.
--
--         If your repository @ProviderType@ is @GitHub@,
--         @GitHub Enterprise Server@, or @Bitbucket@, CodeGuru Reviewer
--         creates webhooks in your repository to trigger CodeGuru Reviewer
--         reviews. If you delete these webhooks, reviews of code in your
--         repository cannot be triggered.
--
--     -   Setting up source code access. This is required for CodeGuru
--         Reviewer to securely clone code in your repository.
--
-- -   __Failed__: The repository failed to associate or disassociate.
--
-- -   __Disassociating__: CodeGuru Reviewer is removing the repository\'s
--     pull request notifications and source code access.
--
-- -   __Disassociated__: CodeGuru Reviewer successfully disassociated the
--     repository. You can create a new association with this repository if
--     you want to review source code in it later. You can control access
--     to code reviews created in anassociated repository with tags after
--     it has been disassociated. For more information, see
--     <https://docs.aws.amazon.com/codeguru/latest/reviewer-ug/auth-and-access-control-using-tags.html Using tags to control access to associated repositories>
--     in the /Amazon CodeGuru Reviewer User Guide/.
listRepositoryAssociations_states :: Lens.Lens' ListRepositoryAssociations (Prelude.Maybe (Prelude.NonEmpty RepositoryAssociationState))
listRepositoryAssociations_states = Lens.lens (\ListRepositoryAssociations' {states} -> states) (\s@ListRepositoryAssociations' {} a -> s {states = a} :: ListRepositoryAssociations) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSPager ListRepositoryAssociations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRepositoryAssociationsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listRepositoryAssociationsResponse_repositoryAssociationSummaries
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listRepositoryAssociations_nextToken
          Lens..~ rs
          Lens.^? listRepositoryAssociationsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListRepositoryAssociations where
  type
    AWSResponse ListRepositoryAssociations =
      ListRepositoryAssociationsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRepositoryAssociationsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "RepositoryAssociationSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRepositoryAssociations where
  hashWithSalt _salt ListRepositoryAssociations' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` names
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` owners
      `Prelude.hashWithSalt` providerTypes
      `Prelude.hashWithSalt` states

instance Prelude.NFData ListRepositoryAssociations where
  rnf ListRepositoryAssociations' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf names
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf owners
      `Prelude.seq` Prelude.rnf providerTypes
      `Prelude.seq` Prelude.rnf states

instance Data.ToHeaders ListRepositoryAssociations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListRepositoryAssociations where
  toPath = Prelude.const "/associations"

instance Data.ToQuery ListRepositoryAssociations where
  toQuery ListRepositoryAssociations' {..} =
    Prelude.mconcat
      [ "MaxResults" Data.=: maxResults,
        "Name"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> names),
        "NextToken" Data.=: nextToken,
        "Owner"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> owners),
        "ProviderType"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> providerTypes
            ),
        "State"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> states)
      ]

-- | /See:/ 'newListRepositoryAssociationsResponse' smart constructor.
data ListRepositoryAssociationsResponse = ListRepositoryAssociationsResponse'
  { -- | The @nextToken@ value to include in a future @ListRecommendations@
    -- request. When the results of a @ListRecommendations@ request exceed
    -- @maxResults@, this value can be used to retrieve the next page of
    -- results. This value is @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of repository associations that meet the criteria of the request.
    repositoryAssociationSummaries :: Prelude.Maybe [RepositoryAssociationSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRepositoryAssociationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRepositoryAssociationsResponse_nextToken' - The @nextToken@ value to include in a future @ListRecommendations@
-- request. When the results of a @ListRecommendations@ request exceed
-- @maxResults@, this value can be used to retrieve the next page of
-- results. This value is @null@ when there are no more results to return.
--
-- 'repositoryAssociationSummaries', 'listRepositoryAssociationsResponse_repositoryAssociationSummaries' - A list of repository associations that meet the criteria of the request.
--
-- 'httpStatus', 'listRepositoryAssociationsResponse_httpStatus' - The response's http status code.
newListRepositoryAssociationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRepositoryAssociationsResponse
newListRepositoryAssociationsResponse pHttpStatus_ =
  ListRepositoryAssociationsResponse'
    { nextToken =
        Prelude.Nothing,
      repositoryAssociationSummaries =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @nextToken@ value to include in a future @ListRecommendations@
-- request. When the results of a @ListRecommendations@ request exceed
-- @maxResults@, this value can be used to retrieve the next page of
-- results. This value is @null@ when there are no more results to return.
listRepositoryAssociationsResponse_nextToken :: Lens.Lens' ListRepositoryAssociationsResponse (Prelude.Maybe Prelude.Text)
listRepositoryAssociationsResponse_nextToken = Lens.lens (\ListRepositoryAssociationsResponse' {nextToken} -> nextToken) (\s@ListRepositoryAssociationsResponse' {} a -> s {nextToken = a} :: ListRepositoryAssociationsResponse)

-- | A list of repository associations that meet the criteria of the request.
listRepositoryAssociationsResponse_repositoryAssociationSummaries :: Lens.Lens' ListRepositoryAssociationsResponse (Prelude.Maybe [RepositoryAssociationSummary])
listRepositoryAssociationsResponse_repositoryAssociationSummaries = Lens.lens (\ListRepositoryAssociationsResponse' {repositoryAssociationSummaries} -> repositoryAssociationSummaries) (\s@ListRepositoryAssociationsResponse' {} a -> s {repositoryAssociationSummaries = a} :: ListRepositoryAssociationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listRepositoryAssociationsResponse_httpStatus :: Lens.Lens' ListRepositoryAssociationsResponse Prelude.Int
listRepositoryAssociationsResponse_httpStatus = Lens.lens (\ListRepositoryAssociationsResponse' {httpStatus} -> httpStatus) (\s@ListRepositoryAssociationsResponse' {} a -> s {httpStatus = a} :: ListRepositoryAssociationsResponse)

instance
  Prelude.NFData
    ListRepositoryAssociationsResponse
  where
  rnf ListRepositoryAssociationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf repositoryAssociationSummaries
      `Prelude.seq` Prelude.rnf httpStatus
