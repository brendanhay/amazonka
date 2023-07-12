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
-- Module      : Amazonka.MigrationHubStrategy.ListApplicationComponents
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of all the application components (processes).
--
-- This operation returns paginated results.
module Amazonka.MigrationHubStrategy.ListApplicationComponents
  ( -- * Creating a Request
    ListApplicationComponents (..),
    newListApplicationComponents,

    -- * Request Lenses
    listApplicationComponents_applicationComponentCriteria,
    listApplicationComponents_filterValue,
    listApplicationComponents_groupIdFilter,
    listApplicationComponents_maxResults,
    listApplicationComponents_nextToken,
    listApplicationComponents_sort,

    -- * Destructuring the Response
    ListApplicationComponentsResponse (..),
    newListApplicationComponentsResponse,

    -- * Response Lenses
    listApplicationComponentsResponse_applicationComponentInfos,
    listApplicationComponentsResponse_nextToken,
    listApplicationComponentsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubStrategy.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListApplicationComponents' smart constructor.
data ListApplicationComponents = ListApplicationComponents'
  { -- | Criteria for filtering the list of application components.
    applicationComponentCriteria :: Prelude.Maybe ApplicationComponentCriteria,
    -- | Specify the value based on the application component criteria type. For
    -- example, if @applicationComponentCriteria@ is set to @SERVER_ID@ and
    -- @filterValue@ is set to @server1@, then ListApplicationComponents
    -- returns all the application components running on server1.
    filterValue :: Prelude.Maybe Prelude.Text,
    -- | The group ID specified in to filter on.
    groupIdFilter :: Prelude.Maybe [Group],
    -- | The maximum number of items to include in the response. The maximum
    -- value is 100.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The token from a previous call that you use to retrieve the next set of
    -- results. For example, if a previous call to this action returned 100
    -- items, but you set @maxResults@ to 10. You\'ll receive a set of 10
    -- results along with a token. You then use the returned token to retrieve
    -- the next set of 10.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether to sort by ascending (@ASC@) or descending (@DESC@)
    -- order.
    sort :: Prelude.Maybe SortOrder
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListApplicationComponents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationComponentCriteria', 'listApplicationComponents_applicationComponentCriteria' - Criteria for filtering the list of application components.
--
-- 'filterValue', 'listApplicationComponents_filterValue' - Specify the value based on the application component criteria type. For
-- example, if @applicationComponentCriteria@ is set to @SERVER_ID@ and
-- @filterValue@ is set to @server1@, then ListApplicationComponents
-- returns all the application components running on server1.
--
-- 'groupIdFilter', 'listApplicationComponents_groupIdFilter' - The group ID specified in to filter on.
--
-- 'maxResults', 'listApplicationComponents_maxResults' - The maximum number of items to include in the response. The maximum
-- value is 100.
--
-- 'nextToken', 'listApplicationComponents_nextToken' - The token from a previous call that you use to retrieve the next set of
-- results. For example, if a previous call to this action returned 100
-- items, but you set @maxResults@ to 10. You\'ll receive a set of 10
-- results along with a token. You then use the returned token to retrieve
-- the next set of 10.
--
-- 'sort', 'listApplicationComponents_sort' - Specifies whether to sort by ascending (@ASC@) or descending (@DESC@)
-- order.
newListApplicationComponents ::
  ListApplicationComponents
newListApplicationComponents =
  ListApplicationComponents'
    { applicationComponentCriteria =
        Prelude.Nothing,
      filterValue = Prelude.Nothing,
      groupIdFilter = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sort = Prelude.Nothing
    }

-- | Criteria for filtering the list of application components.
listApplicationComponents_applicationComponentCriteria :: Lens.Lens' ListApplicationComponents (Prelude.Maybe ApplicationComponentCriteria)
listApplicationComponents_applicationComponentCriteria = Lens.lens (\ListApplicationComponents' {applicationComponentCriteria} -> applicationComponentCriteria) (\s@ListApplicationComponents' {} a -> s {applicationComponentCriteria = a} :: ListApplicationComponents)

-- | Specify the value based on the application component criteria type. For
-- example, if @applicationComponentCriteria@ is set to @SERVER_ID@ and
-- @filterValue@ is set to @server1@, then ListApplicationComponents
-- returns all the application components running on server1.
listApplicationComponents_filterValue :: Lens.Lens' ListApplicationComponents (Prelude.Maybe Prelude.Text)
listApplicationComponents_filterValue = Lens.lens (\ListApplicationComponents' {filterValue} -> filterValue) (\s@ListApplicationComponents' {} a -> s {filterValue = a} :: ListApplicationComponents)

-- | The group ID specified in to filter on.
listApplicationComponents_groupIdFilter :: Lens.Lens' ListApplicationComponents (Prelude.Maybe [Group])
listApplicationComponents_groupIdFilter = Lens.lens (\ListApplicationComponents' {groupIdFilter} -> groupIdFilter) (\s@ListApplicationComponents' {} a -> s {groupIdFilter = a} :: ListApplicationComponents) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of items to include in the response. The maximum
-- value is 100.
listApplicationComponents_maxResults :: Lens.Lens' ListApplicationComponents (Prelude.Maybe Prelude.Int)
listApplicationComponents_maxResults = Lens.lens (\ListApplicationComponents' {maxResults} -> maxResults) (\s@ListApplicationComponents' {} a -> s {maxResults = a} :: ListApplicationComponents)

-- | The token from a previous call that you use to retrieve the next set of
-- results. For example, if a previous call to this action returned 100
-- items, but you set @maxResults@ to 10. You\'ll receive a set of 10
-- results along with a token. You then use the returned token to retrieve
-- the next set of 10.
listApplicationComponents_nextToken :: Lens.Lens' ListApplicationComponents (Prelude.Maybe Prelude.Text)
listApplicationComponents_nextToken = Lens.lens (\ListApplicationComponents' {nextToken} -> nextToken) (\s@ListApplicationComponents' {} a -> s {nextToken = a} :: ListApplicationComponents)

-- | Specifies whether to sort by ascending (@ASC@) or descending (@DESC@)
-- order.
listApplicationComponents_sort :: Lens.Lens' ListApplicationComponents (Prelude.Maybe SortOrder)
listApplicationComponents_sort = Lens.lens (\ListApplicationComponents' {sort} -> sort) (\s@ListApplicationComponents' {} a -> s {sort = a} :: ListApplicationComponents)

instance Core.AWSPager ListApplicationComponents where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listApplicationComponentsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listApplicationComponentsResponse_applicationComponentInfos
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listApplicationComponents_nextToken
          Lens..~ rs
          Lens.^? listApplicationComponentsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListApplicationComponents where
  type
    AWSResponse ListApplicationComponents =
      ListApplicationComponentsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListApplicationComponentsResponse'
            Prelude.<$> ( x
                            Data..?> "applicationComponentInfos"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListApplicationComponents where
  hashWithSalt _salt ListApplicationComponents' {..} =
    _salt
      `Prelude.hashWithSalt` applicationComponentCriteria
      `Prelude.hashWithSalt` filterValue
      `Prelude.hashWithSalt` groupIdFilter
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sort

instance Prelude.NFData ListApplicationComponents where
  rnf ListApplicationComponents' {..} =
    Prelude.rnf applicationComponentCriteria
      `Prelude.seq` Prelude.rnf filterValue
      `Prelude.seq` Prelude.rnf groupIdFilter
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sort

instance Data.ToHeaders ListApplicationComponents where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListApplicationComponents where
  toJSON ListApplicationComponents' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("applicationComponentCriteria" Data..=)
              Prelude.<$> applicationComponentCriteria,
            ("filterValue" Data..=) Prelude.<$> filterValue,
            ("groupIdFilter" Data..=) Prelude.<$> groupIdFilter,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("sort" Data..=) Prelude.<$> sort
          ]
      )

instance Data.ToPath ListApplicationComponents where
  toPath = Prelude.const "/list-applicationcomponents"

instance Data.ToQuery ListApplicationComponents where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListApplicationComponentsResponse' smart constructor.
data ListApplicationComponentsResponse = ListApplicationComponentsResponse'
  { -- | The list of application components with detailed information about each
    -- component.
    applicationComponentInfos :: Prelude.Maybe [ApplicationComponentDetail],
    -- | The token you use to retrieve the next set of results, or null if there
    -- are no more results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListApplicationComponentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationComponentInfos', 'listApplicationComponentsResponse_applicationComponentInfos' - The list of application components with detailed information about each
-- component.
--
-- 'nextToken', 'listApplicationComponentsResponse_nextToken' - The token you use to retrieve the next set of results, or null if there
-- are no more results.
--
-- 'httpStatus', 'listApplicationComponentsResponse_httpStatus' - The response's http status code.
newListApplicationComponentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListApplicationComponentsResponse
newListApplicationComponentsResponse pHttpStatus_ =
  ListApplicationComponentsResponse'
    { applicationComponentInfos =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of application components with detailed information about each
-- component.
listApplicationComponentsResponse_applicationComponentInfos :: Lens.Lens' ListApplicationComponentsResponse (Prelude.Maybe [ApplicationComponentDetail])
listApplicationComponentsResponse_applicationComponentInfos = Lens.lens (\ListApplicationComponentsResponse' {applicationComponentInfos} -> applicationComponentInfos) (\s@ListApplicationComponentsResponse' {} a -> s {applicationComponentInfos = a} :: ListApplicationComponentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token you use to retrieve the next set of results, or null if there
-- are no more results.
listApplicationComponentsResponse_nextToken :: Lens.Lens' ListApplicationComponentsResponse (Prelude.Maybe Prelude.Text)
listApplicationComponentsResponse_nextToken = Lens.lens (\ListApplicationComponentsResponse' {nextToken} -> nextToken) (\s@ListApplicationComponentsResponse' {} a -> s {nextToken = a} :: ListApplicationComponentsResponse)

-- | The response's http status code.
listApplicationComponentsResponse_httpStatus :: Lens.Lens' ListApplicationComponentsResponse Prelude.Int
listApplicationComponentsResponse_httpStatus = Lens.lens (\ListApplicationComponentsResponse' {httpStatus} -> httpStatus) (\s@ListApplicationComponentsResponse' {} a -> s {httpStatus = a} :: ListApplicationComponentsResponse)

instance
  Prelude.NFData
    ListApplicationComponentsResponse
  where
  rnf ListApplicationComponentsResponse' {..} =
    Prelude.rnf applicationComponentInfos
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
