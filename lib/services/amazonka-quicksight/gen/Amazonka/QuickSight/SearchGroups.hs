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
-- Module      : Amazonka.QuickSight.SearchGroups
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use the @SearchGroups@ operation to search groups in a specified Amazon
-- QuickSight namespace using the supplied filters.
module Amazonka.QuickSight.SearchGroups
  ( -- * Creating a Request
    SearchGroups (..),
    newSearchGroups,

    -- * Request Lenses
    searchGroups_maxResults,
    searchGroups_nextToken,
    searchGroups_awsAccountId,
    searchGroups_namespace,
    searchGroups_filters,

    -- * Destructuring the Response
    SearchGroupsResponse (..),
    newSearchGroupsResponse,

    -- * Response Lenses
    searchGroupsResponse_groupList,
    searchGroupsResponse_nextToken,
    searchGroupsResponse_requestId,
    searchGroupsResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSearchGroups' smart constructor.
data SearchGroups = SearchGroups'
  { -- | The maximum number of results to return from this request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A pagination token that can be used in a subsequent request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID for the Amazon Web Services account that the group is in.
    -- Currently, you use the ID for the Amazon Web Services account that
    -- contains your Amazon QuickSight account.
    awsAccountId :: Prelude.Text,
    -- | The namespace that you want to search.
    namespace :: Prelude.Text,
    -- | The structure for the search filters that you want to apply to your
    -- search.
    filters :: Prelude.NonEmpty GroupSearchFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'searchGroups_maxResults' - The maximum number of results to return from this request.
--
-- 'nextToken', 'searchGroups_nextToken' - A pagination token that can be used in a subsequent request.
--
-- 'awsAccountId', 'searchGroups_awsAccountId' - The ID for the Amazon Web Services account that the group is in.
-- Currently, you use the ID for the Amazon Web Services account that
-- contains your Amazon QuickSight account.
--
-- 'namespace', 'searchGroups_namespace' - The namespace that you want to search.
--
-- 'filters', 'searchGroups_filters' - The structure for the search filters that you want to apply to your
-- search.
newSearchGroups ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'namespace'
  Prelude.Text ->
  -- | 'filters'
  Prelude.NonEmpty GroupSearchFilter ->
  SearchGroups
newSearchGroups pAwsAccountId_ pNamespace_ pFilters_ =
  SearchGroups'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      awsAccountId = pAwsAccountId_,
      namespace = pNamespace_,
      filters = Lens.coerced Lens.# pFilters_
    }

-- | The maximum number of results to return from this request.
searchGroups_maxResults :: Lens.Lens' SearchGroups (Prelude.Maybe Prelude.Natural)
searchGroups_maxResults = Lens.lens (\SearchGroups' {maxResults} -> maxResults) (\s@SearchGroups' {} a -> s {maxResults = a} :: SearchGroups)

-- | A pagination token that can be used in a subsequent request.
searchGroups_nextToken :: Lens.Lens' SearchGroups (Prelude.Maybe Prelude.Text)
searchGroups_nextToken = Lens.lens (\SearchGroups' {nextToken} -> nextToken) (\s@SearchGroups' {} a -> s {nextToken = a} :: SearchGroups)

-- | The ID for the Amazon Web Services account that the group is in.
-- Currently, you use the ID for the Amazon Web Services account that
-- contains your Amazon QuickSight account.
searchGroups_awsAccountId :: Lens.Lens' SearchGroups Prelude.Text
searchGroups_awsAccountId = Lens.lens (\SearchGroups' {awsAccountId} -> awsAccountId) (\s@SearchGroups' {} a -> s {awsAccountId = a} :: SearchGroups)

-- | The namespace that you want to search.
searchGroups_namespace :: Lens.Lens' SearchGroups Prelude.Text
searchGroups_namespace = Lens.lens (\SearchGroups' {namespace} -> namespace) (\s@SearchGroups' {} a -> s {namespace = a} :: SearchGroups)

-- | The structure for the search filters that you want to apply to your
-- search.
searchGroups_filters :: Lens.Lens' SearchGroups (Prelude.NonEmpty GroupSearchFilter)
searchGroups_filters = Lens.lens (\SearchGroups' {filters} -> filters) (\s@SearchGroups' {} a -> s {filters = a} :: SearchGroups) Prelude.. Lens.coerced

instance Core.AWSRequest SearchGroups where
  type AWSResponse SearchGroups = SearchGroupsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchGroupsResponse'
            Prelude.<$> (x Data..?> "GroupList" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchGroups where
  hashWithSalt _salt SearchGroups' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` namespace
      `Prelude.hashWithSalt` filters

instance Prelude.NFData SearchGroups where
  rnf SearchGroups' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf namespace
      `Prelude.seq` Prelude.rnf filters

instance Data.ToHeaders SearchGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SearchGroups where
  toJSON SearchGroups' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Filters" Data..= filters)]
      )

instance Data.ToPath SearchGroups where
  toPath SearchGroups' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/namespaces/",
        Data.toBS namespace,
        "/groups-search"
      ]

instance Data.ToQuery SearchGroups where
  toQuery SearchGroups' {..} =
    Prelude.mconcat
      [ "max-results" Data.=: maxResults,
        "next-token" Data.=: nextToken
      ]

-- | /See:/ 'newSearchGroupsResponse' smart constructor.
data SearchGroupsResponse = SearchGroupsResponse'
  { -- | A list of groups in a specified namespace that match the filters you set
    -- in your @SearchGroups@ request.
    groupList :: Prelude.Maybe [Group],
    -- | A pagination token that can be used in a subsequent request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupList', 'searchGroupsResponse_groupList' - A list of groups in a specified namespace that match the filters you set
-- in your @SearchGroups@ request.
--
-- 'nextToken', 'searchGroupsResponse_nextToken' - A pagination token that can be used in a subsequent request.
--
-- 'requestId', 'searchGroupsResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'searchGroupsResponse_status' - The HTTP status of the request.
newSearchGroupsResponse ::
  -- | 'status'
  Prelude.Int ->
  SearchGroupsResponse
newSearchGroupsResponse pStatus_ =
  SearchGroupsResponse'
    { groupList = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      requestId = Prelude.Nothing,
      status = pStatus_
    }

-- | A list of groups in a specified namespace that match the filters you set
-- in your @SearchGroups@ request.
searchGroupsResponse_groupList :: Lens.Lens' SearchGroupsResponse (Prelude.Maybe [Group])
searchGroupsResponse_groupList = Lens.lens (\SearchGroupsResponse' {groupList} -> groupList) (\s@SearchGroupsResponse' {} a -> s {groupList = a} :: SearchGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A pagination token that can be used in a subsequent request.
searchGroupsResponse_nextToken :: Lens.Lens' SearchGroupsResponse (Prelude.Maybe Prelude.Text)
searchGroupsResponse_nextToken = Lens.lens (\SearchGroupsResponse' {nextToken} -> nextToken) (\s@SearchGroupsResponse' {} a -> s {nextToken = a} :: SearchGroupsResponse)

-- | The Amazon Web Services request ID for this operation.
searchGroupsResponse_requestId :: Lens.Lens' SearchGroupsResponse (Prelude.Maybe Prelude.Text)
searchGroupsResponse_requestId = Lens.lens (\SearchGroupsResponse' {requestId} -> requestId) (\s@SearchGroupsResponse' {} a -> s {requestId = a} :: SearchGroupsResponse)

-- | The HTTP status of the request.
searchGroupsResponse_status :: Lens.Lens' SearchGroupsResponse Prelude.Int
searchGroupsResponse_status = Lens.lens (\SearchGroupsResponse' {status} -> status) (\s@SearchGroupsResponse' {} a -> s {status = a} :: SearchGroupsResponse)

instance Prelude.NFData SearchGroupsResponse where
  rnf SearchGroupsResponse' {..} =
    Prelude.rnf groupList
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status
