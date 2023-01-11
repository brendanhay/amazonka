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
-- Module      : Amazonka.Synthetics.ListGroups
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all groups in the account, displaying their names,
-- unique IDs, and ARNs. The groups from all Regions are returned.
module Amazonka.Synthetics.ListGroups
  ( -- * Creating a Request
    ListGroups (..),
    newListGroups,

    -- * Request Lenses
    listGroups_maxResults,
    listGroups_nextToken,

    -- * Destructuring the Response
    ListGroupsResponse (..),
    newListGroupsResponse,

    -- * Response Lenses
    listGroupsResponse_groups,
    listGroupsResponse_nextToken,
    listGroupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Synthetics.Types

-- | /See:/ 'newListGroups' smart constructor.
data ListGroups = ListGroups'
  { -- | Specify this parameter to limit how many groups are returned each time
    -- you use the @ListGroups@ operation. If you omit this parameter, the
    -- default of 20 is used.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token that indicates that there is more data available. You can use
    -- this token in a subsequent operation to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listGroups_maxResults' - Specify this parameter to limit how many groups are returned each time
-- you use the @ListGroups@ operation. If you omit this parameter, the
-- default of 20 is used.
--
-- 'nextToken', 'listGroups_nextToken' - A token that indicates that there is more data available. You can use
-- this token in a subsequent operation to retrieve the next set of
-- results.
newListGroups ::
  ListGroups
newListGroups =
  ListGroups'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Specify this parameter to limit how many groups are returned each time
-- you use the @ListGroups@ operation. If you omit this parameter, the
-- default of 20 is used.
listGroups_maxResults :: Lens.Lens' ListGroups (Prelude.Maybe Prelude.Natural)
listGroups_maxResults = Lens.lens (\ListGroups' {maxResults} -> maxResults) (\s@ListGroups' {} a -> s {maxResults = a} :: ListGroups)

-- | A token that indicates that there is more data available. You can use
-- this token in a subsequent operation to retrieve the next set of
-- results.
listGroups_nextToken :: Lens.Lens' ListGroups (Prelude.Maybe Prelude.Text)
listGroups_nextToken = Lens.lens (\ListGroups' {nextToken} -> nextToken) (\s@ListGroups' {} a -> s {nextToken = a} :: ListGroups)

instance Core.AWSRequest ListGroups where
  type AWSResponse ListGroups = ListGroupsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListGroupsResponse'
            Prelude.<$> (x Data..?> "Groups" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListGroups where
  hashWithSalt _salt ListGroups' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListGroups where
  rnf ListGroups' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListGroups where
  toJSON ListGroups' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListGroups where
  toPath = Prelude.const "/groups"

instance Data.ToQuery ListGroups where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListGroupsResponse' smart constructor.
data ListGroupsResponse = ListGroupsResponse'
  { -- | An array of structures that each contain information about one group.
    groups :: Prelude.Maybe [GroupSummary],
    -- | A token that indicates that there is more data available. You can use
    -- this token in a subsequent @ListGroups@ operation to retrieve the next
    -- set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groups', 'listGroupsResponse_groups' - An array of structures that each contain information about one group.
--
-- 'nextToken', 'listGroupsResponse_nextToken' - A token that indicates that there is more data available. You can use
-- this token in a subsequent @ListGroups@ operation to retrieve the next
-- set of results.
--
-- 'httpStatus', 'listGroupsResponse_httpStatus' - The response's http status code.
newListGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListGroupsResponse
newListGroupsResponse pHttpStatus_ =
  ListGroupsResponse'
    { groups = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of structures that each contain information about one group.
listGroupsResponse_groups :: Lens.Lens' ListGroupsResponse (Prelude.Maybe [GroupSummary])
listGroupsResponse_groups = Lens.lens (\ListGroupsResponse' {groups} -> groups) (\s@ListGroupsResponse' {} a -> s {groups = a} :: ListGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token that indicates that there is more data available. You can use
-- this token in a subsequent @ListGroups@ operation to retrieve the next
-- set of results.
listGroupsResponse_nextToken :: Lens.Lens' ListGroupsResponse (Prelude.Maybe Prelude.Text)
listGroupsResponse_nextToken = Lens.lens (\ListGroupsResponse' {nextToken} -> nextToken) (\s@ListGroupsResponse' {} a -> s {nextToken = a} :: ListGroupsResponse)

-- | The response's http status code.
listGroupsResponse_httpStatus :: Lens.Lens' ListGroupsResponse Prelude.Int
listGroupsResponse_httpStatus = Lens.lens (\ListGroupsResponse' {httpStatus} -> httpStatus) (\s@ListGroupsResponse' {} a -> s {httpStatus = a} :: ListGroupsResponse)

instance Prelude.NFData ListGroupsResponse where
  rnf ListGroupsResponse' {..} =
    Prelude.rnf groups
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
