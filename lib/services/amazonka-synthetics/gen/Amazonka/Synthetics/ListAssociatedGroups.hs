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
-- Module      : Amazonka.Synthetics.ListAssociatedGroups
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the groups that the specified canary is associated
-- with. The canary that you specify must be in the current Region.
module Amazonka.Synthetics.ListAssociatedGroups
  ( -- * Creating a Request
    ListAssociatedGroups (..),
    newListAssociatedGroups,

    -- * Request Lenses
    listAssociatedGroups_maxResults,
    listAssociatedGroups_nextToken,
    listAssociatedGroups_resourceArn,

    -- * Destructuring the Response
    ListAssociatedGroupsResponse (..),
    newListAssociatedGroupsResponse,

    -- * Response Lenses
    listAssociatedGroupsResponse_groups,
    listAssociatedGroupsResponse_nextToken,
    listAssociatedGroupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Synthetics.Types

-- | /See:/ 'newListAssociatedGroups' smart constructor.
data ListAssociatedGroups = ListAssociatedGroups'
  { -- | Specify this parameter to limit how many groups are returned each time
    -- you use the @ListAssociatedGroups@ operation. If you omit this
    -- parameter, the default of 20 is used.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token that indicates that there is more data available. You can use
    -- this token in a subsequent operation to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the canary that you want to view groups for.
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAssociatedGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listAssociatedGroups_maxResults' - Specify this parameter to limit how many groups are returned each time
-- you use the @ListAssociatedGroups@ operation. If you omit this
-- parameter, the default of 20 is used.
--
-- 'nextToken', 'listAssociatedGroups_nextToken' - A token that indicates that there is more data available. You can use
-- this token in a subsequent operation to retrieve the next set of
-- results.
--
-- 'resourceArn', 'listAssociatedGroups_resourceArn' - The ARN of the canary that you want to view groups for.
newListAssociatedGroups ::
  -- | 'resourceArn'
  Prelude.Text ->
  ListAssociatedGroups
newListAssociatedGroups pResourceArn_ =
  ListAssociatedGroups'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      resourceArn = pResourceArn_
    }

-- | Specify this parameter to limit how many groups are returned each time
-- you use the @ListAssociatedGroups@ operation. If you omit this
-- parameter, the default of 20 is used.
listAssociatedGroups_maxResults :: Lens.Lens' ListAssociatedGroups (Prelude.Maybe Prelude.Natural)
listAssociatedGroups_maxResults = Lens.lens (\ListAssociatedGroups' {maxResults} -> maxResults) (\s@ListAssociatedGroups' {} a -> s {maxResults = a} :: ListAssociatedGroups)

-- | A token that indicates that there is more data available. You can use
-- this token in a subsequent operation to retrieve the next set of
-- results.
listAssociatedGroups_nextToken :: Lens.Lens' ListAssociatedGroups (Prelude.Maybe Prelude.Text)
listAssociatedGroups_nextToken = Lens.lens (\ListAssociatedGroups' {nextToken} -> nextToken) (\s@ListAssociatedGroups' {} a -> s {nextToken = a} :: ListAssociatedGroups)

-- | The ARN of the canary that you want to view groups for.
listAssociatedGroups_resourceArn :: Lens.Lens' ListAssociatedGroups Prelude.Text
listAssociatedGroups_resourceArn = Lens.lens (\ListAssociatedGroups' {resourceArn} -> resourceArn) (\s@ListAssociatedGroups' {} a -> s {resourceArn = a} :: ListAssociatedGroups)

instance Core.AWSRequest ListAssociatedGroups where
  type
    AWSResponse ListAssociatedGroups =
      ListAssociatedGroupsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAssociatedGroupsResponse'
            Prelude.<$> (x Data..?> "Groups" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAssociatedGroups where
  hashWithSalt _salt ListAssociatedGroups' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData ListAssociatedGroups where
  rnf ListAssociatedGroups' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resourceArn

instance Data.ToHeaders ListAssociatedGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListAssociatedGroups where
  toJSON ListAssociatedGroups' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListAssociatedGroups where
  toPath ListAssociatedGroups' {..} =
    Prelude.mconcat
      ["/resource/", Data.toBS resourceArn, "/groups"]

instance Data.ToQuery ListAssociatedGroups where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAssociatedGroupsResponse' smart constructor.
data ListAssociatedGroupsResponse = ListAssociatedGroupsResponse'
  { -- | An array of structures that contain information about the groups that
    -- this canary is associated with.
    groups :: Prelude.Maybe [GroupSummary],
    -- | A token that indicates that there is more data available. You can use
    -- this token in a subsequent @ListAssociatedGroups@ operation to retrieve
    -- the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAssociatedGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groups', 'listAssociatedGroupsResponse_groups' - An array of structures that contain information about the groups that
-- this canary is associated with.
--
-- 'nextToken', 'listAssociatedGroupsResponse_nextToken' - A token that indicates that there is more data available. You can use
-- this token in a subsequent @ListAssociatedGroups@ operation to retrieve
-- the next set of results.
--
-- 'httpStatus', 'listAssociatedGroupsResponse_httpStatus' - The response's http status code.
newListAssociatedGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAssociatedGroupsResponse
newListAssociatedGroupsResponse pHttpStatus_ =
  ListAssociatedGroupsResponse'
    { groups =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of structures that contain information about the groups that
-- this canary is associated with.
listAssociatedGroupsResponse_groups :: Lens.Lens' ListAssociatedGroupsResponse (Prelude.Maybe [GroupSummary])
listAssociatedGroupsResponse_groups = Lens.lens (\ListAssociatedGroupsResponse' {groups} -> groups) (\s@ListAssociatedGroupsResponse' {} a -> s {groups = a} :: ListAssociatedGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token that indicates that there is more data available. You can use
-- this token in a subsequent @ListAssociatedGroups@ operation to retrieve
-- the next set of results.
listAssociatedGroupsResponse_nextToken :: Lens.Lens' ListAssociatedGroupsResponse (Prelude.Maybe Prelude.Text)
listAssociatedGroupsResponse_nextToken = Lens.lens (\ListAssociatedGroupsResponse' {nextToken} -> nextToken) (\s@ListAssociatedGroupsResponse' {} a -> s {nextToken = a} :: ListAssociatedGroupsResponse)

-- | The response's http status code.
listAssociatedGroupsResponse_httpStatus :: Lens.Lens' ListAssociatedGroupsResponse Prelude.Int
listAssociatedGroupsResponse_httpStatus = Lens.lens (\ListAssociatedGroupsResponse' {httpStatus} -> httpStatus) (\s@ListAssociatedGroupsResponse' {} a -> s {httpStatus = a} :: ListAssociatedGroupsResponse)

instance Prelude.NFData ListAssociatedGroupsResponse where
  rnf ListAssociatedGroupsResponse' {..} =
    Prelude.rnf groups
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
