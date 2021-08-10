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
-- Module      : Network.AWS.IoT.ListThingGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the thing groups in your account.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListThingGroups
  ( -- * Creating a Request
    ListThingGroups (..),
    newListThingGroups,

    -- * Request Lenses
    listThingGroups_namePrefixFilter,
    listThingGroups_nextToken,
    listThingGroups_maxResults,
    listThingGroups_recursive,
    listThingGroups_parentGroup,

    -- * Destructuring the Response
    ListThingGroupsResponse (..),
    newListThingGroupsResponse,

    -- * Response Lenses
    listThingGroupsResponse_nextToken,
    listThingGroupsResponse_thingGroups,
    listThingGroupsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListThingGroups' smart constructor.
data ListThingGroups = ListThingGroups'
  { -- | A filter that limits the results to those with the specified name
    -- prefix.
    namePrefixFilter :: Prelude.Maybe Prelude.Text,
    -- | To retrieve the next set of results, the @nextToken@ value from a
    -- previous response; otherwise __null__ to receive the first set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return at one time.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If true, return child groups as well.
    recursive :: Prelude.Maybe Prelude.Bool,
    -- | A filter that limits the results to those with the specified parent
    -- group.
    parentGroup :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListThingGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'namePrefixFilter', 'listThingGroups_namePrefixFilter' - A filter that limits the results to those with the specified name
-- prefix.
--
-- 'nextToken', 'listThingGroups_nextToken' - To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
--
-- 'maxResults', 'listThingGroups_maxResults' - The maximum number of results to return at one time.
--
-- 'recursive', 'listThingGroups_recursive' - If true, return child groups as well.
--
-- 'parentGroup', 'listThingGroups_parentGroup' - A filter that limits the results to those with the specified parent
-- group.
newListThingGroups ::
  ListThingGroups
newListThingGroups =
  ListThingGroups'
    { namePrefixFilter =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      recursive = Prelude.Nothing,
      parentGroup = Prelude.Nothing
    }

-- | A filter that limits the results to those with the specified name
-- prefix.
listThingGroups_namePrefixFilter :: Lens.Lens' ListThingGroups (Prelude.Maybe Prelude.Text)
listThingGroups_namePrefixFilter = Lens.lens (\ListThingGroups' {namePrefixFilter} -> namePrefixFilter) (\s@ListThingGroups' {} a -> s {namePrefixFilter = a} :: ListThingGroups)

-- | To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
listThingGroups_nextToken :: Lens.Lens' ListThingGroups (Prelude.Maybe Prelude.Text)
listThingGroups_nextToken = Lens.lens (\ListThingGroups' {nextToken} -> nextToken) (\s@ListThingGroups' {} a -> s {nextToken = a} :: ListThingGroups)

-- | The maximum number of results to return at one time.
listThingGroups_maxResults :: Lens.Lens' ListThingGroups (Prelude.Maybe Prelude.Natural)
listThingGroups_maxResults = Lens.lens (\ListThingGroups' {maxResults} -> maxResults) (\s@ListThingGroups' {} a -> s {maxResults = a} :: ListThingGroups)

-- | If true, return child groups as well.
listThingGroups_recursive :: Lens.Lens' ListThingGroups (Prelude.Maybe Prelude.Bool)
listThingGroups_recursive = Lens.lens (\ListThingGroups' {recursive} -> recursive) (\s@ListThingGroups' {} a -> s {recursive = a} :: ListThingGroups)

-- | A filter that limits the results to those with the specified parent
-- group.
listThingGroups_parentGroup :: Lens.Lens' ListThingGroups (Prelude.Maybe Prelude.Text)
listThingGroups_parentGroup = Lens.lens (\ListThingGroups' {parentGroup} -> parentGroup) (\s@ListThingGroups' {} a -> s {parentGroup = a} :: ListThingGroups)

instance Core.AWSPager ListThingGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listThingGroupsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listThingGroupsResponse_thingGroups
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listThingGroups_nextToken
          Lens..~ rs
          Lens.^? listThingGroupsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListThingGroups where
  type
    AWSResponse ListThingGroups =
      ListThingGroupsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListThingGroupsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "thingGroups" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListThingGroups

instance Prelude.NFData ListThingGroups

instance Core.ToHeaders ListThingGroups where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListThingGroups where
  toPath = Prelude.const "/thing-groups"

instance Core.ToQuery ListThingGroups where
  toQuery ListThingGroups' {..} =
    Prelude.mconcat
      [ "namePrefixFilter" Core.=: namePrefixFilter,
        "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults,
        "recursive" Core.=: recursive,
        "parentGroup" Core.=: parentGroup
      ]

-- | /See:/ 'newListThingGroupsResponse' smart constructor.
data ListThingGroupsResponse = ListThingGroupsResponse'
  { -- | The token to use to get the next set of results. Will not be returned if
    -- operation has returned all results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The thing groups.
    thingGroups :: Prelude.Maybe [GroupNameAndArn],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListThingGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listThingGroupsResponse_nextToken' - The token to use to get the next set of results. Will not be returned if
-- operation has returned all results.
--
-- 'thingGroups', 'listThingGroupsResponse_thingGroups' - The thing groups.
--
-- 'httpStatus', 'listThingGroupsResponse_httpStatus' - The response's http status code.
newListThingGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListThingGroupsResponse
newListThingGroupsResponse pHttpStatus_ =
  ListThingGroupsResponse'
    { nextToken =
        Prelude.Nothing,
      thingGroups = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to get the next set of results. Will not be returned if
-- operation has returned all results.
listThingGroupsResponse_nextToken :: Lens.Lens' ListThingGroupsResponse (Prelude.Maybe Prelude.Text)
listThingGroupsResponse_nextToken = Lens.lens (\ListThingGroupsResponse' {nextToken} -> nextToken) (\s@ListThingGroupsResponse' {} a -> s {nextToken = a} :: ListThingGroupsResponse)

-- | The thing groups.
listThingGroupsResponse_thingGroups :: Lens.Lens' ListThingGroupsResponse (Prelude.Maybe [GroupNameAndArn])
listThingGroupsResponse_thingGroups = Lens.lens (\ListThingGroupsResponse' {thingGroups} -> thingGroups) (\s@ListThingGroupsResponse' {} a -> s {thingGroups = a} :: ListThingGroupsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listThingGroupsResponse_httpStatus :: Lens.Lens' ListThingGroupsResponse Prelude.Int
listThingGroupsResponse_httpStatus = Lens.lens (\ListThingGroupsResponse' {httpStatus} -> httpStatus) (\s@ListThingGroupsResponse' {} a -> s {httpStatus = a} :: ListThingGroupsResponse)

instance Prelude.NFData ListThingGroupsResponse
