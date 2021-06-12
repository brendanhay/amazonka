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
-- Module      : Network.AWS.IoT.ListThingsInThingGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the things in the specified group.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListThingsInThingGroup
  ( -- * Creating a Request
    ListThingsInThingGroup (..),
    newListThingsInThingGroup,

    -- * Request Lenses
    listThingsInThingGroup_nextToken,
    listThingsInThingGroup_maxResults,
    listThingsInThingGroup_recursive,
    listThingsInThingGroup_thingGroupName,

    -- * Destructuring the Response
    ListThingsInThingGroupResponse (..),
    newListThingsInThingGroupResponse,

    -- * Response Lenses
    listThingsInThingGroupResponse_nextToken,
    listThingsInThingGroupResponse_things,
    listThingsInThingGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListThingsInThingGroup' smart constructor.
data ListThingsInThingGroup = ListThingsInThingGroup'
  { -- | To retrieve the next set of results, the @nextToken@ value from a
    -- previous response; otherwise __null__ to receive the first set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return at one time.
    maxResults :: Core.Maybe Core.Natural,
    -- | When true, list things in this thing group and in all child groups as
    -- well.
    recursive :: Core.Maybe Core.Bool,
    -- | The thing group name.
    thingGroupName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListThingsInThingGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listThingsInThingGroup_nextToken' - To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
--
-- 'maxResults', 'listThingsInThingGroup_maxResults' - The maximum number of results to return at one time.
--
-- 'recursive', 'listThingsInThingGroup_recursive' - When true, list things in this thing group and in all child groups as
-- well.
--
-- 'thingGroupName', 'listThingsInThingGroup_thingGroupName' - The thing group name.
newListThingsInThingGroup ::
  -- | 'thingGroupName'
  Core.Text ->
  ListThingsInThingGroup
newListThingsInThingGroup pThingGroupName_ =
  ListThingsInThingGroup'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      recursive = Core.Nothing,
      thingGroupName = pThingGroupName_
    }

-- | To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
listThingsInThingGroup_nextToken :: Lens.Lens' ListThingsInThingGroup (Core.Maybe Core.Text)
listThingsInThingGroup_nextToken = Lens.lens (\ListThingsInThingGroup' {nextToken} -> nextToken) (\s@ListThingsInThingGroup' {} a -> s {nextToken = a} :: ListThingsInThingGroup)

-- | The maximum number of results to return at one time.
listThingsInThingGroup_maxResults :: Lens.Lens' ListThingsInThingGroup (Core.Maybe Core.Natural)
listThingsInThingGroup_maxResults = Lens.lens (\ListThingsInThingGroup' {maxResults} -> maxResults) (\s@ListThingsInThingGroup' {} a -> s {maxResults = a} :: ListThingsInThingGroup)

-- | When true, list things in this thing group and in all child groups as
-- well.
listThingsInThingGroup_recursive :: Lens.Lens' ListThingsInThingGroup (Core.Maybe Core.Bool)
listThingsInThingGroup_recursive = Lens.lens (\ListThingsInThingGroup' {recursive} -> recursive) (\s@ListThingsInThingGroup' {} a -> s {recursive = a} :: ListThingsInThingGroup)

-- | The thing group name.
listThingsInThingGroup_thingGroupName :: Lens.Lens' ListThingsInThingGroup Core.Text
listThingsInThingGroup_thingGroupName = Lens.lens (\ListThingsInThingGroup' {thingGroupName} -> thingGroupName) (\s@ListThingsInThingGroup' {} a -> s {thingGroupName = a} :: ListThingsInThingGroup)

instance Core.AWSPager ListThingsInThingGroup where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listThingsInThingGroupResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listThingsInThingGroupResponse_things
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listThingsInThingGroup_nextToken
          Lens..~ rs
          Lens.^? listThingsInThingGroupResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListThingsInThingGroup where
  type
    AWSResponse ListThingsInThingGroup =
      ListThingsInThingGroupResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListThingsInThingGroupResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "things" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListThingsInThingGroup

instance Core.NFData ListThingsInThingGroup

instance Core.ToHeaders ListThingsInThingGroup where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListThingsInThingGroup where
  toPath ListThingsInThingGroup' {..} =
    Core.mconcat
      [ "/thing-groups/",
        Core.toBS thingGroupName,
        "/things"
      ]

instance Core.ToQuery ListThingsInThingGroup where
  toQuery ListThingsInThingGroup' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults,
        "recursive" Core.=: recursive
      ]

-- | /See:/ 'newListThingsInThingGroupResponse' smart constructor.
data ListThingsInThingGroupResponse = ListThingsInThingGroupResponse'
  { -- | The token to use to get the next set of results, or __null__ if there
    -- are no additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | The things in the specified thing group.
    things :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListThingsInThingGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listThingsInThingGroupResponse_nextToken' - The token to use to get the next set of results, or __null__ if there
-- are no additional results.
--
-- 'things', 'listThingsInThingGroupResponse_things' - The things in the specified thing group.
--
-- 'httpStatus', 'listThingsInThingGroupResponse_httpStatus' - The response's http status code.
newListThingsInThingGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListThingsInThingGroupResponse
newListThingsInThingGroupResponse pHttpStatus_ =
  ListThingsInThingGroupResponse'
    { nextToken =
        Core.Nothing,
      things = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to get the next set of results, or __null__ if there
-- are no additional results.
listThingsInThingGroupResponse_nextToken :: Lens.Lens' ListThingsInThingGroupResponse (Core.Maybe Core.Text)
listThingsInThingGroupResponse_nextToken = Lens.lens (\ListThingsInThingGroupResponse' {nextToken} -> nextToken) (\s@ListThingsInThingGroupResponse' {} a -> s {nextToken = a} :: ListThingsInThingGroupResponse)

-- | The things in the specified thing group.
listThingsInThingGroupResponse_things :: Lens.Lens' ListThingsInThingGroupResponse (Core.Maybe [Core.Text])
listThingsInThingGroupResponse_things = Lens.lens (\ListThingsInThingGroupResponse' {things} -> things) (\s@ListThingsInThingGroupResponse' {} a -> s {things = a} :: ListThingsInThingGroupResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listThingsInThingGroupResponse_httpStatus :: Lens.Lens' ListThingsInThingGroupResponse Core.Int
listThingsInThingGroupResponse_httpStatus = Lens.lens (\ListThingsInThingGroupResponse' {httpStatus} -> httpStatus) (\s@ListThingsInThingGroupResponse' {} a -> s {httpStatus = a} :: ListThingsInThingGroupResponse)

instance Core.NFData ListThingsInThingGroupResponse
