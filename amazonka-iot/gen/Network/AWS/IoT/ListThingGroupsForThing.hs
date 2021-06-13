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
-- Module      : Network.AWS.IoT.ListThingGroupsForThing
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the thing groups to which the specified thing belongs.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListThingGroupsForThing
  ( -- * Creating a Request
    ListThingGroupsForThing (..),
    newListThingGroupsForThing,

    -- * Request Lenses
    listThingGroupsForThing_nextToken,
    listThingGroupsForThing_maxResults,
    listThingGroupsForThing_thingName,

    -- * Destructuring the Response
    ListThingGroupsForThingResponse (..),
    newListThingGroupsForThingResponse,

    -- * Response Lenses
    listThingGroupsForThingResponse_nextToken,
    listThingGroupsForThingResponse_thingGroups,
    listThingGroupsForThingResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListThingGroupsForThing' smart constructor.
data ListThingGroupsForThing = ListThingGroupsForThing'
  { -- | To retrieve the next set of results, the @nextToken@ value from a
    -- previous response; otherwise __null__ to receive the first set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return at one time.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The thing name.
    thingName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListThingGroupsForThing' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listThingGroupsForThing_nextToken' - To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
--
-- 'maxResults', 'listThingGroupsForThing_maxResults' - The maximum number of results to return at one time.
--
-- 'thingName', 'listThingGroupsForThing_thingName' - The thing name.
newListThingGroupsForThing ::
  -- | 'thingName'
  Prelude.Text ->
  ListThingGroupsForThing
newListThingGroupsForThing pThingName_ =
  ListThingGroupsForThing'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      thingName = pThingName_
    }

-- | To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
listThingGroupsForThing_nextToken :: Lens.Lens' ListThingGroupsForThing (Prelude.Maybe Prelude.Text)
listThingGroupsForThing_nextToken = Lens.lens (\ListThingGroupsForThing' {nextToken} -> nextToken) (\s@ListThingGroupsForThing' {} a -> s {nextToken = a} :: ListThingGroupsForThing)

-- | The maximum number of results to return at one time.
listThingGroupsForThing_maxResults :: Lens.Lens' ListThingGroupsForThing (Prelude.Maybe Prelude.Natural)
listThingGroupsForThing_maxResults = Lens.lens (\ListThingGroupsForThing' {maxResults} -> maxResults) (\s@ListThingGroupsForThing' {} a -> s {maxResults = a} :: ListThingGroupsForThing)

-- | The thing name.
listThingGroupsForThing_thingName :: Lens.Lens' ListThingGroupsForThing Prelude.Text
listThingGroupsForThing_thingName = Lens.lens (\ListThingGroupsForThing' {thingName} -> thingName) (\s@ListThingGroupsForThing' {} a -> s {thingName = a} :: ListThingGroupsForThing)

instance Core.AWSPager ListThingGroupsForThing where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listThingGroupsForThingResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listThingGroupsForThingResponse_thingGroups
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listThingGroupsForThing_nextToken
          Lens..~ rs
          Lens.^? listThingGroupsForThingResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListThingGroupsForThing where
  type
    AWSResponse ListThingGroupsForThing =
      ListThingGroupsForThingResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListThingGroupsForThingResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "thingGroups" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListThingGroupsForThing

instance Prelude.NFData ListThingGroupsForThing

instance Core.ToHeaders ListThingGroupsForThing where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListThingGroupsForThing where
  toPath ListThingGroupsForThing' {..} =
    Prelude.mconcat
      ["/things/", Core.toBS thingName, "/thing-groups"]

instance Core.ToQuery ListThingGroupsForThing where
  toQuery ListThingGroupsForThing' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListThingGroupsForThingResponse' smart constructor.
data ListThingGroupsForThingResponse = ListThingGroupsForThingResponse'
  { -- | The token to use to get the next set of results, or __null__ if there
    -- are no additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The thing groups.
    thingGroups :: Prelude.Maybe [GroupNameAndArn],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListThingGroupsForThingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listThingGroupsForThingResponse_nextToken' - The token to use to get the next set of results, or __null__ if there
-- are no additional results.
--
-- 'thingGroups', 'listThingGroupsForThingResponse_thingGroups' - The thing groups.
--
-- 'httpStatus', 'listThingGroupsForThingResponse_httpStatus' - The response's http status code.
newListThingGroupsForThingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListThingGroupsForThingResponse
newListThingGroupsForThingResponse pHttpStatus_ =
  ListThingGroupsForThingResponse'
    { nextToken =
        Prelude.Nothing,
      thingGroups = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to get the next set of results, or __null__ if there
-- are no additional results.
listThingGroupsForThingResponse_nextToken :: Lens.Lens' ListThingGroupsForThingResponse (Prelude.Maybe Prelude.Text)
listThingGroupsForThingResponse_nextToken = Lens.lens (\ListThingGroupsForThingResponse' {nextToken} -> nextToken) (\s@ListThingGroupsForThingResponse' {} a -> s {nextToken = a} :: ListThingGroupsForThingResponse)

-- | The thing groups.
listThingGroupsForThingResponse_thingGroups :: Lens.Lens' ListThingGroupsForThingResponse (Prelude.Maybe [GroupNameAndArn])
listThingGroupsForThingResponse_thingGroups = Lens.lens (\ListThingGroupsForThingResponse' {thingGroups} -> thingGroups) (\s@ListThingGroupsForThingResponse' {} a -> s {thingGroups = a} :: ListThingGroupsForThingResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listThingGroupsForThingResponse_httpStatus :: Lens.Lens' ListThingGroupsForThingResponse Prelude.Int
listThingGroupsForThingResponse_httpStatus = Lens.lens (\ListThingGroupsForThingResponse' {httpStatus} -> httpStatus) (\s@ListThingGroupsForThingResponse' {} a -> s {httpStatus = a} :: ListThingGroupsForThingResponse)

instance
  Prelude.NFData
    ListThingGroupsForThingResponse
