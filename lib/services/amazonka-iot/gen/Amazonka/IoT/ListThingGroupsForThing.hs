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
-- Module      : Amazonka.IoT.ListThingGroupsForThing
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the thing groups to which the specified thing belongs.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions ListThingGroupsForThing>
-- action.
--
-- This operation returns paginated results.
module Amazonka.IoT.ListThingGroupsForThing
  ( -- * Creating a Request
    ListThingGroupsForThing (..),
    newListThingGroupsForThing,

    -- * Request Lenses
    listThingGroupsForThing_maxResults,
    listThingGroupsForThing_nextToken,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListThingGroupsForThing' smart constructor.
data ListThingGroupsForThing = ListThingGroupsForThing'
  { -- | The maximum number of results to return at one time.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | To retrieve the next set of results, the @nextToken@ value from a
    -- previous response; otherwise __null__ to receive the first set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'maxResults', 'listThingGroupsForThing_maxResults' - The maximum number of results to return at one time.
--
-- 'nextToken', 'listThingGroupsForThing_nextToken' - To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
--
-- 'thingName', 'listThingGroupsForThing_thingName' - The thing name.
newListThingGroupsForThing ::
  -- | 'thingName'
  Prelude.Text ->
  ListThingGroupsForThing
newListThingGroupsForThing pThingName_ =
  ListThingGroupsForThing'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      thingName = pThingName_
    }

-- | The maximum number of results to return at one time.
listThingGroupsForThing_maxResults :: Lens.Lens' ListThingGroupsForThing (Prelude.Maybe Prelude.Natural)
listThingGroupsForThing_maxResults = Lens.lens (\ListThingGroupsForThing' {maxResults} -> maxResults) (\s@ListThingGroupsForThing' {} a -> s {maxResults = a} :: ListThingGroupsForThing)

-- | To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
listThingGroupsForThing_nextToken :: Lens.Lens' ListThingGroupsForThing (Prelude.Maybe Prelude.Text)
listThingGroupsForThing_nextToken = Lens.lens (\ListThingGroupsForThing' {nextToken} -> nextToken) (\s@ListThingGroupsForThing' {} a -> s {nextToken = a} :: ListThingGroupsForThing)

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
        Prelude.Just
          Prelude.$ rq
          Prelude.& listThingGroupsForThing_nextToken
          Lens..~ rs
          Lens.^? listThingGroupsForThingResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListThingGroupsForThing where
  type
    AWSResponse ListThingGroupsForThing =
      ListThingGroupsForThingResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListThingGroupsForThingResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "thingGroups" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListThingGroupsForThing where
  hashWithSalt _salt ListThingGroupsForThing' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` thingName

instance Prelude.NFData ListThingGroupsForThing where
  rnf ListThingGroupsForThing' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf thingName

instance Data.ToHeaders ListThingGroupsForThing where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListThingGroupsForThing where
  toPath ListThingGroupsForThing' {..} =
    Prelude.mconcat
      ["/things/", Data.toBS thingName, "/thing-groups"]

instance Data.ToQuery ListThingGroupsForThing where
  toQuery ListThingGroupsForThing' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
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
listThingGroupsForThingResponse_thingGroups = Lens.lens (\ListThingGroupsForThingResponse' {thingGroups} -> thingGroups) (\s@ListThingGroupsForThingResponse' {} a -> s {thingGroups = a} :: ListThingGroupsForThingResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listThingGroupsForThingResponse_httpStatus :: Lens.Lens' ListThingGroupsForThingResponse Prelude.Int
listThingGroupsForThingResponse_httpStatus = Lens.lens (\ListThingGroupsForThingResponse' {httpStatus} -> httpStatus) (\s@ListThingGroupsForThingResponse' {} a -> s {httpStatus = a} :: ListThingGroupsForThingResponse)

instance
  Prelude.NFData
    ListThingGroupsForThingResponse
  where
  rnf ListThingGroupsForThingResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf thingGroups
      `Prelude.seq` Prelude.rnf httpStatus
