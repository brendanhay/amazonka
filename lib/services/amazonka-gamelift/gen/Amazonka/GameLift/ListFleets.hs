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
-- Module      : Amazonka.GameLift.ListFleets
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a collection of fleet resources in an Amazon Web Services
-- Region. You can call this operation to get fleets in a previously
-- selected default Region (see
-- <https://docs.aws.amazon.com/credref/latest/refdocs/setting-global-region.html>or
-- specify a Region in your request. You can filter the result set to find
-- only those fleets that are deployed with a specific build or script. For
-- fleets that have multiple locations, this operation retrieves fleets
-- based on their home Region only.
--
-- This operation can be used in the following ways:
--
-- -   To get a list of all fleets in a Region, don\'t provide a build or
--     script identifier.
--
-- -   To get a list of all fleets where a specific custom game build is
--     deployed, provide the build ID.
--
-- -   To get a list of all Realtime Servers fleets with a specific
--     configuration script, provide the script ID.
--
-- Use the pagination parameters to retrieve results as a set of sequential
-- pages.
--
-- If successful, a list of fleet IDs that match the request parameters is
-- returned. A NextToken value is also returned if there are more result
-- pages to retrieve.
--
-- Fleet resources are not listed in a particular order.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-intro.html Setting up GameLift fleets>
--
-- This operation returns paginated results.
module Amazonka.GameLift.ListFleets
  ( -- * Creating a Request
    ListFleets (..),
    newListFleets,

    -- * Request Lenses
    listFleets_buildId,
    listFleets_limit,
    listFleets_nextToken,
    listFleets_scriptId,

    -- * Destructuring the Response
    ListFleetsResponse (..),
    newListFleetsResponse,

    -- * Response Lenses
    listFleetsResponse_fleetIds,
    listFleetsResponse_nextToken,
    listFleetsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListFleets' smart constructor.
data ListFleets = ListFleets'
  { -- | A unique identifier for the build to request fleets for. Use this
    -- parameter to return only fleets using a specified build. Use either the
    -- build ID or ARN value.
    buildId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return. Use this parameter with
    -- @NextToken@ to get results as a set of sequential pages.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | A token that indicates the start of the next sequential page of results.
    -- Use the token that is returned with a previous call to this operation.
    -- To start at the beginning of the result set, do not specify a value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the Realtime script to request fleets for. Use
    -- this parameter to return only fleets using a specified script. Use
    -- either the script ID or ARN value.
    scriptId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFleets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'buildId', 'listFleets_buildId' - A unique identifier for the build to request fleets for. Use this
-- parameter to return only fleets using a specified build. Use either the
-- build ID or ARN value.
--
-- 'limit', 'listFleets_limit' - The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages.
--
-- 'nextToken', 'listFleets_nextToken' - A token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
--
-- 'scriptId', 'listFleets_scriptId' - A unique identifier for the Realtime script to request fleets for. Use
-- this parameter to return only fleets using a specified script. Use
-- either the script ID or ARN value.
newListFleets ::
  ListFleets
newListFleets =
  ListFleets'
    { buildId = Prelude.Nothing,
      limit = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      scriptId = Prelude.Nothing
    }

-- | A unique identifier for the build to request fleets for. Use this
-- parameter to return only fleets using a specified build. Use either the
-- build ID or ARN value.
listFleets_buildId :: Lens.Lens' ListFleets (Prelude.Maybe Prelude.Text)
listFleets_buildId = Lens.lens (\ListFleets' {buildId} -> buildId) (\s@ListFleets' {} a -> s {buildId = a} :: ListFleets)

-- | The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages.
listFleets_limit :: Lens.Lens' ListFleets (Prelude.Maybe Prelude.Natural)
listFleets_limit = Lens.lens (\ListFleets' {limit} -> limit) (\s@ListFleets' {} a -> s {limit = a} :: ListFleets)

-- | A token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
listFleets_nextToken :: Lens.Lens' ListFleets (Prelude.Maybe Prelude.Text)
listFleets_nextToken = Lens.lens (\ListFleets' {nextToken} -> nextToken) (\s@ListFleets' {} a -> s {nextToken = a} :: ListFleets)

-- | A unique identifier for the Realtime script to request fleets for. Use
-- this parameter to return only fleets using a specified script. Use
-- either the script ID or ARN value.
listFleets_scriptId :: Lens.Lens' ListFleets (Prelude.Maybe Prelude.Text)
listFleets_scriptId = Lens.lens (\ListFleets' {scriptId} -> scriptId) (\s@ListFleets' {} a -> s {scriptId = a} :: ListFleets)

instance Core.AWSPager ListFleets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listFleetsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listFleetsResponse_fleetIds
            Prelude.. Lens._Just
            Prelude.. Lens.to Prelude.toList
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listFleets_nextToken
          Lens..~ rs
          Lens.^? listFleetsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListFleets where
  type AWSResponse ListFleets = ListFleetsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFleetsResponse'
            Prelude.<$> (x Data..?> "FleetIds")
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListFleets where
  hashWithSalt _salt ListFleets' {..} =
    _salt
      `Prelude.hashWithSalt` buildId
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` scriptId

instance Prelude.NFData ListFleets where
  rnf ListFleets' {..} =
    Prelude.rnf buildId
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf scriptId

instance Data.ToHeaders ListFleets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("GameLift.ListFleets" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListFleets where
  toJSON ListFleets' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BuildId" Data..=) Prelude.<$> buildId,
            ("Limit" Data..=) Prelude.<$> limit,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("ScriptId" Data..=) Prelude.<$> scriptId
          ]
      )

instance Data.ToPath ListFleets where
  toPath = Prelude.const "/"

instance Data.ToQuery ListFleets where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListFleetsResponse' smart constructor.
data ListFleetsResponse = ListFleetsResponse'
  { -- | A set of fleet IDs that match the list request.
    fleetIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A token that indicates where to resume retrieving results on the next
    -- call to this operation. If no token is returned, these results represent
    -- the end of the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFleetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetIds', 'listFleetsResponse_fleetIds' - A set of fleet IDs that match the list request.
--
-- 'nextToken', 'listFleetsResponse_nextToken' - A token that indicates where to resume retrieving results on the next
-- call to this operation. If no token is returned, these results represent
-- the end of the list.
--
-- 'httpStatus', 'listFleetsResponse_httpStatus' - The response's http status code.
newListFleetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListFleetsResponse
newListFleetsResponse pHttpStatus_ =
  ListFleetsResponse'
    { fleetIds = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A set of fleet IDs that match the list request.
listFleetsResponse_fleetIds :: Lens.Lens' ListFleetsResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
listFleetsResponse_fleetIds = Lens.lens (\ListFleetsResponse' {fleetIds} -> fleetIds) (\s@ListFleetsResponse' {} a -> s {fleetIds = a} :: ListFleetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token that indicates where to resume retrieving results on the next
-- call to this operation. If no token is returned, these results represent
-- the end of the list.
listFleetsResponse_nextToken :: Lens.Lens' ListFleetsResponse (Prelude.Maybe Prelude.Text)
listFleetsResponse_nextToken = Lens.lens (\ListFleetsResponse' {nextToken} -> nextToken) (\s@ListFleetsResponse' {} a -> s {nextToken = a} :: ListFleetsResponse)

-- | The response's http status code.
listFleetsResponse_httpStatus :: Lens.Lens' ListFleetsResponse Prelude.Int
listFleetsResponse_httpStatus = Lens.lens (\ListFleetsResponse' {httpStatus} -> httpStatus) (\s@ListFleetsResponse' {} a -> s {httpStatus = a} :: ListFleetsResponse)

instance Prelude.NFData ListFleetsResponse where
  rnf ListFleetsResponse' {..} =
    Prelude.rnf fleetIds
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
