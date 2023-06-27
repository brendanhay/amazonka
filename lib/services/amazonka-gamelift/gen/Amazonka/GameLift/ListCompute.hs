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
-- Module      : Amazonka.GameLift.ListCompute
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all compute resources registered to a fleet in your Amazon Web
-- Services account. You can filter the result set by location.
--
-- This operation returns paginated results.
module Amazonka.GameLift.ListCompute
  ( -- * Creating a Request
    ListCompute (..),
    newListCompute,

    -- * Request Lenses
    listCompute_limit,
    listCompute_location,
    listCompute_nextToken,
    listCompute_fleetId,

    -- * Destructuring the Response
    ListComputeResponse (..),
    newListComputeResponse,

    -- * Response Lenses
    listComputeResponse_computeList,
    listComputeResponse_nextToken,
    listComputeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListCompute' smart constructor.
data ListCompute = ListCompute'
  { -- | The maximum number of results to return. Use this parameter with
    -- @NextToken@ to get results as a set of sequential pages.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The name of the custom location that the compute resources are assigned
    -- to.
    location :: Prelude.Maybe Prelude.Text,
    -- | A token that indicates the start of the next sequential page of results.
    -- Use the token that is returned with a previous call to this operation.
    -- To start at the beginning of the result set, do not specify a value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the fleet the compute resources are registered
    -- to.
    fleetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCompute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'listCompute_limit' - The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages.
--
-- 'location', 'listCompute_location' - The name of the custom location that the compute resources are assigned
-- to.
--
-- 'nextToken', 'listCompute_nextToken' - A token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
--
-- 'fleetId', 'listCompute_fleetId' - A unique identifier for the fleet the compute resources are registered
-- to.
newListCompute ::
  -- | 'fleetId'
  Prelude.Text ->
  ListCompute
newListCompute pFleetId_ =
  ListCompute'
    { limit = Prelude.Nothing,
      location = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      fleetId = pFleetId_
    }

-- | The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages.
listCompute_limit :: Lens.Lens' ListCompute (Prelude.Maybe Prelude.Natural)
listCompute_limit = Lens.lens (\ListCompute' {limit} -> limit) (\s@ListCompute' {} a -> s {limit = a} :: ListCompute)

-- | The name of the custom location that the compute resources are assigned
-- to.
listCompute_location :: Lens.Lens' ListCompute (Prelude.Maybe Prelude.Text)
listCompute_location = Lens.lens (\ListCompute' {location} -> location) (\s@ListCompute' {} a -> s {location = a} :: ListCompute)

-- | A token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
listCompute_nextToken :: Lens.Lens' ListCompute (Prelude.Maybe Prelude.Text)
listCompute_nextToken = Lens.lens (\ListCompute' {nextToken} -> nextToken) (\s@ListCompute' {} a -> s {nextToken = a} :: ListCompute)

-- | A unique identifier for the fleet the compute resources are registered
-- to.
listCompute_fleetId :: Lens.Lens' ListCompute Prelude.Text
listCompute_fleetId = Lens.lens (\ListCompute' {fleetId} -> fleetId) (\s@ListCompute' {} a -> s {fleetId = a} :: ListCompute)

instance Core.AWSPager ListCompute where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listComputeResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listComputeResponse_computeList
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listCompute_nextToken
          Lens..~ rs
          Lens.^? listComputeResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListCompute where
  type AWSResponse ListCompute = ListComputeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListComputeResponse'
            Prelude.<$> (x Data..?> "ComputeList" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCompute where
  hashWithSalt _salt ListCompute' {..} =
    _salt
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` fleetId

instance Prelude.NFData ListCompute where
  rnf ListCompute' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf fleetId

instance Data.ToHeaders ListCompute where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("GameLift.ListCompute" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListCompute where
  toJSON ListCompute' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Limit" Data..=) Prelude.<$> limit,
            ("Location" Data..=) Prelude.<$> location,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("FleetId" Data..= fleetId)
          ]
      )

instance Data.ToPath ListCompute where
  toPath = Prelude.const "/"

instance Data.ToQuery ListCompute where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListComputeResponse' smart constructor.
data ListComputeResponse = ListComputeResponse'
  { -- | A list of compute resources registered to the fleet you specified.
    computeList :: Prelude.Maybe [Compute],
    -- | A token that indicates where to resume retrieving results on the next
    -- call to this operation. If no token is returned, these results represent
    -- the end of the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListComputeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'computeList', 'listComputeResponse_computeList' - A list of compute resources registered to the fleet you specified.
--
-- 'nextToken', 'listComputeResponse_nextToken' - A token that indicates where to resume retrieving results on the next
-- call to this operation. If no token is returned, these results represent
-- the end of the list.
--
-- 'httpStatus', 'listComputeResponse_httpStatus' - The response's http status code.
newListComputeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListComputeResponse
newListComputeResponse pHttpStatus_ =
  ListComputeResponse'
    { computeList = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of compute resources registered to the fleet you specified.
listComputeResponse_computeList :: Lens.Lens' ListComputeResponse (Prelude.Maybe [Compute])
listComputeResponse_computeList = Lens.lens (\ListComputeResponse' {computeList} -> computeList) (\s@ListComputeResponse' {} a -> s {computeList = a} :: ListComputeResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token that indicates where to resume retrieving results on the next
-- call to this operation. If no token is returned, these results represent
-- the end of the list.
listComputeResponse_nextToken :: Lens.Lens' ListComputeResponse (Prelude.Maybe Prelude.Text)
listComputeResponse_nextToken = Lens.lens (\ListComputeResponse' {nextToken} -> nextToken) (\s@ListComputeResponse' {} a -> s {nextToken = a} :: ListComputeResponse)

-- | The response's http status code.
listComputeResponse_httpStatus :: Lens.Lens' ListComputeResponse Prelude.Int
listComputeResponse_httpStatus = Lens.lens (\ListComputeResponse' {httpStatus} -> httpStatus) (\s@ListComputeResponse' {} a -> s {httpStatus = a} :: ListComputeResponse)

instance Prelude.NFData ListComputeResponse where
  rnf ListComputeResponse' {..} =
    Prelude.rnf computeList
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
