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
-- Module      : Amazonka.GameLift.DescribeFleetAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves core fleet-wide properties, including the computing hardware
-- and deployment configuration for all instances in the fleet.
--
-- This operation can be used in the following ways:
--
-- -   To get attributes for one or more specific fleets, provide a list of
--     fleet IDs or fleet ARNs.
--
-- -   To get attributes for all fleets, do not provide a fleet identifier.
--
-- When requesting attributes for multiple fleets, use the pagination
-- parameters to retrieve results as a set of sequential pages.
--
-- If successful, a @FleetAttributes@ object is returned for each fleet
-- requested, unless the fleet identifier is not found.
--
-- Some API operations limit the number of fleet IDs that allowed in one
-- request. If a request exceeds this limit, the request fails and the
-- error message contains the maximum allowed number.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-intro.html Setting up GameLift fleets>
--
-- This operation returns paginated results.
module Amazonka.GameLift.DescribeFleetAttributes
  ( -- * Creating a Request
    DescribeFleetAttributes (..),
    newDescribeFleetAttributes,

    -- * Request Lenses
    describeFleetAttributes_fleetIds,
    describeFleetAttributes_limit,
    describeFleetAttributes_nextToken,

    -- * Destructuring the Response
    DescribeFleetAttributesResponse (..),
    newDescribeFleetAttributesResponse,

    -- * Response Lenses
    describeFleetAttributesResponse_fleetAttributes,
    describeFleetAttributesResponse_nextToken,
    describeFleetAttributesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeFleetAttributes' smart constructor.
data DescribeFleetAttributes = DescribeFleetAttributes'
  { -- | A list of unique fleet identifiers to retrieve attributes for. You can
    -- use either the fleet ID or ARN value. To retrieve attributes for all
    -- current fleets, do not include this parameter.
    fleetIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The maximum number of results to return. Use this parameter with
    -- @NextToken@ to get results as a set of sequential pages. This parameter
    -- is ignored when the request specifies one or a list of fleet IDs.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | A token that indicates the start of the next sequential page of results.
    -- Use the token that is returned with a previous call to this operation.
    -- To start at the beginning of the result set, do not specify a value.
    -- This parameter is ignored when the request specifies one or a list of
    -- fleet IDs.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFleetAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetIds', 'describeFleetAttributes_fleetIds' - A list of unique fleet identifiers to retrieve attributes for. You can
-- use either the fleet ID or ARN value. To retrieve attributes for all
-- current fleets, do not include this parameter.
--
-- 'limit', 'describeFleetAttributes_limit' - The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages. This parameter
-- is ignored when the request specifies one or a list of fleet IDs.
--
-- 'nextToken', 'describeFleetAttributes_nextToken' - A token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
-- This parameter is ignored when the request specifies one or a list of
-- fleet IDs.
newDescribeFleetAttributes ::
  DescribeFleetAttributes
newDescribeFleetAttributes =
  DescribeFleetAttributes'
    { fleetIds =
        Prelude.Nothing,
      limit = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | A list of unique fleet identifiers to retrieve attributes for. You can
-- use either the fleet ID or ARN value. To retrieve attributes for all
-- current fleets, do not include this parameter.
describeFleetAttributes_fleetIds :: Lens.Lens' DescribeFleetAttributes (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
describeFleetAttributes_fleetIds = Lens.lens (\DescribeFleetAttributes' {fleetIds} -> fleetIds) (\s@DescribeFleetAttributes' {} a -> s {fleetIds = a} :: DescribeFleetAttributes) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages. This parameter
-- is ignored when the request specifies one or a list of fleet IDs.
describeFleetAttributes_limit :: Lens.Lens' DescribeFleetAttributes (Prelude.Maybe Prelude.Natural)
describeFleetAttributes_limit = Lens.lens (\DescribeFleetAttributes' {limit} -> limit) (\s@DescribeFleetAttributes' {} a -> s {limit = a} :: DescribeFleetAttributes)

-- | A token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
-- This parameter is ignored when the request specifies one or a list of
-- fleet IDs.
describeFleetAttributes_nextToken :: Lens.Lens' DescribeFleetAttributes (Prelude.Maybe Prelude.Text)
describeFleetAttributes_nextToken = Lens.lens (\DescribeFleetAttributes' {nextToken} -> nextToken) (\s@DescribeFleetAttributes' {} a -> s {nextToken = a} :: DescribeFleetAttributes)

instance Core.AWSPager DescribeFleetAttributes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeFleetAttributesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeFleetAttributesResponse_fleetAttributes
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeFleetAttributes_nextToken
          Lens..~ rs
          Lens.^? describeFleetAttributesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeFleetAttributes where
  type
    AWSResponse DescribeFleetAttributes =
      DescribeFleetAttributesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFleetAttributesResponse'
            Prelude.<$> ( x
                            Data..?> "FleetAttributes"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeFleetAttributes where
  hashWithSalt _salt DescribeFleetAttributes' {..} =
    _salt
      `Prelude.hashWithSalt` fleetIds
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeFleetAttributes where
  rnf DescribeFleetAttributes' {..} =
    Prelude.rnf fleetIds
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders DescribeFleetAttributes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GameLift.DescribeFleetAttributes" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeFleetAttributes where
  toJSON DescribeFleetAttributes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FleetIds" Data..=) Prelude.<$> fleetIds,
            ("Limit" Data..=) Prelude.<$> limit,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath DescribeFleetAttributes where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeFleetAttributes where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeFleetAttributesResponse' smart constructor.
data DescribeFleetAttributesResponse = DescribeFleetAttributesResponse'
  { -- | A collection of objects containing attribute metadata for each requested
    -- fleet ID. Attribute objects are returned only for fleets that currently
    -- exist.
    fleetAttributes :: Prelude.Maybe [FleetAttributes],
    -- | A token that indicates where to resume retrieving results on the next
    -- call to this operation. If no token is returned, these results represent
    -- the end of the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFleetAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetAttributes', 'describeFleetAttributesResponse_fleetAttributes' - A collection of objects containing attribute metadata for each requested
-- fleet ID. Attribute objects are returned only for fleets that currently
-- exist.
--
-- 'nextToken', 'describeFleetAttributesResponse_nextToken' - A token that indicates where to resume retrieving results on the next
-- call to this operation. If no token is returned, these results represent
-- the end of the list.
--
-- 'httpStatus', 'describeFleetAttributesResponse_httpStatus' - The response's http status code.
newDescribeFleetAttributesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeFleetAttributesResponse
newDescribeFleetAttributesResponse pHttpStatus_ =
  DescribeFleetAttributesResponse'
    { fleetAttributes =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A collection of objects containing attribute metadata for each requested
-- fleet ID. Attribute objects are returned only for fleets that currently
-- exist.
describeFleetAttributesResponse_fleetAttributes :: Lens.Lens' DescribeFleetAttributesResponse (Prelude.Maybe [FleetAttributes])
describeFleetAttributesResponse_fleetAttributes = Lens.lens (\DescribeFleetAttributesResponse' {fleetAttributes} -> fleetAttributes) (\s@DescribeFleetAttributesResponse' {} a -> s {fleetAttributes = a} :: DescribeFleetAttributesResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token that indicates where to resume retrieving results on the next
-- call to this operation. If no token is returned, these results represent
-- the end of the list.
describeFleetAttributesResponse_nextToken :: Lens.Lens' DescribeFleetAttributesResponse (Prelude.Maybe Prelude.Text)
describeFleetAttributesResponse_nextToken = Lens.lens (\DescribeFleetAttributesResponse' {nextToken} -> nextToken) (\s@DescribeFleetAttributesResponse' {} a -> s {nextToken = a} :: DescribeFleetAttributesResponse)

-- | The response's http status code.
describeFleetAttributesResponse_httpStatus :: Lens.Lens' DescribeFleetAttributesResponse Prelude.Int
describeFleetAttributesResponse_httpStatus = Lens.lens (\DescribeFleetAttributesResponse' {httpStatus} -> httpStatus) (\s@DescribeFleetAttributesResponse' {} a -> s {httpStatus = a} :: DescribeFleetAttributesResponse)

instance
  Prelude.NFData
    DescribeFleetAttributesResponse
  where
  rnf DescribeFleetAttributesResponse' {..} =
    Prelude.rnf fleetAttributes
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
