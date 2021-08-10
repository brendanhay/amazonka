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
-- Module      : Network.AWS.EC2.GetGroupsForCapacityReservation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the resource groups to which a Capacity Reservation has been
-- added.
--
-- This operation returns paginated results.
module Network.AWS.EC2.GetGroupsForCapacityReservation
  ( -- * Creating a Request
    GetGroupsForCapacityReservation (..),
    newGetGroupsForCapacityReservation,

    -- * Request Lenses
    getGroupsForCapacityReservation_nextToken,
    getGroupsForCapacityReservation_dryRun,
    getGroupsForCapacityReservation_maxResults,
    getGroupsForCapacityReservation_capacityReservationId,

    -- * Destructuring the Response
    GetGroupsForCapacityReservationResponse (..),
    newGetGroupsForCapacityReservationResponse,

    -- * Response Lenses
    getGroupsForCapacityReservationResponse_nextToken,
    getGroupsForCapacityReservationResponse_capacityReservationGroups,
    getGroupsForCapacityReservationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetGroupsForCapacityReservation' smart constructor.
data GetGroupsForCapacityReservation = GetGroupsForCapacityReservation'
  { -- | The token to use to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results to return for the request in a single
    -- page. The remaining results can be seen by sending another request with
    -- the returned @nextToken@ value. This value can be between 5 and 500. If
    -- @maxResults@ is given a larger value than 500, you receive an error.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the Capacity Reservation.
    capacityReservationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetGroupsForCapacityReservation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getGroupsForCapacityReservation_nextToken' - The token to use to retrieve the next page of results.
--
-- 'dryRun', 'getGroupsForCapacityReservation_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'getGroupsForCapacityReservation_maxResults' - The maximum number of results to return for the request in a single
-- page. The remaining results can be seen by sending another request with
-- the returned @nextToken@ value. This value can be between 5 and 500. If
-- @maxResults@ is given a larger value than 500, you receive an error.
--
-- 'capacityReservationId', 'getGroupsForCapacityReservation_capacityReservationId' - The ID of the Capacity Reservation.
newGetGroupsForCapacityReservation ::
  -- | 'capacityReservationId'
  Prelude.Text ->
  GetGroupsForCapacityReservation
newGetGroupsForCapacityReservation
  pCapacityReservationId_ =
    GetGroupsForCapacityReservation'
      { nextToken =
          Prelude.Nothing,
        dryRun = Prelude.Nothing,
        maxResults = Prelude.Nothing,
        capacityReservationId =
          pCapacityReservationId_
      }

-- | The token to use to retrieve the next page of results.
getGroupsForCapacityReservation_nextToken :: Lens.Lens' GetGroupsForCapacityReservation (Prelude.Maybe Prelude.Text)
getGroupsForCapacityReservation_nextToken = Lens.lens (\GetGroupsForCapacityReservation' {nextToken} -> nextToken) (\s@GetGroupsForCapacityReservation' {} a -> s {nextToken = a} :: GetGroupsForCapacityReservation)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
getGroupsForCapacityReservation_dryRun :: Lens.Lens' GetGroupsForCapacityReservation (Prelude.Maybe Prelude.Bool)
getGroupsForCapacityReservation_dryRun = Lens.lens (\GetGroupsForCapacityReservation' {dryRun} -> dryRun) (\s@GetGroupsForCapacityReservation' {} a -> s {dryRun = a} :: GetGroupsForCapacityReservation)

-- | The maximum number of results to return for the request in a single
-- page. The remaining results can be seen by sending another request with
-- the returned @nextToken@ value. This value can be between 5 and 500. If
-- @maxResults@ is given a larger value than 500, you receive an error.
getGroupsForCapacityReservation_maxResults :: Lens.Lens' GetGroupsForCapacityReservation (Prelude.Maybe Prelude.Natural)
getGroupsForCapacityReservation_maxResults = Lens.lens (\GetGroupsForCapacityReservation' {maxResults} -> maxResults) (\s@GetGroupsForCapacityReservation' {} a -> s {maxResults = a} :: GetGroupsForCapacityReservation)

-- | The ID of the Capacity Reservation.
getGroupsForCapacityReservation_capacityReservationId :: Lens.Lens' GetGroupsForCapacityReservation Prelude.Text
getGroupsForCapacityReservation_capacityReservationId = Lens.lens (\GetGroupsForCapacityReservation' {capacityReservationId} -> capacityReservationId) (\s@GetGroupsForCapacityReservation' {} a -> s {capacityReservationId = a} :: GetGroupsForCapacityReservation)

instance
  Core.AWSPager
    GetGroupsForCapacityReservation
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getGroupsForCapacityReservationResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getGroupsForCapacityReservationResponse_capacityReservationGroups
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getGroupsForCapacityReservation_nextToken
          Lens..~ rs
          Lens.^? getGroupsForCapacityReservationResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    GetGroupsForCapacityReservation
  where
  type
    AWSResponse GetGroupsForCapacityReservation =
      GetGroupsForCapacityReservationResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetGroupsForCapacityReservationResponse'
            Prelude.<$> (x Core..@? "nextToken")
            Prelude.<*> ( x Core..@? "capacityReservationGroupSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetGroupsForCapacityReservation

instance
  Prelude.NFData
    GetGroupsForCapacityReservation

instance
  Core.ToHeaders
    GetGroupsForCapacityReservation
  where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetGroupsForCapacityReservation where
  toPath = Prelude.const "/"

instance Core.ToQuery GetGroupsForCapacityReservation where
  toQuery GetGroupsForCapacityReservation' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "GetGroupsForCapacityReservation" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "NextToken" Core.=: nextToken,
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        "CapacityReservationId"
          Core.=: capacityReservationId
      ]

-- | /See:/ 'newGetGroupsForCapacityReservationResponse' smart constructor.
data GetGroupsForCapacityReservationResponse = GetGroupsForCapacityReservationResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the resource groups to which the Capacity Reservation
    -- has been added.
    capacityReservationGroups :: Prelude.Maybe [CapacityReservationGroup],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetGroupsForCapacityReservationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getGroupsForCapacityReservationResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'capacityReservationGroups', 'getGroupsForCapacityReservationResponse_capacityReservationGroups' - Information about the resource groups to which the Capacity Reservation
-- has been added.
--
-- 'httpStatus', 'getGroupsForCapacityReservationResponse_httpStatus' - The response's http status code.
newGetGroupsForCapacityReservationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetGroupsForCapacityReservationResponse
newGetGroupsForCapacityReservationResponse
  pHttpStatus_ =
    GetGroupsForCapacityReservationResponse'
      { nextToken =
          Prelude.Nothing,
        capacityReservationGroups =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
getGroupsForCapacityReservationResponse_nextToken :: Lens.Lens' GetGroupsForCapacityReservationResponse (Prelude.Maybe Prelude.Text)
getGroupsForCapacityReservationResponse_nextToken = Lens.lens (\GetGroupsForCapacityReservationResponse' {nextToken} -> nextToken) (\s@GetGroupsForCapacityReservationResponse' {} a -> s {nextToken = a} :: GetGroupsForCapacityReservationResponse)

-- | Information about the resource groups to which the Capacity Reservation
-- has been added.
getGroupsForCapacityReservationResponse_capacityReservationGroups :: Lens.Lens' GetGroupsForCapacityReservationResponse (Prelude.Maybe [CapacityReservationGroup])
getGroupsForCapacityReservationResponse_capacityReservationGroups = Lens.lens (\GetGroupsForCapacityReservationResponse' {capacityReservationGroups} -> capacityReservationGroups) (\s@GetGroupsForCapacityReservationResponse' {} a -> s {capacityReservationGroups = a} :: GetGroupsForCapacityReservationResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getGroupsForCapacityReservationResponse_httpStatus :: Lens.Lens' GetGroupsForCapacityReservationResponse Prelude.Int
getGroupsForCapacityReservationResponse_httpStatus = Lens.lens (\GetGroupsForCapacityReservationResponse' {httpStatus} -> httpStatus) (\s@GetGroupsForCapacityReservationResponse' {} a -> s {httpStatus = a} :: GetGroupsForCapacityReservationResponse)

instance
  Prelude.NFData
    GetGroupsForCapacityReservationResponse
