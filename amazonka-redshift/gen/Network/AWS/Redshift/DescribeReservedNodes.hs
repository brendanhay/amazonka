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
-- Module      : Network.AWS.Redshift.DescribeReservedNodes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the descriptions of the reserved nodes.
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeReservedNodes
  ( -- * Creating a Request
    DescribeReservedNodes (..),
    newDescribeReservedNodes,

    -- * Request Lenses
    describeReservedNodes_reservedNodeId,
    describeReservedNodes_marker,
    describeReservedNodes_maxRecords,

    -- * Destructuring the Response
    DescribeReservedNodesResponse (..),
    newDescribeReservedNodesResponse,

    -- * Response Lenses
    describeReservedNodesResponse_reservedNodes,
    describeReservedNodesResponse_marker,
    describeReservedNodesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeReservedNodes' smart constructor.
data DescribeReservedNodes = DescribeReservedNodes'
  { -- | Identifier for the node reservation.
    reservedNodeId :: Core.Maybe Core.Text,
    -- | An optional parameter that specifies the starting point to return a set
    -- of response records. When the results of a DescribeReservedNodes request
    -- exceed the value specified in @MaxRecords@, AWS returns a value in the
    -- @Marker@ field of the response. You can retrieve the next set of
    -- response records by providing the returned marker value in the @Marker@
    -- parameter and retrying the request.
    marker :: Core.Maybe Core.Text,
    -- | The maximum number of response records to return in each call. If the
    -- number of remaining response records exceeds the specified @MaxRecords@
    -- value, a value is returned in a @marker@ field of the response. You can
    -- retrieve the next set of records by retrying the command with the
    -- returned marker value.
    --
    -- Default: @100@
    --
    -- Constraints: minimum 20, maximum 100.
    maxRecords :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeReservedNodes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reservedNodeId', 'describeReservedNodes_reservedNodeId' - Identifier for the node reservation.
--
-- 'marker', 'describeReservedNodes_marker' - An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeReservedNodes request
-- exceed the value specified in @MaxRecords@, AWS returns a value in the
-- @Marker@ field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the @Marker@
-- parameter and retrying the request.
--
-- 'maxRecords', 'describeReservedNodes_maxRecords' - The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
newDescribeReservedNodes ::
  DescribeReservedNodes
newDescribeReservedNodes =
  DescribeReservedNodes'
    { reservedNodeId =
        Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | Identifier for the node reservation.
describeReservedNodes_reservedNodeId :: Lens.Lens' DescribeReservedNodes (Core.Maybe Core.Text)
describeReservedNodes_reservedNodeId = Lens.lens (\DescribeReservedNodes' {reservedNodeId} -> reservedNodeId) (\s@DescribeReservedNodes' {} a -> s {reservedNodeId = a} :: DescribeReservedNodes)

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeReservedNodes request
-- exceed the value specified in @MaxRecords@, AWS returns a value in the
-- @Marker@ field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the @Marker@
-- parameter and retrying the request.
describeReservedNodes_marker :: Lens.Lens' DescribeReservedNodes (Core.Maybe Core.Text)
describeReservedNodes_marker = Lens.lens (\DescribeReservedNodes' {marker} -> marker) (\s@DescribeReservedNodes' {} a -> s {marker = a} :: DescribeReservedNodes)

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
describeReservedNodes_maxRecords :: Lens.Lens' DescribeReservedNodes (Core.Maybe Core.Int)
describeReservedNodes_maxRecords = Lens.lens (\DescribeReservedNodes' {maxRecords} -> maxRecords) (\s@DescribeReservedNodes' {} a -> s {maxRecords = a} :: DescribeReservedNodes)

instance Core.AWSPager DescribeReservedNodes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeReservedNodesResponse_marker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeReservedNodesResponse_reservedNodes
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeReservedNodes_marker
          Lens..~ rs
          Lens.^? describeReservedNodesResponse_marker
            Core.. Lens._Just

instance Core.AWSRequest DescribeReservedNodes where
  type
    AWSResponse DescribeReservedNodes =
      DescribeReservedNodesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeReservedNodesResult"
      ( \s h x ->
          DescribeReservedNodesResponse'
            Core.<$> ( x Core..@? "ReservedNodes" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "ReservedNode")
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeReservedNodes

instance Core.NFData DescribeReservedNodes

instance Core.ToHeaders DescribeReservedNodes where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeReservedNodes where
  toPath = Core.const "/"

instance Core.ToQuery DescribeReservedNodes where
  toQuery DescribeReservedNodes' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeReservedNodes" :: Core.ByteString),
        "Version" Core.=: ("2012-12-01" :: Core.ByteString),
        "ReservedNodeId" Core.=: reservedNodeId,
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords
      ]

-- |
--
-- /See:/ 'newDescribeReservedNodesResponse' smart constructor.
data DescribeReservedNodesResponse = DescribeReservedNodesResponse'
  { -- | The list of @ReservedNode@ objects.
    reservedNodes :: Core.Maybe [ReservedNode],
    -- | A value that indicates the starting point for the next set of response
    -- records in a subsequent request. If a value is returned in a response,
    -- you can retrieve the next set of records by providing this returned
    -- marker value in the @Marker@ parameter and retrying the command. If the
    -- @Marker@ field is empty, all response records have been retrieved for
    -- the request.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeReservedNodesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reservedNodes', 'describeReservedNodesResponse_reservedNodes' - The list of @ReservedNode@ objects.
--
-- 'marker', 'describeReservedNodesResponse_marker' - A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
--
-- 'httpStatus', 'describeReservedNodesResponse_httpStatus' - The response's http status code.
newDescribeReservedNodesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeReservedNodesResponse
newDescribeReservedNodesResponse pHttpStatus_ =
  DescribeReservedNodesResponse'
    { reservedNodes =
        Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of @ReservedNode@ objects.
describeReservedNodesResponse_reservedNodes :: Lens.Lens' DescribeReservedNodesResponse (Core.Maybe [ReservedNode])
describeReservedNodesResponse_reservedNodes = Lens.lens (\DescribeReservedNodesResponse' {reservedNodes} -> reservedNodes) (\s@DescribeReservedNodesResponse' {} a -> s {reservedNodes = a} :: DescribeReservedNodesResponse) Core.. Lens.mapping Lens._Coerce

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
describeReservedNodesResponse_marker :: Lens.Lens' DescribeReservedNodesResponse (Core.Maybe Core.Text)
describeReservedNodesResponse_marker = Lens.lens (\DescribeReservedNodesResponse' {marker} -> marker) (\s@DescribeReservedNodesResponse' {} a -> s {marker = a} :: DescribeReservedNodesResponse)

-- | The response's http status code.
describeReservedNodesResponse_httpStatus :: Lens.Lens' DescribeReservedNodesResponse Core.Int
describeReservedNodesResponse_httpStatus = Lens.lens (\DescribeReservedNodesResponse' {httpStatus} -> httpStatus) (\s@DescribeReservedNodesResponse' {} a -> s {httpStatus = a} :: DescribeReservedNodesResponse)

instance Core.NFData DescribeReservedNodesResponse
