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
-- Module      : Network.AWS.Redshift.GetReservedNodeExchangeOfferings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of DC2 ReservedNodeOfferings that matches the payment
-- type, term, and usage price of the given DC1 reserved node.
--
-- This operation returns paginated results.
module Network.AWS.Redshift.GetReservedNodeExchangeOfferings
  ( -- * Creating a Request
    GetReservedNodeExchangeOfferings (..),
    newGetReservedNodeExchangeOfferings,

    -- * Request Lenses
    getReservedNodeExchangeOfferings_marker,
    getReservedNodeExchangeOfferings_maxRecords,
    getReservedNodeExchangeOfferings_reservedNodeId,

    -- * Destructuring the Response
    GetReservedNodeExchangeOfferingsResponse (..),
    newGetReservedNodeExchangeOfferingsResponse,

    -- * Response Lenses
    getReservedNodeExchangeOfferingsResponse_reservedNodeOfferings,
    getReservedNodeExchangeOfferingsResponse_marker,
    getReservedNodeExchangeOfferingsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newGetReservedNodeExchangeOfferings' smart constructor.
data GetReservedNodeExchangeOfferings = GetReservedNodeExchangeOfferings'
  { -- | A value that indicates the starting point for the next set of
    -- ReservedNodeOfferings.
    marker :: Prelude.Maybe Prelude.Text,
    -- | An integer setting the maximum number of ReservedNodeOfferings to
    -- retrieve.
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | A string representing the node identifier for the DC1 Reserved Node to
    -- be exchanged.
    reservedNodeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetReservedNodeExchangeOfferings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'getReservedNodeExchangeOfferings_marker' - A value that indicates the starting point for the next set of
-- ReservedNodeOfferings.
--
-- 'maxRecords', 'getReservedNodeExchangeOfferings_maxRecords' - An integer setting the maximum number of ReservedNodeOfferings to
-- retrieve.
--
-- 'reservedNodeId', 'getReservedNodeExchangeOfferings_reservedNodeId' - A string representing the node identifier for the DC1 Reserved Node to
-- be exchanged.
newGetReservedNodeExchangeOfferings ::
  -- | 'reservedNodeId'
  Prelude.Text ->
  GetReservedNodeExchangeOfferings
newGetReservedNodeExchangeOfferings pReservedNodeId_ =
  GetReservedNodeExchangeOfferings'
    { marker =
        Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      reservedNodeId = pReservedNodeId_
    }

-- | A value that indicates the starting point for the next set of
-- ReservedNodeOfferings.
getReservedNodeExchangeOfferings_marker :: Lens.Lens' GetReservedNodeExchangeOfferings (Prelude.Maybe Prelude.Text)
getReservedNodeExchangeOfferings_marker = Lens.lens (\GetReservedNodeExchangeOfferings' {marker} -> marker) (\s@GetReservedNodeExchangeOfferings' {} a -> s {marker = a} :: GetReservedNodeExchangeOfferings)

-- | An integer setting the maximum number of ReservedNodeOfferings to
-- retrieve.
getReservedNodeExchangeOfferings_maxRecords :: Lens.Lens' GetReservedNodeExchangeOfferings (Prelude.Maybe Prelude.Int)
getReservedNodeExchangeOfferings_maxRecords = Lens.lens (\GetReservedNodeExchangeOfferings' {maxRecords} -> maxRecords) (\s@GetReservedNodeExchangeOfferings' {} a -> s {maxRecords = a} :: GetReservedNodeExchangeOfferings)

-- | A string representing the node identifier for the DC1 Reserved Node to
-- be exchanged.
getReservedNodeExchangeOfferings_reservedNodeId :: Lens.Lens' GetReservedNodeExchangeOfferings Prelude.Text
getReservedNodeExchangeOfferings_reservedNodeId = Lens.lens (\GetReservedNodeExchangeOfferings' {reservedNodeId} -> reservedNodeId) (\s@GetReservedNodeExchangeOfferings' {} a -> s {reservedNodeId = a} :: GetReservedNodeExchangeOfferings)

instance
  Core.AWSPager
    GetReservedNodeExchangeOfferings
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getReservedNodeExchangeOfferingsResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getReservedNodeExchangeOfferingsResponse_reservedNodeOfferings
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getReservedNodeExchangeOfferings_marker
          Lens..~ rs
          Lens.^? getReservedNodeExchangeOfferingsResponse_marker
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    GetReservedNodeExchangeOfferings
  where
  type
    AWSResponse GetReservedNodeExchangeOfferings =
      GetReservedNodeExchangeOfferingsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "GetReservedNodeExchangeOfferingsResult"
      ( \s h x ->
          GetReservedNodeExchangeOfferingsResponse'
            Prelude.<$> ( x Core..@? "ReservedNodeOfferings"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "ReservedNodeOffering")
                        )
            Prelude.<*> (x Core..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetReservedNodeExchangeOfferings

instance
  Prelude.NFData
    GetReservedNodeExchangeOfferings

instance
  Core.ToHeaders
    GetReservedNodeExchangeOfferings
  where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetReservedNodeExchangeOfferings where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    GetReservedNodeExchangeOfferings
  where
  toQuery GetReservedNodeExchangeOfferings' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "GetReservedNodeExchangeOfferings" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2012-12-01" :: Prelude.ByteString),
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords,
        "ReservedNodeId" Core.=: reservedNodeId
      ]

-- | /See:/ 'newGetReservedNodeExchangeOfferingsResponse' smart constructor.
data GetReservedNodeExchangeOfferingsResponse = GetReservedNodeExchangeOfferingsResponse'
  { -- | Returns an array of ReservedNodeOffering objects.
    reservedNodeOfferings :: Prelude.Maybe [ReservedNodeOffering],
    -- | An optional parameter that specifies the starting point for returning a
    -- set of response records. When the results of a
    -- @GetReservedNodeExchangeOfferings@ request exceed the value specified in
    -- MaxRecords, Amazon Redshift returns a value in the marker field of the
    -- response. You can retrieve the next set of response records by providing
    -- the returned marker value in the marker parameter and retrying the
    -- request.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetReservedNodeExchangeOfferingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reservedNodeOfferings', 'getReservedNodeExchangeOfferingsResponse_reservedNodeOfferings' - Returns an array of ReservedNodeOffering objects.
--
-- 'marker', 'getReservedNodeExchangeOfferingsResponse_marker' - An optional parameter that specifies the starting point for returning a
-- set of response records. When the results of a
-- @GetReservedNodeExchangeOfferings@ request exceed the value specified in
-- MaxRecords, Amazon Redshift returns a value in the marker field of the
-- response. You can retrieve the next set of response records by providing
-- the returned marker value in the marker parameter and retrying the
-- request.
--
-- 'httpStatus', 'getReservedNodeExchangeOfferingsResponse_httpStatus' - The response's http status code.
newGetReservedNodeExchangeOfferingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetReservedNodeExchangeOfferingsResponse
newGetReservedNodeExchangeOfferingsResponse
  pHttpStatus_ =
    GetReservedNodeExchangeOfferingsResponse'
      { reservedNodeOfferings =
          Prelude.Nothing,
        marker = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Returns an array of ReservedNodeOffering objects.
getReservedNodeExchangeOfferingsResponse_reservedNodeOfferings :: Lens.Lens' GetReservedNodeExchangeOfferingsResponse (Prelude.Maybe [ReservedNodeOffering])
getReservedNodeExchangeOfferingsResponse_reservedNodeOfferings = Lens.lens (\GetReservedNodeExchangeOfferingsResponse' {reservedNodeOfferings} -> reservedNodeOfferings) (\s@GetReservedNodeExchangeOfferingsResponse' {} a -> s {reservedNodeOfferings = a} :: GetReservedNodeExchangeOfferingsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | An optional parameter that specifies the starting point for returning a
-- set of response records. When the results of a
-- @GetReservedNodeExchangeOfferings@ request exceed the value specified in
-- MaxRecords, Amazon Redshift returns a value in the marker field of the
-- response. You can retrieve the next set of response records by providing
-- the returned marker value in the marker parameter and retrying the
-- request.
getReservedNodeExchangeOfferingsResponse_marker :: Lens.Lens' GetReservedNodeExchangeOfferingsResponse (Prelude.Maybe Prelude.Text)
getReservedNodeExchangeOfferingsResponse_marker = Lens.lens (\GetReservedNodeExchangeOfferingsResponse' {marker} -> marker) (\s@GetReservedNodeExchangeOfferingsResponse' {} a -> s {marker = a} :: GetReservedNodeExchangeOfferingsResponse)

-- | The response's http status code.
getReservedNodeExchangeOfferingsResponse_httpStatus :: Lens.Lens' GetReservedNodeExchangeOfferingsResponse Prelude.Int
getReservedNodeExchangeOfferingsResponse_httpStatus = Lens.lens (\GetReservedNodeExchangeOfferingsResponse' {httpStatus} -> httpStatus) (\s@GetReservedNodeExchangeOfferingsResponse' {} a -> s {httpStatus = a} :: GetReservedNodeExchangeOfferingsResponse)

instance
  Prelude.NFData
    GetReservedNodeExchangeOfferingsResponse
