{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Redshift.DescribeTableRestoreStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the status of one or more table restore requests made using the
-- RestoreTableFromClusterSnapshot API action. If you don\'t specify a
-- value for the @TableRestoreRequestId@ parameter, then
-- @DescribeTableRestoreStatus@ returns the status of all table restore
-- requests ordered by the date and time of the request in ascending order.
-- Otherwise @DescribeTableRestoreStatus@ returns the status of the table
-- specified by @TableRestoreRequestId@.
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeTableRestoreStatus
  ( -- * Creating a Request
    DescribeTableRestoreStatus (..),
    newDescribeTableRestoreStatus,

    -- * Request Lenses
    describeTableRestoreStatus_clusterIdentifier,
    describeTableRestoreStatus_marker,
    describeTableRestoreStatus_tableRestoreRequestId,
    describeTableRestoreStatus_maxRecords,

    -- * Destructuring the Response
    DescribeTableRestoreStatusResponse (..),
    newDescribeTableRestoreStatusResponse,

    -- * Response Lenses
    describeTableRestoreStatusResponse_tableRestoreStatusDetails,
    describeTableRestoreStatusResponse_marker,
    describeTableRestoreStatusResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeTableRestoreStatus' smart constructor.
data DescribeTableRestoreStatus = DescribeTableRestoreStatus'
  { -- | The Amazon Redshift cluster that the table is being restored to.
    clusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | An optional pagination token provided by a previous
    -- @DescribeTableRestoreStatus@ request. If this parameter is specified,
    -- the response includes only records beyond the marker, up to the value
    -- specified by the @MaxRecords@ parameter.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the table restore request to return status for. If you
    -- don\'t specify a @TableRestoreRequestId@ value, then
    -- @DescribeTableRestoreStatus@ returns the status of all in-progress table
    -- restore requests.
    tableRestoreRequestId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so that the remaining
    -- results can be retrieved.
    maxRecords :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeTableRestoreStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterIdentifier', 'describeTableRestoreStatus_clusterIdentifier' - The Amazon Redshift cluster that the table is being restored to.
--
-- 'marker', 'describeTableRestoreStatus_marker' - An optional pagination token provided by a previous
-- @DescribeTableRestoreStatus@ request. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by the @MaxRecords@ parameter.
--
-- 'tableRestoreRequestId', 'describeTableRestoreStatus_tableRestoreRequestId' - The identifier of the table restore request to return status for. If you
-- don\'t specify a @TableRestoreRequestId@ value, then
-- @DescribeTableRestoreStatus@ returns the status of all in-progress table
-- restore requests.
--
-- 'maxRecords', 'describeTableRestoreStatus_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
newDescribeTableRestoreStatus ::
  DescribeTableRestoreStatus
newDescribeTableRestoreStatus =
  DescribeTableRestoreStatus'
    { clusterIdentifier =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      tableRestoreRequestId = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | The Amazon Redshift cluster that the table is being restored to.
describeTableRestoreStatus_clusterIdentifier :: Lens.Lens' DescribeTableRestoreStatus (Prelude.Maybe Prelude.Text)
describeTableRestoreStatus_clusterIdentifier = Lens.lens (\DescribeTableRestoreStatus' {clusterIdentifier} -> clusterIdentifier) (\s@DescribeTableRestoreStatus' {} a -> s {clusterIdentifier = a} :: DescribeTableRestoreStatus)

-- | An optional pagination token provided by a previous
-- @DescribeTableRestoreStatus@ request. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by the @MaxRecords@ parameter.
describeTableRestoreStatus_marker :: Lens.Lens' DescribeTableRestoreStatus (Prelude.Maybe Prelude.Text)
describeTableRestoreStatus_marker = Lens.lens (\DescribeTableRestoreStatus' {marker} -> marker) (\s@DescribeTableRestoreStatus' {} a -> s {marker = a} :: DescribeTableRestoreStatus)

-- | The identifier of the table restore request to return status for. If you
-- don\'t specify a @TableRestoreRequestId@ value, then
-- @DescribeTableRestoreStatus@ returns the status of all in-progress table
-- restore requests.
describeTableRestoreStatus_tableRestoreRequestId :: Lens.Lens' DescribeTableRestoreStatus (Prelude.Maybe Prelude.Text)
describeTableRestoreStatus_tableRestoreRequestId = Lens.lens (\DescribeTableRestoreStatus' {tableRestoreRequestId} -> tableRestoreRequestId) (\s@DescribeTableRestoreStatus' {} a -> s {tableRestoreRequestId = a} :: DescribeTableRestoreStatus)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
describeTableRestoreStatus_maxRecords :: Lens.Lens' DescribeTableRestoreStatus (Prelude.Maybe Prelude.Int)
describeTableRestoreStatus_maxRecords = Lens.lens (\DescribeTableRestoreStatus' {maxRecords} -> maxRecords) (\s@DescribeTableRestoreStatus' {} a -> s {maxRecords = a} :: DescribeTableRestoreStatus)

instance Pager.AWSPager DescribeTableRestoreStatus where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeTableRestoreStatusResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeTableRestoreStatusResponse_tableRestoreStatusDetails
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeTableRestoreStatus_marker
          Lens..~ rs
          Lens.^? describeTableRestoreStatusResponse_marker
            Prelude.. Lens._Just

instance
  Prelude.AWSRequest
    DescribeTableRestoreStatus
  where
  type
    Rs DescribeTableRestoreStatus =
      DescribeTableRestoreStatusResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeTableRestoreStatusResult"
      ( \s h x ->
          DescribeTableRestoreStatusResponse'
            Prelude.<$> ( x Prelude..@? "TableRestoreStatusDetails"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may
                              (Prelude.parseXMLList "TableRestoreStatus")
                        )
            Prelude.<*> (x Prelude..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeTableRestoreStatus

instance Prelude.NFData DescribeTableRestoreStatus

instance Prelude.ToHeaders DescribeTableRestoreStatus where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DescribeTableRestoreStatus where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeTableRestoreStatus where
  toQuery DescribeTableRestoreStatus' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DescribeTableRestoreStatus" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2012-12-01" :: Prelude.ByteString),
        "ClusterIdentifier" Prelude.=: clusterIdentifier,
        "Marker" Prelude.=: marker,
        "TableRestoreRequestId"
          Prelude.=: tableRestoreRequestId,
        "MaxRecords" Prelude.=: maxRecords
      ]

-- |
--
-- /See:/ 'newDescribeTableRestoreStatusResponse' smart constructor.
data DescribeTableRestoreStatusResponse = DescribeTableRestoreStatusResponse'
  { -- | A list of status details for one or more table restore requests.
    tableRestoreStatusDetails :: Prelude.Maybe [TableRestoreStatus],
    -- | A pagination token that can be used in a subsequent
    -- DescribeTableRestoreStatus request.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeTableRestoreStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tableRestoreStatusDetails', 'describeTableRestoreStatusResponse_tableRestoreStatusDetails' - A list of status details for one or more table restore requests.
--
-- 'marker', 'describeTableRestoreStatusResponse_marker' - A pagination token that can be used in a subsequent
-- DescribeTableRestoreStatus request.
--
-- 'httpStatus', 'describeTableRestoreStatusResponse_httpStatus' - The response's http status code.
newDescribeTableRestoreStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeTableRestoreStatusResponse
newDescribeTableRestoreStatusResponse pHttpStatus_ =
  DescribeTableRestoreStatusResponse'
    { tableRestoreStatusDetails =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of status details for one or more table restore requests.
describeTableRestoreStatusResponse_tableRestoreStatusDetails :: Lens.Lens' DescribeTableRestoreStatusResponse (Prelude.Maybe [TableRestoreStatus])
describeTableRestoreStatusResponse_tableRestoreStatusDetails = Lens.lens (\DescribeTableRestoreStatusResponse' {tableRestoreStatusDetails} -> tableRestoreStatusDetails) (\s@DescribeTableRestoreStatusResponse' {} a -> s {tableRestoreStatusDetails = a} :: DescribeTableRestoreStatusResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | A pagination token that can be used in a subsequent
-- DescribeTableRestoreStatus request.
describeTableRestoreStatusResponse_marker :: Lens.Lens' DescribeTableRestoreStatusResponse (Prelude.Maybe Prelude.Text)
describeTableRestoreStatusResponse_marker = Lens.lens (\DescribeTableRestoreStatusResponse' {marker} -> marker) (\s@DescribeTableRestoreStatusResponse' {} a -> s {marker = a} :: DescribeTableRestoreStatusResponse)

-- | The response's http status code.
describeTableRestoreStatusResponse_httpStatus :: Lens.Lens' DescribeTableRestoreStatusResponse Prelude.Int
describeTableRestoreStatusResponse_httpStatus = Lens.lens (\DescribeTableRestoreStatusResponse' {httpStatus} -> httpStatus) (\s@DescribeTableRestoreStatusResponse' {} a -> s {httpStatus = a} :: DescribeTableRestoreStatusResponse)

instance
  Prelude.NFData
    DescribeTableRestoreStatusResponse
