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
-- Module      : Amazonka.Redshift.DescribeTableRestoreStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.Redshift.DescribeTableRestoreStatus
  ( -- * Creating a Request
    DescribeTableRestoreStatus (..),
    newDescribeTableRestoreStatus,

    -- * Request Lenses
    describeTableRestoreStatus_clusterIdentifier,
    describeTableRestoreStatus_marker,
    describeTableRestoreStatus_maxRecords,
    describeTableRestoreStatus_tableRestoreRequestId,

    -- * Destructuring the Response
    DescribeTableRestoreStatusResponse (..),
    newDescribeTableRestoreStatusResponse,

    -- * Response Lenses
    describeTableRestoreStatusResponse_marker,
    describeTableRestoreStatusResponse_tableRestoreStatusDetails,
    describeTableRestoreStatusResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so that the remaining
    -- results can be retrieved.
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | The identifier of the table restore request to return status for. If you
    -- don\'t specify a @TableRestoreRequestId@ value, then
    -- @DescribeTableRestoreStatus@ returns the status of all in-progress table
    -- restore requests.
    tableRestoreRequestId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'maxRecords', 'describeTableRestoreStatus_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- 'tableRestoreRequestId', 'describeTableRestoreStatus_tableRestoreRequestId' - The identifier of the table restore request to return status for. If you
-- don\'t specify a @TableRestoreRequestId@ value, then
-- @DescribeTableRestoreStatus@ returns the status of all in-progress table
-- restore requests.
newDescribeTableRestoreStatus ::
  DescribeTableRestoreStatus
newDescribeTableRestoreStatus =
  DescribeTableRestoreStatus'
    { clusterIdentifier =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      tableRestoreRequestId = Prelude.Nothing
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

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
describeTableRestoreStatus_maxRecords :: Lens.Lens' DescribeTableRestoreStatus (Prelude.Maybe Prelude.Int)
describeTableRestoreStatus_maxRecords = Lens.lens (\DescribeTableRestoreStatus' {maxRecords} -> maxRecords) (\s@DescribeTableRestoreStatus' {} a -> s {maxRecords = a} :: DescribeTableRestoreStatus)

-- | The identifier of the table restore request to return status for. If you
-- don\'t specify a @TableRestoreRequestId@ value, then
-- @DescribeTableRestoreStatus@ returns the status of all in-progress table
-- restore requests.
describeTableRestoreStatus_tableRestoreRequestId :: Lens.Lens' DescribeTableRestoreStatus (Prelude.Maybe Prelude.Text)
describeTableRestoreStatus_tableRestoreRequestId = Lens.lens (\DescribeTableRestoreStatus' {tableRestoreRequestId} -> tableRestoreRequestId) (\s@DescribeTableRestoreStatus' {} a -> s {tableRestoreRequestId = a} :: DescribeTableRestoreStatus)

instance Core.AWSPager DescribeTableRestoreStatus where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeTableRestoreStatusResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeTableRestoreStatusResponse_tableRestoreStatusDetails
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeTableRestoreStatus_marker
          Lens..~ rs
          Lens.^? describeTableRestoreStatusResponse_marker
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeTableRestoreStatus where
  type
    AWSResponse DescribeTableRestoreStatus =
      DescribeTableRestoreStatusResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeTableRestoreStatusResult"
      ( \s h x ->
          DescribeTableRestoreStatusResponse'
            Prelude.<$> (x Data..@? "Marker")
            Prelude.<*> ( x Data..@? "TableRestoreStatusDetails"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "TableRestoreStatus")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeTableRestoreStatus where
  hashWithSalt _salt DescribeTableRestoreStatus' {..} =
    _salt `Prelude.hashWithSalt` clusterIdentifier
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxRecords
      `Prelude.hashWithSalt` tableRestoreRequestId

instance Prelude.NFData DescribeTableRestoreStatus where
  rnf DescribeTableRestoreStatus' {..} =
    Prelude.rnf clusterIdentifier
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxRecords
      `Prelude.seq` Prelude.rnf tableRestoreRequestId

instance Data.ToHeaders DescribeTableRestoreStatus where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeTableRestoreStatus where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeTableRestoreStatus where
  toQuery DescribeTableRestoreStatus' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeTableRestoreStatus" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "ClusterIdentifier" Data.=: clusterIdentifier,
        "Marker" Data.=: marker,
        "MaxRecords" Data.=: maxRecords,
        "TableRestoreRequestId"
          Data.=: tableRestoreRequestId
      ]

-- |
--
-- /See:/ 'newDescribeTableRestoreStatusResponse' smart constructor.
data DescribeTableRestoreStatusResponse = DescribeTableRestoreStatusResponse'
  { -- | A pagination token that can be used in a subsequent
    -- DescribeTableRestoreStatus request.
    marker :: Prelude.Maybe Prelude.Text,
    -- | A list of status details for one or more table restore requests.
    tableRestoreStatusDetails :: Prelude.Maybe [TableRestoreStatus],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTableRestoreStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeTableRestoreStatusResponse_marker' - A pagination token that can be used in a subsequent
-- DescribeTableRestoreStatus request.
--
-- 'tableRestoreStatusDetails', 'describeTableRestoreStatusResponse_tableRestoreStatusDetails' - A list of status details for one or more table restore requests.
--
-- 'httpStatus', 'describeTableRestoreStatusResponse_httpStatus' - The response's http status code.
newDescribeTableRestoreStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeTableRestoreStatusResponse
newDescribeTableRestoreStatusResponse pHttpStatus_ =
  DescribeTableRestoreStatusResponse'
    { marker =
        Prelude.Nothing,
      tableRestoreStatusDetails =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A pagination token that can be used in a subsequent
-- DescribeTableRestoreStatus request.
describeTableRestoreStatusResponse_marker :: Lens.Lens' DescribeTableRestoreStatusResponse (Prelude.Maybe Prelude.Text)
describeTableRestoreStatusResponse_marker = Lens.lens (\DescribeTableRestoreStatusResponse' {marker} -> marker) (\s@DescribeTableRestoreStatusResponse' {} a -> s {marker = a} :: DescribeTableRestoreStatusResponse)

-- | A list of status details for one or more table restore requests.
describeTableRestoreStatusResponse_tableRestoreStatusDetails :: Lens.Lens' DescribeTableRestoreStatusResponse (Prelude.Maybe [TableRestoreStatus])
describeTableRestoreStatusResponse_tableRestoreStatusDetails = Lens.lens (\DescribeTableRestoreStatusResponse' {tableRestoreStatusDetails} -> tableRestoreStatusDetails) (\s@DescribeTableRestoreStatusResponse' {} a -> s {tableRestoreStatusDetails = a} :: DescribeTableRestoreStatusResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeTableRestoreStatusResponse_httpStatus :: Lens.Lens' DescribeTableRestoreStatusResponse Prelude.Int
describeTableRestoreStatusResponse_httpStatus = Lens.lens (\DescribeTableRestoreStatusResponse' {httpStatus} -> httpStatus) (\s@DescribeTableRestoreStatusResponse' {} a -> s {httpStatus = a} :: DescribeTableRestoreStatusResponse)

instance
  Prelude.NFData
    DescribeTableRestoreStatusResponse
  where
  rnf DescribeTableRestoreStatusResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf tableRestoreStatusDetails
      `Prelude.seq` Prelude.rnf httpStatus
