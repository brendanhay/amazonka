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
-- Module      : Amazonka.ElastiCache.DescribeServiceUpdates
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns details of the service updates
--
-- This operation returns paginated results.
module Amazonka.ElastiCache.DescribeServiceUpdates
  ( -- * Creating a Request
    DescribeServiceUpdates (..),
    newDescribeServiceUpdates,

    -- * Request Lenses
    describeServiceUpdates_marker,
    describeServiceUpdates_maxRecords,
    describeServiceUpdates_serviceUpdateName,
    describeServiceUpdates_serviceUpdateStatus,

    -- * Destructuring the Response
    DescribeServiceUpdatesResponse (..),
    newDescribeServiceUpdatesResponse,

    -- * Response Lenses
    describeServiceUpdatesResponse_marker,
    describeServiceUpdatesResponse_serviceUpdates,
    describeServiceUpdatesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElastiCache.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeServiceUpdates' smart constructor.
data DescribeServiceUpdates = DescribeServiceUpdates'
  { -- | An optional marker returned from a prior request. Use this marker for
    -- pagination of results from this operation. If this parameter is
    -- specified, the response includes only records beyond the marker, up to
    -- the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | The unique ID of the service update
    serviceUpdateName :: Prelude.Maybe Prelude.Text,
    -- | The status of the service update
    serviceUpdateStatus :: Prelude.Maybe [ServiceUpdateStatus]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeServiceUpdates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeServiceUpdates_marker' - An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
--
-- 'maxRecords', 'describeServiceUpdates_maxRecords' - The maximum number of records to include in the response
--
-- 'serviceUpdateName', 'describeServiceUpdates_serviceUpdateName' - The unique ID of the service update
--
-- 'serviceUpdateStatus', 'describeServiceUpdates_serviceUpdateStatus' - The status of the service update
newDescribeServiceUpdates ::
  DescribeServiceUpdates
newDescribeServiceUpdates =
  DescribeServiceUpdates'
    { marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      serviceUpdateName = Prelude.Nothing,
      serviceUpdateStatus = Prelude.Nothing
    }

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
describeServiceUpdates_marker :: Lens.Lens' DescribeServiceUpdates (Prelude.Maybe Prelude.Text)
describeServiceUpdates_marker = Lens.lens (\DescribeServiceUpdates' {marker} -> marker) (\s@DescribeServiceUpdates' {} a -> s {marker = a} :: DescribeServiceUpdates)

-- | The maximum number of records to include in the response
describeServiceUpdates_maxRecords :: Lens.Lens' DescribeServiceUpdates (Prelude.Maybe Prelude.Int)
describeServiceUpdates_maxRecords = Lens.lens (\DescribeServiceUpdates' {maxRecords} -> maxRecords) (\s@DescribeServiceUpdates' {} a -> s {maxRecords = a} :: DescribeServiceUpdates)

-- | The unique ID of the service update
describeServiceUpdates_serviceUpdateName :: Lens.Lens' DescribeServiceUpdates (Prelude.Maybe Prelude.Text)
describeServiceUpdates_serviceUpdateName = Lens.lens (\DescribeServiceUpdates' {serviceUpdateName} -> serviceUpdateName) (\s@DescribeServiceUpdates' {} a -> s {serviceUpdateName = a} :: DescribeServiceUpdates)

-- | The status of the service update
describeServiceUpdates_serviceUpdateStatus :: Lens.Lens' DescribeServiceUpdates (Prelude.Maybe [ServiceUpdateStatus])
describeServiceUpdates_serviceUpdateStatus = Lens.lens (\DescribeServiceUpdates' {serviceUpdateStatus} -> serviceUpdateStatus) (\s@DescribeServiceUpdates' {} a -> s {serviceUpdateStatus = a} :: DescribeServiceUpdates) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSPager DescribeServiceUpdates where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeServiceUpdatesResponse_marker
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeServiceUpdatesResponse_serviceUpdates
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& describeServiceUpdates_marker
              Lens..~ rs
              Lens.^? describeServiceUpdatesResponse_marker
              Prelude.. Lens._Just

instance Core.AWSRequest DescribeServiceUpdates where
  type
    AWSResponse DescribeServiceUpdates =
      DescribeServiceUpdatesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeServiceUpdatesResult"
      ( \s h x ->
          DescribeServiceUpdatesResponse'
            Prelude.<$> (x Data..@? "Marker")
            Prelude.<*> ( x Data..@? "ServiceUpdates" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "ServiceUpdate")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeServiceUpdates where
  hashWithSalt _salt DescribeServiceUpdates' {..} =
    _salt
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxRecords
      `Prelude.hashWithSalt` serviceUpdateName
      `Prelude.hashWithSalt` serviceUpdateStatus

instance Prelude.NFData DescribeServiceUpdates where
  rnf DescribeServiceUpdates' {..} =
    Prelude.rnf marker `Prelude.seq`
      Prelude.rnf maxRecords `Prelude.seq`
        Prelude.rnf serviceUpdateName `Prelude.seq`
          Prelude.rnf serviceUpdateStatus

instance Data.ToHeaders DescribeServiceUpdates where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeServiceUpdates where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeServiceUpdates where
  toQuery DescribeServiceUpdates' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeServiceUpdates" :: Prelude.ByteString),
        "Version"
          Data.=: ("2015-02-02" :: Prelude.ByteString),
        "Marker" Data.=: marker,
        "MaxRecords" Data.=: maxRecords,
        "ServiceUpdateName" Data.=: serviceUpdateName,
        "ServiceUpdateStatus"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> serviceUpdateStatus
            )
      ]

-- | /See:/ 'newDescribeServiceUpdatesResponse' smart constructor.
data DescribeServiceUpdatesResponse = DescribeServiceUpdatesResponse'
  { -- | An optional marker returned from a prior request. Use this marker for
    -- pagination of results from this operation. If this parameter is
    -- specified, the response includes only records beyond the marker, up to
    -- the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | A list of service updates
    serviceUpdates :: Prelude.Maybe [ServiceUpdate],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeServiceUpdatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeServiceUpdatesResponse_marker' - An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
--
-- 'serviceUpdates', 'describeServiceUpdatesResponse_serviceUpdates' - A list of service updates
--
-- 'httpStatus', 'describeServiceUpdatesResponse_httpStatus' - The response's http status code.
newDescribeServiceUpdatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeServiceUpdatesResponse
newDescribeServiceUpdatesResponse pHttpStatus_ =
  DescribeServiceUpdatesResponse'
    { marker =
        Prelude.Nothing,
      serviceUpdates = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
describeServiceUpdatesResponse_marker :: Lens.Lens' DescribeServiceUpdatesResponse (Prelude.Maybe Prelude.Text)
describeServiceUpdatesResponse_marker = Lens.lens (\DescribeServiceUpdatesResponse' {marker} -> marker) (\s@DescribeServiceUpdatesResponse' {} a -> s {marker = a} :: DescribeServiceUpdatesResponse)

-- | A list of service updates
describeServiceUpdatesResponse_serviceUpdates :: Lens.Lens' DescribeServiceUpdatesResponse (Prelude.Maybe [ServiceUpdate])
describeServiceUpdatesResponse_serviceUpdates = Lens.lens (\DescribeServiceUpdatesResponse' {serviceUpdates} -> serviceUpdates) (\s@DescribeServiceUpdatesResponse' {} a -> s {serviceUpdates = a} :: DescribeServiceUpdatesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeServiceUpdatesResponse_httpStatus :: Lens.Lens' DescribeServiceUpdatesResponse Prelude.Int
describeServiceUpdatesResponse_httpStatus = Lens.lens (\DescribeServiceUpdatesResponse' {httpStatus} -> httpStatus) (\s@DescribeServiceUpdatesResponse' {} a -> s {httpStatus = a} :: DescribeServiceUpdatesResponse)

instance
  Prelude.NFData
    DescribeServiceUpdatesResponse
  where
  rnf DescribeServiceUpdatesResponse' {..} =
    Prelude.rnf marker `Prelude.seq`
      Prelude.rnf serviceUpdates `Prelude.seq`
        Prelude.rnf httpStatus
