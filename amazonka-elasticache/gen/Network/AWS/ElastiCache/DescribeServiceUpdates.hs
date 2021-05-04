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
-- Module      : Network.AWS.ElastiCache.DescribeServiceUpdates
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns details of the service updates
--
-- This operation returns paginated results.
module Network.AWS.ElastiCache.DescribeServiceUpdates
  ( -- * Creating a Request
    DescribeServiceUpdates (..),
    newDescribeServiceUpdates,

    -- * Request Lenses
    describeServiceUpdates_serviceUpdateStatus,
    describeServiceUpdates_serviceUpdateName,
    describeServiceUpdates_marker,
    describeServiceUpdates_maxRecords,

    -- * Destructuring the Response
    DescribeServiceUpdatesResponse (..),
    newDescribeServiceUpdatesResponse,

    -- * Response Lenses
    describeServiceUpdatesResponse_serviceUpdates,
    describeServiceUpdatesResponse_marker,
    describeServiceUpdatesResponse_httpStatus,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeServiceUpdates' smart constructor.
data DescribeServiceUpdates = DescribeServiceUpdates'
  { -- | The status of the service update
    serviceUpdateStatus :: Prelude.Maybe [ServiceUpdateStatus],
    -- | The unique ID of the service update
    serviceUpdateName :: Prelude.Maybe Prelude.Text,
    -- | An optional marker returned from a prior request. Use this marker for
    -- pagination of results from this operation. If this parameter is
    -- specified, the response includes only records beyond the marker, up to
    -- the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response
    maxRecords :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeServiceUpdates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceUpdateStatus', 'describeServiceUpdates_serviceUpdateStatus' - The status of the service update
--
-- 'serviceUpdateName', 'describeServiceUpdates_serviceUpdateName' - The unique ID of the service update
--
-- 'marker', 'describeServiceUpdates_marker' - An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
--
-- 'maxRecords', 'describeServiceUpdates_maxRecords' - The maximum number of records to include in the response
newDescribeServiceUpdates ::
  DescribeServiceUpdates
newDescribeServiceUpdates =
  DescribeServiceUpdates'
    { serviceUpdateStatus =
        Prelude.Nothing,
      serviceUpdateName = Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | The status of the service update
describeServiceUpdates_serviceUpdateStatus :: Lens.Lens' DescribeServiceUpdates (Prelude.Maybe [ServiceUpdateStatus])
describeServiceUpdates_serviceUpdateStatus = Lens.lens (\DescribeServiceUpdates' {serviceUpdateStatus} -> serviceUpdateStatus) (\s@DescribeServiceUpdates' {} a -> s {serviceUpdateStatus = a} :: DescribeServiceUpdates) Prelude.. Lens.mapping Prelude._Coerce

-- | The unique ID of the service update
describeServiceUpdates_serviceUpdateName :: Lens.Lens' DescribeServiceUpdates (Prelude.Maybe Prelude.Text)
describeServiceUpdates_serviceUpdateName = Lens.lens (\DescribeServiceUpdates' {serviceUpdateName} -> serviceUpdateName) (\s@DescribeServiceUpdates' {} a -> s {serviceUpdateName = a} :: DescribeServiceUpdates)

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
describeServiceUpdates_marker :: Lens.Lens' DescribeServiceUpdates (Prelude.Maybe Prelude.Text)
describeServiceUpdates_marker = Lens.lens (\DescribeServiceUpdates' {marker} -> marker) (\s@DescribeServiceUpdates' {} a -> s {marker = a} :: DescribeServiceUpdates)

-- | The maximum number of records to include in the response
describeServiceUpdates_maxRecords :: Lens.Lens' DescribeServiceUpdates (Prelude.Maybe Prelude.Int)
describeServiceUpdates_maxRecords = Lens.lens (\DescribeServiceUpdates' {maxRecords} -> maxRecords) (\s@DescribeServiceUpdates' {} a -> s {maxRecords = a} :: DescribeServiceUpdates)

instance Pager.AWSPager DescribeServiceUpdates where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeServiceUpdatesResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeServiceUpdatesResponse_serviceUpdates
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeServiceUpdates_marker
          Lens..~ rs
          Lens.^? describeServiceUpdatesResponse_marker
            Prelude.. Lens._Just

instance Prelude.AWSRequest DescribeServiceUpdates where
  type
    Rs DescribeServiceUpdates =
      DescribeServiceUpdatesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeServiceUpdatesResult"
      ( \s h x ->
          DescribeServiceUpdatesResponse'
            Prelude.<$> ( x Prelude..@? "ServiceUpdates"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "ServiceUpdate")
                        )
            Prelude.<*> (x Prelude..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeServiceUpdates

instance Prelude.NFData DescribeServiceUpdates

instance Prelude.ToHeaders DescribeServiceUpdates where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DescribeServiceUpdates where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeServiceUpdates where
  toQuery DescribeServiceUpdates' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DescribeServiceUpdates" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2015-02-02" :: Prelude.ByteString),
        "ServiceUpdateStatus"
          Prelude.=: Prelude.toQuery
            ( Prelude.toQueryList "member"
                Prelude.<$> serviceUpdateStatus
            ),
        "ServiceUpdateName" Prelude.=: serviceUpdateName,
        "Marker" Prelude.=: marker,
        "MaxRecords" Prelude.=: maxRecords
      ]

-- | /See:/ 'newDescribeServiceUpdatesResponse' smart constructor.
data DescribeServiceUpdatesResponse = DescribeServiceUpdatesResponse'
  { -- | A list of service updates
    serviceUpdates :: Prelude.Maybe [ServiceUpdate],
    -- | An optional marker returned from a prior request. Use this marker for
    -- pagination of results from this operation. If this parameter is
    -- specified, the response includes only records beyond the marker, up to
    -- the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeServiceUpdatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceUpdates', 'describeServiceUpdatesResponse_serviceUpdates' - A list of service updates
--
-- 'marker', 'describeServiceUpdatesResponse_marker' - An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
--
-- 'httpStatus', 'describeServiceUpdatesResponse_httpStatus' - The response's http status code.
newDescribeServiceUpdatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeServiceUpdatesResponse
newDescribeServiceUpdatesResponse pHttpStatus_ =
  DescribeServiceUpdatesResponse'
    { serviceUpdates =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of service updates
describeServiceUpdatesResponse_serviceUpdates :: Lens.Lens' DescribeServiceUpdatesResponse (Prelude.Maybe [ServiceUpdate])
describeServiceUpdatesResponse_serviceUpdates = Lens.lens (\DescribeServiceUpdatesResponse' {serviceUpdates} -> serviceUpdates) (\s@DescribeServiceUpdatesResponse' {} a -> s {serviceUpdates = a} :: DescribeServiceUpdatesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
describeServiceUpdatesResponse_marker :: Lens.Lens' DescribeServiceUpdatesResponse (Prelude.Maybe Prelude.Text)
describeServiceUpdatesResponse_marker = Lens.lens (\DescribeServiceUpdatesResponse' {marker} -> marker) (\s@DescribeServiceUpdatesResponse' {} a -> s {marker = a} :: DescribeServiceUpdatesResponse)

-- | The response's http status code.
describeServiceUpdatesResponse_httpStatus :: Lens.Lens' DescribeServiceUpdatesResponse Prelude.Int
describeServiceUpdatesResponse_httpStatus = Lens.lens (\DescribeServiceUpdatesResponse' {httpStatus} -> httpStatus) (\s@DescribeServiceUpdatesResponse' {} a -> s {httpStatus = a} :: DescribeServiceUpdatesResponse)

instance
  Prelude.NFData
    DescribeServiceUpdatesResponse
