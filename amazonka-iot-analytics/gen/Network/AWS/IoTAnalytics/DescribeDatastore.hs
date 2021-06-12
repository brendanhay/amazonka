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
-- Module      : Network.AWS.IoTAnalytics.DescribeDatastore
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a data store.
module Network.AWS.IoTAnalytics.DescribeDatastore
  ( -- * Creating a Request
    DescribeDatastore (..),
    newDescribeDatastore,

    -- * Request Lenses
    describeDatastore_includeStatistics,
    describeDatastore_datastoreName,

    -- * Destructuring the Response
    DescribeDatastoreResponse (..),
    newDescribeDatastoreResponse,

    -- * Response Lenses
    describeDatastoreResponse_datastore,
    describeDatastoreResponse_statistics,
    describeDatastoreResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeDatastore' smart constructor.
data DescribeDatastore = DescribeDatastore'
  { -- | If true, additional statistical information about the data store is
    -- included in the response. This feature cannot be used with a data store
    -- whose S3 storage is customer-managed.
    includeStatistics :: Core.Maybe Core.Bool,
    -- | The name of the data store
    datastoreName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeDatastore' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'includeStatistics', 'describeDatastore_includeStatistics' - If true, additional statistical information about the data store is
-- included in the response. This feature cannot be used with a data store
-- whose S3 storage is customer-managed.
--
-- 'datastoreName', 'describeDatastore_datastoreName' - The name of the data store
newDescribeDatastore ::
  -- | 'datastoreName'
  Core.Text ->
  DescribeDatastore
newDescribeDatastore pDatastoreName_ =
  DescribeDatastore'
    { includeStatistics =
        Core.Nothing,
      datastoreName = pDatastoreName_
    }

-- | If true, additional statistical information about the data store is
-- included in the response. This feature cannot be used with a data store
-- whose S3 storage is customer-managed.
describeDatastore_includeStatistics :: Lens.Lens' DescribeDatastore (Core.Maybe Core.Bool)
describeDatastore_includeStatistics = Lens.lens (\DescribeDatastore' {includeStatistics} -> includeStatistics) (\s@DescribeDatastore' {} a -> s {includeStatistics = a} :: DescribeDatastore)

-- | The name of the data store
describeDatastore_datastoreName :: Lens.Lens' DescribeDatastore Core.Text
describeDatastore_datastoreName = Lens.lens (\DescribeDatastore' {datastoreName} -> datastoreName) (\s@DescribeDatastore' {} a -> s {datastoreName = a} :: DescribeDatastore)

instance Core.AWSRequest DescribeDatastore where
  type
    AWSResponse DescribeDatastore =
      DescribeDatastoreResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDatastoreResponse'
            Core.<$> (x Core..?> "datastore")
            Core.<*> (x Core..?> "statistics")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeDatastore

instance Core.NFData DescribeDatastore

instance Core.ToHeaders DescribeDatastore where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeDatastore where
  toPath DescribeDatastore' {..} =
    Core.mconcat
      ["/datastores/", Core.toBS datastoreName]

instance Core.ToQuery DescribeDatastore where
  toQuery DescribeDatastore' {..} =
    Core.mconcat
      ["includeStatistics" Core.=: includeStatistics]

-- | /See:/ 'newDescribeDatastoreResponse' smart constructor.
data DescribeDatastoreResponse = DescribeDatastoreResponse'
  { -- | Information about the data store.
    datastore :: Core.Maybe Datastore,
    -- | Additional statistical information about the data store. Included if the
    -- @includeStatistics@ parameter is set to @true@ in the request.
    statistics :: Core.Maybe DatastoreStatistics,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeDatastoreResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datastore', 'describeDatastoreResponse_datastore' - Information about the data store.
--
-- 'statistics', 'describeDatastoreResponse_statistics' - Additional statistical information about the data store. Included if the
-- @includeStatistics@ parameter is set to @true@ in the request.
--
-- 'httpStatus', 'describeDatastoreResponse_httpStatus' - The response's http status code.
newDescribeDatastoreResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeDatastoreResponse
newDescribeDatastoreResponse pHttpStatus_ =
  DescribeDatastoreResponse'
    { datastore =
        Core.Nothing,
      statistics = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the data store.
describeDatastoreResponse_datastore :: Lens.Lens' DescribeDatastoreResponse (Core.Maybe Datastore)
describeDatastoreResponse_datastore = Lens.lens (\DescribeDatastoreResponse' {datastore} -> datastore) (\s@DescribeDatastoreResponse' {} a -> s {datastore = a} :: DescribeDatastoreResponse)

-- | Additional statistical information about the data store. Included if the
-- @includeStatistics@ parameter is set to @true@ in the request.
describeDatastoreResponse_statistics :: Lens.Lens' DescribeDatastoreResponse (Core.Maybe DatastoreStatistics)
describeDatastoreResponse_statistics = Lens.lens (\DescribeDatastoreResponse' {statistics} -> statistics) (\s@DescribeDatastoreResponse' {} a -> s {statistics = a} :: DescribeDatastoreResponse)

-- | The response's http status code.
describeDatastoreResponse_httpStatus :: Lens.Lens' DescribeDatastoreResponse Core.Int
describeDatastoreResponse_httpStatus = Lens.lens (\DescribeDatastoreResponse' {httpStatus} -> httpStatus) (\s@DescribeDatastoreResponse' {} a -> s {httpStatus = a} :: DescribeDatastoreResponse)

instance Core.NFData DescribeDatastoreResponse
