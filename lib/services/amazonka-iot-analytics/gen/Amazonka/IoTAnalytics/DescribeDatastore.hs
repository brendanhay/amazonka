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
-- Module      : Amazonka.IoTAnalytics.DescribeDatastore
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a data store.
module Amazonka.IoTAnalytics.DescribeDatastore
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTAnalytics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDatastore' smart constructor.
data DescribeDatastore = DescribeDatastore'
  { -- | If true, additional statistical information about the data store is
    -- included in the response. This feature can\'t be used with a data store
    -- whose S3 storage is customer-managed.
    includeStatistics :: Prelude.Maybe Prelude.Bool,
    -- | The name of the data store
    datastoreName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDatastore' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'includeStatistics', 'describeDatastore_includeStatistics' - If true, additional statistical information about the data store is
-- included in the response. This feature can\'t be used with a data store
-- whose S3 storage is customer-managed.
--
-- 'datastoreName', 'describeDatastore_datastoreName' - The name of the data store
newDescribeDatastore ::
  -- | 'datastoreName'
  Prelude.Text ->
  DescribeDatastore
newDescribeDatastore pDatastoreName_ =
  DescribeDatastore'
    { includeStatistics =
        Prelude.Nothing,
      datastoreName = pDatastoreName_
    }

-- | If true, additional statistical information about the data store is
-- included in the response. This feature can\'t be used with a data store
-- whose S3 storage is customer-managed.
describeDatastore_includeStatistics :: Lens.Lens' DescribeDatastore (Prelude.Maybe Prelude.Bool)
describeDatastore_includeStatistics = Lens.lens (\DescribeDatastore' {includeStatistics} -> includeStatistics) (\s@DescribeDatastore' {} a -> s {includeStatistics = a} :: DescribeDatastore)

-- | The name of the data store
describeDatastore_datastoreName :: Lens.Lens' DescribeDatastore Prelude.Text
describeDatastore_datastoreName = Lens.lens (\DescribeDatastore' {datastoreName} -> datastoreName) (\s@DescribeDatastore' {} a -> s {datastoreName = a} :: DescribeDatastore)

instance Core.AWSRequest DescribeDatastore where
  type
    AWSResponse DescribeDatastore =
      DescribeDatastoreResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDatastoreResponse'
            Prelude.<$> (x Data..?> "datastore")
            Prelude.<*> (x Data..?> "statistics")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDatastore where
  hashWithSalt _salt DescribeDatastore' {..} =
    _salt
      `Prelude.hashWithSalt` includeStatistics
      `Prelude.hashWithSalt` datastoreName

instance Prelude.NFData DescribeDatastore where
  rnf DescribeDatastore' {..} =
    Prelude.rnf includeStatistics
      `Prelude.seq` Prelude.rnf datastoreName

instance Data.ToHeaders DescribeDatastore where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeDatastore where
  toPath DescribeDatastore' {..} =
    Prelude.mconcat
      ["/datastores/", Data.toBS datastoreName]

instance Data.ToQuery DescribeDatastore where
  toQuery DescribeDatastore' {..} =
    Prelude.mconcat
      ["includeStatistics" Data.=: includeStatistics]

-- | /See:/ 'newDescribeDatastoreResponse' smart constructor.
data DescribeDatastoreResponse = DescribeDatastoreResponse'
  { -- | Information about the data store.
    datastore :: Prelude.Maybe Datastore,
    -- | Additional statistical information about the data store. Included if the
    -- @includeStatistics@ parameter is set to @true@ in the request.
    statistics :: Prelude.Maybe DatastoreStatistics,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeDatastoreResponse
newDescribeDatastoreResponse pHttpStatus_ =
  DescribeDatastoreResponse'
    { datastore =
        Prelude.Nothing,
      statistics = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the data store.
describeDatastoreResponse_datastore :: Lens.Lens' DescribeDatastoreResponse (Prelude.Maybe Datastore)
describeDatastoreResponse_datastore = Lens.lens (\DescribeDatastoreResponse' {datastore} -> datastore) (\s@DescribeDatastoreResponse' {} a -> s {datastore = a} :: DescribeDatastoreResponse)

-- | Additional statistical information about the data store. Included if the
-- @includeStatistics@ parameter is set to @true@ in the request.
describeDatastoreResponse_statistics :: Lens.Lens' DescribeDatastoreResponse (Prelude.Maybe DatastoreStatistics)
describeDatastoreResponse_statistics = Lens.lens (\DescribeDatastoreResponse' {statistics} -> statistics) (\s@DescribeDatastoreResponse' {} a -> s {statistics = a} :: DescribeDatastoreResponse)

-- | The response's http status code.
describeDatastoreResponse_httpStatus :: Lens.Lens' DescribeDatastoreResponse Prelude.Int
describeDatastoreResponse_httpStatus = Lens.lens (\DescribeDatastoreResponse' {httpStatus} -> httpStatus) (\s@DescribeDatastoreResponse' {} a -> s {httpStatus = a} :: DescribeDatastoreResponse)

instance Prelude.NFData DescribeDatastoreResponse where
  rnf DescribeDatastoreResponse' {..} =
    Prelude.rnf datastore
      `Prelude.seq` Prelude.rnf statistics
      `Prelude.seq` Prelude.rnf httpStatus
