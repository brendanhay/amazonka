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
-- Module      : Amazonka.QuickSight.DescribeDataSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a data source.
module Amazonka.QuickSight.DescribeDataSource
  ( -- * Creating a Request
    DescribeDataSource (..),
    newDescribeDataSource,

    -- * Request Lenses
    describeDataSource_awsAccountId,
    describeDataSource_dataSourceId,

    -- * Destructuring the Response
    DescribeDataSourceResponse (..),
    newDescribeDataSourceResponse,

    -- * Response Lenses
    describeDataSourceResponse_dataSource,
    describeDataSourceResponse_requestId,
    describeDataSourceResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDataSource' smart constructor.
data DescribeDataSource = DescribeDataSource'
  { -- | The Amazon Web Services account ID.
    awsAccountId :: Prelude.Text,
    -- | The ID of the data source. This ID is unique per Amazon Web Services
    -- Region for each Amazon Web Services account.
    dataSourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDataSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'describeDataSource_awsAccountId' - The Amazon Web Services account ID.
--
-- 'dataSourceId', 'describeDataSource_dataSourceId' - The ID of the data source. This ID is unique per Amazon Web Services
-- Region for each Amazon Web Services account.
newDescribeDataSource ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'dataSourceId'
  Prelude.Text ->
  DescribeDataSource
newDescribeDataSource pAwsAccountId_ pDataSourceId_ =
  DescribeDataSource'
    { awsAccountId = pAwsAccountId_,
      dataSourceId = pDataSourceId_
    }

-- | The Amazon Web Services account ID.
describeDataSource_awsAccountId :: Lens.Lens' DescribeDataSource Prelude.Text
describeDataSource_awsAccountId = Lens.lens (\DescribeDataSource' {awsAccountId} -> awsAccountId) (\s@DescribeDataSource' {} a -> s {awsAccountId = a} :: DescribeDataSource)

-- | The ID of the data source. This ID is unique per Amazon Web Services
-- Region for each Amazon Web Services account.
describeDataSource_dataSourceId :: Lens.Lens' DescribeDataSource Prelude.Text
describeDataSource_dataSourceId = Lens.lens (\DescribeDataSource' {dataSourceId} -> dataSourceId) (\s@DescribeDataSource' {} a -> s {dataSourceId = a} :: DescribeDataSource)

instance Core.AWSRequest DescribeDataSource where
  type
    AWSResponse DescribeDataSource =
      DescribeDataSourceResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDataSourceResponse'
            Prelude.<$> (x Data..?> "DataSource")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDataSource where
  hashWithSalt _salt DescribeDataSource' {..} =
    _salt `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` dataSourceId

instance Prelude.NFData DescribeDataSource where
  rnf DescribeDataSource' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf dataSourceId

instance Data.ToHeaders DescribeDataSource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeDataSource where
  toPath DescribeDataSource' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/data-sources/",
        Data.toBS dataSourceId
      ]

instance Data.ToQuery DescribeDataSource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDataSourceResponse' smart constructor.
data DescribeDataSourceResponse = DescribeDataSourceResponse'
  { -- | The information on the data source.
    dataSource :: Prelude.Maybe DataSource,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDataSourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSource', 'describeDataSourceResponse_dataSource' - The information on the data source.
--
-- 'requestId', 'describeDataSourceResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'describeDataSourceResponse_status' - The HTTP status of the request.
newDescribeDataSourceResponse ::
  -- | 'status'
  Prelude.Int ->
  DescribeDataSourceResponse
newDescribeDataSourceResponse pStatus_ =
  DescribeDataSourceResponse'
    { dataSource =
        Prelude.Nothing,
      requestId = Prelude.Nothing,
      status = pStatus_
    }

-- | The information on the data source.
describeDataSourceResponse_dataSource :: Lens.Lens' DescribeDataSourceResponse (Prelude.Maybe DataSource)
describeDataSourceResponse_dataSource = Lens.lens (\DescribeDataSourceResponse' {dataSource} -> dataSource) (\s@DescribeDataSourceResponse' {} a -> s {dataSource = a} :: DescribeDataSourceResponse)

-- | The Amazon Web Services request ID for this operation.
describeDataSourceResponse_requestId :: Lens.Lens' DescribeDataSourceResponse (Prelude.Maybe Prelude.Text)
describeDataSourceResponse_requestId = Lens.lens (\DescribeDataSourceResponse' {requestId} -> requestId) (\s@DescribeDataSourceResponse' {} a -> s {requestId = a} :: DescribeDataSourceResponse)

-- | The HTTP status of the request.
describeDataSourceResponse_status :: Lens.Lens' DescribeDataSourceResponse Prelude.Int
describeDataSourceResponse_status = Lens.lens (\DescribeDataSourceResponse' {status} -> status) (\s@DescribeDataSourceResponse' {} a -> s {status = a} :: DescribeDataSourceResponse)

instance Prelude.NFData DescribeDataSourceResponse where
  rnf DescribeDataSourceResponse' {..} =
    Prelude.rnf dataSource
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status
