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
-- Module      : Amazonka.QuickSight.DescribeDataSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a dataset. This operation doesn\'t support datasets that
-- include uploaded files as a source.
module Amazonka.QuickSight.DescribeDataSet
  ( -- * Creating a Request
    DescribeDataSet (..),
    newDescribeDataSet,

    -- * Request Lenses
    describeDataSet_awsAccountId,
    describeDataSet_dataSetId,

    -- * Destructuring the Response
    DescribeDataSetResponse (..),
    newDescribeDataSetResponse,

    -- * Response Lenses
    describeDataSetResponse_dataSet,
    describeDataSetResponse_requestId,
    describeDataSetResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDataSet' smart constructor.
data DescribeDataSet = DescribeDataSet'
  { -- | The Amazon Web Services account ID.
    awsAccountId :: Prelude.Text,
    -- | The ID for the dataset that you want to create. This ID is unique per
    -- Amazon Web Services Region for each Amazon Web Services account.
    dataSetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDataSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'describeDataSet_awsAccountId' - The Amazon Web Services account ID.
--
-- 'dataSetId', 'describeDataSet_dataSetId' - The ID for the dataset that you want to create. This ID is unique per
-- Amazon Web Services Region for each Amazon Web Services account.
newDescribeDataSet ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'dataSetId'
  Prelude.Text ->
  DescribeDataSet
newDescribeDataSet pAwsAccountId_ pDataSetId_ =
  DescribeDataSet'
    { awsAccountId = pAwsAccountId_,
      dataSetId = pDataSetId_
    }

-- | The Amazon Web Services account ID.
describeDataSet_awsAccountId :: Lens.Lens' DescribeDataSet Prelude.Text
describeDataSet_awsAccountId = Lens.lens (\DescribeDataSet' {awsAccountId} -> awsAccountId) (\s@DescribeDataSet' {} a -> s {awsAccountId = a} :: DescribeDataSet)

-- | The ID for the dataset that you want to create. This ID is unique per
-- Amazon Web Services Region for each Amazon Web Services account.
describeDataSet_dataSetId :: Lens.Lens' DescribeDataSet Prelude.Text
describeDataSet_dataSetId = Lens.lens (\DescribeDataSet' {dataSetId} -> dataSetId) (\s@DescribeDataSet' {} a -> s {dataSetId = a} :: DescribeDataSet)

instance Core.AWSRequest DescribeDataSet where
  type
    AWSResponse DescribeDataSet =
      DescribeDataSetResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDataSetResponse'
            Prelude.<$> (x Data..?> "DataSet")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDataSet where
  hashWithSalt _salt DescribeDataSet' {..} =
    _salt `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` dataSetId

instance Prelude.NFData DescribeDataSet where
  rnf DescribeDataSet' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf dataSetId

instance Data.ToHeaders DescribeDataSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeDataSet where
  toPath DescribeDataSet' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/data-sets/",
        Data.toBS dataSetId
      ]

instance Data.ToQuery DescribeDataSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDataSetResponse' smart constructor.
data DescribeDataSetResponse = DescribeDataSetResponse'
  { -- | Information on the dataset.
    dataSet :: Prelude.Maybe DataSet,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDataSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSet', 'describeDataSetResponse_dataSet' - Information on the dataset.
--
-- 'requestId', 'describeDataSetResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'describeDataSetResponse_status' - The HTTP status of the request.
newDescribeDataSetResponse ::
  -- | 'status'
  Prelude.Int ->
  DescribeDataSetResponse
newDescribeDataSetResponse pStatus_ =
  DescribeDataSetResponse'
    { dataSet = Prelude.Nothing,
      requestId = Prelude.Nothing,
      status = pStatus_
    }

-- | Information on the dataset.
describeDataSetResponse_dataSet :: Lens.Lens' DescribeDataSetResponse (Prelude.Maybe DataSet)
describeDataSetResponse_dataSet = Lens.lens (\DescribeDataSetResponse' {dataSet} -> dataSet) (\s@DescribeDataSetResponse' {} a -> s {dataSet = a} :: DescribeDataSetResponse)

-- | The Amazon Web Services request ID for this operation.
describeDataSetResponse_requestId :: Lens.Lens' DescribeDataSetResponse (Prelude.Maybe Prelude.Text)
describeDataSetResponse_requestId = Lens.lens (\DescribeDataSetResponse' {requestId} -> requestId) (\s@DescribeDataSetResponse' {} a -> s {requestId = a} :: DescribeDataSetResponse)

-- | The HTTP status of the request.
describeDataSetResponse_status :: Lens.Lens' DescribeDataSetResponse Prelude.Int
describeDataSetResponse_status = Lens.lens (\DescribeDataSetResponse' {status} -> status) (\s@DescribeDataSetResponse' {} a -> s {status = a} :: DescribeDataSetResponse)

instance Prelude.NFData DescribeDataSetResponse where
  rnf DescribeDataSetResponse' {..} =
    Prelude.rnf dataSet
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status
