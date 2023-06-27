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
-- Module      : Amazonka.QuickSight.DescribeDataSetRefreshProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the refresh properties of a dataset.
module Amazonka.QuickSight.DescribeDataSetRefreshProperties
  ( -- * Creating a Request
    DescribeDataSetRefreshProperties (..),
    newDescribeDataSetRefreshProperties,

    -- * Request Lenses
    describeDataSetRefreshProperties_awsAccountId,
    describeDataSetRefreshProperties_dataSetId,

    -- * Destructuring the Response
    DescribeDataSetRefreshPropertiesResponse (..),
    newDescribeDataSetRefreshPropertiesResponse,

    -- * Response Lenses
    describeDataSetRefreshPropertiesResponse_dataSetRefreshProperties,
    describeDataSetRefreshPropertiesResponse_requestId,
    describeDataSetRefreshPropertiesResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDataSetRefreshProperties' smart constructor.
data DescribeDataSetRefreshProperties = DescribeDataSetRefreshProperties'
  { -- | The Amazon Web Services account ID.
    awsAccountId :: Prelude.Text,
    -- | The ID of the dataset.
    dataSetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDataSetRefreshProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'describeDataSetRefreshProperties_awsAccountId' - The Amazon Web Services account ID.
--
-- 'dataSetId', 'describeDataSetRefreshProperties_dataSetId' - The ID of the dataset.
newDescribeDataSetRefreshProperties ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'dataSetId'
  Prelude.Text ->
  DescribeDataSetRefreshProperties
newDescribeDataSetRefreshProperties
  pAwsAccountId_
  pDataSetId_ =
    DescribeDataSetRefreshProperties'
      { awsAccountId =
          pAwsAccountId_,
        dataSetId = pDataSetId_
      }

-- | The Amazon Web Services account ID.
describeDataSetRefreshProperties_awsAccountId :: Lens.Lens' DescribeDataSetRefreshProperties Prelude.Text
describeDataSetRefreshProperties_awsAccountId = Lens.lens (\DescribeDataSetRefreshProperties' {awsAccountId} -> awsAccountId) (\s@DescribeDataSetRefreshProperties' {} a -> s {awsAccountId = a} :: DescribeDataSetRefreshProperties)

-- | The ID of the dataset.
describeDataSetRefreshProperties_dataSetId :: Lens.Lens' DescribeDataSetRefreshProperties Prelude.Text
describeDataSetRefreshProperties_dataSetId = Lens.lens (\DescribeDataSetRefreshProperties' {dataSetId} -> dataSetId) (\s@DescribeDataSetRefreshProperties' {} a -> s {dataSetId = a} :: DescribeDataSetRefreshProperties)

instance
  Core.AWSRequest
    DescribeDataSetRefreshProperties
  where
  type
    AWSResponse DescribeDataSetRefreshProperties =
      DescribeDataSetRefreshPropertiesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDataSetRefreshPropertiesResponse'
            Prelude.<$> (x Data..?> "DataSetRefreshProperties")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeDataSetRefreshProperties
  where
  hashWithSalt
    _salt
    DescribeDataSetRefreshProperties' {..} =
      _salt
        `Prelude.hashWithSalt` awsAccountId
        `Prelude.hashWithSalt` dataSetId

instance
  Prelude.NFData
    DescribeDataSetRefreshProperties
  where
  rnf DescribeDataSetRefreshProperties' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf dataSetId

instance
  Data.ToHeaders
    DescribeDataSetRefreshProperties
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeDataSetRefreshProperties where
  toPath DescribeDataSetRefreshProperties' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/data-sets/",
        Data.toBS dataSetId,
        "/refresh-properties"
      ]

instance
  Data.ToQuery
    DescribeDataSetRefreshProperties
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDataSetRefreshPropertiesResponse' smart constructor.
data DescribeDataSetRefreshPropertiesResponse = DescribeDataSetRefreshPropertiesResponse'
  { -- | The dataset refresh properties.
    dataSetRefreshProperties :: Prelude.Maybe DataSetRefreshProperties,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDataSetRefreshPropertiesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSetRefreshProperties', 'describeDataSetRefreshPropertiesResponse_dataSetRefreshProperties' - The dataset refresh properties.
--
-- 'requestId', 'describeDataSetRefreshPropertiesResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'describeDataSetRefreshPropertiesResponse_status' - The HTTP status of the request.
newDescribeDataSetRefreshPropertiesResponse ::
  -- | 'status'
  Prelude.Int ->
  DescribeDataSetRefreshPropertiesResponse
newDescribeDataSetRefreshPropertiesResponse pStatus_ =
  DescribeDataSetRefreshPropertiesResponse'
    { dataSetRefreshProperties =
        Prelude.Nothing,
      requestId = Prelude.Nothing,
      status = pStatus_
    }

-- | The dataset refresh properties.
describeDataSetRefreshPropertiesResponse_dataSetRefreshProperties :: Lens.Lens' DescribeDataSetRefreshPropertiesResponse (Prelude.Maybe DataSetRefreshProperties)
describeDataSetRefreshPropertiesResponse_dataSetRefreshProperties = Lens.lens (\DescribeDataSetRefreshPropertiesResponse' {dataSetRefreshProperties} -> dataSetRefreshProperties) (\s@DescribeDataSetRefreshPropertiesResponse' {} a -> s {dataSetRefreshProperties = a} :: DescribeDataSetRefreshPropertiesResponse)

-- | The Amazon Web Services request ID for this operation.
describeDataSetRefreshPropertiesResponse_requestId :: Lens.Lens' DescribeDataSetRefreshPropertiesResponse (Prelude.Maybe Prelude.Text)
describeDataSetRefreshPropertiesResponse_requestId = Lens.lens (\DescribeDataSetRefreshPropertiesResponse' {requestId} -> requestId) (\s@DescribeDataSetRefreshPropertiesResponse' {} a -> s {requestId = a} :: DescribeDataSetRefreshPropertiesResponse)

-- | The HTTP status of the request.
describeDataSetRefreshPropertiesResponse_status :: Lens.Lens' DescribeDataSetRefreshPropertiesResponse Prelude.Int
describeDataSetRefreshPropertiesResponse_status = Lens.lens (\DescribeDataSetRefreshPropertiesResponse' {status} -> status) (\s@DescribeDataSetRefreshPropertiesResponse' {} a -> s {status = a} :: DescribeDataSetRefreshPropertiesResponse)

instance
  Prelude.NFData
    DescribeDataSetRefreshPropertiesResponse
  where
  rnf DescribeDataSetRefreshPropertiesResponse' {..} =
    Prelude.rnf dataSetRefreshProperties
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status
