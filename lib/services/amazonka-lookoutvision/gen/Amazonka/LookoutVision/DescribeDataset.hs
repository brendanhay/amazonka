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
-- Module      : Amazonka.LookoutVision.DescribeDataset
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe an Amazon Lookout for Vision dataset.
--
-- This operation requires permissions to perform the
-- @lookoutvision:DescribeDataset@ operation.
module Amazonka.LookoutVision.DescribeDataset
  ( -- * Creating a Request
    DescribeDataset (..),
    newDescribeDataset,

    -- * Request Lenses
    describeDataset_projectName,
    describeDataset_datasetType,

    -- * Destructuring the Response
    DescribeDatasetResponse (..),
    newDescribeDatasetResponse,

    -- * Response Lenses
    describeDatasetResponse_datasetDescription,
    describeDatasetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutVision.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDataset' smart constructor.
data DescribeDataset = DescribeDataset'
  { -- | The name of the project that contains the dataset that you want to
    -- describe.
    projectName :: Prelude.Text,
    -- | The type of the dataset to describe. Specify @train@ to describe the
    -- training dataset. Specify @test@ to describe the test dataset. If you
    -- have a single dataset project, specify @train@
    datasetType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDataset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'projectName', 'describeDataset_projectName' - The name of the project that contains the dataset that you want to
-- describe.
--
-- 'datasetType', 'describeDataset_datasetType' - The type of the dataset to describe. Specify @train@ to describe the
-- training dataset. Specify @test@ to describe the test dataset. If you
-- have a single dataset project, specify @train@
newDescribeDataset ::
  -- | 'projectName'
  Prelude.Text ->
  -- | 'datasetType'
  Prelude.Text ->
  DescribeDataset
newDescribeDataset pProjectName_ pDatasetType_ =
  DescribeDataset'
    { projectName = pProjectName_,
      datasetType = pDatasetType_
    }

-- | The name of the project that contains the dataset that you want to
-- describe.
describeDataset_projectName :: Lens.Lens' DescribeDataset Prelude.Text
describeDataset_projectName = Lens.lens (\DescribeDataset' {projectName} -> projectName) (\s@DescribeDataset' {} a -> s {projectName = a} :: DescribeDataset)

-- | The type of the dataset to describe. Specify @train@ to describe the
-- training dataset. Specify @test@ to describe the test dataset. If you
-- have a single dataset project, specify @train@
describeDataset_datasetType :: Lens.Lens' DescribeDataset Prelude.Text
describeDataset_datasetType = Lens.lens (\DescribeDataset' {datasetType} -> datasetType) (\s@DescribeDataset' {} a -> s {datasetType = a} :: DescribeDataset)

instance Core.AWSRequest DescribeDataset where
  type
    AWSResponse DescribeDataset =
      DescribeDatasetResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDatasetResponse'
            Prelude.<$> (x Data..?> "DatasetDescription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDataset where
  hashWithSalt _salt DescribeDataset' {..} =
    _salt
      `Prelude.hashWithSalt` projectName
      `Prelude.hashWithSalt` datasetType

instance Prelude.NFData DescribeDataset where
  rnf DescribeDataset' {..} =
    Prelude.rnf projectName `Prelude.seq`
      Prelude.rnf datasetType

instance Data.ToHeaders DescribeDataset where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeDataset where
  toPath DescribeDataset' {..} =
    Prelude.mconcat
      [ "/2020-11-20/projects/",
        Data.toBS projectName,
        "/datasets/",
        Data.toBS datasetType
      ]

instance Data.ToQuery DescribeDataset where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDatasetResponse' smart constructor.
data DescribeDatasetResponse = DescribeDatasetResponse'
  { -- | The description of the requested dataset.
    datasetDescription :: Prelude.Maybe DatasetDescription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDatasetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datasetDescription', 'describeDatasetResponse_datasetDescription' - The description of the requested dataset.
--
-- 'httpStatus', 'describeDatasetResponse_httpStatus' - The response's http status code.
newDescribeDatasetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDatasetResponse
newDescribeDatasetResponse pHttpStatus_ =
  DescribeDatasetResponse'
    { datasetDescription =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The description of the requested dataset.
describeDatasetResponse_datasetDescription :: Lens.Lens' DescribeDatasetResponse (Prelude.Maybe DatasetDescription)
describeDatasetResponse_datasetDescription = Lens.lens (\DescribeDatasetResponse' {datasetDescription} -> datasetDescription) (\s@DescribeDatasetResponse' {} a -> s {datasetDescription = a} :: DescribeDatasetResponse)

-- | The response's http status code.
describeDatasetResponse_httpStatus :: Lens.Lens' DescribeDatasetResponse Prelude.Int
describeDatasetResponse_httpStatus = Lens.lens (\DescribeDatasetResponse' {httpStatus} -> httpStatus) (\s@DescribeDatasetResponse' {} a -> s {httpStatus = a} :: DescribeDatasetResponse)

instance Prelude.NFData DescribeDatasetResponse where
  rnf DescribeDatasetResponse' {..} =
    Prelude.rnf datasetDescription `Prelude.seq`
      Prelude.rnf httpStatus
