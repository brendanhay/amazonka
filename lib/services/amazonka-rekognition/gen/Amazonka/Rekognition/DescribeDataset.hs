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
-- Module      : Amazonka.Rekognition.DescribeDataset
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an Amazon Rekognition Custom Labels dataset. You can get
-- information such as the current status of a dataset and statistics about
-- the images and labels in a dataset.
--
-- This operation requires permissions to perform the
-- @rekognition:DescribeDataset@ action.
module Amazonka.Rekognition.DescribeDataset
  ( -- * Creating a Request
    DescribeDataset (..),
    newDescribeDataset,

    -- * Request Lenses
    describeDataset_datasetArn,

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
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDataset' smart constructor.
data DescribeDataset = DescribeDataset'
  { -- | The Amazon Resource Name (ARN) of the dataset that you want to describe.
    datasetArn :: Prelude.Text
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
-- 'datasetArn', 'describeDataset_datasetArn' - The Amazon Resource Name (ARN) of the dataset that you want to describe.
newDescribeDataset ::
  -- | 'datasetArn'
  Prelude.Text ->
  DescribeDataset
newDescribeDataset pDatasetArn_ =
  DescribeDataset' {datasetArn = pDatasetArn_}

-- | The Amazon Resource Name (ARN) of the dataset that you want to describe.
describeDataset_datasetArn :: Lens.Lens' DescribeDataset Prelude.Text
describeDataset_datasetArn = Lens.lens (\DescribeDataset' {datasetArn} -> datasetArn) (\s@DescribeDataset' {} a -> s {datasetArn = a} :: DescribeDataset)

instance Core.AWSRequest DescribeDataset where
  type
    AWSResponse DescribeDataset =
      DescribeDatasetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDatasetResponse'
            Prelude.<$> (x Data..?> "DatasetDescription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDataset where
  hashWithSalt _salt DescribeDataset' {..} =
    _salt `Prelude.hashWithSalt` datasetArn

instance Prelude.NFData DescribeDataset where
  rnf DescribeDataset' {..} = Prelude.rnf datasetArn

instance Data.ToHeaders DescribeDataset where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RekognitionService.DescribeDataset" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeDataset where
  toJSON DescribeDataset' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("DatasetArn" Data..= datasetArn)]
      )

instance Data.ToPath DescribeDataset where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeDataset where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDatasetResponse' smart constructor.
data DescribeDatasetResponse = DescribeDatasetResponse'
  { -- | The description for the dataset.
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
-- 'datasetDescription', 'describeDatasetResponse_datasetDescription' - The description for the dataset.
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

-- | The description for the dataset.
describeDatasetResponse_datasetDescription :: Lens.Lens' DescribeDatasetResponse (Prelude.Maybe DatasetDescription)
describeDatasetResponse_datasetDescription = Lens.lens (\DescribeDatasetResponse' {datasetDescription} -> datasetDescription) (\s@DescribeDatasetResponse' {} a -> s {datasetDescription = a} :: DescribeDatasetResponse)

-- | The response's http status code.
describeDatasetResponse_httpStatus :: Lens.Lens' DescribeDatasetResponse Prelude.Int
describeDatasetResponse_httpStatus = Lens.lens (\DescribeDatasetResponse' {httpStatus} -> httpStatus) (\s@DescribeDatasetResponse' {} a -> s {httpStatus = a} :: DescribeDatasetResponse)

instance Prelude.NFData DescribeDatasetResponse where
  rnf DescribeDatasetResponse' {..} =
    Prelude.rnf datasetDescription `Prelude.seq`
      Prelude.rnf httpStatus
