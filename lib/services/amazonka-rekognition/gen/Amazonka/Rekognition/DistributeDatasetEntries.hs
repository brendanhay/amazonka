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
-- Module      : Amazonka.Rekognition.DistributeDatasetEntries
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Distributes the entries (images) in a training dataset across the
-- training dataset and the test dataset for a project.
-- @DistributeDatasetEntries@ moves 20% of the training dataset images to
-- the test dataset. An entry is a JSON Line that describes an image.
--
-- You supply the Amazon Resource Names (ARN) of a project\'s training
-- dataset and test dataset. The training dataset must contain the images
-- that you want to split. The test dataset must be empty. The datasets
-- must belong to the same project. To create training and test datasets
-- for a project, call CreateDataset.
--
-- Distributing a dataset takes a while to complete. To check the status
-- call @DescribeDataset@. The operation is complete when the @Status@
-- field for the training dataset and the test dataset is
-- @UPDATE_COMPLETE@. If the dataset split fails, the value of @Status@ is
-- @UPDATE_FAILED@.
--
-- This operation requires permissions to perform the
-- @rekognition:DistributeDatasetEntries@ action.
module Amazonka.Rekognition.DistributeDatasetEntries
  ( -- * Creating a Request
    DistributeDatasetEntries (..),
    newDistributeDatasetEntries,

    -- * Request Lenses
    distributeDatasetEntries_datasets,

    -- * Destructuring the Response
    DistributeDatasetEntriesResponse (..),
    newDistributeDatasetEntriesResponse,

    -- * Response Lenses
    distributeDatasetEntriesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDistributeDatasetEntries' smart constructor.
data DistributeDatasetEntries = DistributeDatasetEntries'
  { -- | The ARNS for the training dataset and test dataset that you want to use.
    -- The datasets must belong to the same project. The test dataset must be
    -- empty.
    datasets :: Prelude.NonEmpty DistributeDataset
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DistributeDatasetEntries' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datasets', 'distributeDatasetEntries_datasets' - The ARNS for the training dataset and test dataset that you want to use.
-- The datasets must belong to the same project. The test dataset must be
-- empty.
newDistributeDatasetEntries ::
  -- | 'datasets'
  Prelude.NonEmpty DistributeDataset ->
  DistributeDatasetEntries
newDistributeDatasetEntries pDatasets_ =
  DistributeDatasetEntries'
    { datasets =
        Lens.coerced Lens.# pDatasets_
    }

-- | The ARNS for the training dataset and test dataset that you want to use.
-- The datasets must belong to the same project. The test dataset must be
-- empty.
distributeDatasetEntries_datasets :: Lens.Lens' DistributeDatasetEntries (Prelude.NonEmpty DistributeDataset)
distributeDatasetEntries_datasets = Lens.lens (\DistributeDatasetEntries' {datasets} -> datasets) (\s@DistributeDatasetEntries' {} a -> s {datasets = a} :: DistributeDatasetEntries) Prelude.. Lens.coerced

instance Core.AWSRequest DistributeDatasetEntries where
  type
    AWSResponse DistributeDatasetEntries =
      DistributeDatasetEntriesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DistributeDatasetEntriesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DistributeDatasetEntries where
  hashWithSalt _salt DistributeDatasetEntries' {..} =
    _salt `Prelude.hashWithSalt` datasets

instance Prelude.NFData DistributeDatasetEntries where
  rnf DistributeDatasetEntries' {..} =
    Prelude.rnf datasets

instance Data.ToHeaders DistributeDatasetEntries where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RekognitionService.DistributeDatasetEntries" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DistributeDatasetEntries where
  toJSON DistributeDatasetEntries' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Datasets" Data..= datasets)]
      )

instance Data.ToPath DistributeDatasetEntries where
  toPath = Prelude.const "/"

instance Data.ToQuery DistributeDatasetEntries where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDistributeDatasetEntriesResponse' smart constructor.
data DistributeDatasetEntriesResponse = DistributeDatasetEntriesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DistributeDatasetEntriesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'distributeDatasetEntriesResponse_httpStatus' - The response's http status code.
newDistributeDatasetEntriesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DistributeDatasetEntriesResponse
newDistributeDatasetEntriesResponse pHttpStatus_ =
  DistributeDatasetEntriesResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
distributeDatasetEntriesResponse_httpStatus :: Lens.Lens' DistributeDatasetEntriesResponse Prelude.Int
distributeDatasetEntriesResponse_httpStatus = Lens.lens (\DistributeDatasetEntriesResponse' {httpStatus} -> httpStatus) (\s@DistributeDatasetEntriesResponse' {} a -> s {httpStatus = a} :: DistributeDatasetEntriesResponse)

instance
  Prelude.NFData
    DistributeDatasetEntriesResponse
  where
  rnf DistributeDatasetEntriesResponse' {..} =
    Prelude.rnf httpStatus
