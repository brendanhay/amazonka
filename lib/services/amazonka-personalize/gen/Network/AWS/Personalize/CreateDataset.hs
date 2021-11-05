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
-- Module      : Amazonka.Personalize.CreateDataset
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an empty dataset and adds it to the specified dataset group. Use
-- CreateDatasetImportJob to import your training data to a dataset.
--
-- There are three types of datasets:
--
-- -   Interactions
--
-- -   Items
--
-- -   Users
--
-- Each dataset type has an associated schema with required field types.
-- Only the @Interactions@ dataset is required in order to train a model
-- (also referred to as creating a solution).
--
-- A dataset can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
-- -   DELETE PENDING > DELETE IN_PROGRESS
--
-- To get the status of the dataset, call DescribeDataset.
--
-- __Related APIs__
--
-- -   CreateDatasetGroup
--
-- -   ListDatasets
--
-- -   DescribeDataset
--
-- -   DeleteDataset
module Amazonka.Personalize.CreateDataset
  ( -- * Creating a Request
    CreateDataset (..),
    newCreateDataset,

    -- * Request Lenses
    createDataset_name,
    createDataset_schemaArn,
    createDataset_datasetGroupArn,
    createDataset_datasetType,

    -- * Destructuring the Response
    CreateDatasetResponse (..),
    newCreateDatasetResponse,

    -- * Response Lenses
    createDatasetResponse_datasetArn,
    createDatasetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Personalize.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDataset' smart constructor.
data CreateDataset = CreateDataset'
  { -- | The name for the dataset.
    name :: Prelude.Text,
    -- | The ARN of the schema to associate with the dataset. The schema defines
    -- the dataset fields.
    schemaArn :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the dataset group to add the dataset
    -- to.
    datasetGroupArn :: Prelude.Text,
    -- | The type of dataset.
    --
    -- One of the following (case insensitive) values:
    --
    -- -   Interactions
    --
    -- -   Items
    --
    -- -   Users
    datasetType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDataset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'createDataset_name' - The name for the dataset.
--
-- 'schemaArn', 'createDataset_schemaArn' - The ARN of the schema to associate with the dataset. The schema defines
-- the dataset fields.
--
-- 'datasetGroupArn', 'createDataset_datasetGroupArn' - The Amazon Resource Name (ARN) of the dataset group to add the dataset
-- to.
--
-- 'datasetType', 'createDataset_datasetType' - The type of dataset.
--
-- One of the following (case insensitive) values:
--
-- -   Interactions
--
-- -   Items
--
-- -   Users
newCreateDataset ::
  -- | 'name'
  Prelude.Text ->
  -- | 'schemaArn'
  Prelude.Text ->
  -- | 'datasetGroupArn'
  Prelude.Text ->
  -- | 'datasetType'
  Prelude.Text ->
  CreateDataset
newCreateDataset
  pName_
  pSchemaArn_
  pDatasetGroupArn_
  pDatasetType_ =
    CreateDataset'
      { name = pName_,
        schemaArn = pSchemaArn_,
        datasetGroupArn = pDatasetGroupArn_,
        datasetType = pDatasetType_
      }

-- | The name for the dataset.
createDataset_name :: Lens.Lens' CreateDataset Prelude.Text
createDataset_name = Lens.lens (\CreateDataset' {name} -> name) (\s@CreateDataset' {} a -> s {name = a} :: CreateDataset)

-- | The ARN of the schema to associate with the dataset. The schema defines
-- the dataset fields.
createDataset_schemaArn :: Lens.Lens' CreateDataset Prelude.Text
createDataset_schemaArn = Lens.lens (\CreateDataset' {schemaArn} -> schemaArn) (\s@CreateDataset' {} a -> s {schemaArn = a} :: CreateDataset)

-- | The Amazon Resource Name (ARN) of the dataset group to add the dataset
-- to.
createDataset_datasetGroupArn :: Lens.Lens' CreateDataset Prelude.Text
createDataset_datasetGroupArn = Lens.lens (\CreateDataset' {datasetGroupArn} -> datasetGroupArn) (\s@CreateDataset' {} a -> s {datasetGroupArn = a} :: CreateDataset)

-- | The type of dataset.
--
-- One of the following (case insensitive) values:
--
-- -   Interactions
--
-- -   Items
--
-- -   Users
createDataset_datasetType :: Lens.Lens' CreateDataset Prelude.Text
createDataset_datasetType = Lens.lens (\CreateDataset' {datasetType} -> datasetType) (\s@CreateDataset' {} a -> s {datasetType = a} :: CreateDataset)

instance Core.AWSRequest CreateDataset where
  type
    AWSResponse CreateDataset =
      CreateDatasetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDatasetResponse'
            Prelude.<$> (x Core..?> "datasetArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDataset

instance Prelude.NFData CreateDataset

instance Core.ToHeaders CreateDataset where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonPersonalize.CreateDataset" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateDataset where
  toJSON CreateDataset' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Core..= name),
            Prelude.Just ("schemaArn" Core..= schemaArn),
            Prelude.Just
              ("datasetGroupArn" Core..= datasetGroupArn),
            Prelude.Just ("datasetType" Core..= datasetType)
          ]
      )

instance Core.ToPath CreateDataset where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateDataset where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDatasetResponse' smart constructor.
data CreateDatasetResponse = CreateDatasetResponse'
  { -- | The ARN of the dataset.
    datasetArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDatasetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datasetArn', 'createDatasetResponse_datasetArn' - The ARN of the dataset.
--
-- 'httpStatus', 'createDatasetResponse_httpStatus' - The response's http status code.
newCreateDatasetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDatasetResponse
newCreateDatasetResponse pHttpStatus_ =
  CreateDatasetResponse'
    { datasetArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the dataset.
createDatasetResponse_datasetArn :: Lens.Lens' CreateDatasetResponse (Prelude.Maybe Prelude.Text)
createDatasetResponse_datasetArn = Lens.lens (\CreateDatasetResponse' {datasetArn} -> datasetArn) (\s@CreateDatasetResponse' {} a -> s {datasetArn = a} :: CreateDatasetResponse)

-- | The response's http status code.
createDatasetResponse_httpStatus :: Lens.Lens' CreateDatasetResponse Prelude.Int
createDatasetResponse_httpStatus = Lens.lens (\CreateDatasetResponse' {httpStatus} -> httpStatus) (\s@CreateDatasetResponse' {} a -> s {httpStatus = a} :: CreateDatasetResponse)

instance Prelude.NFData CreateDatasetResponse
