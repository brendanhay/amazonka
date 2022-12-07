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
-- Module      : Amazonka.FinSpaceData.GetDataset
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a Dataset.
module Amazonka.FinSpaceData.GetDataset
  ( -- * Creating a Request
    GetDataset (..),
    newGetDataset,

    -- * Request Lenses
    getDataset_datasetId,

    -- * Destructuring the Response
    GetDatasetResponse (..),
    newGetDatasetResponse,

    -- * Response Lenses
    getDatasetResponse_alias,
    getDatasetResponse_datasetDescription,
    getDatasetResponse_datasetTitle,
    getDatasetResponse_kind,
    getDatasetResponse_status,
    getDatasetResponse_datasetArn,
    getDatasetResponse_lastModifiedTime,
    getDatasetResponse_datasetId,
    getDatasetResponse_createTime,
    getDatasetResponse_schemaDefinition,
    getDatasetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpaceData.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request for the GetDataset operation.
--
-- /See:/ 'newGetDataset' smart constructor.
data GetDataset = GetDataset'
  { -- | The unique identifier for a Dataset.
    datasetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDataset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datasetId', 'getDataset_datasetId' - The unique identifier for a Dataset.
newGetDataset ::
  -- | 'datasetId'
  Prelude.Text ->
  GetDataset
newGetDataset pDatasetId_ =
  GetDataset' {datasetId = pDatasetId_}

-- | The unique identifier for a Dataset.
getDataset_datasetId :: Lens.Lens' GetDataset Prelude.Text
getDataset_datasetId = Lens.lens (\GetDataset' {datasetId} -> datasetId) (\s@GetDataset' {} a -> s {datasetId = a} :: GetDataset)

instance Core.AWSRequest GetDataset where
  type AWSResponse GetDataset = GetDatasetResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDatasetResponse'
            Prelude.<$> (x Data..?> "alias")
            Prelude.<*> (x Data..?> "datasetDescription")
            Prelude.<*> (x Data..?> "datasetTitle")
            Prelude.<*> (x Data..?> "kind")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (x Data..?> "datasetArn")
            Prelude.<*> (x Data..?> "lastModifiedTime")
            Prelude.<*> (x Data..?> "datasetId")
            Prelude.<*> (x Data..?> "createTime")
            Prelude.<*> (x Data..?> "schemaDefinition")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDataset where
  hashWithSalt _salt GetDataset' {..} =
    _salt `Prelude.hashWithSalt` datasetId

instance Prelude.NFData GetDataset where
  rnf GetDataset' {..} = Prelude.rnf datasetId

instance Data.ToHeaders GetDataset where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetDataset where
  toPath GetDataset' {..} =
    Prelude.mconcat
      ["/datasetsv2/", Data.toBS datasetId]

instance Data.ToQuery GetDataset where
  toQuery = Prelude.const Prelude.mempty

-- | Response for the GetDataset operation
--
-- /See:/ 'newGetDatasetResponse' smart constructor.
data GetDatasetResponse = GetDatasetResponse'
  { -- | The unique resource identifier for a Dataset.
    alias :: Prelude.Maybe Prelude.Text,
    -- | A description of the Dataset.
    datasetDescription :: Prelude.Maybe Prelude.Text,
    -- | Display title for a Dataset.
    datasetTitle :: Prelude.Maybe Prelude.Text,
    -- | The format in which Dataset data is structured.
    --
    -- -   @TABULAR@ – Data is structured in a tabular format.
    --
    -- -   @NON_TABULAR@ – Data is structured in a non-tabular format.
    kind :: Prelude.Maybe DatasetKind,
    -- | Status of the Dataset creation.
    --
    -- -   @PENDING@ – Dataset is pending creation.
    --
    -- -   @FAILED@ – Dataset creation has failed.
    --
    -- -   @SUCCESS@ – Dataset creation has succeeded.
    --
    -- -   @RUNNING@ – Dataset creation is running.
    status :: Prelude.Maybe DatasetStatus,
    -- | The ARN identifier of the Dataset.
    datasetArn :: Prelude.Maybe Prelude.Text,
    -- | The last time that the Dataset was modified. The value is determined as
    -- epoch time in milliseconds. For example, the value for Monday, November
    -- 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
    lastModifiedTime :: Prelude.Maybe Prelude.Integer,
    -- | The unique identifier for a Dataset.
    datasetId :: Prelude.Maybe Prelude.Text,
    -- | The timestamp at which the Dataset was created in FinSpace. The value is
    -- determined as epoch time in milliseconds. For example, the value for
    -- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
    createTime :: Prelude.Maybe Prelude.Integer,
    -- | Definition for a schema on a tabular Dataset.
    schemaDefinition :: Prelude.Maybe SchemaUnion,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDatasetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alias', 'getDatasetResponse_alias' - The unique resource identifier for a Dataset.
--
-- 'datasetDescription', 'getDatasetResponse_datasetDescription' - A description of the Dataset.
--
-- 'datasetTitle', 'getDatasetResponse_datasetTitle' - Display title for a Dataset.
--
-- 'kind', 'getDatasetResponse_kind' - The format in which Dataset data is structured.
--
-- -   @TABULAR@ – Data is structured in a tabular format.
--
-- -   @NON_TABULAR@ – Data is structured in a non-tabular format.
--
-- 'status', 'getDatasetResponse_status' - Status of the Dataset creation.
--
-- -   @PENDING@ – Dataset is pending creation.
--
-- -   @FAILED@ – Dataset creation has failed.
--
-- -   @SUCCESS@ – Dataset creation has succeeded.
--
-- -   @RUNNING@ – Dataset creation is running.
--
-- 'datasetArn', 'getDatasetResponse_datasetArn' - The ARN identifier of the Dataset.
--
-- 'lastModifiedTime', 'getDatasetResponse_lastModifiedTime' - The last time that the Dataset was modified. The value is determined as
-- epoch time in milliseconds. For example, the value for Monday, November
-- 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
--
-- 'datasetId', 'getDatasetResponse_datasetId' - The unique identifier for a Dataset.
--
-- 'createTime', 'getDatasetResponse_createTime' - The timestamp at which the Dataset was created in FinSpace. The value is
-- determined as epoch time in milliseconds. For example, the value for
-- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
--
-- 'schemaDefinition', 'getDatasetResponse_schemaDefinition' - Definition for a schema on a tabular Dataset.
--
-- 'httpStatus', 'getDatasetResponse_httpStatus' - The response's http status code.
newGetDatasetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDatasetResponse
newGetDatasetResponse pHttpStatus_ =
  GetDatasetResponse'
    { alias = Prelude.Nothing,
      datasetDescription = Prelude.Nothing,
      datasetTitle = Prelude.Nothing,
      kind = Prelude.Nothing,
      status = Prelude.Nothing,
      datasetArn = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      datasetId = Prelude.Nothing,
      createTime = Prelude.Nothing,
      schemaDefinition = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique resource identifier for a Dataset.
getDatasetResponse_alias :: Lens.Lens' GetDatasetResponse (Prelude.Maybe Prelude.Text)
getDatasetResponse_alias = Lens.lens (\GetDatasetResponse' {alias} -> alias) (\s@GetDatasetResponse' {} a -> s {alias = a} :: GetDatasetResponse)

-- | A description of the Dataset.
getDatasetResponse_datasetDescription :: Lens.Lens' GetDatasetResponse (Prelude.Maybe Prelude.Text)
getDatasetResponse_datasetDescription = Lens.lens (\GetDatasetResponse' {datasetDescription} -> datasetDescription) (\s@GetDatasetResponse' {} a -> s {datasetDescription = a} :: GetDatasetResponse)

-- | Display title for a Dataset.
getDatasetResponse_datasetTitle :: Lens.Lens' GetDatasetResponse (Prelude.Maybe Prelude.Text)
getDatasetResponse_datasetTitle = Lens.lens (\GetDatasetResponse' {datasetTitle} -> datasetTitle) (\s@GetDatasetResponse' {} a -> s {datasetTitle = a} :: GetDatasetResponse)

-- | The format in which Dataset data is structured.
--
-- -   @TABULAR@ – Data is structured in a tabular format.
--
-- -   @NON_TABULAR@ – Data is structured in a non-tabular format.
getDatasetResponse_kind :: Lens.Lens' GetDatasetResponse (Prelude.Maybe DatasetKind)
getDatasetResponse_kind = Lens.lens (\GetDatasetResponse' {kind} -> kind) (\s@GetDatasetResponse' {} a -> s {kind = a} :: GetDatasetResponse)

-- | Status of the Dataset creation.
--
-- -   @PENDING@ – Dataset is pending creation.
--
-- -   @FAILED@ – Dataset creation has failed.
--
-- -   @SUCCESS@ – Dataset creation has succeeded.
--
-- -   @RUNNING@ – Dataset creation is running.
getDatasetResponse_status :: Lens.Lens' GetDatasetResponse (Prelude.Maybe DatasetStatus)
getDatasetResponse_status = Lens.lens (\GetDatasetResponse' {status} -> status) (\s@GetDatasetResponse' {} a -> s {status = a} :: GetDatasetResponse)

-- | The ARN identifier of the Dataset.
getDatasetResponse_datasetArn :: Lens.Lens' GetDatasetResponse (Prelude.Maybe Prelude.Text)
getDatasetResponse_datasetArn = Lens.lens (\GetDatasetResponse' {datasetArn} -> datasetArn) (\s@GetDatasetResponse' {} a -> s {datasetArn = a} :: GetDatasetResponse)

-- | The last time that the Dataset was modified. The value is determined as
-- epoch time in milliseconds. For example, the value for Monday, November
-- 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
getDatasetResponse_lastModifiedTime :: Lens.Lens' GetDatasetResponse (Prelude.Maybe Prelude.Integer)
getDatasetResponse_lastModifiedTime = Lens.lens (\GetDatasetResponse' {lastModifiedTime} -> lastModifiedTime) (\s@GetDatasetResponse' {} a -> s {lastModifiedTime = a} :: GetDatasetResponse)

-- | The unique identifier for a Dataset.
getDatasetResponse_datasetId :: Lens.Lens' GetDatasetResponse (Prelude.Maybe Prelude.Text)
getDatasetResponse_datasetId = Lens.lens (\GetDatasetResponse' {datasetId} -> datasetId) (\s@GetDatasetResponse' {} a -> s {datasetId = a} :: GetDatasetResponse)

-- | The timestamp at which the Dataset was created in FinSpace. The value is
-- determined as epoch time in milliseconds. For example, the value for
-- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
getDatasetResponse_createTime :: Lens.Lens' GetDatasetResponse (Prelude.Maybe Prelude.Integer)
getDatasetResponse_createTime = Lens.lens (\GetDatasetResponse' {createTime} -> createTime) (\s@GetDatasetResponse' {} a -> s {createTime = a} :: GetDatasetResponse)

-- | Definition for a schema on a tabular Dataset.
getDatasetResponse_schemaDefinition :: Lens.Lens' GetDatasetResponse (Prelude.Maybe SchemaUnion)
getDatasetResponse_schemaDefinition = Lens.lens (\GetDatasetResponse' {schemaDefinition} -> schemaDefinition) (\s@GetDatasetResponse' {} a -> s {schemaDefinition = a} :: GetDatasetResponse)

-- | The response's http status code.
getDatasetResponse_httpStatus :: Lens.Lens' GetDatasetResponse Prelude.Int
getDatasetResponse_httpStatus = Lens.lens (\GetDatasetResponse' {httpStatus} -> httpStatus) (\s@GetDatasetResponse' {} a -> s {httpStatus = a} :: GetDatasetResponse)

instance Prelude.NFData GetDatasetResponse where
  rnf GetDatasetResponse' {..} =
    Prelude.rnf alias
      `Prelude.seq` Prelude.rnf datasetDescription
      `Prelude.seq` Prelude.rnf datasetTitle
      `Prelude.seq` Prelude.rnf kind
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf datasetArn
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf datasetId
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf schemaDefinition
      `Prelude.seq` Prelude.rnf httpStatus
