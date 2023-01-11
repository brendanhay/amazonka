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
-- Module      : Amazonka.FinSpaceData.UpdateDataset
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a FinSpace Dataset.
module Amazonka.FinSpaceData.UpdateDataset
  ( -- * Creating a Request
    UpdateDataset (..),
    newUpdateDataset,

    -- * Request Lenses
    updateDataset_alias,
    updateDataset_clientToken,
    updateDataset_datasetDescription,
    updateDataset_schemaDefinition,
    updateDataset_datasetId,
    updateDataset_datasetTitle,
    updateDataset_kind,

    -- * Destructuring the Response
    UpdateDatasetResponse (..),
    newUpdateDatasetResponse,

    -- * Response Lenses
    updateDatasetResponse_datasetId,
    updateDatasetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpaceData.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request for an UpdateDataset operation
--
-- /See:/ 'newUpdateDataset' smart constructor.
data UpdateDataset = UpdateDataset'
  { -- | The unique resource identifier for a Dataset.
    alias :: Prelude.Maybe Prelude.Text,
    -- | A token that ensures idempotency. This token expires in 10 minutes.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A description for the Dataset.
    datasetDescription :: Prelude.Maybe Prelude.Text,
    -- | Definition for a schema on a tabular Dataset.
    schemaDefinition :: Prelude.Maybe SchemaUnion,
    -- | The unique identifier for the Dataset to update.
    datasetId :: Prelude.Text,
    -- | A display title for the Dataset.
    datasetTitle :: Prelude.Text,
    -- | The format in which the Dataset data is structured.
    --
    -- -   @TABULAR@ – Data is structured in a tabular format.
    --
    -- -   @NON_TABULAR@ – Data is structured in a non-tabular format.
    kind :: DatasetKind
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDataset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alias', 'updateDataset_alias' - The unique resource identifier for a Dataset.
--
-- 'clientToken', 'updateDataset_clientToken' - A token that ensures idempotency. This token expires in 10 minutes.
--
-- 'datasetDescription', 'updateDataset_datasetDescription' - A description for the Dataset.
--
-- 'schemaDefinition', 'updateDataset_schemaDefinition' - Definition for a schema on a tabular Dataset.
--
-- 'datasetId', 'updateDataset_datasetId' - The unique identifier for the Dataset to update.
--
-- 'datasetTitle', 'updateDataset_datasetTitle' - A display title for the Dataset.
--
-- 'kind', 'updateDataset_kind' - The format in which the Dataset data is structured.
--
-- -   @TABULAR@ – Data is structured in a tabular format.
--
-- -   @NON_TABULAR@ – Data is structured in a non-tabular format.
newUpdateDataset ::
  -- | 'datasetId'
  Prelude.Text ->
  -- | 'datasetTitle'
  Prelude.Text ->
  -- | 'kind'
  DatasetKind ->
  UpdateDataset
newUpdateDataset pDatasetId_ pDatasetTitle_ pKind_ =
  UpdateDataset'
    { alias = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      datasetDescription = Prelude.Nothing,
      schemaDefinition = Prelude.Nothing,
      datasetId = pDatasetId_,
      datasetTitle = pDatasetTitle_,
      kind = pKind_
    }

-- | The unique resource identifier for a Dataset.
updateDataset_alias :: Lens.Lens' UpdateDataset (Prelude.Maybe Prelude.Text)
updateDataset_alias = Lens.lens (\UpdateDataset' {alias} -> alias) (\s@UpdateDataset' {} a -> s {alias = a} :: UpdateDataset)

-- | A token that ensures idempotency. This token expires in 10 minutes.
updateDataset_clientToken :: Lens.Lens' UpdateDataset (Prelude.Maybe Prelude.Text)
updateDataset_clientToken = Lens.lens (\UpdateDataset' {clientToken} -> clientToken) (\s@UpdateDataset' {} a -> s {clientToken = a} :: UpdateDataset)

-- | A description for the Dataset.
updateDataset_datasetDescription :: Lens.Lens' UpdateDataset (Prelude.Maybe Prelude.Text)
updateDataset_datasetDescription = Lens.lens (\UpdateDataset' {datasetDescription} -> datasetDescription) (\s@UpdateDataset' {} a -> s {datasetDescription = a} :: UpdateDataset)

-- | Definition for a schema on a tabular Dataset.
updateDataset_schemaDefinition :: Lens.Lens' UpdateDataset (Prelude.Maybe SchemaUnion)
updateDataset_schemaDefinition = Lens.lens (\UpdateDataset' {schemaDefinition} -> schemaDefinition) (\s@UpdateDataset' {} a -> s {schemaDefinition = a} :: UpdateDataset)

-- | The unique identifier for the Dataset to update.
updateDataset_datasetId :: Lens.Lens' UpdateDataset Prelude.Text
updateDataset_datasetId = Lens.lens (\UpdateDataset' {datasetId} -> datasetId) (\s@UpdateDataset' {} a -> s {datasetId = a} :: UpdateDataset)

-- | A display title for the Dataset.
updateDataset_datasetTitle :: Lens.Lens' UpdateDataset Prelude.Text
updateDataset_datasetTitle = Lens.lens (\UpdateDataset' {datasetTitle} -> datasetTitle) (\s@UpdateDataset' {} a -> s {datasetTitle = a} :: UpdateDataset)

-- | The format in which the Dataset data is structured.
--
-- -   @TABULAR@ – Data is structured in a tabular format.
--
-- -   @NON_TABULAR@ – Data is structured in a non-tabular format.
updateDataset_kind :: Lens.Lens' UpdateDataset DatasetKind
updateDataset_kind = Lens.lens (\UpdateDataset' {kind} -> kind) (\s@UpdateDataset' {} a -> s {kind = a} :: UpdateDataset)

instance Core.AWSRequest UpdateDataset where
  type
    AWSResponse UpdateDataset =
      UpdateDatasetResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDatasetResponse'
            Prelude.<$> (x Data..?> "datasetId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDataset where
  hashWithSalt _salt UpdateDataset' {..} =
    _salt `Prelude.hashWithSalt` alias
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` datasetDescription
      `Prelude.hashWithSalt` schemaDefinition
      `Prelude.hashWithSalt` datasetId
      `Prelude.hashWithSalt` datasetTitle
      `Prelude.hashWithSalt` kind

instance Prelude.NFData UpdateDataset where
  rnf UpdateDataset' {..} =
    Prelude.rnf alias
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf datasetDescription
      `Prelude.seq` Prelude.rnf schemaDefinition
      `Prelude.seq` Prelude.rnf datasetId
      `Prelude.seq` Prelude.rnf datasetTitle
      `Prelude.seq` Prelude.rnf kind

instance Data.ToHeaders UpdateDataset where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateDataset where
  toJSON UpdateDataset' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("alias" Data..=) Prelude.<$> alias,
            ("clientToken" Data..=) Prelude.<$> clientToken,
            ("datasetDescription" Data..=)
              Prelude.<$> datasetDescription,
            ("schemaDefinition" Data..=)
              Prelude.<$> schemaDefinition,
            Prelude.Just ("datasetTitle" Data..= datasetTitle),
            Prelude.Just ("kind" Data..= kind)
          ]
      )

instance Data.ToPath UpdateDataset where
  toPath UpdateDataset' {..} =
    Prelude.mconcat
      ["/datasetsv2/", Data.toBS datasetId]

instance Data.ToQuery UpdateDataset where
  toQuery = Prelude.const Prelude.mempty

-- | The response from an UpdateDataset operation
--
-- /See:/ 'newUpdateDatasetResponse' smart constructor.
data UpdateDatasetResponse = UpdateDatasetResponse'
  { -- | The unique identifier for updated Dataset.
    datasetId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDatasetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datasetId', 'updateDatasetResponse_datasetId' - The unique identifier for updated Dataset.
--
-- 'httpStatus', 'updateDatasetResponse_httpStatus' - The response's http status code.
newUpdateDatasetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateDatasetResponse
newUpdateDatasetResponse pHttpStatus_ =
  UpdateDatasetResponse'
    { datasetId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier for updated Dataset.
updateDatasetResponse_datasetId :: Lens.Lens' UpdateDatasetResponse (Prelude.Maybe Prelude.Text)
updateDatasetResponse_datasetId = Lens.lens (\UpdateDatasetResponse' {datasetId} -> datasetId) (\s@UpdateDatasetResponse' {} a -> s {datasetId = a} :: UpdateDatasetResponse)

-- | The response's http status code.
updateDatasetResponse_httpStatus :: Lens.Lens' UpdateDatasetResponse Prelude.Int
updateDatasetResponse_httpStatus = Lens.lens (\UpdateDatasetResponse' {httpStatus} -> httpStatus) (\s@UpdateDatasetResponse' {} a -> s {httpStatus = a} :: UpdateDatasetResponse)

instance Prelude.NFData UpdateDatasetResponse where
  rnf UpdateDatasetResponse' {..} =
    Prelude.rnf datasetId
      `Prelude.seq` Prelude.rnf httpStatus
