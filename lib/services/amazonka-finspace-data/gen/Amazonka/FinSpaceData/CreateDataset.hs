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
-- Module      : Amazonka.FinSpaceData.CreateDataset
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new FinSpace Dataset.
module Amazonka.FinSpaceData.CreateDataset
  ( -- * Creating a Request
    CreateDataset (..),
    newCreateDataset,

    -- * Request Lenses
    createDataset_alias,
    createDataset_clientToken,
    createDataset_datasetDescription,
    createDataset_ownerInfo,
    createDataset_schemaDefinition,
    createDataset_datasetTitle,
    createDataset_kind,
    createDataset_permissionGroupParams,

    -- * Destructuring the Response
    CreateDatasetResponse (..),
    newCreateDatasetResponse,

    -- * Response Lenses
    createDatasetResponse_datasetId,
    createDatasetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpaceData.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request for a CreateDataset operation
--
-- /See:/ 'newCreateDataset' smart constructor.
data CreateDataset = CreateDataset'
  { -- | The unique resource identifier for a Dataset.
    alias :: Prelude.Maybe Prelude.Text,
    -- | A token that ensures idempotency. This token expires in 10 minutes.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Description of a Dataset.
    datasetDescription :: Prelude.Maybe Prelude.Text,
    -- | Contact information for a Dataset owner.
    ownerInfo :: Prelude.Maybe DatasetOwnerInfo,
    -- | Definition for a schema on a tabular Dataset.
    schemaDefinition :: Prelude.Maybe SchemaUnion,
    -- | Display title for a FinSpace Dataset.
    datasetTitle :: Prelude.Text,
    -- | The format in which Dataset data is structured.
    --
    -- -   @TABULAR@ – Data is structured in a tabular format.
    --
    -- -   @NON_TABULAR@ – Data is structured in a non-tabular format.
    kind :: DatasetKind,
    -- | Permission group parameters for Dataset permissions.
    permissionGroupParams :: PermissionGroupParams
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDataset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alias', 'createDataset_alias' - The unique resource identifier for a Dataset.
--
-- 'clientToken', 'createDataset_clientToken' - A token that ensures idempotency. This token expires in 10 minutes.
--
-- 'datasetDescription', 'createDataset_datasetDescription' - Description of a Dataset.
--
-- 'ownerInfo', 'createDataset_ownerInfo' - Contact information for a Dataset owner.
--
-- 'schemaDefinition', 'createDataset_schemaDefinition' - Definition for a schema on a tabular Dataset.
--
-- 'datasetTitle', 'createDataset_datasetTitle' - Display title for a FinSpace Dataset.
--
-- 'kind', 'createDataset_kind' - The format in which Dataset data is structured.
--
-- -   @TABULAR@ – Data is structured in a tabular format.
--
-- -   @NON_TABULAR@ – Data is structured in a non-tabular format.
--
-- 'permissionGroupParams', 'createDataset_permissionGroupParams' - Permission group parameters for Dataset permissions.
newCreateDataset ::
  -- | 'datasetTitle'
  Prelude.Text ->
  -- | 'kind'
  DatasetKind ->
  -- | 'permissionGroupParams'
  PermissionGroupParams ->
  CreateDataset
newCreateDataset
  pDatasetTitle_
  pKind_
  pPermissionGroupParams_ =
    CreateDataset'
      { alias = Prelude.Nothing,
        clientToken = Prelude.Nothing,
        datasetDescription = Prelude.Nothing,
        ownerInfo = Prelude.Nothing,
        schemaDefinition = Prelude.Nothing,
        datasetTitle = pDatasetTitle_,
        kind = pKind_,
        permissionGroupParams = pPermissionGroupParams_
      }

-- | The unique resource identifier for a Dataset.
createDataset_alias :: Lens.Lens' CreateDataset (Prelude.Maybe Prelude.Text)
createDataset_alias = Lens.lens (\CreateDataset' {alias} -> alias) (\s@CreateDataset' {} a -> s {alias = a} :: CreateDataset)

-- | A token that ensures idempotency. This token expires in 10 minutes.
createDataset_clientToken :: Lens.Lens' CreateDataset (Prelude.Maybe Prelude.Text)
createDataset_clientToken = Lens.lens (\CreateDataset' {clientToken} -> clientToken) (\s@CreateDataset' {} a -> s {clientToken = a} :: CreateDataset)

-- | Description of a Dataset.
createDataset_datasetDescription :: Lens.Lens' CreateDataset (Prelude.Maybe Prelude.Text)
createDataset_datasetDescription = Lens.lens (\CreateDataset' {datasetDescription} -> datasetDescription) (\s@CreateDataset' {} a -> s {datasetDescription = a} :: CreateDataset)

-- | Contact information for a Dataset owner.
createDataset_ownerInfo :: Lens.Lens' CreateDataset (Prelude.Maybe DatasetOwnerInfo)
createDataset_ownerInfo = Lens.lens (\CreateDataset' {ownerInfo} -> ownerInfo) (\s@CreateDataset' {} a -> s {ownerInfo = a} :: CreateDataset)

-- | Definition for a schema on a tabular Dataset.
createDataset_schemaDefinition :: Lens.Lens' CreateDataset (Prelude.Maybe SchemaUnion)
createDataset_schemaDefinition = Lens.lens (\CreateDataset' {schemaDefinition} -> schemaDefinition) (\s@CreateDataset' {} a -> s {schemaDefinition = a} :: CreateDataset)

-- | Display title for a FinSpace Dataset.
createDataset_datasetTitle :: Lens.Lens' CreateDataset Prelude.Text
createDataset_datasetTitle = Lens.lens (\CreateDataset' {datasetTitle} -> datasetTitle) (\s@CreateDataset' {} a -> s {datasetTitle = a} :: CreateDataset)

-- | The format in which Dataset data is structured.
--
-- -   @TABULAR@ – Data is structured in a tabular format.
--
-- -   @NON_TABULAR@ – Data is structured in a non-tabular format.
createDataset_kind :: Lens.Lens' CreateDataset DatasetKind
createDataset_kind = Lens.lens (\CreateDataset' {kind} -> kind) (\s@CreateDataset' {} a -> s {kind = a} :: CreateDataset)

-- | Permission group parameters for Dataset permissions.
createDataset_permissionGroupParams :: Lens.Lens' CreateDataset PermissionGroupParams
createDataset_permissionGroupParams = Lens.lens (\CreateDataset' {permissionGroupParams} -> permissionGroupParams) (\s@CreateDataset' {} a -> s {permissionGroupParams = a} :: CreateDataset)

instance Core.AWSRequest CreateDataset where
  type
    AWSResponse CreateDataset =
      CreateDatasetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDatasetResponse'
            Prelude.<$> (x Data..?> "datasetId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDataset where
  hashWithSalt _salt CreateDataset' {..} =
    _salt `Prelude.hashWithSalt` alias
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` datasetDescription
      `Prelude.hashWithSalt` ownerInfo
      `Prelude.hashWithSalt` schemaDefinition
      `Prelude.hashWithSalt` datasetTitle
      `Prelude.hashWithSalt` kind
      `Prelude.hashWithSalt` permissionGroupParams

instance Prelude.NFData CreateDataset where
  rnf CreateDataset' {..} =
    Prelude.rnf alias
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf datasetDescription
      `Prelude.seq` Prelude.rnf ownerInfo
      `Prelude.seq` Prelude.rnf schemaDefinition
      `Prelude.seq` Prelude.rnf datasetTitle
      `Prelude.seq` Prelude.rnf kind
      `Prelude.seq` Prelude.rnf permissionGroupParams

instance Data.ToHeaders CreateDataset where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateDataset where
  toJSON CreateDataset' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("alias" Data..=) Prelude.<$> alias,
            ("clientToken" Data..=) Prelude.<$> clientToken,
            ("datasetDescription" Data..=)
              Prelude.<$> datasetDescription,
            ("ownerInfo" Data..=) Prelude.<$> ownerInfo,
            ("schemaDefinition" Data..=)
              Prelude.<$> schemaDefinition,
            Prelude.Just ("datasetTitle" Data..= datasetTitle),
            Prelude.Just ("kind" Data..= kind),
            Prelude.Just
              ( "permissionGroupParams"
                  Data..= permissionGroupParams
              )
          ]
      )

instance Data.ToPath CreateDataset where
  toPath = Prelude.const "/datasetsv2"

instance Data.ToQuery CreateDataset where
  toQuery = Prelude.const Prelude.mempty

-- | The response from a CreateDataset operation
--
-- /See:/ 'newCreateDatasetResponse' smart constructor.
data CreateDatasetResponse = CreateDatasetResponse'
  { -- | The unique identifier for the created Dataset.
    datasetId :: Prelude.Maybe Prelude.Text,
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
-- 'datasetId', 'createDatasetResponse_datasetId' - The unique identifier for the created Dataset.
--
-- 'httpStatus', 'createDatasetResponse_httpStatus' - The response's http status code.
newCreateDatasetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDatasetResponse
newCreateDatasetResponse pHttpStatus_ =
  CreateDatasetResponse'
    { datasetId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier for the created Dataset.
createDatasetResponse_datasetId :: Lens.Lens' CreateDatasetResponse (Prelude.Maybe Prelude.Text)
createDatasetResponse_datasetId = Lens.lens (\CreateDatasetResponse' {datasetId} -> datasetId) (\s@CreateDatasetResponse' {} a -> s {datasetId = a} :: CreateDatasetResponse)

-- | The response's http status code.
createDatasetResponse_httpStatus :: Lens.Lens' CreateDatasetResponse Prelude.Int
createDatasetResponse_httpStatus = Lens.lens (\CreateDatasetResponse' {httpStatus} -> httpStatus) (\s@CreateDatasetResponse' {} a -> s {httpStatus = a} :: CreateDatasetResponse)

instance Prelude.NFData CreateDatasetResponse where
  rnf CreateDatasetResponse' {..} =
    Prelude.rnf datasetId
      `Prelude.seq` Prelude.rnf httpStatus
