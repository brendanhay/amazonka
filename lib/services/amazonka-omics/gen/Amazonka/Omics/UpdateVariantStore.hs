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
-- Module      : Amazonka.Omics.UpdateVariantStore
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a variant store.
module Amazonka.Omics.UpdateVariantStore
  ( -- * Creating a Request
    UpdateVariantStore (..),
    newUpdateVariantStore,

    -- * Request Lenses
    updateVariantStore_description,
    updateVariantStore_name,

    -- * Destructuring the Response
    UpdateVariantStoreResponse (..),
    newUpdateVariantStoreResponse,

    -- * Response Lenses
    updateVariantStoreResponse_httpStatus,
    updateVariantStoreResponse_creationTime,
    updateVariantStoreResponse_description,
    updateVariantStoreResponse_id,
    updateVariantStoreResponse_name,
    updateVariantStoreResponse_reference,
    updateVariantStoreResponse_status,
    updateVariantStoreResponse_updateTime,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateVariantStore' smart constructor.
data UpdateVariantStore = UpdateVariantStore'
  { -- | A description for the store.
    description :: Prelude.Maybe Prelude.Text,
    -- | A name for the store.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateVariantStore' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateVariantStore_description' - A description for the store.
--
-- 'name', 'updateVariantStore_name' - A name for the store.
newUpdateVariantStore ::
  -- | 'name'
  Prelude.Text ->
  UpdateVariantStore
newUpdateVariantStore pName_ =
  UpdateVariantStore'
    { description = Prelude.Nothing,
      name = pName_
    }

-- | A description for the store.
updateVariantStore_description :: Lens.Lens' UpdateVariantStore (Prelude.Maybe Prelude.Text)
updateVariantStore_description = Lens.lens (\UpdateVariantStore' {description} -> description) (\s@UpdateVariantStore' {} a -> s {description = a} :: UpdateVariantStore)

-- | A name for the store.
updateVariantStore_name :: Lens.Lens' UpdateVariantStore Prelude.Text
updateVariantStore_name = Lens.lens (\UpdateVariantStore' {name} -> name) (\s@UpdateVariantStore' {} a -> s {name = a} :: UpdateVariantStore)

instance Core.AWSRequest UpdateVariantStore where
  type
    AWSResponse UpdateVariantStore =
      UpdateVariantStoreResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateVariantStoreResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "creationTime")
            Prelude.<*> (x Data..:> "description")
            Prelude.<*> (x Data..:> "id")
            Prelude.<*> (x Data..:> "name")
            Prelude.<*> (x Data..:> "reference")
            Prelude.<*> (x Data..:> "status")
            Prelude.<*> (x Data..:> "updateTime")
      )

instance Prelude.Hashable UpdateVariantStore where
  hashWithSalt _salt UpdateVariantStore' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name

instance Prelude.NFData UpdateVariantStore where
  rnf UpdateVariantStore' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders UpdateVariantStore where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateVariantStore where
  toJSON UpdateVariantStore' {..} =
    Data.object
      ( Prelude.catMaybes
          [("description" Data..=) Prelude.<$> description]
      )

instance Data.ToPath UpdateVariantStore where
  toPath UpdateVariantStore' {..} =
    Prelude.mconcat ["/variantStore/", Data.toBS name]

instance Data.ToQuery UpdateVariantStore where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateVariantStoreResponse' smart constructor.
data UpdateVariantStoreResponse = UpdateVariantStoreResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | When the store was created.
    creationTime :: Data.ISO8601,
    -- | The store\'s description.
    description :: Prelude.Text,
    -- | The store\'s ID.
    id :: Prelude.Text,
    -- | The store\'s name.
    name :: Prelude.Text,
    -- | The store\'s genome reference.
    reference :: ReferenceItem,
    -- | The store\'s status.
    status :: StoreStatus,
    -- | When the store was updated.
    updateTime :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateVariantStoreResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateVariantStoreResponse_httpStatus' - The response's http status code.
--
-- 'creationTime', 'updateVariantStoreResponse_creationTime' - When the store was created.
--
-- 'description', 'updateVariantStoreResponse_description' - The store\'s description.
--
-- 'id', 'updateVariantStoreResponse_id' - The store\'s ID.
--
-- 'name', 'updateVariantStoreResponse_name' - The store\'s name.
--
-- 'reference', 'updateVariantStoreResponse_reference' - The store\'s genome reference.
--
-- 'status', 'updateVariantStoreResponse_status' - The store\'s status.
--
-- 'updateTime', 'updateVariantStoreResponse_updateTime' - When the store was updated.
newUpdateVariantStoreResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'description'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'reference'
  ReferenceItem ->
  -- | 'status'
  StoreStatus ->
  -- | 'updateTime'
  Prelude.UTCTime ->
  UpdateVariantStoreResponse
newUpdateVariantStoreResponse
  pHttpStatus_
  pCreationTime_
  pDescription_
  pId_
  pName_
  pReference_
  pStatus_
  pUpdateTime_ =
    UpdateVariantStoreResponse'
      { httpStatus =
          pHttpStatus_,
        creationTime = Data._Time Lens.# pCreationTime_,
        description = pDescription_,
        id = pId_,
        name = pName_,
        reference = pReference_,
        status = pStatus_,
        updateTime = Data._Time Lens.# pUpdateTime_
      }

-- | The response's http status code.
updateVariantStoreResponse_httpStatus :: Lens.Lens' UpdateVariantStoreResponse Prelude.Int
updateVariantStoreResponse_httpStatus = Lens.lens (\UpdateVariantStoreResponse' {httpStatus} -> httpStatus) (\s@UpdateVariantStoreResponse' {} a -> s {httpStatus = a} :: UpdateVariantStoreResponse)

-- | When the store was created.
updateVariantStoreResponse_creationTime :: Lens.Lens' UpdateVariantStoreResponse Prelude.UTCTime
updateVariantStoreResponse_creationTime = Lens.lens (\UpdateVariantStoreResponse' {creationTime} -> creationTime) (\s@UpdateVariantStoreResponse' {} a -> s {creationTime = a} :: UpdateVariantStoreResponse) Prelude.. Data._Time

-- | The store\'s description.
updateVariantStoreResponse_description :: Lens.Lens' UpdateVariantStoreResponse Prelude.Text
updateVariantStoreResponse_description = Lens.lens (\UpdateVariantStoreResponse' {description} -> description) (\s@UpdateVariantStoreResponse' {} a -> s {description = a} :: UpdateVariantStoreResponse)

-- | The store\'s ID.
updateVariantStoreResponse_id :: Lens.Lens' UpdateVariantStoreResponse Prelude.Text
updateVariantStoreResponse_id = Lens.lens (\UpdateVariantStoreResponse' {id} -> id) (\s@UpdateVariantStoreResponse' {} a -> s {id = a} :: UpdateVariantStoreResponse)

-- | The store\'s name.
updateVariantStoreResponse_name :: Lens.Lens' UpdateVariantStoreResponse Prelude.Text
updateVariantStoreResponse_name = Lens.lens (\UpdateVariantStoreResponse' {name} -> name) (\s@UpdateVariantStoreResponse' {} a -> s {name = a} :: UpdateVariantStoreResponse)

-- | The store\'s genome reference.
updateVariantStoreResponse_reference :: Lens.Lens' UpdateVariantStoreResponse ReferenceItem
updateVariantStoreResponse_reference = Lens.lens (\UpdateVariantStoreResponse' {reference} -> reference) (\s@UpdateVariantStoreResponse' {} a -> s {reference = a} :: UpdateVariantStoreResponse)

-- | The store\'s status.
updateVariantStoreResponse_status :: Lens.Lens' UpdateVariantStoreResponse StoreStatus
updateVariantStoreResponse_status = Lens.lens (\UpdateVariantStoreResponse' {status} -> status) (\s@UpdateVariantStoreResponse' {} a -> s {status = a} :: UpdateVariantStoreResponse)

-- | When the store was updated.
updateVariantStoreResponse_updateTime :: Lens.Lens' UpdateVariantStoreResponse Prelude.UTCTime
updateVariantStoreResponse_updateTime = Lens.lens (\UpdateVariantStoreResponse' {updateTime} -> updateTime) (\s@UpdateVariantStoreResponse' {} a -> s {updateTime = a} :: UpdateVariantStoreResponse) Prelude.. Data._Time

instance Prelude.NFData UpdateVariantStoreResponse where
  rnf UpdateVariantStoreResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf reference
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf updateTime
