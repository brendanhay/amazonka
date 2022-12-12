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
-- Module      : Amazonka.Omics.UpdateAnnotationStore
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an annotation store.
module Amazonka.Omics.UpdateAnnotationStore
  ( -- * Creating a Request
    UpdateAnnotationStore (..),
    newUpdateAnnotationStore,

    -- * Request Lenses
    updateAnnotationStore_description,
    updateAnnotationStore_name,

    -- * Destructuring the Response
    UpdateAnnotationStoreResponse (..),
    newUpdateAnnotationStoreResponse,

    -- * Response Lenses
    updateAnnotationStoreResponse_storeFormat,
    updateAnnotationStoreResponse_storeOptions,
    updateAnnotationStoreResponse_httpStatus,
    updateAnnotationStoreResponse_creationTime,
    updateAnnotationStoreResponse_description,
    updateAnnotationStoreResponse_id,
    updateAnnotationStoreResponse_name,
    updateAnnotationStoreResponse_reference,
    updateAnnotationStoreResponse_status,
    updateAnnotationStoreResponse_updateTime,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateAnnotationStore' smart constructor.
data UpdateAnnotationStore = UpdateAnnotationStore'
  { -- | A description for the store.
    description :: Prelude.Maybe Prelude.Text,
    -- | A name for the store.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAnnotationStore' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateAnnotationStore_description' - A description for the store.
--
-- 'name', 'updateAnnotationStore_name' - A name for the store.
newUpdateAnnotationStore ::
  -- | 'name'
  Prelude.Text ->
  UpdateAnnotationStore
newUpdateAnnotationStore pName_ =
  UpdateAnnotationStore'
    { description =
        Prelude.Nothing,
      name = pName_
    }

-- | A description for the store.
updateAnnotationStore_description :: Lens.Lens' UpdateAnnotationStore (Prelude.Maybe Prelude.Text)
updateAnnotationStore_description = Lens.lens (\UpdateAnnotationStore' {description} -> description) (\s@UpdateAnnotationStore' {} a -> s {description = a} :: UpdateAnnotationStore)

-- | A name for the store.
updateAnnotationStore_name :: Lens.Lens' UpdateAnnotationStore Prelude.Text
updateAnnotationStore_name = Lens.lens (\UpdateAnnotationStore' {name} -> name) (\s@UpdateAnnotationStore' {} a -> s {name = a} :: UpdateAnnotationStore)

instance Core.AWSRequest UpdateAnnotationStore where
  type
    AWSResponse UpdateAnnotationStore =
      UpdateAnnotationStoreResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAnnotationStoreResponse'
            Prelude.<$> (x Data..?> "storeFormat")
            Prelude.<*> (x Data..?> "storeOptions")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "creationTime")
            Prelude.<*> (x Data..:> "description")
            Prelude.<*> (x Data..:> "id")
            Prelude.<*> (x Data..:> "name")
            Prelude.<*> (x Data..:> "reference")
            Prelude.<*> (x Data..:> "status")
            Prelude.<*> (x Data..:> "updateTime")
      )

instance Prelude.Hashable UpdateAnnotationStore where
  hashWithSalt _salt UpdateAnnotationStore' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name

instance Prelude.NFData UpdateAnnotationStore where
  rnf UpdateAnnotationStore' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders UpdateAnnotationStore where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateAnnotationStore where
  toJSON UpdateAnnotationStore' {..} =
    Data.object
      ( Prelude.catMaybes
          [("description" Data..=) Prelude.<$> description]
      )

instance Data.ToPath UpdateAnnotationStore where
  toPath UpdateAnnotationStore' {..} =
    Prelude.mconcat
      ["/annotationStore/", Data.toBS name]

instance Data.ToQuery UpdateAnnotationStore where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAnnotationStoreResponse' smart constructor.
data UpdateAnnotationStoreResponse = UpdateAnnotationStoreResponse'
  { -- | The annotation file format of the store.
    storeFormat :: Prelude.Maybe StoreFormat,
    -- | Parsing options for the store.
    storeOptions :: Prelude.Maybe StoreOptions,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | When the store was created.
    creationTime :: Data.POSIX,
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
    updateTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAnnotationStoreResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'storeFormat', 'updateAnnotationStoreResponse_storeFormat' - The annotation file format of the store.
--
-- 'storeOptions', 'updateAnnotationStoreResponse_storeOptions' - Parsing options for the store.
--
-- 'httpStatus', 'updateAnnotationStoreResponse_httpStatus' - The response's http status code.
--
-- 'creationTime', 'updateAnnotationStoreResponse_creationTime' - When the store was created.
--
-- 'description', 'updateAnnotationStoreResponse_description' - The store\'s description.
--
-- 'id', 'updateAnnotationStoreResponse_id' - The store\'s ID.
--
-- 'name', 'updateAnnotationStoreResponse_name' - The store\'s name.
--
-- 'reference', 'updateAnnotationStoreResponse_reference' - The store\'s genome reference.
--
-- 'status', 'updateAnnotationStoreResponse_status' - The store\'s status.
--
-- 'updateTime', 'updateAnnotationStoreResponse_updateTime' - When the store was updated.
newUpdateAnnotationStoreResponse ::
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
  UpdateAnnotationStoreResponse
newUpdateAnnotationStoreResponse
  pHttpStatus_
  pCreationTime_
  pDescription_
  pId_
  pName_
  pReference_
  pStatus_
  pUpdateTime_ =
    UpdateAnnotationStoreResponse'
      { storeFormat =
          Prelude.Nothing,
        storeOptions = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        creationTime =
          Data._Time Lens.# pCreationTime_,
        description = pDescription_,
        id = pId_,
        name = pName_,
        reference = pReference_,
        status = pStatus_,
        updateTime = Data._Time Lens.# pUpdateTime_
      }

-- | The annotation file format of the store.
updateAnnotationStoreResponse_storeFormat :: Lens.Lens' UpdateAnnotationStoreResponse (Prelude.Maybe StoreFormat)
updateAnnotationStoreResponse_storeFormat = Lens.lens (\UpdateAnnotationStoreResponse' {storeFormat} -> storeFormat) (\s@UpdateAnnotationStoreResponse' {} a -> s {storeFormat = a} :: UpdateAnnotationStoreResponse)

-- | Parsing options for the store.
updateAnnotationStoreResponse_storeOptions :: Lens.Lens' UpdateAnnotationStoreResponse (Prelude.Maybe StoreOptions)
updateAnnotationStoreResponse_storeOptions = Lens.lens (\UpdateAnnotationStoreResponse' {storeOptions} -> storeOptions) (\s@UpdateAnnotationStoreResponse' {} a -> s {storeOptions = a} :: UpdateAnnotationStoreResponse)

-- | The response's http status code.
updateAnnotationStoreResponse_httpStatus :: Lens.Lens' UpdateAnnotationStoreResponse Prelude.Int
updateAnnotationStoreResponse_httpStatus = Lens.lens (\UpdateAnnotationStoreResponse' {httpStatus} -> httpStatus) (\s@UpdateAnnotationStoreResponse' {} a -> s {httpStatus = a} :: UpdateAnnotationStoreResponse)

-- | When the store was created.
updateAnnotationStoreResponse_creationTime :: Lens.Lens' UpdateAnnotationStoreResponse Prelude.UTCTime
updateAnnotationStoreResponse_creationTime = Lens.lens (\UpdateAnnotationStoreResponse' {creationTime} -> creationTime) (\s@UpdateAnnotationStoreResponse' {} a -> s {creationTime = a} :: UpdateAnnotationStoreResponse) Prelude.. Data._Time

-- | The store\'s description.
updateAnnotationStoreResponse_description :: Lens.Lens' UpdateAnnotationStoreResponse Prelude.Text
updateAnnotationStoreResponse_description = Lens.lens (\UpdateAnnotationStoreResponse' {description} -> description) (\s@UpdateAnnotationStoreResponse' {} a -> s {description = a} :: UpdateAnnotationStoreResponse)

-- | The store\'s ID.
updateAnnotationStoreResponse_id :: Lens.Lens' UpdateAnnotationStoreResponse Prelude.Text
updateAnnotationStoreResponse_id = Lens.lens (\UpdateAnnotationStoreResponse' {id} -> id) (\s@UpdateAnnotationStoreResponse' {} a -> s {id = a} :: UpdateAnnotationStoreResponse)

-- | The store\'s name.
updateAnnotationStoreResponse_name :: Lens.Lens' UpdateAnnotationStoreResponse Prelude.Text
updateAnnotationStoreResponse_name = Lens.lens (\UpdateAnnotationStoreResponse' {name} -> name) (\s@UpdateAnnotationStoreResponse' {} a -> s {name = a} :: UpdateAnnotationStoreResponse)

-- | The store\'s genome reference.
updateAnnotationStoreResponse_reference :: Lens.Lens' UpdateAnnotationStoreResponse ReferenceItem
updateAnnotationStoreResponse_reference = Lens.lens (\UpdateAnnotationStoreResponse' {reference} -> reference) (\s@UpdateAnnotationStoreResponse' {} a -> s {reference = a} :: UpdateAnnotationStoreResponse)

-- | The store\'s status.
updateAnnotationStoreResponse_status :: Lens.Lens' UpdateAnnotationStoreResponse StoreStatus
updateAnnotationStoreResponse_status = Lens.lens (\UpdateAnnotationStoreResponse' {status} -> status) (\s@UpdateAnnotationStoreResponse' {} a -> s {status = a} :: UpdateAnnotationStoreResponse)

-- | When the store was updated.
updateAnnotationStoreResponse_updateTime :: Lens.Lens' UpdateAnnotationStoreResponse Prelude.UTCTime
updateAnnotationStoreResponse_updateTime = Lens.lens (\UpdateAnnotationStoreResponse' {updateTime} -> updateTime) (\s@UpdateAnnotationStoreResponse' {} a -> s {updateTime = a} :: UpdateAnnotationStoreResponse) Prelude.. Data._Time

instance Prelude.NFData UpdateAnnotationStoreResponse where
  rnf UpdateAnnotationStoreResponse' {..} =
    Prelude.rnf storeFormat
      `Prelude.seq` Prelude.rnf storeOptions
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf reference
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf updateTime
