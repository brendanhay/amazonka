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
-- Module      : Amazonka.Omics.CreateAnnotationStore
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an annotation store.
module Amazonka.Omics.CreateAnnotationStore
  ( -- * Creating a Request
    CreateAnnotationStore (..),
    newCreateAnnotationStore,

    -- * Request Lenses
    createAnnotationStore_description,
    createAnnotationStore_name,
    createAnnotationStore_reference,
    createAnnotationStore_sseConfig,
    createAnnotationStore_storeOptions,
    createAnnotationStore_tags,
    createAnnotationStore_storeFormat,

    -- * Destructuring the Response
    CreateAnnotationStoreResponse (..),
    newCreateAnnotationStoreResponse,

    -- * Response Lenses
    createAnnotationStoreResponse_reference,
    createAnnotationStoreResponse_storeFormat,
    createAnnotationStoreResponse_storeOptions,
    createAnnotationStoreResponse_httpStatus,
    createAnnotationStoreResponse_id,
    createAnnotationStoreResponse_status,
    createAnnotationStoreResponse_name,
    createAnnotationStoreResponse_creationTime,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateAnnotationStore' smart constructor.
data CreateAnnotationStore = CreateAnnotationStore'
  { -- | A description for the store.
    description :: Prelude.Maybe Prelude.Text,
    -- | A name for the store.
    name :: Prelude.Maybe Prelude.Text,
    -- | The genome reference for the store\'s annotations.
    reference :: Prelude.Maybe ReferenceItem,
    -- | Server-side encryption (SSE) settings for the store.
    sseConfig :: Prelude.Maybe SseConfig,
    -- | File parsing options for the annotation store.
    storeOptions :: Prelude.Maybe StoreOptions,
    -- | Tags for the store.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The annotation file format of the store.
    storeFormat :: StoreFormat
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAnnotationStore' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createAnnotationStore_description' - A description for the store.
--
-- 'name', 'createAnnotationStore_name' - A name for the store.
--
-- 'reference', 'createAnnotationStore_reference' - The genome reference for the store\'s annotations.
--
-- 'sseConfig', 'createAnnotationStore_sseConfig' - Server-side encryption (SSE) settings for the store.
--
-- 'storeOptions', 'createAnnotationStore_storeOptions' - File parsing options for the annotation store.
--
-- 'tags', 'createAnnotationStore_tags' - Tags for the store.
--
-- 'storeFormat', 'createAnnotationStore_storeFormat' - The annotation file format of the store.
newCreateAnnotationStore ::
  -- | 'storeFormat'
  StoreFormat ->
  CreateAnnotationStore
newCreateAnnotationStore pStoreFormat_ =
  CreateAnnotationStore'
    { description =
        Prelude.Nothing,
      name = Prelude.Nothing,
      reference = Prelude.Nothing,
      sseConfig = Prelude.Nothing,
      storeOptions = Prelude.Nothing,
      tags = Prelude.Nothing,
      storeFormat = pStoreFormat_
    }

-- | A description for the store.
createAnnotationStore_description :: Lens.Lens' CreateAnnotationStore (Prelude.Maybe Prelude.Text)
createAnnotationStore_description = Lens.lens (\CreateAnnotationStore' {description} -> description) (\s@CreateAnnotationStore' {} a -> s {description = a} :: CreateAnnotationStore)

-- | A name for the store.
createAnnotationStore_name :: Lens.Lens' CreateAnnotationStore (Prelude.Maybe Prelude.Text)
createAnnotationStore_name = Lens.lens (\CreateAnnotationStore' {name} -> name) (\s@CreateAnnotationStore' {} a -> s {name = a} :: CreateAnnotationStore)

-- | The genome reference for the store\'s annotations.
createAnnotationStore_reference :: Lens.Lens' CreateAnnotationStore (Prelude.Maybe ReferenceItem)
createAnnotationStore_reference = Lens.lens (\CreateAnnotationStore' {reference} -> reference) (\s@CreateAnnotationStore' {} a -> s {reference = a} :: CreateAnnotationStore)

-- | Server-side encryption (SSE) settings for the store.
createAnnotationStore_sseConfig :: Lens.Lens' CreateAnnotationStore (Prelude.Maybe SseConfig)
createAnnotationStore_sseConfig = Lens.lens (\CreateAnnotationStore' {sseConfig} -> sseConfig) (\s@CreateAnnotationStore' {} a -> s {sseConfig = a} :: CreateAnnotationStore)

-- | File parsing options for the annotation store.
createAnnotationStore_storeOptions :: Lens.Lens' CreateAnnotationStore (Prelude.Maybe StoreOptions)
createAnnotationStore_storeOptions = Lens.lens (\CreateAnnotationStore' {storeOptions} -> storeOptions) (\s@CreateAnnotationStore' {} a -> s {storeOptions = a} :: CreateAnnotationStore)

-- | Tags for the store.
createAnnotationStore_tags :: Lens.Lens' CreateAnnotationStore (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createAnnotationStore_tags = Lens.lens (\CreateAnnotationStore' {tags} -> tags) (\s@CreateAnnotationStore' {} a -> s {tags = a} :: CreateAnnotationStore) Prelude.. Lens.mapping Lens.coerced

-- | The annotation file format of the store.
createAnnotationStore_storeFormat :: Lens.Lens' CreateAnnotationStore StoreFormat
createAnnotationStore_storeFormat = Lens.lens (\CreateAnnotationStore' {storeFormat} -> storeFormat) (\s@CreateAnnotationStore' {} a -> s {storeFormat = a} :: CreateAnnotationStore)

instance Core.AWSRequest CreateAnnotationStore where
  type
    AWSResponse CreateAnnotationStore =
      CreateAnnotationStoreResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAnnotationStoreResponse'
            Prelude.<$> (x Data..?> "reference")
            Prelude.<*> (x Data..?> "storeFormat")
            Prelude.<*> (x Data..?> "storeOptions")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "id")
            Prelude.<*> (x Data..:> "status")
            Prelude.<*> (x Data..:> "name")
            Prelude.<*> (x Data..:> "creationTime")
      )

instance Prelude.Hashable CreateAnnotationStore where
  hashWithSalt _salt CreateAnnotationStore' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` reference
      `Prelude.hashWithSalt` sseConfig
      `Prelude.hashWithSalt` storeOptions
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` storeFormat

instance Prelude.NFData CreateAnnotationStore where
  rnf CreateAnnotationStore' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf reference
      `Prelude.seq` Prelude.rnf sseConfig
      `Prelude.seq` Prelude.rnf storeOptions
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf storeFormat

instance Data.ToHeaders CreateAnnotationStore where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateAnnotationStore where
  toJSON CreateAnnotationStore' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("name" Data..=) Prelude.<$> name,
            ("reference" Data..=) Prelude.<$> reference,
            ("sseConfig" Data..=) Prelude.<$> sseConfig,
            ("storeOptions" Data..=) Prelude.<$> storeOptions,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("storeFormat" Data..= storeFormat)
          ]
      )

instance Data.ToPath CreateAnnotationStore where
  toPath = Prelude.const "/annotationStore"

instance Data.ToQuery CreateAnnotationStore where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAnnotationStoreResponse' smart constructor.
data CreateAnnotationStoreResponse = CreateAnnotationStoreResponse'
  { -- | The store\'s genome reference. Required for all stores except TSV format
    -- with generic annotations.
    reference :: Prelude.Maybe ReferenceItem,
    -- | The annotation file format of the store.
    storeFormat :: Prelude.Maybe StoreFormat,
    -- | The store\'s file parsing options.
    storeOptions :: Prelude.Maybe StoreOptions,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The store\'s ID.
    id :: Prelude.Text,
    -- | The store\'s status.
    status :: StoreStatus,
    -- | The store\'s name.
    name :: Prelude.Text,
    -- | When the store was created.
    creationTime :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAnnotationStoreResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reference', 'createAnnotationStoreResponse_reference' - The store\'s genome reference. Required for all stores except TSV format
-- with generic annotations.
--
-- 'storeFormat', 'createAnnotationStoreResponse_storeFormat' - The annotation file format of the store.
--
-- 'storeOptions', 'createAnnotationStoreResponse_storeOptions' - The store\'s file parsing options.
--
-- 'httpStatus', 'createAnnotationStoreResponse_httpStatus' - The response's http status code.
--
-- 'id', 'createAnnotationStoreResponse_id' - The store\'s ID.
--
-- 'status', 'createAnnotationStoreResponse_status' - The store\'s status.
--
-- 'name', 'createAnnotationStoreResponse_name' - The store\'s name.
--
-- 'creationTime', 'createAnnotationStoreResponse_creationTime' - When the store was created.
newCreateAnnotationStoreResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'id'
  Prelude.Text ->
  -- | 'status'
  StoreStatus ->
  -- | 'name'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  CreateAnnotationStoreResponse
newCreateAnnotationStoreResponse
  pHttpStatus_
  pId_
  pStatus_
  pName_
  pCreationTime_ =
    CreateAnnotationStoreResponse'
      { reference =
          Prelude.Nothing,
        storeFormat = Prelude.Nothing,
        storeOptions = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        id = pId_,
        status = pStatus_,
        name = pName_,
        creationTime =
          Data._Time Lens.# pCreationTime_
      }

-- | The store\'s genome reference. Required for all stores except TSV format
-- with generic annotations.
createAnnotationStoreResponse_reference :: Lens.Lens' CreateAnnotationStoreResponse (Prelude.Maybe ReferenceItem)
createAnnotationStoreResponse_reference = Lens.lens (\CreateAnnotationStoreResponse' {reference} -> reference) (\s@CreateAnnotationStoreResponse' {} a -> s {reference = a} :: CreateAnnotationStoreResponse)

-- | The annotation file format of the store.
createAnnotationStoreResponse_storeFormat :: Lens.Lens' CreateAnnotationStoreResponse (Prelude.Maybe StoreFormat)
createAnnotationStoreResponse_storeFormat = Lens.lens (\CreateAnnotationStoreResponse' {storeFormat} -> storeFormat) (\s@CreateAnnotationStoreResponse' {} a -> s {storeFormat = a} :: CreateAnnotationStoreResponse)

-- | The store\'s file parsing options.
createAnnotationStoreResponse_storeOptions :: Lens.Lens' CreateAnnotationStoreResponse (Prelude.Maybe StoreOptions)
createAnnotationStoreResponse_storeOptions = Lens.lens (\CreateAnnotationStoreResponse' {storeOptions} -> storeOptions) (\s@CreateAnnotationStoreResponse' {} a -> s {storeOptions = a} :: CreateAnnotationStoreResponse)

-- | The response's http status code.
createAnnotationStoreResponse_httpStatus :: Lens.Lens' CreateAnnotationStoreResponse Prelude.Int
createAnnotationStoreResponse_httpStatus = Lens.lens (\CreateAnnotationStoreResponse' {httpStatus} -> httpStatus) (\s@CreateAnnotationStoreResponse' {} a -> s {httpStatus = a} :: CreateAnnotationStoreResponse)

-- | The store\'s ID.
createAnnotationStoreResponse_id :: Lens.Lens' CreateAnnotationStoreResponse Prelude.Text
createAnnotationStoreResponse_id = Lens.lens (\CreateAnnotationStoreResponse' {id} -> id) (\s@CreateAnnotationStoreResponse' {} a -> s {id = a} :: CreateAnnotationStoreResponse)

-- | The store\'s status.
createAnnotationStoreResponse_status :: Lens.Lens' CreateAnnotationStoreResponse StoreStatus
createAnnotationStoreResponse_status = Lens.lens (\CreateAnnotationStoreResponse' {status} -> status) (\s@CreateAnnotationStoreResponse' {} a -> s {status = a} :: CreateAnnotationStoreResponse)

-- | The store\'s name.
createAnnotationStoreResponse_name :: Lens.Lens' CreateAnnotationStoreResponse Prelude.Text
createAnnotationStoreResponse_name = Lens.lens (\CreateAnnotationStoreResponse' {name} -> name) (\s@CreateAnnotationStoreResponse' {} a -> s {name = a} :: CreateAnnotationStoreResponse)

-- | When the store was created.
createAnnotationStoreResponse_creationTime :: Lens.Lens' CreateAnnotationStoreResponse Prelude.UTCTime
createAnnotationStoreResponse_creationTime = Lens.lens (\CreateAnnotationStoreResponse' {creationTime} -> creationTime) (\s@CreateAnnotationStoreResponse' {} a -> s {creationTime = a} :: CreateAnnotationStoreResponse) Prelude.. Data._Time

instance Prelude.NFData CreateAnnotationStoreResponse where
  rnf CreateAnnotationStoreResponse' {..} =
    Prelude.rnf reference
      `Prelude.seq` Prelude.rnf storeFormat
      `Prelude.seq` Prelude.rnf storeOptions
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf creationTime
