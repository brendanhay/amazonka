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
-- Module      : Amazonka.Omics.CreateVariantStore
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a variant store.
module Amazonka.Omics.CreateVariantStore
  ( -- * Creating a Request
    CreateVariantStore (..),
    newCreateVariantStore,

    -- * Request Lenses
    createVariantStore_description,
    createVariantStore_name,
    createVariantStore_sseConfig,
    createVariantStore_tags,
    createVariantStore_reference,

    -- * Destructuring the Response
    CreateVariantStoreResponse (..),
    newCreateVariantStoreResponse,

    -- * Response Lenses
    createVariantStoreResponse_reference,
    createVariantStoreResponse_httpStatus,
    createVariantStoreResponse_id,
    createVariantStoreResponse_status,
    createVariantStoreResponse_name,
    createVariantStoreResponse_creationTime,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateVariantStore' smart constructor.
data CreateVariantStore = CreateVariantStore'
  { -- | A description for the store.
    description :: Prelude.Maybe Prelude.Text,
    -- | A name for the store.
    name :: Prelude.Maybe Prelude.Text,
    -- | Server-side encryption (SSE) settings for the store.
    sseConfig :: Prelude.Maybe SseConfig,
    -- | Tags for the store.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The genome reference for the store\'s variants.
    reference :: ReferenceItem
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVariantStore' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createVariantStore_description' - A description for the store.
--
-- 'name', 'createVariantStore_name' - A name for the store.
--
-- 'sseConfig', 'createVariantStore_sseConfig' - Server-side encryption (SSE) settings for the store.
--
-- 'tags', 'createVariantStore_tags' - Tags for the store.
--
-- 'reference', 'createVariantStore_reference' - The genome reference for the store\'s variants.
newCreateVariantStore ::
  -- | 'reference'
  ReferenceItem ->
  CreateVariantStore
newCreateVariantStore pReference_ =
  CreateVariantStore'
    { description = Prelude.Nothing,
      name = Prelude.Nothing,
      sseConfig = Prelude.Nothing,
      tags = Prelude.Nothing,
      reference = pReference_
    }

-- | A description for the store.
createVariantStore_description :: Lens.Lens' CreateVariantStore (Prelude.Maybe Prelude.Text)
createVariantStore_description = Lens.lens (\CreateVariantStore' {description} -> description) (\s@CreateVariantStore' {} a -> s {description = a} :: CreateVariantStore)

-- | A name for the store.
createVariantStore_name :: Lens.Lens' CreateVariantStore (Prelude.Maybe Prelude.Text)
createVariantStore_name = Lens.lens (\CreateVariantStore' {name} -> name) (\s@CreateVariantStore' {} a -> s {name = a} :: CreateVariantStore)

-- | Server-side encryption (SSE) settings for the store.
createVariantStore_sseConfig :: Lens.Lens' CreateVariantStore (Prelude.Maybe SseConfig)
createVariantStore_sseConfig = Lens.lens (\CreateVariantStore' {sseConfig} -> sseConfig) (\s@CreateVariantStore' {} a -> s {sseConfig = a} :: CreateVariantStore)

-- | Tags for the store.
createVariantStore_tags :: Lens.Lens' CreateVariantStore (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createVariantStore_tags = Lens.lens (\CreateVariantStore' {tags} -> tags) (\s@CreateVariantStore' {} a -> s {tags = a} :: CreateVariantStore) Prelude.. Lens.mapping Lens.coerced

-- | The genome reference for the store\'s variants.
createVariantStore_reference :: Lens.Lens' CreateVariantStore ReferenceItem
createVariantStore_reference = Lens.lens (\CreateVariantStore' {reference} -> reference) (\s@CreateVariantStore' {} a -> s {reference = a} :: CreateVariantStore)

instance Core.AWSRequest CreateVariantStore where
  type
    AWSResponse CreateVariantStore =
      CreateVariantStoreResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateVariantStoreResponse'
            Prelude.<$> (x Data..?> "reference")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "id")
            Prelude.<*> (x Data..:> "status")
            Prelude.<*> (x Data..:> "name")
            Prelude.<*> (x Data..:> "creationTime")
      )

instance Prelude.Hashable CreateVariantStore where
  hashWithSalt _salt CreateVariantStore' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` sseConfig
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` reference

instance Prelude.NFData CreateVariantStore where
  rnf CreateVariantStore' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf sseConfig
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf reference

instance Data.ToHeaders CreateVariantStore where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateVariantStore where
  toJSON CreateVariantStore' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("name" Data..=) Prelude.<$> name,
            ("sseConfig" Data..=) Prelude.<$> sseConfig,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("reference" Data..= reference)
          ]
      )

instance Data.ToPath CreateVariantStore where
  toPath = Prelude.const "/variantStore"

instance Data.ToQuery CreateVariantStore where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateVariantStoreResponse' smart constructor.
data CreateVariantStoreResponse = CreateVariantStoreResponse'
  { -- | The store\'s genome reference.
    reference :: Prelude.Maybe ReferenceItem,
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
-- Create a value of 'CreateVariantStoreResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reference', 'createVariantStoreResponse_reference' - The store\'s genome reference.
--
-- 'httpStatus', 'createVariantStoreResponse_httpStatus' - The response's http status code.
--
-- 'id', 'createVariantStoreResponse_id' - The store\'s ID.
--
-- 'status', 'createVariantStoreResponse_status' - The store\'s status.
--
-- 'name', 'createVariantStoreResponse_name' - The store\'s name.
--
-- 'creationTime', 'createVariantStoreResponse_creationTime' - When the store was created.
newCreateVariantStoreResponse ::
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
  CreateVariantStoreResponse
newCreateVariantStoreResponse
  pHttpStatus_
  pId_
  pStatus_
  pName_
  pCreationTime_ =
    CreateVariantStoreResponse'
      { reference =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        id = pId_,
        status = pStatus_,
        name = pName_,
        creationTime = Data._Time Lens.# pCreationTime_
      }

-- | The store\'s genome reference.
createVariantStoreResponse_reference :: Lens.Lens' CreateVariantStoreResponse (Prelude.Maybe ReferenceItem)
createVariantStoreResponse_reference = Lens.lens (\CreateVariantStoreResponse' {reference} -> reference) (\s@CreateVariantStoreResponse' {} a -> s {reference = a} :: CreateVariantStoreResponse)

-- | The response's http status code.
createVariantStoreResponse_httpStatus :: Lens.Lens' CreateVariantStoreResponse Prelude.Int
createVariantStoreResponse_httpStatus = Lens.lens (\CreateVariantStoreResponse' {httpStatus} -> httpStatus) (\s@CreateVariantStoreResponse' {} a -> s {httpStatus = a} :: CreateVariantStoreResponse)

-- | The store\'s ID.
createVariantStoreResponse_id :: Lens.Lens' CreateVariantStoreResponse Prelude.Text
createVariantStoreResponse_id = Lens.lens (\CreateVariantStoreResponse' {id} -> id) (\s@CreateVariantStoreResponse' {} a -> s {id = a} :: CreateVariantStoreResponse)

-- | The store\'s status.
createVariantStoreResponse_status :: Lens.Lens' CreateVariantStoreResponse StoreStatus
createVariantStoreResponse_status = Lens.lens (\CreateVariantStoreResponse' {status} -> status) (\s@CreateVariantStoreResponse' {} a -> s {status = a} :: CreateVariantStoreResponse)

-- | The store\'s name.
createVariantStoreResponse_name :: Lens.Lens' CreateVariantStoreResponse Prelude.Text
createVariantStoreResponse_name = Lens.lens (\CreateVariantStoreResponse' {name} -> name) (\s@CreateVariantStoreResponse' {} a -> s {name = a} :: CreateVariantStoreResponse)

-- | When the store was created.
createVariantStoreResponse_creationTime :: Lens.Lens' CreateVariantStoreResponse Prelude.UTCTime
createVariantStoreResponse_creationTime = Lens.lens (\CreateVariantStoreResponse' {creationTime} -> creationTime) (\s@CreateVariantStoreResponse' {} a -> s {creationTime = a} :: CreateVariantStoreResponse) Prelude.. Data._Time

instance Prelude.NFData CreateVariantStoreResponse where
  rnf CreateVariantStoreResponse' {..} =
    Prelude.rnf reference
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf creationTime
