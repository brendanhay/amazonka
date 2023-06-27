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
-- Module      : Amazonka.Omics.GetAnnotationStore
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about an annotation store.
module Amazonka.Omics.GetAnnotationStore
  ( -- * Creating a Request
    GetAnnotationStore (..),
    newGetAnnotationStore,

    -- * Request Lenses
    getAnnotationStore_name,

    -- * Destructuring the Response
    GetAnnotationStoreResponse (..),
    newGetAnnotationStoreResponse,

    -- * Response Lenses
    getAnnotationStoreResponse_storeFormat,
    getAnnotationStoreResponse_storeOptions,
    getAnnotationStoreResponse_httpStatus,
    getAnnotationStoreResponse_id,
    getAnnotationStoreResponse_reference,
    getAnnotationStoreResponse_status,
    getAnnotationStoreResponse_storeArn,
    getAnnotationStoreResponse_name,
    getAnnotationStoreResponse_description,
    getAnnotationStoreResponse_sseConfig,
    getAnnotationStoreResponse_creationTime,
    getAnnotationStoreResponse_updateTime,
    getAnnotationStoreResponse_tags,
    getAnnotationStoreResponse_statusMessage,
    getAnnotationStoreResponse_storeSizeBytes,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetAnnotationStore' smart constructor.
data GetAnnotationStore = GetAnnotationStore'
  { -- | The store\'s name.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAnnotationStore' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getAnnotationStore_name' - The store\'s name.
newGetAnnotationStore ::
  -- | 'name'
  Prelude.Text ->
  GetAnnotationStore
newGetAnnotationStore pName_ =
  GetAnnotationStore' {name = pName_}

-- | The store\'s name.
getAnnotationStore_name :: Lens.Lens' GetAnnotationStore Prelude.Text
getAnnotationStore_name = Lens.lens (\GetAnnotationStore' {name} -> name) (\s@GetAnnotationStore' {} a -> s {name = a} :: GetAnnotationStore)

instance Core.AWSRequest GetAnnotationStore where
  type
    AWSResponse GetAnnotationStore =
      GetAnnotationStoreResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAnnotationStoreResponse'
            Prelude.<$> (x Data..?> "storeFormat")
            Prelude.<*> (x Data..?> "storeOptions")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "id")
            Prelude.<*> (x Data..:> "reference")
            Prelude.<*> (x Data..:> "status")
            Prelude.<*> (x Data..:> "storeArn")
            Prelude.<*> (x Data..:> "name")
            Prelude.<*> (x Data..:> "description")
            Prelude.<*> (x Data..:> "sseConfig")
            Prelude.<*> (x Data..:> "creationTime")
            Prelude.<*> (x Data..:> "updateTime")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..:> "statusMessage")
            Prelude.<*> (x Data..:> "storeSizeBytes")
      )

instance Prelude.Hashable GetAnnotationStore where
  hashWithSalt _salt GetAnnotationStore' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData GetAnnotationStore where
  rnf GetAnnotationStore' {..} = Prelude.rnf name

instance Data.ToHeaders GetAnnotationStore where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetAnnotationStore where
  toPath GetAnnotationStore' {..} =
    Prelude.mconcat
      ["/annotationStore/", Data.toBS name]

instance Data.ToQuery GetAnnotationStore where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAnnotationStoreResponse' smart constructor.
data GetAnnotationStoreResponse = GetAnnotationStoreResponse'
  { -- | The store\'s annotation file format.
    storeFormat :: Prelude.Maybe StoreFormat,
    -- | The store\'s parsing options.
    storeOptions :: Prelude.Maybe StoreOptions,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The store\'s ID.
    id :: Prelude.Text,
    -- | The store\'s genome reference.
    reference :: ReferenceItem,
    -- | The store\'s status.
    status :: StoreStatus,
    -- | The store\'s ARN.
    storeArn :: Prelude.Text,
    -- | The store\'s name.
    name :: Prelude.Text,
    -- | The store\'s description.
    description :: Prelude.Text,
    -- | The store\'s server-side encryption (SSE) settings.
    sseConfig :: SseConfig,
    -- | When the store was created.
    creationTime :: Data.ISO8601,
    -- | When the store was updated.
    updateTime :: Data.ISO8601,
    -- | The store\'s tags.
    tags :: Prelude.HashMap Prelude.Text Prelude.Text,
    -- | A status message.
    statusMessage :: Prelude.Text,
    -- | The store\'s size in bytes.
    storeSizeBytes :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAnnotationStoreResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'storeFormat', 'getAnnotationStoreResponse_storeFormat' - The store\'s annotation file format.
--
-- 'storeOptions', 'getAnnotationStoreResponse_storeOptions' - The store\'s parsing options.
--
-- 'httpStatus', 'getAnnotationStoreResponse_httpStatus' - The response's http status code.
--
-- 'id', 'getAnnotationStoreResponse_id' - The store\'s ID.
--
-- 'reference', 'getAnnotationStoreResponse_reference' - The store\'s genome reference.
--
-- 'status', 'getAnnotationStoreResponse_status' - The store\'s status.
--
-- 'storeArn', 'getAnnotationStoreResponse_storeArn' - The store\'s ARN.
--
-- 'name', 'getAnnotationStoreResponse_name' - The store\'s name.
--
-- 'description', 'getAnnotationStoreResponse_description' - The store\'s description.
--
-- 'sseConfig', 'getAnnotationStoreResponse_sseConfig' - The store\'s server-side encryption (SSE) settings.
--
-- 'creationTime', 'getAnnotationStoreResponse_creationTime' - When the store was created.
--
-- 'updateTime', 'getAnnotationStoreResponse_updateTime' - When the store was updated.
--
-- 'tags', 'getAnnotationStoreResponse_tags' - The store\'s tags.
--
-- 'statusMessage', 'getAnnotationStoreResponse_statusMessage' - A status message.
--
-- 'storeSizeBytes', 'getAnnotationStoreResponse_storeSizeBytes' - The store\'s size in bytes.
newGetAnnotationStoreResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'id'
  Prelude.Text ->
  -- | 'reference'
  ReferenceItem ->
  -- | 'status'
  StoreStatus ->
  -- | 'storeArn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'description'
  Prelude.Text ->
  -- | 'sseConfig'
  SseConfig ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'updateTime'
  Prelude.UTCTime ->
  -- | 'statusMessage'
  Prelude.Text ->
  -- | 'storeSizeBytes'
  Prelude.Integer ->
  GetAnnotationStoreResponse
newGetAnnotationStoreResponse
  pHttpStatus_
  pId_
  pReference_
  pStatus_
  pStoreArn_
  pName_
  pDescription_
  pSseConfig_
  pCreationTime_
  pUpdateTime_
  pStatusMessage_
  pStoreSizeBytes_ =
    GetAnnotationStoreResponse'
      { storeFormat =
          Prelude.Nothing,
        storeOptions = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        id = pId_,
        reference = pReference_,
        status = pStatus_,
        storeArn = pStoreArn_,
        name = pName_,
        description = pDescription_,
        sseConfig = pSseConfig_,
        creationTime = Data._Time Lens.# pCreationTime_,
        updateTime = Data._Time Lens.# pUpdateTime_,
        tags = Prelude.mempty,
        statusMessage = pStatusMessage_,
        storeSizeBytes = pStoreSizeBytes_
      }

-- | The store\'s annotation file format.
getAnnotationStoreResponse_storeFormat :: Lens.Lens' GetAnnotationStoreResponse (Prelude.Maybe StoreFormat)
getAnnotationStoreResponse_storeFormat = Lens.lens (\GetAnnotationStoreResponse' {storeFormat} -> storeFormat) (\s@GetAnnotationStoreResponse' {} a -> s {storeFormat = a} :: GetAnnotationStoreResponse)

-- | The store\'s parsing options.
getAnnotationStoreResponse_storeOptions :: Lens.Lens' GetAnnotationStoreResponse (Prelude.Maybe StoreOptions)
getAnnotationStoreResponse_storeOptions = Lens.lens (\GetAnnotationStoreResponse' {storeOptions} -> storeOptions) (\s@GetAnnotationStoreResponse' {} a -> s {storeOptions = a} :: GetAnnotationStoreResponse)

-- | The response's http status code.
getAnnotationStoreResponse_httpStatus :: Lens.Lens' GetAnnotationStoreResponse Prelude.Int
getAnnotationStoreResponse_httpStatus = Lens.lens (\GetAnnotationStoreResponse' {httpStatus} -> httpStatus) (\s@GetAnnotationStoreResponse' {} a -> s {httpStatus = a} :: GetAnnotationStoreResponse)

-- | The store\'s ID.
getAnnotationStoreResponse_id :: Lens.Lens' GetAnnotationStoreResponse Prelude.Text
getAnnotationStoreResponse_id = Lens.lens (\GetAnnotationStoreResponse' {id} -> id) (\s@GetAnnotationStoreResponse' {} a -> s {id = a} :: GetAnnotationStoreResponse)

-- | The store\'s genome reference.
getAnnotationStoreResponse_reference :: Lens.Lens' GetAnnotationStoreResponse ReferenceItem
getAnnotationStoreResponse_reference = Lens.lens (\GetAnnotationStoreResponse' {reference} -> reference) (\s@GetAnnotationStoreResponse' {} a -> s {reference = a} :: GetAnnotationStoreResponse)

-- | The store\'s status.
getAnnotationStoreResponse_status :: Lens.Lens' GetAnnotationStoreResponse StoreStatus
getAnnotationStoreResponse_status = Lens.lens (\GetAnnotationStoreResponse' {status} -> status) (\s@GetAnnotationStoreResponse' {} a -> s {status = a} :: GetAnnotationStoreResponse)

-- | The store\'s ARN.
getAnnotationStoreResponse_storeArn :: Lens.Lens' GetAnnotationStoreResponse Prelude.Text
getAnnotationStoreResponse_storeArn = Lens.lens (\GetAnnotationStoreResponse' {storeArn} -> storeArn) (\s@GetAnnotationStoreResponse' {} a -> s {storeArn = a} :: GetAnnotationStoreResponse)

-- | The store\'s name.
getAnnotationStoreResponse_name :: Lens.Lens' GetAnnotationStoreResponse Prelude.Text
getAnnotationStoreResponse_name = Lens.lens (\GetAnnotationStoreResponse' {name} -> name) (\s@GetAnnotationStoreResponse' {} a -> s {name = a} :: GetAnnotationStoreResponse)

-- | The store\'s description.
getAnnotationStoreResponse_description :: Lens.Lens' GetAnnotationStoreResponse Prelude.Text
getAnnotationStoreResponse_description = Lens.lens (\GetAnnotationStoreResponse' {description} -> description) (\s@GetAnnotationStoreResponse' {} a -> s {description = a} :: GetAnnotationStoreResponse)

-- | The store\'s server-side encryption (SSE) settings.
getAnnotationStoreResponse_sseConfig :: Lens.Lens' GetAnnotationStoreResponse SseConfig
getAnnotationStoreResponse_sseConfig = Lens.lens (\GetAnnotationStoreResponse' {sseConfig} -> sseConfig) (\s@GetAnnotationStoreResponse' {} a -> s {sseConfig = a} :: GetAnnotationStoreResponse)

-- | When the store was created.
getAnnotationStoreResponse_creationTime :: Lens.Lens' GetAnnotationStoreResponse Prelude.UTCTime
getAnnotationStoreResponse_creationTime = Lens.lens (\GetAnnotationStoreResponse' {creationTime} -> creationTime) (\s@GetAnnotationStoreResponse' {} a -> s {creationTime = a} :: GetAnnotationStoreResponse) Prelude.. Data._Time

-- | When the store was updated.
getAnnotationStoreResponse_updateTime :: Lens.Lens' GetAnnotationStoreResponse Prelude.UTCTime
getAnnotationStoreResponse_updateTime = Lens.lens (\GetAnnotationStoreResponse' {updateTime} -> updateTime) (\s@GetAnnotationStoreResponse' {} a -> s {updateTime = a} :: GetAnnotationStoreResponse) Prelude.. Data._Time

-- | The store\'s tags.
getAnnotationStoreResponse_tags :: Lens.Lens' GetAnnotationStoreResponse (Prelude.HashMap Prelude.Text Prelude.Text)
getAnnotationStoreResponse_tags = Lens.lens (\GetAnnotationStoreResponse' {tags} -> tags) (\s@GetAnnotationStoreResponse' {} a -> s {tags = a} :: GetAnnotationStoreResponse) Prelude.. Lens.coerced

-- | A status message.
getAnnotationStoreResponse_statusMessage :: Lens.Lens' GetAnnotationStoreResponse Prelude.Text
getAnnotationStoreResponse_statusMessage = Lens.lens (\GetAnnotationStoreResponse' {statusMessage} -> statusMessage) (\s@GetAnnotationStoreResponse' {} a -> s {statusMessage = a} :: GetAnnotationStoreResponse)

-- | The store\'s size in bytes.
getAnnotationStoreResponse_storeSizeBytes :: Lens.Lens' GetAnnotationStoreResponse Prelude.Integer
getAnnotationStoreResponse_storeSizeBytes = Lens.lens (\GetAnnotationStoreResponse' {storeSizeBytes} -> storeSizeBytes) (\s@GetAnnotationStoreResponse' {} a -> s {storeSizeBytes = a} :: GetAnnotationStoreResponse)

instance Prelude.NFData GetAnnotationStoreResponse where
  rnf GetAnnotationStoreResponse' {..} =
    Prelude.rnf storeFormat
      `Prelude.seq` Prelude.rnf storeOptions
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf reference
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf storeArn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf sseConfig
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf updateTime
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf storeSizeBytes
