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
-- Module      : Amazonka.Omics.GetVariantStore
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a variant store.
module Amazonka.Omics.GetVariantStore
  ( -- * Creating a Request
    GetVariantStore (..),
    newGetVariantStore,

    -- * Request Lenses
    getVariantStore_name,

    -- * Destructuring the Response
    GetVariantStoreResponse (..),
    newGetVariantStoreResponse,

    -- * Response Lenses
    getVariantStoreResponse_httpStatus,
    getVariantStoreResponse_id,
    getVariantStoreResponse_reference,
    getVariantStoreResponse_status,
    getVariantStoreResponse_storeArn,
    getVariantStoreResponse_name,
    getVariantStoreResponse_description,
    getVariantStoreResponse_sseConfig,
    getVariantStoreResponse_creationTime,
    getVariantStoreResponse_updateTime,
    getVariantStoreResponse_tags,
    getVariantStoreResponse_statusMessage,
    getVariantStoreResponse_storeSizeBytes,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetVariantStore' smart constructor.
data GetVariantStore = GetVariantStore'
  { -- | The store\'s name.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetVariantStore' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getVariantStore_name' - The store\'s name.
newGetVariantStore ::
  -- | 'name'
  Prelude.Text ->
  GetVariantStore
newGetVariantStore pName_ =
  GetVariantStore' {name = pName_}

-- | The store\'s name.
getVariantStore_name :: Lens.Lens' GetVariantStore Prelude.Text
getVariantStore_name = Lens.lens (\GetVariantStore' {name} -> name) (\s@GetVariantStore' {} a -> s {name = a} :: GetVariantStore)

instance Core.AWSRequest GetVariantStore where
  type
    AWSResponse GetVariantStore =
      GetVariantStoreResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetVariantStoreResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
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

instance Prelude.Hashable GetVariantStore where
  hashWithSalt _salt GetVariantStore' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData GetVariantStore where
  rnf GetVariantStore' {..} = Prelude.rnf name

instance Data.ToHeaders GetVariantStore where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetVariantStore where
  toPath GetVariantStore' {..} =
    Prelude.mconcat ["/variantStore/", Data.toBS name]

instance Data.ToQuery GetVariantStore where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetVariantStoreResponse' smart constructor.
data GetVariantStoreResponse = GetVariantStoreResponse'
  { -- | The response's http status code.
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
    -- | The store\'s status message.
    statusMessage :: Prelude.Text,
    -- | The store\'s size in bytes.
    storeSizeBytes :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetVariantStoreResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getVariantStoreResponse_httpStatus' - The response's http status code.
--
-- 'id', 'getVariantStoreResponse_id' - The store\'s ID.
--
-- 'reference', 'getVariantStoreResponse_reference' - The store\'s genome reference.
--
-- 'status', 'getVariantStoreResponse_status' - The store\'s status.
--
-- 'storeArn', 'getVariantStoreResponse_storeArn' - The store\'s ARN.
--
-- 'name', 'getVariantStoreResponse_name' - The store\'s name.
--
-- 'description', 'getVariantStoreResponse_description' - The store\'s description.
--
-- 'sseConfig', 'getVariantStoreResponse_sseConfig' - The store\'s server-side encryption (SSE) settings.
--
-- 'creationTime', 'getVariantStoreResponse_creationTime' - When the store was created.
--
-- 'updateTime', 'getVariantStoreResponse_updateTime' - When the store was updated.
--
-- 'tags', 'getVariantStoreResponse_tags' - The store\'s tags.
--
-- 'statusMessage', 'getVariantStoreResponse_statusMessage' - The store\'s status message.
--
-- 'storeSizeBytes', 'getVariantStoreResponse_storeSizeBytes' - The store\'s size in bytes.
newGetVariantStoreResponse ::
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
  GetVariantStoreResponse
newGetVariantStoreResponse
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
    GetVariantStoreResponse'
      { httpStatus = pHttpStatus_,
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

-- | The response's http status code.
getVariantStoreResponse_httpStatus :: Lens.Lens' GetVariantStoreResponse Prelude.Int
getVariantStoreResponse_httpStatus = Lens.lens (\GetVariantStoreResponse' {httpStatus} -> httpStatus) (\s@GetVariantStoreResponse' {} a -> s {httpStatus = a} :: GetVariantStoreResponse)

-- | The store\'s ID.
getVariantStoreResponse_id :: Lens.Lens' GetVariantStoreResponse Prelude.Text
getVariantStoreResponse_id = Lens.lens (\GetVariantStoreResponse' {id} -> id) (\s@GetVariantStoreResponse' {} a -> s {id = a} :: GetVariantStoreResponse)

-- | The store\'s genome reference.
getVariantStoreResponse_reference :: Lens.Lens' GetVariantStoreResponse ReferenceItem
getVariantStoreResponse_reference = Lens.lens (\GetVariantStoreResponse' {reference} -> reference) (\s@GetVariantStoreResponse' {} a -> s {reference = a} :: GetVariantStoreResponse)

-- | The store\'s status.
getVariantStoreResponse_status :: Lens.Lens' GetVariantStoreResponse StoreStatus
getVariantStoreResponse_status = Lens.lens (\GetVariantStoreResponse' {status} -> status) (\s@GetVariantStoreResponse' {} a -> s {status = a} :: GetVariantStoreResponse)

-- | The store\'s ARN.
getVariantStoreResponse_storeArn :: Lens.Lens' GetVariantStoreResponse Prelude.Text
getVariantStoreResponse_storeArn = Lens.lens (\GetVariantStoreResponse' {storeArn} -> storeArn) (\s@GetVariantStoreResponse' {} a -> s {storeArn = a} :: GetVariantStoreResponse)

-- | The store\'s name.
getVariantStoreResponse_name :: Lens.Lens' GetVariantStoreResponse Prelude.Text
getVariantStoreResponse_name = Lens.lens (\GetVariantStoreResponse' {name} -> name) (\s@GetVariantStoreResponse' {} a -> s {name = a} :: GetVariantStoreResponse)

-- | The store\'s description.
getVariantStoreResponse_description :: Lens.Lens' GetVariantStoreResponse Prelude.Text
getVariantStoreResponse_description = Lens.lens (\GetVariantStoreResponse' {description} -> description) (\s@GetVariantStoreResponse' {} a -> s {description = a} :: GetVariantStoreResponse)

-- | The store\'s server-side encryption (SSE) settings.
getVariantStoreResponse_sseConfig :: Lens.Lens' GetVariantStoreResponse SseConfig
getVariantStoreResponse_sseConfig = Lens.lens (\GetVariantStoreResponse' {sseConfig} -> sseConfig) (\s@GetVariantStoreResponse' {} a -> s {sseConfig = a} :: GetVariantStoreResponse)

-- | When the store was created.
getVariantStoreResponse_creationTime :: Lens.Lens' GetVariantStoreResponse Prelude.UTCTime
getVariantStoreResponse_creationTime = Lens.lens (\GetVariantStoreResponse' {creationTime} -> creationTime) (\s@GetVariantStoreResponse' {} a -> s {creationTime = a} :: GetVariantStoreResponse) Prelude.. Data._Time

-- | When the store was updated.
getVariantStoreResponse_updateTime :: Lens.Lens' GetVariantStoreResponse Prelude.UTCTime
getVariantStoreResponse_updateTime = Lens.lens (\GetVariantStoreResponse' {updateTime} -> updateTime) (\s@GetVariantStoreResponse' {} a -> s {updateTime = a} :: GetVariantStoreResponse) Prelude.. Data._Time

-- | The store\'s tags.
getVariantStoreResponse_tags :: Lens.Lens' GetVariantStoreResponse (Prelude.HashMap Prelude.Text Prelude.Text)
getVariantStoreResponse_tags = Lens.lens (\GetVariantStoreResponse' {tags} -> tags) (\s@GetVariantStoreResponse' {} a -> s {tags = a} :: GetVariantStoreResponse) Prelude.. Lens.coerced

-- | The store\'s status message.
getVariantStoreResponse_statusMessage :: Lens.Lens' GetVariantStoreResponse Prelude.Text
getVariantStoreResponse_statusMessage = Lens.lens (\GetVariantStoreResponse' {statusMessage} -> statusMessage) (\s@GetVariantStoreResponse' {} a -> s {statusMessage = a} :: GetVariantStoreResponse)

-- | The store\'s size in bytes.
getVariantStoreResponse_storeSizeBytes :: Lens.Lens' GetVariantStoreResponse Prelude.Integer
getVariantStoreResponse_storeSizeBytes = Lens.lens (\GetVariantStoreResponse' {storeSizeBytes} -> storeSizeBytes) (\s@GetVariantStoreResponse' {} a -> s {storeSizeBytes = a} :: GetVariantStoreResponse)

instance Prelude.NFData GetVariantStoreResponse where
  rnf GetVariantStoreResponse' {..} =
    Prelude.rnf httpStatus
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
