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
-- Module      : Amazonka.Omics.CreateReferenceStore
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a reference store.
module Amazonka.Omics.CreateReferenceStore
  ( -- * Creating a Request
    CreateReferenceStore (..),
    newCreateReferenceStore,

    -- * Request Lenses
    createReferenceStore_clientToken,
    createReferenceStore_description,
    createReferenceStore_sseConfig,
    createReferenceStore_tags,
    createReferenceStore_name,

    -- * Destructuring the Response
    CreateReferenceStoreResponse (..),
    newCreateReferenceStoreResponse,

    -- * Response Lenses
    createReferenceStoreResponse_description,
    createReferenceStoreResponse_name,
    createReferenceStoreResponse_sseConfig,
    createReferenceStoreResponse_httpStatus,
    createReferenceStoreResponse_arn,
    createReferenceStoreResponse_creationTime,
    createReferenceStoreResponse_id,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateReferenceStore' smart constructor.
data CreateReferenceStore = CreateReferenceStore'
  { -- | To ensure that requests don\'t run multiple times, specify a unique
    -- token for each request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A description for the store.
    description :: Prelude.Maybe Prelude.Text,
    -- | Server-side encryption (SSE) settings for the store.
    sseConfig :: Prelude.Maybe SseConfig,
    -- | Tags for the store.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A name for the store.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateReferenceStore' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createReferenceStore_clientToken' - To ensure that requests don\'t run multiple times, specify a unique
-- token for each request.
--
-- 'description', 'createReferenceStore_description' - A description for the store.
--
-- 'sseConfig', 'createReferenceStore_sseConfig' - Server-side encryption (SSE) settings for the store.
--
-- 'tags', 'createReferenceStore_tags' - Tags for the store.
--
-- 'name', 'createReferenceStore_name' - A name for the store.
newCreateReferenceStore ::
  -- | 'name'
  Prelude.Text ->
  CreateReferenceStore
newCreateReferenceStore pName_ =
  CreateReferenceStore'
    { clientToken =
        Prelude.Nothing,
      description = Prelude.Nothing,
      sseConfig = Prelude.Nothing,
      tags = Prelude.Nothing,
      name = pName_
    }

-- | To ensure that requests don\'t run multiple times, specify a unique
-- token for each request.
createReferenceStore_clientToken :: Lens.Lens' CreateReferenceStore (Prelude.Maybe Prelude.Text)
createReferenceStore_clientToken = Lens.lens (\CreateReferenceStore' {clientToken} -> clientToken) (\s@CreateReferenceStore' {} a -> s {clientToken = a} :: CreateReferenceStore)

-- | A description for the store.
createReferenceStore_description :: Lens.Lens' CreateReferenceStore (Prelude.Maybe Prelude.Text)
createReferenceStore_description = Lens.lens (\CreateReferenceStore' {description} -> description) (\s@CreateReferenceStore' {} a -> s {description = a} :: CreateReferenceStore)

-- | Server-side encryption (SSE) settings for the store.
createReferenceStore_sseConfig :: Lens.Lens' CreateReferenceStore (Prelude.Maybe SseConfig)
createReferenceStore_sseConfig = Lens.lens (\CreateReferenceStore' {sseConfig} -> sseConfig) (\s@CreateReferenceStore' {} a -> s {sseConfig = a} :: CreateReferenceStore)

-- | Tags for the store.
createReferenceStore_tags :: Lens.Lens' CreateReferenceStore (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createReferenceStore_tags = Lens.lens (\CreateReferenceStore' {tags} -> tags) (\s@CreateReferenceStore' {} a -> s {tags = a} :: CreateReferenceStore) Prelude.. Lens.mapping Lens.coerced

-- | A name for the store.
createReferenceStore_name :: Lens.Lens' CreateReferenceStore Prelude.Text
createReferenceStore_name = Lens.lens (\CreateReferenceStore' {name} -> name) (\s@CreateReferenceStore' {} a -> s {name = a} :: CreateReferenceStore)

instance Core.AWSRequest CreateReferenceStore where
  type
    AWSResponse CreateReferenceStore =
      CreateReferenceStoreResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateReferenceStoreResponse'
            Prelude.<$> (x Data..?> "description")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "sseConfig")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "arn")
            Prelude.<*> (x Data..:> "creationTime")
            Prelude.<*> (x Data..:> "id")
      )

instance Prelude.Hashable CreateReferenceStore where
  hashWithSalt _salt CreateReferenceStore' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` sseConfig
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateReferenceStore where
  rnf CreateReferenceStore' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf sseConfig
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateReferenceStore where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateReferenceStore where
  toJSON CreateReferenceStore' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("description" Data..=) Prelude.<$> description,
            ("sseConfig" Data..=) Prelude.<$> sseConfig,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("name" Data..= name)
          ]
      )

instance Data.ToPath CreateReferenceStore where
  toPath = Prelude.const "/referencestore"

instance Data.ToQuery CreateReferenceStore where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateReferenceStoreResponse' smart constructor.
data CreateReferenceStoreResponse = CreateReferenceStoreResponse'
  { -- | The store\'s description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The store\'s name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The store\'s SSE settings.
    sseConfig :: Prelude.Maybe SseConfig,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The store\'s ARN.
    arn :: Prelude.Text,
    -- | When the store was created.
    creationTime :: Data.ISO8601,
    -- | The store\'s ID.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateReferenceStoreResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createReferenceStoreResponse_description' - The store\'s description.
--
-- 'name', 'createReferenceStoreResponse_name' - The store\'s name.
--
-- 'sseConfig', 'createReferenceStoreResponse_sseConfig' - The store\'s SSE settings.
--
-- 'httpStatus', 'createReferenceStoreResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'createReferenceStoreResponse_arn' - The store\'s ARN.
--
-- 'creationTime', 'createReferenceStoreResponse_creationTime' - When the store was created.
--
-- 'id', 'createReferenceStoreResponse_id' - The store\'s ID.
newCreateReferenceStoreResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'id'
  Prelude.Text ->
  CreateReferenceStoreResponse
newCreateReferenceStoreResponse
  pHttpStatus_
  pArn_
  pCreationTime_
  pId_ =
    CreateReferenceStoreResponse'
      { description =
          Prelude.Nothing,
        name = Prelude.Nothing,
        sseConfig = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        arn = pArn_,
        creationTime =
          Data._Time Lens.# pCreationTime_,
        id = pId_
      }

-- | The store\'s description.
createReferenceStoreResponse_description :: Lens.Lens' CreateReferenceStoreResponse (Prelude.Maybe Prelude.Text)
createReferenceStoreResponse_description = Lens.lens (\CreateReferenceStoreResponse' {description} -> description) (\s@CreateReferenceStoreResponse' {} a -> s {description = a} :: CreateReferenceStoreResponse)

-- | The store\'s name.
createReferenceStoreResponse_name :: Lens.Lens' CreateReferenceStoreResponse (Prelude.Maybe Prelude.Text)
createReferenceStoreResponse_name = Lens.lens (\CreateReferenceStoreResponse' {name} -> name) (\s@CreateReferenceStoreResponse' {} a -> s {name = a} :: CreateReferenceStoreResponse)

-- | The store\'s SSE settings.
createReferenceStoreResponse_sseConfig :: Lens.Lens' CreateReferenceStoreResponse (Prelude.Maybe SseConfig)
createReferenceStoreResponse_sseConfig = Lens.lens (\CreateReferenceStoreResponse' {sseConfig} -> sseConfig) (\s@CreateReferenceStoreResponse' {} a -> s {sseConfig = a} :: CreateReferenceStoreResponse)

-- | The response's http status code.
createReferenceStoreResponse_httpStatus :: Lens.Lens' CreateReferenceStoreResponse Prelude.Int
createReferenceStoreResponse_httpStatus = Lens.lens (\CreateReferenceStoreResponse' {httpStatus} -> httpStatus) (\s@CreateReferenceStoreResponse' {} a -> s {httpStatus = a} :: CreateReferenceStoreResponse)

-- | The store\'s ARN.
createReferenceStoreResponse_arn :: Lens.Lens' CreateReferenceStoreResponse Prelude.Text
createReferenceStoreResponse_arn = Lens.lens (\CreateReferenceStoreResponse' {arn} -> arn) (\s@CreateReferenceStoreResponse' {} a -> s {arn = a} :: CreateReferenceStoreResponse)

-- | When the store was created.
createReferenceStoreResponse_creationTime :: Lens.Lens' CreateReferenceStoreResponse Prelude.UTCTime
createReferenceStoreResponse_creationTime = Lens.lens (\CreateReferenceStoreResponse' {creationTime} -> creationTime) (\s@CreateReferenceStoreResponse' {} a -> s {creationTime = a} :: CreateReferenceStoreResponse) Prelude.. Data._Time

-- | The store\'s ID.
createReferenceStoreResponse_id :: Lens.Lens' CreateReferenceStoreResponse Prelude.Text
createReferenceStoreResponse_id = Lens.lens (\CreateReferenceStoreResponse' {id} -> id) (\s@CreateReferenceStoreResponse' {} a -> s {id = a} :: CreateReferenceStoreResponse)

instance Prelude.NFData CreateReferenceStoreResponse where
  rnf CreateReferenceStoreResponse' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf sseConfig
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf id
