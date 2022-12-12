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
-- Module      : Amazonka.Omics.CreateSequenceStore
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a sequence store.
module Amazonka.Omics.CreateSequenceStore
  ( -- * Creating a Request
    CreateSequenceStore (..),
    newCreateSequenceStore,

    -- * Request Lenses
    createSequenceStore_clientToken,
    createSequenceStore_description,
    createSequenceStore_sseConfig,
    createSequenceStore_tags,
    createSequenceStore_name,

    -- * Destructuring the Response
    CreateSequenceStoreResponse (..),
    newCreateSequenceStoreResponse,

    -- * Response Lenses
    createSequenceStoreResponse_description,
    createSequenceStoreResponse_name,
    createSequenceStoreResponse_sseConfig,
    createSequenceStoreResponse_httpStatus,
    createSequenceStoreResponse_arn,
    createSequenceStoreResponse_creationTime,
    createSequenceStoreResponse_id,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateSequenceStore' smart constructor.
data CreateSequenceStore = CreateSequenceStore'
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
-- Create a value of 'CreateSequenceStore' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createSequenceStore_clientToken' - To ensure that requests don\'t run multiple times, specify a unique
-- token for each request.
--
-- 'description', 'createSequenceStore_description' - A description for the store.
--
-- 'sseConfig', 'createSequenceStore_sseConfig' - Server-side encryption (SSE) settings for the store.
--
-- 'tags', 'createSequenceStore_tags' - Tags for the store.
--
-- 'name', 'createSequenceStore_name' - A name for the store.
newCreateSequenceStore ::
  -- | 'name'
  Prelude.Text ->
  CreateSequenceStore
newCreateSequenceStore pName_ =
  CreateSequenceStore'
    { clientToken = Prelude.Nothing,
      description = Prelude.Nothing,
      sseConfig = Prelude.Nothing,
      tags = Prelude.Nothing,
      name = pName_
    }

-- | To ensure that requests don\'t run multiple times, specify a unique
-- token for each request.
createSequenceStore_clientToken :: Lens.Lens' CreateSequenceStore (Prelude.Maybe Prelude.Text)
createSequenceStore_clientToken = Lens.lens (\CreateSequenceStore' {clientToken} -> clientToken) (\s@CreateSequenceStore' {} a -> s {clientToken = a} :: CreateSequenceStore)

-- | A description for the store.
createSequenceStore_description :: Lens.Lens' CreateSequenceStore (Prelude.Maybe Prelude.Text)
createSequenceStore_description = Lens.lens (\CreateSequenceStore' {description} -> description) (\s@CreateSequenceStore' {} a -> s {description = a} :: CreateSequenceStore)

-- | Server-side encryption (SSE) settings for the store.
createSequenceStore_sseConfig :: Lens.Lens' CreateSequenceStore (Prelude.Maybe SseConfig)
createSequenceStore_sseConfig = Lens.lens (\CreateSequenceStore' {sseConfig} -> sseConfig) (\s@CreateSequenceStore' {} a -> s {sseConfig = a} :: CreateSequenceStore)

-- | Tags for the store.
createSequenceStore_tags :: Lens.Lens' CreateSequenceStore (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createSequenceStore_tags = Lens.lens (\CreateSequenceStore' {tags} -> tags) (\s@CreateSequenceStore' {} a -> s {tags = a} :: CreateSequenceStore) Prelude.. Lens.mapping Lens.coerced

-- | A name for the store.
createSequenceStore_name :: Lens.Lens' CreateSequenceStore Prelude.Text
createSequenceStore_name = Lens.lens (\CreateSequenceStore' {name} -> name) (\s@CreateSequenceStore' {} a -> s {name = a} :: CreateSequenceStore)

instance Core.AWSRequest CreateSequenceStore where
  type
    AWSResponse CreateSequenceStore =
      CreateSequenceStoreResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSequenceStoreResponse'
            Prelude.<$> (x Data..?> "description")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "sseConfig")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "arn")
            Prelude.<*> (x Data..:> "creationTime")
            Prelude.<*> (x Data..:> "id")
      )

instance Prelude.Hashable CreateSequenceStore where
  hashWithSalt _salt CreateSequenceStore' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` sseConfig
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateSequenceStore where
  rnf CreateSequenceStore' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf sseConfig
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateSequenceStore where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateSequenceStore where
  toJSON CreateSequenceStore' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("description" Data..=) Prelude.<$> description,
            ("sseConfig" Data..=) Prelude.<$> sseConfig,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("name" Data..= name)
          ]
      )

instance Data.ToPath CreateSequenceStore where
  toPath = Prelude.const "/sequencestore"

instance Data.ToQuery CreateSequenceStore where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSequenceStoreResponse' smart constructor.
data CreateSequenceStoreResponse = CreateSequenceStoreResponse'
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
    creationTime :: Data.POSIX,
    -- | The store\'s ID.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSequenceStoreResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createSequenceStoreResponse_description' - The store\'s description.
--
-- 'name', 'createSequenceStoreResponse_name' - The store\'s name.
--
-- 'sseConfig', 'createSequenceStoreResponse_sseConfig' - The store\'s SSE settings.
--
-- 'httpStatus', 'createSequenceStoreResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'createSequenceStoreResponse_arn' - The store\'s ARN.
--
-- 'creationTime', 'createSequenceStoreResponse_creationTime' - When the store was created.
--
-- 'id', 'createSequenceStoreResponse_id' - The store\'s ID.
newCreateSequenceStoreResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'id'
  Prelude.Text ->
  CreateSequenceStoreResponse
newCreateSequenceStoreResponse
  pHttpStatus_
  pArn_
  pCreationTime_
  pId_ =
    CreateSequenceStoreResponse'
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
createSequenceStoreResponse_description :: Lens.Lens' CreateSequenceStoreResponse (Prelude.Maybe Prelude.Text)
createSequenceStoreResponse_description = Lens.lens (\CreateSequenceStoreResponse' {description} -> description) (\s@CreateSequenceStoreResponse' {} a -> s {description = a} :: CreateSequenceStoreResponse)

-- | The store\'s name.
createSequenceStoreResponse_name :: Lens.Lens' CreateSequenceStoreResponse (Prelude.Maybe Prelude.Text)
createSequenceStoreResponse_name = Lens.lens (\CreateSequenceStoreResponse' {name} -> name) (\s@CreateSequenceStoreResponse' {} a -> s {name = a} :: CreateSequenceStoreResponse)

-- | The store\'s SSE settings.
createSequenceStoreResponse_sseConfig :: Lens.Lens' CreateSequenceStoreResponse (Prelude.Maybe SseConfig)
createSequenceStoreResponse_sseConfig = Lens.lens (\CreateSequenceStoreResponse' {sseConfig} -> sseConfig) (\s@CreateSequenceStoreResponse' {} a -> s {sseConfig = a} :: CreateSequenceStoreResponse)

-- | The response's http status code.
createSequenceStoreResponse_httpStatus :: Lens.Lens' CreateSequenceStoreResponse Prelude.Int
createSequenceStoreResponse_httpStatus = Lens.lens (\CreateSequenceStoreResponse' {httpStatus} -> httpStatus) (\s@CreateSequenceStoreResponse' {} a -> s {httpStatus = a} :: CreateSequenceStoreResponse)

-- | The store\'s ARN.
createSequenceStoreResponse_arn :: Lens.Lens' CreateSequenceStoreResponse Prelude.Text
createSequenceStoreResponse_arn = Lens.lens (\CreateSequenceStoreResponse' {arn} -> arn) (\s@CreateSequenceStoreResponse' {} a -> s {arn = a} :: CreateSequenceStoreResponse)

-- | When the store was created.
createSequenceStoreResponse_creationTime :: Lens.Lens' CreateSequenceStoreResponse Prelude.UTCTime
createSequenceStoreResponse_creationTime = Lens.lens (\CreateSequenceStoreResponse' {creationTime} -> creationTime) (\s@CreateSequenceStoreResponse' {} a -> s {creationTime = a} :: CreateSequenceStoreResponse) Prelude.. Data._Time

-- | The store\'s ID.
createSequenceStoreResponse_id :: Lens.Lens' CreateSequenceStoreResponse Prelude.Text
createSequenceStoreResponse_id = Lens.lens (\CreateSequenceStoreResponse' {id} -> id) (\s@CreateSequenceStoreResponse' {} a -> s {id = a} :: CreateSequenceStoreResponse)

instance Prelude.NFData CreateSequenceStoreResponse where
  rnf CreateSequenceStoreResponse' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf sseConfig
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf id
