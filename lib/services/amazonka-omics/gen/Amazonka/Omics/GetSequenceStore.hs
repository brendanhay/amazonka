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
-- Module      : Amazonka.Omics.GetSequenceStore
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a sequence store.
module Amazonka.Omics.GetSequenceStore
  ( -- * Creating a Request
    GetSequenceStore (..),
    newGetSequenceStore,

    -- * Request Lenses
    getSequenceStore_id,

    -- * Destructuring the Response
    GetSequenceStoreResponse (..),
    newGetSequenceStoreResponse,

    -- * Response Lenses
    getSequenceStoreResponse_description,
    getSequenceStoreResponse_name,
    getSequenceStoreResponse_sseConfig,
    getSequenceStoreResponse_httpStatus,
    getSequenceStoreResponse_arn,
    getSequenceStoreResponse_creationTime,
    getSequenceStoreResponse_id,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetSequenceStore' smart constructor.
data GetSequenceStore = GetSequenceStore'
  { -- | The store\'s ID.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSequenceStore' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getSequenceStore_id' - The store\'s ID.
newGetSequenceStore ::
  -- | 'id'
  Prelude.Text ->
  GetSequenceStore
newGetSequenceStore pId_ =
  GetSequenceStore' {id = pId_}

-- | The store\'s ID.
getSequenceStore_id :: Lens.Lens' GetSequenceStore Prelude.Text
getSequenceStore_id = Lens.lens (\GetSequenceStore' {id} -> id) (\s@GetSequenceStore' {} a -> s {id = a} :: GetSequenceStore)

instance Core.AWSRequest GetSequenceStore where
  type
    AWSResponse GetSequenceStore =
      GetSequenceStoreResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSequenceStoreResponse'
            Prelude.<$> (x Data..?> "description")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "sseConfig")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "arn")
            Prelude.<*> (x Data..:> "creationTime")
            Prelude.<*> (x Data..:> "id")
      )

instance Prelude.Hashable GetSequenceStore where
  hashWithSalt _salt GetSequenceStore' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData GetSequenceStore where
  rnf GetSequenceStore' {..} = Prelude.rnf id

instance Data.ToHeaders GetSequenceStore where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetSequenceStore where
  toPath GetSequenceStore' {..} =
    Prelude.mconcat ["/sequencestore/", Data.toBS id]

instance Data.ToQuery GetSequenceStore where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSequenceStoreResponse' smart constructor.
data GetSequenceStoreResponse = GetSequenceStoreResponse'
  { -- | The store\'s description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The store\'s name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The store\'s server-side encryption (SSE) settings.
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
-- Create a value of 'GetSequenceStoreResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'getSequenceStoreResponse_description' - The store\'s description.
--
-- 'name', 'getSequenceStoreResponse_name' - The store\'s name.
--
-- 'sseConfig', 'getSequenceStoreResponse_sseConfig' - The store\'s server-side encryption (SSE) settings.
--
-- 'httpStatus', 'getSequenceStoreResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'getSequenceStoreResponse_arn' - The store\'s ARN.
--
-- 'creationTime', 'getSequenceStoreResponse_creationTime' - When the store was created.
--
-- 'id', 'getSequenceStoreResponse_id' - The store\'s ID.
newGetSequenceStoreResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'id'
  Prelude.Text ->
  GetSequenceStoreResponse
newGetSequenceStoreResponse
  pHttpStatus_
  pArn_
  pCreationTime_
  pId_ =
    GetSequenceStoreResponse'
      { description =
          Prelude.Nothing,
        name = Prelude.Nothing,
        sseConfig = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        arn = pArn_,
        creationTime = Data._Time Lens.# pCreationTime_,
        id = pId_
      }

-- | The store\'s description.
getSequenceStoreResponse_description :: Lens.Lens' GetSequenceStoreResponse (Prelude.Maybe Prelude.Text)
getSequenceStoreResponse_description = Lens.lens (\GetSequenceStoreResponse' {description} -> description) (\s@GetSequenceStoreResponse' {} a -> s {description = a} :: GetSequenceStoreResponse)

-- | The store\'s name.
getSequenceStoreResponse_name :: Lens.Lens' GetSequenceStoreResponse (Prelude.Maybe Prelude.Text)
getSequenceStoreResponse_name = Lens.lens (\GetSequenceStoreResponse' {name} -> name) (\s@GetSequenceStoreResponse' {} a -> s {name = a} :: GetSequenceStoreResponse)

-- | The store\'s server-side encryption (SSE) settings.
getSequenceStoreResponse_sseConfig :: Lens.Lens' GetSequenceStoreResponse (Prelude.Maybe SseConfig)
getSequenceStoreResponse_sseConfig = Lens.lens (\GetSequenceStoreResponse' {sseConfig} -> sseConfig) (\s@GetSequenceStoreResponse' {} a -> s {sseConfig = a} :: GetSequenceStoreResponse)

-- | The response's http status code.
getSequenceStoreResponse_httpStatus :: Lens.Lens' GetSequenceStoreResponse Prelude.Int
getSequenceStoreResponse_httpStatus = Lens.lens (\GetSequenceStoreResponse' {httpStatus} -> httpStatus) (\s@GetSequenceStoreResponse' {} a -> s {httpStatus = a} :: GetSequenceStoreResponse)

-- | The store\'s ARN.
getSequenceStoreResponse_arn :: Lens.Lens' GetSequenceStoreResponse Prelude.Text
getSequenceStoreResponse_arn = Lens.lens (\GetSequenceStoreResponse' {arn} -> arn) (\s@GetSequenceStoreResponse' {} a -> s {arn = a} :: GetSequenceStoreResponse)

-- | When the store was created.
getSequenceStoreResponse_creationTime :: Lens.Lens' GetSequenceStoreResponse Prelude.UTCTime
getSequenceStoreResponse_creationTime = Lens.lens (\GetSequenceStoreResponse' {creationTime} -> creationTime) (\s@GetSequenceStoreResponse' {} a -> s {creationTime = a} :: GetSequenceStoreResponse) Prelude.. Data._Time

-- | The store\'s ID.
getSequenceStoreResponse_id :: Lens.Lens' GetSequenceStoreResponse Prelude.Text
getSequenceStoreResponse_id = Lens.lens (\GetSequenceStoreResponse' {id} -> id) (\s@GetSequenceStoreResponse' {} a -> s {id = a} :: GetSequenceStoreResponse)

instance Prelude.NFData GetSequenceStoreResponse where
  rnf GetSequenceStoreResponse' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf name `Prelude.seq`
        Prelude.rnf sseConfig `Prelude.seq`
          Prelude.rnf httpStatus `Prelude.seq`
            Prelude.rnf arn `Prelude.seq`
              Prelude.rnf creationTime `Prelude.seq`
                Prelude.rnf id
