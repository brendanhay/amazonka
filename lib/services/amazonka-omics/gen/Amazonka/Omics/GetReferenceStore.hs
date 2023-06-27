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
-- Module      : Amazonka.Omics.GetReferenceStore
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a reference store.
module Amazonka.Omics.GetReferenceStore
  ( -- * Creating a Request
    GetReferenceStore (..),
    newGetReferenceStore,

    -- * Request Lenses
    getReferenceStore_id,

    -- * Destructuring the Response
    GetReferenceStoreResponse (..),
    newGetReferenceStoreResponse,

    -- * Response Lenses
    getReferenceStoreResponse_description,
    getReferenceStoreResponse_name,
    getReferenceStoreResponse_sseConfig,
    getReferenceStoreResponse_httpStatus,
    getReferenceStoreResponse_id,
    getReferenceStoreResponse_arn,
    getReferenceStoreResponse_creationTime,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetReferenceStore' smart constructor.
data GetReferenceStore = GetReferenceStore'
  { -- | The store\'s ID.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetReferenceStore' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getReferenceStore_id' - The store\'s ID.
newGetReferenceStore ::
  -- | 'id'
  Prelude.Text ->
  GetReferenceStore
newGetReferenceStore pId_ =
  GetReferenceStore' {id = pId_}

-- | The store\'s ID.
getReferenceStore_id :: Lens.Lens' GetReferenceStore Prelude.Text
getReferenceStore_id = Lens.lens (\GetReferenceStore' {id} -> id) (\s@GetReferenceStore' {} a -> s {id = a} :: GetReferenceStore)

instance Core.AWSRequest GetReferenceStore where
  type
    AWSResponse GetReferenceStore =
      GetReferenceStoreResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetReferenceStoreResponse'
            Prelude.<$> (x Data..?> "description")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "sseConfig")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "id")
            Prelude.<*> (x Data..:> "arn")
            Prelude.<*> (x Data..:> "creationTime")
      )

instance Prelude.Hashable GetReferenceStore where
  hashWithSalt _salt GetReferenceStore' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData GetReferenceStore where
  rnf GetReferenceStore' {..} = Prelude.rnf id

instance Data.ToHeaders GetReferenceStore where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetReferenceStore where
  toPath GetReferenceStore' {..} =
    Prelude.mconcat ["/referencestore/", Data.toBS id]

instance Data.ToQuery GetReferenceStore where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetReferenceStoreResponse' smart constructor.
data GetReferenceStoreResponse = GetReferenceStoreResponse'
  { -- | The store\'s description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The store\'s name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The store\'s server-side encryption (SSE) settings.
    sseConfig :: Prelude.Maybe SseConfig,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The store\'s ID.
    id :: Prelude.Text,
    -- | The store\'s ARN.
    arn :: Prelude.Text,
    -- | When the store was created.
    creationTime :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetReferenceStoreResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'getReferenceStoreResponse_description' - The store\'s description.
--
-- 'name', 'getReferenceStoreResponse_name' - The store\'s name.
--
-- 'sseConfig', 'getReferenceStoreResponse_sseConfig' - The store\'s server-side encryption (SSE) settings.
--
-- 'httpStatus', 'getReferenceStoreResponse_httpStatus' - The response's http status code.
--
-- 'id', 'getReferenceStoreResponse_id' - The store\'s ID.
--
-- 'arn', 'getReferenceStoreResponse_arn' - The store\'s ARN.
--
-- 'creationTime', 'getReferenceStoreResponse_creationTime' - When the store was created.
newGetReferenceStoreResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'id'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  GetReferenceStoreResponse
newGetReferenceStoreResponse
  pHttpStatus_
  pId_
  pArn_
  pCreationTime_ =
    GetReferenceStoreResponse'
      { description =
          Prelude.Nothing,
        name = Prelude.Nothing,
        sseConfig = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        id = pId_,
        arn = pArn_,
        creationTime = Data._Time Lens.# pCreationTime_
      }

-- | The store\'s description.
getReferenceStoreResponse_description :: Lens.Lens' GetReferenceStoreResponse (Prelude.Maybe Prelude.Text)
getReferenceStoreResponse_description = Lens.lens (\GetReferenceStoreResponse' {description} -> description) (\s@GetReferenceStoreResponse' {} a -> s {description = a} :: GetReferenceStoreResponse)

-- | The store\'s name.
getReferenceStoreResponse_name :: Lens.Lens' GetReferenceStoreResponse (Prelude.Maybe Prelude.Text)
getReferenceStoreResponse_name = Lens.lens (\GetReferenceStoreResponse' {name} -> name) (\s@GetReferenceStoreResponse' {} a -> s {name = a} :: GetReferenceStoreResponse)

-- | The store\'s server-side encryption (SSE) settings.
getReferenceStoreResponse_sseConfig :: Lens.Lens' GetReferenceStoreResponse (Prelude.Maybe SseConfig)
getReferenceStoreResponse_sseConfig = Lens.lens (\GetReferenceStoreResponse' {sseConfig} -> sseConfig) (\s@GetReferenceStoreResponse' {} a -> s {sseConfig = a} :: GetReferenceStoreResponse)

-- | The response's http status code.
getReferenceStoreResponse_httpStatus :: Lens.Lens' GetReferenceStoreResponse Prelude.Int
getReferenceStoreResponse_httpStatus = Lens.lens (\GetReferenceStoreResponse' {httpStatus} -> httpStatus) (\s@GetReferenceStoreResponse' {} a -> s {httpStatus = a} :: GetReferenceStoreResponse)

-- | The store\'s ID.
getReferenceStoreResponse_id :: Lens.Lens' GetReferenceStoreResponse Prelude.Text
getReferenceStoreResponse_id = Lens.lens (\GetReferenceStoreResponse' {id} -> id) (\s@GetReferenceStoreResponse' {} a -> s {id = a} :: GetReferenceStoreResponse)

-- | The store\'s ARN.
getReferenceStoreResponse_arn :: Lens.Lens' GetReferenceStoreResponse Prelude.Text
getReferenceStoreResponse_arn = Lens.lens (\GetReferenceStoreResponse' {arn} -> arn) (\s@GetReferenceStoreResponse' {} a -> s {arn = a} :: GetReferenceStoreResponse)

-- | When the store was created.
getReferenceStoreResponse_creationTime :: Lens.Lens' GetReferenceStoreResponse Prelude.UTCTime
getReferenceStoreResponse_creationTime = Lens.lens (\GetReferenceStoreResponse' {creationTime} -> creationTime) (\s@GetReferenceStoreResponse' {} a -> s {creationTime = a} :: GetReferenceStoreResponse) Prelude.. Data._Time

instance Prelude.NFData GetReferenceStoreResponse where
  rnf GetReferenceStoreResponse' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf sseConfig
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationTime
