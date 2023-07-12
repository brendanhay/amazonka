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
-- Module      : Amazonka.Omics.GetReadSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a file from a read set.
module Amazonka.Omics.GetReadSet
  ( -- * Creating a Request
    GetReadSet (..),
    newGetReadSet,

    -- * Request Lenses
    getReadSet_file,
    getReadSet_id,
    getReadSet_partNumber,
    getReadSet_sequenceStoreId,

    -- * Destructuring the Response
    GetReadSetResponse (..),
    newGetReadSetResponse,

    -- * Response Lenses
    getReadSetResponse_httpStatus,
    getReadSetResponse_payload,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetReadSet' smart constructor.
data GetReadSet = GetReadSet'
  { -- | The file to retrieve.
    file :: Prelude.Maybe ReadSetFile,
    -- | The read set\'s ID.
    id :: Prelude.Text,
    -- | The part number to retrieve.
    partNumber :: Prelude.Natural,
    -- | The read set\'s sequence store ID.
    sequenceStoreId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetReadSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'file', 'getReadSet_file' - The file to retrieve.
--
-- 'id', 'getReadSet_id' - The read set\'s ID.
--
-- 'partNumber', 'getReadSet_partNumber' - The part number to retrieve.
--
-- 'sequenceStoreId', 'getReadSet_sequenceStoreId' - The read set\'s sequence store ID.
newGetReadSet ::
  -- | 'id'
  Prelude.Text ->
  -- | 'partNumber'
  Prelude.Natural ->
  -- | 'sequenceStoreId'
  Prelude.Text ->
  GetReadSet
newGetReadSet pId_ pPartNumber_ pSequenceStoreId_ =
  GetReadSet'
    { file = Prelude.Nothing,
      id = pId_,
      partNumber = pPartNumber_,
      sequenceStoreId = pSequenceStoreId_
    }

-- | The file to retrieve.
getReadSet_file :: Lens.Lens' GetReadSet (Prelude.Maybe ReadSetFile)
getReadSet_file = Lens.lens (\GetReadSet' {file} -> file) (\s@GetReadSet' {} a -> s {file = a} :: GetReadSet)

-- | The read set\'s ID.
getReadSet_id :: Lens.Lens' GetReadSet Prelude.Text
getReadSet_id = Lens.lens (\GetReadSet' {id} -> id) (\s@GetReadSet' {} a -> s {id = a} :: GetReadSet)

-- | The part number to retrieve.
getReadSet_partNumber :: Lens.Lens' GetReadSet Prelude.Natural
getReadSet_partNumber = Lens.lens (\GetReadSet' {partNumber} -> partNumber) (\s@GetReadSet' {} a -> s {partNumber = a} :: GetReadSet)

-- | The read set\'s sequence store ID.
getReadSet_sequenceStoreId :: Lens.Lens' GetReadSet Prelude.Text
getReadSet_sequenceStoreId = Lens.lens (\GetReadSet' {sequenceStoreId} -> sequenceStoreId) (\s@GetReadSet' {} a -> s {sequenceStoreId = a} :: GetReadSet)

instance Core.AWSRequest GetReadSet where
  type AWSResponse GetReadSet = GetReadSetResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveBody
      ( \s h x ->
          GetReadSetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Prelude.pure x)
      )

instance Prelude.Hashable GetReadSet where
  hashWithSalt _salt GetReadSet' {..} =
    _salt
      `Prelude.hashWithSalt` file
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` partNumber
      `Prelude.hashWithSalt` sequenceStoreId

instance Prelude.NFData GetReadSet where
  rnf GetReadSet' {..} =
    Prelude.rnf file
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf partNumber
      `Prelude.seq` Prelude.rnf sequenceStoreId

instance Data.ToHeaders GetReadSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetReadSet where
  toPath GetReadSet' {..} =
    Prelude.mconcat
      [ "/sequencestore/",
        Data.toBS sequenceStoreId,
        "/readset/",
        Data.toBS id
      ]

instance Data.ToQuery GetReadSet where
  toQuery GetReadSet' {..} =
    Prelude.mconcat
      [ "file" Data.=: file,
        "partNumber" Data.=: partNumber
      ]

-- | /See:/ 'newGetReadSetResponse' smart constructor.
data GetReadSetResponse = GetReadSetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The read set file payload.
    payload :: Data.ResponseBody
  }
  deriving (Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetReadSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getReadSetResponse_httpStatus' - The response's http status code.
--
-- 'payload', 'getReadSetResponse_payload' - The read set file payload.
newGetReadSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'payload'
  Data.ResponseBody ->
  GetReadSetResponse
newGetReadSetResponse pHttpStatus_ pPayload_ =
  GetReadSetResponse'
    { httpStatus = pHttpStatus_,
      payload = pPayload_
    }

-- | The response's http status code.
getReadSetResponse_httpStatus :: Lens.Lens' GetReadSetResponse Prelude.Int
getReadSetResponse_httpStatus = Lens.lens (\GetReadSetResponse' {httpStatus} -> httpStatus) (\s@GetReadSetResponse' {} a -> s {httpStatus = a} :: GetReadSetResponse)

-- | The read set file payload.
getReadSetResponse_payload :: Lens.Lens' GetReadSetResponse Data.ResponseBody
getReadSetResponse_payload = Lens.lens (\GetReadSetResponse' {payload} -> payload) (\s@GetReadSetResponse' {} a -> s {payload = a} :: GetReadSetResponse)
