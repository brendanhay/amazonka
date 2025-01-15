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
-- Module      : Amazonka.Omics.GetReference
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a reference file.
module Amazonka.Omics.GetReference
  ( -- * Creating a Request
    GetReference (..),
    newGetReference,

    -- * Request Lenses
    getReference_file,
    getReference_range,
    getReference_id,
    getReference_partNumber,
    getReference_referenceStoreId,

    -- * Destructuring the Response
    GetReferenceResponse (..),
    newGetReferenceResponse,

    -- * Response Lenses
    getReferenceResponse_httpStatus,
    getReferenceResponse_payload,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetReference' smart constructor.
data GetReference = GetReference'
  { -- | The file to retrieve.
    file :: Prelude.Maybe ReferenceFile,
    -- | The range to retrieve.
    range :: Prelude.Maybe Prelude.Text,
    -- | The reference\'s ID.
    id :: Prelude.Text,
    -- | The part number to retrieve.
    partNumber :: Prelude.Natural,
    -- | The reference\'s store ID.
    referenceStoreId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetReference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'file', 'getReference_file' - The file to retrieve.
--
-- 'range', 'getReference_range' - The range to retrieve.
--
-- 'id', 'getReference_id' - The reference\'s ID.
--
-- 'partNumber', 'getReference_partNumber' - The part number to retrieve.
--
-- 'referenceStoreId', 'getReference_referenceStoreId' - The reference\'s store ID.
newGetReference ::
  -- | 'id'
  Prelude.Text ->
  -- | 'partNumber'
  Prelude.Natural ->
  -- | 'referenceStoreId'
  Prelude.Text ->
  GetReference
newGetReference pId_ pPartNumber_ pReferenceStoreId_ =
  GetReference'
    { file = Prelude.Nothing,
      range = Prelude.Nothing,
      id = pId_,
      partNumber = pPartNumber_,
      referenceStoreId = pReferenceStoreId_
    }

-- | The file to retrieve.
getReference_file :: Lens.Lens' GetReference (Prelude.Maybe ReferenceFile)
getReference_file = Lens.lens (\GetReference' {file} -> file) (\s@GetReference' {} a -> s {file = a} :: GetReference)

-- | The range to retrieve.
getReference_range :: Lens.Lens' GetReference (Prelude.Maybe Prelude.Text)
getReference_range = Lens.lens (\GetReference' {range} -> range) (\s@GetReference' {} a -> s {range = a} :: GetReference)

-- | The reference\'s ID.
getReference_id :: Lens.Lens' GetReference Prelude.Text
getReference_id = Lens.lens (\GetReference' {id} -> id) (\s@GetReference' {} a -> s {id = a} :: GetReference)

-- | The part number to retrieve.
getReference_partNumber :: Lens.Lens' GetReference Prelude.Natural
getReference_partNumber = Lens.lens (\GetReference' {partNumber} -> partNumber) (\s@GetReference' {} a -> s {partNumber = a} :: GetReference)

-- | The reference\'s store ID.
getReference_referenceStoreId :: Lens.Lens' GetReference Prelude.Text
getReference_referenceStoreId = Lens.lens (\GetReference' {referenceStoreId} -> referenceStoreId) (\s@GetReference' {} a -> s {referenceStoreId = a} :: GetReference)

instance Core.AWSRequest GetReference where
  type AWSResponse GetReference = GetReferenceResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveBody
      ( \s h x ->
          GetReferenceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Prelude.pure x)
      )

instance Prelude.Hashable GetReference where
  hashWithSalt _salt GetReference' {..} =
    _salt
      `Prelude.hashWithSalt` file
      `Prelude.hashWithSalt` range
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` partNumber
      `Prelude.hashWithSalt` referenceStoreId

instance Prelude.NFData GetReference where
  rnf GetReference' {..} =
    Prelude.rnf file `Prelude.seq`
      Prelude.rnf range `Prelude.seq`
        Prelude.rnf id `Prelude.seq`
          Prelude.rnf partNumber `Prelude.seq`
            Prelude.rnf referenceStoreId

instance Data.ToHeaders GetReference where
  toHeaders GetReference' {..} =
    Prelude.mconcat
      [ "Range" Data.=# range,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToPath GetReference where
  toPath GetReference' {..} =
    Prelude.mconcat
      [ "/referencestore/",
        Data.toBS referenceStoreId,
        "/reference/",
        Data.toBS id
      ]

instance Data.ToQuery GetReference where
  toQuery GetReference' {..} =
    Prelude.mconcat
      [ "file" Data.=: file,
        "partNumber" Data.=: partNumber
      ]

-- | /See:/ 'newGetReferenceResponse' smart constructor.
data GetReferenceResponse = GetReferenceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The reference file payload.
    payload :: Data.ResponseBody
  }
  deriving (Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetReferenceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getReferenceResponse_httpStatus' - The response's http status code.
--
-- 'payload', 'getReferenceResponse_payload' - The reference file payload.
newGetReferenceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'payload'
  Data.ResponseBody ->
  GetReferenceResponse
newGetReferenceResponse pHttpStatus_ pPayload_ =
  GetReferenceResponse'
    { httpStatus = pHttpStatus_,
      payload = pPayload_
    }

-- | The response's http status code.
getReferenceResponse_httpStatus :: Lens.Lens' GetReferenceResponse Prelude.Int
getReferenceResponse_httpStatus = Lens.lens (\GetReferenceResponse' {httpStatus} -> httpStatus) (\s@GetReferenceResponse' {} a -> s {httpStatus = a} :: GetReferenceResponse)

-- | The reference file payload.
getReferenceResponse_payload :: Lens.Lens' GetReferenceResponse Data.ResponseBody
getReferenceResponse_payload = Lens.lens (\GetReferenceResponse' {payload} -> payload) (\s@GetReferenceResponse' {} a -> s {payload = a} :: GetReferenceResponse)
