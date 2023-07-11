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
-- Module      : Amazonka.Omics.BatchDeleteReadSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes one or more read sets.
module Amazonka.Omics.BatchDeleteReadSet
  ( -- * Creating a Request
    BatchDeleteReadSet (..),
    newBatchDeleteReadSet,

    -- * Request Lenses
    batchDeleteReadSet_ids,
    batchDeleteReadSet_sequenceStoreId,

    -- * Destructuring the Response
    BatchDeleteReadSetResponse (..),
    newBatchDeleteReadSetResponse,

    -- * Response Lenses
    batchDeleteReadSetResponse_errors,
    batchDeleteReadSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchDeleteReadSet' smart constructor.
data BatchDeleteReadSet = BatchDeleteReadSet'
  { -- | The read sets\' IDs.
    ids :: Prelude.NonEmpty Prelude.Text,
    -- | The read sets\' sequence store ID.
    sequenceStoreId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDeleteReadSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ids', 'batchDeleteReadSet_ids' - The read sets\' IDs.
--
-- 'sequenceStoreId', 'batchDeleteReadSet_sequenceStoreId' - The read sets\' sequence store ID.
newBatchDeleteReadSet ::
  -- | 'ids'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'sequenceStoreId'
  Prelude.Text ->
  BatchDeleteReadSet
newBatchDeleteReadSet pIds_ pSequenceStoreId_ =
  BatchDeleteReadSet'
    { ids =
        Lens.coerced Lens.# pIds_,
      sequenceStoreId = pSequenceStoreId_
    }

-- | The read sets\' IDs.
batchDeleteReadSet_ids :: Lens.Lens' BatchDeleteReadSet (Prelude.NonEmpty Prelude.Text)
batchDeleteReadSet_ids = Lens.lens (\BatchDeleteReadSet' {ids} -> ids) (\s@BatchDeleteReadSet' {} a -> s {ids = a} :: BatchDeleteReadSet) Prelude.. Lens.coerced

-- | The read sets\' sequence store ID.
batchDeleteReadSet_sequenceStoreId :: Lens.Lens' BatchDeleteReadSet Prelude.Text
batchDeleteReadSet_sequenceStoreId = Lens.lens (\BatchDeleteReadSet' {sequenceStoreId} -> sequenceStoreId) (\s@BatchDeleteReadSet' {} a -> s {sequenceStoreId = a} :: BatchDeleteReadSet)

instance Core.AWSRequest BatchDeleteReadSet where
  type
    AWSResponse BatchDeleteReadSet =
      BatchDeleteReadSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchDeleteReadSetResponse'
            Prelude.<$> (x Data..?> "errors" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchDeleteReadSet where
  hashWithSalt _salt BatchDeleteReadSet' {..} =
    _salt
      `Prelude.hashWithSalt` ids
      `Prelude.hashWithSalt` sequenceStoreId

instance Prelude.NFData BatchDeleteReadSet where
  rnf BatchDeleteReadSet' {..} =
    Prelude.rnf ids
      `Prelude.seq` Prelude.rnf sequenceStoreId

instance Data.ToHeaders BatchDeleteReadSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchDeleteReadSet where
  toJSON BatchDeleteReadSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ids" Data..= ids)]
      )

instance Data.ToPath BatchDeleteReadSet where
  toPath BatchDeleteReadSet' {..} =
    Prelude.mconcat
      [ "/sequencestore/",
        Data.toBS sequenceStoreId,
        "/readset/batch/delete"
      ]

instance Data.ToQuery BatchDeleteReadSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchDeleteReadSetResponse' smart constructor.
data BatchDeleteReadSetResponse = BatchDeleteReadSetResponse'
  { -- | Errors returned by individual delete operations.
    errors :: Prelude.Maybe [ReadSetBatchError],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDeleteReadSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errors', 'batchDeleteReadSetResponse_errors' - Errors returned by individual delete operations.
--
-- 'httpStatus', 'batchDeleteReadSetResponse_httpStatus' - The response's http status code.
newBatchDeleteReadSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchDeleteReadSetResponse
newBatchDeleteReadSetResponse pHttpStatus_ =
  BatchDeleteReadSetResponse'
    { errors =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Errors returned by individual delete operations.
batchDeleteReadSetResponse_errors :: Lens.Lens' BatchDeleteReadSetResponse (Prelude.Maybe [ReadSetBatchError])
batchDeleteReadSetResponse_errors = Lens.lens (\BatchDeleteReadSetResponse' {errors} -> errors) (\s@BatchDeleteReadSetResponse' {} a -> s {errors = a} :: BatchDeleteReadSetResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchDeleteReadSetResponse_httpStatus :: Lens.Lens' BatchDeleteReadSetResponse Prelude.Int
batchDeleteReadSetResponse_httpStatus = Lens.lens (\BatchDeleteReadSetResponse' {httpStatus} -> httpStatus) (\s@BatchDeleteReadSetResponse' {} a -> s {httpStatus = a} :: BatchDeleteReadSetResponse)

instance Prelude.NFData BatchDeleteReadSetResponse where
  rnf BatchDeleteReadSetResponse' {..} =
    Prelude.rnf errors
      `Prelude.seq` Prelude.rnf httpStatus
