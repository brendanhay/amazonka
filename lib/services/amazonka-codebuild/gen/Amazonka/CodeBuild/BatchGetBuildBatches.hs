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
-- Module      : Amazonka.CodeBuild.BatchGetBuildBatches
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about one or more batch builds.
module Amazonka.CodeBuild.BatchGetBuildBatches
  ( -- * Creating a Request
    BatchGetBuildBatches (..),
    newBatchGetBuildBatches,

    -- * Request Lenses
    batchGetBuildBatches_ids,

    -- * Destructuring the Response
    BatchGetBuildBatchesResponse (..),
    newBatchGetBuildBatchesResponse,

    -- * Response Lenses
    batchGetBuildBatchesResponse_buildBatches,
    batchGetBuildBatchesResponse_buildBatchesNotFound,
    batchGetBuildBatchesResponse_httpStatus,
  )
where

import Amazonka.CodeBuild.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchGetBuildBatches' smart constructor.
data BatchGetBuildBatches = BatchGetBuildBatches'
  { -- | An array that contains the batch build identifiers to retrieve.
    ids :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetBuildBatches' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ids', 'batchGetBuildBatches_ids' - An array that contains the batch build identifiers to retrieve.
newBatchGetBuildBatches ::
  BatchGetBuildBatches
newBatchGetBuildBatches =
  BatchGetBuildBatches' {ids = Prelude.mempty}

-- | An array that contains the batch build identifiers to retrieve.
batchGetBuildBatches_ids :: Lens.Lens' BatchGetBuildBatches [Prelude.Text]
batchGetBuildBatches_ids = Lens.lens (\BatchGetBuildBatches' {ids} -> ids) (\s@BatchGetBuildBatches' {} a -> s {ids = a} :: BatchGetBuildBatches) Prelude.. Lens.coerced

instance Core.AWSRequest BatchGetBuildBatches where
  type
    AWSResponse BatchGetBuildBatches =
      BatchGetBuildBatchesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetBuildBatchesResponse'
            Prelude.<$> (x Data..?> "buildBatches" Core..!@ Prelude.mempty)
            Prelude.<*> ( x Data..?> "buildBatchesNotFound"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchGetBuildBatches where
  hashWithSalt _salt BatchGetBuildBatches' {..} =
    _salt `Prelude.hashWithSalt` ids

instance Prelude.NFData BatchGetBuildBatches where
  rnf BatchGetBuildBatches' {..} = Prelude.rnf ids

instance Data.ToHeaders BatchGetBuildBatches where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeBuild_20161006.BatchGetBuildBatches" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchGetBuildBatches where
  toJSON BatchGetBuildBatches' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ids" Data..= ids)]
      )

instance Data.ToPath BatchGetBuildBatches where
  toPath = Prelude.const "/"

instance Data.ToQuery BatchGetBuildBatches where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchGetBuildBatchesResponse' smart constructor.
data BatchGetBuildBatchesResponse = BatchGetBuildBatchesResponse'
  { -- | An array of @BuildBatch@ objects that represent the retrieved batch
    -- builds.
    buildBatches :: Prelude.Maybe [BuildBatch],
    -- | An array that contains the identifiers of any batch builds that are not
    -- found.
    buildBatchesNotFound :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetBuildBatchesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'buildBatches', 'batchGetBuildBatchesResponse_buildBatches' - An array of @BuildBatch@ objects that represent the retrieved batch
-- builds.
--
-- 'buildBatchesNotFound', 'batchGetBuildBatchesResponse_buildBatchesNotFound' - An array that contains the identifiers of any batch builds that are not
-- found.
--
-- 'httpStatus', 'batchGetBuildBatchesResponse_httpStatus' - The response's http status code.
newBatchGetBuildBatchesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchGetBuildBatchesResponse
newBatchGetBuildBatchesResponse pHttpStatus_ =
  BatchGetBuildBatchesResponse'
    { buildBatches =
        Prelude.Nothing,
      buildBatchesNotFound = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of @BuildBatch@ objects that represent the retrieved batch
-- builds.
batchGetBuildBatchesResponse_buildBatches :: Lens.Lens' BatchGetBuildBatchesResponse (Prelude.Maybe [BuildBatch])
batchGetBuildBatchesResponse_buildBatches = Lens.lens (\BatchGetBuildBatchesResponse' {buildBatches} -> buildBatches) (\s@BatchGetBuildBatchesResponse' {} a -> s {buildBatches = a} :: BatchGetBuildBatchesResponse) Prelude.. Lens.mapping Lens.coerced

-- | An array that contains the identifiers of any batch builds that are not
-- found.
batchGetBuildBatchesResponse_buildBatchesNotFound :: Lens.Lens' BatchGetBuildBatchesResponse (Prelude.Maybe [Prelude.Text])
batchGetBuildBatchesResponse_buildBatchesNotFound = Lens.lens (\BatchGetBuildBatchesResponse' {buildBatchesNotFound} -> buildBatchesNotFound) (\s@BatchGetBuildBatchesResponse' {} a -> s {buildBatchesNotFound = a} :: BatchGetBuildBatchesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchGetBuildBatchesResponse_httpStatus :: Lens.Lens' BatchGetBuildBatchesResponse Prelude.Int
batchGetBuildBatchesResponse_httpStatus = Lens.lens (\BatchGetBuildBatchesResponse' {httpStatus} -> httpStatus) (\s@BatchGetBuildBatchesResponse' {} a -> s {httpStatus = a} :: BatchGetBuildBatchesResponse)

instance Prelude.NFData BatchGetBuildBatchesResponse where
  rnf BatchGetBuildBatchesResponse' {..} =
    Prelude.rnf buildBatches
      `Prelude.seq` Prelude.rnf buildBatchesNotFound
      `Prelude.seq` Prelude.rnf httpStatus
