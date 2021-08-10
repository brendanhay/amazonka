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
-- Module      : Network.AWS.CodeBuild.BatchGetBuildBatches
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about one or more batch builds.
module Network.AWS.CodeBuild.BatchGetBuildBatches
  ( -- * Creating a Request
    BatchGetBuildBatches (..),
    newBatchGetBuildBatches,

    -- * Request Lenses
    batchGetBuildBatches_ids,

    -- * Destructuring the Response
    BatchGetBuildBatchesResponse (..),
    newBatchGetBuildBatchesResponse,

    -- * Response Lenses
    batchGetBuildBatchesResponse_buildBatchesNotFound,
    batchGetBuildBatchesResponse_buildBatches,
    batchGetBuildBatchesResponse_httpStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
batchGetBuildBatches_ids = Lens.lens (\BatchGetBuildBatches' {ids} -> ids) (\s@BatchGetBuildBatches' {} a -> s {ids = a} :: BatchGetBuildBatches) Prelude.. Lens._Coerce

instance Core.AWSRequest BatchGetBuildBatches where
  type
    AWSResponse BatchGetBuildBatches =
      BatchGetBuildBatchesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetBuildBatchesResponse'
            Prelude.<$> ( x Core..?> "buildBatchesNotFound"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "buildBatches" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchGetBuildBatches

instance Prelude.NFData BatchGetBuildBatches

instance Core.ToHeaders BatchGetBuildBatches where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeBuild_20161006.BatchGetBuildBatches" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON BatchGetBuildBatches where
  toJSON BatchGetBuildBatches' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("ids" Core..= ids)]
      )

instance Core.ToPath BatchGetBuildBatches where
  toPath = Prelude.const "/"

instance Core.ToQuery BatchGetBuildBatches where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchGetBuildBatchesResponse' smart constructor.
data BatchGetBuildBatchesResponse = BatchGetBuildBatchesResponse'
  { -- | An array that contains the identifiers of any batch builds that are not
    -- found.
    buildBatchesNotFound :: Prelude.Maybe [Prelude.Text],
    -- | An array of @BuildBatch@ objects that represent the retrieved batch
    -- builds.
    buildBatches :: Prelude.Maybe [BuildBatch],
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
-- 'buildBatchesNotFound', 'batchGetBuildBatchesResponse_buildBatchesNotFound' - An array that contains the identifiers of any batch builds that are not
-- found.
--
-- 'buildBatches', 'batchGetBuildBatchesResponse_buildBatches' - An array of @BuildBatch@ objects that represent the retrieved batch
-- builds.
--
-- 'httpStatus', 'batchGetBuildBatchesResponse_httpStatus' - The response's http status code.
newBatchGetBuildBatchesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchGetBuildBatchesResponse
newBatchGetBuildBatchesResponse pHttpStatus_ =
  BatchGetBuildBatchesResponse'
    { buildBatchesNotFound =
        Prelude.Nothing,
      buildBatches = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array that contains the identifiers of any batch builds that are not
-- found.
batchGetBuildBatchesResponse_buildBatchesNotFound :: Lens.Lens' BatchGetBuildBatchesResponse (Prelude.Maybe [Prelude.Text])
batchGetBuildBatchesResponse_buildBatchesNotFound = Lens.lens (\BatchGetBuildBatchesResponse' {buildBatchesNotFound} -> buildBatchesNotFound) (\s@BatchGetBuildBatchesResponse' {} a -> s {buildBatchesNotFound = a} :: BatchGetBuildBatchesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | An array of @BuildBatch@ objects that represent the retrieved batch
-- builds.
batchGetBuildBatchesResponse_buildBatches :: Lens.Lens' BatchGetBuildBatchesResponse (Prelude.Maybe [BuildBatch])
batchGetBuildBatchesResponse_buildBatches = Lens.lens (\BatchGetBuildBatchesResponse' {buildBatches} -> buildBatches) (\s@BatchGetBuildBatchesResponse' {} a -> s {buildBatches = a} :: BatchGetBuildBatchesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
batchGetBuildBatchesResponse_httpStatus :: Lens.Lens' BatchGetBuildBatchesResponse Prelude.Int
batchGetBuildBatchesResponse_httpStatus = Lens.lens (\BatchGetBuildBatchesResponse' {httpStatus} -> httpStatus) (\s@BatchGetBuildBatchesResponse' {} a -> s {httpStatus = a} :: BatchGetBuildBatchesResponse)

instance Prelude.NFData BatchGetBuildBatchesResponse
