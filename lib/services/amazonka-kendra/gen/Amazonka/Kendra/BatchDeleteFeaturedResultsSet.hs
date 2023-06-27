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
-- Module      : Amazonka.Kendra.BatchDeleteFeaturedResultsSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes one or more sets of featured results. Features results are
-- placed above all other results for certain queries. If there\'s an exact
-- match of a query, then one or more specific documents are featured in
-- the search results.
module Amazonka.Kendra.BatchDeleteFeaturedResultsSet
  ( -- * Creating a Request
    BatchDeleteFeaturedResultsSet (..),
    newBatchDeleteFeaturedResultsSet,

    -- * Request Lenses
    batchDeleteFeaturedResultsSet_indexId,
    batchDeleteFeaturedResultsSet_featuredResultsSetIds,

    -- * Destructuring the Response
    BatchDeleteFeaturedResultsSetResponse (..),
    newBatchDeleteFeaturedResultsSetResponse,

    -- * Response Lenses
    batchDeleteFeaturedResultsSetResponse_httpStatus,
    batchDeleteFeaturedResultsSetResponse_errors,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchDeleteFeaturedResultsSet' smart constructor.
data BatchDeleteFeaturedResultsSet = BatchDeleteFeaturedResultsSet'
  { -- | The identifier of the index used for featuring results.
    indexId :: Prelude.Text,
    -- | The identifiers of the featured results sets that you want to delete.
    featuredResultsSetIds :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDeleteFeaturedResultsSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'indexId', 'batchDeleteFeaturedResultsSet_indexId' - The identifier of the index used for featuring results.
--
-- 'featuredResultsSetIds', 'batchDeleteFeaturedResultsSet_featuredResultsSetIds' - The identifiers of the featured results sets that you want to delete.
newBatchDeleteFeaturedResultsSet ::
  -- | 'indexId'
  Prelude.Text ->
  -- | 'featuredResultsSetIds'
  Prelude.NonEmpty Prelude.Text ->
  BatchDeleteFeaturedResultsSet
newBatchDeleteFeaturedResultsSet
  pIndexId_
  pFeaturedResultsSetIds_ =
    BatchDeleteFeaturedResultsSet'
      { indexId = pIndexId_,
        featuredResultsSetIds =
          Lens.coerced
            Lens.# pFeaturedResultsSetIds_
      }

-- | The identifier of the index used for featuring results.
batchDeleteFeaturedResultsSet_indexId :: Lens.Lens' BatchDeleteFeaturedResultsSet Prelude.Text
batchDeleteFeaturedResultsSet_indexId = Lens.lens (\BatchDeleteFeaturedResultsSet' {indexId} -> indexId) (\s@BatchDeleteFeaturedResultsSet' {} a -> s {indexId = a} :: BatchDeleteFeaturedResultsSet)

-- | The identifiers of the featured results sets that you want to delete.
batchDeleteFeaturedResultsSet_featuredResultsSetIds :: Lens.Lens' BatchDeleteFeaturedResultsSet (Prelude.NonEmpty Prelude.Text)
batchDeleteFeaturedResultsSet_featuredResultsSetIds = Lens.lens (\BatchDeleteFeaturedResultsSet' {featuredResultsSetIds} -> featuredResultsSetIds) (\s@BatchDeleteFeaturedResultsSet' {} a -> s {featuredResultsSetIds = a} :: BatchDeleteFeaturedResultsSet) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    BatchDeleteFeaturedResultsSet
  where
  type
    AWSResponse BatchDeleteFeaturedResultsSet =
      BatchDeleteFeaturedResultsSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchDeleteFeaturedResultsSetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "Errors" Core..!@ Prelude.mempty)
      )

instance
  Prelude.Hashable
    BatchDeleteFeaturedResultsSet
  where
  hashWithSalt _salt BatchDeleteFeaturedResultsSet' {..} =
    _salt
      `Prelude.hashWithSalt` indexId
      `Prelude.hashWithSalt` featuredResultsSetIds

instance Prelude.NFData BatchDeleteFeaturedResultsSet where
  rnf BatchDeleteFeaturedResultsSet' {..} =
    Prelude.rnf indexId
      `Prelude.seq` Prelude.rnf featuredResultsSetIds

instance Data.ToHeaders BatchDeleteFeaturedResultsSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSKendraFrontendService.BatchDeleteFeaturedResultsSet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchDeleteFeaturedResultsSet where
  toJSON BatchDeleteFeaturedResultsSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("IndexId" Data..= indexId),
            Prelude.Just
              ( "FeaturedResultsSetIds"
                  Data..= featuredResultsSetIds
              )
          ]
      )

instance Data.ToPath BatchDeleteFeaturedResultsSet where
  toPath = Prelude.const "/"

instance Data.ToQuery BatchDeleteFeaturedResultsSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchDeleteFeaturedResultsSetResponse' smart constructor.
data BatchDeleteFeaturedResultsSetResponse = BatchDeleteFeaturedResultsSetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The list of errors for the featured results set IDs, explaining why they
    -- couldn\'t be removed from the index.
    errors :: [BatchDeleteFeaturedResultsSetError]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDeleteFeaturedResultsSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'batchDeleteFeaturedResultsSetResponse_httpStatus' - The response's http status code.
--
-- 'errors', 'batchDeleteFeaturedResultsSetResponse_errors' - The list of errors for the featured results set IDs, explaining why they
-- couldn\'t be removed from the index.
newBatchDeleteFeaturedResultsSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchDeleteFeaturedResultsSetResponse
newBatchDeleteFeaturedResultsSetResponse pHttpStatus_ =
  BatchDeleteFeaturedResultsSetResponse'
    { httpStatus =
        pHttpStatus_,
      errors = Prelude.mempty
    }

-- | The response's http status code.
batchDeleteFeaturedResultsSetResponse_httpStatus :: Lens.Lens' BatchDeleteFeaturedResultsSetResponse Prelude.Int
batchDeleteFeaturedResultsSetResponse_httpStatus = Lens.lens (\BatchDeleteFeaturedResultsSetResponse' {httpStatus} -> httpStatus) (\s@BatchDeleteFeaturedResultsSetResponse' {} a -> s {httpStatus = a} :: BatchDeleteFeaturedResultsSetResponse)

-- | The list of errors for the featured results set IDs, explaining why they
-- couldn\'t be removed from the index.
batchDeleteFeaturedResultsSetResponse_errors :: Lens.Lens' BatchDeleteFeaturedResultsSetResponse [BatchDeleteFeaturedResultsSetError]
batchDeleteFeaturedResultsSetResponse_errors = Lens.lens (\BatchDeleteFeaturedResultsSetResponse' {errors} -> errors) (\s@BatchDeleteFeaturedResultsSetResponse' {} a -> s {errors = a} :: BatchDeleteFeaturedResultsSetResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    BatchDeleteFeaturedResultsSetResponse
  where
  rnf BatchDeleteFeaturedResultsSetResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf errors
