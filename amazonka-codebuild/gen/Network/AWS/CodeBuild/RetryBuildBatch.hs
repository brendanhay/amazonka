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
-- Module      : Network.AWS.CodeBuild.RetryBuildBatch
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restarts a failed batch build. Only batch builds that have failed can be
-- retried.
module Network.AWS.CodeBuild.RetryBuildBatch
  ( -- * Creating a Request
    RetryBuildBatch (..),
    newRetryBuildBatch,

    -- * Request Lenses
    retryBuildBatch_idempotencyToken,
    retryBuildBatch_retryType,
    retryBuildBatch_id,

    -- * Destructuring the Response
    RetryBuildBatchResponse (..),
    newRetryBuildBatchResponse,

    -- * Response Lenses
    retryBuildBatchResponse_buildBatch,
    retryBuildBatchResponse_httpStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRetryBuildBatch' smart constructor.
data RetryBuildBatch = RetryBuildBatch'
  { -- | A unique, case sensitive identifier you provide to ensure the
    -- idempotency of the @RetryBuildBatch@ request. The token is included in
    -- the @RetryBuildBatch@ request and is valid for five minutes. If you
    -- repeat the @RetryBuildBatch@ request with the same token, but change a
    -- parameter, AWS CodeBuild returns a parameter mismatch error.
    idempotencyToken :: Core.Maybe Core.Text,
    -- | Specifies the type of retry to perform.
    retryType :: Core.Maybe RetryBuildBatchType,
    -- | Specifies the identifier of the batch build to restart.
    id :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RetryBuildBatch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'idempotencyToken', 'retryBuildBatch_idempotencyToken' - A unique, case sensitive identifier you provide to ensure the
-- idempotency of the @RetryBuildBatch@ request. The token is included in
-- the @RetryBuildBatch@ request and is valid for five minutes. If you
-- repeat the @RetryBuildBatch@ request with the same token, but change a
-- parameter, AWS CodeBuild returns a parameter mismatch error.
--
-- 'retryType', 'retryBuildBatch_retryType' - Specifies the type of retry to perform.
--
-- 'id', 'retryBuildBatch_id' - Specifies the identifier of the batch build to restart.
newRetryBuildBatch ::
  RetryBuildBatch
newRetryBuildBatch =
  RetryBuildBatch'
    { idempotencyToken = Core.Nothing,
      retryType = Core.Nothing,
      id = Core.Nothing
    }

-- | A unique, case sensitive identifier you provide to ensure the
-- idempotency of the @RetryBuildBatch@ request. The token is included in
-- the @RetryBuildBatch@ request and is valid for five minutes. If you
-- repeat the @RetryBuildBatch@ request with the same token, but change a
-- parameter, AWS CodeBuild returns a parameter mismatch error.
retryBuildBatch_idempotencyToken :: Lens.Lens' RetryBuildBatch (Core.Maybe Core.Text)
retryBuildBatch_idempotencyToken = Lens.lens (\RetryBuildBatch' {idempotencyToken} -> idempotencyToken) (\s@RetryBuildBatch' {} a -> s {idempotencyToken = a} :: RetryBuildBatch)

-- | Specifies the type of retry to perform.
retryBuildBatch_retryType :: Lens.Lens' RetryBuildBatch (Core.Maybe RetryBuildBatchType)
retryBuildBatch_retryType = Lens.lens (\RetryBuildBatch' {retryType} -> retryType) (\s@RetryBuildBatch' {} a -> s {retryType = a} :: RetryBuildBatch)

-- | Specifies the identifier of the batch build to restart.
retryBuildBatch_id :: Lens.Lens' RetryBuildBatch (Core.Maybe Core.Text)
retryBuildBatch_id = Lens.lens (\RetryBuildBatch' {id} -> id) (\s@RetryBuildBatch' {} a -> s {id = a} :: RetryBuildBatch)

instance Core.AWSRequest RetryBuildBatch where
  type
    AWSResponse RetryBuildBatch =
      RetryBuildBatchResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RetryBuildBatchResponse'
            Core.<$> (x Core..?> "buildBatch")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable RetryBuildBatch

instance Core.NFData RetryBuildBatch

instance Core.ToHeaders RetryBuildBatch where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeBuild_20161006.RetryBuildBatch" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON RetryBuildBatch where
  toJSON RetryBuildBatch' {..} =
    Core.object
      ( Core.catMaybes
          [ ("idempotencyToken" Core..=)
              Core.<$> idempotencyToken,
            ("retryType" Core..=) Core.<$> retryType,
            ("id" Core..=) Core.<$> id
          ]
      )

instance Core.ToPath RetryBuildBatch where
  toPath = Core.const "/"

instance Core.ToQuery RetryBuildBatch where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newRetryBuildBatchResponse' smart constructor.
data RetryBuildBatchResponse = RetryBuildBatchResponse'
  { buildBatch :: Core.Maybe BuildBatch,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RetryBuildBatchResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'buildBatch', 'retryBuildBatchResponse_buildBatch' - Undocumented member.
--
-- 'httpStatus', 'retryBuildBatchResponse_httpStatus' - The response's http status code.
newRetryBuildBatchResponse ::
  -- | 'httpStatus'
  Core.Int ->
  RetryBuildBatchResponse
newRetryBuildBatchResponse pHttpStatus_ =
  RetryBuildBatchResponse'
    { buildBatch = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
retryBuildBatchResponse_buildBatch :: Lens.Lens' RetryBuildBatchResponse (Core.Maybe BuildBatch)
retryBuildBatchResponse_buildBatch = Lens.lens (\RetryBuildBatchResponse' {buildBatch} -> buildBatch) (\s@RetryBuildBatchResponse' {} a -> s {buildBatch = a} :: RetryBuildBatchResponse)

-- | The response's http status code.
retryBuildBatchResponse_httpStatus :: Lens.Lens' RetryBuildBatchResponse Core.Int
retryBuildBatchResponse_httpStatus = Lens.lens (\RetryBuildBatchResponse' {httpStatus} -> httpStatus) (\s@RetryBuildBatchResponse' {} a -> s {httpStatus = a} :: RetryBuildBatchResponse)

instance Core.NFData RetryBuildBatchResponse
