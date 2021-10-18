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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRetryBuildBatch' smart constructor.
data RetryBuildBatch = RetryBuildBatch'
  { -- | A unique, case sensitive identifier you provide to ensure the
    -- idempotency of the @RetryBuildBatch@ request. The token is included in
    -- the @RetryBuildBatch@ request and is valid for five minutes. If you
    -- repeat the @RetryBuildBatch@ request with the same token, but change a
    -- parameter, CodeBuild returns a parameter mismatch error.
    idempotencyToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies the type of retry to perform.
    retryType :: Prelude.Maybe RetryBuildBatchType,
    -- | Specifies the identifier of the batch build to restart.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- parameter, CodeBuild returns a parameter mismatch error.
--
-- 'retryType', 'retryBuildBatch_retryType' - Specifies the type of retry to perform.
--
-- 'id', 'retryBuildBatch_id' - Specifies the identifier of the batch build to restart.
newRetryBuildBatch ::
  RetryBuildBatch
newRetryBuildBatch =
  RetryBuildBatch'
    { idempotencyToken =
        Prelude.Nothing,
      retryType = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | A unique, case sensitive identifier you provide to ensure the
-- idempotency of the @RetryBuildBatch@ request. The token is included in
-- the @RetryBuildBatch@ request and is valid for five minutes. If you
-- repeat the @RetryBuildBatch@ request with the same token, but change a
-- parameter, CodeBuild returns a parameter mismatch error.
retryBuildBatch_idempotencyToken :: Lens.Lens' RetryBuildBatch (Prelude.Maybe Prelude.Text)
retryBuildBatch_idempotencyToken = Lens.lens (\RetryBuildBatch' {idempotencyToken} -> idempotencyToken) (\s@RetryBuildBatch' {} a -> s {idempotencyToken = a} :: RetryBuildBatch)

-- | Specifies the type of retry to perform.
retryBuildBatch_retryType :: Lens.Lens' RetryBuildBatch (Prelude.Maybe RetryBuildBatchType)
retryBuildBatch_retryType = Lens.lens (\RetryBuildBatch' {retryType} -> retryType) (\s@RetryBuildBatch' {} a -> s {retryType = a} :: RetryBuildBatch)

-- | Specifies the identifier of the batch build to restart.
retryBuildBatch_id :: Lens.Lens' RetryBuildBatch (Prelude.Maybe Prelude.Text)
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
            Prelude.<$> (x Core..?> "buildBatch")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RetryBuildBatch

instance Prelude.NFData RetryBuildBatch

instance Core.ToHeaders RetryBuildBatch where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeBuild_20161006.RetryBuildBatch" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON RetryBuildBatch where
  toJSON RetryBuildBatch' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("idempotencyToken" Core..=)
              Prelude.<$> idempotencyToken,
            ("retryType" Core..=) Prelude.<$> retryType,
            ("id" Core..=) Prelude.<$> id
          ]
      )

instance Core.ToPath RetryBuildBatch where
  toPath = Prelude.const "/"

instance Core.ToQuery RetryBuildBatch where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRetryBuildBatchResponse' smart constructor.
data RetryBuildBatchResponse = RetryBuildBatchResponse'
  { buildBatch :: Prelude.Maybe BuildBatch,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  RetryBuildBatchResponse
newRetryBuildBatchResponse pHttpStatus_ =
  RetryBuildBatchResponse'
    { buildBatch =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
retryBuildBatchResponse_buildBatch :: Lens.Lens' RetryBuildBatchResponse (Prelude.Maybe BuildBatch)
retryBuildBatchResponse_buildBatch = Lens.lens (\RetryBuildBatchResponse' {buildBatch} -> buildBatch) (\s@RetryBuildBatchResponse' {} a -> s {buildBatch = a} :: RetryBuildBatchResponse)

-- | The response's http status code.
retryBuildBatchResponse_httpStatus :: Lens.Lens' RetryBuildBatchResponse Prelude.Int
retryBuildBatchResponse_httpStatus = Lens.lens (\RetryBuildBatchResponse' {httpStatus} -> httpStatus) (\s@RetryBuildBatchResponse' {} a -> s {httpStatus = a} :: RetryBuildBatchResponse)

instance Prelude.NFData RetryBuildBatchResponse
