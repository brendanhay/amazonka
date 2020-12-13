{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.RetryBuildBatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restarts a failed batch build. Only batch builds that have failed can be retried.
module Network.AWS.CodeBuild.RetryBuildBatch
  ( -- * Creating a request
    RetryBuildBatch (..),
    mkRetryBuildBatch,

    -- ** Request lenses
    rbbIdempotencyToken,
    rbbId,
    rbbRetryType,

    -- * Destructuring the response
    RetryBuildBatchResponse (..),
    mkRetryBuildBatchResponse,

    -- ** Response lenses
    rbbrsBuildBatch,
    rbbrsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRetryBuildBatch' smart constructor.
data RetryBuildBatch = RetryBuildBatch'
  { -- | A unique, case sensitive identifier you provide to ensure the idempotency of the @RetryBuildBatch@ request. The token is included in the @RetryBuildBatch@ request and is valid for five minutes. If you repeat the @RetryBuildBatch@ request with the same token, but change a parameter, AWS CodeBuild returns a parameter mismatch error.
    idempotencyToken :: Lude.Maybe Lude.Text,
    -- | Specifies the identifier of the batch build to restart.
    id :: Lude.Maybe Lude.Text,
    -- | Specifies the type of retry to perform.
    retryType :: Lude.Maybe RetryBuildBatchType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RetryBuildBatch' with the minimum fields required to make a request.
--
-- * 'idempotencyToken' - A unique, case sensitive identifier you provide to ensure the idempotency of the @RetryBuildBatch@ request. The token is included in the @RetryBuildBatch@ request and is valid for five minutes. If you repeat the @RetryBuildBatch@ request with the same token, but change a parameter, AWS CodeBuild returns a parameter mismatch error.
-- * 'id' - Specifies the identifier of the batch build to restart.
-- * 'retryType' - Specifies the type of retry to perform.
mkRetryBuildBatch ::
  RetryBuildBatch
mkRetryBuildBatch =
  RetryBuildBatch'
    { idempotencyToken = Lude.Nothing,
      id = Lude.Nothing,
      retryType = Lude.Nothing
    }

-- | A unique, case sensitive identifier you provide to ensure the idempotency of the @RetryBuildBatch@ request. The token is included in the @RetryBuildBatch@ request and is valid for five minutes. If you repeat the @RetryBuildBatch@ request with the same token, but change a parameter, AWS CodeBuild returns a parameter mismatch error.
--
-- /Note:/ Consider using 'idempotencyToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rbbIdempotencyToken :: Lens.Lens' RetryBuildBatch (Lude.Maybe Lude.Text)
rbbIdempotencyToken = Lens.lens (idempotencyToken :: RetryBuildBatch -> Lude.Maybe Lude.Text) (\s a -> s {idempotencyToken = a} :: RetryBuildBatch)
{-# DEPRECATED rbbIdempotencyToken "Use generic-lens or generic-optics with 'idempotencyToken' instead." #-}

-- | Specifies the identifier of the batch build to restart.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rbbId :: Lens.Lens' RetryBuildBatch (Lude.Maybe Lude.Text)
rbbId = Lens.lens (id :: RetryBuildBatch -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: RetryBuildBatch)
{-# DEPRECATED rbbId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Specifies the type of retry to perform.
--
-- /Note:/ Consider using 'retryType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rbbRetryType :: Lens.Lens' RetryBuildBatch (Lude.Maybe RetryBuildBatchType)
rbbRetryType = Lens.lens (retryType :: RetryBuildBatch -> Lude.Maybe RetryBuildBatchType) (\s a -> s {retryType = a} :: RetryBuildBatch)
{-# DEPRECATED rbbRetryType "Use generic-lens or generic-optics with 'retryType' instead." #-}

instance Lude.AWSRequest RetryBuildBatch where
  type Rs RetryBuildBatch = RetryBuildBatchResponse
  request = Req.postJSON codeBuildService
  response =
    Res.receiveJSON
      ( \s h x ->
          RetryBuildBatchResponse'
            Lude.<$> (x Lude..?> "buildBatch") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RetryBuildBatch where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeBuild_20161006.RetryBuildBatch" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RetryBuildBatch where
  toJSON RetryBuildBatch' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("idempotencyToken" Lude..=) Lude.<$> idempotencyToken,
            ("id" Lude..=) Lude.<$> id,
            ("retryType" Lude..=) Lude.<$> retryType
          ]
      )

instance Lude.ToPath RetryBuildBatch where
  toPath = Lude.const "/"

instance Lude.ToQuery RetryBuildBatch where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRetryBuildBatchResponse' smart constructor.
data RetryBuildBatchResponse = RetryBuildBatchResponse'
  { buildBatch :: Lude.Maybe BuildBatch,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RetryBuildBatchResponse' with the minimum fields required to make a request.
--
-- * 'buildBatch' -
-- * 'responseStatus' - The response status code.
mkRetryBuildBatchResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RetryBuildBatchResponse
mkRetryBuildBatchResponse pResponseStatus_ =
  RetryBuildBatchResponse'
    { buildBatch = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'buildBatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rbbrsBuildBatch :: Lens.Lens' RetryBuildBatchResponse (Lude.Maybe BuildBatch)
rbbrsBuildBatch = Lens.lens (buildBatch :: RetryBuildBatchResponse -> Lude.Maybe BuildBatch) (\s a -> s {buildBatch = a} :: RetryBuildBatchResponse)
{-# DEPRECATED rbbrsBuildBatch "Use generic-lens or generic-optics with 'buildBatch' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rbbrsResponseStatus :: Lens.Lens' RetryBuildBatchResponse Lude.Int
rbbrsResponseStatus = Lens.lens (responseStatus :: RetryBuildBatchResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RetryBuildBatchResponse)
{-# DEPRECATED rbbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
