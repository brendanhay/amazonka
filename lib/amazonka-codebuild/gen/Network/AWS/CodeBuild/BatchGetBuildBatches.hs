{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.BatchGetBuildBatches
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about one or more batch builds.
module Network.AWS.CodeBuild.BatchGetBuildBatches
  ( -- * Creating a request
    BatchGetBuildBatches (..),
    mkBatchGetBuildBatches,

    -- ** Request lenses
    bgbbIds,

    -- * Destructuring the response
    BatchGetBuildBatchesResponse (..),
    mkBatchGetBuildBatchesResponse,

    -- ** Response lenses
    bgbbrsBuildBatches,
    bgbbrsBuildBatchesNotFound,
    bgbbrsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkBatchGetBuildBatches' smart constructor.
newtype BatchGetBuildBatches = BatchGetBuildBatches'
  { ids ::
      [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchGetBuildBatches' with the minimum fields required to make a request.
--
-- * 'ids' - An array that contains the batch build identifiers to retrieve.
mkBatchGetBuildBatches ::
  BatchGetBuildBatches
mkBatchGetBuildBatches = BatchGetBuildBatches' {ids = Lude.mempty}

-- | An array that contains the batch build identifiers to retrieve.
--
-- /Note:/ Consider using 'ids' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgbbIds :: Lens.Lens' BatchGetBuildBatches [Lude.Text]
bgbbIds = Lens.lens (ids :: BatchGetBuildBatches -> [Lude.Text]) (\s a -> s {ids = a} :: BatchGetBuildBatches)
{-# DEPRECATED bgbbIds "Use generic-lens or generic-optics with 'ids' instead." #-}

instance Lude.AWSRequest BatchGetBuildBatches where
  type Rs BatchGetBuildBatches = BatchGetBuildBatchesResponse
  request = Req.postJSON codeBuildService
  response =
    Res.receiveJSON
      ( \s h x ->
          BatchGetBuildBatchesResponse'
            Lude.<$> (x Lude..?> "buildBatches" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "buildBatchesNotFound" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BatchGetBuildBatches where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeBuild_20161006.BatchGetBuildBatches" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON BatchGetBuildBatches where
  toJSON BatchGetBuildBatches' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("ids" Lude..= ids)])

instance Lude.ToPath BatchGetBuildBatches where
  toPath = Lude.const "/"

instance Lude.ToQuery BatchGetBuildBatches where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkBatchGetBuildBatchesResponse' smart constructor.
data BatchGetBuildBatchesResponse = BatchGetBuildBatchesResponse'
  { buildBatches ::
      Lude.Maybe [BuildBatch],
    buildBatchesNotFound ::
      Lude.Maybe [Lude.Text],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchGetBuildBatchesResponse' with the minimum fields required to make a request.
--
-- * 'buildBatches' - An array of @BuildBatch@ objects that represent the retrieved batch builds.
-- * 'buildBatchesNotFound' - An array that contains the identifiers of any batch builds that are not found.
-- * 'responseStatus' - The response status code.
mkBatchGetBuildBatchesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchGetBuildBatchesResponse
mkBatchGetBuildBatchesResponse pResponseStatus_ =
  BatchGetBuildBatchesResponse'
    { buildBatches = Lude.Nothing,
      buildBatchesNotFound = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of @BuildBatch@ objects that represent the retrieved batch builds.
--
-- /Note:/ Consider using 'buildBatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgbbrsBuildBatches :: Lens.Lens' BatchGetBuildBatchesResponse (Lude.Maybe [BuildBatch])
bgbbrsBuildBatches = Lens.lens (buildBatches :: BatchGetBuildBatchesResponse -> Lude.Maybe [BuildBatch]) (\s a -> s {buildBatches = a} :: BatchGetBuildBatchesResponse)
{-# DEPRECATED bgbbrsBuildBatches "Use generic-lens or generic-optics with 'buildBatches' instead." #-}

-- | An array that contains the identifiers of any batch builds that are not found.
--
-- /Note:/ Consider using 'buildBatchesNotFound' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgbbrsBuildBatchesNotFound :: Lens.Lens' BatchGetBuildBatchesResponse (Lude.Maybe [Lude.Text])
bgbbrsBuildBatchesNotFound = Lens.lens (buildBatchesNotFound :: BatchGetBuildBatchesResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {buildBatchesNotFound = a} :: BatchGetBuildBatchesResponse)
{-# DEPRECATED bgbbrsBuildBatchesNotFound "Use generic-lens or generic-optics with 'buildBatchesNotFound' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgbbrsResponseStatus :: Lens.Lens' BatchGetBuildBatchesResponse Lude.Int
bgbbrsResponseStatus = Lens.lens (responseStatus :: BatchGetBuildBatchesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchGetBuildBatchesResponse)
{-# DEPRECATED bgbbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
