{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.BatchGetBuilds
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about one or more builds.
module Network.AWS.CodeBuild.BatchGetBuilds
  ( -- * Creating a request
    BatchGetBuilds (..),
    mkBatchGetBuilds,

    -- ** Request lenses
    bgbIds,

    -- * Destructuring the response
    BatchGetBuildsResponse (..),
    mkBatchGetBuildsResponse,

    -- ** Response lenses
    bgbrsBuilds,
    bgbrsBuildsNotFound,
    bgbrsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkBatchGetBuilds' smart constructor.
newtype BatchGetBuilds = BatchGetBuilds'
  { -- | The IDs of the builds.
    ids :: Lude.NonEmpty Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchGetBuilds' with the minimum fields required to make a request.
--
-- * 'ids' - The IDs of the builds.
mkBatchGetBuilds ::
  -- | 'ids'
  Lude.NonEmpty Lude.Text ->
  BatchGetBuilds
mkBatchGetBuilds pIds_ = BatchGetBuilds' {ids = pIds_}

-- | The IDs of the builds.
--
-- /Note:/ Consider using 'ids' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgbIds :: Lens.Lens' BatchGetBuilds (Lude.NonEmpty Lude.Text)
bgbIds = Lens.lens (ids :: BatchGetBuilds -> Lude.NonEmpty Lude.Text) (\s a -> s {ids = a} :: BatchGetBuilds)
{-# DEPRECATED bgbIds "Use generic-lens or generic-optics with 'ids' instead." #-}

instance Lude.AWSRequest BatchGetBuilds where
  type Rs BatchGetBuilds = BatchGetBuildsResponse
  request = Req.postJSON codeBuildService
  response =
    Res.receiveJSON
      ( \s h x ->
          BatchGetBuildsResponse'
            Lude.<$> (x Lude..?> "builds" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "buildsNotFound")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BatchGetBuilds where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeBuild_20161006.BatchGetBuilds" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON BatchGetBuilds where
  toJSON BatchGetBuilds' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("ids" Lude..= ids)])

instance Lude.ToPath BatchGetBuilds where
  toPath = Lude.const "/"

instance Lude.ToQuery BatchGetBuilds where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkBatchGetBuildsResponse' smart constructor.
data BatchGetBuildsResponse = BatchGetBuildsResponse'
  { -- | Information about the requested builds.
    builds :: Lude.Maybe [Build],
    -- | The IDs of builds for which information could not be found.
    buildsNotFound :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchGetBuildsResponse' with the minimum fields required to make a request.
--
-- * 'builds' - Information about the requested builds.
-- * 'buildsNotFound' - The IDs of builds for which information could not be found.
-- * 'responseStatus' - The response status code.
mkBatchGetBuildsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchGetBuildsResponse
mkBatchGetBuildsResponse pResponseStatus_ =
  BatchGetBuildsResponse'
    { builds = Lude.Nothing,
      buildsNotFound = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the requested builds.
--
-- /Note:/ Consider using 'builds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgbrsBuilds :: Lens.Lens' BatchGetBuildsResponse (Lude.Maybe [Build])
bgbrsBuilds = Lens.lens (builds :: BatchGetBuildsResponse -> Lude.Maybe [Build]) (\s a -> s {builds = a} :: BatchGetBuildsResponse)
{-# DEPRECATED bgbrsBuilds "Use generic-lens or generic-optics with 'builds' instead." #-}

-- | The IDs of builds for which information could not be found.
--
-- /Note:/ Consider using 'buildsNotFound' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgbrsBuildsNotFound :: Lens.Lens' BatchGetBuildsResponse (Lude.Maybe (Lude.NonEmpty Lude.Text))
bgbrsBuildsNotFound = Lens.lens (buildsNotFound :: BatchGetBuildsResponse -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {buildsNotFound = a} :: BatchGetBuildsResponse)
{-# DEPRECATED bgbrsBuildsNotFound "Use generic-lens or generic-optics with 'buildsNotFound' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgbrsResponseStatus :: Lens.Lens' BatchGetBuildsResponse Lude.Int
bgbrsResponseStatus = Lens.lens (responseStatus :: BatchGetBuildsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchGetBuildsResponse)
{-# DEPRECATED bgbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
