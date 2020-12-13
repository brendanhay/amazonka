{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.BatchDeleteBuilds
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes one or more builds.
module Network.AWS.CodeBuild.BatchDeleteBuilds
  ( -- * Creating a request
    BatchDeleteBuilds (..),
    mkBatchDeleteBuilds,

    -- ** Request lenses
    bdbIds,

    -- * Destructuring the response
    BatchDeleteBuildsResponse (..),
    mkBatchDeleteBuildsResponse,

    -- ** Response lenses
    bdbrsBuildsNotDeleted,
    bdbrsBuildsDeleted,
    bdbrsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkBatchDeleteBuilds' smart constructor.
newtype BatchDeleteBuilds = BatchDeleteBuilds'
  { -- | The IDs of the builds to delete.
    ids :: Lude.NonEmpty Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchDeleteBuilds' with the minimum fields required to make a request.
--
-- * 'ids' - The IDs of the builds to delete.
mkBatchDeleteBuilds ::
  -- | 'ids'
  Lude.NonEmpty Lude.Text ->
  BatchDeleteBuilds
mkBatchDeleteBuilds pIds_ = BatchDeleteBuilds' {ids = pIds_}

-- | The IDs of the builds to delete.
--
-- /Note:/ Consider using 'ids' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdbIds :: Lens.Lens' BatchDeleteBuilds (Lude.NonEmpty Lude.Text)
bdbIds = Lens.lens (ids :: BatchDeleteBuilds -> Lude.NonEmpty Lude.Text) (\s a -> s {ids = a} :: BatchDeleteBuilds)
{-# DEPRECATED bdbIds "Use generic-lens or generic-optics with 'ids' instead." #-}

instance Lude.AWSRequest BatchDeleteBuilds where
  type Rs BatchDeleteBuilds = BatchDeleteBuildsResponse
  request = Req.postJSON codeBuildService
  response =
    Res.receiveJSON
      ( \s h x ->
          BatchDeleteBuildsResponse'
            Lude.<$> (x Lude..?> "buildsNotDeleted" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "buildsDeleted")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BatchDeleteBuilds where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeBuild_20161006.BatchDeleteBuilds" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON BatchDeleteBuilds where
  toJSON BatchDeleteBuilds' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("ids" Lude..= ids)])

instance Lude.ToPath BatchDeleteBuilds where
  toPath = Lude.const "/"

instance Lude.ToQuery BatchDeleteBuilds where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkBatchDeleteBuildsResponse' smart constructor.
data BatchDeleteBuildsResponse = BatchDeleteBuildsResponse'
  { -- | Information about any builds that could not be successfully deleted.
    buildsNotDeleted :: Lude.Maybe [BuildNotDeleted],
    -- | The IDs of the builds that were successfully deleted.
    buildsDeleted :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchDeleteBuildsResponse' with the minimum fields required to make a request.
--
-- * 'buildsNotDeleted' - Information about any builds that could not be successfully deleted.
-- * 'buildsDeleted' - The IDs of the builds that were successfully deleted.
-- * 'responseStatus' - The response status code.
mkBatchDeleteBuildsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchDeleteBuildsResponse
mkBatchDeleteBuildsResponse pResponseStatus_ =
  BatchDeleteBuildsResponse'
    { buildsNotDeleted = Lude.Nothing,
      buildsDeleted = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about any builds that could not be successfully deleted.
--
-- /Note:/ Consider using 'buildsNotDeleted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdbrsBuildsNotDeleted :: Lens.Lens' BatchDeleteBuildsResponse (Lude.Maybe [BuildNotDeleted])
bdbrsBuildsNotDeleted = Lens.lens (buildsNotDeleted :: BatchDeleteBuildsResponse -> Lude.Maybe [BuildNotDeleted]) (\s a -> s {buildsNotDeleted = a} :: BatchDeleteBuildsResponse)
{-# DEPRECATED bdbrsBuildsNotDeleted "Use generic-lens or generic-optics with 'buildsNotDeleted' instead." #-}

-- | The IDs of the builds that were successfully deleted.
--
-- /Note:/ Consider using 'buildsDeleted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdbrsBuildsDeleted :: Lens.Lens' BatchDeleteBuildsResponse (Lude.Maybe (Lude.NonEmpty Lude.Text))
bdbrsBuildsDeleted = Lens.lens (buildsDeleted :: BatchDeleteBuildsResponse -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {buildsDeleted = a} :: BatchDeleteBuildsResponse)
{-# DEPRECATED bdbrsBuildsDeleted "Use generic-lens or generic-optics with 'buildsDeleted' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdbrsResponseStatus :: Lens.Lens' BatchDeleteBuildsResponse Lude.Int
bdbrsResponseStatus = Lens.lens (responseStatus :: BatchDeleteBuildsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchDeleteBuildsResponse)
{-# DEPRECATED bdbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
