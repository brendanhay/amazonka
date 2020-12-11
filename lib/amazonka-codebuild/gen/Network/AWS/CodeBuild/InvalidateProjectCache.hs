{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.InvalidateProjectCache
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets the cache for a project.
module Network.AWS.CodeBuild.InvalidateProjectCache
  ( -- * Creating a request
    InvalidateProjectCache (..),
    mkInvalidateProjectCache,

    -- ** Request lenses
    ipcProjectName,

    -- * Destructuring the response
    InvalidateProjectCacheResponse (..),
    mkInvalidateProjectCacheResponse,

    -- ** Response lenses
    ipcrsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkInvalidateProjectCache' smart constructor.
newtype InvalidateProjectCache = InvalidateProjectCache'
  { projectName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InvalidateProjectCache' with the minimum fields required to make a request.
--
-- * 'projectName' - The name of the AWS CodeBuild build project that the cache is reset for.
mkInvalidateProjectCache ::
  -- | 'projectName'
  Lude.Text ->
  InvalidateProjectCache
mkInvalidateProjectCache pProjectName_ =
  InvalidateProjectCache' {projectName = pProjectName_}

-- | The name of the AWS CodeBuild build project that the cache is reset for.
--
-- /Note:/ Consider using 'projectName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipcProjectName :: Lens.Lens' InvalidateProjectCache Lude.Text
ipcProjectName = Lens.lens (projectName :: InvalidateProjectCache -> Lude.Text) (\s a -> s {projectName = a} :: InvalidateProjectCache)
{-# DEPRECATED ipcProjectName "Use generic-lens or generic-optics with 'projectName' instead." #-}

instance Lude.AWSRequest InvalidateProjectCache where
  type Rs InvalidateProjectCache = InvalidateProjectCacheResponse
  request = Req.postJSON codeBuildService
  response =
    Res.receiveEmpty
      ( \s h x ->
          InvalidateProjectCacheResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders InvalidateProjectCache where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeBuild_20161006.InvalidateProjectCache" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON InvalidateProjectCache where
  toJSON InvalidateProjectCache' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("projectName" Lude..= projectName)])

instance Lude.ToPath InvalidateProjectCache where
  toPath = Lude.const "/"

instance Lude.ToQuery InvalidateProjectCache where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkInvalidateProjectCacheResponse' smart constructor.
newtype InvalidateProjectCacheResponse = InvalidateProjectCacheResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InvalidateProjectCacheResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkInvalidateProjectCacheResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  InvalidateProjectCacheResponse
mkInvalidateProjectCacheResponse pResponseStatus_ =
  InvalidateProjectCacheResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipcrsResponseStatus :: Lens.Lens' InvalidateProjectCacheResponse Lude.Int
ipcrsResponseStatus = Lens.lens (responseStatus :: InvalidateProjectCacheResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: InvalidateProjectCacheResponse)
{-# DEPRECATED ipcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
