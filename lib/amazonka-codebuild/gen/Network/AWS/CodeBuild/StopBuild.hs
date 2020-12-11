{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.StopBuild
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attempts to stop running a build.
module Network.AWS.CodeBuild.StopBuild
  ( -- * Creating a request
    StopBuild (..),
    mkStopBuild,

    -- ** Request lenses
    sbId,

    -- * Destructuring the response
    StopBuildResponse (..),
    mkStopBuildResponse,

    -- ** Response lenses
    sbrsBuild,
    sbrsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStopBuild' smart constructor.
newtype StopBuild = StopBuild' {id :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopBuild' with the minimum fields required to make a request.
--
-- * 'id' - The ID of the build.
mkStopBuild ::
  -- | 'id'
  Lude.Text ->
  StopBuild
mkStopBuild pId_ = StopBuild' {id = pId_}

-- | The ID of the build.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbId :: Lens.Lens' StopBuild Lude.Text
sbId = Lens.lens (id :: StopBuild -> Lude.Text) (\s a -> s {id = a} :: StopBuild)
{-# DEPRECATED sbId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest StopBuild where
  type Rs StopBuild = StopBuildResponse
  request = Req.postJSON codeBuildService
  response =
    Res.receiveJSON
      ( \s h x ->
          StopBuildResponse'
            Lude.<$> (x Lude..?> "build") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StopBuild where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeBuild_20161006.StopBuild" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopBuild where
  toJSON StopBuild' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("id" Lude..= id)])

instance Lude.ToPath StopBuild where
  toPath = Lude.const "/"

instance Lude.ToQuery StopBuild where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStopBuildResponse' smart constructor.
data StopBuildResponse = StopBuildResponse'
  { build ::
      Lude.Maybe Build,
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

-- | Creates a value of 'StopBuildResponse' with the minimum fields required to make a request.
--
-- * 'build' - Information about the build.
-- * 'responseStatus' - The response status code.
mkStopBuildResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StopBuildResponse
mkStopBuildResponse pResponseStatus_ =
  StopBuildResponse'
    { build = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the build.
--
-- /Note:/ Consider using 'build' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbrsBuild :: Lens.Lens' StopBuildResponse (Lude.Maybe Build)
sbrsBuild = Lens.lens (build :: StopBuildResponse -> Lude.Maybe Build) (\s a -> s {build = a} :: StopBuildResponse)
{-# DEPRECATED sbrsBuild "Use generic-lens or generic-optics with 'build' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbrsResponseStatus :: Lens.Lens' StopBuildResponse Lude.Int
sbrsResponseStatus = Lens.lens (responseStatus :: StopBuildResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StopBuildResponse)
{-# DEPRECATED sbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
