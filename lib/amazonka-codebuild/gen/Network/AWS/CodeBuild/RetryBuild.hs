{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.RetryBuild
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restarts a build.
module Network.AWS.CodeBuild.RetryBuild
  ( -- * Creating a request
    RetryBuild (..),
    mkRetryBuild,

    -- ** Request lenses
    rbIdempotencyToken,
    rbId,

    -- * Destructuring the response
    RetryBuildResponse (..),
    mkRetryBuildResponse,

    -- ** Response lenses
    rbrsBuild,
    rbrsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRetryBuild' smart constructor.
data RetryBuild = RetryBuild'
  { idempotencyToken ::
      Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RetryBuild' with the minimum fields required to make a request.
--
-- * 'id' - Specifies the identifier of the build to restart.
-- * 'idempotencyToken' - A unique, case sensitive identifier you provide to ensure the idempotency of the @RetryBuild@ request. The token is included in the @RetryBuild@ request and is valid for five minutes. If you repeat the @RetryBuild@ request with the same token, but change a parameter, AWS CodeBuild returns a parameter mismatch error.
mkRetryBuild ::
  RetryBuild
mkRetryBuild =
  RetryBuild' {idempotencyToken = Lude.Nothing, id = Lude.Nothing}

-- | A unique, case sensitive identifier you provide to ensure the idempotency of the @RetryBuild@ request. The token is included in the @RetryBuild@ request and is valid for five minutes. If you repeat the @RetryBuild@ request with the same token, but change a parameter, AWS CodeBuild returns a parameter mismatch error.
--
-- /Note:/ Consider using 'idempotencyToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rbIdempotencyToken :: Lens.Lens' RetryBuild (Lude.Maybe Lude.Text)
rbIdempotencyToken = Lens.lens (idempotencyToken :: RetryBuild -> Lude.Maybe Lude.Text) (\s a -> s {idempotencyToken = a} :: RetryBuild)
{-# DEPRECATED rbIdempotencyToken "Use generic-lens or generic-optics with 'idempotencyToken' instead." #-}

-- | Specifies the identifier of the build to restart.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rbId :: Lens.Lens' RetryBuild (Lude.Maybe Lude.Text)
rbId = Lens.lens (id :: RetryBuild -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: RetryBuild)
{-# DEPRECATED rbId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest RetryBuild where
  type Rs RetryBuild = RetryBuildResponse
  request = Req.postJSON codeBuildService
  response =
    Res.receiveJSON
      ( \s h x ->
          RetryBuildResponse'
            Lude.<$> (x Lude..?> "build") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RetryBuild where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeBuild_20161006.RetryBuild" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RetryBuild where
  toJSON RetryBuild' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("idempotencyToken" Lude..=) Lude.<$> idempotencyToken,
            ("id" Lude..=) Lude.<$> id
          ]
      )

instance Lude.ToPath RetryBuild where
  toPath = Lude.const "/"

instance Lude.ToQuery RetryBuild where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRetryBuildResponse' smart constructor.
data RetryBuildResponse = RetryBuildResponse'
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

-- | Creates a value of 'RetryBuildResponse' with the minimum fields required to make a request.
--
-- * 'build' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkRetryBuildResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RetryBuildResponse
mkRetryBuildResponse pResponseStatus_ =
  RetryBuildResponse'
    { build = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'build' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rbrsBuild :: Lens.Lens' RetryBuildResponse (Lude.Maybe Build)
rbrsBuild = Lens.lens (build :: RetryBuildResponse -> Lude.Maybe Build) (\s a -> s {build = a} :: RetryBuildResponse)
{-# DEPRECATED rbrsBuild "Use generic-lens or generic-optics with 'build' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rbrsResponseStatus :: Lens.Lens' RetryBuildResponse Lude.Int
rbrsResponseStatus = Lens.lens (responseStatus :: RetryBuildResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RetryBuildResponse)
{-# DEPRECATED rbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
