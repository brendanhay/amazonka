{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.StartImageBuilder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the specified image builder.
module Network.AWS.AppStream.StartImageBuilder
  ( -- * Creating a request
    StartImageBuilder (..),
    mkStartImageBuilder,

    -- ** Request lenses
    sibAppstreamAgentVersion,
    sibName,

    -- * Destructuring the response
    StartImageBuilderResponse (..),
    mkStartImageBuilderResponse,

    -- ** Response lenses
    srsImageBuilder,
    srsResponseStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartImageBuilder' smart constructor.
data StartImageBuilder = StartImageBuilder'
  { appstreamAgentVersion ::
      Lude.Maybe Lude.Text,
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartImageBuilder' with the minimum fields required to make a request.
--
-- * 'appstreamAgentVersion' - The version of the AppStream 2.0 agent to use for this image builder. To use the latest version of the AppStream 2.0 agent, specify [LATEST].
-- * 'name' - The name of the image builder.
mkStartImageBuilder ::
  -- | 'name'
  Lude.Text ->
  StartImageBuilder
mkStartImageBuilder pName_ =
  StartImageBuilder'
    { appstreamAgentVersion = Lude.Nothing,
      name = pName_
    }

-- | The version of the AppStream 2.0 agent to use for this image builder. To use the latest version of the AppStream 2.0 agent, specify [LATEST].
--
-- /Note:/ Consider using 'appstreamAgentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sibAppstreamAgentVersion :: Lens.Lens' StartImageBuilder (Lude.Maybe Lude.Text)
sibAppstreamAgentVersion = Lens.lens (appstreamAgentVersion :: StartImageBuilder -> Lude.Maybe Lude.Text) (\s a -> s {appstreamAgentVersion = a} :: StartImageBuilder)
{-# DEPRECATED sibAppstreamAgentVersion "Use generic-lens or generic-optics with 'appstreamAgentVersion' instead." #-}

-- | The name of the image builder.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sibName :: Lens.Lens' StartImageBuilder Lude.Text
sibName = Lens.lens (name :: StartImageBuilder -> Lude.Text) (\s a -> s {name = a} :: StartImageBuilder)
{-# DEPRECATED sibName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest StartImageBuilder where
  type Rs StartImageBuilder = StartImageBuilderResponse
  request = Req.postJSON appStreamService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartImageBuilderResponse'
            Lude.<$> (x Lude..?> "ImageBuilder") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartImageBuilder where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("PhotonAdminProxyService.StartImageBuilder" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartImageBuilder where
  toJSON StartImageBuilder' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AppstreamAgentVersion" Lude..=) Lude.<$> appstreamAgentVersion,
            Lude.Just ("Name" Lude..= name)
          ]
      )

instance Lude.ToPath StartImageBuilder where
  toPath = Lude.const "/"

instance Lude.ToQuery StartImageBuilder where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartImageBuilderResponse' smart constructor.
data StartImageBuilderResponse = StartImageBuilderResponse'
  { imageBuilder ::
      Lude.Maybe ImageBuilder,
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

-- | Creates a value of 'StartImageBuilderResponse' with the minimum fields required to make a request.
--
-- * 'imageBuilder' - Information about the image builder.
-- * 'responseStatus' - The response status code.
mkStartImageBuilderResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartImageBuilderResponse
mkStartImageBuilderResponse pResponseStatus_ =
  StartImageBuilderResponse'
    { imageBuilder = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the image builder.
--
-- /Note:/ Consider using 'imageBuilder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsImageBuilder :: Lens.Lens' StartImageBuilderResponse (Lude.Maybe ImageBuilder)
srsImageBuilder = Lens.lens (imageBuilder :: StartImageBuilderResponse -> Lude.Maybe ImageBuilder) (\s a -> s {imageBuilder = a} :: StartImageBuilderResponse)
{-# DEPRECATED srsImageBuilder "Use generic-lens or generic-optics with 'imageBuilder' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' StartImageBuilderResponse Lude.Int
srsResponseStatus = Lens.lens (responseStatus :: StartImageBuilderResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartImageBuilderResponse)
{-# DEPRECATED srsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
