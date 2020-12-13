{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.StopImageBuilder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the specified image builder.
module Network.AWS.AppStream.StopImageBuilder
  ( -- * Creating a request
    StopImageBuilder (..),
    mkStopImageBuilder,

    -- ** Request lenses
    sibfName,

    -- * Destructuring the response
    StopImageBuilderResponse (..),
    mkStopImageBuilderResponse,

    -- ** Response lenses
    sibrsImageBuilder,
    sibrsResponseStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStopImageBuilder' smart constructor.
newtype StopImageBuilder = StopImageBuilder'
  { -- | The name of the image builder.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopImageBuilder' with the minimum fields required to make a request.
--
-- * 'name' - The name of the image builder.
mkStopImageBuilder ::
  -- | 'name'
  Lude.Text ->
  StopImageBuilder
mkStopImageBuilder pName_ = StopImageBuilder' {name = pName_}

-- | The name of the image builder.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sibfName :: Lens.Lens' StopImageBuilder Lude.Text
sibfName = Lens.lens (name :: StopImageBuilder -> Lude.Text) (\s a -> s {name = a} :: StopImageBuilder)
{-# DEPRECATED sibfName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest StopImageBuilder where
  type Rs StopImageBuilder = StopImageBuilderResponse
  request = Req.postJSON appStreamService
  response =
    Res.receiveJSON
      ( \s h x ->
          StopImageBuilderResponse'
            Lude.<$> (x Lude..?> "ImageBuilder") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StopImageBuilder where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("PhotonAdminProxyService.StopImageBuilder" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopImageBuilder where
  toJSON StopImageBuilder' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Name" Lude..= name)])

instance Lude.ToPath StopImageBuilder where
  toPath = Lude.const "/"

instance Lude.ToQuery StopImageBuilder where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStopImageBuilderResponse' smart constructor.
data StopImageBuilderResponse = StopImageBuilderResponse'
  { -- | Information about the image builder.
    imageBuilder :: Lude.Maybe ImageBuilder,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopImageBuilderResponse' with the minimum fields required to make a request.
--
-- * 'imageBuilder' - Information about the image builder.
-- * 'responseStatus' - The response status code.
mkStopImageBuilderResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StopImageBuilderResponse
mkStopImageBuilderResponse pResponseStatus_ =
  StopImageBuilderResponse'
    { imageBuilder = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the image builder.
--
-- /Note:/ Consider using 'imageBuilder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sibrsImageBuilder :: Lens.Lens' StopImageBuilderResponse (Lude.Maybe ImageBuilder)
sibrsImageBuilder = Lens.lens (imageBuilder :: StopImageBuilderResponse -> Lude.Maybe ImageBuilder) (\s a -> s {imageBuilder = a} :: StopImageBuilderResponse)
{-# DEPRECATED sibrsImageBuilder "Use generic-lens or generic-optics with 'imageBuilder' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sibrsResponseStatus :: Lens.Lens' StopImageBuilderResponse Lude.Int
sibrsResponseStatus = Lens.lens (responseStatus :: StopImageBuilderResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StopImageBuilderResponse)
{-# DEPRECATED sibrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
