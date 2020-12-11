{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.DeleteImageBuilder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified image builder and releases the capacity.
module Network.AWS.AppStream.DeleteImageBuilder
  ( -- * Creating a request
    DeleteImageBuilder (..),
    mkDeleteImageBuilder,

    -- ** Request lenses
    dibName,

    -- * Destructuring the response
    DeleteImageBuilderResponse (..),
    mkDeleteImageBuilderResponse,

    -- ** Response lenses
    dibrsImageBuilder,
    dibrsResponseStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteImageBuilder' smart constructor.
newtype DeleteImageBuilder = DeleteImageBuilder' {name :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteImageBuilder' with the minimum fields required to make a request.
--
-- * 'name' - The name of the image builder.
mkDeleteImageBuilder ::
  -- | 'name'
  Lude.Text ->
  DeleteImageBuilder
mkDeleteImageBuilder pName_ = DeleteImageBuilder' {name = pName_}

-- | The name of the image builder.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dibName :: Lens.Lens' DeleteImageBuilder Lude.Text
dibName = Lens.lens (name :: DeleteImageBuilder -> Lude.Text) (\s a -> s {name = a} :: DeleteImageBuilder)
{-# DEPRECATED dibName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest DeleteImageBuilder where
  type Rs DeleteImageBuilder = DeleteImageBuilderResponse
  request = Req.postJSON appStreamService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteImageBuilderResponse'
            Lude.<$> (x Lude..?> "ImageBuilder") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteImageBuilder where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("PhotonAdminProxyService.DeleteImageBuilder" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteImageBuilder where
  toJSON DeleteImageBuilder' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Name" Lude..= name)])

instance Lude.ToPath DeleteImageBuilder where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteImageBuilder where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteImageBuilderResponse' smart constructor.
data DeleteImageBuilderResponse = DeleteImageBuilderResponse'
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

-- | Creates a value of 'DeleteImageBuilderResponse' with the minimum fields required to make a request.
--
-- * 'imageBuilder' - Information about the image builder.
-- * 'responseStatus' - The response status code.
mkDeleteImageBuilderResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteImageBuilderResponse
mkDeleteImageBuilderResponse pResponseStatus_ =
  DeleteImageBuilderResponse'
    { imageBuilder = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the image builder.
--
-- /Note:/ Consider using 'imageBuilder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dibrsImageBuilder :: Lens.Lens' DeleteImageBuilderResponse (Lude.Maybe ImageBuilder)
dibrsImageBuilder = Lens.lens (imageBuilder :: DeleteImageBuilderResponse -> Lude.Maybe ImageBuilder) (\s a -> s {imageBuilder = a} :: DeleteImageBuilderResponse)
{-# DEPRECATED dibrsImageBuilder "Use generic-lens or generic-optics with 'imageBuilder' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dibrsResponseStatus :: Lens.Lens' DeleteImageBuilderResponse Lude.Int
dibrsResponseStatus = Lens.lens (responseStatus :: DeleteImageBuilderResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteImageBuilderResponse)
{-# DEPRECATED dibrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
