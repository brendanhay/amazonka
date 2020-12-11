{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.DeleteMLTransform
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an AWS Glue machine learning transform. Machine learning transforms are a special type of transform that use machine learning to learn the details of the transformation to be performed by learning from examples provided by humans. These transformations are then saved by AWS Glue. If you no longer need a transform, you can delete it by calling @DeleteMLTransforms@ . However, any AWS Glue jobs that still reference the deleted transform will no longer succeed.
module Network.AWS.Glue.DeleteMLTransform
  ( -- * Creating a request
    DeleteMLTransform (..),
    mkDeleteMLTransform,

    -- ** Request lenses
    dmltTransformId,

    -- * Destructuring the response
    DeleteMLTransformResponse (..),
    mkDeleteMLTransformResponse,

    -- ** Response lenses
    dmltrsTransformId,
    dmltrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteMLTransform' smart constructor.
newtype DeleteMLTransform = DeleteMLTransform'
  { transformId ::
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

-- | Creates a value of 'DeleteMLTransform' with the minimum fields required to make a request.
--
-- * 'transformId' - The unique identifier of the transform to delete.
mkDeleteMLTransform ::
  -- | 'transformId'
  Lude.Text ->
  DeleteMLTransform
mkDeleteMLTransform pTransformId_ =
  DeleteMLTransform' {transformId = pTransformId_}

-- | The unique identifier of the transform to delete.
--
-- /Note:/ Consider using 'transformId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmltTransformId :: Lens.Lens' DeleteMLTransform Lude.Text
dmltTransformId = Lens.lens (transformId :: DeleteMLTransform -> Lude.Text) (\s a -> s {transformId = a} :: DeleteMLTransform)
{-# DEPRECATED dmltTransformId "Use generic-lens or generic-optics with 'transformId' instead." #-}

instance Lude.AWSRequest DeleteMLTransform where
  type Rs DeleteMLTransform = DeleteMLTransformResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteMLTransformResponse'
            Lude.<$> (x Lude..?> "TransformId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteMLTransform where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.DeleteMLTransform" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteMLTransform where
  toJSON DeleteMLTransform' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("TransformId" Lude..= transformId)])

instance Lude.ToPath DeleteMLTransform where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteMLTransform where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteMLTransformResponse' smart constructor.
data DeleteMLTransformResponse = DeleteMLTransformResponse'
  { transformId ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DeleteMLTransformResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'transformId' - The unique identifier of the transform that was deleted.
mkDeleteMLTransformResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteMLTransformResponse
mkDeleteMLTransformResponse pResponseStatus_ =
  DeleteMLTransformResponse'
    { transformId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The unique identifier of the transform that was deleted.
--
-- /Note:/ Consider using 'transformId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmltrsTransformId :: Lens.Lens' DeleteMLTransformResponse (Lude.Maybe Lude.Text)
dmltrsTransformId = Lens.lens (transformId :: DeleteMLTransformResponse -> Lude.Maybe Lude.Text) (\s a -> s {transformId = a} :: DeleteMLTransformResponse)
{-# DEPRECATED dmltrsTransformId "Use generic-lens or generic-optics with 'transformId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmltrsResponseStatus :: Lens.Lens' DeleteMLTransformResponse Lude.Int
dmltrsResponseStatus = Lens.lens (responseStatus :: DeleteMLTransformResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteMLTransformResponse)
{-# DEPRECATED dmltrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
