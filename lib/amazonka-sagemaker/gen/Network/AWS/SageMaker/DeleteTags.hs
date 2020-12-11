{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DeleteTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified tags from an Amazon SageMaker resource.
--
-- To list a resource's tags, use the @ListTags@ API.
module Network.AWS.SageMaker.DeleteTags
  ( -- * Creating a request
    DeleteTags (..),
    mkDeleteTags,

    -- ** Request lenses
    dtResourceARN,
    dtTagKeys,

    -- * Destructuring the response
    DeleteTagsResponse (..),
    mkDeleteTagsResponse,

    -- ** Response lenses
    dtsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkDeleteTags' smart constructor.
data DeleteTags = DeleteTags'
  { resourceARN :: Lude.Text,
    tagKeys :: Lude.NonEmpty Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTags' with the minimum fields required to make a request.
--
-- * 'resourceARN' - The Amazon Resource Name (ARN) of the resource whose tags you want to delete.
-- * 'tagKeys' - An array or one or more tag keys to delete.
mkDeleteTags ::
  -- | 'resourceARN'
  Lude.Text ->
  -- | 'tagKeys'
  Lude.NonEmpty Lude.Text ->
  DeleteTags
mkDeleteTags pResourceARN_ pTagKeys_ =
  DeleteTags' {resourceARN = pResourceARN_, tagKeys = pTagKeys_}

-- | The Amazon Resource Name (ARN) of the resource whose tags you want to delete.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtResourceARN :: Lens.Lens' DeleteTags Lude.Text
dtResourceARN = Lens.lens (resourceARN :: DeleteTags -> Lude.Text) (\s a -> s {resourceARN = a} :: DeleteTags)
{-# DEPRECATED dtResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | An array or one or more tag keys to delete.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtTagKeys :: Lens.Lens' DeleteTags (Lude.NonEmpty Lude.Text)
dtTagKeys = Lens.lens (tagKeys :: DeleteTags -> Lude.NonEmpty Lude.Text) (\s a -> s {tagKeys = a} :: DeleteTags)
{-# DEPRECATED dtTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

instance Lude.AWSRequest DeleteTags where
  type Rs DeleteTags = DeleteTagsResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteTagsResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteTags where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.DeleteTags" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteTags where
  toJSON DeleteTags' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ResourceArn" Lude..= resourceARN),
            Lude.Just ("TagKeys" Lude..= tagKeys)
          ]
      )

instance Lude.ToPath DeleteTags where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteTags where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteTagsResponse' smart constructor.
newtype DeleteTagsResponse = DeleteTagsResponse'
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

-- | Creates a value of 'DeleteTagsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteTagsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteTagsResponse
mkDeleteTagsResponse pResponseStatus_ =
  DeleteTagsResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsrsResponseStatus :: Lens.Lens' DeleteTagsResponse Lude.Int
dtsrsResponseStatus = Lens.lens (responseStatus :: DeleteTagsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteTagsResponse)
{-# DEPRECATED dtsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
