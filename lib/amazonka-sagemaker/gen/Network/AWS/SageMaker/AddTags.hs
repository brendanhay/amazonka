{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.AddTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or overwrites one or more tags for the specified Amazon SageMaker resource. You can add tags to notebook instances, training jobs, hyperparameter tuning jobs, batch transform jobs, models, labeling jobs, work teams, endpoint configurations, and endpoints.
--
-- Each tag consists of a key and an optional value. Tag keys must be unique per resource. For more information about tags, see For more information, see <https://aws.amazon.com/answers/account-management/aws-tagging-strategies/ AWS Tagging Strategies> .
module Network.AWS.SageMaker.AddTags
  ( -- * Creating a request
    AddTags (..),
    mkAddTags,

    -- ** Request lenses
    atResourceARN,
    atTags,

    -- * Destructuring the response
    AddTagsResponse (..),
    mkAddTagsResponse,

    -- ** Response lenses
    atrsTags,
    atrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkAddTags' smart constructor.
data AddTags = AddTags'
  { -- | The Amazon Resource Name (ARN) of the resource that you want to tag.
    resourceARN :: Lude.Text,
    -- | An array of @Tag@ objects. Each tag is a key-value pair. Only the @key@ parameter is required. If you don't specify a value, Amazon SageMaker sets the value to an empty string.
    tags :: [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddTags' with the minimum fields required to make a request.
--
-- * 'resourceARN' - The Amazon Resource Name (ARN) of the resource that you want to tag.
-- * 'tags' - An array of @Tag@ objects. Each tag is a key-value pair. Only the @key@ parameter is required. If you don't specify a value, Amazon SageMaker sets the value to an empty string.
mkAddTags ::
  -- | 'resourceARN'
  Lude.Text ->
  AddTags
mkAddTags pResourceARN_ =
  AddTags' {resourceARN = pResourceARN_, tags = Lude.mempty}

-- | The Amazon Resource Name (ARN) of the resource that you want to tag.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atResourceARN :: Lens.Lens' AddTags Lude.Text
atResourceARN = Lens.lens (resourceARN :: AddTags -> Lude.Text) (\s a -> s {resourceARN = a} :: AddTags)
{-# DEPRECATED atResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | An array of @Tag@ objects. Each tag is a key-value pair. Only the @key@ parameter is required. If you don't specify a value, Amazon SageMaker sets the value to an empty string.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atTags :: Lens.Lens' AddTags [Tag]
atTags = Lens.lens (tags :: AddTags -> [Tag]) (\s a -> s {tags = a} :: AddTags)
{-# DEPRECATED atTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest AddTags where
  type Rs AddTags = AddTagsResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          AddTagsResponse'
            Lude.<$> (x Lude..?> "Tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AddTags where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target" Lude.=# ("SageMaker.AddTags" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AddTags where
  toJSON AddTags' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ResourceArn" Lude..= resourceARN),
            Lude.Just ("Tags" Lude..= tags)
          ]
      )

instance Lude.ToPath AddTags where
  toPath = Lude.const "/"

instance Lude.ToQuery AddTags where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAddTagsResponse' smart constructor.
data AddTagsResponse = AddTagsResponse'
  { -- | A list of tags associated with the Amazon SageMaker resource.
    tags :: Lude.Maybe [Tag],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddTagsResponse' with the minimum fields required to make a request.
--
-- * 'tags' - A list of tags associated with the Amazon SageMaker resource.
-- * 'responseStatus' - The response status code.
mkAddTagsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AddTagsResponse
mkAddTagsResponse pResponseStatus_ =
  AddTagsResponse'
    { tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of tags associated with the Amazon SageMaker resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atrsTags :: Lens.Lens' AddTagsResponse (Lude.Maybe [Tag])
atrsTags = Lens.lens (tags :: AddTagsResponse -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: AddTagsResponse)
{-# DEPRECATED atrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atrsResponseStatus :: Lens.Lens' AddTagsResponse Lude.Int
atrsResponseStatus = Lens.lens (responseStatus :: AddTagsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AddTagsResponse)
{-# DEPRECATED atrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
