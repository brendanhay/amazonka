{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.AddTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more tags to an object, up to a limit of 10. Each tag consists of a key and an optional value. If you add a tag using a key that is already associated with the ML object, @AddTags@ updates the tag's value.
module Network.AWS.MachineLearning.AddTags
  ( -- * Creating a request
    AddTags (..),
    mkAddTags,

    -- ** Request lenses
    atTags,
    atResourceId,
    atResourceType,

    -- * Destructuring the response
    AddTagsResponse (..),
    mkAddTagsResponse,

    -- ** Response lenses
    atrsResourceId,
    atrsResourceType,
    atrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAddTags' smart constructor.
data AddTags = AddTags'
  { tags :: [Tag],
    resourceId :: Lude.Text,
    resourceType :: TaggableResourceType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddTags' with the minimum fields required to make a request.
--
-- * 'resourceId' - The ID of the ML object to tag. For example, @exampleModelId@ .
-- * 'resourceType' - The type of the ML object to tag.
-- * 'tags' - The key-value pairs to use to create tags. If you specify a key without specifying a value, Amazon ML creates a tag with the specified key and a value of null.
mkAddTags ::
  -- | 'resourceId'
  Lude.Text ->
  -- | 'resourceType'
  TaggableResourceType ->
  AddTags
mkAddTags pResourceId_ pResourceType_ =
  AddTags'
    { tags = Lude.mempty,
      resourceId = pResourceId_,
      resourceType = pResourceType_
    }

-- | The key-value pairs to use to create tags. If you specify a key without specifying a value, Amazon ML creates a tag with the specified key and a value of null.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atTags :: Lens.Lens' AddTags [Tag]
atTags = Lens.lens (tags :: AddTags -> [Tag]) (\s a -> s {tags = a} :: AddTags)
{-# DEPRECATED atTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The ID of the ML object to tag. For example, @exampleModelId@ .
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atResourceId :: Lens.Lens' AddTags Lude.Text
atResourceId = Lens.lens (resourceId :: AddTags -> Lude.Text) (\s a -> s {resourceId = a} :: AddTags)
{-# DEPRECATED atResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The type of the ML object to tag.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atResourceType :: Lens.Lens' AddTags TaggableResourceType
atResourceType = Lens.lens (resourceType :: AddTags -> TaggableResourceType) (\s a -> s {resourceType = a} :: AddTags)
{-# DEPRECATED atResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

instance Lude.AWSRequest AddTags where
  type Rs AddTags = AddTagsResponse
  request = Req.postJSON machineLearningService
  response =
    Res.receiveJSON
      ( \s h x ->
          AddTagsResponse'
            Lude.<$> (x Lude..?> "ResourceId")
            Lude.<*> (x Lude..?> "ResourceType")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AddTags where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonML_20141212.AddTags" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AddTags where
  toJSON AddTags' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Tags" Lude..= tags),
            Lude.Just ("ResourceId" Lude..= resourceId),
            Lude.Just ("ResourceType" Lude..= resourceType)
          ]
      )

instance Lude.ToPath AddTags where
  toPath = Lude.const "/"

instance Lude.ToQuery AddTags where
  toQuery = Lude.const Lude.mempty

-- | Amazon ML returns the following elements.
--
-- /See:/ 'mkAddTagsResponse' smart constructor.
data AddTagsResponse = AddTagsResponse'
  { resourceId ::
      Lude.Maybe Lude.Text,
    resourceType :: Lude.Maybe TaggableResourceType,
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

-- | Creates a value of 'AddTagsResponse' with the minimum fields required to make a request.
--
-- * 'resourceId' - The ID of the ML object that was tagged.
-- * 'resourceType' - The type of the ML object that was tagged.
-- * 'responseStatus' - The response status code.
mkAddTagsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AddTagsResponse
mkAddTagsResponse pResponseStatus_ =
  AddTagsResponse'
    { resourceId = Lude.Nothing,
      resourceType = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the ML object that was tagged.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atrsResourceId :: Lens.Lens' AddTagsResponse (Lude.Maybe Lude.Text)
atrsResourceId = Lens.lens (resourceId :: AddTagsResponse -> Lude.Maybe Lude.Text) (\s a -> s {resourceId = a} :: AddTagsResponse)
{-# DEPRECATED atrsResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The type of the ML object that was tagged.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atrsResourceType :: Lens.Lens' AddTagsResponse (Lude.Maybe TaggableResourceType)
atrsResourceType = Lens.lens (resourceType :: AddTagsResponse -> Lude.Maybe TaggableResourceType) (\s a -> s {resourceType = a} :: AddTagsResponse)
{-# DEPRECATED atrsResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atrsResponseStatus :: Lens.Lens' AddTagsResponse Lude.Int
atrsResponseStatus = Lens.lens (responseStatus :: AddTagsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AddTagsResponse)
{-# DEPRECATED atrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
