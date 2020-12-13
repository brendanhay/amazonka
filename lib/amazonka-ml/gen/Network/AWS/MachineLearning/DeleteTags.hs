{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.DeleteTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified tags associated with an ML object. After this operation is complete, you can't recover deleted tags.
--
-- If you specify a tag that doesn't exist, Amazon ML ignores it.
module Network.AWS.MachineLearning.DeleteTags
  ( -- * Creating a request
    DeleteTags (..),
    mkDeleteTags,

    -- ** Request lenses
    dtResourceId,
    dtResourceType,
    dtTagKeys,

    -- * Destructuring the response
    DeleteTagsResponse (..),
    mkDeleteTagsResponse,

    -- ** Response lenses
    dtrsResourceId,
    dtrsResourceType,
    dtrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteTags' smart constructor.
data DeleteTags = DeleteTags'
  { -- | The ID of the tagged ML object. For example, @exampleModelId@ .
    resourceId :: Lude.Text,
    -- | The type of the tagged ML object.
    resourceType :: TaggableResourceType,
    -- | One or more tags to delete.
    tagKeys :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTags' with the minimum fields required to make a request.
--
-- * 'resourceId' - The ID of the tagged ML object. For example, @exampleModelId@ .
-- * 'resourceType' - The type of the tagged ML object.
-- * 'tagKeys' - One or more tags to delete.
mkDeleteTags ::
  -- | 'resourceId'
  Lude.Text ->
  -- | 'resourceType'
  TaggableResourceType ->
  DeleteTags
mkDeleteTags pResourceId_ pResourceType_ =
  DeleteTags'
    { resourceId = pResourceId_,
      resourceType = pResourceType_,
      tagKeys = Lude.mempty
    }

-- | The ID of the tagged ML object. For example, @exampleModelId@ .
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtResourceId :: Lens.Lens' DeleteTags Lude.Text
dtResourceId = Lens.lens (resourceId :: DeleteTags -> Lude.Text) (\s a -> s {resourceId = a} :: DeleteTags)
{-# DEPRECATED dtResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The type of the tagged ML object.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtResourceType :: Lens.Lens' DeleteTags TaggableResourceType
dtResourceType = Lens.lens (resourceType :: DeleteTags -> TaggableResourceType) (\s a -> s {resourceType = a} :: DeleteTags)
{-# DEPRECATED dtResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | One or more tags to delete.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtTagKeys :: Lens.Lens' DeleteTags [Lude.Text]
dtTagKeys = Lens.lens (tagKeys :: DeleteTags -> [Lude.Text]) (\s a -> s {tagKeys = a} :: DeleteTags)
{-# DEPRECATED dtTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

instance Lude.AWSRequest DeleteTags where
  type Rs DeleteTags = DeleteTagsResponse
  request = Req.postJSON machineLearningService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteTagsResponse'
            Lude.<$> (x Lude..?> "ResourceId")
            Lude.<*> (x Lude..?> "ResourceType")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteTags where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonML_20141212.DeleteTags" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteTags where
  toJSON DeleteTags' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ResourceId" Lude..= resourceId),
            Lude.Just ("ResourceType" Lude..= resourceType),
            Lude.Just ("TagKeys" Lude..= tagKeys)
          ]
      )

instance Lude.ToPath DeleteTags where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteTags where
  toQuery = Lude.const Lude.mempty

-- | Amazon ML returns the following elements.
--
-- /See:/ 'mkDeleteTagsResponse' smart constructor.
data DeleteTagsResponse = DeleteTagsResponse'
  { -- | The ID of the ML object from which tags were deleted.
    resourceId :: Lude.Maybe Lude.Text,
    -- | The type of the ML object from which tags were deleted.
    resourceType :: Lude.Maybe TaggableResourceType,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTagsResponse' with the minimum fields required to make a request.
--
-- * 'resourceId' - The ID of the ML object from which tags were deleted.
-- * 'resourceType' - The type of the ML object from which tags were deleted.
-- * 'responseStatus' - The response status code.
mkDeleteTagsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteTagsResponse
mkDeleteTagsResponse pResponseStatus_ =
  DeleteTagsResponse'
    { resourceId = Lude.Nothing,
      resourceType = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the ML object from which tags were deleted.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsResourceId :: Lens.Lens' DeleteTagsResponse (Lude.Maybe Lude.Text)
dtrsResourceId = Lens.lens (resourceId :: DeleteTagsResponse -> Lude.Maybe Lude.Text) (\s a -> s {resourceId = a} :: DeleteTagsResponse)
{-# DEPRECATED dtrsResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The type of the ML object from which tags were deleted.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsResourceType :: Lens.Lens' DeleteTagsResponse (Lude.Maybe TaggableResourceType)
dtrsResourceType = Lens.lens (resourceType :: DeleteTagsResponse -> Lude.Maybe TaggableResourceType) (\s a -> s {resourceType = a} :: DeleteTagsResponse)
{-# DEPRECATED dtrsResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsResponseStatus :: Lens.Lens' DeleteTagsResponse Lude.Int
dtrsResponseStatus = Lens.lens (responseStatus :: DeleteTagsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteTagsResponse)
{-# DEPRECATED dtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
