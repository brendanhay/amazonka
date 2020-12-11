{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    dTagKeys,
    dResourceId,
    dResourceType,

    -- * Destructuring the response
    DeleteTagsResponse (..),
    mkDeleteTagsResponse,

    -- ** Response lenses
    drsResourceId,
    drsResourceType,
    drsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteTags' smart constructor.
data DeleteTags = DeleteTags'
  { tagKeys :: [Lude.Text],
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
    { tagKeys = Lude.mempty,
      resourceId = pResourceId_,
      resourceType = pResourceType_
    }

-- | One or more tags to delete.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dTagKeys :: Lens.Lens' DeleteTags [Lude.Text]
dTagKeys = Lens.lens (tagKeys :: DeleteTags -> [Lude.Text]) (\s a -> s {tagKeys = a} :: DeleteTags)
{-# DEPRECATED dTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

-- | The ID of the tagged ML object. For example, @exampleModelId@ .
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dResourceId :: Lens.Lens' DeleteTags Lude.Text
dResourceId = Lens.lens (resourceId :: DeleteTags -> Lude.Text) (\s a -> s {resourceId = a} :: DeleteTags)
{-# DEPRECATED dResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The type of the tagged ML object.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dResourceType :: Lens.Lens' DeleteTags TaggableResourceType
dResourceType = Lens.lens (resourceType :: DeleteTags -> TaggableResourceType) (\s a -> s {resourceType = a} :: DeleteTags)
{-# DEPRECATED dResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

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
          [ Lude.Just ("TagKeys" Lude..= tagKeys),
            Lude.Just ("ResourceId" Lude..= resourceId),
            Lude.Just ("ResourceType" Lude..= resourceType)
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
drsResourceId :: Lens.Lens' DeleteTagsResponse (Lude.Maybe Lude.Text)
drsResourceId = Lens.lens (resourceId :: DeleteTagsResponse -> Lude.Maybe Lude.Text) (\s a -> s {resourceId = a} :: DeleteTagsResponse)
{-# DEPRECATED drsResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The type of the ML object from which tags were deleted.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResourceType :: Lens.Lens' DeleteTagsResponse (Lude.Maybe TaggableResourceType)
drsResourceType = Lens.lens (resourceType :: DeleteTagsResponse -> Lude.Maybe TaggableResourceType) (\s a -> s {resourceType = a} :: DeleteTagsResponse)
{-# DEPRECATED drsResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteTagsResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DeleteTagsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteTagsResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
