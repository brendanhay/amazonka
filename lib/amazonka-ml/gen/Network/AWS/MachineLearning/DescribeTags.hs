{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.DescribeTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of the tags for your Amazon ML object.
module Network.AWS.MachineLearning.DescribeTags
  ( -- * Creating a request
    DescribeTags (..),
    mkDescribeTags,

    -- ** Request lenses
    dtResourceId,
    dtResourceType,

    -- * Destructuring the response
    DescribeTagsResponse (..),
    mkDescribeTagsResponse,

    -- ** Response lenses
    dtrsResourceId,
    dtrsResourceType,
    dtrsTags,
    dtrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeTags' smart constructor.
data DescribeTags = DescribeTags'
  { resourceId :: Lude.Text,
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

-- | Creates a value of 'DescribeTags' with the minimum fields required to make a request.
--
-- * 'resourceId' - The ID of the ML object. For example, @exampleModelId@ .
-- * 'resourceType' - The type of the ML object.
mkDescribeTags ::
  -- | 'resourceId'
  Lude.Text ->
  -- | 'resourceType'
  TaggableResourceType ->
  DescribeTags
mkDescribeTags pResourceId_ pResourceType_ =
  DescribeTags'
    { resourceId = pResourceId_,
      resourceType = pResourceType_
    }

-- | The ID of the ML object. For example, @exampleModelId@ .
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtResourceId :: Lens.Lens' DescribeTags Lude.Text
dtResourceId = Lens.lens (resourceId :: DescribeTags -> Lude.Text) (\s a -> s {resourceId = a} :: DescribeTags)
{-# DEPRECATED dtResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The type of the ML object.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtResourceType :: Lens.Lens' DescribeTags TaggableResourceType
dtResourceType = Lens.lens (resourceType :: DescribeTags -> TaggableResourceType) (\s a -> s {resourceType = a} :: DescribeTags)
{-# DEPRECATED dtResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

instance Lude.AWSRequest DescribeTags where
  type Rs DescribeTags = DescribeTagsResponse
  request = Req.postJSON machineLearningService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeTagsResponse'
            Lude.<$> (x Lude..?> "ResourceId")
            Lude.<*> (x Lude..?> "ResourceType")
            Lude.<*> (x Lude..?> "Tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeTags where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonML_20141212.DescribeTags" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeTags where
  toJSON DescribeTags' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ResourceId" Lude..= resourceId),
            Lude.Just ("ResourceType" Lude..= resourceType)
          ]
      )

instance Lude.ToPath DescribeTags where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeTags where
  toQuery = Lude.const Lude.mempty

-- | Amazon ML returns the following elements.
--
-- /See:/ 'mkDescribeTagsResponse' smart constructor.
data DescribeTagsResponse = DescribeTagsResponse'
  { resourceId ::
      Lude.Maybe Lude.Text,
    resourceType :: Lude.Maybe TaggableResourceType,
    tags :: Lude.Maybe [Tag],
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

-- | Creates a value of 'DescribeTagsResponse' with the minimum fields required to make a request.
--
-- * 'resourceId' - The ID of the tagged ML object.
-- * 'resourceType' - The type of the tagged ML object.
-- * 'responseStatus' - The response status code.
-- * 'tags' - A list of tags associated with the ML object.
mkDescribeTagsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeTagsResponse
mkDescribeTagsResponse pResponseStatus_ =
  DescribeTagsResponse'
    { resourceId = Lude.Nothing,
      resourceType = Lude.Nothing,
      tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the tagged ML object.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsResourceId :: Lens.Lens' DescribeTagsResponse (Lude.Maybe Lude.Text)
dtrsResourceId = Lens.lens (resourceId :: DescribeTagsResponse -> Lude.Maybe Lude.Text) (\s a -> s {resourceId = a} :: DescribeTagsResponse)
{-# DEPRECATED dtrsResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The type of the tagged ML object.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsResourceType :: Lens.Lens' DescribeTagsResponse (Lude.Maybe TaggableResourceType)
dtrsResourceType = Lens.lens (resourceType :: DescribeTagsResponse -> Lude.Maybe TaggableResourceType) (\s a -> s {resourceType = a} :: DescribeTagsResponse)
{-# DEPRECATED dtrsResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | A list of tags associated with the ML object.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsTags :: Lens.Lens' DescribeTagsResponse (Lude.Maybe [Tag])
dtrsTags = Lens.lens (tags :: DescribeTagsResponse -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: DescribeTagsResponse)
{-# DEPRECATED dtrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsResponseStatus :: Lens.Lens' DescribeTagsResponse Lude.Int
dtrsResponseStatus = Lens.lens (responseStatus :: DescribeTagsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeTagsResponse)
{-# DEPRECATED dtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
