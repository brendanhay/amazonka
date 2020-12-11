{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.RemoveTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes tags from an Amazon EMR resource. Tags make it easier to associate clusters in various ways, such as grouping clusters to track your Amazon EMR resource allocation costs. For more information, see <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-plan-tags.html Tag Clusters> .
--
-- The following example removes the stack tag with value Prod from a cluster:
module Network.AWS.EMR.RemoveTags
  ( -- * Creating a request
    RemoveTags (..),
    mkRemoveTags,

    -- ** Request lenses
    rtResourceId,
    rtTagKeys,

    -- * Destructuring the response
    RemoveTagsResponse (..),
    mkRemoveTagsResponse,

    -- ** Response lenses
    rtrsResponseStatus,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | This input identifies a cluster and a list of tags to remove.
--
-- /See:/ 'mkRemoveTags' smart constructor.
data RemoveTags = RemoveTags'
  { resourceId :: Lude.Text,
    tagKeys :: [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveTags' with the minimum fields required to make a request.
--
-- * 'resourceId' - The Amazon EMR resource identifier from which tags will be removed. This value must be a cluster identifier.
-- * 'tagKeys' - A list of tag keys to remove from a resource.
mkRemoveTags ::
  -- | 'resourceId'
  Lude.Text ->
  RemoveTags
mkRemoveTags pResourceId_ =
  RemoveTags' {resourceId = pResourceId_, tagKeys = Lude.mempty}

-- | The Amazon EMR resource identifier from which tags will be removed. This value must be a cluster identifier.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtResourceId :: Lens.Lens' RemoveTags Lude.Text
rtResourceId = Lens.lens (resourceId :: RemoveTags -> Lude.Text) (\s a -> s {resourceId = a} :: RemoveTags)
{-# DEPRECATED rtResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | A list of tag keys to remove from a resource.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtTagKeys :: Lens.Lens' RemoveTags [Lude.Text]
rtTagKeys = Lens.lens (tagKeys :: RemoveTags -> [Lude.Text]) (\s a -> s {tagKeys = a} :: RemoveTags)
{-# DEPRECATED rtTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

instance Lude.AWSRequest RemoveTags where
  type Rs RemoveTags = RemoveTagsResponse
  request = Req.postJSON emrService
  response =
    Res.receiveEmpty
      ( \s h x ->
          RemoveTagsResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RemoveTags where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("ElasticMapReduce.RemoveTags" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RemoveTags where
  toJSON RemoveTags' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ResourceId" Lude..= resourceId),
            Lude.Just ("TagKeys" Lude..= tagKeys)
          ]
      )

instance Lude.ToPath RemoveTags where
  toPath = Lude.const "/"

instance Lude.ToQuery RemoveTags where
  toQuery = Lude.const Lude.mempty

-- | This output indicates the result of removing tags from a resource.
--
-- /See:/ 'mkRemoveTagsResponse' smart constructor.
newtype RemoveTagsResponse = RemoveTagsResponse'
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

-- | Creates a value of 'RemoveTagsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkRemoveTagsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RemoveTagsResponse
mkRemoveTagsResponse pResponseStatus_ =
  RemoveTagsResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrsResponseStatus :: Lens.Lens' RemoveTagsResponse Lude.Int
rtrsResponseStatus = Lens.lens (responseStatus :: RemoveTagsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RemoveTagsResponse)
{-# DEPRECATED rtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
