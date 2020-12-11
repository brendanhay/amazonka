{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.RemoveTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified tags from the specified Elastic Load Balancing resources. You can remove the tags for one or more Application Load Balancers, Network Load Balancers, Gateway Load Balancers, target groups, listeners, or rules.
module Network.AWS.ELBv2.RemoveTags
  ( -- * Creating a request
    RemoveTags (..),
    mkRemoveTags,

    -- ** Request lenses
    rtResourceARNs,
    rtTagKeys,

    -- * Destructuring the response
    RemoveTagsResponse (..),
    mkRemoveTagsResponse,

    -- ** Response lenses
    rtrsResponseStatus,
  )
where

import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRemoveTags' smart constructor.
data RemoveTags = RemoveTags'
  { resourceARNs :: [Lude.Text],
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
-- * 'resourceARNs' - The Amazon Resource Name (ARN) of the resource.
-- * 'tagKeys' - The tag keys for the tags to remove.
mkRemoveTags ::
  RemoveTags
mkRemoveTags =
  RemoveTags' {resourceARNs = Lude.mempty, tagKeys = Lude.mempty}

-- | The Amazon Resource Name (ARN) of the resource.
--
-- /Note:/ Consider using 'resourceARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtResourceARNs :: Lens.Lens' RemoveTags [Lude.Text]
rtResourceARNs = Lens.lens (resourceARNs :: RemoveTags -> [Lude.Text]) (\s a -> s {resourceARNs = a} :: RemoveTags)
{-# DEPRECATED rtResourceARNs "Use generic-lens or generic-optics with 'resourceARNs' instead." #-}

-- | The tag keys for the tags to remove.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtTagKeys :: Lens.Lens' RemoveTags [Lude.Text]
rtTagKeys = Lens.lens (tagKeys :: RemoveTags -> [Lude.Text]) (\s a -> s {tagKeys = a} :: RemoveTags)
{-# DEPRECATED rtTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

instance Lude.AWSRequest RemoveTags where
  type Rs RemoveTags = RemoveTagsResponse
  request = Req.postQuery eLBv2Service
  response =
    Res.receiveXMLWrapper
      "RemoveTagsResult"
      ( \s h x ->
          RemoveTagsResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RemoveTags where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath RemoveTags where
  toPath = Lude.const "/"

instance Lude.ToQuery RemoveTags where
  toQuery RemoveTags' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("RemoveTags" :: Lude.ByteString),
        "Version" Lude.=: ("2015-12-01" :: Lude.ByteString),
        "ResourceArns" Lude.=: Lude.toQueryList "member" resourceARNs,
        "TagKeys" Lude.=: Lude.toQueryList "member" tagKeys
      ]

-- | /See:/ 'mkRemoveTagsResponse' smart constructor.
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
