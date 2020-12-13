{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.RemoveTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes one or more tags from the specified load balancer.
module Network.AWS.ELB.RemoveTags
  ( -- * Creating a request
    RemoveTags (..),
    mkRemoveTags,

    -- ** Request lenses
    rtLoadBalancerNames,
    rtTags,

    -- * Destructuring the response
    RemoveTagsResponse (..),
    mkRemoveTagsResponse,

    -- ** Response lenses
    rtrsResponseStatus,
  )
where

import Network.AWS.ELB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for RemoveTags.
--
-- /See:/ 'mkRemoveTags' smart constructor.
data RemoveTags = RemoveTags'
  { -- | The name of the load balancer. You can specify a maximum of one load balancer name.
    loadBalancerNames :: [Lude.Text],
    -- | The list of tag keys to remove.
    tags :: Lude.NonEmpty TagKeyOnly
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveTags' with the minimum fields required to make a request.
--
-- * 'loadBalancerNames' - The name of the load balancer. You can specify a maximum of one load balancer name.
-- * 'tags' - The list of tag keys to remove.
mkRemoveTags ::
  -- | 'tags'
  Lude.NonEmpty TagKeyOnly ->
  RemoveTags
mkRemoveTags pTags_ =
  RemoveTags' {loadBalancerNames = Lude.mempty, tags = pTags_}

-- | The name of the load balancer. You can specify a maximum of one load balancer name.
--
-- /Note:/ Consider using 'loadBalancerNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtLoadBalancerNames :: Lens.Lens' RemoveTags [Lude.Text]
rtLoadBalancerNames = Lens.lens (loadBalancerNames :: RemoveTags -> [Lude.Text]) (\s a -> s {loadBalancerNames = a} :: RemoveTags)
{-# DEPRECATED rtLoadBalancerNames "Use generic-lens or generic-optics with 'loadBalancerNames' instead." #-}

-- | The list of tag keys to remove.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtTags :: Lens.Lens' RemoveTags (Lude.NonEmpty TagKeyOnly)
rtTags = Lens.lens (tags :: RemoveTags -> Lude.NonEmpty TagKeyOnly) (\s a -> s {tags = a} :: RemoveTags)
{-# DEPRECATED rtTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest RemoveTags where
  type Rs RemoveTags = RemoveTagsResponse
  request = Req.postQuery elbService
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
        "Version" Lude.=: ("2012-06-01" :: Lude.ByteString),
        "LoadBalancerNames"
          Lude.=: Lude.toQueryList "member" loadBalancerNames,
        "Tags" Lude.=: Lude.toQueryList "member" tags
      ]

-- | Contains the output of RemoveTags.
--
-- /See:/ 'mkRemoveTagsResponse' smart constructor.
newtype RemoveTagsResponse = RemoveTagsResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
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
