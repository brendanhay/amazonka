{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.DescribeTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the tags associated with the specified load balancers.
module Network.AWS.ELB.DescribeTags
  ( -- * Creating a request
    DescribeTags (..),
    mkDescribeTags,

    -- ** Request lenses
    dtLoadBalancerNames,

    -- * Destructuring the response
    DescribeTagsResponse (..),
    mkDescribeTagsResponse,

    -- ** Response lenses
    dtrsTagDescriptions,
    dtrsResponseStatus,
  )
where

import Network.AWS.ELB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for DescribeTags.
--
-- /See:/ 'mkDescribeTags' smart constructor.
newtype DescribeTags = DescribeTags'
  { loadBalancerNames ::
      Lude.NonEmpty Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTags' with the minimum fields required to make a request.
--
-- * 'loadBalancerNames' - The names of the load balancers.
mkDescribeTags ::
  -- | 'loadBalancerNames'
  Lude.NonEmpty Lude.Text ->
  DescribeTags
mkDescribeTags pLoadBalancerNames_ =
  DescribeTags' {loadBalancerNames = pLoadBalancerNames_}

-- | The names of the load balancers.
--
-- /Note:/ Consider using 'loadBalancerNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtLoadBalancerNames :: Lens.Lens' DescribeTags (Lude.NonEmpty Lude.Text)
dtLoadBalancerNames = Lens.lens (loadBalancerNames :: DescribeTags -> Lude.NonEmpty Lude.Text) (\s a -> s {loadBalancerNames = a} :: DescribeTags)
{-# DEPRECATED dtLoadBalancerNames "Use generic-lens or generic-optics with 'loadBalancerNames' instead." #-}

instance Lude.AWSRequest DescribeTags where
  type Rs DescribeTags = DescribeTagsResponse
  request = Req.postQuery elbService
  response =
    Res.receiveXMLWrapper
      "DescribeTagsResult"
      ( \s h x ->
          DescribeTagsResponse'
            Lude.<$> ( x Lude..@? "TagDescriptions" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeTags where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeTags where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeTags where
  toQuery DescribeTags' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeTags" :: Lude.ByteString),
        "Version" Lude.=: ("2012-06-01" :: Lude.ByteString),
        "LoadBalancerNames"
          Lude.=: Lude.toQueryList "member" loadBalancerNames
      ]

-- | Contains the output for DescribeTags.
--
-- /See:/ 'mkDescribeTagsResponse' smart constructor.
data DescribeTagsResponse = DescribeTagsResponse'
  { tagDescriptions ::
      Lude.Maybe [TagDescription],
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
-- * 'responseStatus' - The response status code.
-- * 'tagDescriptions' - Information about the tags.
mkDescribeTagsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeTagsResponse
mkDescribeTagsResponse pResponseStatus_ =
  DescribeTagsResponse'
    { tagDescriptions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the tags.
--
-- /Note:/ Consider using 'tagDescriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsTagDescriptions :: Lens.Lens' DescribeTagsResponse (Lude.Maybe [TagDescription])
dtrsTagDescriptions = Lens.lens (tagDescriptions :: DescribeTagsResponse -> Lude.Maybe [TagDescription]) (\s a -> s {tagDescriptions = a} :: DescribeTagsResponse)
{-# DEPRECATED dtrsTagDescriptions "Use generic-lens or generic-optics with 'tagDescriptions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsResponseStatus :: Lens.Lens' DescribeTagsResponse Lude.Int
dtrsResponseStatus = Lens.lens (responseStatus :: DescribeTagsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeTagsResponse)
{-# DEPRECATED dtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
