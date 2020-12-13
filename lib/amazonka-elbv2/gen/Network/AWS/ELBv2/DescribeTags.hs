{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.DescribeTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the tags for the specified Elastic Load Balancing resources. You can describe the tags for one or more Application Load Balancers, Network Load Balancers, Gateway Load Balancers, target groups, listeners, or rules.
module Network.AWS.ELBv2.DescribeTags
  ( -- * Creating a request
    DescribeTags (..),
    mkDescribeTags,

    -- ** Request lenses
    dtResourceARNs,

    -- * Destructuring the response
    DescribeTagsResponse (..),
    mkDescribeTagsResponse,

    -- ** Response lenses
    dtsrsTagDescriptions,
    dtsrsResponseStatus,
  )
where

import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeTags' smart constructor.
newtype DescribeTags = DescribeTags'
  { -- | The Amazon Resource Names (ARN) of the resources. You can specify up to 20 resources in a single call.
    resourceARNs :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTags' with the minimum fields required to make a request.
--
-- * 'resourceARNs' - The Amazon Resource Names (ARN) of the resources. You can specify up to 20 resources in a single call.
mkDescribeTags ::
  DescribeTags
mkDescribeTags = DescribeTags' {resourceARNs = Lude.mempty}

-- | The Amazon Resource Names (ARN) of the resources. You can specify up to 20 resources in a single call.
--
-- /Note:/ Consider using 'resourceARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtResourceARNs :: Lens.Lens' DescribeTags [Lude.Text]
dtResourceARNs = Lens.lens (resourceARNs :: DescribeTags -> [Lude.Text]) (\s a -> s {resourceARNs = a} :: DescribeTags)
{-# DEPRECATED dtResourceARNs "Use generic-lens or generic-optics with 'resourceARNs' instead." #-}

instance Lude.AWSRequest DescribeTags where
  type Rs DescribeTags = DescribeTagsResponse
  request = Req.postQuery eLBv2Service
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
        "Version" Lude.=: ("2015-12-01" :: Lude.ByteString),
        "ResourceArns" Lude.=: Lude.toQueryList "member" resourceARNs
      ]

-- | /See:/ 'mkDescribeTagsResponse' smart constructor.
data DescribeTagsResponse = DescribeTagsResponse'
  { -- | Information about the tags.
    tagDescriptions :: Lude.Maybe [TagDescription],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTagsResponse' with the minimum fields required to make a request.
--
-- * 'tagDescriptions' - Information about the tags.
-- * 'responseStatus' - The response status code.
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
dtsrsTagDescriptions :: Lens.Lens' DescribeTagsResponse (Lude.Maybe [TagDescription])
dtsrsTagDescriptions = Lens.lens (tagDescriptions :: DescribeTagsResponse -> Lude.Maybe [TagDescription]) (\s a -> s {tagDescriptions = a} :: DescribeTagsResponse)
{-# DEPRECATED dtsrsTagDescriptions "Use generic-lens or generic-optics with 'tagDescriptions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsrsResponseStatus :: Lens.Lens' DescribeTagsResponse Lude.Int
dtsrsResponseStatus = Lens.lens (responseStatus :: DescribeTagsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeTagsResponse)
{-# DEPRECATED dtsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
