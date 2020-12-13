{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.DescribeTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the tags associated with the specified AWS Direct Connect resources.
module Network.AWS.DirectConnect.DescribeTags
  ( -- * Creating a request
    DescribeTags (..),
    mkDescribeTags,

    -- ** Request lenses
    dtResourceARNs,

    -- * Destructuring the response
    DescribeTagsResponse (..),
    mkDescribeTagsResponse,

    -- ** Response lenses
    dtrsResourceTags,
    dtrsResponseStatus,
  )
where

import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeTags' smart constructor.
newtype DescribeTags = DescribeTags'
  { -- | The Amazon Resource Names (ARNs) of the resources.
    resourceARNs :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTags' with the minimum fields required to make a request.
--
-- * 'resourceARNs' - The Amazon Resource Names (ARNs) of the resources.
mkDescribeTags ::
  DescribeTags
mkDescribeTags = DescribeTags' {resourceARNs = Lude.mempty}

-- | The Amazon Resource Names (ARNs) of the resources.
--
-- /Note:/ Consider using 'resourceARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtResourceARNs :: Lens.Lens' DescribeTags [Lude.Text]
dtResourceARNs = Lens.lens (resourceARNs :: DescribeTags -> [Lude.Text]) (\s a -> s {resourceARNs = a} :: DescribeTags)
{-# DEPRECATED dtResourceARNs "Use generic-lens or generic-optics with 'resourceARNs' instead." #-}

instance Lude.AWSRequest DescribeTags where
  type Rs DescribeTags = DescribeTagsResponse
  request = Req.postJSON directConnectService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeTagsResponse'
            Lude.<$> (x Lude..?> "resourceTags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeTags where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OvertureService.DescribeTags" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeTags where
  toJSON DescribeTags' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("resourceArns" Lude..= resourceARNs)])

instance Lude.ToPath DescribeTags where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeTags where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeTagsResponse' smart constructor.
data DescribeTagsResponse = DescribeTagsResponse'
  { -- | Information about the tags.
    resourceTags :: Lude.Maybe [ResourceTag],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTagsResponse' with the minimum fields required to make a request.
--
-- * 'resourceTags' - Information about the tags.
-- * 'responseStatus' - The response status code.
mkDescribeTagsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeTagsResponse
mkDescribeTagsResponse pResponseStatus_ =
  DescribeTagsResponse'
    { resourceTags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the tags.
--
-- /Note:/ Consider using 'resourceTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsResourceTags :: Lens.Lens' DescribeTagsResponse (Lude.Maybe [ResourceTag])
dtrsResourceTags = Lens.lens (resourceTags :: DescribeTagsResponse -> Lude.Maybe [ResourceTag]) (\s a -> s {resourceTags = a} :: DescribeTagsResponse)
{-# DEPRECATED dtrsResourceTags "Use generic-lens or generic-optics with 'resourceTags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsResponseStatus :: Lens.Lens' DescribeTagsResponse Lude.Int
dtrsResponseStatus = Lens.lens (responseStatus :: DescribeTagsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeTagsResponse)
{-# DEPRECATED dtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
