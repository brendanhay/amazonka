{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.ListTagsForResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists tags that are attached to the specified resource.
--
-- You can attach tags to the following resources in AWS Organizations.
--
--     * AWS account
--
--
--     * Organization root
--
--
--     * Organizational unit (OU)
--
--
--     * Policy (any type)
--
--
-- This operation can be called only from the organization's management account or by a member account that is a delegated administrator for an AWS service.
--
-- This operation returns paginated results.
module Network.AWS.Organizations.ListTagsForResource
  ( -- * Creating a request
    ListTagsForResource (..),
    mkListTagsForResource,

    -- ** Request lenses
    ltfrNextToken,
    ltfrResourceId,

    -- * Destructuring the response
    ListTagsForResourceResponse (..),
    mkListTagsForResourceResponse,

    -- ** Response lenses
    ltfrrsNextToken,
    ltfrrsTags,
    ltfrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListTagsForResource' smart constructor.
data ListTagsForResource = ListTagsForResource'
  { nextToken ::
      Lude.Maybe Lude.Text,
    resourceId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTagsForResource' with the minimum fields required to make a request.
--
-- * 'nextToken' - The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
-- * 'resourceId' - The ID of the resource with the tags to list.
--
-- You can specify any of the following taggable resources.
--
--     * AWS account – specify the account ID number.
--
--
--     * Organizational unit – specify the OU ID that begins with @ou-@ and looks similar to: @ou-/1a2b-34uvwxyz/ @
--
--
--     * Root – specify the root ID that begins with @r-@ and looks similar to: @r-/1a2b/ @
--
--
--     * Policy – specify the policy ID that begins with @p-@ andlooks similar to: @p-/12abcdefg3/ @
mkListTagsForResource ::
  -- | 'resourceId'
  Lude.Text ->
  ListTagsForResource
mkListTagsForResource pResourceId_ =
  ListTagsForResource'
    { nextToken = Lude.Nothing,
      resourceId = pResourceId_
    }

-- | The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrNextToken :: Lens.Lens' ListTagsForResource (Lude.Maybe Lude.Text)
ltfrNextToken = Lens.lens (nextToken :: ListTagsForResource -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTagsForResource)
{-# DEPRECATED ltfrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The ID of the resource with the tags to list.
--
-- You can specify any of the following taggable resources.
--
--     * AWS account – specify the account ID number.
--
--
--     * Organizational unit – specify the OU ID that begins with @ou-@ and looks similar to: @ou-/1a2b-34uvwxyz/ @
--
--
--     * Root – specify the root ID that begins with @r-@ and looks similar to: @r-/1a2b/ @
--
--
--     * Policy – specify the policy ID that begins with @p-@ andlooks similar to: @p-/12abcdefg3/ @
--
--
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrResourceId :: Lens.Lens' ListTagsForResource Lude.Text
ltfrResourceId = Lens.lens (resourceId :: ListTagsForResource -> Lude.Text) (\s a -> s {resourceId = a} :: ListTagsForResource)
{-# DEPRECATED ltfrResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

instance Page.AWSPager ListTagsForResource where
  page rq rs
    | Page.stop (rs Lens.^. ltfrrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ltfrrsTags) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ltfrNextToken Lens..~ rs Lens.^. ltfrrsNextToken

instance Lude.AWSRequest ListTagsForResource where
  type Rs ListTagsForResource = ListTagsForResourceResponse
  request = Req.postJSON organizationsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListTagsForResourceResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListTagsForResource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSOrganizationsV20161128.ListTagsForResource" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListTagsForResource where
  toJSON ListTagsForResource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            Lude.Just ("ResourceId" Lude..= resourceId)
          ]
      )

instance Lude.ToPath ListTagsForResource where
  toPath = Lude.const "/"

instance Lude.ToQuery ListTagsForResource where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListTagsForResourceResponse' smart constructor.
data ListTagsForResourceResponse = ListTagsForResourceResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListTagsForResourceResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
-- * 'responseStatus' - The response status code.
-- * 'tags' - The tags that are assigned to the resource.
mkListTagsForResourceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListTagsForResourceResponse
mkListTagsForResourceResponse pResponseStatus_ =
  ListTagsForResourceResponse'
    { nextToken = Lude.Nothing,
      tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrrsNextToken :: Lens.Lens' ListTagsForResourceResponse (Lude.Maybe Lude.Text)
ltfrrsNextToken = Lens.lens (nextToken :: ListTagsForResourceResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTagsForResourceResponse)
{-# DEPRECATED ltfrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The tags that are assigned to the resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrrsTags :: Lens.Lens' ListTagsForResourceResponse (Lude.Maybe [Tag])
ltfrrsTags = Lens.lens (tags :: ListTagsForResourceResponse -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: ListTagsForResourceResponse)
{-# DEPRECATED ltfrrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrrsResponseStatus :: Lens.Lens' ListTagsForResourceResponse Lude.Int
ltfrrsResponseStatus = Lens.lens (responseStatus :: ListTagsForResourceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTagsForResourceResponse)
{-# DEPRECATED ltfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
