{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.ListGroupVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the versions of a group.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListGroupVersions
  ( -- * Creating a request
    ListGroupVersions (..),
    mkListGroupVersions,

    -- ** Request lenses
    lgvNextToken,
    lgvMaxResults,
    lgvGroupId,

    -- * Destructuring the response
    ListGroupVersionsResponse (..),
    mkListGroupVersionsResponse,

    -- ** Response lenses
    lgvrsVersions,
    lgvrsNextToken,
    lgvrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListGroupVersions' smart constructor.
data ListGroupVersions = ListGroupVersions'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Text,
    groupId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListGroupVersions' with the minimum fields required to make a request.
--
-- * 'groupId' - The ID of the Greengrass group.
-- * 'maxResults' - The maximum number of results to be returned per request.
-- * 'nextToken' - The token for the next set of results, or ''null'' if there are no additional results.
mkListGroupVersions ::
  -- | 'groupId'
  Lude.Text ->
  ListGroupVersions
mkListGroupVersions pGroupId_ =
  ListGroupVersions'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      groupId = pGroupId_
    }

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgvNextToken :: Lens.Lens' ListGroupVersions (Lude.Maybe Lude.Text)
lgvNextToken = Lens.lens (nextToken :: ListGroupVersions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListGroupVersions)
{-# DEPRECATED lgvNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to be returned per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgvMaxResults :: Lens.Lens' ListGroupVersions (Lude.Maybe Lude.Text)
lgvMaxResults = Lens.lens (maxResults :: ListGroupVersions -> Lude.Maybe Lude.Text) (\s a -> s {maxResults = a} :: ListGroupVersions)
{-# DEPRECATED lgvMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The ID of the Greengrass group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgvGroupId :: Lens.Lens' ListGroupVersions Lude.Text
lgvGroupId = Lens.lens (groupId :: ListGroupVersions -> Lude.Text) (\s a -> s {groupId = a} :: ListGroupVersions)
{-# DEPRECATED lgvGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

instance Page.AWSPager ListGroupVersions where
  page rq rs
    | Page.stop (rs Lens.^. lgvrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lgvrsVersions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lgvNextToken Lens..~ rs Lens.^. lgvrsNextToken

instance Lude.AWSRequest ListGroupVersions where
  type Rs ListGroupVersions = ListGroupVersionsResponse
  request = Req.get greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListGroupVersionsResponse'
            Lude.<$> (x Lude..?> "Versions" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListGroupVersions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListGroupVersions where
  toPath ListGroupVersions' {..} =
    Lude.mconcat
      ["/greengrass/groups/", Lude.toBS groupId, "/versions"]

instance Lude.ToQuery ListGroupVersions where
  toQuery ListGroupVersions' {..} =
    Lude.mconcat
      ["NextToken" Lude.=: nextToken, "MaxResults" Lude.=: maxResults]

-- | /See:/ 'mkListGroupVersionsResponse' smart constructor.
data ListGroupVersionsResponse = ListGroupVersionsResponse'
  { versions ::
      Lude.Maybe [VersionInformation],
    nextToken :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListGroupVersionsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token for the next set of results, or ''null'' if there are no additional results.
-- * 'responseStatus' - The response status code.
-- * 'versions' - Information about a version.
mkListGroupVersionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListGroupVersionsResponse
mkListGroupVersionsResponse pResponseStatus_ =
  ListGroupVersionsResponse'
    { versions = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about a version.
--
-- /Note:/ Consider using 'versions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgvrsVersions :: Lens.Lens' ListGroupVersionsResponse (Lude.Maybe [VersionInformation])
lgvrsVersions = Lens.lens (versions :: ListGroupVersionsResponse -> Lude.Maybe [VersionInformation]) (\s a -> s {versions = a} :: ListGroupVersionsResponse)
{-# DEPRECATED lgvrsVersions "Use generic-lens or generic-optics with 'versions' instead." #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgvrsNextToken :: Lens.Lens' ListGroupVersionsResponse (Lude.Maybe Lude.Text)
lgvrsNextToken = Lens.lens (nextToken :: ListGroupVersionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListGroupVersionsResponse)
{-# DEPRECATED lgvrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgvrsResponseStatus :: Lens.Lens' ListGroupVersionsResponse Lude.Int
lgvrsResponseStatus = Lens.lens (responseStatus :: ListGroupVersionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListGroupVersionsResponse)
{-# DEPRECATED lgvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
