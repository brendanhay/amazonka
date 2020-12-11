{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.ListParents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the root or organizational units (OUs) that serve as the immediate parent of the specified child OU or account. This operation, along with 'ListChildren' enables you to traverse the tree structure that makes up this root.
--
-- This operation can be called only from the organization's management account or by a member account that is a delegated administrator for an AWS service.
--
-- This operation returns paginated results.
module Network.AWS.Organizations.ListParents
  ( -- * Creating a request
    ListParents (..),
    mkListParents,

    -- ** Request lenses
    lNextToken,
    lMaxResults,
    lChildId,

    -- * Destructuring the response
    ListParentsResponse (..),
    mkListParentsResponse,

    -- ** Response lenses
    lrsNextToken,
    lrsParents,
    lrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListParents' smart constructor.
data ListParents = ListParents'
  { nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    childId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListParents' with the minimum fields required to make a request.
--
-- * 'childId' - The unique identifier (ID) of the OU or account whose parent containers you want to list. Don't specify a root.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a child ID string requires one of the following:
--
--     * __Account__ - A string that consists of exactly 12 digits.
--
--
--     * __Organizational unit (OU)__ - A string that begins with "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that contains the OU). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
--
--
-- * 'maxResults' - The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
-- * 'nextToken' - The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
mkListParents ::
  -- | 'childId'
  Lude.Text ->
  ListParents
mkListParents pChildId_ =
  ListParents'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      childId = pChildId_
    }

-- | The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lNextToken :: Lens.Lens' ListParents (Lude.Maybe Lude.Text)
lNextToken = Lens.lens (nextToken :: ListParents -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListParents)
{-# DEPRECATED lNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lMaxResults :: Lens.Lens' ListParents (Lude.Maybe Lude.Natural)
lMaxResults = Lens.lens (maxResults :: ListParents -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListParents)
{-# DEPRECATED lMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The unique identifier (ID) of the OU or account whose parent containers you want to list. Don't specify a root.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a child ID string requires one of the following:
--
--     * __Account__ - A string that consists of exactly 12 digits.
--
--
--     * __Organizational unit (OU)__ - A string that begins with "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that contains the OU). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
--
--
--
-- /Note:/ Consider using 'childId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lChildId :: Lens.Lens' ListParents Lude.Text
lChildId = Lens.lens (childId :: ListParents -> Lude.Text) (\s a -> s {childId = a} :: ListParents)
{-# DEPRECATED lChildId "Use generic-lens or generic-optics with 'childId' instead." #-}

instance Page.AWSPager ListParents where
  page rq rs
    | Page.stop (rs Lens.^. lrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lrsParents) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lNextToken Lens..~ rs Lens.^. lrsNextToken

instance Lude.AWSRequest ListParents where
  type Rs ListParents = ListParentsResponse
  request = Req.postJSON organizationsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListParentsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Parents" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListParents where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSOrganizationsV20161128.ListParents" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListParents where
  toJSON ListParents' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("ChildId" Lude..= childId)
          ]
      )

instance Lude.ToPath ListParents where
  toPath = Lude.const "/"

instance Lude.ToQuery ListParents where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListParentsResponse' smart constructor.
data ListParentsResponse = ListParentsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    parents :: Lude.Maybe [Parent],
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

-- | Creates a value of 'ListParentsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
-- * 'parents' - A list of parents for the specified child account or OU.
-- * 'responseStatus' - The response status code.
mkListParentsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListParentsResponse
mkListParentsResponse pResponseStatus_ =
  ListParentsResponse'
    { nextToken = Lude.Nothing,
      parents = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsNextToken :: Lens.Lens' ListParentsResponse (Lude.Maybe Lude.Text)
lrsNextToken = Lens.lens (nextToken :: ListParentsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListParentsResponse)
{-# DEPRECATED lrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of parents for the specified child account or OU.
--
-- /Note:/ Consider using 'parents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsParents :: Lens.Lens' ListParentsResponse (Lude.Maybe [Parent])
lrsParents = Lens.lens (parents :: ListParentsResponse -> Lude.Maybe [Parent]) (\s a -> s {parents = a} :: ListParentsResponse)
{-# DEPRECATED lrsParents "Use generic-lens or generic-optics with 'parents' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsResponseStatus :: Lens.Lens' ListParentsResponse Lude.Int
lrsResponseStatus = Lens.lens (responseStatus :: ListParentsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListParentsResponse)
{-# DEPRECATED lrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
