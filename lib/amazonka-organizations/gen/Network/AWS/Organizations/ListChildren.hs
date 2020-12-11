{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.ListChildren
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of the organizational units (OUs) or accounts that are contained in the specified parent OU or root. This operation, along with 'ListParents' enables you to traverse the tree structure that makes up this root.
--
-- This operation can be called only from the organization's management account or by a member account that is a delegated administrator for an AWS service.
--
-- This operation returns paginated results.
module Network.AWS.Organizations.ListChildren
  ( -- * Creating a request
    ListChildren (..),
    mkListChildren,

    -- ** Request lenses
    lcNextToken,
    lcMaxResults,
    lcParentId,
    lcChildType,

    -- * Destructuring the response
    ListChildrenResponse (..),
    mkListChildrenResponse,

    -- ** Response lenses
    lcrsChildren,
    lcrsNextToken,
    lcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListChildren' smart constructor.
data ListChildren = ListChildren'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    parentId :: Lude.Text,
    childType :: ChildType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListChildren' with the minimum fields required to make a request.
--
-- * 'childType' - Filters the output to include only the specified child type.
-- * 'maxResults' - The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
-- * 'nextToken' - The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
-- * 'parentId' - The unique identifier (ID) for the parent root or OU whose children you want to list.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a parent ID string requires one of the following:
--
--     * __Root__ - A string that begins with "r-" followed by from 4 to 32 lowercase letters or digits.
--
--
--     * __Organizational unit (OU)__ - A string that begins with "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that the OU is in). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
mkListChildren ::
  -- | 'parentId'
  Lude.Text ->
  -- | 'childType'
  ChildType ->
  ListChildren
mkListChildren pParentId_ pChildType_ =
  ListChildren'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      parentId = pParentId_,
      childType = pChildType_
    }

-- | The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcNextToken :: Lens.Lens' ListChildren (Lude.Maybe Lude.Text)
lcNextToken = Lens.lens (nextToken :: ListChildren -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListChildren)
{-# DEPRECATED lcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcMaxResults :: Lens.Lens' ListChildren (Lude.Maybe Lude.Natural)
lcMaxResults = Lens.lens (maxResults :: ListChildren -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListChildren)
{-# DEPRECATED lcMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The unique identifier (ID) for the parent root or OU whose children you want to list.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a parent ID string requires one of the following:
--
--     * __Root__ - A string that begins with "r-" followed by from 4 to 32 lowercase letters or digits.
--
--
--     * __Organizational unit (OU)__ - A string that begins with "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that the OU is in). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
--
--
--
-- /Note:/ Consider using 'parentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcParentId :: Lens.Lens' ListChildren Lude.Text
lcParentId = Lens.lens (parentId :: ListChildren -> Lude.Text) (\s a -> s {parentId = a} :: ListChildren)
{-# DEPRECATED lcParentId "Use generic-lens or generic-optics with 'parentId' instead." #-}

-- | Filters the output to include only the specified child type.
--
-- /Note:/ Consider using 'childType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcChildType :: Lens.Lens' ListChildren ChildType
lcChildType = Lens.lens (childType :: ListChildren -> ChildType) (\s a -> s {childType = a} :: ListChildren)
{-# DEPRECATED lcChildType "Use generic-lens or generic-optics with 'childType' instead." #-}

instance Page.AWSPager ListChildren where
  page rq rs
    | Page.stop (rs Lens.^. lcrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lcrsChildren) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lcNextToken Lens..~ rs Lens.^. lcrsNextToken

instance Lude.AWSRequest ListChildren where
  type Rs ListChildren = ListChildrenResponse
  request = Req.postJSON organizationsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListChildrenResponse'
            Lude.<$> (x Lude..?> "Children" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListChildren where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSOrganizationsV20161128.ListChildren" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListChildren where
  toJSON ListChildren' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("ParentId" Lude..= parentId),
            Lude.Just ("ChildType" Lude..= childType)
          ]
      )

instance Lude.ToPath ListChildren where
  toPath = Lude.const "/"

instance Lude.ToQuery ListChildren where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListChildrenResponse' smart constructor.
data ListChildrenResponse = ListChildrenResponse'
  { children ::
      Lude.Maybe [Child],
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

-- | Creates a value of 'ListChildrenResponse' with the minimum fields required to make a request.
--
-- * 'children' - The list of children of the specified parent container.
-- * 'nextToken' - If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
-- * 'responseStatus' - The response status code.
mkListChildrenResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListChildrenResponse
mkListChildrenResponse pResponseStatus_ =
  ListChildrenResponse'
    { children = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of children of the specified parent container.
--
-- /Note:/ Consider using 'children' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrsChildren :: Lens.Lens' ListChildrenResponse (Lude.Maybe [Child])
lcrsChildren = Lens.lens (children :: ListChildrenResponse -> Lude.Maybe [Child]) (\s a -> s {children = a} :: ListChildrenResponse)
{-# DEPRECATED lcrsChildren "Use generic-lens or generic-optics with 'children' instead." #-}

-- | If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrsNextToken :: Lens.Lens' ListChildrenResponse (Lude.Maybe Lude.Text)
lcrsNextToken = Lens.lens (nextToken :: ListChildrenResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListChildrenResponse)
{-# DEPRECATED lcrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrsResponseStatus :: Lens.Lens' ListChildrenResponse Lude.Int
lcrsResponseStatus = Lens.lens (responseStatus :: ListChildrenResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListChildrenResponse)
{-# DEPRECATED lcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
