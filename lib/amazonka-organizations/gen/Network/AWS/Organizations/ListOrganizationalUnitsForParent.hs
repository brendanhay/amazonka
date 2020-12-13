{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.ListOrganizationalUnitsForParent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the organizational units (OUs) in a parent organizational unit or root.
--
-- This operation can be called only from the organization's management account or by a member account that is a delegated administrator for an AWS service.
--
-- This operation returns paginated results.
module Network.AWS.Organizations.ListOrganizationalUnitsForParent
  ( -- * Creating a request
    ListOrganizationalUnitsForParent (..),
    mkListOrganizationalUnitsForParent,

    -- ** Request lenses
    loufpNextToken,
    loufpMaxResults,
    loufpParentId,

    -- * Destructuring the response
    ListOrganizationalUnitsForParentResponse (..),
    mkListOrganizationalUnitsForParentResponse,

    -- ** Response lenses
    loufprsNextToken,
    loufprsOrganizationalUnits,
    loufprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListOrganizationalUnitsForParent' smart constructor.
data ListOrganizationalUnitsForParent = ListOrganizationalUnitsForParent'
  { -- | The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
    maxResults :: Lude.Maybe Lude.Natural,
    -- | The unique identifier (ID) of the root or OU whose child OUs you want to list.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for a parent ID string requires one of the following:
    --
    --     * __Root__ - A string that begins with "r-" followed by from 4 to 32 lowercase letters or digits.
    --
    --
    --     * __Organizational unit (OU)__ - A string that begins with "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that the OU is in). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
    parentId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListOrganizationalUnitsForParent' with the minimum fields required to make a request.
--
-- * 'nextToken' - The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
-- * 'maxResults' - The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
-- * 'parentId' - The unique identifier (ID) of the root or OU whose child OUs you want to list.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a parent ID string requires one of the following:
--
--     * __Root__ - A string that begins with "r-" followed by from 4 to 32 lowercase letters or digits.
--
--
--     * __Organizational unit (OU)__ - A string that begins with "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that the OU is in). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
mkListOrganizationalUnitsForParent ::
  -- | 'parentId'
  Lude.Text ->
  ListOrganizationalUnitsForParent
mkListOrganizationalUnitsForParent pParentId_ =
  ListOrganizationalUnitsForParent'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      parentId = pParentId_
    }

-- | The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loufpNextToken :: Lens.Lens' ListOrganizationalUnitsForParent (Lude.Maybe Lude.Text)
loufpNextToken = Lens.lens (nextToken :: ListOrganizationalUnitsForParent -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListOrganizationalUnitsForParent)
{-# DEPRECATED loufpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loufpMaxResults :: Lens.Lens' ListOrganizationalUnitsForParent (Lude.Maybe Lude.Natural)
loufpMaxResults = Lens.lens (maxResults :: ListOrganizationalUnitsForParent -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListOrganizationalUnitsForParent)
{-# DEPRECATED loufpMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The unique identifier (ID) of the root or OU whose child OUs you want to list.
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
loufpParentId :: Lens.Lens' ListOrganizationalUnitsForParent Lude.Text
loufpParentId = Lens.lens (parentId :: ListOrganizationalUnitsForParent -> Lude.Text) (\s a -> s {parentId = a} :: ListOrganizationalUnitsForParent)
{-# DEPRECATED loufpParentId "Use generic-lens or generic-optics with 'parentId' instead." #-}

instance Page.AWSPager ListOrganizationalUnitsForParent where
  page rq rs
    | Page.stop (rs Lens.^. loufprsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. loufprsOrganizationalUnits) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& loufpNextToken Lens..~ rs Lens.^. loufprsNextToken

instance Lude.AWSRequest ListOrganizationalUnitsForParent where
  type
    Rs ListOrganizationalUnitsForParent =
      ListOrganizationalUnitsForParentResponse
  request = Req.postJSON organizationsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListOrganizationalUnitsForParentResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "OrganizationalUnits" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListOrganizationalUnitsForParent where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSOrganizationsV20161128.ListOrganizationalUnitsForParent" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListOrganizationalUnitsForParent where
  toJSON ListOrganizationalUnitsForParent' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("ParentId" Lude..= parentId)
          ]
      )

instance Lude.ToPath ListOrganizationalUnitsForParent where
  toPath = Lude.const "/"

instance Lude.ToQuery ListOrganizationalUnitsForParent where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListOrganizationalUnitsForParentResponse' smart constructor.
data ListOrganizationalUnitsForParentResponse = ListOrganizationalUnitsForParentResponse'
  { -- | If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list of the OUs in the specified root or parent OU.
    organizationalUnits :: Lude.Maybe [OrganizationalUnit],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListOrganizationalUnitsForParentResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
-- * 'organizationalUnits' - A list of the OUs in the specified root or parent OU.
-- * 'responseStatus' - The response status code.
mkListOrganizationalUnitsForParentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListOrganizationalUnitsForParentResponse
mkListOrganizationalUnitsForParentResponse pResponseStatus_ =
  ListOrganizationalUnitsForParentResponse'
    { nextToken =
        Lude.Nothing,
      organizationalUnits = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loufprsNextToken :: Lens.Lens' ListOrganizationalUnitsForParentResponse (Lude.Maybe Lude.Text)
loufprsNextToken = Lens.lens (nextToken :: ListOrganizationalUnitsForParentResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListOrganizationalUnitsForParentResponse)
{-# DEPRECATED loufprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of the OUs in the specified root or parent OU.
--
-- /Note:/ Consider using 'organizationalUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loufprsOrganizationalUnits :: Lens.Lens' ListOrganizationalUnitsForParentResponse (Lude.Maybe [OrganizationalUnit])
loufprsOrganizationalUnits = Lens.lens (organizationalUnits :: ListOrganizationalUnitsForParentResponse -> Lude.Maybe [OrganizationalUnit]) (\s a -> s {organizationalUnits = a} :: ListOrganizationalUnitsForParentResponse)
{-# DEPRECATED loufprsOrganizationalUnits "Use generic-lens or generic-optics with 'organizationalUnits' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loufprsResponseStatus :: Lens.Lens' ListOrganizationalUnitsForParentResponse Lude.Int
loufprsResponseStatus = Lens.lens (responseStatus :: ListOrganizationalUnitsForParentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListOrganizationalUnitsForParentResponse)
{-# DEPRECATED loufprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
