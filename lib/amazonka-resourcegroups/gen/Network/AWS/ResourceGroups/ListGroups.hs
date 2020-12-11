{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.ListGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of existing resource groups in your account.
--
-- This operation returns paginated results.
module Network.AWS.ResourceGroups.ListGroups
  ( -- * Creating a request
    ListGroups (..),
    mkListGroups,

    -- ** Request lenses
    lgFilters,
    lgNextToken,
    lgMaxResults,

    -- * Destructuring the response
    ListGroupsResponse (..),
    mkListGroupsResponse,

    -- ** Response lenses
    lgrsGroups,
    lgrsNextToken,
    lgrsGroupIdentifiers,
    lgrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import Network.AWS.ResourceGroups.Types
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListGroups' smart constructor.
data ListGroups = ListGroups'
  { filters :: Lude.Maybe [GroupFilter],
    nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListGroups' with the minimum fields required to make a request.
--
-- * 'filters' - Filters, formatted as 'GroupFilter' objects, that you want to apply to a @ListGroups@ operation.
--
--
--     * @resource-type@ - Filter the results to include only those of the specified resource types. Specify up to five resource types in the format @AWS::/ServiceCode/ ::/ResourceType/ @ . For example, @AWS::EC2::Instance@ , or @AWS::S3::Bucket@ .
--
--
--     * @configuration-type@ - Filter the results to include only those groups that have the specified configuration types attached. The current supported values are:
--
--     * AWS:EC2::CapacityReservationPool
--
--
--
--
-- * 'maxResults' - The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that the service might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
-- * 'nextToken' - The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value provided by a previous call's @NextToken@ response to indicate where the output should continue from.
mkListGroups ::
  ListGroups
mkListGroups =
  ListGroups'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Filters, formatted as 'GroupFilter' objects, that you want to apply to a @ListGroups@ operation.
--
--
--     * @resource-type@ - Filter the results to include only those of the specified resource types. Specify up to five resource types in the format @AWS::/ServiceCode/ ::/ResourceType/ @ . For example, @AWS::EC2::Instance@ , or @AWS::S3::Bucket@ .
--
--
--     * @configuration-type@ - Filter the results to include only those groups that have the specified configuration types attached. The current supported values are:
--
--     * AWS:EC2::CapacityReservationPool
--
--
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgFilters :: Lens.Lens' ListGroups (Lude.Maybe [GroupFilter])
lgFilters = Lens.lens (filters :: ListGroups -> Lude.Maybe [GroupFilter]) (\s a -> s {filters = a} :: ListGroups)
{-# DEPRECATED lgFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value provided by a previous call's @NextToken@ response to indicate where the output should continue from.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgNextToken :: Lens.Lens' ListGroups (Lude.Maybe Lude.Text)
lgNextToken = Lens.lens (nextToken :: ListGroups -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListGroups)
{-# DEPRECATED lgNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that the service might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgMaxResults :: Lens.Lens' ListGroups (Lude.Maybe Lude.Natural)
lgMaxResults = Lens.lens (maxResults :: ListGroups -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListGroups)
{-# DEPRECATED lgMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListGroups where
  page rq rs
    | Page.stop (rs Lens.^. lgrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lgrsGroupIdentifiers) = Lude.Nothing
    | Page.stop (rs Lens.^. lgrsGroups) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lgNextToken Lens..~ rs Lens.^. lgrsNextToken

instance Lude.AWSRequest ListGroups where
  type Rs ListGroups = ListGroupsResponse
  request = Req.postJSON resourceGroupsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListGroupsResponse'
            Lude.<$> (x Lude..?> "Groups" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "GroupIdentifiers" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListGroups where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON ListGroups where
  toJSON ListGroups' {..} =
    Lude.object
      (Lude.catMaybes [("Filters" Lude..=) Lude.<$> filters])

instance Lude.ToPath ListGroups where
  toPath = Lude.const "/groups-list"

instance Lude.ToQuery ListGroups where
  toQuery ListGroups' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | /See:/ 'mkListGroupsResponse' smart constructor.
data ListGroupsResponse = ListGroupsResponse'
  { groups ::
      Lude.Maybe [Group],
    nextToken :: Lude.Maybe Lude.Text,
    groupIdentifiers :: Lude.Maybe [GroupIdentifier],
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

-- | Creates a value of 'ListGroupsResponse' with the minimum fields required to make a request.
--
-- * 'groupIdentifiers' - A list of 'GroupIdentifier' objects. Each identifier is an object that contains both the @Name@ and the @GroupArn@ .
-- * 'groups' - This output element is deprecated and shouldn't be used. Refer to @GroupIdentifiers@ instead.
-- * 'nextToken' - If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
-- * 'responseStatus' - The response status code.
mkListGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListGroupsResponse
mkListGroupsResponse pResponseStatus_ =
  ListGroupsResponse'
    { groups = Lude.Nothing,
      nextToken = Lude.Nothing,
      groupIdentifiers = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | This output element is deprecated and shouldn't be used. Refer to @GroupIdentifiers@ instead.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrsGroups :: Lens.Lens' ListGroupsResponse (Lude.Maybe [Group])
lgrsGroups = Lens.lens (groups :: ListGroupsResponse -> Lude.Maybe [Group]) (\s a -> s {groups = a} :: ListGroupsResponse)
{-# DEPRECATED lgrsGroups "Use generic-lens or generic-optics with 'groups' instead." #-}

-- | If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrsNextToken :: Lens.Lens' ListGroupsResponse (Lude.Maybe Lude.Text)
lgrsNextToken = Lens.lens (nextToken :: ListGroupsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListGroupsResponse)
{-# DEPRECATED lgrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of 'GroupIdentifier' objects. Each identifier is an object that contains both the @Name@ and the @GroupArn@ .
--
-- /Note:/ Consider using 'groupIdentifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrsGroupIdentifiers :: Lens.Lens' ListGroupsResponse (Lude.Maybe [GroupIdentifier])
lgrsGroupIdentifiers = Lens.lens (groupIdentifiers :: ListGroupsResponse -> Lude.Maybe [GroupIdentifier]) (\s a -> s {groupIdentifiers = a} :: ListGroupsResponse)
{-# DEPRECATED lgrsGroupIdentifiers "Use generic-lens or generic-optics with 'groupIdentifiers' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrsResponseStatus :: Lens.Lens' ListGroupsResponse Lude.Int
lgrsResponseStatus = Lens.lens (responseStatus :: ListGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListGroupsResponse)
{-# DEPRECATED lgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
