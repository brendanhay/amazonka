{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.SearchResources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of AWS resource identifiers that matches the specified query. The query uses the same format as a resource query in a CreateGroup or UpdateGroupQuery operation.
--
-- This operation returns paginated results.
module Network.AWS.ResourceGroups.SearchResources
  ( -- * Creating a request
    SearchResources (..),
    mkSearchResources,

    -- ** Request lenses
    srResourceQuery,
    srNextToken,
    srMaxResults,

    -- * Destructuring the response
    SearchResourcesResponse (..),
    mkSearchResourcesResponse,

    -- ** Response lenses
    srrsQueryErrors,
    srrsNextToken,
    srrsResourceIdentifiers,
    srrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import Network.AWS.ResourceGroups.Types
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSearchResources' smart constructor.
data SearchResources = SearchResources'
  { -- | The search query, using the same formats that are supported for resource group definition. For more information, see 'CreateGroup' .
    resourceQuery :: ResourceQuery,
    -- | The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value provided by a previous call's @NextToken@ response to indicate where the output should continue from.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that the service might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SearchResources' with the minimum fields required to make a request.
--
-- * 'resourceQuery' - The search query, using the same formats that are supported for resource group definition. For more information, see 'CreateGroup' .
-- * 'nextToken' - The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value provided by a previous call's @NextToken@ response to indicate where the output should continue from.
-- * 'maxResults' - The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that the service might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
mkSearchResources ::
  -- | 'resourceQuery'
  ResourceQuery ->
  SearchResources
mkSearchResources pResourceQuery_ =
  SearchResources'
    { resourceQuery = pResourceQuery_,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The search query, using the same formats that are supported for resource group definition. For more information, see 'CreateGroup' .
--
-- /Note:/ Consider using 'resourceQuery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srResourceQuery :: Lens.Lens' SearchResources ResourceQuery
srResourceQuery = Lens.lens (resourceQuery :: SearchResources -> ResourceQuery) (\s a -> s {resourceQuery = a} :: SearchResources)
{-# DEPRECATED srResourceQuery "Use generic-lens or generic-optics with 'resourceQuery' instead." #-}

-- | The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value provided by a previous call's @NextToken@ response to indicate where the output should continue from.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srNextToken :: Lens.Lens' SearchResources (Lude.Maybe Lude.Text)
srNextToken = Lens.lens (nextToken :: SearchResources -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: SearchResources)
{-# DEPRECATED srNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that the service might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srMaxResults :: Lens.Lens' SearchResources (Lude.Maybe Lude.Natural)
srMaxResults = Lens.lens (maxResults :: SearchResources -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: SearchResources)
{-# DEPRECATED srMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager SearchResources where
  page rq rs
    | Page.stop (rs Lens.^. srrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. srrsResourceIdentifiers) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& srNextToken Lens..~ rs Lens.^. srrsNextToken

instance Lude.AWSRequest SearchResources where
  type Rs SearchResources = SearchResourcesResponse
  request = Req.postJSON resourceGroupsService
  response =
    Res.receiveJSON
      ( \s h x ->
          SearchResourcesResponse'
            Lude.<$> (x Lude..?> "QueryErrors" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "ResourceIdentifiers" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SearchResources where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON SearchResources where
  toJSON SearchResources' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ResourceQuery" Lude..= resourceQuery),
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath SearchResources where
  toPath = Lude.const "/resources/search"

instance Lude.ToQuery SearchResources where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSearchResourcesResponse' smart constructor.
data SearchResourcesResponse = SearchResourcesResponse'
  { -- | A list of @QueryError@ objects. Each error is an object that contains @ErrorCode@ and @Message@ structures. Possible values for @ErrorCode@ are @CLOUDFORMATION_STACK_INACTIVE@ and @CLOUDFORMATION_STACK_NOT_EXISTING@ .
    queryErrors :: Lude.Maybe [QueryError],
    -- | If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
    nextToken :: Lude.Maybe Lude.Text,
    -- | The ARNs and resource types of resources that are members of the group that you specified.
    resourceIdentifiers :: Lude.Maybe [ResourceIdentifier],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SearchResourcesResponse' with the minimum fields required to make a request.
--
-- * 'queryErrors' - A list of @QueryError@ objects. Each error is an object that contains @ErrorCode@ and @Message@ structures. Possible values for @ErrorCode@ are @CLOUDFORMATION_STACK_INACTIVE@ and @CLOUDFORMATION_STACK_NOT_EXISTING@ .
-- * 'nextToken' - If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
-- * 'resourceIdentifiers' - The ARNs and resource types of resources that are members of the group that you specified.
-- * 'responseStatus' - The response status code.
mkSearchResourcesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SearchResourcesResponse
mkSearchResourcesResponse pResponseStatus_ =
  SearchResourcesResponse'
    { queryErrors = Lude.Nothing,
      nextToken = Lude.Nothing,
      resourceIdentifiers = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of @QueryError@ objects. Each error is an object that contains @ErrorCode@ and @Message@ structures. Possible values for @ErrorCode@ are @CLOUDFORMATION_STACK_INACTIVE@ and @CLOUDFORMATION_STACK_NOT_EXISTING@ .
--
-- /Note:/ Consider using 'queryErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrsQueryErrors :: Lens.Lens' SearchResourcesResponse (Lude.Maybe [QueryError])
srrsQueryErrors = Lens.lens (queryErrors :: SearchResourcesResponse -> Lude.Maybe [QueryError]) (\s a -> s {queryErrors = a} :: SearchResourcesResponse)
{-# DEPRECATED srrsQueryErrors "Use generic-lens or generic-optics with 'queryErrors' instead." #-}

-- | If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrsNextToken :: Lens.Lens' SearchResourcesResponse (Lude.Maybe Lude.Text)
srrsNextToken = Lens.lens (nextToken :: SearchResourcesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: SearchResourcesResponse)
{-# DEPRECATED srrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The ARNs and resource types of resources that are members of the group that you specified.
--
-- /Note:/ Consider using 'resourceIdentifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrsResourceIdentifiers :: Lens.Lens' SearchResourcesResponse (Lude.Maybe [ResourceIdentifier])
srrsResourceIdentifiers = Lens.lens (resourceIdentifiers :: SearchResourcesResponse -> Lude.Maybe [ResourceIdentifier]) (\s a -> s {resourceIdentifiers = a} :: SearchResourcesResponse)
{-# DEPRECATED srrsResourceIdentifiers "Use generic-lens or generic-optics with 'resourceIdentifiers' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrsResponseStatus :: Lens.Lens' SearchResourcesResponse Lude.Int
srrsResponseStatus = Lens.lens (responseStatus :: SearchResourcesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SearchResourcesResponse)
{-# DEPRECATED srrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
