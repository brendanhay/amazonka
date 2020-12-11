{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.ListComplianceItems
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For a specified resource ID, this API action returns a list of compliance statuses for different resource types. Currently, you can only specify one resource ID per call. List results depend on the criteria specified in the filter.
--
-- This operation returns paginated results.
module Network.AWS.SSM.ListComplianceItems
  ( -- * Creating a request
    ListComplianceItems (..),
    mkListComplianceItems,

    -- ** Request lenses
    lResourceIds,
    lFilters,
    lNextToken,
    lMaxResults,
    lResourceTypes,

    -- * Destructuring the response
    ListComplianceItemsResponse (..),
    mkListComplianceItemsResponse,

    -- ** Response lenses
    lcirsComplianceItems,
    lcirsNextToken,
    lcirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkListComplianceItems' smart constructor.
data ListComplianceItems = ListComplianceItems'
  { resourceIds ::
      Lude.Maybe (Lude.NonEmpty Lude.Text),
    filters :: Lude.Maybe [ComplianceStringFilter],
    nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    resourceTypes ::
      Lude.Maybe (Lude.NonEmpty Lude.Text)
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListComplianceItems' with the minimum fields required to make a request.
--
-- * 'filters' - One or more compliance filters. Use a filter to return a more specific list of results.
-- * 'maxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
-- * 'nextToken' - A token to start the list. Use this token to get the next set of results.
-- * 'resourceIds' - The ID for the resources from which to get compliance information. Currently, you can only specify one resource ID.
-- * 'resourceTypes' - The type of resource from which to get compliance information. Currently, the only supported resource type is @ManagedInstance@ .
mkListComplianceItems ::
  ListComplianceItems
mkListComplianceItems =
  ListComplianceItems'
    { resourceIds = Lude.Nothing,
      filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      resourceTypes = Lude.Nothing
    }

-- | The ID for the resources from which to get compliance information. Currently, you can only specify one resource ID.
--
-- /Note:/ Consider using 'resourceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lResourceIds :: Lens.Lens' ListComplianceItems (Lude.Maybe (Lude.NonEmpty Lude.Text))
lResourceIds = Lens.lens (resourceIds :: ListComplianceItems -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {resourceIds = a} :: ListComplianceItems)
{-# DEPRECATED lResourceIds "Use generic-lens or generic-optics with 'resourceIds' instead." #-}

-- | One or more compliance filters. Use a filter to return a more specific list of results.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lFilters :: Lens.Lens' ListComplianceItems (Lude.Maybe [ComplianceStringFilter])
lFilters = Lens.lens (filters :: ListComplianceItems -> Lude.Maybe [ComplianceStringFilter]) (\s a -> s {filters = a} :: ListComplianceItems)
{-# DEPRECATED lFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | A token to start the list. Use this token to get the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lNextToken :: Lens.Lens' ListComplianceItems (Lude.Maybe Lude.Text)
lNextToken = Lens.lens (nextToken :: ListComplianceItems -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListComplianceItems)
{-# DEPRECATED lNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lMaxResults :: Lens.Lens' ListComplianceItems (Lude.Maybe Lude.Natural)
lMaxResults = Lens.lens (maxResults :: ListComplianceItems -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListComplianceItems)
{-# DEPRECATED lMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The type of resource from which to get compliance information. Currently, the only supported resource type is @ManagedInstance@ .
--
-- /Note:/ Consider using 'resourceTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lResourceTypes :: Lens.Lens' ListComplianceItems (Lude.Maybe (Lude.NonEmpty Lude.Text))
lResourceTypes = Lens.lens (resourceTypes :: ListComplianceItems -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {resourceTypes = a} :: ListComplianceItems)
{-# DEPRECATED lResourceTypes "Use generic-lens or generic-optics with 'resourceTypes' instead." #-}

instance Page.AWSPager ListComplianceItems where
  page rq rs
    | Page.stop (rs Lens.^. lcirsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lcirsComplianceItems) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lNextToken Lens..~ rs Lens.^. lcirsNextToken

instance Lude.AWSRequest ListComplianceItems where
  type Rs ListComplianceItems = ListComplianceItemsResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListComplianceItemsResponse'
            Lude.<$> (x Lude..?> "ComplianceItems" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListComplianceItems where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.ListComplianceItems" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListComplianceItems where
  toJSON ListComplianceItems' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ResourceIds" Lude..=) Lude.<$> resourceIds,
            ("Filters" Lude..=) Lude.<$> filters,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            ("ResourceTypes" Lude..=) Lude.<$> resourceTypes
          ]
      )

instance Lude.ToPath ListComplianceItems where
  toPath = Lude.const "/"

instance Lude.ToQuery ListComplianceItems where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListComplianceItemsResponse' smart constructor.
data ListComplianceItemsResponse = ListComplianceItemsResponse'
  { complianceItems ::
      Lude.Maybe [ComplianceItem],
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

-- | Creates a value of 'ListComplianceItemsResponse' with the minimum fields required to make a request.
--
-- * 'complianceItems' - A list of compliance information for the specified resource ID.
-- * 'nextToken' - The token for the next set of items to return. Use this token to get the next set of results.
-- * 'responseStatus' - The response status code.
mkListComplianceItemsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListComplianceItemsResponse
mkListComplianceItemsResponse pResponseStatus_ =
  ListComplianceItemsResponse'
    { complianceItems = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of compliance information for the specified resource ID.
--
-- /Note:/ Consider using 'complianceItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcirsComplianceItems :: Lens.Lens' ListComplianceItemsResponse (Lude.Maybe [ComplianceItem])
lcirsComplianceItems = Lens.lens (complianceItems :: ListComplianceItemsResponse -> Lude.Maybe [ComplianceItem]) (\s a -> s {complianceItems = a} :: ListComplianceItemsResponse)
{-# DEPRECATED lcirsComplianceItems "Use generic-lens or generic-optics with 'complianceItems' instead." #-}

-- | The token for the next set of items to return. Use this token to get the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcirsNextToken :: Lens.Lens' ListComplianceItemsResponse (Lude.Maybe Lude.Text)
lcirsNextToken = Lens.lens (nextToken :: ListComplianceItemsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListComplianceItemsResponse)
{-# DEPRECATED lcirsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcirsResponseStatus :: Lens.Lens' ListComplianceItemsResponse Lude.Int
lcirsResponseStatus = Lens.lens (responseStatus :: ListComplianceItemsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListComplianceItemsResponse)
{-# DEPRECATED lcirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
