{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeManagedPrefixLists
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes your managed prefix lists and any AWS-managed prefix lists.
--
-- To view the entries for your prefix list, use 'GetManagedPrefixListEntries' .
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeManagedPrefixLists
  ( -- * Creating a request
    DescribeManagedPrefixLists (..),
    mkDescribeManagedPrefixLists,

    -- ** Request lenses
    dmplsFilters,
    dmplsPrefixListIds,
    dmplsNextToken,
    dmplsDryRun,
    dmplsMaxResults,

    -- * Destructuring the response
    DescribeManagedPrefixListsResponse (..),
    mkDescribeManagedPrefixListsResponse,

    -- ** Response lenses
    dmplsrsNextToken,
    dmplsrsPrefixLists,
    dmplsrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeManagedPrefixLists' smart constructor.
data DescribeManagedPrefixLists = DescribeManagedPrefixLists'
  { -- | One or more filters.
    --
    --
    --     * @owner-id@ - The ID of the prefix list owner.
    --
    --
    --     * @prefix-list-id@ - The ID of the prefix list.
    --
    --
    --     * @prefix-list-name@ - The name of the prefix list.
    filters :: Lude.Maybe [Filter],
    -- | One or more prefix list IDs.
    prefixListIds :: Lude.Maybe [Lude.Text],
    -- | The token for the next page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeManagedPrefixLists' with the minimum fields required to make a request.
--
-- * 'filters' - One or more filters.
--
--
--     * @owner-id@ - The ID of the prefix list owner.
--
--
--     * @prefix-list-id@ - The ID of the prefix list.
--
--
--     * @prefix-list-name@ - The name of the prefix list.
--
--
-- * 'prefixListIds' - One or more prefix list IDs.
-- * 'nextToken' - The token for the next page of results.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'maxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
mkDescribeManagedPrefixLists ::
  DescribeManagedPrefixLists
mkDescribeManagedPrefixLists =
  DescribeManagedPrefixLists'
    { filters = Lude.Nothing,
      prefixListIds = Lude.Nothing,
      nextToken = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | One or more filters.
--
--
--     * @owner-id@ - The ID of the prefix list owner.
--
--
--     * @prefix-list-id@ - The ID of the prefix list.
--
--
--     * @prefix-list-name@ - The name of the prefix list.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmplsFilters :: Lens.Lens' DescribeManagedPrefixLists (Lude.Maybe [Filter])
dmplsFilters = Lens.lens (filters :: DescribeManagedPrefixLists -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeManagedPrefixLists)
{-# DEPRECATED dmplsFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | One or more prefix list IDs.
--
-- /Note:/ Consider using 'prefixListIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmplsPrefixListIds :: Lens.Lens' DescribeManagedPrefixLists (Lude.Maybe [Lude.Text])
dmplsPrefixListIds = Lens.lens (prefixListIds :: DescribeManagedPrefixLists -> Lude.Maybe [Lude.Text]) (\s a -> s {prefixListIds = a} :: DescribeManagedPrefixLists)
{-# DEPRECATED dmplsPrefixListIds "Use generic-lens or generic-optics with 'prefixListIds' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmplsNextToken :: Lens.Lens' DescribeManagedPrefixLists (Lude.Maybe Lude.Text)
dmplsNextToken = Lens.lens (nextToken :: DescribeManagedPrefixLists -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeManagedPrefixLists)
{-# DEPRECATED dmplsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmplsDryRun :: Lens.Lens' DescribeManagedPrefixLists (Lude.Maybe Lude.Bool)
dmplsDryRun = Lens.lens (dryRun :: DescribeManagedPrefixLists -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeManagedPrefixLists)
{-# DEPRECATED dmplsDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmplsMaxResults :: Lens.Lens' DescribeManagedPrefixLists (Lude.Maybe Lude.Natural)
dmplsMaxResults = Lens.lens (maxResults :: DescribeManagedPrefixLists -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeManagedPrefixLists)
{-# DEPRECATED dmplsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeManagedPrefixLists where
  page rq rs
    | Page.stop (rs Lens.^. dmplsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dmplsrsPrefixLists) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dmplsNextToken Lens..~ rs Lens.^. dmplsrsNextToken

instance Lude.AWSRequest DescribeManagedPrefixLists where
  type
    Rs DescribeManagedPrefixLists =
      DescribeManagedPrefixListsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeManagedPrefixListsResponse'
            Lude.<$> (x Lude..@? "nextToken")
            Lude.<*> ( x Lude..@? "prefixListSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeManagedPrefixLists where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeManagedPrefixLists where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeManagedPrefixLists where
  toQuery DescribeManagedPrefixLists' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeManagedPrefixLists" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        Lude.toQuery
          (Lude.toQueryList "PrefixListId" Lude.<$> prefixListIds),
        "NextToken" Lude.=: nextToken,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeManagedPrefixListsResponse' smart constructor.
data DescribeManagedPrefixListsResponse = DescribeManagedPrefixListsResponse'
  { -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Information about the prefix lists.
    prefixLists :: Lude.Maybe [ManagedPrefixList],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeManagedPrefixListsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'prefixLists' - Information about the prefix lists.
-- * 'responseStatus' - The response status code.
mkDescribeManagedPrefixListsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeManagedPrefixListsResponse
mkDescribeManagedPrefixListsResponse pResponseStatus_ =
  DescribeManagedPrefixListsResponse'
    { nextToken = Lude.Nothing,
      prefixLists = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmplsrsNextToken :: Lens.Lens' DescribeManagedPrefixListsResponse (Lude.Maybe Lude.Text)
dmplsrsNextToken = Lens.lens (nextToken :: DescribeManagedPrefixListsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeManagedPrefixListsResponse)
{-# DEPRECATED dmplsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the prefix lists.
--
-- /Note:/ Consider using 'prefixLists' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmplsrsPrefixLists :: Lens.Lens' DescribeManagedPrefixListsResponse (Lude.Maybe [ManagedPrefixList])
dmplsrsPrefixLists = Lens.lens (prefixLists :: DescribeManagedPrefixListsResponse -> Lude.Maybe [ManagedPrefixList]) (\s a -> s {prefixLists = a} :: DescribeManagedPrefixListsResponse)
{-# DEPRECATED dmplsrsPrefixLists "Use generic-lens or generic-optics with 'prefixLists' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmplsrsResponseStatus :: Lens.Lens' DescribeManagedPrefixListsResponse Lude.Int
dmplsrsResponseStatus = Lens.lens (responseStatus :: DescribeManagedPrefixListsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeManagedPrefixListsResponse)
{-# DEPRECATED dmplsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
