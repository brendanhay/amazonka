{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribePrefixLists
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes available AWS services in a prefix list format, which includes the prefix list name and prefix list ID of the service and the IP address range for the service.
--
-- We recommend that you use 'DescribeManagedPrefixLists' instead.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribePrefixLists
  ( -- * Creating a request
    DescribePrefixLists (..),
    mkDescribePrefixLists,

    -- ** Request lenses
    dplFilters,
    dplPrefixListIds,
    dplNextToken,
    dplDryRun,
    dplMaxResults,

    -- * Destructuring the response
    DescribePrefixListsResponse (..),
    mkDescribePrefixListsResponse,

    -- ** Response lenses
    dplrsNextToken,
    dplrsPrefixLists,
    dplrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribePrefixLists' smart constructor.
data DescribePrefixLists = DescribePrefixLists'
  { filters ::
      Lude.Maybe [Filter],
    prefixListIds :: Lude.Maybe [Lude.Text],
    nextToken :: Lude.Maybe Lude.Text,
    dryRun :: Lude.Maybe Lude.Bool,
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribePrefixLists' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'filters' - One or more filters.
--
--
--     * @prefix-list-id@ : The ID of a prefix list.
--
--
--     * @prefix-list-name@ : The name of a prefix list.
--
--
-- * 'maxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
-- * 'nextToken' - The token for the next page of results.
-- * 'prefixListIds' - One or more prefix list IDs.
mkDescribePrefixLists ::
  DescribePrefixLists
mkDescribePrefixLists =
  DescribePrefixLists'
    { filters = Lude.Nothing,
      prefixListIds = Lude.Nothing,
      nextToken = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | One or more filters.
--
--
--     * @prefix-list-id@ : The ID of a prefix list.
--
--
--     * @prefix-list-name@ : The name of a prefix list.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dplFilters :: Lens.Lens' DescribePrefixLists (Lude.Maybe [Filter])
dplFilters = Lens.lens (filters :: DescribePrefixLists -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribePrefixLists)
{-# DEPRECATED dplFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | One or more prefix list IDs.
--
-- /Note:/ Consider using 'prefixListIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dplPrefixListIds :: Lens.Lens' DescribePrefixLists (Lude.Maybe [Lude.Text])
dplPrefixListIds = Lens.lens (prefixListIds :: DescribePrefixLists -> Lude.Maybe [Lude.Text]) (\s a -> s {prefixListIds = a} :: DescribePrefixLists)
{-# DEPRECATED dplPrefixListIds "Use generic-lens or generic-optics with 'prefixListIds' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dplNextToken :: Lens.Lens' DescribePrefixLists (Lude.Maybe Lude.Text)
dplNextToken = Lens.lens (nextToken :: DescribePrefixLists -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribePrefixLists)
{-# DEPRECATED dplNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dplDryRun :: Lens.Lens' DescribePrefixLists (Lude.Maybe Lude.Bool)
dplDryRun = Lens.lens (dryRun :: DescribePrefixLists -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribePrefixLists)
{-# DEPRECATED dplDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dplMaxResults :: Lens.Lens' DescribePrefixLists (Lude.Maybe Lude.Int)
dplMaxResults = Lens.lens (maxResults :: DescribePrefixLists -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribePrefixLists)
{-# DEPRECATED dplMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribePrefixLists where
  page rq rs
    | Page.stop (rs Lens.^. dplrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dplrsPrefixLists) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dplNextToken Lens..~ rs Lens.^. dplrsNextToken

instance Lude.AWSRequest DescribePrefixLists where
  type Rs DescribePrefixLists = DescribePrefixListsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribePrefixListsResponse'
            Lude.<$> (x Lude..@? "nextToken")
            Lude.<*> ( x Lude..@? "prefixListSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribePrefixLists where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribePrefixLists where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribePrefixLists where
  toQuery DescribePrefixLists' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribePrefixLists" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        Lude.toQuery
          (Lude.toQueryList "PrefixListId" Lude.<$> prefixListIds),
        "NextToken" Lude.=: nextToken,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribePrefixListsResponse' smart constructor.
data DescribePrefixListsResponse = DescribePrefixListsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    prefixLists ::
      Lude.Maybe [PrefixList],
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

-- | Creates a value of 'DescribePrefixListsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'prefixLists' - All available prefix lists.
-- * 'responseStatus' - The response status code.
mkDescribePrefixListsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribePrefixListsResponse
mkDescribePrefixListsResponse pResponseStatus_ =
  DescribePrefixListsResponse'
    { nextToken = Lude.Nothing,
      prefixLists = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dplrsNextToken :: Lens.Lens' DescribePrefixListsResponse (Lude.Maybe Lude.Text)
dplrsNextToken = Lens.lens (nextToken :: DescribePrefixListsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribePrefixListsResponse)
{-# DEPRECATED dplrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | All available prefix lists.
--
-- /Note:/ Consider using 'prefixLists' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dplrsPrefixLists :: Lens.Lens' DescribePrefixListsResponse (Lude.Maybe [PrefixList])
dplrsPrefixLists = Lens.lens (prefixLists :: DescribePrefixListsResponse -> Lude.Maybe [PrefixList]) (\s a -> s {prefixLists = a} :: DescribePrefixListsResponse)
{-# DEPRECATED dplrsPrefixLists "Use generic-lens or generic-optics with 'prefixLists' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dplrsResponseStatus :: Lens.Lens' DescribePrefixListsResponse Lude.Int
dplrsResponseStatus = Lens.lens (responseStatus :: DescribePrefixListsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribePrefixListsResponse)
{-# DEPRECATED dplrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
