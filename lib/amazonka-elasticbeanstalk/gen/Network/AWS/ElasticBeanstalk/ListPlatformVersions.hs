{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.ListPlatformVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the platform versions available for your account in an AWS Region. Provides summary information about each platform version. Compare to 'DescribePlatformVersion' , which provides full details about a single platform version.
--
-- For definitions of platform version and other platform-related terms, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/platforms-glossary.html AWS Elastic Beanstalk Platforms Glossary> .
--
-- This operation returns paginated results.
module Network.AWS.ElasticBeanstalk.ListPlatformVersions
  ( -- * Creating a request
    ListPlatformVersions (..),
    mkListPlatformVersions,

    -- ** Request lenses
    lpvFilters,
    lpvNextToken,
    lpvMaxRecords,

    -- * Destructuring the response
    ListPlatformVersionsResponse (..),
    mkListPlatformVersionsResponse,

    -- ** Response lenses
    lpvrsNextToken,
    lpvrsPlatformSummaryList,
    lpvrsResponseStatus,
  )
where

import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListPlatformVersions' smart constructor.
data ListPlatformVersions = ListPlatformVersions'
  { -- | Criteria for restricting the resulting list of platform versions. The filter is interpreted as a logical conjunction (AND) of the separate @PlatformFilter@ terms.
    filters :: Lude.Maybe [PlatformFilter],
    -- | For a paginated request. Specify a token from a previous response page to retrieve the next response page. All other parameter values must be identical to the ones specified in the initial request.
    --
    -- If no @NextToken@ is specified, the first page is retrieved.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of platform version values returned in one call.
    maxRecords :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPlatformVersions' with the minimum fields required to make a request.
--
-- * 'filters' - Criteria for restricting the resulting list of platform versions. The filter is interpreted as a logical conjunction (AND) of the separate @PlatformFilter@ terms.
-- * 'nextToken' - For a paginated request. Specify a token from a previous response page to retrieve the next response page. All other parameter values must be identical to the ones specified in the initial request.
--
-- If no @NextToken@ is specified, the first page is retrieved.
-- * 'maxRecords' - The maximum number of platform version values returned in one call.
mkListPlatformVersions ::
  ListPlatformVersions
mkListPlatformVersions =
  ListPlatformVersions'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxRecords = Lude.Nothing
    }

-- | Criteria for restricting the resulting list of platform versions. The filter is interpreted as a logical conjunction (AND) of the separate @PlatformFilter@ terms.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpvFilters :: Lens.Lens' ListPlatformVersions (Lude.Maybe [PlatformFilter])
lpvFilters = Lens.lens (filters :: ListPlatformVersions -> Lude.Maybe [PlatformFilter]) (\s a -> s {filters = a} :: ListPlatformVersions)
{-# DEPRECATED lpvFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | For a paginated request. Specify a token from a previous response page to retrieve the next response page. All other parameter values must be identical to the ones specified in the initial request.
--
-- If no @NextToken@ is specified, the first page is retrieved.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpvNextToken :: Lens.Lens' ListPlatformVersions (Lude.Maybe Lude.Text)
lpvNextToken = Lens.lens (nextToken :: ListPlatformVersions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListPlatformVersions)
{-# DEPRECATED lpvNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of platform version values returned in one call.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpvMaxRecords :: Lens.Lens' ListPlatformVersions (Lude.Maybe Lude.Natural)
lpvMaxRecords = Lens.lens (maxRecords :: ListPlatformVersions -> Lude.Maybe Lude.Natural) (\s a -> s {maxRecords = a} :: ListPlatformVersions)
{-# DEPRECATED lpvMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Page.AWSPager ListPlatformVersions where
  page rq rs
    | Page.stop (rs Lens.^. lpvrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lpvrsPlatformSummaryList) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lpvNextToken Lens..~ rs Lens.^. lpvrsNextToken

instance Lude.AWSRequest ListPlatformVersions where
  type Rs ListPlatformVersions = ListPlatformVersionsResponse
  request = Req.postQuery elasticBeanstalkService
  response =
    Res.receiveXMLWrapper
      "ListPlatformVersionsResult"
      ( \s h x ->
          ListPlatformVersionsResponse'
            Lude.<$> (x Lude..@? "NextToken")
            Lude.<*> ( x Lude..@? "PlatformSummaryList" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListPlatformVersions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListPlatformVersions where
  toPath = Lude.const "/"

instance Lude.ToQuery ListPlatformVersions where
  toQuery ListPlatformVersions' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ListPlatformVersions" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "Filters"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> filters),
        "NextToken" Lude.=: nextToken,
        "MaxRecords" Lude.=: maxRecords
      ]

-- | /See:/ 'mkListPlatformVersionsResponse' smart constructor.
data ListPlatformVersionsResponse = ListPlatformVersionsResponse'
  { -- | In a paginated request, if this value isn't @null@ , it's the token that you can pass in a subsequent request to get the next response page.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Summary information about the platform versions.
    platformSummaryList :: Lude.Maybe [PlatformSummary],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPlatformVersionsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - In a paginated request, if this value isn't @null@ , it's the token that you can pass in a subsequent request to get the next response page.
-- * 'platformSummaryList' - Summary information about the platform versions.
-- * 'responseStatus' - The response status code.
mkListPlatformVersionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListPlatformVersionsResponse
mkListPlatformVersionsResponse pResponseStatus_ =
  ListPlatformVersionsResponse'
    { nextToken = Lude.Nothing,
      platformSummaryList = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | In a paginated request, if this value isn't @null@ , it's the token that you can pass in a subsequent request to get the next response page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpvrsNextToken :: Lens.Lens' ListPlatformVersionsResponse (Lude.Maybe Lude.Text)
lpvrsNextToken = Lens.lens (nextToken :: ListPlatformVersionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListPlatformVersionsResponse)
{-# DEPRECATED lpvrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Summary information about the platform versions.
--
-- /Note:/ Consider using 'platformSummaryList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpvrsPlatformSummaryList :: Lens.Lens' ListPlatformVersionsResponse (Lude.Maybe [PlatformSummary])
lpvrsPlatformSummaryList = Lens.lens (platformSummaryList :: ListPlatformVersionsResponse -> Lude.Maybe [PlatformSummary]) (\s a -> s {platformSummaryList = a} :: ListPlatformVersionsResponse)
{-# DEPRECATED lpvrsPlatformSummaryList "Use generic-lens or generic-optics with 'platformSummaryList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpvrsResponseStatus :: Lens.Lens' ListPlatformVersionsResponse Lude.Int
lpvrsResponseStatus = Lens.lens (responseStatus :: ListPlatformVersionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListPlatformVersionsResponse)
{-# DEPRECATED lpvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
