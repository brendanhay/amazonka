{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.ListPlatformBranches
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the platform branches available for your account in an AWS Region. Provides summary information about each platform branch.
--
-- For definitions of platform branch and other platform-related terms, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/platforms-glossary.html AWS Elastic Beanstalk Platforms Glossary> .
module Network.AWS.ElasticBeanstalk.ListPlatformBranches
  ( -- * Creating a request
    ListPlatformBranches (..),
    mkListPlatformBranches,

    -- ** Request lenses
    lpbFilters,
    lpbNextToken,
    lpbMaxRecords,

    -- * Destructuring the response
    ListPlatformBranchesResponse (..),
    mkListPlatformBranchesResponse,

    -- ** Response lenses
    lpbrsPlatformBranchSummaryList,
    lpbrsNextToken,
    lpbrsResponseStatus,
  )
where

import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListPlatformBranches' smart constructor.
data ListPlatformBranches = ListPlatformBranches'
  { -- | Criteria for restricting the resulting list of platform branches. The filter is evaluated as a logical conjunction (AND) of the separate @SearchFilter@ terms.
    --
    -- The following list shows valid attribute values for each of the @SearchFilter@ terms. Most operators take a single value. The @in@ and @not_in@ operators can take multiple values.
    --
    --     * @Attribute = BranchName@ :
    --
    --     * @Operator@ : @=@ | @!=@ | @begins_with@ | @ends_with@ | @contains@ | @in@ | @not_in@
    --
    --
    --
    --
    --     * @Attribute = LifecycleState@ :
    --
    --     * @Operator@ : @=@ | @!=@ | @in@ | @not_in@
    --
    --
    --     * @Values@ : @beta@ | @supported@ | @deprecated@ | @retired@
    --
    --
    --
    --
    --     * @Attribute = PlatformName@ :
    --
    --     * @Operator@ : @=@ | @!=@ | @begins_with@ | @ends_with@ | @contains@ | @in@ | @not_in@
    --
    --
    --
    --
    --     * @Attribute = TierType@ :
    --
    --     * @Operator@ : @=@ | @!=@
    --
    --
    --     * @Values@ : @WebServer/Standard@ | @Worker/SQS/HTTP@
    --
    --
    --
    --
    -- Array size: limited to 10 @SearchFilter@ objects.
    -- Within each @SearchFilter@ item, the @Values@ array is limited to 10 items.
    filters :: Lude.Maybe [SearchFilter],
    -- | For a paginated request. Specify a token from a previous response page to retrieve the next response page. All other parameter values must be identical to the ones specified in the initial request.
    --
    -- If no @NextToken@ is specified, the first page is retrieved.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of platform branch values returned in one call.
    maxRecords :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPlatformBranches' with the minimum fields required to make a request.
--
-- * 'filters' - Criteria for restricting the resulting list of platform branches. The filter is evaluated as a logical conjunction (AND) of the separate @SearchFilter@ terms.
--
-- The following list shows valid attribute values for each of the @SearchFilter@ terms. Most operators take a single value. The @in@ and @not_in@ operators can take multiple values.
--
--     * @Attribute = BranchName@ :
--
--     * @Operator@ : @=@ | @!=@ | @begins_with@ | @ends_with@ | @contains@ | @in@ | @not_in@
--
--
--
--
--     * @Attribute = LifecycleState@ :
--
--     * @Operator@ : @=@ | @!=@ | @in@ | @not_in@
--
--
--     * @Values@ : @beta@ | @supported@ | @deprecated@ | @retired@
--
--
--
--
--     * @Attribute = PlatformName@ :
--
--     * @Operator@ : @=@ | @!=@ | @begins_with@ | @ends_with@ | @contains@ | @in@ | @not_in@
--
--
--
--
--     * @Attribute = TierType@ :
--
--     * @Operator@ : @=@ | @!=@
--
--
--     * @Values@ : @WebServer/Standard@ | @Worker/SQS/HTTP@
--
--
--
--
-- Array size: limited to 10 @SearchFilter@ objects.
-- Within each @SearchFilter@ item, the @Values@ array is limited to 10 items.
-- * 'nextToken' - For a paginated request. Specify a token from a previous response page to retrieve the next response page. All other parameter values must be identical to the ones specified in the initial request.
--
-- If no @NextToken@ is specified, the first page is retrieved.
-- * 'maxRecords' - The maximum number of platform branch values returned in one call.
mkListPlatformBranches ::
  ListPlatformBranches
mkListPlatformBranches =
  ListPlatformBranches'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxRecords = Lude.Nothing
    }

-- | Criteria for restricting the resulting list of platform branches. The filter is evaluated as a logical conjunction (AND) of the separate @SearchFilter@ terms.
--
-- The following list shows valid attribute values for each of the @SearchFilter@ terms. Most operators take a single value. The @in@ and @not_in@ operators can take multiple values.
--
--     * @Attribute = BranchName@ :
--
--     * @Operator@ : @=@ | @!=@ | @begins_with@ | @ends_with@ | @contains@ | @in@ | @not_in@
--
--
--
--
--     * @Attribute = LifecycleState@ :
--
--     * @Operator@ : @=@ | @!=@ | @in@ | @not_in@
--
--
--     * @Values@ : @beta@ | @supported@ | @deprecated@ | @retired@
--
--
--
--
--     * @Attribute = PlatformName@ :
--
--     * @Operator@ : @=@ | @!=@ | @begins_with@ | @ends_with@ | @contains@ | @in@ | @not_in@
--
--
--
--
--     * @Attribute = TierType@ :
--
--     * @Operator@ : @=@ | @!=@
--
--
--     * @Values@ : @WebServer/Standard@ | @Worker/SQS/HTTP@
--
--
--
--
-- Array size: limited to 10 @SearchFilter@ objects.
-- Within each @SearchFilter@ item, the @Values@ array is limited to 10 items.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpbFilters :: Lens.Lens' ListPlatformBranches (Lude.Maybe [SearchFilter])
lpbFilters = Lens.lens (filters :: ListPlatformBranches -> Lude.Maybe [SearchFilter]) (\s a -> s {filters = a} :: ListPlatformBranches)
{-# DEPRECATED lpbFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | For a paginated request. Specify a token from a previous response page to retrieve the next response page. All other parameter values must be identical to the ones specified in the initial request.
--
-- If no @NextToken@ is specified, the first page is retrieved.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpbNextToken :: Lens.Lens' ListPlatformBranches (Lude.Maybe Lude.Text)
lpbNextToken = Lens.lens (nextToken :: ListPlatformBranches -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListPlatformBranches)
{-# DEPRECATED lpbNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of platform branch values returned in one call.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpbMaxRecords :: Lens.Lens' ListPlatformBranches (Lude.Maybe Lude.Natural)
lpbMaxRecords = Lens.lens (maxRecords :: ListPlatformBranches -> Lude.Maybe Lude.Natural) (\s a -> s {maxRecords = a} :: ListPlatformBranches)
{-# DEPRECATED lpbMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Lude.AWSRequest ListPlatformBranches where
  type Rs ListPlatformBranches = ListPlatformBranchesResponse
  request = Req.postQuery elasticBeanstalkService
  response =
    Res.receiveXMLWrapper
      "ListPlatformBranchesResult"
      ( \s h x ->
          ListPlatformBranchesResponse'
            Lude.<$> ( x Lude..@? "PlatformBranchSummaryList" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (x Lude..@? "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListPlatformBranches where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListPlatformBranches where
  toPath = Lude.const "/"

instance Lude.ToQuery ListPlatformBranches where
  toQuery ListPlatformBranches' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ListPlatformBranches" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "Filters"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> filters),
        "NextToken" Lude.=: nextToken,
        "MaxRecords" Lude.=: maxRecords
      ]

-- | /See:/ 'mkListPlatformBranchesResponse' smart constructor.
data ListPlatformBranchesResponse = ListPlatformBranchesResponse'
  { -- | Summary information about the platform branches.
    platformBranchSummaryList :: Lude.Maybe [PlatformBranchSummary],
    -- | In a paginated request, if this value isn't @null@ , it's the token that you can pass in a subsequent request to get the next response page.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPlatformBranchesResponse' with the minimum fields required to make a request.
--
-- * 'platformBranchSummaryList' - Summary information about the platform branches.
-- * 'nextToken' - In a paginated request, if this value isn't @null@ , it's the token that you can pass in a subsequent request to get the next response page.
-- * 'responseStatus' - The response status code.
mkListPlatformBranchesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListPlatformBranchesResponse
mkListPlatformBranchesResponse pResponseStatus_ =
  ListPlatformBranchesResponse'
    { platformBranchSummaryList =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Summary information about the platform branches.
--
-- /Note:/ Consider using 'platformBranchSummaryList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpbrsPlatformBranchSummaryList :: Lens.Lens' ListPlatformBranchesResponse (Lude.Maybe [PlatformBranchSummary])
lpbrsPlatformBranchSummaryList = Lens.lens (platformBranchSummaryList :: ListPlatformBranchesResponse -> Lude.Maybe [PlatformBranchSummary]) (\s a -> s {platformBranchSummaryList = a} :: ListPlatformBranchesResponse)
{-# DEPRECATED lpbrsPlatformBranchSummaryList "Use generic-lens or generic-optics with 'platformBranchSummaryList' instead." #-}

-- | In a paginated request, if this value isn't @null@ , it's the token that you can pass in a subsequent request to get the next response page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpbrsNextToken :: Lens.Lens' ListPlatformBranchesResponse (Lude.Maybe Lude.Text)
lpbrsNextToken = Lens.lens (nextToken :: ListPlatformBranchesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListPlatformBranchesResponse)
{-# DEPRECATED lpbrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpbrsResponseStatus :: Lens.Lens' ListPlatformBranchesResponse Lude.Int
lpbrsResponseStatus = Lens.lens (responseStatus :: ListPlatformBranchesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListPlatformBranchesResponse)
{-# DEPRECATED lpbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
