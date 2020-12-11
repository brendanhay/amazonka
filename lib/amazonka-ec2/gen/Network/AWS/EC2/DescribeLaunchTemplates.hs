{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeLaunchTemplates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more launch templates.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeLaunchTemplates
  ( -- * Creating a request
    DescribeLaunchTemplates (..),
    mkDescribeLaunchTemplates,

    -- ** Request lenses
    dltsFilters,
    dltsNextToken,
    dltsLaunchTemplateIds,
    dltsDryRun,
    dltsMaxResults,
    dltsLaunchTemplateNames,

    -- * Destructuring the response
    DescribeLaunchTemplatesResponse (..),
    mkDescribeLaunchTemplatesResponse,

    -- ** Response lenses
    dltsrsLaunchTemplates,
    dltsrsNextToken,
    dltsrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeLaunchTemplates' smart constructor.
data DescribeLaunchTemplates = DescribeLaunchTemplates'
  { filters ::
      Lude.Maybe [Filter],
    nextToken :: Lude.Maybe Lude.Text,
    launchTemplateIds :: Lude.Maybe [Lude.Text],
    dryRun :: Lude.Maybe Lude.Bool,
    maxResults :: Lude.Maybe Lude.Natural,
    launchTemplateNames ::
      Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeLaunchTemplates' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'filters' - One or more filters.
--
--
--     * @create-time@ - The time the launch template was created.
--
--
--     * @launch-template-name@ - The name of the launch template.
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
-- * 'launchTemplateIds' - One or more launch template IDs.
-- * 'launchTemplateNames' - One or more launch template names.
-- * 'maxResults' - The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned @NextToken@ value. This value can be between 1 and 200.
-- * 'nextToken' - The token to request the next page of results.
mkDescribeLaunchTemplates ::
  DescribeLaunchTemplates
mkDescribeLaunchTemplates =
  DescribeLaunchTemplates'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      launchTemplateIds = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing,
      launchTemplateNames = Lude.Nothing
    }

-- | One or more filters.
--
--
--     * @create-time@ - The time the launch template was created.
--
--
--     * @launch-template-name@ - The name of the launch template.
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltsFilters :: Lens.Lens' DescribeLaunchTemplates (Lude.Maybe [Filter])
dltsFilters = Lens.lens (filters :: DescribeLaunchTemplates -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeLaunchTemplates)
{-# DEPRECATED dltsFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token to request the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltsNextToken :: Lens.Lens' DescribeLaunchTemplates (Lude.Maybe Lude.Text)
dltsNextToken = Lens.lens (nextToken :: DescribeLaunchTemplates -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeLaunchTemplates)
{-# DEPRECATED dltsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | One or more launch template IDs.
--
-- /Note:/ Consider using 'launchTemplateIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltsLaunchTemplateIds :: Lens.Lens' DescribeLaunchTemplates (Lude.Maybe [Lude.Text])
dltsLaunchTemplateIds = Lens.lens (launchTemplateIds :: DescribeLaunchTemplates -> Lude.Maybe [Lude.Text]) (\s a -> s {launchTemplateIds = a} :: DescribeLaunchTemplates)
{-# DEPRECATED dltsLaunchTemplateIds "Use generic-lens or generic-optics with 'launchTemplateIds' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltsDryRun :: Lens.Lens' DescribeLaunchTemplates (Lude.Maybe Lude.Bool)
dltsDryRun = Lens.lens (dryRun :: DescribeLaunchTemplates -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeLaunchTemplates)
{-# DEPRECATED dltsDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned @NextToken@ value. This value can be between 1 and 200.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltsMaxResults :: Lens.Lens' DescribeLaunchTemplates (Lude.Maybe Lude.Natural)
dltsMaxResults = Lens.lens (maxResults :: DescribeLaunchTemplates -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeLaunchTemplates)
{-# DEPRECATED dltsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | One or more launch template names.
--
-- /Note:/ Consider using 'launchTemplateNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltsLaunchTemplateNames :: Lens.Lens' DescribeLaunchTemplates (Lude.Maybe [Lude.Text])
dltsLaunchTemplateNames = Lens.lens (launchTemplateNames :: DescribeLaunchTemplates -> Lude.Maybe [Lude.Text]) (\s a -> s {launchTemplateNames = a} :: DescribeLaunchTemplates)
{-# DEPRECATED dltsLaunchTemplateNames "Use generic-lens or generic-optics with 'launchTemplateNames' instead." #-}

instance Page.AWSPager DescribeLaunchTemplates where
  page rq rs
    | Page.stop (rs Lens.^. dltsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dltsrsLaunchTemplates) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dltsNextToken Lens..~ rs Lens.^. dltsrsNextToken

instance Lude.AWSRequest DescribeLaunchTemplates where
  type Rs DescribeLaunchTemplates = DescribeLaunchTemplatesResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeLaunchTemplatesResponse'
            Lude.<$> ( x Lude..@? "launchTemplates" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeLaunchTemplates where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeLaunchTemplates where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeLaunchTemplates where
  toQuery DescribeLaunchTemplates' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeLaunchTemplates" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "NextToken" Lude.=: nextToken,
        Lude.toQuery
          (Lude.toQueryList "LaunchTemplateId" Lude.<$> launchTemplateIds),
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults,
        Lude.toQuery
          ( Lude.toQueryList "LaunchTemplateName"
              Lude.<$> launchTemplateNames
          )
      ]

-- | /See:/ 'mkDescribeLaunchTemplatesResponse' smart constructor.
data DescribeLaunchTemplatesResponse = DescribeLaunchTemplatesResponse'
  { launchTemplates ::
      Lude.Maybe [LaunchTemplate],
    nextToken ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DescribeLaunchTemplatesResponse' with the minimum fields required to make a request.
--
-- * 'launchTemplates' - Information about the launch templates.
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkDescribeLaunchTemplatesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeLaunchTemplatesResponse
mkDescribeLaunchTemplatesResponse pResponseStatus_ =
  DescribeLaunchTemplatesResponse'
    { launchTemplates = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the launch templates.
--
-- /Note:/ Consider using 'launchTemplates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltsrsLaunchTemplates :: Lens.Lens' DescribeLaunchTemplatesResponse (Lude.Maybe [LaunchTemplate])
dltsrsLaunchTemplates = Lens.lens (launchTemplates :: DescribeLaunchTemplatesResponse -> Lude.Maybe [LaunchTemplate]) (\s a -> s {launchTemplates = a} :: DescribeLaunchTemplatesResponse)
{-# DEPRECATED dltsrsLaunchTemplates "Use generic-lens or generic-optics with 'launchTemplates' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltsrsNextToken :: Lens.Lens' DescribeLaunchTemplatesResponse (Lude.Maybe Lude.Text)
dltsrsNextToken = Lens.lens (nextToken :: DescribeLaunchTemplatesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeLaunchTemplatesResponse)
{-# DEPRECATED dltsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltsrsResponseStatus :: Lens.Lens' DescribeLaunchTemplatesResponse Lude.Int
dltsrsResponseStatus = Lens.lens (responseStatus :: DescribeLaunchTemplatesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeLaunchTemplatesResponse)
{-# DEPRECATED dltsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
