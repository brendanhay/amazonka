{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeLaunchTemplateVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more versions of a specified launch template. You can describe all versions, individual versions, or a range of versions. You can also describe all the latest versions or all the default versions of all the launch templates in your account.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeLaunchTemplateVersions
  ( -- * Creating a request
    DescribeLaunchTemplateVersions (..),
    mkDescribeLaunchTemplateVersions,

    -- ** Request lenses
    dltvsLaunchTemplateName,
    dltvsLaunchTemplateId,
    dltvsMinVersion,
    dltvsFilters,
    dltvsMaxVersion,
    dltvsVersions,
    dltvsNextToken,
    dltvsDryRun,
    dltvsMaxResults,

    -- * Destructuring the response
    DescribeLaunchTemplateVersionsResponse (..),
    mkDescribeLaunchTemplateVersionsResponse,

    -- ** Response lenses
    dltvrsNextToken,
    dltvrsLaunchTemplateVersions,
    dltvrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeLaunchTemplateVersions' smart constructor.
data DescribeLaunchTemplateVersions = DescribeLaunchTemplateVersions'
  { launchTemplateName ::
      Lude.Maybe Lude.Text,
    launchTemplateId ::
      Lude.Maybe Lude.Text,
    minVersion ::
      Lude.Maybe Lude.Text,
    filters ::
      Lude.Maybe [Filter],
    maxVersion ::
      Lude.Maybe Lude.Text,
    versions ::
      Lude.Maybe [Lude.Text],
    nextToken ::
      Lude.Maybe Lude.Text,
    dryRun ::
      Lude.Maybe Lude.Bool,
    maxResults ::
      Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeLaunchTemplateVersions' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'filters' - One or more filters.
--
--
--     * @create-time@ - The time the launch template version was created.
--
--
--     * @ebs-optimized@ - A boolean that indicates whether the instance is optimized for Amazon EBS I/O.
--
--
--     * @iam-instance-profile@ - The ARN of the IAM instance profile.
--
--
--     * @image-id@ - The ID of the AMI.
--
--
--     * @instance-type@ - The instance type.
--
--
--     * @is-default-version@ - A boolean that indicates whether the launch template version is the default version.
--
--
--     * @kernel-id@ - The kernel ID.
--
--
--     * @ram-disk-id@ - The RAM disk ID.
--
--
-- * 'launchTemplateId' - The ID of the launch template. To describe one or more versions of a specified launch template, you must specify either the launch template ID or the launch template name in the request. To describe all the latest or default launch template versions in your account, you must omit this parameter.
-- * 'launchTemplateName' - The name of the launch template. To describe one or more versions of a specified launch template, you must specify either the launch template ID or the launch template name in the request. To describe all the latest or default launch template versions in your account, you must omit this parameter.
-- * 'maxResults' - The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned @NextToken@ value. This value can be between 1 and 200.
-- * 'maxVersion' - The version number up to which to describe launch template versions.
-- * 'minVersion' - The version number after which to describe launch template versions.
-- * 'nextToken' - The token to request the next page of results.
-- * 'versions' - One or more versions of the launch template. Valid values depend on whether you are describing a specified launch template (by ID or name) or all launch templates in your account.
--
-- To describe one or more versions of a specified launch template, valid values are @> Latest@ , @> Default@ , and numbers.
-- To describe all launch templates in your account that are defined as the latest version, the valid value is @> Latest@ . To describe all launch templates in your account that are defined as the default version, the valid value is @> Default@ . You can specify @> Latest@ and @> Default@ in the same call. You cannot specify numbers.
mkDescribeLaunchTemplateVersions ::
  DescribeLaunchTemplateVersions
mkDescribeLaunchTemplateVersions =
  DescribeLaunchTemplateVersions'
    { launchTemplateName =
        Lude.Nothing,
      launchTemplateId = Lude.Nothing,
      minVersion = Lude.Nothing,
      filters = Lude.Nothing,
      maxVersion = Lude.Nothing,
      versions = Lude.Nothing,
      nextToken = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The name of the launch template. To describe one or more versions of a specified launch template, you must specify either the launch template ID or the launch template name in the request. To describe all the latest or default launch template versions in your account, you must omit this parameter.
--
-- /Note:/ Consider using 'launchTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvsLaunchTemplateName :: Lens.Lens' DescribeLaunchTemplateVersions (Lude.Maybe Lude.Text)
dltvsLaunchTemplateName = Lens.lens (launchTemplateName :: DescribeLaunchTemplateVersions -> Lude.Maybe Lude.Text) (\s a -> s {launchTemplateName = a} :: DescribeLaunchTemplateVersions)
{-# DEPRECATED dltvsLaunchTemplateName "Use generic-lens or generic-optics with 'launchTemplateName' instead." #-}

-- | The ID of the launch template. To describe one or more versions of a specified launch template, you must specify either the launch template ID or the launch template name in the request. To describe all the latest or default launch template versions in your account, you must omit this parameter.
--
-- /Note:/ Consider using 'launchTemplateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvsLaunchTemplateId :: Lens.Lens' DescribeLaunchTemplateVersions (Lude.Maybe Lude.Text)
dltvsLaunchTemplateId = Lens.lens (launchTemplateId :: DescribeLaunchTemplateVersions -> Lude.Maybe Lude.Text) (\s a -> s {launchTemplateId = a} :: DescribeLaunchTemplateVersions)
{-# DEPRECATED dltvsLaunchTemplateId "Use generic-lens or generic-optics with 'launchTemplateId' instead." #-}

-- | The version number after which to describe launch template versions.
--
-- /Note:/ Consider using 'minVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvsMinVersion :: Lens.Lens' DescribeLaunchTemplateVersions (Lude.Maybe Lude.Text)
dltvsMinVersion = Lens.lens (minVersion :: DescribeLaunchTemplateVersions -> Lude.Maybe Lude.Text) (\s a -> s {minVersion = a} :: DescribeLaunchTemplateVersions)
{-# DEPRECATED dltvsMinVersion "Use generic-lens or generic-optics with 'minVersion' instead." #-}

-- | One or more filters.
--
--
--     * @create-time@ - The time the launch template version was created.
--
--
--     * @ebs-optimized@ - A boolean that indicates whether the instance is optimized for Amazon EBS I/O.
--
--
--     * @iam-instance-profile@ - The ARN of the IAM instance profile.
--
--
--     * @image-id@ - The ID of the AMI.
--
--
--     * @instance-type@ - The instance type.
--
--
--     * @is-default-version@ - A boolean that indicates whether the launch template version is the default version.
--
--
--     * @kernel-id@ - The kernel ID.
--
--
--     * @ram-disk-id@ - The RAM disk ID.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvsFilters :: Lens.Lens' DescribeLaunchTemplateVersions (Lude.Maybe [Filter])
dltvsFilters = Lens.lens (filters :: DescribeLaunchTemplateVersions -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeLaunchTemplateVersions)
{-# DEPRECATED dltvsFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The version number up to which to describe launch template versions.
--
-- /Note:/ Consider using 'maxVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvsMaxVersion :: Lens.Lens' DescribeLaunchTemplateVersions (Lude.Maybe Lude.Text)
dltvsMaxVersion = Lens.lens (maxVersion :: DescribeLaunchTemplateVersions -> Lude.Maybe Lude.Text) (\s a -> s {maxVersion = a} :: DescribeLaunchTemplateVersions)
{-# DEPRECATED dltvsMaxVersion "Use generic-lens or generic-optics with 'maxVersion' instead." #-}

-- | One or more versions of the launch template. Valid values depend on whether you are describing a specified launch template (by ID or name) or all launch templates in your account.
--
-- To describe one or more versions of a specified launch template, valid values are @> Latest@ , @> Default@ , and numbers.
-- To describe all launch templates in your account that are defined as the latest version, the valid value is @> Latest@ . To describe all launch templates in your account that are defined as the default version, the valid value is @> Default@ . You can specify @> Latest@ and @> Default@ in the same call. You cannot specify numbers.
--
-- /Note:/ Consider using 'versions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvsVersions :: Lens.Lens' DescribeLaunchTemplateVersions (Lude.Maybe [Lude.Text])
dltvsVersions = Lens.lens (versions :: DescribeLaunchTemplateVersions -> Lude.Maybe [Lude.Text]) (\s a -> s {versions = a} :: DescribeLaunchTemplateVersions)
{-# DEPRECATED dltvsVersions "Use generic-lens or generic-optics with 'versions' instead." #-}

-- | The token to request the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvsNextToken :: Lens.Lens' DescribeLaunchTemplateVersions (Lude.Maybe Lude.Text)
dltvsNextToken = Lens.lens (nextToken :: DescribeLaunchTemplateVersions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeLaunchTemplateVersions)
{-# DEPRECATED dltvsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvsDryRun :: Lens.Lens' DescribeLaunchTemplateVersions (Lude.Maybe Lude.Bool)
dltvsDryRun = Lens.lens (dryRun :: DescribeLaunchTemplateVersions -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeLaunchTemplateVersions)
{-# DEPRECATED dltvsDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned @NextToken@ value. This value can be between 1 and 200.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvsMaxResults :: Lens.Lens' DescribeLaunchTemplateVersions (Lude.Maybe Lude.Int)
dltvsMaxResults = Lens.lens (maxResults :: DescribeLaunchTemplateVersions -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeLaunchTemplateVersions)
{-# DEPRECATED dltvsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeLaunchTemplateVersions where
  page rq rs
    | Page.stop (rs Lens.^. dltvrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dltvrsLaunchTemplateVersions) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dltvsNextToken Lens..~ rs Lens.^. dltvrsNextToken

instance Lude.AWSRequest DescribeLaunchTemplateVersions where
  type
    Rs DescribeLaunchTemplateVersions =
      DescribeLaunchTemplateVersionsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeLaunchTemplateVersionsResponse'
            Lude.<$> (x Lude..@? "nextToken")
            Lude.<*> ( x Lude..@? "launchTemplateVersionSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeLaunchTemplateVersions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeLaunchTemplateVersions where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeLaunchTemplateVersions where
  toQuery DescribeLaunchTemplateVersions' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeLaunchTemplateVersions" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "LaunchTemplateName" Lude.=: launchTemplateName,
        "LaunchTemplateId" Lude.=: launchTemplateId,
        "MinVersion" Lude.=: minVersion,
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "MaxVersion" Lude.=: maxVersion,
        Lude.toQuery
          (Lude.toQueryList "LaunchTemplateVersion" Lude.<$> versions),
        "NextToken" Lude.=: nextToken,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeLaunchTemplateVersionsResponse' smart constructor.
data DescribeLaunchTemplateVersionsResponse = DescribeLaunchTemplateVersionsResponse'
  { nextToken ::
      Lude.Maybe
        Lude.Text,
    launchTemplateVersions ::
      Lude.Maybe
        [LaunchTemplateVersion],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeLaunchTemplateVersionsResponse' with the minimum fields required to make a request.
--
-- * 'launchTemplateVersions' - Information about the launch template versions.
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkDescribeLaunchTemplateVersionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeLaunchTemplateVersionsResponse
mkDescribeLaunchTemplateVersionsResponse pResponseStatus_ =
  DescribeLaunchTemplateVersionsResponse'
    { nextToken = Lude.Nothing,
      launchTemplateVersions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvrsNextToken :: Lens.Lens' DescribeLaunchTemplateVersionsResponse (Lude.Maybe Lude.Text)
dltvrsNextToken = Lens.lens (nextToken :: DescribeLaunchTemplateVersionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeLaunchTemplateVersionsResponse)
{-# DEPRECATED dltvrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the launch template versions.
--
-- /Note:/ Consider using 'launchTemplateVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvrsLaunchTemplateVersions :: Lens.Lens' DescribeLaunchTemplateVersionsResponse (Lude.Maybe [LaunchTemplateVersion])
dltvrsLaunchTemplateVersions = Lens.lens (launchTemplateVersions :: DescribeLaunchTemplateVersionsResponse -> Lude.Maybe [LaunchTemplateVersion]) (\s a -> s {launchTemplateVersions = a} :: DescribeLaunchTemplateVersionsResponse)
{-# DEPRECATED dltvrsLaunchTemplateVersions "Use generic-lens or generic-optics with 'launchTemplateVersions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvrsResponseStatus :: Lens.Lens' DescribeLaunchTemplateVersionsResponse Lude.Int
dltvrsResponseStatus = Lens.lens (responseStatus :: DescribeLaunchTemplateVersionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeLaunchTemplateVersionsResponse)
{-# DEPRECATED dltvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
