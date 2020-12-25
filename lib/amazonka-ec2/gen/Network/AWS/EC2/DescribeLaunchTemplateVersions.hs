{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    dltvsDryRun,
    dltvsFilters,
    dltvsLaunchTemplateId,
    dltvsLaunchTemplateName,
    dltvsMaxResults,
    dltvsMaxVersion,
    dltvsMinVersion,
    dltvsNextToken,
    dltvsVersions,

    -- * Destructuring the response
    DescribeLaunchTemplateVersionsResponse (..),
    mkDescribeLaunchTemplateVersionsResponse,

    -- ** Response lenses
    dltvrfrsLaunchTemplateVersions,
    dltvrfrsNextToken,
    dltvrfrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeLaunchTemplateVersions' smart constructor.
data DescribeLaunchTemplateVersions = DescribeLaunchTemplateVersions'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
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
    filters :: Core.Maybe [Types.Filter],
    -- | The ID of the launch template. To describe one or more versions of a specified launch template, you must specify either the launch template ID or the launch template name in the request. To describe all the latest or default launch template versions in your account, you must omit this parameter.
    launchTemplateId :: Core.Maybe Types.LaunchTemplateId,
    -- | The name of the launch template. To describe one or more versions of a specified launch template, you must specify either the launch template ID or the launch template name in the request. To describe all the latest or default launch template versions in your account, you must omit this parameter.
    launchTemplateName :: Core.Maybe Types.LaunchTemplateName,
    -- | The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned @NextToken@ value. This value can be between 1 and 200.
    maxResults :: Core.Maybe Core.Int,
    -- | The version number up to which to describe launch template versions.
    maxVersion :: Core.Maybe Types.String,
    -- | The version number after which to describe launch template versions.
    minVersion :: Core.Maybe Types.String,
    -- | The token to request the next page of results.
    nextToken :: Core.Maybe Types.String,
    -- | One or more versions of the launch template. Valid values depend on whether you are describing a specified launch template (by ID or name) or all launch templates in your account.
    --
    -- To describe one or more versions of a specified launch template, valid values are @> Latest@ , @> Default@ , and numbers.
    -- To describe all launch templates in your account that are defined as the latest version, the valid value is @> Latest@ . To describe all launch templates in your account that are defined as the default version, the valid value is @> Default@ . You can specify @> Latest@ and @> Default@ in the same call. You cannot specify numbers.
    versions :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLaunchTemplateVersions' value with any optional fields omitted.
mkDescribeLaunchTemplateVersions ::
  DescribeLaunchTemplateVersions
mkDescribeLaunchTemplateVersions =
  DescribeLaunchTemplateVersions'
    { dryRun = Core.Nothing,
      filters = Core.Nothing,
      launchTemplateId = Core.Nothing,
      launchTemplateName = Core.Nothing,
      maxResults = Core.Nothing,
      maxVersion = Core.Nothing,
      minVersion = Core.Nothing,
      nextToken = Core.Nothing,
      versions = Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvsDryRun :: Lens.Lens' DescribeLaunchTemplateVersions (Core.Maybe Core.Bool)
dltvsDryRun = Lens.field @"dryRun"
{-# DEPRECATED dltvsDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

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
dltvsFilters :: Lens.Lens' DescribeLaunchTemplateVersions (Core.Maybe [Types.Filter])
dltvsFilters = Lens.field @"filters"
{-# DEPRECATED dltvsFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The ID of the launch template. To describe one or more versions of a specified launch template, you must specify either the launch template ID or the launch template name in the request. To describe all the latest or default launch template versions in your account, you must omit this parameter.
--
-- /Note:/ Consider using 'launchTemplateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvsLaunchTemplateId :: Lens.Lens' DescribeLaunchTemplateVersions (Core.Maybe Types.LaunchTemplateId)
dltvsLaunchTemplateId = Lens.field @"launchTemplateId"
{-# DEPRECATED dltvsLaunchTemplateId "Use generic-lens or generic-optics with 'launchTemplateId' instead." #-}

-- | The name of the launch template. To describe one or more versions of a specified launch template, you must specify either the launch template ID or the launch template name in the request. To describe all the latest or default launch template versions in your account, you must omit this parameter.
--
-- /Note:/ Consider using 'launchTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvsLaunchTemplateName :: Lens.Lens' DescribeLaunchTemplateVersions (Core.Maybe Types.LaunchTemplateName)
dltvsLaunchTemplateName = Lens.field @"launchTemplateName"
{-# DEPRECATED dltvsLaunchTemplateName "Use generic-lens or generic-optics with 'launchTemplateName' instead." #-}

-- | The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned @NextToken@ value. This value can be between 1 and 200.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvsMaxResults :: Lens.Lens' DescribeLaunchTemplateVersions (Core.Maybe Core.Int)
dltvsMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dltvsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The version number up to which to describe launch template versions.
--
-- /Note:/ Consider using 'maxVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvsMaxVersion :: Lens.Lens' DescribeLaunchTemplateVersions (Core.Maybe Types.String)
dltvsMaxVersion = Lens.field @"maxVersion"
{-# DEPRECATED dltvsMaxVersion "Use generic-lens or generic-optics with 'maxVersion' instead." #-}

-- | The version number after which to describe launch template versions.
--
-- /Note:/ Consider using 'minVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvsMinVersion :: Lens.Lens' DescribeLaunchTemplateVersions (Core.Maybe Types.String)
dltvsMinVersion = Lens.field @"minVersion"
{-# DEPRECATED dltvsMinVersion "Use generic-lens or generic-optics with 'minVersion' instead." #-}

-- | The token to request the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvsNextToken :: Lens.Lens' DescribeLaunchTemplateVersions (Core.Maybe Types.String)
dltvsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dltvsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | One or more versions of the launch template. Valid values depend on whether you are describing a specified launch template (by ID or name) or all launch templates in your account.
--
-- To describe one or more versions of a specified launch template, valid values are @> Latest@ , @> Default@ , and numbers.
-- To describe all launch templates in your account that are defined as the latest version, the valid value is @> Latest@ . To describe all launch templates in your account that are defined as the default version, the valid value is @> Default@ . You can specify @> Latest@ and @> Default@ in the same call. You cannot specify numbers.
--
-- /Note:/ Consider using 'versions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvsVersions :: Lens.Lens' DescribeLaunchTemplateVersions (Core.Maybe [Types.String])
dltvsVersions = Lens.field @"versions"
{-# DEPRECATED dltvsVersions "Use generic-lens or generic-optics with 'versions' instead." #-}

instance Core.AWSRequest DescribeLaunchTemplateVersions where
  type
    Rs DescribeLaunchTemplateVersions =
      DescribeLaunchTemplateVersionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DescribeLaunchTemplateVersions")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryList "Filter" Core.<$> filters)
                Core.<> (Core.toQueryValue "LaunchTemplateId" Core.<$> launchTemplateId)
                Core.<> ( Core.toQueryValue "LaunchTemplateName"
                            Core.<$> launchTemplateName
                        )
                Core.<> (Core.toQueryValue "MaxResults" Core.<$> maxResults)
                Core.<> (Core.toQueryValue "MaxVersion" Core.<$> maxVersion)
                Core.<> (Core.toQueryValue "MinVersion" Core.<$> minVersion)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
                Core.<> (Core.toQueryList "LaunchTemplateVersion" Core.<$> versions)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeLaunchTemplateVersionsResponse'
            Core.<$> ( x Core..@? "launchTemplateVersionSet"
                         Core..<@> Core.parseXMLList "item"
                     )
            Core.<*> (x Core..@? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeLaunchTemplateVersions where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"launchTemplateVersions" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeLaunchTemplateVersionsResponse' smart constructor.
data DescribeLaunchTemplateVersionsResponse = DescribeLaunchTemplateVersionsResponse'
  { -- | Information about the launch template versions.
    launchTemplateVersions :: Core.Maybe [Types.LaunchTemplateVersion],
    -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeLaunchTemplateVersionsResponse' value with any optional fields omitted.
mkDescribeLaunchTemplateVersionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeLaunchTemplateVersionsResponse
mkDescribeLaunchTemplateVersionsResponse responseStatus =
  DescribeLaunchTemplateVersionsResponse'
    { launchTemplateVersions =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Information about the launch template versions.
--
-- /Note:/ Consider using 'launchTemplateVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvrfrsLaunchTemplateVersions :: Lens.Lens' DescribeLaunchTemplateVersionsResponse (Core.Maybe [Types.LaunchTemplateVersion])
dltvrfrsLaunchTemplateVersions = Lens.field @"launchTemplateVersions"
{-# DEPRECATED dltvrfrsLaunchTemplateVersions "Use generic-lens or generic-optics with 'launchTemplateVersions' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvrfrsNextToken :: Lens.Lens' DescribeLaunchTemplateVersionsResponse (Core.Maybe Types.String)
dltvrfrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dltvrfrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvrfrsResponseStatus :: Lens.Lens' DescribeLaunchTemplateVersionsResponse Core.Int
dltvrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dltvrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
