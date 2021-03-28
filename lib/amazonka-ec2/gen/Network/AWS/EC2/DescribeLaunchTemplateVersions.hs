{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeLaunchTemplateVersions (..)
    , mkDescribeLaunchTemplateVersions
    -- ** Request lenses
    , dltvsDryRun
    , dltvsFilters
    , dltvsLaunchTemplateId
    , dltvsLaunchTemplateName
    , dltvsMaxResults
    , dltvsMaxVersion
    , dltvsMinVersion
    , dltvsNextToken
    , dltvsVersions

    -- * Destructuring the response
    , DescribeLaunchTemplateVersionsResponse (..)
    , mkDescribeLaunchTemplateVersionsResponse
    -- ** Response lenses
    , dltvrfrsLaunchTemplateVersions
    , dltvrfrsNextToken
    , dltvrfrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeLaunchTemplateVersions' smart constructor.
data DescribeLaunchTemplateVersions = DescribeLaunchTemplateVersions'
  { dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , filters :: Core.Maybe [Types.Filter]
    -- ^ One or more filters.
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
  , launchTemplateId :: Core.Maybe Types.LaunchTemplateId
    -- ^ The ID of the launch template. To describe one or more versions of a specified launch template, you must specify either the launch template ID or the launch template name in the request. To describe all the latest or default launch template versions in your account, you must omit this parameter.
  , launchTemplateName :: Core.Maybe Types.LaunchTemplateName
    -- ^ The name of the launch template. To describe one or more versions of a specified launch template, you must specify either the launch template ID or the launch template name in the request. To describe all the latest or default launch template versions in your account, you must omit this parameter.
  , maxResults :: Core.Maybe Core.Int
    -- ^ The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned @NextToken@ value. This value can be between 1 and 200.
  , maxVersion :: Core.Maybe Core.Text
    -- ^ The version number up to which to describe launch template versions.
  , minVersion :: Core.Maybe Core.Text
    -- ^ The version number after which to describe launch template versions.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token to request the next page of results.
  , versions :: Core.Maybe [Core.Text]
    -- ^ One or more versions of the launch template. Valid values depend on whether you are describing a specified launch template (by ID or name) or all launch templates in your account.
--
-- To describe one or more versions of a specified launch template, valid values are @> Latest@ , @> Default@ , and numbers.
-- To describe all launch templates in your account that are defined as the latest version, the valid value is @> Latest@ . To describe all launch templates in your account that are defined as the default version, the valid value is @> Default@ . You can specify @> Latest@ and @> Default@ in the same call. You cannot specify numbers.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLaunchTemplateVersions' value with any optional fields omitted.
mkDescribeLaunchTemplateVersions
    :: DescribeLaunchTemplateVersions
mkDescribeLaunchTemplateVersions
  = DescribeLaunchTemplateVersions'{dryRun = Core.Nothing,
                                    filters = Core.Nothing, launchTemplateId = Core.Nothing,
                                    launchTemplateName = Core.Nothing, maxResults = Core.Nothing,
                                    maxVersion = Core.Nothing, minVersion = Core.Nothing,
                                    nextToken = Core.Nothing, versions = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvsDryRun :: Lens.Lens' DescribeLaunchTemplateVersions (Core.Maybe Core.Bool)
dltvsDryRun = Lens.field @"dryRun"
{-# INLINEABLE dltvsDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

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
{-# INLINEABLE dltvsFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The ID of the launch template. To describe one or more versions of a specified launch template, you must specify either the launch template ID or the launch template name in the request. To describe all the latest or default launch template versions in your account, you must omit this parameter.
--
-- /Note:/ Consider using 'launchTemplateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvsLaunchTemplateId :: Lens.Lens' DescribeLaunchTemplateVersions (Core.Maybe Types.LaunchTemplateId)
dltvsLaunchTemplateId = Lens.field @"launchTemplateId"
{-# INLINEABLE dltvsLaunchTemplateId #-}
{-# DEPRECATED launchTemplateId "Use generic-lens or generic-optics with 'launchTemplateId' instead"  #-}

-- | The name of the launch template. To describe one or more versions of a specified launch template, you must specify either the launch template ID or the launch template name in the request. To describe all the latest or default launch template versions in your account, you must omit this parameter.
--
-- /Note:/ Consider using 'launchTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvsLaunchTemplateName :: Lens.Lens' DescribeLaunchTemplateVersions (Core.Maybe Types.LaunchTemplateName)
dltvsLaunchTemplateName = Lens.field @"launchTemplateName"
{-# INLINEABLE dltvsLaunchTemplateName #-}
{-# DEPRECATED launchTemplateName "Use generic-lens or generic-optics with 'launchTemplateName' instead"  #-}

-- | The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned @NextToken@ value. This value can be between 1 and 200.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvsMaxResults :: Lens.Lens' DescribeLaunchTemplateVersions (Core.Maybe Core.Int)
dltvsMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dltvsMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The version number up to which to describe launch template versions.
--
-- /Note:/ Consider using 'maxVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvsMaxVersion :: Lens.Lens' DescribeLaunchTemplateVersions (Core.Maybe Core.Text)
dltvsMaxVersion = Lens.field @"maxVersion"
{-# INLINEABLE dltvsMaxVersion #-}
{-# DEPRECATED maxVersion "Use generic-lens or generic-optics with 'maxVersion' instead"  #-}

-- | The version number after which to describe launch template versions.
--
-- /Note:/ Consider using 'minVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvsMinVersion :: Lens.Lens' DescribeLaunchTemplateVersions (Core.Maybe Core.Text)
dltvsMinVersion = Lens.field @"minVersion"
{-# INLINEABLE dltvsMinVersion #-}
{-# DEPRECATED minVersion "Use generic-lens or generic-optics with 'minVersion' instead"  #-}

-- | The token to request the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvsNextToken :: Lens.Lens' DescribeLaunchTemplateVersions (Core.Maybe Core.Text)
dltvsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dltvsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | One or more versions of the launch template. Valid values depend on whether you are describing a specified launch template (by ID or name) or all launch templates in your account.
--
-- To describe one or more versions of a specified launch template, valid values are @> Latest@ , @> Default@ , and numbers.
-- To describe all launch templates in your account that are defined as the latest version, the valid value is @> Latest@ . To describe all launch templates in your account that are defined as the default version, the valid value is @> Default@ . You can specify @> Latest@ and @> Default@ in the same call. You cannot specify numbers.
--
-- /Note:/ Consider using 'versions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvsVersions :: Lens.Lens' DescribeLaunchTemplateVersions (Core.Maybe [Core.Text])
dltvsVersions = Lens.field @"versions"
{-# INLINEABLE dltvsVersions #-}
{-# DEPRECATED versions "Use generic-lens or generic-optics with 'versions' instead"  #-}

instance Core.ToQuery DescribeLaunchTemplateVersions where
        toQuery DescribeLaunchTemplateVersions{..}
          = Core.toQueryPair "Action"
              ("DescribeLaunchTemplateVersions" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryList "Filter") filters
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "LaunchTemplateId")
                launchTemplateId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "LaunchTemplateName")
                launchTemplateName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxVersion") maxVersion
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MinVersion") minVersion
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "LaunchTemplateVersion")
                versions

instance Core.ToHeaders DescribeLaunchTemplateVersions where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeLaunchTemplateVersions where
        type Rs DescribeLaunchTemplateVersions =
             DescribeLaunchTemplateVersionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 DescribeLaunchTemplateVersionsResponse' Core.<$>
                   (x Core..@? "launchTemplateVersionSet" Core..<@>
                      Core.parseXMLList "item")
                     Core.<*> x Core..@? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeLaunchTemplateVersions where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"launchTemplateVersions" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeLaunchTemplateVersionsResponse' smart constructor.
data DescribeLaunchTemplateVersionsResponse = DescribeLaunchTemplateVersionsResponse'
  { launchTemplateVersions :: Core.Maybe [Types.LaunchTemplateVersion]
    -- ^ Information about the launch template versions.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeLaunchTemplateVersionsResponse' value with any optional fields omitted.
mkDescribeLaunchTemplateVersionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeLaunchTemplateVersionsResponse
mkDescribeLaunchTemplateVersionsResponse responseStatus
  = DescribeLaunchTemplateVersionsResponse'{launchTemplateVersions =
                                              Core.Nothing,
                                            nextToken = Core.Nothing, responseStatus}

-- | Information about the launch template versions.
--
-- /Note:/ Consider using 'launchTemplateVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvrfrsLaunchTemplateVersions :: Lens.Lens' DescribeLaunchTemplateVersionsResponse (Core.Maybe [Types.LaunchTemplateVersion])
dltvrfrsLaunchTemplateVersions = Lens.field @"launchTemplateVersions"
{-# INLINEABLE dltvrfrsLaunchTemplateVersions #-}
{-# DEPRECATED launchTemplateVersions "Use generic-lens or generic-optics with 'launchTemplateVersions' instead"  #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvrfrsNextToken :: Lens.Lens' DescribeLaunchTemplateVersionsResponse (Core.Maybe Core.Text)
dltvrfrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dltvrfrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvrfrsResponseStatus :: Lens.Lens' DescribeLaunchTemplateVersionsResponse Core.Int
dltvrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dltvrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
