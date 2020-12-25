{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.DescribeEnvironments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns descriptions for existing environments.
--
-- This operation returns paginated results.
module Network.AWS.ElasticBeanstalk.DescribeEnvironments
  ( -- * Creating a request
    DescribeEnvironments (..),
    mkDescribeEnvironments,

    -- ** Request lenses
    desApplicationName,
    desEnvironmentIds,
    desEnvironmentNames,
    desIncludeDeleted,
    desIncludedDeletedBackTo,
    desMaxRecords,
    desNextToken,
    desVersionLabel,

    -- * Destructuring the response
    Types.EnvironmentDescriptionsMessage (..),
    Types.mkEnvironmentDescriptionsMessage,

    -- ** Response lenses
    Types.edmEnvironments,
    Types.edmNextToken,
  )
where

import qualified Network.AWS.ElasticBeanstalk.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to describe one or more environments.
--
-- /See:/ 'mkDescribeEnvironments' smart constructor.
data DescribeEnvironments = DescribeEnvironments'
  { -- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to include only those that are associated with this application.
    applicationName :: Core.Maybe Types.ApplicationName,
    -- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to include only those that have the specified IDs.
    environmentIds :: Core.Maybe [Types.EnvironmentId],
    -- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to include only those that have the specified names.
    environmentNames :: Core.Maybe [Types.EnvironmentName],
    -- | Indicates whether to include deleted environments:
    --
    -- @true@ : Environments that have been deleted after @IncludedDeletedBackTo@ are displayed.
    -- @false@ : Do not include deleted environments.
    includeDeleted :: Core.Maybe Core.Bool,
    -- | If specified when @IncludeDeleted@ is set to @true@ , then environments deleted after this date are displayed.
    includedDeletedBackTo :: Core.Maybe Core.UTCTime,
    -- | For a paginated request. Specify a maximum number of environments to include in each response.
    --
    -- If no @MaxRecords@ is specified, all available environments are retrieved in a single response.
    maxRecords :: Core.Maybe Core.Natural,
    -- | For a paginated request. Specify a token from a previous response page to retrieve the next response page. All other parameter values must be identical to the ones specified in the initial request.
    --
    -- If no @NextToken@ is specified, the first page is retrieved.
    nextToken :: Core.Maybe Types.Token,
    -- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to include only those that are associated with this application version.
    versionLabel :: Core.Maybe Types.VersionLabel
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeEnvironments' value with any optional fields omitted.
mkDescribeEnvironments ::
  DescribeEnvironments
mkDescribeEnvironments =
  DescribeEnvironments'
    { applicationName = Core.Nothing,
      environmentIds = Core.Nothing,
      environmentNames = Core.Nothing,
      includeDeleted = Core.Nothing,
      includedDeletedBackTo = Core.Nothing,
      maxRecords = Core.Nothing,
      nextToken = Core.Nothing,
      versionLabel = Core.Nothing
    }

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to include only those that are associated with this application.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desApplicationName :: Lens.Lens' DescribeEnvironments (Core.Maybe Types.ApplicationName)
desApplicationName = Lens.field @"applicationName"
{-# DEPRECATED desApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to include only those that have the specified IDs.
--
-- /Note:/ Consider using 'environmentIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desEnvironmentIds :: Lens.Lens' DescribeEnvironments (Core.Maybe [Types.EnvironmentId])
desEnvironmentIds = Lens.field @"environmentIds"
{-# DEPRECATED desEnvironmentIds "Use generic-lens or generic-optics with 'environmentIds' instead." #-}

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to include only those that have the specified names.
--
-- /Note:/ Consider using 'environmentNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desEnvironmentNames :: Lens.Lens' DescribeEnvironments (Core.Maybe [Types.EnvironmentName])
desEnvironmentNames = Lens.field @"environmentNames"
{-# DEPRECATED desEnvironmentNames "Use generic-lens or generic-optics with 'environmentNames' instead." #-}

-- | Indicates whether to include deleted environments:
--
-- @true@ : Environments that have been deleted after @IncludedDeletedBackTo@ are displayed.
-- @false@ : Do not include deleted environments.
--
-- /Note:/ Consider using 'includeDeleted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desIncludeDeleted :: Lens.Lens' DescribeEnvironments (Core.Maybe Core.Bool)
desIncludeDeleted = Lens.field @"includeDeleted"
{-# DEPRECATED desIncludeDeleted "Use generic-lens or generic-optics with 'includeDeleted' instead." #-}

-- | If specified when @IncludeDeleted@ is set to @true@ , then environments deleted after this date are displayed.
--
-- /Note:/ Consider using 'includedDeletedBackTo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desIncludedDeletedBackTo :: Lens.Lens' DescribeEnvironments (Core.Maybe Core.UTCTime)
desIncludedDeletedBackTo = Lens.field @"includedDeletedBackTo"
{-# DEPRECATED desIncludedDeletedBackTo "Use generic-lens or generic-optics with 'includedDeletedBackTo' instead." #-}

-- | For a paginated request. Specify a maximum number of environments to include in each response.
--
-- If no @MaxRecords@ is specified, all available environments are retrieved in a single response.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desMaxRecords :: Lens.Lens' DescribeEnvironments (Core.Maybe Core.Natural)
desMaxRecords = Lens.field @"maxRecords"
{-# DEPRECATED desMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | For a paginated request. Specify a token from a previous response page to retrieve the next response page. All other parameter values must be identical to the ones specified in the initial request.
--
-- If no @NextToken@ is specified, the first page is retrieved.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desNextToken :: Lens.Lens' DescribeEnvironments (Core.Maybe Types.Token)
desNextToken = Lens.field @"nextToken"
{-# DEPRECATED desNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to include only those that are associated with this application version.
--
-- /Note:/ Consider using 'versionLabel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desVersionLabel :: Lens.Lens' DescribeEnvironments (Core.Maybe Types.VersionLabel)
desVersionLabel = Lens.field @"versionLabel"
{-# DEPRECATED desVersionLabel "Use generic-lens or generic-optics with 'versionLabel' instead." #-}

instance Core.AWSRequest DescribeEnvironments where
  type Rs DescribeEnvironments = Types.EnvironmentDescriptionsMessage
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
            ( Core.pure ("Action", "DescribeEnvironments")
                Core.<> (Core.pure ("Version", "2010-12-01"))
                Core.<> (Core.toQueryValue "ApplicationName" Core.<$> applicationName)
                Core.<> ( Core.toQueryValue
                            "EnvironmentIds"
                            (Core.toQueryList "member" Core.<$> environmentIds)
                        )
                Core.<> ( Core.toQueryValue
                            "EnvironmentNames"
                            (Core.toQueryList "member" Core.<$> environmentNames)
                        )
                Core.<> (Core.toQueryValue "IncludeDeleted" Core.<$> includeDeleted)
                Core.<> ( Core.toQueryValue "IncludedDeletedBackTo"
                            Core.<$> includedDeletedBackTo
                        )
                Core.<> (Core.toQueryValue "MaxRecords" Core.<$> maxRecords)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
                Core.<> (Core.toQueryValue "VersionLabel" Core.<$> versionLabel)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeEnvironmentsResult"
      (\s h x -> Core.parseXML x)

instance Pager.AWSPager DescribeEnvironments where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"environments" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )
