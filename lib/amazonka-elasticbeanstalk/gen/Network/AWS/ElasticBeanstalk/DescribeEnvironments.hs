{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeEnvironments (..)
    , mkDescribeEnvironments
    -- ** Request lenses
    , desApplicationName
    , desEnvironmentIds
    , desEnvironmentNames
    , desIncludeDeleted
    , desIncludedDeletedBackTo
    , desMaxRecords
    , desNextToken
    , desVersionLabel

     -- * Destructuring the response
    , Types.EnvironmentDescriptionsMessage (..)
    , Types.mkEnvironmentDescriptionsMessage
    -- ** Response lenses
    , Types.edmEnvironments
    , Types.edmNextToken
    ) where

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
  { applicationName :: Core.Maybe Types.ApplicationName
    -- ^ If specified, AWS Elastic Beanstalk restricts the returned descriptions to include only those that are associated with this application.
  , environmentIds :: Core.Maybe [Types.EnvironmentId]
    -- ^ If specified, AWS Elastic Beanstalk restricts the returned descriptions to include only those that have the specified IDs.
  , environmentNames :: Core.Maybe [Types.EnvironmentName]
    -- ^ If specified, AWS Elastic Beanstalk restricts the returned descriptions to include only those that have the specified names.
  , includeDeleted :: Core.Maybe Core.Bool
    -- ^ Indicates whether to include deleted environments:
--
-- @true@ : Environments that have been deleted after @IncludedDeletedBackTo@ are displayed.
-- @false@ : Do not include deleted environments.
  , includedDeletedBackTo :: Core.Maybe Core.UTCTime
    -- ^ If specified when @IncludeDeleted@ is set to @true@ , then environments deleted after this date are displayed. 
  , maxRecords :: Core.Maybe Core.Natural
    -- ^ For a paginated request. Specify a maximum number of environments to include in each response.
--
-- If no @MaxRecords@ is specified, all available environments are retrieved in a single response.
  , nextToken :: Core.Maybe Types.Token
    -- ^ For a paginated request. Specify a token from a previous response page to retrieve the next response page. All other parameter values must be identical to the ones specified in the initial request.
--
-- If no @NextToken@ is specified, the first page is retrieved.
  , versionLabel :: Core.Maybe Types.VersionLabel
    -- ^ If specified, AWS Elastic Beanstalk restricts the returned descriptions to include only those that are associated with this application version.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeEnvironments' value with any optional fields omitted.
mkDescribeEnvironments
    :: DescribeEnvironments
mkDescribeEnvironments
  = DescribeEnvironments'{applicationName = Core.Nothing,
                          environmentIds = Core.Nothing, environmentNames = Core.Nothing,
                          includeDeleted = Core.Nothing,
                          includedDeletedBackTo = Core.Nothing, maxRecords = Core.Nothing,
                          nextToken = Core.Nothing, versionLabel = Core.Nothing}

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to include only those that are associated with this application.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desApplicationName :: Lens.Lens' DescribeEnvironments (Core.Maybe Types.ApplicationName)
desApplicationName = Lens.field @"applicationName"
{-# INLINEABLE desApplicationName #-}
{-# DEPRECATED applicationName "Use generic-lens or generic-optics with 'applicationName' instead"  #-}

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to include only those that have the specified IDs.
--
-- /Note:/ Consider using 'environmentIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desEnvironmentIds :: Lens.Lens' DescribeEnvironments (Core.Maybe [Types.EnvironmentId])
desEnvironmentIds = Lens.field @"environmentIds"
{-# INLINEABLE desEnvironmentIds #-}
{-# DEPRECATED environmentIds "Use generic-lens or generic-optics with 'environmentIds' instead"  #-}

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to include only those that have the specified names.
--
-- /Note:/ Consider using 'environmentNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desEnvironmentNames :: Lens.Lens' DescribeEnvironments (Core.Maybe [Types.EnvironmentName])
desEnvironmentNames = Lens.field @"environmentNames"
{-# INLINEABLE desEnvironmentNames #-}
{-# DEPRECATED environmentNames "Use generic-lens or generic-optics with 'environmentNames' instead"  #-}

-- | Indicates whether to include deleted environments:
--
-- @true@ : Environments that have been deleted after @IncludedDeletedBackTo@ are displayed.
-- @false@ : Do not include deleted environments.
--
-- /Note:/ Consider using 'includeDeleted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desIncludeDeleted :: Lens.Lens' DescribeEnvironments (Core.Maybe Core.Bool)
desIncludeDeleted = Lens.field @"includeDeleted"
{-# INLINEABLE desIncludeDeleted #-}
{-# DEPRECATED includeDeleted "Use generic-lens or generic-optics with 'includeDeleted' instead"  #-}

-- | If specified when @IncludeDeleted@ is set to @true@ , then environments deleted after this date are displayed. 
--
-- /Note:/ Consider using 'includedDeletedBackTo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desIncludedDeletedBackTo :: Lens.Lens' DescribeEnvironments (Core.Maybe Core.UTCTime)
desIncludedDeletedBackTo = Lens.field @"includedDeletedBackTo"
{-# INLINEABLE desIncludedDeletedBackTo #-}
{-# DEPRECATED includedDeletedBackTo "Use generic-lens or generic-optics with 'includedDeletedBackTo' instead"  #-}

-- | For a paginated request. Specify a maximum number of environments to include in each response.
--
-- If no @MaxRecords@ is specified, all available environments are retrieved in a single response.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desMaxRecords :: Lens.Lens' DescribeEnvironments (Core.Maybe Core.Natural)
desMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE desMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

-- | For a paginated request. Specify a token from a previous response page to retrieve the next response page. All other parameter values must be identical to the ones specified in the initial request.
--
-- If no @NextToken@ is specified, the first page is retrieved.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desNextToken :: Lens.Lens' DescribeEnvironments (Core.Maybe Types.Token)
desNextToken = Lens.field @"nextToken"
{-# INLINEABLE desNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to include only those that are associated with this application version.
--
-- /Note:/ Consider using 'versionLabel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desVersionLabel :: Lens.Lens' DescribeEnvironments (Core.Maybe Types.VersionLabel)
desVersionLabel = Lens.field @"versionLabel"
{-# INLINEABLE desVersionLabel #-}
{-# DEPRECATED versionLabel "Use generic-lens or generic-optics with 'versionLabel' instead"  #-}

instance Core.ToQuery DescribeEnvironments where
        toQuery DescribeEnvironments{..}
          = Core.toQueryPair "Action" ("DescribeEnvironments" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ApplicationName")
                applicationName
              Core.<>
              Core.toQueryPair "EnvironmentIds"
                (Core.maybe Core.mempty (Core.toQueryList "member") environmentIds)
              Core.<>
              Core.toQueryPair "EnvironmentNames"
                (Core.maybe Core.mempty (Core.toQueryList "member")
                   environmentNames)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "IncludeDeleted")
                includeDeleted
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "IncludedDeletedBackTo")
                includedDeletedBackTo
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxRecords") maxRecords
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "VersionLabel")
                versionLabel

instance Core.ToHeaders DescribeEnvironments where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeEnvironments where
        type Rs DescribeEnvironments = Types.EnvironmentDescriptionsMessage
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
          = Response.receiveXMLWrapper "DescribeEnvironmentsResult"
              (\ s h x -> Core.parseXML x)
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeEnvironments where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"environments" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")
