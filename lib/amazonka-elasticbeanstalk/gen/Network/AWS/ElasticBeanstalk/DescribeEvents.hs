{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.DescribeEvents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns list of event descriptions matching criteria up to the last 6 weeks.
--
-- This operation returns paginated results.
module Network.AWS.ElasticBeanstalk.DescribeEvents
  ( -- * Creating a request
    DescribeEvents (..),
    mkDescribeEvents,

    -- ** Request lenses
    deApplicationName,
    deEndTime,
    deEnvironmentId,
    deEnvironmentName,
    deMaxRecords,
    deNextToken,
    dePlatformArn,
    deRequestId,
    deSeverity,
    deStartTime,
    deTemplateName,
    deVersionLabel,

    -- * Destructuring the response
    DescribeEventsResponse (..),
    mkDescribeEventsResponse,

    -- ** Response lenses
    derrsEvents,
    derrsNextToken,
    derrsResponseStatus,
  )
where

import qualified Network.AWS.ElasticBeanstalk.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to retrieve a list of events for an environment.
--
-- /See:/ 'mkDescribeEvents' smart constructor.
data DescribeEvents = DescribeEvents'
  { -- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to include only those associated with this application.
    applicationName :: Core.Maybe Types.ApplicationName,
    -- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to those that occur up to, but not including, the @EndTime@ .
    endTime :: Core.Maybe Core.UTCTime,
    -- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to those associated with this environment.
    environmentId :: Core.Maybe Types.EnvironmentId,
    -- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to those associated with this environment.
    environmentName :: Core.Maybe Types.EnvironmentName,
    -- | Specifies the maximum number of events that can be returned, beginning with the most recent event.
    maxRecords :: Core.Maybe Core.Natural,
    -- | Pagination token. If specified, the events return the next batch of results.
    nextToken :: Core.Maybe Types.Token,
    -- | The ARN of a custom platform version. If specified, AWS Elastic Beanstalk restricts the returned descriptions to those associated with this custom platform version.
    platformArn :: Core.Maybe Types.PlatformArn,
    -- | If specified, AWS Elastic Beanstalk restricts the described events to include only those associated with this request ID.
    requestId :: Core.Maybe Types.RequestId,
    -- | If specified, limits the events returned from this call to include only those with the specified severity or higher.
    severity :: Core.Maybe Types.EventSeverity,
    -- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to those that occur on or after this time.
    startTime :: Core.Maybe Core.UTCTime,
    -- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to those that are associated with this environment configuration.
    templateName :: Core.Maybe Types.ConfigurationTemplateName,
    -- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to those associated with this application version.
    versionLabel :: Core.Maybe Types.VersionLabel
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeEvents' value with any optional fields omitted.
mkDescribeEvents ::
  DescribeEvents
mkDescribeEvents =
  DescribeEvents'
    { applicationName = Core.Nothing,
      endTime = Core.Nothing,
      environmentId = Core.Nothing,
      environmentName = Core.Nothing,
      maxRecords = Core.Nothing,
      nextToken = Core.Nothing,
      platformArn = Core.Nothing,
      requestId = Core.Nothing,
      severity = Core.Nothing,
      startTime = Core.Nothing,
      templateName = Core.Nothing,
      versionLabel = Core.Nothing
    }

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to include only those associated with this application.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deApplicationName :: Lens.Lens' DescribeEvents (Core.Maybe Types.ApplicationName)
deApplicationName = Lens.field @"applicationName"
{-# DEPRECATED deApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to those that occur up to, but not including, the @EndTime@ .
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deEndTime :: Lens.Lens' DescribeEvents (Core.Maybe Core.UTCTime)
deEndTime = Lens.field @"endTime"
{-# DEPRECATED deEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to those associated with this environment.
--
-- /Note:/ Consider using 'environmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deEnvironmentId :: Lens.Lens' DescribeEvents (Core.Maybe Types.EnvironmentId)
deEnvironmentId = Lens.field @"environmentId"
{-# DEPRECATED deEnvironmentId "Use generic-lens or generic-optics with 'environmentId' instead." #-}

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to those associated with this environment.
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deEnvironmentName :: Lens.Lens' DescribeEvents (Core.Maybe Types.EnvironmentName)
deEnvironmentName = Lens.field @"environmentName"
{-# DEPRECATED deEnvironmentName "Use generic-lens or generic-optics with 'environmentName' instead." #-}

-- | Specifies the maximum number of events that can be returned, beginning with the most recent event.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deMaxRecords :: Lens.Lens' DescribeEvents (Core.Maybe Core.Natural)
deMaxRecords = Lens.field @"maxRecords"
{-# DEPRECATED deMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | Pagination token. If specified, the events return the next batch of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deNextToken :: Lens.Lens' DescribeEvents (Core.Maybe Types.Token)
deNextToken = Lens.field @"nextToken"
{-# DEPRECATED deNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The ARN of a custom platform version. If specified, AWS Elastic Beanstalk restricts the returned descriptions to those associated with this custom platform version.
--
-- /Note:/ Consider using 'platformArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dePlatformArn :: Lens.Lens' DescribeEvents (Core.Maybe Types.PlatformArn)
dePlatformArn = Lens.field @"platformArn"
{-# DEPRECATED dePlatformArn "Use generic-lens or generic-optics with 'platformArn' instead." #-}

-- | If specified, AWS Elastic Beanstalk restricts the described events to include only those associated with this request ID.
--
-- /Note:/ Consider using 'requestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deRequestId :: Lens.Lens' DescribeEvents (Core.Maybe Types.RequestId)
deRequestId = Lens.field @"requestId"
{-# DEPRECATED deRequestId "Use generic-lens or generic-optics with 'requestId' instead." #-}

-- | If specified, limits the events returned from this call to include only those with the specified severity or higher.
--
-- /Note:/ Consider using 'severity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deSeverity :: Lens.Lens' DescribeEvents (Core.Maybe Types.EventSeverity)
deSeverity = Lens.field @"severity"
{-# DEPRECATED deSeverity "Use generic-lens or generic-optics with 'severity' instead." #-}

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to those that occur on or after this time.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deStartTime :: Lens.Lens' DescribeEvents (Core.Maybe Core.UTCTime)
deStartTime = Lens.field @"startTime"
{-# DEPRECATED deStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to those that are associated with this environment configuration.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deTemplateName :: Lens.Lens' DescribeEvents (Core.Maybe Types.ConfigurationTemplateName)
deTemplateName = Lens.field @"templateName"
{-# DEPRECATED deTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to those associated with this application version.
--
-- /Note:/ Consider using 'versionLabel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deVersionLabel :: Lens.Lens' DescribeEvents (Core.Maybe Types.VersionLabel)
deVersionLabel = Lens.field @"versionLabel"
{-# DEPRECATED deVersionLabel "Use generic-lens or generic-optics with 'versionLabel' instead." #-}

instance Core.AWSRequest DescribeEvents where
  type Rs DescribeEvents = DescribeEventsResponse
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
            ( Core.pure ("Action", "DescribeEvents")
                Core.<> (Core.pure ("Version", "2010-12-01"))
                Core.<> (Core.toQueryValue "ApplicationName" Core.<$> applicationName)
                Core.<> (Core.toQueryValue "EndTime" Core.<$> endTime)
                Core.<> (Core.toQueryValue "EnvironmentId" Core.<$> environmentId)
                Core.<> (Core.toQueryValue "EnvironmentName" Core.<$> environmentName)
                Core.<> (Core.toQueryValue "MaxRecords" Core.<$> maxRecords)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
                Core.<> (Core.toQueryValue "PlatformArn" Core.<$> platformArn)
                Core.<> (Core.toQueryValue "RequestId" Core.<$> requestId)
                Core.<> (Core.toQueryValue "Severity" Core.<$> severity)
                Core.<> (Core.toQueryValue "StartTime" Core.<$> startTime)
                Core.<> (Core.toQueryValue "TemplateName" Core.<$> templateName)
                Core.<> (Core.toQueryValue "VersionLabel" Core.<$> versionLabel)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeEventsResult"
      ( \s h x ->
          DescribeEventsResponse'
            Core.<$> (x Core..@? "Events" Core..<@> Core.parseXMLList "member")
            Core.<*> (x Core..@? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeEvents where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"events" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | Result message wrapping a list of event descriptions.
--
-- /See:/ 'mkDescribeEventsResponse' smart constructor.
data DescribeEventsResponse = DescribeEventsResponse'
  { -- | A list of 'EventDescription' .
    events :: Core.Maybe [Types.EventDescription],
    -- | If returned, this indicates that there are more results to obtain. Use this token in the next 'DescribeEvents' call to get the next batch of events.
    nextToken :: Core.Maybe Types.Token,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeEventsResponse' value with any optional fields omitted.
mkDescribeEventsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeEventsResponse
mkDescribeEventsResponse responseStatus =
  DescribeEventsResponse'
    { events = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A list of 'EventDescription' .
--
-- /Note:/ Consider using 'events' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrsEvents :: Lens.Lens' DescribeEventsResponse (Core.Maybe [Types.EventDescription])
derrsEvents = Lens.field @"events"
{-# DEPRECATED derrsEvents "Use generic-lens or generic-optics with 'events' instead." #-}

-- | If returned, this indicates that there are more results to obtain. Use this token in the next 'DescribeEvents' call to get the next batch of events.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrsNextToken :: Lens.Lens' DescribeEventsResponse (Core.Maybe Types.Token)
derrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED derrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrsResponseStatus :: Lens.Lens' DescribeEventsResponse Core.Int
derrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED derrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
