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
    desRequestId,
    desTemplateName,
    desStartTime,
    desSeverity,
    desNextToken,
    desVersionLabel,
    desPlatformARN,
    desEnvironmentName,
    desMaxRecords,
    desEndTime,
    desApplicationName,
    desEnvironmentId,

    -- * Destructuring the response
    DescribeEventsResponse (..),
    mkDescribeEventsResponse,

    -- ** Response lenses
    dersNextToken,
    dersEvents,
    dersResponseStatus,
  )
where

import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request to retrieve a list of events for an environment.
--
-- /See:/ 'mkDescribeEvents' smart constructor.
data DescribeEvents = DescribeEvents'
  { -- | If specified, AWS Elastic Beanstalk restricts the described events to include only those associated with this request ID.
    requestId :: Lude.Maybe Lude.Text,
    -- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to those that are associated with this environment configuration.
    templateName :: Lude.Maybe Lude.Text,
    -- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to those that occur on or after this time.
    startTime :: Lude.Maybe Lude.DateTime,
    -- | If specified, limits the events returned from this call to include only those with the specified severity or higher.
    severity :: Lude.Maybe EventSeverity,
    -- | Pagination token. If specified, the events return the next batch of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to those associated with this application version.
    versionLabel :: Lude.Maybe Lude.Text,
    -- | The ARN of a custom platform version. If specified, AWS Elastic Beanstalk restricts the returned descriptions to those associated with this custom platform version.
    platformARN :: Lude.Maybe Lude.Text,
    -- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to those associated with this environment.
    environmentName :: Lude.Maybe Lude.Text,
    -- | Specifies the maximum number of events that can be returned, beginning with the most recent event.
    maxRecords :: Lude.Maybe Lude.Natural,
    -- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to those that occur up to, but not including, the @EndTime@ .
    endTime :: Lude.Maybe Lude.DateTime,
    -- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to include only those associated with this application.
    applicationName :: Lude.Maybe Lude.Text,
    -- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to those associated with this environment.
    environmentId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEvents' with the minimum fields required to make a request.
--
-- * 'requestId' - If specified, AWS Elastic Beanstalk restricts the described events to include only those associated with this request ID.
-- * 'templateName' - If specified, AWS Elastic Beanstalk restricts the returned descriptions to those that are associated with this environment configuration.
-- * 'startTime' - If specified, AWS Elastic Beanstalk restricts the returned descriptions to those that occur on or after this time.
-- * 'severity' - If specified, limits the events returned from this call to include only those with the specified severity or higher.
-- * 'nextToken' - Pagination token. If specified, the events return the next batch of results.
-- * 'versionLabel' - If specified, AWS Elastic Beanstalk restricts the returned descriptions to those associated with this application version.
-- * 'platformARN' - The ARN of a custom platform version. If specified, AWS Elastic Beanstalk restricts the returned descriptions to those associated with this custom platform version.
-- * 'environmentName' - If specified, AWS Elastic Beanstalk restricts the returned descriptions to those associated with this environment.
-- * 'maxRecords' - Specifies the maximum number of events that can be returned, beginning with the most recent event.
-- * 'endTime' - If specified, AWS Elastic Beanstalk restricts the returned descriptions to those that occur up to, but not including, the @EndTime@ .
-- * 'applicationName' - If specified, AWS Elastic Beanstalk restricts the returned descriptions to include only those associated with this application.
-- * 'environmentId' - If specified, AWS Elastic Beanstalk restricts the returned descriptions to those associated with this environment.
mkDescribeEvents ::
  DescribeEvents
mkDescribeEvents =
  DescribeEvents'
    { requestId = Lude.Nothing,
      templateName = Lude.Nothing,
      startTime = Lude.Nothing,
      severity = Lude.Nothing,
      nextToken = Lude.Nothing,
      versionLabel = Lude.Nothing,
      platformARN = Lude.Nothing,
      environmentName = Lude.Nothing,
      maxRecords = Lude.Nothing,
      endTime = Lude.Nothing,
      applicationName = Lude.Nothing,
      environmentId = Lude.Nothing
    }

-- | If specified, AWS Elastic Beanstalk restricts the described events to include only those associated with this request ID.
--
-- /Note:/ Consider using 'requestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desRequestId :: Lens.Lens' DescribeEvents (Lude.Maybe Lude.Text)
desRequestId = Lens.lens (requestId :: DescribeEvents -> Lude.Maybe Lude.Text) (\s a -> s {requestId = a} :: DescribeEvents)
{-# DEPRECATED desRequestId "Use generic-lens or generic-optics with 'requestId' instead." #-}

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to those that are associated with this environment configuration.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desTemplateName :: Lens.Lens' DescribeEvents (Lude.Maybe Lude.Text)
desTemplateName = Lens.lens (templateName :: DescribeEvents -> Lude.Maybe Lude.Text) (\s a -> s {templateName = a} :: DescribeEvents)
{-# DEPRECATED desTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to those that occur on or after this time.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desStartTime :: Lens.Lens' DescribeEvents (Lude.Maybe Lude.DateTime)
desStartTime = Lens.lens (startTime :: DescribeEvents -> Lude.Maybe Lude.DateTime) (\s a -> s {startTime = a} :: DescribeEvents)
{-# DEPRECATED desStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | If specified, limits the events returned from this call to include only those with the specified severity or higher.
--
-- /Note:/ Consider using 'severity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desSeverity :: Lens.Lens' DescribeEvents (Lude.Maybe EventSeverity)
desSeverity = Lens.lens (severity :: DescribeEvents -> Lude.Maybe EventSeverity) (\s a -> s {severity = a} :: DescribeEvents)
{-# DEPRECATED desSeverity "Use generic-lens or generic-optics with 'severity' instead." #-}

-- | Pagination token. If specified, the events return the next batch of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desNextToken :: Lens.Lens' DescribeEvents (Lude.Maybe Lude.Text)
desNextToken = Lens.lens (nextToken :: DescribeEvents -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeEvents)
{-# DEPRECATED desNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to those associated with this application version.
--
-- /Note:/ Consider using 'versionLabel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desVersionLabel :: Lens.Lens' DescribeEvents (Lude.Maybe Lude.Text)
desVersionLabel = Lens.lens (versionLabel :: DescribeEvents -> Lude.Maybe Lude.Text) (\s a -> s {versionLabel = a} :: DescribeEvents)
{-# DEPRECATED desVersionLabel "Use generic-lens or generic-optics with 'versionLabel' instead." #-}

-- | The ARN of a custom platform version. If specified, AWS Elastic Beanstalk restricts the returned descriptions to those associated with this custom platform version.
--
-- /Note:/ Consider using 'platformARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desPlatformARN :: Lens.Lens' DescribeEvents (Lude.Maybe Lude.Text)
desPlatformARN = Lens.lens (platformARN :: DescribeEvents -> Lude.Maybe Lude.Text) (\s a -> s {platformARN = a} :: DescribeEvents)
{-# DEPRECATED desPlatformARN "Use generic-lens or generic-optics with 'platformARN' instead." #-}

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to those associated with this environment.
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desEnvironmentName :: Lens.Lens' DescribeEvents (Lude.Maybe Lude.Text)
desEnvironmentName = Lens.lens (environmentName :: DescribeEvents -> Lude.Maybe Lude.Text) (\s a -> s {environmentName = a} :: DescribeEvents)
{-# DEPRECATED desEnvironmentName "Use generic-lens or generic-optics with 'environmentName' instead." #-}

-- | Specifies the maximum number of events that can be returned, beginning with the most recent event.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desMaxRecords :: Lens.Lens' DescribeEvents (Lude.Maybe Lude.Natural)
desMaxRecords = Lens.lens (maxRecords :: DescribeEvents -> Lude.Maybe Lude.Natural) (\s a -> s {maxRecords = a} :: DescribeEvents)
{-# DEPRECATED desMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to those that occur up to, but not including, the @EndTime@ .
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desEndTime :: Lens.Lens' DescribeEvents (Lude.Maybe Lude.DateTime)
desEndTime = Lens.lens (endTime :: DescribeEvents -> Lude.Maybe Lude.DateTime) (\s a -> s {endTime = a} :: DescribeEvents)
{-# DEPRECATED desEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to include only those associated with this application.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desApplicationName :: Lens.Lens' DescribeEvents (Lude.Maybe Lude.Text)
desApplicationName = Lens.lens (applicationName :: DescribeEvents -> Lude.Maybe Lude.Text) (\s a -> s {applicationName = a} :: DescribeEvents)
{-# DEPRECATED desApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to those associated with this environment.
--
-- /Note:/ Consider using 'environmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desEnvironmentId :: Lens.Lens' DescribeEvents (Lude.Maybe Lude.Text)
desEnvironmentId = Lens.lens (environmentId :: DescribeEvents -> Lude.Maybe Lude.Text) (\s a -> s {environmentId = a} :: DescribeEvents)
{-# DEPRECATED desEnvironmentId "Use generic-lens or generic-optics with 'environmentId' instead." #-}

instance Page.AWSPager DescribeEvents where
  page rq rs
    | Page.stop (rs Lens.^. dersNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dersEvents) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& desNextToken Lens..~ rs Lens.^. dersNextToken

instance Lude.AWSRequest DescribeEvents where
  type Rs DescribeEvents = DescribeEventsResponse
  request = Req.postQuery elasticBeanstalkService
  response =
    Res.receiveXMLWrapper
      "DescribeEventsResult"
      ( \s h x ->
          DescribeEventsResponse'
            Lude.<$> (x Lude..@? "NextToken")
            Lude.<*> ( x Lude..@? "Events" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeEvents where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeEvents where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeEvents where
  toQuery DescribeEvents' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeEvents" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "RequestId" Lude.=: requestId,
        "TemplateName" Lude.=: templateName,
        "StartTime" Lude.=: startTime,
        "Severity" Lude.=: severity,
        "NextToken" Lude.=: nextToken,
        "VersionLabel" Lude.=: versionLabel,
        "PlatformArn" Lude.=: platformARN,
        "EnvironmentName" Lude.=: environmentName,
        "MaxRecords" Lude.=: maxRecords,
        "EndTime" Lude.=: endTime,
        "ApplicationName" Lude.=: applicationName,
        "EnvironmentId" Lude.=: environmentId
      ]

-- | Result message wrapping a list of event descriptions.
--
-- /See:/ 'mkDescribeEventsResponse' smart constructor.
data DescribeEventsResponse = DescribeEventsResponse'
  { -- | If returned, this indicates that there are more results to obtain. Use this token in the next 'DescribeEvents' call to get the next batch of events.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list of 'EventDescription' .
    events :: Lude.Maybe [EventDescription],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEventsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If returned, this indicates that there are more results to obtain. Use this token in the next 'DescribeEvents' call to get the next batch of events.
-- * 'events' - A list of 'EventDescription' .
-- * 'responseStatus' - The response status code.
mkDescribeEventsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeEventsResponse
mkDescribeEventsResponse pResponseStatus_ =
  DescribeEventsResponse'
    { nextToken = Lude.Nothing,
      events = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If returned, this indicates that there are more results to obtain. Use this token in the next 'DescribeEvents' call to get the next batch of events.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dersNextToken :: Lens.Lens' DescribeEventsResponse (Lude.Maybe Lude.Text)
dersNextToken = Lens.lens (nextToken :: DescribeEventsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeEventsResponse)
{-# DEPRECATED dersNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of 'EventDescription' .
--
-- /Note:/ Consider using 'events' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dersEvents :: Lens.Lens' DescribeEventsResponse (Lude.Maybe [EventDescription])
dersEvents = Lens.lens (events :: DescribeEventsResponse -> Lude.Maybe [EventDescription]) (\s a -> s {events = a} :: DescribeEventsResponse)
{-# DEPRECATED dersEvents "Use generic-lens or generic-optics with 'events' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dersResponseStatus :: Lens.Lens' DescribeEventsResponse Lude.Int
dersResponseStatus = Lens.lens (responseStatus :: DescribeEventsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeEventsResponse)
{-# DEPRECATED dersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
