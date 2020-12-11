{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    deRequestId,
    deTemplateName,
    deStartTime,
    deSeverity,
    deNextToken,
    deVersionLabel,
    dePlatformARN,
    deEnvironmentName,
    deMaxRecords,
    deEndTime,
    deApplicationName,
    deEnvironmentId,

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
  { requestId ::
      Lude.Maybe Lude.Text,
    templateName :: Lude.Maybe Lude.Text,
    startTime :: Lude.Maybe Lude.ISO8601,
    severity :: Lude.Maybe EventSeverity,
    nextToken :: Lude.Maybe Lude.Text,
    versionLabel :: Lude.Maybe Lude.Text,
    platformARN :: Lude.Maybe Lude.Text,
    environmentName :: Lude.Maybe Lude.Text,
    maxRecords :: Lude.Maybe Lude.Natural,
    endTime :: Lude.Maybe Lude.ISO8601,
    applicationName :: Lude.Maybe Lude.Text,
    environmentId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEvents' with the minimum fields required to make a request.
--
-- * 'applicationName' - If specified, AWS Elastic Beanstalk restricts the returned descriptions to include only those associated with this application.
-- * 'endTime' - If specified, AWS Elastic Beanstalk restricts the returned descriptions to those that occur up to, but not including, the @EndTime@ .
-- * 'environmentId' - If specified, AWS Elastic Beanstalk restricts the returned descriptions to those associated with this environment.
-- * 'environmentName' - If specified, AWS Elastic Beanstalk restricts the returned descriptions to those associated with this environment.
-- * 'maxRecords' - Specifies the maximum number of events that can be returned, beginning with the most recent event.
-- * 'nextToken' - Pagination token. If specified, the events return the next batch of results.
-- * 'platformARN' - The ARN of a custom platform version. If specified, AWS Elastic Beanstalk restricts the returned descriptions to those associated with this custom platform version.
-- * 'requestId' - If specified, AWS Elastic Beanstalk restricts the described events to include only those associated with this request ID.
-- * 'severity' - If specified, limits the events returned from this call to include only those with the specified severity or higher.
-- * 'startTime' - If specified, AWS Elastic Beanstalk restricts the returned descriptions to those that occur on or after this time.
-- * 'templateName' - If specified, AWS Elastic Beanstalk restricts the returned descriptions to those that are associated with this environment configuration.
-- * 'versionLabel' - If specified, AWS Elastic Beanstalk restricts the returned descriptions to those associated with this application version.
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
deRequestId :: Lens.Lens' DescribeEvents (Lude.Maybe Lude.Text)
deRequestId = Lens.lens (requestId :: DescribeEvents -> Lude.Maybe Lude.Text) (\s a -> s {requestId = a} :: DescribeEvents)
{-# DEPRECATED deRequestId "Use generic-lens or generic-optics with 'requestId' instead." #-}

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to those that are associated with this environment configuration.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deTemplateName :: Lens.Lens' DescribeEvents (Lude.Maybe Lude.Text)
deTemplateName = Lens.lens (templateName :: DescribeEvents -> Lude.Maybe Lude.Text) (\s a -> s {templateName = a} :: DescribeEvents)
{-# DEPRECATED deTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to those that occur on or after this time.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deStartTime :: Lens.Lens' DescribeEvents (Lude.Maybe Lude.ISO8601)
deStartTime = Lens.lens (startTime :: DescribeEvents -> Lude.Maybe Lude.ISO8601) (\s a -> s {startTime = a} :: DescribeEvents)
{-# DEPRECATED deStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | If specified, limits the events returned from this call to include only those with the specified severity or higher.
--
-- /Note:/ Consider using 'severity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deSeverity :: Lens.Lens' DescribeEvents (Lude.Maybe EventSeverity)
deSeverity = Lens.lens (severity :: DescribeEvents -> Lude.Maybe EventSeverity) (\s a -> s {severity = a} :: DescribeEvents)
{-# DEPRECATED deSeverity "Use generic-lens or generic-optics with 'severity' instead." #-}

-- | Pagination token. If specified, the events return the next batch of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deNextToken :: Lens.Lens' DescribeEvents (Lude.Maybe Lude.Text)
deNextToken = Lens.lens (nextToken :: DescribeEvents -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeEvents)
{-# DEPRECATED deNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to those associated with this application version.
--
-- /Note:/ Consider using 'versionLabel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deVersionLabel :: Lens.Lens' DescribeEvents (Lude.Maybe Lude.Text)
deVersionLabel = Lens.lens (versionLabel :: DescribeEvents -> Lude.Maybe Lude.Text) (\s a -> s {versionLabel = a} :: DescribeEvents)
{-# DEPRECATED deVersionLabel "Use generic-lens or generic-optics with 'versionLabel' instead." #-}

-- | The ARN of a custom platform version. If specified, AWS Elastic Beanstalk restricts the returned descriptions to those associated with this custom platform version.
--
-- /Note:/ Consider using 'platformARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dePlatformARN :: Lens.Lens' DescribeEvents (Lude.Maybe Lude.Text)
dePlatformARN = Lens.lens (platformARN :: DescribeEvents -> Lude.Maybe Lude.Text) (\s a -> s {platformARN = a} :: DescribeEvents)
{-# DEPRECATED dePlatformARN "Use generic-lens or generic-optics with 'platformARN' instead." #-}

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to those associated with this environment.
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deEnvironmentName :: Lens.Lens' DescribeEvents (Lude.Maybe Lude.Text)
deEnvironmentName = Lens.lens (environmentName :: DescribeEvents -> Lude.Maybe Lude.Text) (\s a -> s {environmentName = a} :: DescribeEvents)
{-# DEPRECATED deEnvironmentName "Use generic-lens or generic-optics with 'environmentName' instead." #-}

-- | Specifies the maximum number of events that can be returned, beginning with the most recent event.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deMaxRecords :: Lens.Lens' DescribeEvents (Lude.Maybe Lude.Natural)
deMaxRecords = Lens.lens (maxRecords :: DescribeEvents -> Lude.Maybe Lude.Natural) (\s a -> s {maxRecords = a} :: DescribeEvents)
{-# DEPRECATED deMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to those that occur up to, but not including, the @EndTime@ .
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deEndTime :: Lens.Lens' DescribeEvents (Lude.Maybe Lude.ISO8601)
deEndTime = Lens.lens (endTime :: DescribeEvents -> Lude.Maybe Lude.ISO8601) (\s a -> s {endTime = a} :: DescribeEvents)
{-# DEPRECATED deEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to include only those associated with this application.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deApplicationName :: Lens.Lens' DescribeEvents (Lude.Maybe Lude.Text)
deApplicationName = Lens.lens (applicationName :: DescribeEvents -> Lude.Maybe Lude.Text) (\s a -> s {applicationName = a} :: DescribeEvents)
{-# DEPRECATED deApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to those associated with this environment.
--
-- /Note:/ Consider using 'environmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deEnvironmentId :: Lens.Lens' DescribeEvents (Lude.Maybe Lude.Text)
deEnvironmentId = Lens.lens (environmentId :: DescribeEvents -> Lude.Maybe Lude.Text) (\s a -> s {environmentId = a} :: DescribeEvents)
{-# DEPRECATED deEnvironmentId "Use generic-lens or generic-optics with 'environmentId' instead." #-}

instance Page.AWSPager DescribeEvents where
  page rq rs
    | Page.stop (rs Lens.^. dersNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dersEvents) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& deNextToken Lens..~ rs Lens.^. dersNextToken

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
  { nextToken ::
      Lude.Maybe Lude.Text,
    events :: Lude.Maybe [EventDescription],
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

-- | Creates a value of 'DescribeEventsResponse' with the minimum fields required to make a request.
--
-- * 'events' - A list of 'EventDescription' .
-- * 'nextToken' - If returned, this indicates that there are more results to obtain. Use this token in the next 'DescribeEvents' call to get the next batch of events.
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
