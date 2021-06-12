{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.DescribeEvents
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns list of event descriptions matching criteria up to the last 6
-- weeks.
--
-- This action returns the most recent 1,000 events from the specified
-- @NextToken@.
--
-- This operation returns paginated results.
module Network.AWS.ElasticBeanstalk.DescribeEvents
  ( -- * Creating a Request
    DescribeEvents (..),
    newDescribeEvents,

    -- * Request Lenses
    describeEvents_templateName,
    describeEvents_nextToken,
    describeEvents_severity,
    describeEvents_environmentId,
    describeEvents_startTime,
    describeEvents_environmentName,
    describeEvents_endTime,
    describeEvents_platformArn,
    describeEvents_versionLabel,
    describeEvents_requestId,
    describeEvents_applicationName,
    describeEvents_maxRecords,

    -- * Destructuring the Response
    DescribeEventsResponse (..),
    newDescribeEventsResponse,

    -- * Response Lenses
    describeEventsResponse_nextToken,
    describeEventsResponse_events,
    describeEventsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to retrieve a list of events for an environment.
--
-- /See:/ 'newDescribeEvents' smart constructor.
data DescribeEvents = DescribeEvents'
  { -- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
    -- to those that are associated with this environment configuration.
    templateName :: Core.Maybe Core.Text,
    -- | Pagination token. If specified, the events return the next batch of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | If specified, limits the events returned from this call to include only
    -- those with the specified severity or higher.
    severity :: Core.Maybe EventSeverity,
    -- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
    -- to those associated with this environment.
    environmentId :: Core.Maybe Core.Text,
    -- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
    -- to those that occur on or after this time.
    startTime :: Core.Maybe Core.ISO8601,
    -- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
    -- to those associated with this environment.
    environmentName :: Core.Maybe Core.Text,
    -- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
    -- to those that occur up to, but not including, the @EndTime@.
    endTime :: Core.Maybe Core.ISO8601,
    -- | The ARN of a custom platform version. If specified, AWS Elastic
    -- Beanstalk restricts the returned descriptions to those associated with
    -- this custom platform version.
    platformArn :: Core.Maybe Core.Text,
    -- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
    -- to those associated with this application version.
    versionLabel :: Core.Maybe Core.Text,
    -- | If specified, AWS Elastic Beanstalk restricts the described events to
    -- include only those associated with this request ID.
    requestId :: Core.Maybe Core.Text,
    -- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
    -- to include only those associated with this application.
    applicationName :: Core.Maybe Core.Text,
    -- | Specifies the maximum number of events that can be returned, beginning
    -- with the most recent event.
    maxRecords :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeEvents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateName', 'describeEvents_templateName' - If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to those that are associated with this environment configuration.
--
-- 'nextToken', 'describeEvents_nextToken' - Pagination token. If specified, the events return the next batch of
-- results.
--
-- 'severity', 'describeEvents_severity' - If specified, limits the events returned from this call to include only
-- those with the specified severity or higher.
--
-- 'environmentId', 'describeEvents_environmentId' - If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to those associated with this environment.
--
-- 'startTime', 'describeEvents_startTime' - If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to those that occur on or after this time.
--
-- 'environmentName', 'describeEvents_environmentName' - If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to those associated with this environment.
--
-- 'endTime', 'describeEvents_endTime' - If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to those that occur up to, but not including, the @EndTime@.
--
-- 'platformArn', 'describeEvents_platformArn' - The ARN of a custom platform version. If specified, AWS Elastic
-- Beanstalk restricts the returned descriptions to those associated with
-- this custom platform version.
--
-- 'versionLabel', 'describeEvents_versionLabel' - If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to those associated with this application version.
--
-- 'requestId', 'describeEvents_requestId' - If specified, AWS Elastic Beanstalk restricts the described events to
-- include only those associated with this request ID.
--
-- 'applicationName', 'describeEvents_applicationName' - If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to include only those associated with this application.
--
-- 'maxRecords', 'describeEvents_maxRecords' - Specifies the maximum number of events that can be returned, beginning
-- with the most recent event.
newDescribeEvents ::
  DescribeEvents
newDescribeEvents =
  DescribeEvents'
    { templateName = Core.Nothing,
      nextToken = Core.Nothing,
      severity = Core.Nothing,
      environmentId = Core.Nothing,
      startTime = Core.Nothing,
      environmentName = Core.Nothing,
      endTime = Core.Nothing,
      platformArn = Core.Nothing,
      versionLabel = Core.Nothing,
      requestId = Core.Nothing,
      applicationName = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to those that are associated with this environment configuration.
describeEvents_templateName :: Lens.Lens' DescribeEvents (Core.Maybe Core.Text)
describeEvents_templateName = Lens.lens (\DescribeEvents' {templateName} -> templateName) (\s@DescribeEvents' {} a -> s {templateName = a} :: DescribeEvents)

-- | Pagination token. If specified, the events return the next batch of
-- results.
describeEvents_nextToken :: Lens.Lens' DescribeEvents (Core.Maybe Core.Text)
describeEvents_nextToken = Lens.lens (\DescribeEvents' {nextToken} -> nextToken) (\s@DescribeEvents' {} a -> s {nextToken = a} :: DescribeEvents)

-- | If specified, limits the events returned from this call to include only
-- those with the specified severity or higher.
describeEvents_severity :: Lens.Lens' DescribeEvents (Core.Maybe EventSeverity)
describeEvents_severity = Lens.lens (\DescribeEvents' {severity} -> severity) (\s@DescribeEvents' {} a -> s {severity = a} :: DescribeEvents)

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to those associated with this environment.
describeEvents_environmentId :: Lens.Lens' DescribeEvents (Core.Maybe Core.Text)
describeEvents_environmentId = Lens.lens (\DescribeEvents' {environmentId} -> environmentId) (\s@DescribeEvents' {} a -> s {environmentId = a} :: DescribeEvents)

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to those that occur on or after this time.
describeEvents_startTime :: Lens.Lens' DescribeEvents (Core.Maybe Core.UTCTime)
describeEvents_startTime = Lens.lens (\DescribeEvents' {startTime} -> startTime) (\s@DescribeEvents' {} a -> s {startTime = a} :: DescribeEvents) Core.. Lens.mapping Core._Time

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to those associated with this environment.
describeEvents_environmentName :: Lens.Lens' DescribeEvents (Core.Maybe Core.Text)
describeEvents_environmentName = Lens.lens (\DescribeEvents' {environmentName} -> environmentName) (\s@DescribeEvents' {} a -> s {environmentName = a} :: DescribeEvents)

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to those that occur up to, but not including, the @EndTime@.
describeEvents_endTime :: Lens.Lens' DescribeEvents (Core.Maybe Core.UTCTime)
describeEvents_endTime = Lens.lens (\DescribeEvents' {endTime} -> endTime) (\s@DescribeEvents' {} a -> s {endTime = a} :: DescribeEvents) Core.. Lens.mapping Core._Time

-- | The ARN of a custom platform version. If specified, AWS Elastic
-- Beanstalk restricts the returned descriptions to those associated with
-- this custom platform version.
describeEvents_platformArn :: Lens.Lens' DescribeEvents (Core.Maybe Core.Text)
describeEvents_platformArn = Lens.lens (\DescribeEvents' {platformArn} -> platformArn) (\s@DescribeEvents' {} a -> s {platformArn = a} :: DescribeEvents)

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to those associated with this application version.
describeEvents_versionLabel :: Lens.Lens' DescribeEvents (Core.Maybe Core.Text)
describeEvents_versionLabel = Lens.lens (\DescribeEvents' {versionLabel} -> versionLabel) (\s@DescribeEvents' {} a -> s {versionLabel = a} :: DescribeEvents)

-- | If specified, AWS Elastic Beanstalk restricts the described events to
-- include only those associated with this request ID.
describeEvents_requestId :: Lens.Lens' DescribeEvents (Core.Maybe Core.Text)
describeEvents_requestId = Lens.lens (\DescribeEvents' {requestId} -> requestId) (\s@DescribeEvents' {} a -> s {requestId = a} :: DescribeEvents)

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to include only those associated with this application.
describeEvents_applicationName :: Lens.Lens' DescribeEvents (Core.Maybe Core.Text)
describeEvents_applicationName = Lens.lens (\DescribeEvents' {applicationName} -> applicationName) (\s@DescribeEvents' {} a -> s {applicationName = a} :: DescribeEvents)

-- | Specifies the maximum number of events that can be returned, beginning
-- with the most recent event.
describeEvents_maxRecords :: Lens.Lens' DescribeEvents (Core.Maybe Core.Natural)
describeEvents_maxRecords = Lens.lens (\DescribeEvents' {maxRecords} -> maxRecords) (\s@DescribeEvents' {} a -> s {maxRecords = a} :: DescribeEvents)

instance Core.AWSPager DescribeEvents where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeEventsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeEventsResponse_events Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeEvents_nextToken
          Lens..~ rs
          Lens.^? describeEventsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest DescribeEvents where
  type
    AWSResponse DescribeEvents =
      DescribeEventsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeEventsResult"
      ( \s h x ->
          DescribeEventsResponse'
            Core.<$> (x Core..@? "NextToken")
            Core.<*> ( x Core..@? "Events" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeEvents

instance Core.NFData DescribeEvents

instance Core.ToHeaders DescribeEvents where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeEvents where
  toPath = Core.const "/"

instance Core.ToQuery DescribeEvents where
  toQuery DescribeEvents' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeEvents" :: Core.ByteString),
        "Version" Core.=: ("2010-12-01" :: Core.ByteString),
        "TemplateName" Core.=: templateName,
        "NextToken" Core.=: nextToken,
        "Severity" Core.=: severity,
        "EnvironmentId" Core.=: environmentId,
        "StartTime" Core.=: startTime,
        "EnvironmentName" Core.=: environmentName,
        "EndTime" Core.=: endTime,
        "PlatformArn" Core.=: platformArn,
        "VersionLabel" Core.=: versionLabel,
        "RequestId" Core.=: requestId,
        "ApplicationName" Core.=: applicationName,
        "MaxRecords" Core.=: maxRecords
      ]

-- | Result message wrapping a list of event descriptions.
--
-- /See:/ 'newDescribeEventsResponse' smart constructor.
data DescribeEventsResponse = DescribeEventsResponse'
  { -- | If returned, this indicates that there are more results to obtain. Use
    -- this token in the next DescribeEvents call to get the next batch of
    -- events.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of EventDescription.
    events :: Core.Maybe [EventDescription],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeEventsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeEventsResponse_nextToken' - If returned, this indicates that there are more results to obtain. Use
-- this token in the next DescribeEvents call to get the next batch of
-- events.
--
-- 'events', 'describeEventsResponse_events' - A list of EventDescription.
--
-- 'httpStatus', 'describeEventsResponse_httpStatus' - The response's http status code.
newDescribeEventsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeEventsResponse
newDescribeEventsResponse pHttpStatus_ =
  DescribeEventsResponse'
    { nextToken = Core.Nothing,
      events = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If returned, this indicates that there are more results to obtain. Use
-- this token in the next DescribeEvents call to get the next batch of
-- events.
describeEventsResponse_nextToken :: Lens.Lens' DescribeEventsResponse (Core.Maybe Core.Text)
describeEventsResponse_nextToken = Lens.lens (\DescribeEventsResponse' {nextToken} -> nextToken) (\s@DescribeEventsResponse' {} a -> s {nextToken = a} :: DescribeEventsResponse)

-- | A list of EventDescription.
describeEventsResponse_events :: Lens.Lens' DescribeEventsResponse (Core.Maybe [EventDescription])
describeEventsResponse_events = Lens.lens (\DescribeEventsResponse' {events} -> events) (\s@DescribeEventsResponse' {} a -> s {events = a} :: DescribeEventsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeEventsResponse_httpStatus :: Lens.Lens' DescribeEventsResponse Core.Int
describeEventsResponse_httpStatus = Lens.lens (\DescribeEventsResponse' {httpStatus} -> httpStatus) (\s@DescribeEventsResponse' {} a -> s {httpStatus = a} :: DescribeEventsResponse)

instance Core.NFData DescribeEventsResponse
