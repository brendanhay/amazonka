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
-- Module      : Amazonka.ElasticBeanstalk.DescribeEvents
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.ElasticBeanstalk.DescribeEvents
  ( -- * Creating a Request
    DescribeEvents (..),
    newDescribeEvents,

    -- * Request Lenses
    describeEvents_applicationName,
    describeEvents_endTime,
    describeEvents_environmentId,
    describeEvents_environmentName,
    describeEvents_maxRecords,
    describeEvents_nextToken,
    describeEvents_platformArn,
    describeEvents_requestId,
    describeEvents_severity,
    describeEvents_startTime,
    describeEvents_templateName,
    describeEvents_versionLabel,

    -- * Destructuring the Response
    DescribeEventsResponse (..),
    newDescribeEventsResponse,

    -- * Response Lenses
    describeEventsResponse_events,
    describeEventsResponse_nextToken,
    describeEventsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticBeanstalk.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request to retrieve a list of events for an environment.
--
-- /See:/ 'newDescribeEvents' smart constructor.
data DescribeEvents = DescribeEvents'
  { -- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
    -- to include only those associated with this application.
    applicationName :: Prelude.Maybe Prelude.Text,
    -- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
    -- to those that occur up to, but not including, the @EndTime@.
    endTime :: Prelude.Maybe Data.ISO8601,
    -- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
    -- to those associated with this environment.
    environmentId :: Prelude.Maybe Prelude.Text,
    -- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
    -- to those associated with this environment.
    environmentName :: Prelude.Maybe Prelude.Text,
    -- | Specifies the maximum number of events that can be returned, beginning
    -- with the most recent event.
    maxRecords :: Prelude.Maybe Prelude.Natural,
    -- | Pagination token. If specified, the events return the next batch of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ARN of a custom platform version. If specified, AWS Elastic
    -- Beanstalk restricts the returned descriptions to those associated with
    -- this custom platform version.
    platformArn :: Prelude.Maybe Prelude.Text,
    -- | If specified, AWS Elastic Beanstalk restricts the described events to
    -- include only those associated with this request ID.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | If specified, limits the events returned from this call to include only
    -- those with the specified severity or higher.
    severity :: Prelude.Maybe EventSeverity,
    -- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
    -- to those that occur on or after this time.
    startTime :: Prelude.Maybe Data.ISO8601,
    -- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
    -- to those that are associated with this environment configuration.
    templateName :: Prelude.Maybe Prelude.Text,
    -- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
    -- to those associated with this application version.
    versionLabel :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEvents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationName', 'describeEvents_applicationName' - If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to include only those associated with this application.
--
-- 'endTime', 'describeEvents_endTime' - If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to those that occur up to, but not including, the @EndTime@.
--
-- 'environmentId', 'describeEvents_environmentId' - If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to those associated with this environment.
--
-- 'environmentName', 'describeEvents_environmentName' - If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to those associated with this environment.
--
-- 'maxRecords', 'describeEvents_maxRecords' - Specifies the maximum number of events that can be returned, beginning
-- with the most recent event.
--
-- 'nextToken', 'describeEvents_nextToken' - Pagination token. If specified, the events return the next batch of
-- results.
--
-- 'platformArn', 'describeEvents_platformArn' - The ARN of a custom platform version. If specified, AWS Elastic
-- Beanstalk restricts the returned descriptions to those associated with
-- this custom platform version.
--
-- 'requestId', 'describeEvents_requestId' - If specified, AWS Elastic Beanstalk restricts the described events to
-- include only those associated with this request ID.
--
-- 'severity', 'describeEvents_severity' - If specified, limits the events returned from this call to include only
-- those with the specified severity or higher.
--
-- 'startTime', 'describeEvents_startTime' - If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to those that occur on or after this time.
--
-- 'templateName', 'describeEvents_templateName' - If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to those that are associated with this environment configuration.
--
-- 'versionLabel', 'describeEvents_versionLabel' - If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to those associated with this application version.
newDescribeEvents ::
  DescribeEvents
newDescribeEvents =
  DescribeEvents'
    { applicationName = Prelude.Nothing,
      endTime = Prelude.Nothing,
      environmentId = Prelude.Nothing,
      environmentName = Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      platformArn = Prelude.Nothing,
      requestId = Prelude.Nothing,
      severity = Prelude.Nothing,
      startTime = Prelude.Nothing,
      templateName = Prelude.Nothing,
      versionLabel = Prelude.Nothing
    }

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to include only those associated with this application.
describeEvents_applicationName :: Lens.Lens' DescribeEvents (Prelude.Maybe Prelude.Text)
describeEvents_applicationName = Lens.lens (\DescribeEvents' {applicationName} -> applicationName) (\s@DescribeEvents' {} a -> s {applicationName = a} :: DescribeEvents)

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to those that occur up to, but not including, the @EndTime@.
describeEvents_endTime :: Lens.Lens' DescribeEvents (Prelude.Maybe Prelude.UTCTime)
describeEvents_endTime = Lens.lens (\DescribeEvents' {endTime} -> endTime) (\s@DescribeEvents' {} a -> s {endTime = a} :: DescribeEvents) Prelude.. Lens.mapping Data._Time

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to those associated with this environment.
describeEvents_environmentId :: Lens.Lens' DescribeEvents (Prelude.Maybe Prelude.Text)
describeEvents_environmentId = Lens.lens (\DescribeEvents' {environmentId} -> environmentId) (\s@DescribeEvents' {} a -> s {environmentId = a} :: DescribeEvents)

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to those associated with this environment.
describeEvents_environmentName :: Lens.Lens' DescribeEvents (Prelude.Maybe Prelude.Text)
describeEvents_environmentName = Lens.lens (\DescribeEvents' {environmentName} -> environmentName) (\s@DescribeEvents' {} a -> s {environmentName = a} :: DescribeEvents)

-- | Specifies the maximum number of events that can be returned, beginning
-- with the most recent event.
describeEvents_maxRecords :: Lens.Lens' DescribeEvents (Prelude.Maybe Prelude.Natural)
describeEvents_maxRecords = Lens.lens (\DescribeEvents' {maxRecords} -> maxRecords) (\s@DescribeEvents' {} a -> s {maxRecords = a} :: DescribeEvents)

-- | Pagination token. If specified, the events return the next batch of
-- results.
describeEvents_nextToken :: Lens.Lens' DescribeEvents (Prelude.Maybe Prelude.Text)
describeEvents_nextToken = Lens.lens (\DescribeEvents' {nextToken} -> nextToken) (\s@DescribeEvents' {} a -> s {nextToken = a} :: DescribeEvents)

-- | The ARN of a custom platform version. If specified, AWS Elastic
-- Beanstalk restricts the returned descriptions to those associated with
-- this custom platform version.
describeEvents_platformArn :: Lens.Lens' DescribeEvents (Prelude.Maybe Prelude.Text)
describeEvents_platformArn = Lens.lens (\DescribeEvents' {platformArn} -> platformArn) (\s@DescribeEvents' {} a -> s {platformArn = a} :: DescribeEvents)

-- | If specified, AWS Elastic Beanstalk restricts the described events to
-- include only those associated with this request ID.
describeEvents_requestId :: Lens.Lens' DescribeEvents (Prelude.Maybe Prelude.Text)
describeEvents_requestId = Lens.lens (\DescribeEvents' {requestId} -> requestId) (\s@DescribeEvents' {} a -> s {requestId = a} :: DescribeEvents)

-- | If specified, limits the events returned from this call to include only
-- those with the specified severity or higher.
describeEvents_severity :: Lens.Lens' DescribeEvents (Prelude.Maybe EventSeverity)
describeEvents_severity = Lens.lens (\DescribeEvents' {severity} -> severity) (\s@DescribeEvents' {} a -> s {severity = a} :: DescribeEvents)

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to those that occur on or after this time.
describeEvents_startTime :: Lens.Lens' DescribeEvents (Prelude.Maybe Prelude.UTCTime)
describeEvents_startTime = Lens.lens (\DescribeEvents' {startTime} -> startTime) (\s@DescribeEvents' {} a -> s {startTime = a} :: DescribeEvents) Prelude.. Lens.mapping Data._Time

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to those that are associated with this environment configuration.
describeEvents_templateName :: Lens.Lens' DescribeEvents (Prelude.Maybe Prelude.Text)
describeEvents_templateName = Lens.lens (\DescribeEvents' {templateName} -> templateName) (\s@DescribeEvents' {} a -> s {templateName = a} :: DescribeEvents)

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to those associated with this application version.
describeEvents_versionLabel :: Lens.Lens' DescribeEvents (Prelude.Maybe Prelude.Text)
describeEvents_versionLabel = Lens.lens (\DescribeEvents' {versionLabel} -> versionLabel) (\s@DescribeEvents' {} a -> s {versionLabel = a} :: DescribeEvents)

instance Core.AWSPager DescribeEvents where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeEventsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeEventsResponse_events
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeEvents_nextToken
          Lens..~ rs
          Lens.^? describeEventsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeEvents where
  type
    AWSResponse DescribeEvents =
      DescribeEventsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeEventsResult"
      ( \s h x ->
          DescribeEventsResponse'
            Prelude.<$> ( x
                            Data..@? "Events"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (x Data..@? "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeEvents where
  hashWithSalt _salt DescribeEvents' {..} =
    _salt
      `Prelude.hashWithSalt` applicationName
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` environmentId
      `Prelude.hashWithSalt` environmentName
      `Prelude.hashWithSalt` maxRecords
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` platformArn
      `Prelude.hashWithSalt` requestId
      `Prelude.hashWithSalt` severity
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` templateName
      `Prelude.hashWithSalt` versionLabel

instance Prelude.NFData DescribeEvents where
  rnf DescribeEvents' {..} =
    Prelude.rnf applicationName
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf environmentId
      `Prelude.seq` Prelude.rnf environmentName
      `Prelude.seq` Prelude.rnf maxRecords
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf platformArn
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf severity
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf templateName
      `Prelude.seq` Prelude.rnf versionLabel

instance Data.ToHeaders DescribeEvents where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeEvents where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeEvents where
  toQuery DescribeEvents' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeEvents" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-12-01" :: Prelude.ByteString),
        "ApplicationName" Data.=: applicationName,
        "EndTime" Data.=: endTime,
        "EnvironmentId" Data.=: environmentId,
        "EnvironmentName" Data.=: environmentName,
        "MaxRecords" Data.=: maxRecords,
        "NextToken" Data.=: nextToken,
        "PlatformArn" Data.=: platformArn,
        "RequestId" Data.=: requestId,
        "Severity" Data.=: severity,
        "StartTime" Data.=: startTime,
        "TemplateName" Data.=: templateName,
        "VersionLabel" Data.=: versionLabel
      ]

-- | Result message wrapping a list of event descriptions.
--
-- /See:/ 'newDescribeEventsResponse' smart constructor.
data DescribeEventsResponse = DescribeEventsResponse'
  { -- | A list of EventDescription.
    events :: Prelude.Maybe [EventDescription],
    -- | If returned, this indicates that there are more results to obtain. Use
    -- this token in the next DescribeEvents call to get the next batch of
    -- events.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEventsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'events', 'describeEventsResponse_events' - A list of EventDescription.
--
-- 'nextToken', 'describeEventsResponse_nextToken' - If returned, this indicates that there are more results to obtain. Use
-- this token in the next DescribeEvents call to get the next batch of
-- events.
--
-- 'httpStatus', 'describeEventsResponse_httpStatus' - The response's http status code.
newDescribeEventsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeEventsResponse
newDescribeEventsResponse pHttpStatus_ =
  DescribeEventsResponse'
    { events = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of EventDescription.
describeEventsResponse_events :: Lens.Lens' DescribeEventsResponse (Prelude.Maybe [EventDescription])
describeEventsResponse_events = Lens.lens (\DescribeEventsResponse' {events} -> events) (\s@DescribeEventsResponse' {} a -> s {events = a} :: DescribeEventsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If returned, this indicates that there are more results to obtain. Use
-- this token in the next DescribeEvents call to get the next batch of
-- events.
describeEventsResponse_nextToken :: Lens.Lens' DescribeEventsResponse (Prelude.Maybe Prelude.Text)
describeEventsResponse_nextToken = Lens.lens (\DescribeEventsResponse' {nextToken} -> nextToken) (\s@DescribeEventsResponse' {} a -> s {nextToken = a} :: DescribeEventsResponse)

-- | The response's http status code.
describeEventsResponse_httpStatus :: Lens.Lens' DescribeEventsResponse Prelude.Int
describeEventsResponse_httpStatus = Lens.lens (\DescribeEventsResponse' {httpStatus} -> httpStatus) (\s@DescribeEventsResponse' {} a -> s {httpStatus = a} :: DescribeEventsResponse)

instance Prelude.NFData DescribeEventsResponse where
  rnf DescribeEventsResponse' {..} =
    Prelude.rnf events
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
