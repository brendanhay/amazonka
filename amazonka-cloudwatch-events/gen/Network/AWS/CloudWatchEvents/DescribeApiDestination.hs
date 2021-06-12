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
-- Module      : Network.AWS.CloudWatchEvents.DescribeApiDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves details about an API destination.
module Network.AWS.CloudWatchEvents.DescribeApiDestination
  ( -- * Creating a Request
    DescribeApiDestination (..),
    newDescribeApiDestination,

    -- * Request Lenses
    describeApiDestination_name,

    -- * Destructuring the Response
    DescribeApiDestinationResponse (..),
    newDescribeApiDestinationResponse,

    -- * Response Lenses
    describeApiDestinationResponse_httpMethod,
    describeApiDestinationResponse_creationTime,
    describeApiDestinationResponse_apiDestinationArn,
    describeApiDestinationResponse_invocationEndpoint,
    describeApiDestinationResponse_apiDestinationState,
    describeApiDestinationResponse_connectionArn,
    describeApiDestinationResponse_name,
    describeApiDestinationResponse_lastModifiedTime,
    describeApiDestinationResponse_description,
    describeApiDestinationResponse_invocationRateLimitPerSecond,
    describeApiDestinationResponse_httpStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeApiDestination' smart constructor.
data DescribeApiDestination = DescribeApiDestination'
  { -- | The name of the API destination to retrieve.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeApiDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'describeApiDestination_name' - The name of the API destination to retrieve.
newDescribeApiDestination ::
  -- | 'name'
  Core.Text ->
  DescribeApiDestination
newDescribeApiDestination pName_ =
  DescribeApiDestination' {name = pName_}

-- | The name of the API destination to retrieve.
describeApiDestination_name :: Lens.Lens' DescribeApiDestination Core.Text
describeApiDestination_name = Lens.lens (\DescribeApiDestination' {name} -> name) (\s@DescribeApiDestination' {} a -> s {name = a} :: DescribeApiDestination)

instance Core.AWSRequest DescribeApiDestination where
  type
    AWSResponse DescribeApiDestination =
      DescribeApiDestinationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeApiDestinationResponse'
            Core.<$> (x Core..?> "HttpMethod")
            Core.<*> (x Core..?> "CreationTime")
            Core.<*> (x Core..?> "ApiDestinationArn")
            Core.<*> (x Core..?> "InvocationEndpoint")
            Core.<*> (x Core..?> "ApiDestinationState")
            Core.<*> (x Core..?> "ConnectionArn")
            Core.<*> (x Core..?> "Name")
            Core.<*> (x Core..?> "LastModifiedTime")
            Core.<*> (x Core..?> "Description")
            Core.<*> (x Core..?> "InvocationRateLimitPerSecond")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeApiDestination

instance Core.NFData DescribeApiDestination

instance Core.ToHeaders DescribeApiDestination where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSEvents.DescribeApiDestination" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeApiDestination where
  toJSON DescribeApiDestination' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.ToPath DescribeApiDestination where
  toPath = Core.const "/"

instance Core.ToQuery DescribeApiDestination where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeApiDestinationResponse' smart constructor.
data DescribeApiDestinationResponse = DescribeApiDestinationResponse'
  { -- | The method to use to connect to the HTTP endpoint.
    httpMethod :: Core.Maybe ApiDestinationHttpMethod,
    -- | A time stamp for the time that the API destination was created.
    creationTime :: Core.Maybe Core.POSIX,
    -- | The ARN of the API destination retrieved.
    apiDestinationArn :: Core.Maybe Core.Text,
    -- | The URL to use to connect to the HTTP endpoint.
    invocationEndpoint :: Core.Maybe Core.Text,
    -- | The state of the API destination retrieved.
    apiDestinationState :: Core.Maybe ApiDestinationState,
    -- | The ARN of the connection specified for the API destination retrieved.
    connectionArn :: Core.Maybe Core.Text,
    -- | The name of the API destination retrieved.
    name :: Core.Maybe Core.Text,
    -- | A time stamp for the time that the API destination was last modified.
    lastModifiedTime :: Core.Maybe Core.POSIX,
    -- | The description for the API destination retrieved.
    description :: Core.Maybe Core.Text,
    -- | The maximum number of invocations per second to specified for the API
    -- destination. Note that if you set the invocation rate maximum to a value
    -- lower the rate necessary to send all events received on to the
    -- destination HTTP endpoint, some events may not be delivered within the
    -- 24-hour retry window. If you plan to set the rate lower than the rate
    -- necessary to deliver all events, consider using a dead-letter queue to
    -- catch events that are not delivered within 24 hours.
    invocationRateLimitPerSecond :: Core.Maybe Core.Natural,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeApiDestinationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpMethod', 'describeApiDestinationResponse_httpMethod' - The method to use to connect to the HTTP endpoint.
--
-- 'creationTime', 'describeApiDestinationResponse_creationTime' - A time stamp for the time that the API destination was created.
--
-- 'apiDestinationArn', 'describeApiDestinationResponse_apiDestinationArn' - The ARN of the API destination retrieved.
--
-- 'invocationEndpoint', 'describeApiDestinationResponse_invocationEndpoint' - The URL to use to connect to the HTTP endpoint.
--
-- 'apiDestinationState', 'describeApiDestinationResponse_apiDestinationState' - The state of the API destination retrieved.
--
-- 'connectionArn', 'describeApiDestinationResponse_connectionArn' - The ARN of the connection specified for the API destination retrieved.
--
-- 'name', 'describeApiDestinationResponse_name' - The name of the API destination retrieved.
--
-- 'lastModifiedTime', 'describeApiDestinationResponse_lastModifiedTime' - A time stamp for the time that the API destination was last modified.
--
-- 'description', 'describeApiDestinationResponse_description' - The description for the API destination retrieved.
--
-- 'invocationRateLimitPerSecond', 'describeApiDestinationResponse_invocationRateLimitPerSecond' - The maximum number of invocations per second to specified for the API
-- destination. Note that if you set the invocation rate maximum to a value
-- lower the rate necessary to send all events received on to the
-- destination HTTP endpoint, some events may not be delivered within the
-- 24-hour retry window. If you plan to set the rate lower than the rate
-- necessary to deliver all events, consider using a dead-letter queue to
-- catch events that are not delivered within 24 hours.
--
-- 'httpStatus', 'describeApiDestinationResponse_httpStatus' - The response's http status code.
newDescribeApiDestinationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeApiDestinationResponse
newDescribeApiDestinationResponse pHttpStatus_ =
  DescribeApiDestinationResponse'
    { httpMethod =
        Core.Nothing,
      creationTime = Core.Nothing,
      apiDestinationArn = Core.Nothing,
      invocationEndpoint = Core.Nothing,
      apiDestinationState = Core.Nothing,
      connectionArn = Core.Nothing,
      name = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      description = Core.Nothing,
      invocationRateLimitPerSecond = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The method to use to connect to the HTTP endpoint.
describeApiDestinationResponse_httpMethod :: Lens.Lens' DescribeApiDestinationResponse (Core.Maybe ApiDestinationHttpMethod)
describeApiDestinationResponse_httpMethod = Lens.lens (\DescribeApiDestinationResponse' {httpMethod} -> httpMethod) (\s@DescribeApiDestinationResponse' {} a -> s {httpMethod = a} :: DescribeApiDestinationResponse)

-- | A time stamp for the time that the API destination was created.
describeApiDestinationResponse_creationTime :: Lens.Lens' DescribeApiDestinationResponse (Core.Maybe Core.UTCTime)
describeApiDestinationResponse_creationTime = Lens.lens (\DescribeApiDestinationResponse' {creationTime} -> creationTime) (\s@DescribeApiDestinationResponse' {} a -> s {creationTime = a} :: DescribeApiDestinationResponse) Core.. Lens.mapping Core._Time

-- | The ARN of the API destination retrieved.
describeApiDestinationResponse_apiDestinationArn :: Lens.Lens' DescribeApiDestinationResponse (Core.Maybe Core.Text)
describeApiDestinationResponse_apiDestinationArn = Lens.lens (\DescribeApiDestinationResponse' {apiDestinationArn} -> apiDestinationArn) (\s@DescribeApiDestinationResponse' {} a -> s {apiDestinationArn = a} :: DescribeApiDestinationResponse)

-- | The URL to use to connect to the HTTP endpoint.
describeApiDestinationResponse_invocationEndpoint :: Lens.Lens' DescribeApiDestinationResponse (Core.Maybe Core.Text)
describeApiDestinationResponse_invocationEndpoint = Lens.lens (\DescribeApiDestinationResponse' {invocationEndpoint} -> invocationEndpoint) (\s@DescribeApiDestinationResponse' {} a -> s {invocationEndpoint = a} :: DescribeApiDestinationResponse)

-- | The state of the API destination retrieved.
describeApiDestinationResponse_apiDestinationState :: Lens.Lens' DescribeApiDestinationResponse (Core.Maybe ApiDestinationState)
describeApiDestinationResponse_apiDestinationState = Lens.lens (\DescribeApiDestinationResponse' {apiDestinationState} -> apiDestinationState) (\s@DescribeApiDestinationResponse' {} a -> s {apiDestinationState = a} :: DescribeApiDestinationResponse)

-- | The ARN of the connection specified for the API destination retrieved.
describeApiDestinationResponse_connectionArn :: Lens.Lens' DescribeApiDestinationResponse (Core.Maybe Core.Text)
describeApiDestinationResponse_connectionArn = Lens.lens (\DescribeApiDestinationResponse' {connectionArn} -> connectionArn) (\s@DescribeApiDestinationResponse' {} a -> s {connectionArn = a} :: DescribeApiDestinationResponse)

-- | The name of the API destination retrieved.
describeApiDestinationResponse_name :: Lens.Lens' DescribeApiDestinationResponse (Core.Maybe Core.Text)
describeApiDestinationResponse_name = Lens.lens (\DescribeApiDestinationResponse' {name} -> name) (\s@DescribeApiDestinationResponse' {} a -> s {name = a} :: DescribeApiDestinationResponse)

-- | A time stamp for the time that the API destination was last modified.
describeApiDestinationResponse_lastModifiedTime :: Lens.Lens' DescribeApiDestinationResponse (Core.Maybe Core.UTCTime)
describeApiDestinationResponse_lastModifiedTime = Lens.lens (\DescribeApiDestinationResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeApiDestinationResponse' {} a -> s {lastModifiedTime = a} :: DescribeApiDestinationResponse) Core.. Lens.mapping Core._Time

-- | The description for the API destination retrieved.
describeApiDestinationResponse_description :: Lens.Lens' DescribeApiDestinationResponse (Core.Maybe Core.Text)
describeApiDestinationResponse_description = Lens.lens (\DescribeApiDestinationResponse' {description} -> description) (\s@DescribeApiDestinationResponse' {} a -> s {description = a} :: DescribeApiDestinationResponse)

-- | The maximum number of invocations per second to specified for the API
-- destination. Note that if you set the invocation rate maximum to a value
-- lower the rate necessary to send all events received on to the
-- destination HTTP endpoint, some events may not be delivered within the
-- 24-hour retry window. If you plan to set the rate lower than the rate
-- necessary to deliver all events, consider using a dead-letter queue to
-- catch events that are not delivered within 24 hours.
describeApiDestinationResponse_invocationRateLimitPerSecond :: Lens.Lens' DescribeApiDestinationResponse (Core.Maybe Core.Natural)
describeApiDestinationResponse_invocationRateLimitPerSecond = Lens.lens (\DescribeApiDestinationResponse' {invocationRateLimitPerSecond} -> invocationRateLimitPerSecond) (\s@DescribeApiDestinationResponse' {} a -> s {invocationRateLimitPerSecond = a} :: DescribeApiDestinationResponse)

-- | The response's http status code.
describeApiDestinationResponse_httpStatus :: Lens.Lens' DescribeApiDestinationResponse Core.Int
describeApiDestinationResponse_httpStatus = Lens.lens (\DescribeApiDestinationResponse' {httpStatus} -> httpStatus) (\s@DescribeApiDestinationResponse' {} a -> s {httpStatus = a} :: DescribeApiDestinationResponse)

instance Core.NFData DescribeApiDestinationResponse
