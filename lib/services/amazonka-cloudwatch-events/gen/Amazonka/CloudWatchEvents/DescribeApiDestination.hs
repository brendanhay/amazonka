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
-- Module      : Amazonka.CloudWatchEvents.DescribeApiDestination
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves details about an API destination.
module Amazonka.CloudWatchEvents.DescribeApiDestination
  ( -- * Creating a Request
    DescribeApiDestination (..),
    newDescribeApiDestination,

    -- * Request Lenses
    describeApiDestination_name,

    -- * Destructuring the Response
    DescribeApiDestinationResponse (..),
    newDescribeApiDestinationResponse,

    -- * Response Lenses
    describeApiDestinationResponse_apiDestinationArn,
    describeApiDestinationResponse_apiDestinationState,
    describeApiDestinationResponse_connectionArn,
    describeApiDestinationResponse_creationTime,
    describeApiDestinationResponse_description,
    describeApiDestinationResponse_httpMethod,
    describeApiDestinationResponse_invocationEndpoint,
    describeApiDestinationResponse_invocationRateLimitPerSecond,
    describeApiDestinationResponse_lastModifiedTime,
    describeApiDestinationResponse_name,
    describeApiDestinationResponse_httpStatus,
  )
where

import Amazonka.CloudWatchEvents.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeApiDestination' smart constructor.
data DescribeApiDestination = DescribeApiDestination'
  { -- | The name of the API destination to retrieve.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DescribeApiDestination
newDescribeApiDestination pName_ =
  DescribeApiDestination' {name = pName_}

-- | The name of the API destination to retrieve.
describeApiDestination_name :: Lens.Lens' DescribeApiDestination Prelude.Text
describeApiDestination_name = Lens.lens (\DescribeApiDestination' {name} -> name) (\s@DescribeApiDestination' {} a -> s {name = a} :: DescribeApiDestination)

instance Core.AWSRequest DescribeApiDestination where
  type
    AWSResponse DescribeApiDestination =
      DescribeApiDestinationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeApiDestinationResponse'
            Prelude.<$> (x Data..?> "ApiDestinationArn")
            Prelude.<*> (x Data..?> "ApiDestinationState")
            Prelude.<*> (x Data..?> "ConnectionArn")
            Prelude.<*> (x Data..?> "CreationTime")
            Prelude.<*> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "HttpMethod")
            Prelude.<*> (x Data..?> "InvocationEndpoint")
            Prelude.<*> (x Data..?> "InvocationRateLimitPerSecond")
            Prelude.<*> (x Data..?> "LastModifiedTime")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeApiDestination where
  hashWithSalt _salt DescribeApiDestination' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DescribeApiDestination where
  rnf DescribeApiDestination' {..} = Prelude.rnf name

instance Data.ToHeaders DescribeApiDestination where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSEvents.DescribeApiDestination" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeApiDestination where
  toJSON DescribeApiDestination' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Data..= name)]
      )

instance Data.ToPath DescribeApiDestination where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeApiDestination where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeApiDestinationResponse' smart constructor.
data DescribeApiDestinationResponse = DescribeApiDestinationResponse'
  { -- | The ARN of the API destination retrieved.
    apiDestinationArn :: Prelude.Maybe Prelude.Text,
    -- | The state of the API destination retrieved.
    apiDestinationState :: Prelude.Maybe ApiDestinationState,
    -- | The ARN of the connection specified for the API destination retrieved.
    connectionArn :: Prelude.Maybe Prelude.Text,
    -- | A time stamp for the time that the API destination was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The description for the API destination retrieved.
    description :: Prelude.Maybe Prelude.Text,
    -- | The method to use to connect to the HTTP endpoint.
    httpMethod :: Prelude.Maybe ApiDestinationHttpMethod,
    -- | The URL to use to connect to the HTTP endpoint.
    invocationEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of invocations per second to specified for the API
    -- destination. Note that if you set the invocation rate maximum to a value
    -- lower the rate necessary to send all events received on to the
    -- destination HTTP endpoint, some events may not be delivered within the
    -- 24-hour retry window. If you plan to set the rate lower than the rate
    -- necessary to deliver all events, consider using a dead-letter queue to
    -- catch events that are not delivered within 24 hours.
    invocationRateLimitPerSecond :: Prelude.Maybe Prelude.Natural,
    -- | A time stamp for the time that the API destination was last modified.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the API destination retrieved.
    name :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeApiDestinationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiDestinationArn', 'describeApiDestinationResponse_apiDestinationArn' - The ARN of the API destination retrieved.
--
-- 'apiDestinationState', 'describeApiDestinationResponse_apiDestinationState' - The state of the API destination retrieved.
--
-- 'connectionArn', 'describeApiDestinationResponse_connectionArn' - The ARN of the connection specified for the API destination retrieved.
--
-- 'creationTime', 'describeApiDestinationResponse_creationTime' - A time stamp for the time that the API destination was created.
--
-- 'description', 'describeApiDestinationResponse_description' - The description for the API destination retrieved.
--
-- 'httpMethod', 'describeApiDestinationResponse_httpMethod' - The method to use to connect to the HTTP endpoint.
--
-- 'invocationEndpoint', 'describeApiDestinationResponse_invocationEndpoint' - The URL to use to connect to the HTTP endpoint.
--
-- 'invocationRateLimitPerSecond', 'describeApiDestinationResponse_invocationRateLimitPerSecond' - The maximum number of invocations per second to specified for the API
-- destination. Note that if you set the invocation rate maximum to a value
-- lower the rate necessary to send all events received on to the
-- destination HTTP endpoint, some events may not be delivered within the
-- 24-hour retry window. If you plan to set the rate lower than the rate
-- necessary to deliver all events, consider using a dead-letter queue to
-- catch events that are not delivered within 24 hours.
--
-- 'lastModifiedTime', 'describeApiDestinationResponse_lastModifiedTime' - A time stamp for the time that the API destination was last modified.
--
-- 'name', 'describeApiDestinationResponse_name' - The name of the API destination retrieved.
--
-- 'httpStatus', 'describeApiDestinationResponse_httpStatus' - The response's http status code.
newDescribeApiDestinationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeApiDestinationResponse
newDescribeApiDestinationResponse pHttpStatus_ =
  DescribeApiDestinationResponse'
    { apiDestinationArn =
        Prelude.Nothing,
      apiDestinationState = Prelude.Nothing,
      connectionArn = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      description = Prelude.Nothing,
      httpMethod = Prelude.Nothing,
      invocationEndpoint = Prelude.Nothing,
      invocationRateLimitPerSecond =
        Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      name = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the API destination retrieved.
describeApiDestinationResponse_apiDestinationArn :: Lens.Lens' DescribeApiDestinationResponse (Prelude.Maybe Prelude.Text)
describeApiDestinationResponse_apiDestinationArn = Lens.lens (\DescribeApiDestinationResponse' {apiDestinationArn} -> apiDestinationArn) (\s@DescribeApiDestinationResponse' {} a -> s {apiDestinationArn = a} :: DescribeApiDestinationResponse)

-- | The state of the API destination retrieved.
describeApiDestinationResponse_apiDestinationState :: Lens.Lens' DescribeApiDestinationResponse (Prelude.Maybe ApiDestinationState)
describeApiDestinationResponse_apiDestinationState = Lens.lens (\DescribeApiDestinationResponse' {apiDestinationState} -> apiDestinationState) (\s@DescribeApiDestinationResponse' {} a -> s {apiDestinationState = a} :: DescribeApiDestinationResponse)

-- | The ARN of the connection specified for the API destination retrieved.
describeApiDestinationResponse_connectionArn :: Lens.Lens' DescribeApiDestinationResponse (Prelude.Maybe Prelude.Text)
describeApiDestinationResponse_connectionArn = Lens.lens (\DescribeApiDestinationResponse' {connectionArn} -> connectionArn) (\s@DescribeApiDestinationResponse' {} a -> s {connectionArn = a} :: DescribeApiDestinationResponse)

-- | A time stamp for the time that the API destination was created.
describeApiDestinationResponse_creationTime :: Lens.Lens' DescribeApiDestinationResponse (Prelude.Maybe Prelude.UTCTime)
describeApiDestinationResponse_creationTime = Lens.lens (\DescribeApiDestinationResponse' {creationTime} -> creationTime) (\s@DescribeApiDestinationResponse' {} a -> s {creationTime = a} :: DescribeApiDestinationResponse) Prelude.. Lens.mapping Data._Time

-- | The description for the API destination retrieved.
describeApiDestinationResponse_description :: Lens.Lens' DescribeApiDestinationResponse (Prelude.Maybe Prelude.Text)
describeApiDestinationResponse_description = Lens.lens (\DescribeApiDestinationResponse' {description} -> description) (\s@DescribeApiDestinationResponse' {} a -> s {description = a} :: DescribeApiDestinationResponse)

-- | The method to use to connect to the HTTP endpoint.
describeApiDestinationResponse_httpMethod :: Lens.Lens' DescribeApiDestinationResponse (Prelude.Maybe ApiDestinationHttpMethod)
describeApiDestinationResponse_httpMethod = Lens.lens (\DescribeApiDestinationResponse' {httpMethod} -> httpMethod) (\s@DescribeApiDestinationResponse' {} a -> s {httpMethod = a} :: DescribeApiDestinationResponse)

-- | The URL to use to connect to the HTTP endpoint.
describeApiDestinationResponse_invocationEndpoint :: Lens.Lens' DescribeApiDestinationResponse (Prelude.Maybe Prelude.Text)
describeApiDestinationResponse_invocationEndpoint = Lens.lens (\DescribeApiDestinationResponse' {invocationEndpoint} -> invocationEndpoint) (\s@DescribeApiDestinationResponse' {} a -> s {invocationEndpoint = a} :: DescribeApiDestinationResponse)

-- | The maximum number of invocations per second to specified for the API
-- destination. Note that if you set the invocation rate maximum to a value
-- lower the rate necessary to send all events received on to the
-- destination HTTP endpoint, some events may not be delivered within the
-- 24-hour retry window. If you plan to set the rate lower than the rate
-- necessary to deliver all events, consider using a dead-letter queue to
-- catch events that are not delivered within 24 hours.
describeApiDestinationResponse_invocationRateLimitPerSecond :: Lens.Lens' DescribeApiDestinationResponse (Prelude.Maybe Prelude.Natural)
describeApiDestinationResponse_invocationRateLimitPerSecond = Lens.lens (\DescribeApiDestinationResponse' {invocationRateLimitPerSecond} -> invocationRateLimitPerSecond) (\s@DescribeApiDestinationResponse' {} a -> s {invocationRateLimitPerSecond = a} :: DescribeApiDestinationResponse)

-- | A time stamp for the time that the API destination was last modified.
describeApiDestinationResponse_lastModifiedTime :: Lens.Lens' DescribeApiDestinationResponse (Prelude.Maybe Prelude.UTCTime)
describeApiDestinationResponse_lastModifiedTime = Lens.lens (\DescribeApiDestinationResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeApiDestinationResponse' {} a -> s {lastModifiedTime = a} :: DescribeApiDestinationResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the API destination retrieved.
describeApiDestinationResponse_name :: Lens.Lens' DescribeApiDestinationResponse (Prelude.Maybe Prelude.Text)
describeApiDestinationResponse_name = Lens.lens (\DescribeApiDestinationResponse' {name} -> name) (\s@DescribeApiDestinationResponse' {} a -> s {name = a} :: DescribeApiDestinationResponse)

-- | The response's http status code.
describeApiDestinationResponse_httpStatus :: Lens.Lens' DescribeApiDestinationResponse Prelude.Int
describeApiDestinationResponse_httpStatus = Lens.lens (\DescribeApiDestinationResponse' {httpStatus} -> httpStatus) (\s@DescribeApiDestinationResponse' {} a -> s {httpStatus = a} :: DescribeApiDestinationResponse)

instance
  Prelude.NFData
    DescribeApiDestinationResponse
  where
  rnf DescribeApiDestinationResponse' {..} =
    Prelude.rnf apiDestinationArn
      `Prelude.seq` Prelude.rnf apiDestinationState
      `Prelude.seq` Prelude.rnf connectionArn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf httpMethod
      `Prelude.seq` Prelude.rnf invocationEndpoint
      `Prelude.seq` Prelude.rnf invocationRateLimitPerSecond
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf httpStatus
