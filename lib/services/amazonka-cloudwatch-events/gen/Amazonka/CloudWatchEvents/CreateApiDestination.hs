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
-- Module      : Amazonka.CloudWatchEvents.CreateApiDestination
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an API destination, which is an HTTP invocation endpoint
-- configured as a target for events.
module Amazonka.CloudWatchEvents.CreateApiDestination
  ( -- * Creating a Request
    CreateApiDestination (..),
    newCreateApiDestination,

    -- * Request Lenses
    createApiDestination_invocationRateLimitPerSecond,
    createApiDestination_description,
    createApiDestination_name,
    createApiDestination_connectionArn,
    createApiDestination_invocationEndpoint,
    createApiDestination_httpMethod,

    -- * Destructuring the Response
    CreateApiDestinationResponse (..),
    newCreateApiDestinationResponse,

    -- * Response Lenses
    createApiDestinationResponse_lastModifiedTime,
    createApiDestinationResponse_apiDestinationState,
    createApiDestinationResponse_creationTime,
    createApiDestinationResponse_apiDestinationArn,
    createApiDestinationResponse_httpStatus,
  )
where

import Amazonka.CloudWatchEvents.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateApiDestination' smart constructor.
data CreateApiDestination = CreateApiDestination'
  { -- | The maximum number of requests per second to send to the HTTP invocation
    -- endpoint.
    invocationRateLimitPerSecond :: Prelude.Maybe Prelude.Natural,
    -- | A description for the API destination to create.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name for the API destination to create.
    name :: Prelude.Text,
    -- | The ARN of the connection to use for the API destination. The
    -- destination endpoint must support the authorization type specified for
    -- the connection.
    connectionArn :: Prelude.Text,
    -- | The URL to the HTTP invocation endpoint for the API destination.
    invocationEndpoint :: Prelude.Text,
    -- | The method to use for the request to the HTTP invocation endpoint.
    httpMethod :: ApiDestinationHttpMethod
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateApiDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'invocationRateLimitPerSecond', 'createApiDestination_invocationRateLimitPerSecond' - The maximum number of requests per second to send to the HTTP invocation
-- endpoint.
--
-- 'description', 'createApiDestination_description' - A description for the API destination to create.
--
-- 'name', 'createApiDestination_name' - The name for the API destination to create.
--
-- 'connectionArn', 'createApiDestination_connectionArn' - The ARN of the connection to use for the API destination. The
-- destination endpoint must support the authorization type specified for
-- the connection.
--
-- 'invocationEndpoint', 'createApiDestination_invocationEndpoint' - The URL to the HTTP invocation endpoint for the API destination.
--
-- 'httpMethod', 'createApiDestination_httpMethod' - The method to use for the request to the HTTP invocation endpoint.
newCreateApiDestination ::
  -- | 'name'
  Prelude.Text ->
  -- | 'connectionArn'
  Prelude.Text ->
  -- | 'invocationEndpoint'
  Prelude.Text ->
  -- | 'httpMethod'
  ApiDestinationHttpMethod ->
  CreateApiDestination
newCreateApiDestination
  pName_
  pConnectionArn_
  pInvocationEndpoint_
  pHttpMethod_ =
    CreateApiDestination'
      { invocationRateLimitPerSecond =
          Prelude.Nothing,
        description = Prelude.Nothing,
        name = pName_,
        connectionArn = pConnectionArn_,
        invocationEndpoint = pInvocationEndpoint_,
        httpMethod = pHttpMethod_
      }

-- | The maximum number of requests per second to send to the HTTP invocation
-- endpoint.
createApiDestination_invocationRateLimitPerSecond :: Lens.Lens' CreateApiDestination (Prelude.Maybe Prelude.Natural)
createApiDestination_invocationRateLimitPerSecond = Lens.lens (\CreateApiDestination' {invocationRateLimitPerSecond} -> invocationRateLimitPerSecond) (\s@CreateApiDestination' {} a -> s {invocationRateLimitPerSecond = a} :: CreateApiDestination)

-- | A description for the API destination to create.
createApiDestination_description :: Lens.Lens' CreateApiDestination (Prelude.Maybe Prelude.Text)
createApiDestination_description = Lens.lens (\CreateApiDestination' {description} -> description) (\s@CreateApiDestination' {} a -> s {description = a} :: CreateApiDestination)

-- | The name for the API destination to create.
createApiDestination_name :: Lens.Lens' CreateApiDestination Prelude.Text
createApiDestination_name = Lens.lens (\CreateApiDestination' {name} -> name) (\s@CreateApiDestination' {} a -> s {name = a} :: CreateApiDestination)

-- | The ARN of the connection to use for the API destination. The
-- destination endpoint must support the authorization type specified for
-- the connection.
createApiDestination_connectionArn :: Lens.Lens' CreateApiDestination Prelude.Text
createApiDestination_connectionArn = Lens.lens (\CreateApiDestination' {connectionArn} -> connectionArn) (\s@CreateApiDestination' {} a -> s {connectionArn = a} :: CreateApiDestination)

-- | The URL to the HTTP invocation endpoint for the API destination.
createApiDestination_invocationEndpoint :: Lens.Lens' CreateApiDestination Prelude.Text
createApiDestination_invocationEndpoint = Lens.lens (\CreateApiDestination' {invocationEndpoint} -> invocationEndpoint) (\s@CreateApiDestination' {} a -> s {invocationEndpoint = a} :: CreateApiDestination)

-- | The method to use for the request to the HTTP invocation endpoint.
createApiDestination_httpMethod :: Lens.Lens' CreateApiDestination ApiDestinationHttpMethod
createApiDestination_httpMethod = Lens.lens (\CreateApiDestination' {httpMethod} -> httpMethod) (\s@CreateApiDestination' {} a -> s {httpMethod = a} :: CreateApiDestination)

instance Core.AWSRequest CreateApiDestination where
  type
    AWSResponse CreateApiDestination =
      CreateApiDestinationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateApiDestinationResponse'
            Prelude.<$> (x Data..?> "LastModifiedTime")
            Prelude.<*> (x Data..?> "ApiDestinationState")
            Prelude.<*> (x Data..?> "CreationTime")
            Prelude.<*> (x Data..?> "ApiDestinationArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateApiDestination where
  hashWithSalt _salt CreateApiDestination' {..} =
    _salt
      `Prelude.hashWithSalt` invocationRateLimitPerSecond
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` connectionArn
      `Prelude.hashWithSalt` invocationEndpoint
      `Prelude.hashWithSalt` httpMethod

instance Prelude.NFData CreateApiDestination where
  rnf CreateApiDestination' {..} =
    Prelude.rnf invocationRateLimitPerSecond
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf connectionArn
      `Prelude.seq` Prelude.rnf invocationEndpoint
      `Prelude.seq` Prelude.rnf httpMethod

instance Data.ToHeaders CreateApiDestination where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSEvents.CreateApiDestination" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateApiDestination where
  toJSON CreateApiDestination' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("InvocationRateLimitPerSecond" Data..=)
              Prelude.<$> invocationRateLimitPerSecond,
            ("Description" Data..=) Prelude.<$> description,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("ConnectionArn" Data..= connectionArn),
            Prelude.Just
              ("InvocationEndpoint" Data..= invocationEndpoint),
            Prelude.Just ("HttpMethod" Data..= httpMethod)
          ]
      )

instance Data.ToPath CreateApiDestination where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateApiDestination where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateApiDestinationResponse' smart constructor.
data CreateApiDestinationResponse = CreateApiDestinationResponse'
  { -- | A time stamp indicating the time that the API destination was last
    -- modified.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The state of the API destination that was created by the request.
    apiDestinationState :: Prelude.Maybe ApiDestinationState,
    -- | A time stamp indicating the time that the API destination was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The ARN of the API destination that was created by the request.
    apiDestinationArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateApiDestinationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedTime', 'createApiDestinationResponse_lastModifiedTime' - A time stamp indicating the time that the API destination was last
-- modified.
--
-- 'apiDestinationState', 'createApiDestinationResponse_apiDestinationState' - The state of the API destination that was created by the request.
--
-- 'creationTime', 'createApiDestinationResponse_creationTime' - A time stamp indicating the time that the API destination was created.
--
-- 'apiDestinationArn', 'createApiDestinationResponse_apiDestinationArn' - The ARN of the API destination that was created by the request.
--
-- 'httpStatus', 'createApiDestinationResponse_httpStatus' - The response's http status code.
newCreateApiDestinationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateApiDestinationResponse
newCreateApiDestinationResponse pHttpStatus_ =
  CreateApiDestinationResponse'
    { lastModifiedTime =
        Prelude.Nothing,
      apiDestinationState = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      apiDestinationArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A time stamp indicating the time that the API destination was last
-- modified.
createApiDestinationResponse_lastModifiedTime :: Lens.Lens' CreateApiDestinationResponse (Prelude.Maybe Prelude.UTCTime)
createApiDestinationResponse_lastModifiedTime = Lens.lens (\CreateApiDestinationResponse' {lastModifiedTime} -> lastModifiedTime) (\s@CreateApiDestinationResponse' {} a -> s {lastModifiedTime = a} :: CreateApiDestinationResponse) Prelude.. Lens.mapping Data._Time

-- | The state of the API destination that was created by the request.
createApiDestinationResponse_apiDestinationState :: Lens.Lens' CreateApiDestinationResponse (Prelude.Maybe ApiDestinationState)
createApiDestinationResponse_apiDestinationState = Lens.lens (\CreateApiDestinationResponse' {apiDestinationState} -> apiDestinationState) (\s@CreateApiDestinationResponse' {} a -> s {apiDestinationState = a} :: CreateApiDestinationResponse)

-- | A time stamp indicating the time that the API destination was created.
createApiDestinationResponse_creationTime :: Lens.Lens' CreateApiDestinationResponse (Prelude.Maybe Prelude.UTCTime)
createApiDestinationResponse_creationTime = Lens.lens (\CreateApiDestinationResponse' {creationTime} -> creationTime) (\s@CreateApiDestinationResponse' {} a -> s {creationTime = a} :: CreateApiDestinationResponse) Prelude.. Lens.mapping Data._Time

-- | The ARN of the API destination that was created by the request.
createApiDestinationResponse_apiDestinationArn :: Lens.Lens' CreateApiDestinationResponse (Prelude.Maybe Prelude.Text)
createApiDestinationResponse_apiDestinationArn = Lens.lens (\CreateApiDestinationResponse' {apiDestinationArn} -> apiDestinationArn) (\s@CreateApiDestinationResponse' {} a -> s {apiDestinationArn = a} :: CreateApiDestinationResponse)

-- | The response's http status code.
createApiDestinationResponse_httpStatus :: Lens.Lens' CreateApiDestinationResponse Prelude.Int
createApiDestinationResponse_httpStatus = Lens.lens (\CreateApiDestinationResponse' {httpStatus} -> httpStatus) (\s@CreateApiDestinationResponse' {} a -> s {httpStatus = a} :: CreateApiDestinationResponse)

instance Prelude.NFData CreateApiDestinationResponse where
  rnf CreateApiDestinationResponse' {..} =
    Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf apiDestinationState
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf apiDestinationArn
      `Prelude.seq` Prelude.rnf httpStatus
