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
-- Module      : Network.AWS.CloudWatchEvents.UpdateApiDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an API destination.
module Network.AWS.CloudWatchEvents.UpdateApiDestination
  ( -- * Creating a Request
    UpdateApiDestination (..),
    newUpdateApiDestination,

    -- * Request Lenses
    updateApiDestination_httpMethod,
    updateApiDestination_invocationEndpoint,
    updateApiDestination_connectionArn,
    updateApiDestination_description,
    updateApiDestination_invocationRateLimitPerSecond,
    updateApiDestination_name,

    -- * Destructuring the Response
    UpdateApiDestinationResponse (..),
    newUpdateApiDestinationResponse,

    -- * Response Lenses
    updateApiDestinationResponse_creationTime,
    updateApiDestinationResponse_apiDestinationArn,
    updateApiDestinationResponse_apiDestinationState,
    updateApiDestinationResponse_lastModifiedTime,
    updateApiDestinationResponse_httpStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateApiDestination' smart constructor.
data UpdateApiDestination = UpdateApiDestination'
  { -- | The method to use for the API destination.
    httpMethod :: Core.Maybe ApiDestinationHttpMethod,
    -- | The URL to the endpoint to use for the API destination.
    invocationEndpoint :: Core.Maybe Core.Text,
    -- | The ARN of the connection to use for the API destination.
    connectionArn :: Core.Maybe Core.Text,
    -- | The name of the API destination to update.
    description :: Core.Maybe Core.Text,
    -- | The maximum number of invocations per second to send to the API
    -- destination.
    invocationRateLimitPerSecond :: Core.Maybe Core.Natural,
    -- | The name of the API destination to update.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateApiDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpMethod', 'updateApiDestination_httpMethod' - The method to use for the API destination.
--
-- 'invocationEndpoint', 'updateApiDestination_invocationEndpoint' - The URL to the endpoint to use for the API destination.
--
-- 'connectionArn', 'updateApiDestination_connectionArn' - The ARN of the connection to use for the API destination.
--
-- 'description', 'updateApiDestination_description' - The name of the API destination to update.
--
-- 'invocationRateLimitPerSecond', 'updateApiDestination_invocationRateLimitPerSecond' - The maximum number of invocations per second to send to the API
-- destination.
--
-- 'name', 'updateApiDestination_name' - The name of the API destination to update.
newUpdateApiDestination ::
  -- | 'name'
  Core.Text ->
  UpdateApiDestination
newUpdateApiDestination pName_ =
  UpdateApiDestination'
    { httpMethod = Core.Nothing,
      invocationEndpoint = Core.Nothing,
      connectionArn = Core.Nothing,
      description = Core.Nothing,
      invocationRateLimitPerSecond = Core.Nothing,
      name = pName_
    }

-- | The method to use for the API destination.
updateApiDestination_httpMethod :: Lens.Lens' UpdateApiDestination (Core.Maybe ApiDestinationHttpMethod)
updateApiDestination_httpMethod = Lens.lens (\UpdateApiDestination' {httpMethod} -> httpMethod) (\s@UpdateApiDestination' {} a -> s {httpMethod = a} :: UpdateApiDestination)

-- | The URL to the endpoint to use for the API destination.
updateApiDestination_invocationEndpoint :: Lens.Lens' UpdateApiDestination (Core.Maybe Core.Text)
updateApiDestination_invocationEndpoint = Lens.lens (\UpdateApiDestination' {invocationEndpoint} -> invocationEndpoint) (\s@UpdateApiDestination' {} a -> s {invocationEndpoint = a} :: UpdateApiDestination)

-- | The ARN of the connection to use for the API destination.
updateApiDestination_connectionArn :: Lens.Lens' UpdateApiDestination (Core.Maybe Core.Text)
updateApiDestination_connectionArn = Lens.lens (\UpdateApiDestination' {connectionArn} -> connectionArn) (\s@UpdateApiDestination' {} a -> s {connectionArn = a} :: UpdateApiDestination)

-- | The name of the API destination to update.
updateApiDestination_description :: Lens.Lens' UpdateApiDestination (Core.Maybe Core.Text)
updateApiDestination_description = Lens.lens (\UpdateApiDestination' {description} -> description) (\s@UpdateApiDestination' {} a -> s {description = a} :: UpdateApiDestination)

-- | The maximum number of invocations per second to send to the API
-- destination.
updateApiDestination_invocationRateLimitPerSecond :: Lens.Lens' UpdateApiDestination (Core.Maybe Core.Natural)
updateApiDestination_invocationRateLimitPerSecond = Lens.lens (\UpdateApiDestination' {invocationRateLimitPerSecond} -> invocationRateLimitPerSecond) (\s@UpdateApiDestination' {} a -> s {invocationRateLimitPerSecond = a} :: UpdateApiDestination)

-- | The name of the API destination to update.
updateApiDestination_name :: Lens.Lens' UpdateApiDestination Core.Text
updateApiDestination_name = Lens.lens (\UpdateApiDestination' {name} -> name) (\s@UpdateApiDestination' {} a -> s {name = a} :: UpdateApiDestination)

instance Core.AWSRequest UpdateApiDestination where
  type
    AWSResponse UpdateApiDestination =
      UpdateApiDestinationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateApiDestinationResponse'
            Core.<$> (x Core..?> "CreationTime")
            Core.<*> (x Core..?> "ApiDestinationArn")
            Core.<*> (x Core..?> "ApiDestinationState")
            Core.<*> (x Core..?> "LastModifiedTime")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateApiDestination

instance Core.NFData UpdateApiDestination

instance Core.ToHeaders UpdateApiDestination where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSEvents.UpdateApiDestination" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateApiDestination where
  toJSON UpdateApiDestination' {..} =
    Core.object
      ( Core.catMaybes
          [ ("HttpMethod" Core..=) Core.<$> httpMethod,
            ("InvocationEndpoint" Core..=)
              Core.<$> invocationEndpoint,
            ("ConnectionArn" Core..=) Core.<$> connectionArn,
            ("Description" Core..=) Core.<$> description,
            ("InvocationRateLimitPerSecond" Core..=)
              Core.<$> invocationRateLimitPerSecond,
            Core.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath UpdateApiDestination where
  toPath = Core.const "/"

instance Core.ToQuery UpdateApiDestination where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateApiDestinationResponse' smart constructor.
data UpdateApiDestinationResponse = UpdateApiDestinationResponse'
  { -- | A time stamp for the time that the API destination was created.
    creationTime :: Core.Maybe Core.POSIX,
    -- | The ARN of the API destination that was updated.
    apiDestinationArn :: Core.Maybe Core.Text,
    -- | The state of the API destination that was updated.
    apiDestinationState :: Core.Maybe ApiDestinationState,
    -- | A time stamp for the time that the API destination was last modified.
    lastModifiedTime :: Core.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateApiDestinationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'updateApiDestinationResponse_creationTime' - A time stamp for the time that the API destination was created.
--
-- 'apiDestinationArn', 'updateApiDestinationResponse_apiDestinationArn' - The ARN of the API destination that was updated.
--
-- 'apiDestinationState', 'updateApiDestinationResponse_apiDestinationState' - The state of the API destination that was updated.
--
-- 'lastModifiedTime', 'updateApiDestinationResponse_lastModifiedTime' - A time stamp for the time that the API destination was last modified.
--
-- 'httpStatus', 'updateApiDestinationResponse_httpStatus' - The response's http status code.
newUpdateApiDestinationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateApiDestinationResponse
newUpdateApiDestinationResponse pHttpStatus_ =
  UpdateApiDestinationResponse'
    { creationTime =
        Core.Nothing,
      apiDestinationArn = Core.Nothing,
      apiDestinationState = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A time stamp for the time that the API destination was created.
updateApiDestinationResponse_creationTime :: Lens.Lens' UpdateApiDestinationResponse (Core.Maybe Core.UTCTime)
updateApiDestinationResponse_creationTime = Lens.lens (\UpdateApiDestinationResponse' {creationTime} -> creationTime) (\s@UpdateApiDestinationResponse' {} a -> s {creationTime = a} :: UpdateApiDestinationResponse) Core.. Lens.mapping Core._Time

-- | The ARN of the API destination that was updated.
updateApiDestinationResponse_apiDestinationArn :: Lens.Lens' UpdateApiDestinationResponse (Core.Maybe Core.Text)
updateApiDestinationResponse_apiDestinationArn = Lens.lens (\UpdateApiDestinationResponse' {apiDestinationArn} -> apiDestinationArn) (\s@UpdateApiDestinationResponse' {} a -> s {apiDestinationArn = a} :: UpdateApiDestinationResponse)

-- | The state of the API destination that was updated.
updateApiDestinationResponse_apiDestinationState :: Lens.Lens' UpdateApiDestinationResponse (Core.Maybe ApiDestinationState)
updateApiDestinationResponse_apiDestinationState = Lens.lens (\UpdateApiDestinationResponse' {apiDestinationState} -> apiDestinationState) (\s@UpdateApiDestinationResponse' {} a -> s {apiDestinationState = a} :: UpdateApiDestinationResponse)

-- | A time stamp for the time that the API destination was last modified.
updateApiDestinationResponse_lastModifiedTime :: Lens.Lens' UpdateApiDestinationResponse (Core.Maybe Core.UTCTime)
updateApiDestinationResponse_lastModifiedTime = Lens.lens (\UpdateApiDestinationResponse' {lastModifiedTime} -> lastModifiedTime) (\s@UpdateApiDestinationResponse' {} a -> s {lastModifiedTime = a} :: UpdateApiDestinationResponse) Core.. Lens.mapping Core._Time

-- | The response's http status code.
updateApiDestinationResponse_httpStatus :: Lens.Lens' UpdateApiDestinationResponse Core.Int
updateApiDestinationResponse_httpStatus = Lens.lens (\UpdateApiDestinationResponse' {httpStatus} -> httpStatus) (\s@UpdateApiDestinationResponse' {} a -> s {httpStatus = a} :: UpdateApiDestinationResponse)

instance Core.NFData UpdateApiDestinationResponse
