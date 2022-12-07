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
-- Module      : Amazonka.CloudWatchEvents.UpdateApiDestination
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an API destination.
module Amazonka.CloudWatchEvents.UpdateApiDestination
  ( -- * Creating a Request
    UpdateApiDestination (..),
    newUpdateApiDestination,

    -- * Request Lenses
    updateApiDestination_invocationRateLimitPerSecond,
    updateApiDestination_invocationEndpoint,
    updateApiDestination_description,
    updateApiDestination_connectionArn,
    updateApiDestination_httpMethod,
    updateApiDestination_name,

    -- * Destructuring the Response
    UpdateApiDestinationResponse (..),
    newUpdateApiDestinationResponse,

    -- * Response Lenses
    updateApiDestinationResponse_lastModifiedTime,
    updateApiDestinationResponse_apiDestinationState,
    updateApiDestinationResponse_creationTime,
    updateApiDestinationResponse_apiDestinationArn,
    updateApiDestinationResponse_httpStatus,
  )
where

import Amazonka.CloudWatchEvents.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateApiDestination' smart constructor.
data UpdateApiDestination = UpdateApiDestination'
  { -- | The maximum number of invocations per second to send to the API
    -- destination.
    invocationRateLimitPerSecond :: Prelude.Maybe Prelude.Natural,
    -- | The URL to the endpoint to use for the API destination.
    invocationEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The name of the API destination to update.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the connection to use for the API destination.
    connectionArn :: Prelude.Maybe Prelude.Text,
    -- | The method to use for the API destination.
    httpMethod :: Prelude.Maybe ApiDestinationHttpMethod,
    -- | The name of the API destination to update.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateApiDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'invocationRateLimitPerSecond', 'updateApiDestination_invocationRateLimitPerSecond' - The maximum number of invocations per second to send to the API
-- destination.
--
-- 'invocationEndpoint', 'updateApiDestination_invocationEndpoint' - The URL to the endpoint to use for the API destination.
--
-- 'description', 'updateApiDestination_description' - The name of the API destination to update.
--
-- 'connectionArn', 'updateApiDestination_connectionArn' - The ARN of the connection to use for the API destination.
--
-- 'httpMethod', 'updateApiDestination_httpMethod' - The method to use for the API destination.
--
-- 'name', 'updateApiDestination_name' - The name of the API destination to update.
newUpdateApiDestination ::
  -- | 'name'
  Prelude.Text ->
  UpdateApiDestination
newUpdateApiDestination pName_ =
  UpdateApiDestination'
    { invocationRateLimitPerSecond =
        Prelude.Nothing,
      invocationEndpoint = Prelude.Nothing,
      description = Prelude.Nothing,
      connectionArn = Prelude.Nothing,
      httpMethod = Prelude.Nothing,
      name = pName_
    }

-- | The maximum number of invocations per second to send to the API
-- destination.
updateApiDestination_invocationRateLimitPerSecond :: Lens.Lens' UpdateApiDestination (Prelude.Maybe Prelude.Natural)
updateApiDestination_invocationRateLimitPerSecond = Lens.lens (\UpdateApiDestination' {invocationRateLimitPerSecond} -> invocationRateLimitPerSecond) (\s@UpdateApiDestination' {} a -> s {invocationRateLimitPerSecond = a} :: UpdateApiDestination)

-- | The URL to the endpoint to use for the API destination.
updateApiDestination_invocationEndpoint :: Lens.Lens' UpdateApiDestination (Prelude.Maybe Prelude.Text)
updateApiDestination_invocationEndpoint = Lens.lens (\UpdateApiDestination' {invocationEndpoint} -> invocationEndpoint) (\s@UpdateApiDestination' {} a -> s {invocationEndpoint = a} :: UpdateApiDestination)

-- | The name of the API destination to update.
updateApiDestination_description :: Lens.Lens' UpdateApiDestination (Prelude.Maybe Prelude.Text)
updateApiDestination_description = Lens.lens (\UpdateApiDestination' {description} -> description) (\s@UpdateApiDestination' {} a -> s {description = a} :: UpdateApiDestination)

-- | The ARN of the connection to use for the API destination.
updateApiDestination_connectionArn :: Lens.Lens' UpdateApiDestination (Prelude.Maybe Prelude.Text)
updateApiDestination_connectionArn = Lens.lens (\UpdateApiDestination' {connectionArn} -> connectionArn) (\s@UpdateApiDestination' {} a -> s {connectionArn = a} :: UpdateApiDestination)

-- | The method to use for the API destination.
updateApiDestination_httpMethod :: Lens.Lens' UpdateApiDestination (Prelude.Maybe ApiDestinationHttpMethod)
updateApiDestination_httpMethod = Lens.lens (\UpdateApiDestination' {httpMethod} -> httpMethod) (\s@UpdateApiDestination' {} a -> s {httpMethod = a} :: UpdateApiDestination)

-- | The name of the API destination to update.
updateApiDestination_name :: Lens.Lens' UpdateApiDestination Prelude.Text
updateApiDestination_name = Lens.lens (\UpdateApiDestination' {name} -> name) (\s@UpdateApiDestination' {} a -> s {name = a} :: UpdateApiDestination)

instance Core.AWSRequest UpdateApiDestination where
  type
    AWSResponse UpdateApiDestination =
      UpdateApiDestinationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateApiDestinationResponse'
            Prelude.<$> (x Data..?> "LastModifiedTime")
            Prelude.<*> (x Data..?> "ApiDestinationState")
            Prelude.<*> (x Data..?> "CreationTime")
            Prelude.<*> (x Data..?> "ApiDestinationArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateApiDestination where
  hashWithSalt _salt UpdateApiDestination' {..} =
    _salt
      `Prelude.hashWithSalt` invocationRateLimitPerSecond
      `Prelude.hashWithSalt` invocationEndpoint
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` connectionArn
      `Prelude.hashWithSalt` httpMethod
      `Prelude.hashWithSalt` name

instance Prelude.NFData UpdateApiDestination where
  rnf UpdateApiDestination' {..} =
    Prelude.rnf invocationRateLimitPerSecond
      `Prelude.seq` Prelude.rnf invocationEndpoint
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf connectionArn
      `Prelude.seq` Prelude.rnf httpMethod
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders UpdateApiDestination where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSEvents.UpdateApiDestination" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateApiDestination where
  toJSON UpdateApiDestination' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("InvocationRateLimitPerSecond" Data..=)
              Prelude.<$> invocationRateLimitPerSecond,
            ("InvocationEndpoint" Data..=)
              Prelude.<$> invocationEndpoint,
            ("Description" Data..=) Prelude.<$> description,
            ("ConnectionArn" Data..=) Prelude.<$> connectionArn,
            ("HttpMethod" Data..=) Prelude.<$> httpMethod,
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath UpdateApiDestination where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateApiDestination where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateApiDestinationResponse' smart constructor.
data UpdateApiDestinationResponse = UpdateApiDestinationResponse'
  { -- | A time stamp for the time that the API destination was last modified.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The state of the API destination that was updated.
    apiDestinationState :: Prelude.Maybe ApiDestinationState,
    -- | A time stamp for the time that the API destination was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The ARN of the API destination that was updated.
    apiDestinationArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateApiDestinationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedTime', 'updateApiDestinationResponse_lastModifiedTime' - A time stamp for the time that the API destination was last modified.
--
-- 'apiDestinationState', 'updateApiDestinationResponse_apiDestinationState' - The state of the API destination that was updated.
--
-- 'creationTime', 'updateApiDestinationResponse_creationTime' - A time stamp for the time that the API destination was created.
--
-- 'apiDestinationArn', 'updateApiDestinationResponse_apiDestinationArn' - The ARN of the API destination that was updated.
--
-- 'httpStatus', 'updateApiDestinationResponse_httpStatus' - The response's http status code.
newUpdateApiDestinationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateApiDestinationResponse
newUpdateApiDestinationResponse pHttpStatus_ =
  UpdateApiDestinationResponse'
    { lastModifiedTime =
        Prelude.Nothing,
      apiDestinationState = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      apiDestinationArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A time stamp for the time that the API destination was last modified.
updateApiDestinationResponse_lastModifiedTime :: Lens.Lens' UpdateApiDestinationResponse (Prelude.Maybe Prelude.UTCTime)
updateApiDestinationResponse_lastModifiedTime = Lens.lens (\UpdateApiDestinationResponse' {lastModifiedTime} -> lastModifiedTime) (\s@UpdateApiDestinationResponse' {} a -> s {lastModifiedTime = a} :: UpdateApiDestinationResponse) Prelude.. Lens.mapping Data._Time

-- | The state of the API destination that was updated.
updateApiDestinationResponse_apiDestinationState :: Lens.Lens' UpdateApiDestinationResponse (Prelude.Maybe ApiDestinationState)
updateApiDestinationResponse_apiDestinationState = Lens.lens (\UpdateApiDestinationResponse' {apiDestinationState} -> apiDestinationState) (\s@UpdateApiDestinationResponse' {} a -> s {apiDestinationState = a} :: UpdateApiDestinationResponse)

-- | A time stamp for the time that the API destination was created.
updateApiDestinationResponse_creationTime :: Lens.Lens' UpdateApiDestinationResponse (Prelude.Maybe Prelude.UTCTime)
updateApiDestinationResponse_creationTime = Lens.lens (\UpdateApiDestinationResponse' {creationTime} -> creationTime) (\s@UpdateApiDestinationResponse' {} a -> s {creationTime = a} :: UpdateApiDestinationResponse) Prelude.. Lens.mapping Data._Time

-- | The ARN of the API destination that was updated.
updateApiDestinationResponse_apiDestinationArn :: Lens.Lens' UpdateApiDestinationResponse (Prelude.Maybe Prelude.Text)
updateApiDestinationResponse_apiDestinationArn = Lens.lens (\UpdateApiDestinationResponse' {apiDestinationArn} -> apiDestinationArn) (\s@UpdateApiDestinationResponse' {} a -> s {apiDestinationArn = a} :: UpdateApiDestinationResponse)

-- | The response's http status code.
updateApiDestinationResponse_httpStatus :: Lens.Lens' UpdateApiDestinationResponse Prelude.Int
updateApiDestinationResponse_httpStatus = Lens.lens (\UpdateApiDestinationResponse' {httpStatus} -> httpStatus) (\s@UpdateApiDestinationResponse' {} a -> s {httpStatus = a} :: UpdateApiDestinationResponse)

instance Prelude.NFData UpdateApiDestinationResponse where
  rnf UpdateApiDestinationResponse' {..} =
    Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf apiDestinationState
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf apiDestinationArn
      `Prelude.seq` Prelude.rnf httpStatus
