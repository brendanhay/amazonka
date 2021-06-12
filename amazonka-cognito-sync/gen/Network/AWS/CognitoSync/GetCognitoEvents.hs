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
-- Module      : Network.AWS.CognitoSync.GetCognitoEvents
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the events and the corresponding Lambda functions associated with
-- an identity pool.
--
-- This API can only be called with developer credentials. You cannot call
-- this API with the temporary user credentials provided by Cognito
-- Identity.
module Network.AWS.CognitoSync.GetCognitoEvents
  ( -- * Creating a Request
    GetCognitoEvents (..),
    newGetCognitoEvents,

    -- * Request Lenses
    getCognitoEvents_identityPoolId,

    -- * Destructuring the Response
    GetCognitoEventsResponse (..),
    newGetCognitoEventsResponse,

    -- * Response Lenses
    getCognitoEventsResponse_events,
    getCognitoEventsResponse_httpStatus,
  )
where

import Network.AWS.CognitoSync.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request for a list of the configured Cognito Events
--
-- /See:/ 'newGetCognitoEvents' smart constructor.
data GetCognitoEvents = GetCognitoEvents'
  { -- | The Cognito Identity Pool ID for the request
    identityPoolId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetCognitoEvents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityPoolId', 'getCognitoEvents_identityPoolId' - The Cognito Identity Pool ID for the request
newGetCognitoEvents ::
  -- | 'identityPoolId'
  Core.Text ->
  GetCognitoEvents
newGetCognitoEvents pIdentityPoolId_ =
  GetCognitoEvents'
    { identityPoolId =
        pIdentityPoolId_
    }

-- | The Cognito Identity Pool ID for the request
getCognitoEvents_identityPoolId :: Lens.Lens' GetCognitoEvents Core.Text
getCognitoEvents_identityPoolId = Lens.lens (\GetCognitoEvents' {identityPoolId} -> identityPoolId) (\s@GetCognitoEvents' {} a -> s {identityPoolId = a} :: GetCognitoEvents)

instance Core.AWSRequest GetCognitoEvents where
  type
    AWSResponse GetCognitoEvents =
      GetCognitoEventsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCognitoEventsResponse'
            Core.<$> (x Core..?> "Events" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetCognitoEvents

instance Core.NFData GetCognitoEvents

instance Core.ToHeaders GetCognitoEvents where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetCognitoEvents where
  toPath GetCognitoEvents' {..} =
    Core.mconcat
      [ "/identitypools/",
        Core.toBS identityPoolId,
        "/events"
      ]

instance Core.ToQuery GetCognitoEvents where
  toQuery = Core.const Core.mempty

-- | The response from the GetCognitoEvents request
--
-- /See:/ 'newGetCognitoEventsResponse' smart constructor.
data GetCognitoEventsResponse = GetCognitoEventsResponse'
  { -- | The Cognito Events returned from the GetCognitoEvents request
    events :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetCognitoEventsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'events', 'getCognitoEventsResponse_events' - The Cognito Events returned from the GetCognitoEvents request
--
-- 'httpStatus', 'getCognitoEventsResponse_httpStatus' - The response's http status code.
newGetCognitoEventsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetCognitoEventsResponse
newGetCognitoEventsResponse pHttpStatus_ =
  GetCognitoEventsResponse'
    { events = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Cognito Events returned from the GetCognitoEvents request
getCognitoEventsResponse_events :: Lens.Lens' GetCognitoEventsResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
getCognitoEventsResponse_events = Lens.lens (\GetCognitoEventsResponse' {events} -> events) (\s@GetCognitoEventsResponse' {} a -> s {events = a} :: GetCognitoEventsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getCognitoEventsResponse_httpStatus :: Lens.Lens' GetCognitoEventsResponse Core.Int
getCognitoEventsResponse_httpStatus = Lens.lens (\GetCognitoEventsResponse' {httpStatus} -> httpStatus) (\s@GetCognitoEventsResponse' {} a -> s {httpStatus = a} :: GetCognitoEventsResponse)

instance Core.NFData GetCognitoEventsResponse
