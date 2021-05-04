{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request for a list of the configured Cognito Events
--
-- /See:/ 'newGetCognitoEvents' smart constructor.
data GetCognitoEvents = GetCognitoEvents'
  { -- | The Cognito Identity Pool ID for the request
    identityPoolId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  GetCognitoEvents
newGetCognitoEvents pIdentityPoolId_ =
  GetCognitoEvents'
    { identityPoolId =
        pIdentityPoolId_
    }

-- | The Cognito Identity Pool ID for the request
getCognitoEvents_identityPoolId :: Lens.Lens' GetCognitoEvents Prelude.Text
getCognitoEvents_identityPoolId = Lens.lens (\GetCognitoEvents' {identityPoolId} -> identityPoolId) (\s@GetCognitoEvents' {} a -> s {identityPoolId = a} :: GetCognitoEvents)

instance Prelude.AWSRequest GetCognitoEvents where
  type Rs GetCognitoEvents = GetCognitoEventsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCognitoEventsResponse'
            Prelude.<$> (x Prelude..?> "Events" Prelude..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCognitoEvents

instance Prelude.NFData GetCognitoEvents

instance Prelude.ToHeaders GetCognitoEvents where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath GetCognitoEvents where
  toPath GetCognitoEvents' {..} =
    Prelude.mconcat
      [ "/identitypools/",
        Prelude.toBS identityPoolId,
        "/events"
      ]

instance Prelude.ToQuery GetCognitoEvents where
  toQuery = Prelude.const Prelude.mempty

-- | The response from the GetCognitoEvents request
--
-- /See:/ 'newGetCognitoEventsResponse' smart constructor.
data GetCognitoEventsResponse = GetCognitoEventsResponse'
  { -- | The Cognito Events returned from the GetCognitoEvents request
    events :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  GetCognitoEventsResponse
newGetCognitoEventsResponse pHttpStatus_ =
  GetCognitoEventsResponse'
    { events = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Cognito Events returned from the GetCognitoEvents request
getCognitoEventsResponse_events :: Lens.Lens' GetCognitoEventsResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getCognitoEventsResponse_events = Lens.lens (\GetCognitoEventsResponse' {events} -> events) (\s@GetCognitoEventsResponse' {} a -> s {events = a} :: GetCognitoEventsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
getCognitoEventsResponse_httpStatus :: Lens.Lens' GetCognitoEventsResponse Prelude.Int
getCognitoEventsResponse_httpStatus = Lens.lens (\GetCognitoEventsResponse' {httpStatus} -> httpStatus) (\s@GetCognitoEventsResponse' {} a -> s {httpStatus = a} :: GetCognitoEventsResponse)

instance Prelude.NFData GetCognitoEventsResponse
