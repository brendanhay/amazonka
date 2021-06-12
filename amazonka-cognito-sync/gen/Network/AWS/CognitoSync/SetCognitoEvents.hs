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
-- Module      : Network.AWS.CognitoSync.SetCognitoEvents
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the AWS Lambda function for a given event type for an identity
-- pool. This request only updates the key\/value pair specified. Other
-- key\/values pairs are not updated. To remove a key value pair, pass a
-- empty value for the particular key.
--
-- This API can only be called with developer credentials. You cannot call
-- this API with the temporary user credentials provided by Cognito
-- Identity.
module Network.AWS.CognitoSync.SetCognitoEvents
  ( -- * Creating a Request
    SetCognitoEvents (..),
    newSetCognitoEvents,

    -- * Request Lenses
    setCognitoEvents_identityPoolId,
    setCognitoEvents_events,

    -- * Destructuring the Response
    SetCognitoEventsResponse (..),
    newSetCognitoEventsResponse,
  )
where

import Network.AWS.CognitoSync.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to configure Cognito Events\"
--
-- \"
--
-- /See:/ 'newSetCognitoEvents' smart constructor.
data SetCognitoEvents = SetCognitoEvents'
  { -- | The Cognito Identity Pool to use when configuring Cognito Events
    identityPoolId :: Core.Text,
    -- | The events to configure
    events :: Core.HashMap Core.Text Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SetCognitoEvents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityPoolId', 'setCognitoEvents_identityPoolId' - The Cognito Identity Pool to use when configuring Cognito Events
--
-- 'events', 'setCognitoEvents_events' - The events to configure
newSetCognitoEvents ::
  -- | 'identityPoolId'
  Core.Text ->
  SetCognitoEvents
newSetCognitoEvents pIdentityPoolId_ =
  SetCognitoEvents'
    { identityPoolId =
        pIdentityPoolId_,
      events = Core.mempty
    }

-- | The Cognito Identity Pool to use when configuring Cognito Events
setCognitoEvents_identityPoolId :: Lens.Lens' SetCognitoEvents Core.Text
setCognitoEvents_identityPoolId = Lens.lens (\SetCognitoEvents' {identityPoolId} -> identityPoolId) (\s@SetCognitoEvents' {} a -> s {identityPoolId = a} :: SetCognitoEvents)

-- | The events to configure
setCognitoEvents_events :: Lens.Lens' SetCognitoEvents (Core.HashMap Core.Text Core.Text)
setCognitoEvents_events = Lens.lens (\SetCognitoEvents' {events} -> events) (\s@SetCognitoEvents' {} a -> s {events = a} :: SetCognitoEvents) Core.. Lens._Coerce

instance Core.AWSRequest SetCognitoEvents where
  type
    AWSResponse SetCognitoEvents =
      SetCognitoEventsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull SetCognitoEventsResponse'

instance Core.Hashable SetCognitoEvents

instance Core.NFData SetCognitoEvents

instance Core.ToHeaders SetCognitoEvents where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON SetCognitoEvents where
  toJSON SetCognitoEvents' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("Events" Core..= events)]
      )

instance Core.ToPath SetCognitoEvents where
  toPath SetCognitoEvents' {..} =
    Core.mconcat
      [ "/identitypools/",
        Core.toBS identityPoolId,
        "/events"
      ]

instance Core.ToQuery SetCognitoEvents where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newSetCognitoEventsResponse' smart constructor.
data SetCognitoEventsResponse = SetCognitoEventsResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SetCognitoEventsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newSetCognitoEventsResponse ::
  SetCognitoEventsResponse
newSetCognitoEventsResponse =
  SetCognitoEventsResponse'

instance Core.NFData SetCognitoEventsResponse
