{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.SetCognitoEvents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the AWS Lambda function for a given event type for an identity pool. This request only updates the key/value pair specified. Other key/values pairs are not updated. To remove a key value pair, pass a empty value for the particular key.
--
-- This API can only be called with developer credentials. You cannot call this API with the temporary user credentials provided by Cognito Identity.
module Network.AWS.CognitoSync.SetCognitoEvents
  ( -- * Creating a request
    SetCognitoEvents (..),
    mkSetCognitoEvents,

    -- ** Request lenses
    sceIdentityPoolId,
    sceEvents,

    -- * Destructuring the response
    SetCognitoEventsResponse (..),
    mkSetCognitoEventsResponse,
  )
where

import Network.AWS.CognitoSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | A request to configure Cognito Events"
--
-- /See:/ 'mkSetCognitoEvents' smart constructor.
data SetCognitoEvents = SetCognitoEvents'
  { identityPoolId ::
      Lude.Text,
    events :: Lude.HashMap Lude.Text (Lude.Text)
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetCognitoEvents' with the minimum fields required to make a request.
--
-- * 'events' - The events to configure
-- * 'identityPoolId' - The Cognito Identity Pool to use when configuring Cognito Events
mkSetCognitoEvents ::
  -- | 'identityPoolId'
  Lude.Text ->
  SetCognitoEvents
mkSetCognitoEvents pIdentityPoolId_ =
  SetCognitoEvents'
    { identityPoolId = pIdentityPoolId_,
      events = Lude.mempty
    }

-- | The Cognito Identity Pool to use when configuring Cognito Events
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sceIdentityPoolId :: Lens.Lens' SetCognitoEvents Lude.Text
sceIdentityPoolId = Lens.lens (identityPoolId :: SetCognitoEvents -> Lude.Text) (\s a -> s {identityPoolId = a} :: SetCognitoEvents)
{-# DEPRECATED sceIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

-- | The events to configure
--
-- /Note:/ Consider using 'events' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sceEvents :: Lens.Lens' SetCognitoEvents (Lude.HashMap Lude.Text (Lude.Text))
sceEvents = Lens.lens (events :: SetCognitoEvents -> Lude.HashMap Lude.Text (Lude.Text)) (\s a -> s {events = a} :: SetCognitoEvents)
{-# DEPRECATED sceEvents "Use generic-lens or generic-optics with 'events' instead." #-}

instance Lude.AWSRequest SetCognitoEvents where
  type Rs SetCognitoEvents = SetCognitoEventsResponse
  request = Req.postJSON cognitoSyncService
  response = Res.receiveNull SetCognitoEventsResponse'

instance Lude.ToHeaders SetCognitoEvents where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON SetCognitoEvents where
  toJSON SetCognitoEvents' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("Events" Lude..= events)])

instance Lude.ToPath SetCognitoEvents where
  toPath SetCognitoEvents' {..} =
    Lude.mconcat
      ["/identitypools/", Lude.toBS identityPoolId, "/events"]

instance Lude.ToQuery SetCognitoEvents where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSetCognitoEventsResponse' smart constructor.
data SetCognitoEventsResponse = SetCognitoEventsResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetCognitoEventsResponse' with the minimum fields required to make a request.
mkSetCognitoEventsResponse ::
  SetCognitoEventsResponse
mkSetCognitoEventsResponse = SetCognitoEventsResponse'
