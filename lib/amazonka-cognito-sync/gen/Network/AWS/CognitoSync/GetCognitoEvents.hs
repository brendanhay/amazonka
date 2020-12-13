{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.GetCognitoEvents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the events and the corresponding Lambda functions associated with an identity pool.
--
-- This API can only be called with developer credentials. You cannot call this API with the temporary user credentials provided by Cognito Identity.
module Network.AWS.CognitoSync.GetCognitoEvents
  ( -- * Creating a request
    GetCognitoEvents (..),
    mkGetCognitoEvents,

    -- ** Request lenses
    gceIdentityPoolId,

    -- * Destructuring the response
    GetCognitoEventsResponse (..),
    mkGetCognitoEventsResponse,

    -- ** Response lenses
    gcersEvents,
    gcersResponseStatus,
  )
where

import Network.AWS.CognitoSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | A request for a list of the configured Cognito Events
--
-- /See:/ 'mkGetCognitoEvents' smart constructor.
newtype GetCognitoEvents = GetCognitoEvents'
  { -- | The Cognito Identity Pool ID for the request
    identityPoolId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCognitoEvents' with the minimum fields required to make a request.
--
-- * 'identityPoolId' - The Cognito Identity Pool ID for the request
mkGetCognitoEvents ::
  -- | 'identityPoolId'
  Lude.Text ->
  GetCognitoEvents
mkGetCognitoEvents pIdentityPoolId_ =
  GetCognitoEvents' {identityPoolId = pIdentityPoolId_}

-- | The Cognito Identity Pool ID for the request
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gceIdentityPoolId :: Lens.Lens' GetCognitoEvents Lude.Text
gceIdentityPoolId = Lens.lens (identityPoolId :: GetCognitoEvents -> Lude.Text) (\s a -> s {identityPoolId = a} :: GetCognitoEvents)
{-# DEPRECATED gceIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

instance Lude.AWSRequest GetCognitoEvents where
  type Rs GetCognitoEvents = GetCognitoEventsResponse
  request = Req.get cognitoSyncService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetCognitoEventsResponse'
            Lude.<$> (x Lude..?> "Events" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetCognitoEvents where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetCognitoEvents where
  toPath GetCognitoEvents' {..} =
    Lude.mconcat
      ["/identitypools/", Lude.toBS identityPoolId, "/events"]

instance Lude.ToQuery GetCognitoEvents where
  toQuery = Lude.const Lude.mempty

-- | The response from the GetCognitoEvents request
--
-- /See:/ 'mkGetCognitoEventsResponse' smart constructor.
data GetCognitoEventsResponse = GetCognitoEventsResponse'
  { -- | The Cognito Events returned from the GetCognitoEvents request
    events :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCognitoEventsResponse' with the minimum fields required to make a request.
--
-- * 'events' - The Cognito Events returned from the GetCognitoEvents request
-- * 'responseStatus' - The response status code.
mkGetCognitoEventsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetCognitoEventsResponse
mkGetCognitoEventsResponse pResponseStatus_ =
  GetCognitoEventsResponse'
    { events = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Cognito Events returned from the GetCognitoEvents request
--
-- /Note:/ Consider using 'events' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcersEvents :: Lens.Lens' GetCognitoEventsResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
gcersEvents = Lens.lens (events :: GetCognitoEventsResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {events = a} :: GetCognitoEventsResponse)
{-# DEPRECATED gcersEvents "Use generic-lens or generic-optics with 'events' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcersResponseStatus :: Lens.Lens' GetCognitoEventsResponse Lude.Int
gcersResponseStatus = Lens.lens (responseStatus :: GetCognitoEventsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetCognitoEventsResponse)
{-# DEPRECATED gcersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
