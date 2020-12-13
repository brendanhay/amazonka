{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.GetFederationToken
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a token for federation.
module Network.AWS.Connect.GetFederationToken
  ( -- * Creating a request
    GetFederationToken (..),
    mkGetFederationToken,

    -- ** Request lenses
    gftInstanceId,

    -- * Destructuring the response
    GetFederationTokenResponse (..),
    mkGetFederationTokenResponse,

    -- ** Response lenses
    gftrsCredentials,
    gftrsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetFederationToken' smart constructor.
newtype GetFederationToken = GetFederationToken'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetFederationToken' with the minimum fields required to make a request.
--
-- * 'instanceId' - The identifier of the Amazon Connect instance.
mkGetFederationToken ::
  -- | 'instanceId'
  Lude.Text ->
  GetFederationToken
mkGetFederationToken pInstanceId_ =
  GetFederationToken' {instanceId = pInstanceId_}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gftInstanceId :: Lens.Lens' GetFederationToken Lude.Text
gftInstanceId = Lens.lens (instanceId :: GetFederationToken -> Lude.Text) (\s a -> s {instanceId = a} :: GetFederationToken)
{-# DEPRECATED gftInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

instance Lude.AWSRequest GetFederationToken where
  type Rs GetFederationToken = GetFederationTokenResponse
  request = Req.get connectService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetFederationTokenResponse'
            Lude.<$> (x Lude..?> "Credentials") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetFederationToken where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetFederationToken where
  toPath GetFederationToken' {..} =
    Lude.mconcat ["/user/federate/", Lude.toBS instanceId]

instance Lude.ToQuery GetFederationToken where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetFederationTokenResponse' smart constructor.
data GetFederationTokenResponse = GetFederationTokenResponse'
  { -- | The credentials to use for federation.
    credentials :: Lude.Maybe Credentials,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetFederationTokenResponse' with the minimum fields required to make a request.
--
-- * 'credentials' - The credentials to use for federation.
-- * 'responseStatus' - The response status code.
mkGetFederationTokenResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetFederationTokenResponse
mkGetFederationTokenResponse pResponseStatus_ =
  GetFederationTokenResponse'
    { credentials = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The credentials to use for federation.
--
-- /Note:/ Consider using 'credentials' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gftrsCredentials :: Lens.Lens' GetFederationTokenResponse (Lude.Maybe Credentials)
gftrsCredentials = Lens.lens (credentials :: GetFederationTokenResponse -> Lude.Maybe Credentials) (\s a -> s {credentials = a} :: GetFederationTokenResponse)
{-# DEPRECATED gftrsCredentials "Use generic-lens or generic-optics with 'credentials' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gftrsResponseStatus :: Lens.Lens' GetFederationTokenResponse Lude.Int
gftrsResponseStatus = Lens.lens (responseStatus :: GetFederationTokenResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetFederationTokenResponse)
{-# DEPRECATED gftrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
