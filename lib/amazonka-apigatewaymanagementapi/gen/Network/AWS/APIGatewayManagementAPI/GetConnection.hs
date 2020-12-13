{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGatewayManagementAPI.GetConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get information about the connection with the provided id.
module Network.AWS.APIGatewayManagementAPI.GetConnection
  ( -- * Creating a request
    GetConnection (..),
    mkGetConnection,

    -- ** Request lenses
    gcConnectionId,

    -- * Destructuring the response
    GetConnectionResponse (..),
    mkGetConnectionResponse,

    -- ** Response lenses
    gcrsConnectedAt,
    gcrsLastActiveAt,
    gcrsIdentity,
    gcrsResponseStatus,
  )
where

import Network.AWS.APIGatewayManagementAPI.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetConnection' smart constructor.
newtype GetConnection = GetConnection'
  { connectionId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetConnection' with the minimum fields required to make a request.
--
-- * 'connectionId' -
mkGetConnection ::
  -- | 'connectionId'
  Lude.Text ->
  GetConnection
mkGetConnection pConnectionId_ =
  GetConnection' {connectionId = pConnectionId_}

-- | Undocumented field.
--
-- /Note:/ Consider using 'connectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcConnectionId :: Lens.Lens' GetConnection Lude.Text
gcConnectionId = Lens.lens (connectionId :: GetConnection -> Lude.Text) (\s a -> s {connectionId = a} :: GetConnection)
{-# DEPRECATED gcConnectionId "Use generic-lens or generic-optics with 'connectionId' instead." #-}

instance Lude.AWSRequest GetConnection where
  type Rs GetConnection = GetConnectionResponse
  request = Req.get apiGatewayManagementAPIService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetConnectionResponse'
            Lude.<$> (x Lude..?> "connectedAt")
            Lude.<*> (x Lude..?> "lastActiveAt")
            Lude.<*> (x Lude..?> "identity")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetConnection where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetConnection where
  toPath GetConnection' {..} =
    Lude.mconcat ["/@connections/", Lude.toBS connectionId]

instance Lude.ToQuery GetConnection where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetConnectionResponse' smart constructor.
data GetConnectionResponse = GetConnectionResponse'
  { -- | The time in ISO 8601 format for when the connection was established.
    connectedAt :: Lude.Maybe Lude.Timestamp,
    -- | The time in ISO 8601 format for when the connection was last active.
    lastActiveAt :: Lude.Maybe Lude.Timestamp,
    identity :: Lude.Maybe Identity,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetConnectionResponse' with the minimum fields required to make a request.
--
-- * 'connectedAt' - The time in ISO 8601 format for when the connection was established.
-- * 'lastActiveAt' - The time in ISO 8601 format for when the connection was last active.
-- * 'identity' -
-- * 'responseStatus' - The response status code.
mkGetConnectionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetConnectionResponse
mkGetConnectionResponse pResponseStatus_ =
  GetConnectionResponse'
    { connectedAt = Lude.Nothing,
      lastActiveAt = Lude.Nothing,
      identity = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The time in ISO 8601 format for when the connection was established.
--
-- /Note:/ Consider using 'connectedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrsConnectedAt :: Lens.Lens' GetConnectionResponse (Lude.Maybe Lude.Timestamp)
gcrsConnectedAt = Lens.lens (connectedAt :: GetConnectionResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {connectedAt = a} :: GetConnectionResponse)
{-# DEPRECATED gcrsConnectedAt "Use generic-lens or generic-optics with 'connectedAt' instead." #-}

-- | The time in ISO 8601 format for when the connection was last active.
--
-- /Note:/ Consider using 'lastActiveAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrsLastActiveAt :: Lens.Lens' GetConnectionResponse (Lude.Maybe Lude.Timestamp)
gcrsLastActiveAt = Lens.lens (lastActiveAt :: GetConnectionResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastActiveAt = a} :: GetConnectionResponse)
{-# DEPRECATED gcrsLastActiveAt "Use generic-lens or generic-optics with 'lastActiveAt' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'identity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrsIdentity :: Lens.Lens' GetConnectionResponse (Lude.Maybe Identity)
gcrsIdentity = Lens.lens (identity :: GetConnectionResponse -> Lude.Maybe Identity) (\s a -> s {identity = a} :: GetConnectionResponse)
{-# DEPRECATED gcrsIdentity "Use generic-lens or generic-optics with 'identity' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrsResponseStatus :: Lens.Lens' GetConnectionResponse Lude.Int
gcrsResponseStatus = Lens.lens (responseStatus :: GetConnectionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetConnectionResponse)
{-# DEPRECATED gcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
