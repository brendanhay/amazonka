{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGatewayManagementAPI.PostToConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends the provided data to the specified connection.
module Network.AWS.APIGatewayManagementAPI.PostToConnection
  ( -- * Creating a request
    PostToConnection (..),
    mkPostToConnection,

    -- ** Request lenses
    ptcData,
    ptcConnectionId,

    -- * Destructuring the response
    PostToConnectionResponse (..),
    mkPostToConnectionResponse,
  )
where

import Network.AWS.APIGatewayManagementAPI.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPostToConnection' smart constructor.
data PostToConnection = PostToConnection'
  { -- | The data to be sent to the client specified by its connection id.
    data' :: Lude.ByteString,
    -- | The identifier of the connection that a specific client is using.
    connectionId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PostToConnection' with the minimum fields required to make a request.
--
-- * 'data'' - The data to be sent to the client specified by its connection id.
-- * 'connectionId' - The identifier of the connection that a specific client is using.
mkPostToConnection ::
  -- | 'data''
  Lude.ByteString ->
  -- | 'connectionId'
  Lude.Text ->
  PostToConnection
mkPostToConnection pData_ pConnectionId_ =
  PostToConnection' {data' = pData_, connectionId = pConnectionId_}

-- | The data to be sent to the client specified by its connection id.
--
-- /Note:/ Consider using 'data'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptcData :: Lens.Lens' PostToConnection Lude.ByteString
ptcData = Lens.lens (data' :: PostToConnection -> Lude.ByteString) (\s a -> s {data' = a} :: PostToConnection)
{-# DEPRECATED ptcData "Use generic-lens or generic-optics with 'data'' instead." #-}

-- | The identifier of the connection that a specific client is using.
--
-- /Note:/ Consider using 'connectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptcConnectionId :: Lens.Lens' PostToConnection Lude.Text
ptcConnectionId = Lens.lens (connectionId :: PostToConnection -> Lude.Text) (\s a -> s {connectionId = a} :: PostToConnection)
{-# DEPRECATED ptcConnectionId "Use generic-lens or generic-optics with 'connectionId' instead." #-}

instance Lude.AWSRequest PostToConnection where
  type Rs PostToConnection = PostToConnectionResponse
  request = Req.postBody apiGatewayManagementAPIService
  response = Res.receiveNull PostToConnectionResponse'

instance Lude.ToBody PostToConnection where
  toBody = Lude.toBody Lude.. data'

instance Lude.ToHeaders PostToConnection where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath PostToConnection where
  toPath PostToConnection' {..} =
    Lude.mconcat ["/@connections/", Lude.toBS connectionId]

instance Lude.ToQuery PostToConnection where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPostToConnectionResponse' smart constructor.
data PostToConnectionResponse = PostToConnectionResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PostToConnectionResponse' with the minimum fields required to make a request.
mkPostToConnectionResponse ::
  PostToConnectionResponse
mkPostToConnectionResponse = PostToConnectionResponse'
