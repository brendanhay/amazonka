{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.TestConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Tests the connection between the replication instance and the endpoint.
module Network.AWS.DMS.TestConnection
  ( -- * Creating a request
    TestConnection (..),
    mkTestConnection,

    -- ** Request lenses
    tcReplicationInstanceARN,
    tcEndpointARN,

    -- * Destructuring the response
    TestConnectionResponse (..),
    mkTestConnectionResponse,

    -- ** Response lenses
    tcrsConnection,
    tcrsResponseStatus,
  )
where

import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkTestConnection' smart constructor.
data TestConnection = TestConnection'
  { replicationInstanceARN ::
      Lude.Text,
    endpointARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TestConnection' with the minimum fields required to make a request.
--
-- * 'endpointARN' - The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
-- * 'replicationInstanceARN' - The Amazon Resource Name (ARN) of the replication instance.
mkTestConnection ::
  -- | 'replicationInstanceARN'
  Lude.Text ->
  -- | 'endpointARN'
  Lude.Text ->
  TestConnection
mkTestConnection pReplicationInstanceARN_ pEndpointARN_ =
  TestConnection'
    { replicationInstanceARN =
        pReplicationInstanceARN_,
      endpointARN = pEndpointARN_
    }

-- | The Amazon Resource Name (ARN) of the replication instance.
--
-- /Note:/ Consider using 'replicationInstanceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcReplicationInstanceARN :: Lens.Lens' TestConnection Lude.Text
tcReplicationInstanceARN = Lens.lens (replicationInstanceARN :: TestConnection -> Lude.Text) (\s a -> s {replicationInstanceARN = a} :: TestConnection)
{-# DEPRECATED tcReplicationInstanceARN "Use generic-lens or generic-optics with 'replicationInstanceARN' instead." #-}

-- | The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
--
-- /Note:/ Consider using 'endpointARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcEndpointARN :: Lens.Lens' TestConnection Lude.Text
tcEndpointARN = Lens.lens (endpointARN :: TestConnection -> Lude.Text) (\s a -> s {endpointARN = a} :: TestConnection)
{-# DEPRECATED tcEndpointARN "Use generic-lens or generic-optics with 'endpointARN' instead." #-}

instance Lude.AWSRequest TestConnection where
  type Rs TestConnection = TestConnectionResponse
  request = Req.postJSON dmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          TestConnectionResponse'
            Lude.<$> (x Lude..?> "Connection") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders TestConnection where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonDMSv20160101.TestConnection" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON TestConnection where
  toJSON TestConnection' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("ReplicationInstanceArn" Lude..= replicationInstanceARN),
            Lude.Just ("EndpointArn" Lude..= endpointARN)
          ]
      )

instance Lude.ToPath TestConnection where
  toPath = Lude.const "/"

instance Lude.ToQuery TestConnection where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkTestConnectionResponse' smart constructor.
data TestConnectionResponse = TestConnectionResponse'
  { connection ::
      Lude.Maybe Connection,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TestConnectionResponse' with the minimum fields required to make a request.
--
-- * 'connection' - The connection tested.
-- * 'responseStatus' - The response status code.
mkTestConnectionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  TestConnectionResponse
mkTestConnectionResponse pResponseStatus_ =
  TestConnectionResponse'
    { connection = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The connection tested.
--
-- /Note:/ Consider using 'connection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcrsConnection :: Lens.Lens' TestConnectionResponse (Lude.Maybe Connection)
tcrsConnection = Lens.lens (connection :: TestConnectionResponse -> Lude.Maybe Connection) (\s a -> s {connection = a} :: TestConnectionResponse)
{-# DEPRECATED tcrsConnection "Use generic-lens or generic-optics with 'connection' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcrsResponseStatus :: Lens.Lens' TestConnectionResponse Lude.Int
tcrsResponseStatus = Lens.lens (responseStatus :: TestConnectionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: TestConnectionResponse)
{-# DEPRECATED tcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
