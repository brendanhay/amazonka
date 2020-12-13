{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.DeleteConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the connection between a replication instance and an endpoint.
module Network.AWS.DMS.DeleteConnection
  ( -- * Creating a request
    DeleteConnection (..),
    mkDeleteConnection,

    -- ** Request lenses
    dcReplicationInstanceARN,
    dcEndpointARN,

    -- * Destructuring the response
    DeleteConnectionResponse (..),
    mkDeleteConnectionResponse,

    -- ** Response lenses
    dcrsConnection,
    dcrsResponseStatus,
  )
where

import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDeleteConnection' smart constructor.
data DeleteConnection = DeleteConnection'
  { -- | The Amazon Resource Name (ARN) of the replication instance.
    replicationInstanceARN :: Lude.Text,
    -- | The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
    endpointARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteConnection' with the minimum fields required to make a request.
--
-- * 'replicationInstanceARN' - The Amazon Resource Name (ARN) of the replication instance.
-- * 'endpointARN' - The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
mkDeleteConnection ::
  -- | 'replicationInstanceARN'
  Lude.Text ->
  -- | 'endpointARN'
  Lude.Text ->
  DeleteConnection
mkDeleteConnection pReplicationInstanceARN_ pEndpointARN_ =
  DeleteConnection'
    { replicationInstanceARN =
        pReplicationInstanceARN_,
      endpointARN = pEndpointARN_
    }

-- | The Amazon Resource Name (ARN) of the replication instance.
--
-- /Note:/ Consider using 'replicationInstanceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcReplicationInstanceARN :: Lens.Lens' DeleteConnection Lude.Text
dcReplicationInstanceARN = Lens.lens (replicationInstanceARN :: DeleteConnection -> Lude.Text) (\s a -> s {replicationInstanceARN = a} :: DeleteConnection)
{-# DEPRECATED dcReplicationInstanceARN "Use generic-lens or generic-optics with 'replicationInstanceARN' instead." #-}

-- | The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
--
-- /Note:/ Consider using 'endpointARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcEndpointARN :: Lens.Lens' DeleteConnection Lude.Text
dcEndpointARN = Lens.lens (endpointARN :: DeleteConnection -> Lude.Text) (\s a -> s {endpointARN = a} :: DeleteConnection)
{-# DEPRECATED dcEndpointARN "Use generic-lens or generic-optics with 'endpointARN' instead." #-}

instance Lude.AWSRequest DeleteConnection where
  type Rs DeleteConnection = DeleteConnectionResponse
  request = Req.postJSON dmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteConnectionResponse'
            Lude.<$> (x Lude..?> "Connection") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteConnection where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonDMSv20160101.DeleteConnection" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteConnection where
  toJSON DeleteConnection' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("ReplicationInstanceArn" Lude..= replicationInstanceARN),
            Lude.Just ("EndpointArn" Lude..= endpointARN)
          ]
      )

instance Lude.ToPath DeleteConnection where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteConnection where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkDeleteConnectionResponse' smart constructor.
data DeleteConnectionResponse = DeleteConnectionResponse'
  { -- | The connection that is being deleted.
    connection :: Lude.Maybe Connection,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteConnectionResponse' with the minimum fields required to make a request.
--
-- * 'connection' - The connection that is being deleted.
-- * 'responseStatus' - The response status code.
mkDeleteConnectionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteConnectionResponse
mkDeleteConnectionResponse pResponseStatus_ =
  DeleteConnectionResponse'
    { connection = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The connection that is being deleted.
--
-- /Note:/ Consider using 'connection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsConnection :: Lens.Lens' DeleteConnectionResponse (Lude.Maybe Connection)
dcrsConnection = Lens.lens (connection :: DeleteConnectionResponse -> Lude.Maybe Connection) (\s a -> s {connection = a} :: DeleteConnectionResponse)
{-# DEPRECATED dcrsConnection "Use generic-lens or generic-optics with 'connection' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsResponseStatus :: Lens.Lens' DeleteConnectionResponse Lude.Int
dcrsResponseStatus = Lens.lens (responseStatus :: DeleteConnectionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteConnectionResponse)
{-# DEPRECATED dcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
