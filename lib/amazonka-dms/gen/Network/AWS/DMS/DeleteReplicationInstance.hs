{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.DeleteReplicationInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified replication instance.
module Network.AWS.DMS.DeleteReplicationInstance
  ( -- * Creating a request
    DeleteReplicationInstance (..),
    mkDeleteReplicationInstance,

    -- ** Request lenses
    driReplicationInstanceARN,

    -- * Destructuring the response
    DeleteReplicationInstanceResponse (..),
    mkDeleteReplicationInstanceResponse,

    -- ** Response lenses
    drirsReplicationInstance,
    drirsResponseStatus,
  )
where

import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDeleteReplicationInstance' smart constructor.
newtype DeleteReplicationInstance = DeleteReplicationInstance'
  { replicationInstanceARN ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteReplicationInstance' with the minimum fields required to make a request.
--
-- * 'replicationInstanceARN' - The Amazon Resource Name (ARN) of the replication instance to be deleted.
mkDeleteReplicationInstance ::
  -- | 'replicationInstanceARN'
  Lude.Text ->
  DeleteReplicationInstance
mkDeleteReplicationInstance pReplicationInstanceARN_ =
  DeleteReplicationInstance'
    { replicationInstanceARN =
        pReplicationInstanceARN_
    }

-- | The Amazon Resource Name (ARN) of the replication instance to be deleted.
--
-- /Note:/ Consider using 'replicationInstanceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
driReplicationInstanceARN :: Lens.Lens' DeleteReplicationInstance Lude.Text
driReplicationInstanceARN = Lens.lens (replicationInstanceARN :: DeleteReplicationInstance -> Lude.Text) (\s a -> s {replicationInstanceARN = a} :: DeleteReplicationInstance)
{-# DEPRECATED driReplicationInstanceARN "Use generic-lens or generic-optics with 'replicationInstanceARN' instead." #-}

instance Lude.AWSRequest DeleteReplicationInstance where
  type
    Rs DeleteReplicationInstance =
      DeleteReplicationInstanceResponse
  request = Req.postJSON dmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteReplicationInstanceResponse'
            Lude.<$> (x Lude..?> "ReplicationInstance")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteReplicationInstance where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonDMSv20160101.DeleteReplicationInstance" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteReplicationInstance where
  toJSON DeleteReplicationInstance' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("ReplicationInstanceArn" Lude..= replicationInstanceARN)
          ]
      )

instance Lude.ToPath DeleteReplicationInstance where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteReplicationInstance where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkDeleteReplicationInstanceResponse' smart constructor.
data DeleteReplicationInstanceResponse = DeleteReplicationInstanceResponse'
  { replicationInstance ::
      Lude.Maybe
        ReplicationInstance,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteReplicationInstanceResponse' with the minimum fields required to make a request.
--
-- * 'replicationInstance' - The replication instance that was deleted.
-- * 'responseStatus' - The response status code.
mkDeleteReplicationInstanceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteReplicationInstanceResponse
mkDeleteReplicationInstanceResponse pResponseStatus_ =
  DeleteReplicationInstanceResponse'
    { replicationInstance =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The replication instance that was deleted.
--
-- /Note:/ Consider using 'replicationInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drirsReplicationInstance :: Lens.Lens' DeleteReplicationInstanceResponse (Lude.Maybe ReplicationInstance)
drirsReplicationInstance = Lens.lens (replicationInstance :: DeleteReplicationInstanceResponse -> Lude.Maybe ReplicationInstance) (\s a -> s {replicationInstance = a} :: DeleteReplicationInstanceResponse)
{-# DEPRECATED drirsReplicationInstance "Use generic-lens or generic-optics with 'replicationInstance' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drirsResponseStatus :: Lens.Lens' DeleteReplicationInstanceResponse Lude.Int
drirsResponseStatus = Lens.lens (responseStatus :: DeleteReplicationInstanceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteReplicationInstanceResponse)
{-# DEPRECATED drirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
