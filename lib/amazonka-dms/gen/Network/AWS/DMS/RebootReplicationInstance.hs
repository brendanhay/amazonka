{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.RebootReplicationInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reboots a replication instance. Rebooting results in a momentary outage, until the replication instance becomes available again.
module Network.AWS.DMS.RebootReplicationInstance
  ( -- * Creating a request
    RebootReplicationInstance (..),
    mkRebootReplicationInstance,

    -- ** Request lenses
    rriForceFailover,
    rriReplicationInstanceARN,

    -- * Destructuring the response
    RebootReplicationInstanceResponse (..),
    mkRebootReplicationInstanceResponse,

    -- ** Response lenses
    rrirsReplicationInstance,
    rrirsResponseStatus,
  )
where

import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRebootReplicationInstance' smart constructor.
data RebootReplicationInstance = RebootReplicationInstance'
  { forceFailover ::
      Lude.Maybe Lude.Bool,
    replicationInstanceARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RebootReplicationInstance' with the minimum fields required to make a request.
--
-- * 'forceFailover' - If this parameter is @true@ , the reboot is conducted through a Multi-AZ failover. (If the instance isn't configured for Multi-AZ, then you can't specify @true@ .)
-- * 'replicationInstanceARN' - The Amazon Resource Name (ARN) of the replication instance.
mkRebootReplicationInstance ::
  -- | 'replicationInstanceARN'
  Lude.Text ->
  RebootReplicationInstance
mkRebootReplicationInstance pReplicationInstanceARN_ =
  RebootReplicationInstance'
    { forceFailover = Lude.Nothing,
      replicationInstanceARN = pReplicationInstanceARN_
    }

-- | If this parameter is @true@ , the reboot is conducted through a Multi-AZ failover. (If the instance isn't configured for Multi-AZ, then you can't specify @true@ .)
--
-- /Note:/ Consider using 'forceFailover' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rriForceFailover :: Lens.Lens' RebootReplicationInstance (Lude.Maybe Lude.Bool)
rriForceFailover = Lens.lens (forceFailover :: RebootReplicationInstance -> Lude.Maybe Lude.Bool) (\s a -> s {forceFailover = a} :: RebootReplicationInstance)
{-# DEPRECATED rriForceFailover "Use generic-lens or generic-optics with 'forceFailover' instead." #-}

-- | The Amazon Resource Name (ARN) of the replication instance.
--
-- /Note:/ Consider using 'replicationInstanceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rriReplicationInstanceARN :: Lens.Lens' RebootReplicationInstance Lude.Text
rriReplicationInstanceARN = Lens.lens (replicationInstanceARN :: RebootReplicationInstance -> Lude.Text) (\s a -> s {replicationInstanceARN = a} :: RebootReplicationInstance)
{-# DEPRECATED rriReplicationInstanceARN "Use generic-lens or generic-optics with 'replicationInstanceARN' instead." #-}

instance Lude.AWSRequest RebootReplicationInstance where
  type
    Rs RebootReplicationInstance =
      RebootReplicationInstanceResponse
  request = Req.postJSON dmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          RebootReplicationInstanceResponse'
            Lude.<$> (x Lude..?> "ReplicationInstance")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RebootReplicationInstance where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonDMSv20160101.RebootReplicationInstance" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RebootReplicationInstance where
  toJSON RebootReplicationInstance' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ForceFailover" Lude..=) Lude.<$> forceFailover,
            Lude.Just
              ("ReplicationInstanceArn" Lude..= replicationInstanceARN)
          ]
      )

instance Lude.ToPath RebootReplicationInstance where
  toPath = Lude.const "/"

instance Lude.ToQuery RebootReplicationInstance where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRebootReplicationInstanceResponse' smart constructor.
data RebootReplicationInstanceResponse = RebootReplicationInstanceResponse'
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

-- | Creates a value of 'RebootReplicationInstanceResponse' with the minimum fields required to make a request.
--
-- * 'replicationInstance' - The replication instance that is being rebooted.
-- * 'responseStatus' - The response status code.
mkRebootReplicationInstanceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RebootReplicationInstanceResponse
mkRebootReplicationInstanceResponse pResponseStatus_ =
  RebootReplicationInstanceResponse'
    { replicationInstance =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The replication instance that is being rebooted.
--
-- /Note:/ Consider using 'replicationInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrirsReplicationInstance :: Lens.Lens' RebootReplicationInstanceResponse (Lude.Maybe ReplicationInstance)
rrirsReplicationInstance = Lens.lens (replicationInstance :: RebootReplicationInstanceResponse -> Lude.Maybe ReplicationInstance) (\s a -> s {replicationInstance = a} :: RebootReplicationInstanceResponse)
{-# DEPRECATED rrirsReplicationInstance "Use generic-lens or generic-optics with 'replicationInstance' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrirsResponseStatus :: Lens.Lens' RebootReplicationInstanceResponse Lude.Int
rrirsResponseStatus = Lens.lens (responseStatus :: RebootReplicationInstanceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RebootReplicationInstanceResponse)
{-# DEPRECATED rrirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
