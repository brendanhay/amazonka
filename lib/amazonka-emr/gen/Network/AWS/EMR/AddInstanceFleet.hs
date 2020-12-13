{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.AddInstanceFleet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an instance fleet to a running cluster.
module Network.AWS.EMR.AddInstanceFleet
  ( -- * Creating a request
    AddInstanceFleet (..),
    mkAddInstanceFleet,

    -- ** Request lenses
    aifInstanceFleet,
    aifClusterId,

    -- * Destructuring the response
    AddInstanceFleetResponse (..),
    mkAddInstanceFleetResponse,

    -- ** Response lenses
    aifrsClusterARN,
    aifrsClusterId,
    aifrsInstanceFleetId,
    aifrsResponseStatus,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAddInstanceFleet' smart constructor.
data AddInstanceFleet = AddInstanceFleet'
  { -- | Specifies the configuration of the instance fleet.
    instanceFleet :: InstanceFleetConfig,
    -- | The unique identifier of the cluster.
    clusterId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddInstanceFleet' with the minimum fields required to make a request.
--
-- * 'instanceFleet' - Specifies the configuration of the instance fleet.
-- * 'clusterId' - The unique identifier of the cluster.
mkAddInstanceFleet ::
  -- | 'instanceFleet'
  InstanceFleetConfig ->
  -- | 'clusterId'
  Lude.Text ->
  AddInstanceFleet
mkAddInstanceFleet pInstanceFleet_ pClusterId_ =
  AddInstanceFleet'
    { instanceFleet = pInstanceFleet_,
      clusterId = pClusterId_
    }

-- | Specifies the configuration of the instance fleet.
--
-- /Note:/ Consider using 'instanceFleet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aifInstanceFleet :: Lens.Lens' AddInstanceFleet InstanceFleetConfig
aifInstanceFleet = Lens.lens (instanceFleet :: AddInstanceFleet -> InstanceFleetConfig) (\s a -> s {instanceFleet = a} :: AddInstanceFleet)
{-# DEPRECATED aifInstanceFleet "Use generic-lens or generic-optics with 'instanceFleet' instead." #-}

-- | The unique identifier of the cluster.
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aifClusterId :: Lens.Lens' AddInstanceFleet Lude.Text
aifClusterId = Lens.lens (clusterId :: AddInstanceFleet -> Lude.Text) (\s a -> s {clusterId = a} :: AddInstanceFleet)
{-# DEPRECATED aifClusterId "Use generic-lens or generic-optics with 'clusterId' instead." #-}

instance Lude.AWSRequest AddInstanceFleet where
  type Rs AddInstanceFleet = AddInstanceFleetResponse
  request = Req.postJSON emrService
  response =
    Res.receiveJSON
      ( \s h x ->
          AddInstanceFleetResponse'
            Lude.<$> (x Lude..?> "ClusterArn")
            Lude.<*> (x Lude..?> "ClusterId")
            Lude.<*> (x Lude..?> "InstanceFleetId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AddInstanceFleet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("ElasticMapReduce.AddInstanceFleet" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AddInstanceFleet where
  toJSON AddInstanceFleet' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("InstanceFleet" Lude..= instanceFleet),
            Lude.Just ("ClusterId" Lude..= clusterId)
          ]
      )

instance Lude.ToPath AddInstanceFleet where
  toPath = Lude.const "/"

instance Lude.ToQuery AddInstanceFleet where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAddInstanceFleetResponse' smart constructor.
data AddInstanceFleetResponse = AddInstanceFleetResponse'
  { -- | The Amazon Resource Name of the cluster.
    clusterARN :: Lude.Maybe Lude.Text,
    -- | The unique identifier of the cluster.
    clusterId :: Lude.Maybe Lude.Text,
    -- | The unique identifier of the instance fleet.
    instanceFleetId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddInstanceFleetResponse' with the minimum fields required to make a request.
--
-- * 'clusterARN' - The Amazon Resource Name of the cluster.
-- * 'clusterId' - The unique identifier of the cluster.
-- * 'instanceFleetId' - The unique identifier of the instance fleet.
-- * 'responseStatus' - The response status code.
mkAddInstanceFleetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AddInstanceFleetResponse
mkAddInstanceFleetResponse pResponseStatus_ =
  AddInstanceFleetResponse'
    { clusterARN = Lude.Nothing,
      clusterId = Lude.Nothing,
      instanceFleetId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name of the cluster.
--
-- /Note:/ Consider using 'clusterARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aifrsClusterARN :: Lens.Lens' AddInstanceFleetResponse (Lude.Maybe Lude.Text)
aifrsClusterARN = Lens.lens (clusterARN :: AddInstanceFleetResponse -> Lude.Maybe Lude.Text) (\s a -> s {clusterARN = a} :: AddInstanceFleetResponse)
{-# DEPRECATED aifrsClusterARN "Use generic-lens or generic-optics with 'clusterARN' instead." #-}

-- | The unique identifier of the cluster.
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aifrsClusterId :: Lens.Lens' AddInstanceFleetResponse (Lude.Maybe Lude.Text)
aifrsClusterId = Lens.lens (clusterId :: AddInstanceFleetResponse -> Lude.Maybe Lude.Text) (\s a -> s {clusterId = a} :: AddInstanceFleetResponse)
{-# DEPRECATED aifrsClusterId "Use generic-lens or generic-optics with 'clusterId' instead." #-}

-- | The unique identifier of the instance fleet.
--
-- /Note:/ Consider using 'instanceFleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aifrsInstanceFleetId :: Lens.Lens' AddInstanceFleetResponse (Lude.Maybe Lude.Text)
aifrsInstanceFleetId = Lens.lens (instanceFleetId :: AddInstanceFleetResponse -> Lude.Maybe Lude.Text) (\s a -> s {instanceFleetId = a} :: AddInstanceFleetResponse)
{-# DEPRECATED aifrsInstanceFleetId "Use generic-lens or generic-optics with 'instanceFleetId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aifrsResponseStatus :: Lens.Lens' AddInstanceFleetResponse Lude.Int
aifrsResponseStatus = Lens.lens (responseStatus :: AddInstanceFleetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AddInstanceFleetResponse)
{-# DEPRECATED aifrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
