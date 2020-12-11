{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.ModifyInstanceFleet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the target On-Demand and target Spot capacities for the instance fleet with the specified InstanceFleetID within the cluster specified using ClusterID. The call either succeeds or fails atomically.
module Network.AWS.EMR.ModifyInstanceFleet
  ( -- * Creating a request
    ModifyInstanceFleet (..),
    mkModifyInstanceFleet,

    -- ** Request lenses
    mifClusterId,
    mifInstanceFleet,

    -- * Destructuring the response
    ModifyInstanceFleetResponse (..),
    mkModifyInstanceFleetResponse,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyInstanceFleet' smart constructor.
data ModifyInstanceFleet = ModifyInstanceFleet'
  { clusterId ::
      Lude.Text,
    instanceFleet :: InstanceFleetModifyConfig
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyInstanceFleet' with the minimum fields required to make a request.
--
-- * 'clusterId' - The unique identifier of the cluster.
-- * 'instanceFleet' - The unique identifier of the instance fleet.
mkModifyInstanceFleet ::
  -- | 'clusterId'
  Lude.Text ->
  -- | 'instanceFleet'
  InstanceFleetModifyConfig ->
  ModifyInstanceFleet
mkModifyInstanceFleet pClusterId_ pInstanceFleet_ =
  ModifyInstanceFleet'
    { clusterId = pClusterId_,
      instanceFleet = pInstanceFleet_
    }

-- | The unique identifier of the cluster.
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mifClusterId :: Lens.Lens' ModifyInstanceFleet Lude.Text
mifClusterId = Lens.lens (clusterId :: ModifyInstanceFleet -> Lude.Text) (\s a -> s {clusterId = a} :: ModifyInstanceFleet)
{-# DEPRECATED mifClusterId "Use generic-lens or generic-optics with 'clusterId' instead." #-}

-- | The unique identifier of the instance fleet.
--
-- /Note:/ Consider using 'instanceFleet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mifInstanceFleet :: Lens.Lens' ModifyInstanceFleet InstanceFleetModifyConfig
mifInstanceFleet = Lens.lens (instanceFleet :: ModifyInstanceFleet -> InstanceFleetModifyConfig) (\s a -> s {instanceFleet = a} :: ModifyInstanceFleet)
{-# DEPRECATED mifInstanceFleet "Use generic-lens or generic-optics with 'instanceFleet' instead." #-}

instance Lude.AWSRequest ModifyInstanceFleet where
  type Rs ModifyInstanceFleet = ModifyInstanceFleetResponse
  request = Req.postJSON emrService
  response = Res.receiveNull ModifyInstanceFleetResponse'

instance Lude.ToHeaders ModifyInstanceFleet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("ElasticMapReduce.ModifyInstanceFleet" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ModifyInstanceFleet where
  toJSON ModifyInstanceFleet' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ClusterId" Lude..= clusterId),
            Lude.Just ("InstanceFleet" Lude..= instanceFleet)
          ]
      )

instance Lude.ToPath ModifyInstanceFleet where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyInstanceFleet where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkModifyInstanceFleetResponse' smart constructor.
data ModifyInstanceFleetResponse = ModifyInstanceFleetResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyInstanceFleetResponse' with the minimum fields required to make a request.
mkModifyInstanceFleetResponse ::
  ModifyInstanceFleetResponse
mkModifyInstanceFleetResponse = ModifyInstanceFleetResponse'
