{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.ModifyCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the number of steps that can be executed concurrently for the cluster specified using ClusterID.
module Network.AWS.EMR.ModifyCluster
  ( -- * Creating a request
    ModifyCluster (..),
    mkModifyCluster,

    -- ** Request lenses
    mcStepConcurrencyLevel,
    mcClusterId,

    -- * Destructuring the response
    ModifyClusterResponse (..),
    mkModifyClusterResponse,

    -- ** Response lenses
    mcrsStepConcurrencyLevel,
    mcrsResponseStatus,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyCluster' smart constructor.
data ModifyCluster = ModifyCluster'
  { stepConcurrencyLevel ::
      Lude.Maybe Lude.Int,
    clusterId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyCluster' with the minimum fields required to make a request.
--
-- * 'clusterId' - The unique identifier of the cluster.
-- * 'stepConcurrencyLevel' - The number of steps that can be executed concurrently. You can specify a maximum of 256 steps.
mkModifyCluster ::
  -- | 'clusterId'
  Lude.Text ->
  ModifyCluster
mkModifyCluster pClusterId_ =
  ModifyCluster'
    { stepConcurrencyLevel = Lude.Nothing,
      clusterId = pClusterId_
    }

-- | The number of steps that can be executed concurrently. You can specify a maximum of 256 steps.
--
-- /Note:/ Consider using 'stepConcurrencyLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcStepConcurrencyLevel :: Lens.Lens' ModifyCluster (Lude.Maybe Lude.Int)
mcStepConcurrencyLevel = Lens.lens (stepConcurrencyLevel :: ModifyCluster -> Lude.Maybe Lude.Int) (\s a -> s {stepConcurrencyLevel = a} :: ModifyCluster)
{-# DEPRECATED mcStepConcurrencyLevel "Use generic-lens or generic-optics with 'stepConcurrencyLevel' instead." #-}

-- | The unique identifier of the cluster.
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcClusterId :: Lens.Lens' ModifyCluster Lude.Text
mcClusterId = Lens.lens (clusterId :: ModifyCluster -> Lude.Text) (\s a -> s {clusterId = a} :: ModifyCluster)
{-# DEPRECATED mcClusterId "Use generic-lens or generic-optics with 'clusterId' instead." #-}

instance Lude.AWSRequest ModifyCluster where
  type Rs ModifyCluster = ModifyClusterResponse
  request = Req.postJSON emrService
  response =
    Res.receiveJSON
      ( \s h x ->
          ModifyClusterResponse'
            Lude.<$> (x Lude..?> "StepConcurrencyLevel")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyCluster where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("ElasticMapReduce.ModifyCluster" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ModifyCluster where
  toJSON ModifyCluster' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("StepConcurrencyLevel" Lude..=) Lude.<$> stepConcurrencyLevel,
            Lude.Just ("ClusterId" Lude..= clusterId)
          ]
      )

instance Lude.ToPath ModifyCluster where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyCluster where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkModifyClusterResponse' smart constructor.
data ModifyClusterResponse = ModifyClusterResponse'
  { stepConcurrencyLevel ::
      Lude.Maybe Lude.Int,
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

-- | Creates a value of 'ModifyClusterResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'stepConcurrencyLevel' - The number of steps that can be executed concurrently.
mkModifyClusterResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyClusterResponse
mkModifyClusterResponse pResponseStatus_ =
  ModifyClusterResponse'
    { stepConcurrencyLevel = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The number of steps that can be executed concurrently.
--
-- /Note:/ Consider using 'stepConcurrencyLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcrsStepConcurrencyLevel :: Lens.Lens' ModifyClusterResponse (Lude.Maybe Lude.Int)
mcrsStepConcurrencyLevel = Lens.lens (stepConcurrencyLevel :: ModifyClusterResponse -> Lude.Maybe Lude.Int) (\s a -> s {stepConcurrencyLevel = a} :: ModifyClusterResponse)
{-# DEPRECATED mcrsStepConcurrencyLevel "Use generic-lens or generic-optics with 'stepConcurrencyLevel' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcrsResponseStatus :: Lens.Lens' ModifyClusterResponse Lude.Int
mcrsResponseStatus = Lens.lens (responseStatus :: ModifyClusterResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyClusterResponse)
{-# DEPRECATED mcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
