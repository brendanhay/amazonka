{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.PutAutoScalingPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates an automatic scaling policy for a core instance group or task instance group in an Amazon EMR cluster. The automatic scaling policy defines how an instance group dynamically adds and terminates EC2 instances in response to the value of a CloudWatch metric.
module Network.AWS.EMR.PutAutoScalingPolicy
  ( -- * Creating a request
    PutAutoScalingPolicy (..),
    mkPutAutoScalingPolicy,

    -- ** Request lenses
    paspClusterId,
    paspAutoScalingPolicy,
    paspInstanceGroupId,

    -- * Destructuring the response
    PutAutoScalingPolicyResponse (..),
    mkPutAutoScalingPolicyResponse,

    -- ** Response lenses
    pasprsClusterARN,
    pasprsClusterId,
    pasprsAutoScalingPolicy,
    pasprsInstanceGroupId,
    pasprsResponseStatus,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutAutoScalingPolicy' smart constructor.
data PutAutoScalingPolicy = PutAutoScalingPolicy'
  { -- | Specifies the ID of a cluster. The instance group to which the automatic scaling policy is applied is within this cluster.
    clusterId :: Lude.Text,
    -- | Specifies the definition of the automatic scaling policy.
    autoScalingPolicy :: AutoScalingPolicy,
    -- | Specifies the ID of the instance group to which the automatic scaling policy is applied.
    instanceGroupId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutAutoScalingPolicy' with the minimum fields required to make a request.
--
-- * 'clusterId' - Specifies the ID of a cluster. The instance group to which the automatic scaling policy is applied is within this cluster.
-- * 'autoScalingPolicy' - Specifies the definition of the automatic scaling policy.
-- * 'instanceGroupId' - Specifies the ID of the instance group to which the automatic scaling policy is applied.
mkPutAutoScalingPolicy ::
  -- | 'clusterId'
  Lude.Text ->
  -- | 'autoScalingPolicy'
  AutoScalingPolicy ->
  -- | 'instanceGroupId'
  Lude.Text ->
  PutAutoScalingPolicy
mkPutAutoScalingPolicy
  pClusterId_
  pAutoScalingPolicy_
  pInstanceGroupId_ =
    PutAutoScalingPolicy'
      { clusterId = pClusterId_,
        autoScalingPolicy = pAutoScalingPolicy_,
        instanceGroupId = pInstanceGroupId_
      }

-- | Specifies the ID of a cluster. The instance group to which the automatic scaling policy is applied is within this cluster.
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paspClusterId :: Lens.Lens' PutAutoScalingPolicy Lude.Text
paspClusterId = Lens.lens (clusterId :: PutAutoScalingPolicy -> Lude.Text) (\s a -> s {clusterId = a} :: PutAutoScalingPolicy)
{-# DEPRECATED paspClusterId "Use generic-lens or generic-optics with 'clusterId' instead." #-}

-- | Specifies the definition of the automatic scaling policy.
--
-- /Note:/ Consider using 'autoScalingPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paspAutoScalingPolicy :: Lens.Lens' PutAutoScalingPolicy AutoScalingPolicy
paspAutoScalingPolicy = Lens.lens (autoScalingPolicy :: PutAutoScalingPolicy -> AutoScalingPolicy) (\s a -> s {autoScalingPolicy = a} :: PutAutoScalingPolicy)
{-# DEPRECATED paspAutoScalingPolicy "Use generic-lens or generic-optics with 'autoScalingPolicy' instead." #-}

-- | Specifies the ID of the instance group to which the automatic scaling policy is applied.
--
-- /Note:/ Consider using 'instanceGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paspInstanceGroupId :: Lens.Lens' PutAutoScalingPolicy Lude.Text
paspInstanceGroupId = Lens.lens (instanceGroupId :: PutAutoScalingPolicy -> Lude.Text) (\s a -> s {instanceGroupId = a} :: PutAutoScalingPolicy)
{-# DEPRECATED paspInstanceGroupId "Use generic-lens or generic-optics with 'instanceGroupId' instead." #-}

instance Lude.AWSRequest PutAutoScalingPolicy where
  type Rs PutAutoScalingPolicy = PutAutoScalingPolicyResponse
  request = Req.postJSON emrService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutAutoScalingPolicyResponse'
            Lude.<$> (x Lude..?> "ClusterArn")
            Lude.<*> (x Lude..?> "ClusterId")
            Lude.<*> (x Lude..?> "AutoScalingPolicy")
            Lude.<*> (x Lude..?> "InstanceGroupId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutAutoScalingPolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("ElasticMapReduce.PutAutoScalingPolicy" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutAutoScalingPolicy where
  toJSON PutAutoScalingPolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ClusterId" Lude..= clusterId),
            Lude.Just ("AutoScalingPolicy" Lude..= autoScalingPolicy),
            Lude.Just ("InstanceGroupId" Lude..= instanceGroupId)
          ]
      )

instance Lude.ToPath PutAutoScalingPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery PutAutoScalingPolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutAutoScalingPolicyResponse' smart constructor.
data PutAutoScalingPolicyResponse = PutAutoScalingPolicyResponse'
  { -- | The Amazon Resource Name of the cluster.
    clusterARN :: Lude.Maybe Lude.Text,
    -- | Specifies the ID of a cluster. The instance group to which the automatic scaling policy is applied is within this cluster.
    clusterId :: Lude.Maybe Lude.Text,
    -- | The automatic scaling policy definition.
    autoScalingPolicy :: Lude.Maybe AutoScalingPolicyDescription,
    -- | Specifies the ID of the instance group to which the scaling policy is applied.
    instanceGroupId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutAutoScalingPolicyResponse' with the minimum fields required to make a request.
--
-- * 'clusterARN' - The Amazon Resource Name of the cluster.
-- * 'clusterId' - Specifies the ID of a cluster. The instance group to which the automatic scaling policy is applied is within this cluster.
-- * 'autoScalingPolicy' - The automatic scaling policy definition.
-- * 'instanceGroupId' - Specifies the ID of the instance group to which the scaling policy is applied.
-- * 'responseStatus' - The response status code.
mkPutAutoScalingPolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutAutoScalingPolicyResponse
mkPutAutoScalingPolicyResponse pResponseStatus_ =
  PutAutoScalingPolicyResponse'
    { clusterARN = Lude.Nothing,
      clusterId = Lude.Nothing,
      autoScalingPolicy = Lude.Nothing,
      instanceGroupId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name of the cluster.
--
-- /Note:/ Consider using 'clusterARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pasprsClusterARN :: Lens.Lens' PutAutoScalingPolicyResponse (Lude.Maybe Lude.Text)
pasprsClusterARN = Lens.lens (clusterARN :: PutAutoScalingPolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {clusterARN = a} :: PutAutoScalingPolicyResponse)
{-# DEPRECATED pasprsClusterARN "Use generic-lens or generic-optics with 'clusterARN' instead." #-}

-- | Specifies the ID of a cluster. The instance group to which the automatic scaling policy is applied is within this cluster.
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pasprsClusterId :: Lens.Lens' PutAutoScalingPolicyResponse (Lude.Maybe Lude.Text)
pasprsClusterId = Lens.lens (clusterId :: PutAutoScalingPolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {clusterId = a} :: PutAutoScalingPolicyResponse)
{-# DEPRECATED pasprsClusterId "Use generic-lens or generic-optics with 'clusterId' instead." #-}

-- | The automatic scaling policy definition.
--
-- /Note:/ Consider using 'autoScalingPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pasprsAutoScalingPolicy :: Lens.Lens' PutAutoScalingPolicyResponse (Lude.Maybe AutoScalingPolicyDescription)
pasprsAutoScalingPolicy = Lens.lens (autoScalingPolicy :: PutAutoScalingPolicyResponse -> Lude.Maybe AutoScalingPolicyDescription) (\s a -> s {autoScalingPolicy = a} :: PutAutoScalingPolicyResponse)
{-# DEPRECATED pasprsAutoScalingPolicy "Use generic-lens or generic-optics with 'autoScalingPolicy' instead." #-}

-- | Specifies the ID of the instance group to which the scaling policy is applied.
--
-- /Note:/ Consider using 'instanceGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pasprsInstanceGroupId :: Lens.Lens' PutAutoScalingPolicyResponse (Lude.Maybe Lude.Text)
pasprsInstanceGroupId = Lens.lens (instanceGroupId :: PutAutoScalingPolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {instanceGroupId = a} :: PutAutoScalingPolicyResponse)
{-# DEPRECATED pasprsInstanceGroupId "Use generic-lens or generic-optics with 'instanceGroupId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pasprsResponseStatus :: Lens.Lens' PutAutoScalingPolicyResponse Lude.Int
pasprsResponseStatus = Lens.lens (responseStatus :: PutAutoScalingPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutAutoScalingPolicyResponse)
{-# DEPRECATED pasprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
