{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DetachElasticLoadBalancer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches a specified Elastic Load Balancing instance from its layer.
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.DetachElasticLoadBalancer
  ( -- * Creating a request
    DetachElasticLoadBalancer (..),
    mkDetachElasticLoadBalancer,

    -- ** Request lenses
    delbElasticLoadBalancerName,
    delbLayerId,

    -- * Destructuring the response
    DetachElasticLoadBalancerResponse (..),
    mkDetachElasticLoadBalancerResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDetachElasticLoadBalancer' smart constructor.
data DetachElasticLoadBalancer = DetachElasticLoadBalancer'
  { -- | The Elastic Load Balancing instance's name.
    elasticLoadBalancerName :: Lude.Text,
    -- | The ID of the layer that the Elastic Load Balancing instance is attached to.
    layerId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetachElasticLoadBalancer' with the minimum fields required to make a request.
--
-- * 'elasticLoadBalancerName' - The Elastic Load Balancing instance's name.
-- * 'layerId' - The ID of the layer that the Elastic Load Balancing instance is attached to.
mkDetachElasticLoadBalancer ::
  -- | 'elasticLoadBalancerName'
  Lude.Text ->
  -- | 'layerId'
  Lude.Text ->
  DetachElasticLoadBalancer
mkDetachElasticLoadBalancer pElasticLoadBalancerName_ pLayerId_ =
  DetachElasticLoadBalancer'
    { elasticLoadBalancerName =
        pElasticLoadBalancerName_,
      layerId = pLayerId_
    }

-- | The Elastic Load Balancing instance's name.
--
-- /Note:/ Consider using 'elasticLoadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delbElasticLoadBalancerName :: Lens.Lens' DetachElasticLoadBalancer Lude.Text
delbElasticLoadBalancerName = Lens.lens (elasticLoadBalancerName :: DetachElasticLoadBalancer -> Lude.Text) (\s a -> s {elasticLoadBalancerName = a} :: DetachElasticLoadBalancer)
{-# DEPRECATED delbElasticLoadBalancerName "Use generic-lens or generic-optics with 'elasticLoadBalancerName' instead." #-}

-- | The ID of the layer that the Elastic Load Balancing instance is attached to.
--
-- /Note:/ Consider using 'layerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delbLayerId :: Lens.Lens' DetachElasticLoadBalancer Lude.Text
delbLayerId = Lens.lens (layerId :: DetachElasticLoadBalancer -> Lude.Text) (\s a -> s {layerId = a} :: DetachElasticLoadBalancer)
{-# DEPRECATED delbLayerId "Use generic-lens or generic-optics with 'layerId' instead." #-}

instance Lude.AWSRequest DetachElasticLoadBalancer where
  type
    Rs DetachElasticLoadBalancer =
      DetachElasticLoadBalancerResponse
  request = Req.postJSON opsWorksService
  response = Res.receiveNull DetachElasticLoadBalancerResponse'

instance Lude.ToHeaders DetachElasticLoadBalancer where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.DetachElasticLoadBalancer" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DetachElasticLoadBalancer where
  toJSON DetachElasticLoadBalancer' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("ElasticLoadBalancerName" Lude..= elasticLoadBalancerName),
            Lude.Just ("LayerId" Lude..= layerId)
          ]
      )

instance Lude.ToPath DetachElasticLoadBalancer where
  toPath = Lude.const "/"

instance Lude.ToQuery DetachElasticLoadBalancer where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDetachElasticLoadBalancerResponse' smart constructor.
data DetachElasticLoadBalancerResponse = DetachElasticLoadBalancerResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetachElasticLoadBalancerResponse' with the minimum fields required to make a request.
mkDetachElasticLoadBalancerResponse ::
  DetachElasticLoadBalancerResponse
mkDetachElasticLoadBalancerResponse =
  DetachElasticLoadBalancerResponse'
