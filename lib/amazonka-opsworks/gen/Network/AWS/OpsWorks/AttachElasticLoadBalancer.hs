{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.AttachElasticLoadBalancer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches an Elastic Load Balancing load balancer to a specified layer. AWS OpsWorks Stacks does not support Application Load Balancer. You can only use Classic Load Balancer with AWS OpsWorks Stacks. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/layers-elb.html Elastic Load Balancing> .
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.AttachElasticLoadBalancer
  ( -- * Creating a request
    AttachElasticLoadBalancer (..),
    mkAttachElasticLoadBalancer,

    -- ** Request lenses
    aelbElasticLoadBalancerName,
    aelbLayerId,

    -- * Destructuring the response
    AttachElasticLoadBalancerResponse (..),
    mkAttachElasticLoadBalancerResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAttachElasticLoadBalancer' smart constructor.
data AttachElasticLoadBalancer = AttachElasticLoadBalancer'
  { elasticLoadBalancerName ::
      Lude.Text,
    layerId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttachElasticLoadBalancer' with the minimum fields required to make a request.
--
-- * 'elasticLoadBalancerName' - The Elastic Load Balancing instance's name.
-- * 'layerId' - The ID of the layer to which the Elastic Load Balancing instance is to be attached.
mkAttachElasticLoadBalancer ::
  -- | 'elasticLoadBalancerName'
  Lude.Text ->
  -- | 'layerId'
  Lude.Text ->
  AttachElasticLoadBalancer
mkAttachElasticLoadBalancer pElasticLoadBalancerName_ pLayerId_ =
  AttachElasticLoadBalancer'
    { elasticLoadBalancerName =
        pElasticLoadBalancerName_,
      layerId = pLayerId_
    }

-- | The Elastic Load Balancing instance's name.
--
-- /Note:/ Consider using 'elasticLoadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aelbElasticLoadBalancerName :: Lens.Lens' AttachElasticLoadBalancer Lude.Text
aelbElasticLoadBalancerName = Lens.lens (elasticLoadBalancerName :: AttachElasticLoadBalancer -> Lude.Text) (\s a -> s {elasticLoadBalancerName = a} :: AttachElasticLoadBalancer)
{-# DEPRECATED aelbElasticLoadBalancerName "Use generic-lens or generic-optics with 'elasticLoadBalancerName' instead." #-}

-- | The ID of the layer to which the Elastic Load Balancing instance is to be attached.
--
-- /Note:/ Consider using 'layerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aelbLayerId :: Lens.Lens' AttachElasticLoadBalancer Lude.Text
aelbLayerId = Lens.lens (layerId :: AttachElasticLoadBalancer -> Lude.Text) (\s a -> s {layerId = a} :: AttachElasticLoadBalancer)
{-# DEPRECATED aelbLayerId "Use generic-lens or generic-optics with 'layerId' instead." #-}

instance Lude.AWSRequest AttachElasticLoadBalancer where
  type
    Rs AttachElasticLoadBalancer =
      AttachElasticLoadBalancerResponse
  request = Req.postJSON opsWorksService
  response = Res.receiveNull AttachElasticLoadBalancerResponse'

instance Lude.ToHeaders AttachElasticLoadBalancer where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.AttachElasticLoadBalancer" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AttachElasticLoadBalancer where
  toJSON AttachElasticLoadBalancer' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("ElasticLoadBalancerName" Lude..= elasticLoadBalancerName),
            Lude.Just ("LayerId" Lude..= layerId)
          ]
      )

instance Lude.ToPath AttachElasticLoadBalancer where
  toPath = Lude.const "/"

instance Lude.ToQuery AttachElasticLoadBalancer where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAttachElasticLoadBalancerResponse' smart constructor.
data AttachElasticLoadBalancerResponse = AttachElasticLoadBalancerResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttachElasticLoadBalancerResponse' with the minimum fields required to make a request.
mkAttachElasticLoadBalancerResponse ::
  AttachElasticLoadBalancerResponse
mkAttachElasticLoadBalancerResponse =
  AttachElasticLoadBalancerResponse'
