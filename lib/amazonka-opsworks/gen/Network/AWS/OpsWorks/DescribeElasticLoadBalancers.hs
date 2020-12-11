{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribeElasticLoadBalancers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a stack's Elastic Load Balancing instances.
--
-- __Required Permissions__ : To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information about user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.DescribeElasticLoadBalancers
  ( -- * Creating a request
    DescribeElasticLoadBalancers (..),
    mkDescribeElasticLoadBalancers,

    -- ** Request lenses
    delbLayerIds,
    delbStackId,

    -- * Destructuring the response
    DescribeElasticLoadBalancersResponse (..),
    mkDescribeElasticLoadBalancersResponse,

    -- ** Response lenses
    delbrsElasticLoadBalancers,
    delbrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeElasticLoadBalancers' smart constructor.
data DescribeElasticLoadBalancers = DescribeElasticLoadBalancers'
  { layerIds ::
      Lude.Maybe [Lude.Text],
    stackId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeElasticLoadBalancers' with the minimum fields required to make a request.
--
-- * 'layerIds' - A list of layer IDs. The action describes the Elastic Load Balancing instances for the specified layers.
-- * 'stackId' - A stack ID. The action describes the stack's Elastic Load Balancing instances.
mkDescribeElasticLoadBalancers ::
  DescribeElasticLoadBalancers
mkDescribeElasticLoadBalancers =
  DescribeElasticLoadBalancers'
    { layerIds = Lude.Nothing,
      stackId = Lude.Nothing
    }

-- | A list of layer IDs. The action describes the Elastic Load Balancing instances for the specified layers.
--
-- /Note:/ Consider using 'layerIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delbLayerIds :: Lens.Lens' DescribeElasticLoadBalancers (Lude.Maybe [Lude.Text])
delbLayerIds = Lens.lens (layerIds :: DescribeElasticLoadBalancers -> Lude.Maybe [Lude.Text]) (\s a -> s {layerIds = a} :: DescribeElasticLoadBalancers)
{-# DEPRECATED delbLayerIds "Use generic-lens or generic-optics with 'layerIds' instead." #-}

-- | A stack ID. The action describes the stack's Elastic Load Balancing instances.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delbStackId :: Lens.Lens' DescribeElasticLoadBalancers (Lude.Maybe Lude.Text)
delbStackId = Lens.lens (stackId :: DescribeElasticLoadBalancers -> Lude.Maybe Lude.Text) (\s a -> s {stackId = a} :: DescribeElasticLoadBalancers)
{-# DEPRECATED delbStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

instance Lude.AWSRequest DescribeElasticLoadBalancers where
  type
    Rs DescribeElasticLoadBalancers =
      DescribeElasticLoadBalancersResponse
  request = Req.postJSON opsWorksService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeElasticLoadBalancersResponse'
            Lude.<$> (x Lude..?> "ElasticLoadBalancers" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeElasticLoadBalancers where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "OpsWorks_20130218.DescribeElasticLoadBalancers" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeElasticLoadBalancers where
  toJSON DescribeElasticLoadBalancers' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("LayerIds" Lude..=) Lude.<$> layerIds,
            ("StackId" Lude..=) Lude.<$> stackId
          ]
      )

instance Lude.ToPath DescribeElasticLoadBalancers where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeElasticLoadBalancers where
  toQuery = Lude.const Lude.mempty

-- | Contains the response to a @DescribeElasticLoadBalancers@ request.
--
-- /See:/ 'mkDescribeElasticLoadBalancersResponse' smart constructor.
data DescribeElasticLoadBalancersResponse = DescribeElasticLoadBalancersResponse'
  { elasticLoadBalancers ::
      Lude.Maybe
        [ElasticLoadBalancer],
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

-- | Creates a value of 'DescribeElasticLoadBalancersResponse' with the minimum fields required to make a request.
--
-- * 'elasticLoadBalancers' - A list of @ElasticLoadBalancer@ objects that describe the specified Elastic Load Balancing instances.
-- * 'responseStatus' - The response status code.
mkDescribeElasticLoadBalancersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeElasticLoadBalancersResponse
mkDescribeElasticLoadBalancersResponse pResponseStatus_ =
  DescribeElasticLoadBalancersResponse'
    { elasticLoadBalancers =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of @ElasticLoadBalancer@ objects that describe the specified Elastic Load Balancing instances.
--
-- /Note:/ Consider using 'elasticLoadBalancers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delbrsElasticLoadBalancers :: Lens.Lens' DescribeElasticLoadBalancersResponse (Lude.Maybe [ElasticLoadBalancer])
delbrsElasticLoadBalancers = Lens.lens (elasticLoadBalancers :: DescribeElasticLoadBalancersResponse -> Lude.Maybe [ElasticLoadBalancer]) (\s a -> s {elasticLoadBalancers = a} :: DescribeElasticLoadBalancersResponse)
{-# DEPRECATED delbrsElasticLoadBalancers "Use generic-lens or generic-optics with 'elasticLoadBalancers' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delbrsResponseStatus :: Lens.Lens' DescribeElasticLoadBalancersResponse Lude.Int
delbrsResponseStatus = Lens.lens (responseStatus :: DescribeElasticLoadBalancersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeElasticLoadBalancersResponse)
{-# DEPRECATED delbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
