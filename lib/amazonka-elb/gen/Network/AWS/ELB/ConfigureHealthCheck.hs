{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.ConfigureHealthCheck
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Specifies the health check settings to use when evaluating the health state of your EC2 instances.
--
-- For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-healthchecks.html Configure Health Checks for Your Load Balancer> in the /Classic Load Balancers Guide/ .
module Network.AWS.ELB.ConfigureHealthCheck
  ( -- * Creating a request
    ConfigureHealthCheck (..),
    mkConfigureHealthCheck,

    -- ** Request lenses
    chcLoadBalancerName,
    chcHealthCheck,

    -- * Destructuring the response
    ConfigureHealthCheckResponse (..),
    mkConfigureHealthCheckResponse,

    -- ** Response lenses
    chcrsHealthCheck,
    chcrsResponseStatus,
  )
where

import Network.AWS.ELB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for ConfigureHealthCheck.
--
-- /See:/ 'mkConfigureHealthCheck' smart constructor.
data ConfigureHealthCheck = ConfigureHealthCheck'
  { loadBalancerName ::
      Lude.Text,
    healthCheck :: HealthCheck
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConfigureHealthCheck' with the minimum fields required to make a request.
--
-- * 'healthCheck' - The configuration information.
-- * 'loadBalancerName' - The name of the load balancer.
mkConfigureHealthCheck ::
  -- | 'loadBalancerName'
  Lude.Text ->
  -- | 'healthCheck'
  HealthCheck ->
  ConfigureHealthCheck
mkConfigureHealthCheck pLoadBalancerName_ pHealthCheck_ =
  ConfigureHealthCheck'
    { loadBalancerName = pLoadBalancerName_,
      healthCheck = pHealthCheck_
    }

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chcLoadBalancerName :: Lens.Lens' ConfigureHealthCheck Lude.Text
chcLoadBalancerName = Lens.lens (loadBalancerName :: ConfigureHealthCheck -> Lude.Text) (\s a -> s {loadBalancerName = a} :: ConfigureHealthCheck)
{-# DEPRECATED chcLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

-- | The configuration information.
--
-- /Note:/ Consider using 'healthCheck' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chcHealthCheck :: Lens.Lens' ConfigureHealthCheck HealthCheck
chcHealthCheck = Lens.lens (healthCheck :: ConfigureHealthCheck -> HealthCheck) (\s a -> s {healthCheck = a} :: ConfigureHealthCheck)
{-# DEPRECATED chcHealthCheck "Use generic-lens or generic-optics with 'healthCheck' instead." #-}

instance Lude.AWSRequest ConfigureHealthCheck where
  type Rs ConfigureHealthCheck = ConfigureHealthCheckResponse
  request = Req.postQuery elbService
  response =
    Res.receiveXMLWrapper
      "ConfigureHealthCheckResult"
      ( \s h x ->
          ConfigureHealthCheckResponse'
            Lude.<$> (x Lude..@? "HealthCheck") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ConfigureHealthCheck where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ConfigureHealthCheck where
  toPath = Lude.const "/"

instance Lude.ToQuery ConfigureHealthCheck where
  toQuery ConfigureHealthCheck' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ConfigureHealthCheck" :: Lude.ByteString),
        "Version" Lude.=: ("2012-06-01" :: Lude.ByteString),
        "LoadBalancerName" Lude.=: loadBalancerName,
        "HealthCheck" Lude.=: healthCheck
      ]

-- | Contains the output of ConfigureHealthCheck.
--
-- /See:/ 'mkConfigureHealthCheckResponse' smart constructor.
data ConfigureHealthCheckResponse = ConfigureHealthCheckResponse'
  { healthCheck ::
      Lude.Maybe HealthCheck,
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

-- | Creates a value of 'ConfigureHealthCheckResponse' with the minimum fields required to make a request.
--
-- * 'healthCheck' - The updated health check.
-- * 'responseStatus' - The response status code.
mkConfigureHealthCheckResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ConfigureHealthCheckResponse
mkConfigureHealthCheckResponse pResponseStatus_ =
  ConfigureHealthCheckResponse'
    { healthCheck = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The updated health check.
--
-- /Note:/ Consider using 'healthCheck' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chcrsHealthCheck :: Lens.Lens' ConfigureHealthCheckResponse (Lude.Maybe HealthCheck)
chcrsHealthCheck = Lens.lens (healthCheck :: ConfigureHealthCheckResponse -> Lude.Maybe HealthCheck) (\s a -> s {healthCheck = a} :: ConfigureHealthCheckResponse)
{-# DEPRECATED chcrsHealthCheck "Use generic-lens or generic-optics with 'healthCheck' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chcrsResponseStatus :: Lens.Lens' ConfigureHealthCheckResponse Lude.Int
chcrsResponseStatus = Lens.lens (responseStatus :: ConfigureHealthCheckResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ConfigureHealthCheckResponse)
{-# DEPRECATED chcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
