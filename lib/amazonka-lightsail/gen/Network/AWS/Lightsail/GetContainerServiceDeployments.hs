{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetContainerServiceDeployments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the deployments for your Amazon Lightsail container service
--
-- A deployment specifies the settings, such as the ports and launch command, of containers that are deployed to your container service.
-- The deployments are ordered by version in ascending order. The newest version is listed at the top of the response.
module Network.AWS.Lightsail.GetContainerServiceDeployments
  ( -- * Creating a request
    GetContainerServiceDeployments (..),
    mkGetContainerServiceDeployments,

    -- ** Request lenses
    gcsdServiceName,

    -- * Destructuring the response
    GetContainerServiceDeploymentsResponse (..),
    mkGetContainerServiceDeploymentsResponse,

    -- ** Response lenses
    gcsdrsDeployments,
    gcsdrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetContainerServiceDeployments' smart constructor.
newtype GetContainerServiceDeployments = GetContainerServiceDeployments'
  { serviceName ::
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

-- | Creates a value of 'GetContainerServiceDeployments' with the minimum fields required to make a request.
--
-- * 'serviceName' - The name of the container service for which to return deployments.
mkGetContainerServiceDeployments ::
  -- | 'serviceName'
  Lude.Text ->
  GetContainerServiceDeployments
mkGetContainerServiceDeployments pServiceName_ =
  GetContainerServiceDeployments' {serviceName = pServiceName_}

-- | The name of the container service for which to return deployments.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsdServiceName :: Lens.Lens' GetContainerServiceDeployments Lude.Text
gcsdServiceName = Lens.lens (serviceName :: GetContainerServiceDeployments -> Lude.Text) (\s a -> s {serviceName = a} :: GetContainerServiceDeployments)
{-# DEPRECATED gcsdServiceName "Use generic-lens or generic-optics with 'serviceName' instead." #-}

instance Lude.AWSRequest GetContainerServiceDeployments where
  type
    Rs GetContainerServiceDeployments =
      GetContainerServiceDeploymentsResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetContainerServiceDeploymentsResponse'
            Lude.<$> (x Lude..?> "deployments" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetContainerServiceDeployments where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Lightsail_20161128.GetContainerServiceDeployments" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetContainerServiceDeployments where
  toJSON GetContainerServiceDeployments' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("serviceName" Lude..= serviceName)])

instance Lude.ToPath GetContainerServiceDeployments where
  toPath = Lude.const "/"

instance Lude.ToQuery GetContainerServiceDeployments where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetContainerServiceDeploymentsResponse' smart constructor.
data GetContainerServiceDeploymentsResponse = GetContainerServiceDeploymentsResponse'
  { deployments ::
      Lude.Maybe
        [ContainerServiceDeployment],
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

-- | Creates a value of 'GetContainerServiceDeploymentsResponse' with the minimum fields required to make a request.
--
-- * 'deployments' - An array of objects that describe deployments for a container service.
-- * 'responseStatus' - The response status code.
mkGetContainerServiceDeploymentsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetContainerServiceDeploymentsResponse
mkGetContainerServiceDeploymentsResponse pResponseStatus_ =
  GetContainerServiceDeploymentsResponse'
    { deployments =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe deployments for a container service.
--
-- /Note:/ Consider using 'deployments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsdrsDeployments :: Lens.Lens' GetContainerServiceDeploymentsResponse (Lude.Maybe [ContainerServiceDeployment])
gcsdrsDeployments = Lens.lens (deployments :: GetContainerServiceDeploymentsResponse -> Lude.Maybe [ContainerServiceDeployment]) (\s a -> s {deployments = a} :: GetContainerServiceDeploymentsResponse)
{-# DEPRECATED gcsdrsDeployments "Use generic-lens or generic-optics with 'deployments' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsdrsResponseStatus :: Lens.Lens' GetContainerServiceDeploymentsResponse Lude.Int
gcsdrsResponseStatus = Lens.lens (responseStatus :: GetContainerServiceDeploymentsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetContainerServiceDeploymentsResponse)
{-# DEPRECATED gcsdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
