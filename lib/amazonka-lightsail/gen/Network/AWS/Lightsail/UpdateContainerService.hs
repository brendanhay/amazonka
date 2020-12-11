{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.UpdateContainerService
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the configuration of your Amazon Lightsail container service, such as its power, scale, and public domain names.
module Network.AWS.Lightsail.UpdateContainerService
  ( -- * Creating a request
    UpdateContainerService (..),
    mkUpdateContainerService,

    -- ** Request lenses
    ucsScale,
    ucsPower,
    ucsIsDisabled,
    ucsPublicDomainNames,
    ucsServiceName,

    -- * Destructuring the response
    UpdateContainerServiceResponse (..),
    mkUpdateContainerServiceResponse,

    -- ** Response lenses
    ucsrsContainerService,
    ucsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateContainerService' smart constructor.
data UpdateContainerService = UpdateContainerService'
  { scale ::
      Lude.Maybe Lude.Natural,
    power :: Lude.Maybe ContainerServicePowerName,
    isDisabled :: Lude.Maybe Lude.Bool,
    publicDomainNames ::
      Lude.Maybe
        (Lude.HashMap Lude.Text ([Lude.Text])),
    serviceName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateContainerService' with the minimum fields required to make a request.
--
-- * 'isDisabled' - A Boolean value to indicate whether the container service is disabled.
-- * 'power' - The power for the container service.
--
-- The power specifies the amount of memory, vCPUs, and base monthly cost of each node of the container service. The @power@ and @scale@ of a container service makes up its configured capacity. To determine the monthly price of your container service, multiply the base price of the @power@ with the @scale@ (the number of nodes) of the service.
-- Use the @GetContainerServicePowers@ action to view the specifications of each power option.
-- * 'publicDomainNames' - The public domain names to use with the container service, such as @example.com@ and @www.example.com@ .
--
-- You can specify up to four public domain names for a container service. The domain names that you specify are used when you create a deployment with a container configured as the public endpoint of your container service.
-- If you don't specify public domain names, then you can use the default domain of the container service.
-- /Important:/ You must create and validate an SSL/TLS certificate before you can use public domain names with your container service. Use the @CreateCertificate@ action to create a certificate for the public domain names you want to use with your container service.
-- You can specify public domain names using a string to array map as shown in the example later on this page.
-- * 'scale' - The scale for the container service.
--
-- The scale specifies the allocated compute nodes of the container service. The @power@ and @scale@ of a container service makes up its configured capacity. To determine the monthly price of your container service, multiply the base price of the @power@ with the @scale@ (the number of nodes) of the service.
-- * 'serviceName' - The name of the container service to update.
mkUpdateContainerService ::
  -- | 'serviceName'
  Lude.Text ->
  UpdateContainerService
mkUpdateContainerService pServiceName_ =
  UpdateContainerService'
    { scale = Lude.Nothing,
      power = Lude.Nothing,
      isDisabled = Lude.Nothing,
      publicDomainNames = Lude.Nothing,
      serviceName = pServiceName_
    }

-- | The scale for the container service.
--
-- The scale specifies the allocated compute nodes of the container service. The @power@ and @scale@ of a container service makes up its configured capacity. To determine the monthly price of your container service, multiply the base price of the @power@ with the @scale@ (the number of nodes) of the service.
--
-- /Note:/ Consider using 'scale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsScale :: Lens.Lens' UpdateContainerService (Lude.Maybe Lude.Natural)
ucsScale = Lens.lens (scale :: UpdateContainerService -> Lude.Maybe Lude.Natural) (\s a -> s {scale = a} :: UpdateContainerService)
{-# DEPRECATED ucsScale "Use generic-lens or generic-optics with 'scale' instead." #-}

-- | The power for the container service.
--
-- The power specifies the amount of memory, vCPUs, and base monthly cost of each node of the container service. The @power@ and @scale@ of a container service makes up its configured capacity. To determine the monthly price of your container service, multiply the base price of the @power@ with the @scale@ (the number of nodes) of the service.
-- Use the @GetContainerServicePowers@ action to view the specifications of each power option.
--
-- /Note:/ Consider using 'power' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsPower :: Lens.Lens' UpdateContainerService (Lude.Maybe ContainerServicePowerName)
ucsPower = Lens.lens (power :: UpdateContainerService -> Lude.Maybe ContainerServicePowerName) (\s a -> s {power = a} :: UpdateContainerService)
{-# DEPRECATED ucsPower "Use generic-lens or generic-optics with 'power' instead." #-}

-- | A Boolean value to indicate whether the container service is disabled.
--
-- /Note:/ Consider using 'isDisabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsIsDisabled :: Lens.Lens' UpdateContainerService (Lude.Maybe Lude.Bool)
ucsIsDisabled = Lens.lens (isDisabled :: UpdateContainerService -> Lude.Maybe Lude.Bool) (\s a -> s {isDisabled = a} :: UpdateContainerService)
{-# DEPRECATED ucsIsDisabled "Use generic-lens or generic-optics with 'isDisabled' instead." #-}

-- | The public domain names to use with the container service, such as @example.com@ and @www.example.com@ .
--
-- You can specify up to four public domain names for a container service. The domain names that you specify are used when you create a deployment with a container configured as the public endpoint of your container service.
-- If you don't specify public domain names, then you can use the default domain of the container service.
-- /Important:/ You must create and validate an SSL/TLS certificate before you can use public domain names with your container service. Use the @CreateCertificate@ action to create a certificate for the public domain names you want to use with your container service.
-- You can specify public domain names using a string to array map as shown in the example later on this page.
--
-- /Note:/ Consider using 'publicDomainNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsPublicDomainNames :: Lens.Lens' UpdateContainerService (Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])))
ucsPublicDomainNames = Lens.lens (publicDomainNames :: UpdateContainerService -> Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text]))) (\s a -> s {publicDomainNames = a} :: UpdateContainerService)
{-# DEPRECATED ucsPublicDomainNames "Use generic-lens or generic-optics with 'publicDomainNames' instead." #-}

-- | The name of the container service to update.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsServiceName :: Lens.Lens' UpdateContainerService Lude.Text
ucsServiceName = Lens.lens (serviceName :: UpdateContainerService -> Lude.Text) (\s a -> s {serviceName = a} :: UpdateContainerService)
{-# DEPRECATED ucsServiceName "Use generic-lens or generic-optics with 'serviceName' instead." #-}

instance Lude.AWSRequest UpdateContainerService where
  type Rs UpdateContainerService = UpdateContainerServiceResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateContainerServiceResponse'
            Lude.<$> (x Lude..?> "containerService")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateContainerService where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.UpdateContainerService" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateContainerService where
  toJSON UpdateContainerService' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("scale" Lude..=) Lude.<$> scale,
            ("power" Lude..=) Lude.<$> power,
            ("isDisabled" Lude..=) Lude.<$> isDisabled,
            ("publicDomainNames" Lude..=) Lude.<$> publicDomainNames,
            Lude.Just ("serviceName" Lude..= serviceName)
          ]
      )

instance Lude.ToPath UpdateContainerService where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateContainerService where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateContainerServiceResponse' smart constructor.
data UpdateContainerServiceResponse = UpdateContainerServiceResponse'
  { containerService ::
      Lude.Maybe ContainerService,
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

-- | Creates a value of 'UpdateContainerServiceResponse' with the minimum fields required to make a request.
--
-- * 'containerService' - An object that describes a container service.
-- * 'responseStatus' - The response status code.
mkUpdateContainerServiceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateContainerServiceResponse
mkUpdateContainerServiceResponse pResponseStatus_ =
  UpdateContainerServiceResponse'
    { containerService = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An object that describes a container service.
--
-- /Note:/ Consider using 'containerService' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsrsContainerService :: Lens.Lens' UpdateContainerServiceResponse (Lude.Maybe ContainerService)
ucsrsContainerService = Lens.lens (containerService :: UpdateContainerServiceResponse -> Lude.Maybe ContainerService) (\s a -> s {containerService = a} :: UpdateContainerServiceResponse)
{-# DEPRECATED ucsrsContainerService "Use generic-lens or generic-optics with 'containerService' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsrsResponseStatus :: Lens.Lens' UpdateContainerServiceResponse Lude.Int
ucsrsResponseStatus = Lens.lens (responseStatus :: UpdateContainerServiceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateContainerServiceResponse)
{-# DEPRECATED ucsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
