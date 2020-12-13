{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.CreateVPCEConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a configuration record in Device Farm for your Amazon Virtual Private Cloud (VPC) endpoint.
module Network.AWS.DeviceFarm.CreateVPCEConfiguration
  ( -- * Creating a request
    CreateVPCEConfiguration (..),
    mkCreateVPCEConfiguration,

    -- ** Request lenses
    cvecVpceServiceName,
    cvecVpceConfigurationName,
    cvecServiceDNSName,
    cvecVpceConfigurationDescription,

    -- * Destructuring the response
    CreateVPCEConfigurationResponse (..),
    mkCreateVPCEConfigurationResponse,

    -- ** Response lenses
    cvecrsVpceConfiguration,
    cvecrsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateVPCEConfiguration' smart constructor.
data CreateVPCEConfiguration = CreateVPCEConfiguration'
  { -- | The name of the VPC endpoint service running in your AWS account that you want Device Farm to test.
    vpceServiceName :: Lude.Text,
    -- | The friendly name you give to your VPC endpoint configuration, to manage your configurations more easily.
    vpceConfigurationName :: Lude.Text,
    -- | The DNS name of the service running in your VPC that you want Device Farm to test.
    serviceDNSName :: Lude.Text,
    -- | An optional description that provides details about your VPC endpoint configuration.
    vpceConfigurationDescription :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateVPCEConfiguration' with the minimum fields required to make a request.
--
-- * 'vpceServiceName' - The name of the VPC endpoint service running in your AWS account that you want Device Farm to test.
-- * 'vpceConfigurationName' - The friendly name you give to your VPC endpoint configuration, to manage your configurations more easily.
-- * 'serviceDNSName' - The DNS name of the service running in your VPC that you want Device Farm to test.
-- * 'vpceConfigurationDescription' - An optional description that provides details about your VPC endpoint configuration.
mkCreateVPCEConfiguration ::
  -- | 'vpceServiceName'
  Lude.Text ->
  -- | 'vpceConfigurationName'
  Lude.Text ->
  -- | 'serviceDNSName'
  Lude.Text ->
  CreateVPCEConfiguration
mkCreateVPCEConfiguration
  pVpceServiceName_
  pVpceConfigurationName_
  pServiceDNSName_ =
    CreateVPCEConfiguration'
      { vpceServiceName = pVpceServiceName_,
        vpceConfigurationName = pVpceConfigurationName_,
        serviceDNSName = pServiceDNSName_,
        vpceConfigurationDescription = Lude.Nothing
      }

-- | The name of the VPC endpoint service running in your AWS account that you want Device Farm to test.
--
-- /Note:/ Consider using 'vpceServiceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvecVpceServiceName :: Lens.Lens' CreateVPCEConfiguration Lude.Text
cvecVpceServiceName = Lens.lens (vpceServiceName :: CreateVPCEConfiguration -> Lude.Text) (\s a -> s {vpceServiceName = a} :: CreateVPCEConfiguration)
{-# DEPRECATED cvecVpceServiceName "Use generic-lens or generic-optics with 'vpceServiceName' instead." #-}

-- | The friendly name you give to your VPC endpoint configuration, to manage your configurations more easily.
--
-- /Note:/ Consider using 'vpceConfigurationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvecVpceConfigurationName :: Lens.Lens' CreateVPCEConfiguration Lude.Text
cvecVpceConfigurationName = Lens.lens (vpceConfigurationName :: CreateVPCEConfiguration -> Lude.Text) (\s a -> s {vpceConfigurationName = a} :: CreateVPCEConfiguration)
{-# DEPRECATED cvecVpceConfigurationName "Use generic-lens or generic-optics with 'vpceConfigurationName' instead." #-}

-- | The DNS name of the service running in your VPC that you want Device Farm to test.
--
-- /Note:/ Consider using 'serviceDNSName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvecServiceDNSName :: Lens.Lens' CreateVPCEConfiguration Lude.Text
cvecServiceDNSName = Lens.lens (serviceDNSName :: CreateVPCEConfiguration -> Lude.Text) (\s a -> s {serviceDNSName = a} :: CreateVPCEConfiguration)
{-# DEPRECATED cvecServiceDNSName "Use generic-lens or generic-optics with 'serviceDNSName' instead." #-}

-- | An optional description that provides details about your VPC endpoint configuration.
--
-- /Note:/ Consider using 'vpceConfigurationDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvecVpceConfigurationDescription :: Lens.Lens' CreateVPCEConfiguration (Lude.Maybe Lude.Text)
cvecVpceConfigurationDescription = Lens.lens (vpceConfigurationDescription :: CreateVPCEConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {vpceConfigurationDescription = a} :: CreateVPCEConfiguration)
{-# DEPRECATED cvecVpceConfigurationDescription "Use generic-lens or generic-optics with 'vpceConfigurationDescription' instead." #-}

instance Lude.AWSRequest CreateVPCEConfiguration where
  type Rs CreateVPCEConfiguration = CreateVPCEConfigurationResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateVPCEConfigurationResponse'
            Lude.<$> (x Lude..?> "vpceConfiguration")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateVPCEConfiguration where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.CreateVPCEConfiguration" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateVPCEConfiguration where
  toJSON CreateVPCEConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("vpceServiceName" Lude..= vpceServiceName),
            Lude.Just ("vpceConfigurationName" Lude..= vpceConfigurationName),
            Lude.Just ("serviceDnsName" Lude..= serviceDNSName),
            ("vpceConfigurationDescription" Lude..=)
              Lude.<$> vpceConfigurationDescription
          ]
      )

instance Lude.ToPath CreateVPCEConfiguration where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateVPCEConfiguration where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateVPCEConfigurationResponse' smart constructor.
data CreateVPCEConfigurationResponse = CreateVPCEConfigurationResponse'
  { -- | An object that contains information about your VPC endpoint configuration.
    vpceConfiguration :: Lude.Maybe VPCEConfiguration,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateVPCEConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'vpceConfiguration' - An object that contains information about your VPC endpoint configuration.
-- * 'responseStatus' - The response status code.
mkCreateVPCEConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateVPCEConfigurationResponse
mkCreateVPCEConfigurationResponse pResponseStatus_ =
  CreateVPCEConfigurationResponse'
    { vpceConfiguration =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An object that contains information about your VPC endpoint configuration.
--
-- /Note:/ Consider using 'vpceConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvecrsVpceConfiguration :: Lens.Lens' CreateVPCEConfigurationResponse (Lude.Maybe VPCEConfiguration)
cvecrsVpceConfiguration = Lens.lens (vpceConfiguration :: CreateVPCEConfigurationResponse -> Lude.Maybe VPCEConfiguration) (\s a -> s {vpceConfiguration = a} :: CreateVPCEConfigurationResponse)
{-# DEPRECATED cvecrsVpceConfiguration "Use generic-lens or generic-optics with 'vpceConfiguration' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvecrsResponseStatus :: Lens.Lens' CreateVPCEConfigurationResponse Lude.Int
cvecrsResponseStatus = Lens.lens (responseStatus :: CreateVPCEConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateVPCEConfigurationResponse)
{-# DEPRECATED cvecrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
