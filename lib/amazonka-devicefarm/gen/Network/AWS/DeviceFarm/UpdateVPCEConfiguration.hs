{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.UpdateVPCEConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates information about an Amazon Virtual Private Cloud (VPC) endpoint configuration.
module Network.AWS.DeviceFarm.UpdateVPCEConfiguration
  ( -- * Creating a request
    UpdateVPCEConfiguration (..),
    mkUpdateVPCEConfiguration,

    -- ** Request lenses
    uvecVpceServiceName,
    uvecVpceConfigurationName,
    uvecServiceDNSName,
    uvecVpceConfigurationDescription,
    uvecArn,

    -- * Destructuring the response
    UpdateVPCEConfigurationResponse (..),
    mkUpdateVPCEConfigurationResponse,

    -- ** Response lenses
    uvecrsVpceConfiguration,
    uvecrsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateVPCEConfiguration' smart constructor.
data UpdateVPCEConfiguration = UpdateVPCEConfiguration'
  { vpceServiceName ::
      Lude.Maybe Lude.Text,
    vpceConfigurationName ::
      Lude.Maybe Lude.Text,
    serviceDNSName :: Lude.Maybe Lude.Text,
    vpceConfigurationDescription ::
      Lude.Maybe Lude.Text,
    arn :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateVPCEConfiguration' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the VPC endpoint configuration you want to update.
-- * 'serviceDNSName' - The DNS (domain) name used to connect to your private service in your VPC. The DNS name must not already be in use on the internet.
-- * 'vpceConfigurationDescription' - An optional description that provides details about your VPC endpoint configuration.
-- * 'vpceConfigurationName' - The friendly name you give to your VPC endpoint configuration to manage your configurations more easily.
-- * 'vpceServiceName' - The name of the VPC endpoint service running in your AWS account that you want Device Farm to test.
mkUpdateVPCEConfiguration ::
  -- | 'arn'
  Lude.Text ->
  UpdateVPCEConfiguration
mkUpdateVPCEConfiguration pArn_ =
  UpdateVPCEConfiguration'
    { vpceServiceName = Lude.Nothing,
      vpceConfigurationName = Lude.Nothing,
      serviceDNSName = Lude.Nothing,
      vpceConfigurationDescription = Lude.Nothing,
      arn = pArn_
    }

-- | The name of the VPC endpoint service running in your AWS account that you want Device Farm to test.
--
-- /Note:/ Consider using 'vpceServiceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvecVpceServiceName :: Lens.Lens' UpdateVPCEConfiguration (Lude.Maybe Lude.Text)
uvecVpceServiceName = Lens.lens (vpceServiceName :: UpdateVPCEConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {vpceServiceName = a} :: UpdateVPCEConfiguration)
{-# DEPRECATED uvecVpceServiceName "Use generic-lens or generic-optics with 'vpceServiceName' instead." #-}

-- | The friendly name you give to your VPC endpoint configuration to manage your configurations more easily.
--
-- /Note:/ Consider using 'vpceConfigurationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvecVpceConfigurationName :: Lens.Lens' UpdateVPCEConfiguration (Lude.Maybe Lude.Text)
uvecVpceConfigurationName = Lens.lens (vpceConfigurationName :: UpdateVPCEConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {vpceConfigurationName = a} :: UpdateVPCEConfiguration)
{-# DEPRECATED uvecVpceConfigurationName "Use generic-lens or generic-optics with 'vpceConfigurationName' instead." #-}

-- | The DNS (domain) name used to connect to your private service in your VPC. The DNS name must not already be in use on the internet.
--
-- /Note:/ Consider using 'serviceDNSName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvecServiceDNSName :: Lens.Lens' UpdateVPCEConfiguration (Lude.Maybe Lude.Text)
uvecServiceDNSName = Lens.lens (serviceDNSName :: UpdateVPCEConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {serviceDNSName = a} :: UpdateVPCEConfiguration)
{-# DEPRECATED uvecServiceDNSName "Use generic-lens or generic-optics with 'serviceDNSName' instead." #-}

-- | An optional description that provides details about your VPC endpoint configuration.
--
-- /Note:/ Consider using 'vpceConfigurationDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvecVpceConfigurationDescription :: Lens.Lens' UpdateVPCEConfiguration (Lude.Maybe Lude.Text)
uvecVpceConfigurationDescription = Lens.lens (vpceConfigurationDescription :: UpdateVPCEConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {vpceConfigurationDescription = a} :: UpdateVPCEConfiguration)
{-# DEPRECATED uvecVpceConfigurationDescription "Use generic-lens or generic-optics with 'vpceConfigurationDescription' instead." #-}

-- | The Amazon Resource Name (ARN) of the VPC endpoint configuration you want to update.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvecArn :: Lens.Lens' UpdateVPCEConfiguration Lude.Text
uvecArn = Lens.lens (arn :: UpdateVPCEConfiguration -> Lude.Text) (\s a -> s {arn = a} :: UpdateVPCEConfiguration)
{-# DEPRECATED uvecArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Lude.AWSRequest UpdateVPCEConfiguration where
  type Rs UpdateVPCEConfiguration = UpdateVPCEConfigurationResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateVPCEConfigurationResponse'
            Lude.<$> (x Lude..?> "vpceConfiguration")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateVPCEConfiguration where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.UpdateVPCEConfiguration" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateVPCEConfiguration where
  toJSON UpdateVPCEConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("vpceServiceName" Lude..=) Lude.<$> vpceServiceName,
            ("vpceConfigurationName" Lude..=) Lude.<$> vpceConfigurationName,
            ("serviceDnsName" Lude..=) Lude.<$> serviceDNSName,
            ("vpceConfigurationDescription" Lude..=)
              Lude.<$> vpceConfigurationDescription,
            Lude.Just ("arn" Lude..= arn)
          ]
      )

instance Lude.ToPath UpdateVPCEConfiguration where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateVPCEConfiguration where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateVPCEConfigurationResponse' smart constructor.
data UpdateVPCEConfigurationResponse = UpdateVPCEConfigurationResponse'
  { vpceConfiguration ::
      Lude.Maybe
        VPCEConfiguration,
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

-- | Creates a value of 'UpdateVPCEConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'vpceConfiguration' - An object that contains information about your VPC endpoint configuration.
mkUpdateVPCEConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateVPCEConfigurationResponse
mkUpdateVPCEConfigurationResponse pResponseStatus_ =
  UpdateVPCEConfigurationResponse'
    { vpceConfiguration =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An object that contains information about your VPC endpoint configuration.
--
-- /Note:/ Consider using 'vpceConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvecrsVpceConfiguration :: Lens.Lens' UpdateVPCEConfigurationResponse (Lude.Maybe VPCEConfiguration)
uvecrsVpceConfiguration = Lens.lens (vpceConfiguration :: UpdateVPCEConfigurationResponse -> Lude.Maybe VPCEConfiguration) (\s a -> s {vpceConfiguration = a} :: UpdateVPCEConfigurationResponse)
{-# DEPRECATED uvecrsVpceConfiguration "Use generic-lens or generic-optics with 'vpceConfiguration' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvecrsResponseStatus :: Lens.Lens' UpdateVPCEConfigurationResponse Lude.Int
uvecrsResponseStatus = Lens.lens (responseStatus :: UpdateVPCEConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateVPCEConfigurationResponse)
{-# DEPRECATED uvecrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
