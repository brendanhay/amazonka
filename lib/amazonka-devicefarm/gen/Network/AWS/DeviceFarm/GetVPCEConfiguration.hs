{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.GetVPCEConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the configuration settings for your Amazon Virtual Private Cloud (VPC) endpoint.
module Network.AWS.DeviceFarm.GetVPCEConfiguration
  ( -- * Creating a request
    GetVPCEConfiguration (..),
    mkGetVPCEConfiguration,

    -- ** Request lenses
    gvecArn,

    -- * Destructuring the response
    GetVPCEConfigurationResponse (..),
    mkGetVPCEConfigurationResponse,

    -- ** Response lenses
    gvecrsVpceConfiguration,
    gvecrsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetVPCEConfiguration' smart constructor.
newtype GetVPCEConfiguration = GetVPCEConfiguration'
  { arn ::
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

-- | Creates a value of 'GetVPCEConfiguration' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the VPC endpoint configuration you want to describe.
mkGetVPCEConfiguration ::
  -- | 'arn'
  Lude.Text ->
  GetVPCEConfiguration
mkGetVPCEConfiguration pArn_ = GetVPCEConfiguration' {arn = pArn_}

-- | The Amazon Resource Name (ARN) of the VPC endpoint configuration you want to describe.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvecArn :: Lens.Lens' GetVPCEConfiguration Lude.Text
gvecArn = Lens.lens (arn :: GetVPCEConfiguration -> Lude.Text) (\s a -> s {arn = a} :: GetVPCEConfiguration)
{-# DEPRECATED gvecArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Lude.AWSRequest GetVPCEConfiguration where
  type Rs GetVPCEConfiguration = GetVPCEConfigurationResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetVPCEConfigurationResponse'
            Lude.<$> (x Lude..?> "vpceConfiguration")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetVPCEConfiguration where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.GetVPCEConfiguration" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetVPCEConfiguration where
  toJSON GetVPCEConfiguration' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("arn" Lude..= arn)])

instance Lude.ToPath GetVPCEConfiguration where
  toPath = Lude.const "/"

instance Lude.ToQuery GetVPCEConfiguration where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetVPCEConfigurationResponse' smart constructor.
data GetVPCEConfigurationResponse = GetVPCEConfigurationResponse'
  { vpceConfiguration ::
      Lude.Maybe VPCEConfiguration,
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

-- | Creates a value of 'GetVPCEConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'vpceConfiguration' - An object that contains information about your VPC endpoint configuration.
mkGetVPCEConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetVPCEConfigurationResponse
mkGetVPCEConfigurationResponse pResponseStatus_ =
  GetVPCEConfigurationResponse'
    { vpceConfiguration = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An object that contains information about your VPC endpoint configuration.
--
-- /Note:/ Consider using 'vpceConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvecrsVpceConfiguration :: Lens.Lens' GetVPCEConfigurationResponse (Lude.Maybe VPCEConfiguration)
gvecrsVpceConfiguration = Lens.lens (vpceConfiguration :: GetVPCEConfigurationResponse -> Lude.Maybe VPCEConfiguration) (\s a -> s {vpceConfiguration = a} :: GetVPCEConfigurationResponse)
{-# DEPRECATED gvecrsVpceConfiguration "Use generic-lens or generic-optics with 'vpceConfiguration' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvecrsResponseStatus :: Lens.Lens' GetVPCEConfigurationResponse Lude.Int
gvecrsResponseStatus = Lens.lens (responseStatus :: GetVPCEConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetVPCEConfigurationResponse)
{-# DEPRECATED gvecrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
