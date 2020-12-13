{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.CreateCustomAvailabilityZone
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a custom Availability Zone (AZ).
--
-- A custom AZ is an on-premises AZ that is integrated with a VMware vSphere cluster.
-- For more information about RDS on VMware, see the <https://docs.aws.amazon.com/AmazonRDS/latest/RDSonVMwareUserGuide/rds-on-vmware.html /RDS on VMware User Guide./ >
module Network.AWS.RDS.CreateCustomAvailabilityZone
  ( -- * Creating a request
    CreateCustomAvailabilityZone (..),
    mkCreateCustomAvailabilityZone,

    -- ** Request lenses
    ccazVPNTunnelOriginatorIP,
    ccazCustomAvailabilityZoneName,
    ccazNewVPNTunnelName,
    ccazExistingVPNId,

    -- * Destructuring the response
    CreateCustomAvailabilityZoneResponse (..),
    mkCreateCustomAvailabilityZoneResponse,

    -- ** Response lenses
    ccazrsCustomAvailabilityZone,
    ccazrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkCreateCustomAvailabilityZone' smart constructor.
data CreateCustomAvailabilityZone = CreateCustomAvailabilityZone'
  { -- | The IP address of network traffic from your on-premises data center. A custom AZ receives the network traffic.
    --
    -- Specify this parameter only if @ExistingVpnId@ isn't specified.
    vpnTunnelOriginatorIP :: Lude.Maybe Lude.Text,
    -- | The name of the custom Availability Zone (AZ).
    customAvailabilityZoneName :: Lude.Text,
    -- | The name of a new VPN tunnel between the Amazon RDS website and the VMware vSphere cluster.
    --
    -- Specify this parameter only if @ExistingVpnId@ isn't specified.
    newVPNTunnelName :: Lude.Maybe Lude.Text,
    -- | The ID of an existing virtual private network (VPN) between the Amazon RDS website and the VMware vSphere cluster.
    existingVPNId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCustomAvailabilityZone' with the minimum fields required to make a request.
--
-- * 'vpnTunnelOriginatorIP' - The IP address of network traffic from your on-premises data center. A custom AZ receives the network traffic.
--
-- Specify this parameter only if @ExistingVpnId@ isn't specified.
-- * 'customAvailabilityZoneName' - The name of the custom Availability Zone (AZ).
-- * 'newVPNTunnelName' - The name of a new VPN tunnel between the Amazon RDS website and the VMware vSphere cluster.
--
-- Specify this parameter only if @ExistingVpnId@ isn't specified.
-- * 'existingVPNId' - The ID of an existing virtual private network (VPN) between the Amazon RDS website and the VMware vSphere cluster.
mkCreateCustomAvailabilityZone ::
  -- | 'customAvailabilityZoneName'
  Lude.Text ->
  CreateCustomAvailabilityZone
mkCreateCustomAvailabilityZone pCustomAvailabilityZoneName_ =
  CreateCustomAvailabilityZone'
    { vpnTunnelOriginatorIP =
        Lude.Nothing,
      customAvailabilityZoneName = pCustomAvailabilityZoneName_,
      newVPNTunnelName = Lude.Nothing,
      existingVPNId = Lude.Nothing
    }

-- | The IP address of network traffic from your on-premises data center. A custom AZ receives the network traffic.
--
-- Specify this parameter only if @ExistingVpnId@ isn't specified.
--
-- /Note:/ Consider using 'vpnTunnelOriginatorIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccazVPNTunnelOriginatorIP :: Lens.Lens' CreateCustomAvailabilityZone (Lude.Maybe Lude.Text)
ccazVPNTunnelOriginatorIP = Lens.lens (vpnTunnelOriginatorIP :: CreateCustomAvailabilityZone -> Lude.Maybe Lude.Text) (\s a -> s {vpnTunnelOriginatorIP = a} :: CreateCustomAvailabilityZone)
{-# DEPRECATED ccazVPNTunnelOriginatorIP "Use generic-lens or generic-optics with 'vpnTunnelOriginatorIP' instead." #-}

-- | The name of the custom Availability Zone (AZ).
--
-- /Note:/ Consider using 'customAvailabilityZoneName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccazCustomAvailabilityZoneName :: Lens.Lens' CreateCustomAvailabilityZone Lude.Text
ccazCustomAvailabilityZoneName = Lens.lens (customAvailabilityZoneName :: CreateCustomAvailabilityZone -> Lude.Text) (\s a -> s {customAvailabilityZoneName = a} :: CreateCustomAvailabilityZone)
{-# DEPRECATED ccazCustomAvailabilityZoneName "Use generic-lens or generic-optics with 'customAvailabilityZoneName' instead." #-}

-- | The name of a new VPN tunnel between the Amazon RDS website and the VMware vSphere cluster.
--
-- Specify this parameter only if @ExistingVpnId@ isn't specified.
--
-- /Note:/ Consider using 'newVPNTunnelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccazNewVPNTunnelName :: Lens.Lens' CreateCustomAvailabilityZone (Lude.Maybe Lude.Text)
ccazNewVPNTunnelName = Lens.lens (newVPNTunnelName :: CreateCustomAvailabilityZone -> Lude.Maybe Lude.Text) (\s a -> s {newVPNTunnelName = a} :: CreateCustomAvailabilityZone)
{-# DEPRECATED ccazNewVPNTunnelName "Use generic-lens or generic-optics with 'newVPNTunnelName' instead." #-}

-- | The ID of an existing virtual private network (VPN) between the Amazon RDS website and the VMware vSphere cluster.
--
-- /Note:/ Consider using 'existingVPNId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccazExistingVPNId :: Lens.Lens' CreateCustomAvailabilityZone (Lude.Maybe Lude.Text)
ccazExistingVPNId = Lens.lens (existingVPNId :: CreateCustomAvailabilityZone -> Lude.Maybe Lude.Text) (\s a -> s {existingVPNId = a} :: CreateCustomAvailabilityZone)
{-# DEPRECATED ccazExistingVPNId "Use generic-lens or generic-optics with 'existingVPNId' instead." #-}

instance Lude.AWSRequest CreateCustomAvailabilityZone where
  type
    Rs CreateCustomAvailabilityZone =
      CreateCustomAvailabilityZoneResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "CreateCustomAvailabilityZoneResult"
      ( \s h x ->
          CreateCustomAvailabilityZoneResponse'
            Lude.<$> (x Lude..@? "CustomAvailabilityZone")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateCustomAvailabilityZone where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateCustomAvailabilityZone where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateCustomAvailabilityZone where
  toQuery CreateCustomAvailabilityZone' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("CreateCustomAvailabilityZone" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "VpnTunnelOriginatorIP" Lude.=: vpnTunnelOriginatorIP,
        "CustomAvailabilityZoneName" Lude.=: customAvailabilityZoneName,
        "NewVpnTunnelName" Lude.=: newVPNTunnelName,
        "ExistingVpnId" Lude.=: existingVPNId
      ]

-- | /See:/ 'mkCreateCustomAvailabilityZoneResponse' smart constructor.
data CreateCustomAvailabilityZoneResponse = CreateCustomAvailabilityZoneResponse'
  { customAvailabilityZone :: Lude.Maybe CustomAvailabilityZone,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCustomAvailabilityZoneResponse' with the minimum fields required to make a request.
--
-- * 'customAvailabilityZone' -
-- * 'responseStatus' - The response status code.
mkCreateCustomAvailabilityZoneResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateCustomAvailabilityZoneResponse
mkCreateCustomAvailabilityZoneResponse pResponseStatus_ =
  CreateCustomAvailabilityZoneResponse'
    { customAvailabilityZone =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'customAvailabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccazrsCustomAvailabilityZone :: Lens.Lens' CreateCustomAvailabilityZoneResponse (Lude.Maybe CustomAvailabilityZone)
ccazrsCustomAvailabilityZone = Lens.lens (customAvailabilityZone :: CreateCustomAvailabilityZoneResponse -> Lude.Maybe CustomAvailabilityZone) (\s a -> s {customAvailabilityZone = a} :: CreateCustomAvailabilityZoneResponse)
{-# DEPRECATED ccazrsCustomAvailabilityZone "Use generic-lens or generic-optics with 'customAvailabilityZone' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccazrsResponseStatus :: Lens.Lens' CreateCustomAvailabilityZoneResponse Lude.Int
ccazrsResponseStatus = Lens.lens (responseStatus :: CreateCustomAvailabilityZoneResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateCustomAvailabilityZoneResponse)
{-# DEPRECATED ccazrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
