{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyVPCAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified attribute of the specified VPC.
module Network.AWS.EC2.ModifyVPCAttribute
  ( -- * Creating a request
    ModifyVPCAttribute (..),
    mkModifyVPCAttribute,

    -- ** Request lenses
    mvaEnableDNSHostnames,
    mvaEnableDNSSupport,
    mvaVPCId,

    -- * Destructuring the response
    ModifyVPCAttributeResponse (..),
    mkModifyVPCAttributeResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyVPCAttribute' smart constructor.
data ModifyVPCAttribute = ModifyVPCAttribute'
  { -- | Indicates whether the instances launched in the VPC get DNS hostnames. If enabled, instances in the VPC get DNS hostnames; otherwise, they do not.
    --
    -- You cannot modify the DNS resolution and DNS hostnames attributes in the same request. Use separate requests for each attribute. You can only enable DNS hostnames if you've enabled DNS support.
    enableDNSHostnames :: Lude.Maybe AttributeBooleanValue,
    -- | Indicates whether the DNS resolution is supported for the VPC. If enabled, queries to the Amazon provided DNS server at the 169.254.169.253 IP address, or the reserved IP address at the base of the VPC network range "plus two" succeed. If disabled, the Amazon provided DNS service in the VPC that resolves public DNS hostnames to IP addresses is not enabled.
    --
    -- You cannot modify the DNS resolution and DNS hostnames attributes in the same request. Use separate requests for each attribute.
    enableDNSSupport :: Lude.Maybe AttributeBooleanValue,
    -- | The ID of the VPC.
    vpcId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyVPCAttribute' with the minimum fields required to make a request.
--
-- * 'enableDNSHostnames' - Indicates whether the instances launched in the VPC get DNS hostnames. If enabled, instances in the VPC get DNS hostnames; otherwise, they do not.
--
-- You cannot modify the DNS resolution and DNS hostnames attributes in the same request. Use separate requests for each attribute. You can only enable DNS hostnames if you've enabled DNS support.
-- * 'enableDNSSupport' - Indicates whether the DNS resolution is supported for the VPC. If enabled, queries to the Amazon provided DNS server at the 169.254.169.253 IP address, or the reserved IP address at the base of the VPC network range "plus two" succeed. If disabled, the Amazon provided DNS service in the VPC that resolves public DNS hostnames to IP addresses is not enabled.
--
-- You cannot modify the DNS resolution and DNS hostnames attributes in the same request. Use separate requests for each attribute.
-- * 'vpcId' - The ID of the VPC.
mkModifyVPCAttribute ::
  -- | 'vpcId'
  Lude.Text ->
  ModifyVPCAttribute
mkModifyVPCAttribute pVPCId_ =
  ModifyVPCAttribute'
    { enableDNSHostnames = Lude.Nothing,
      enableDNSSupport = Lude.Nothing,
      vpcId = pVPCId_
    }

-- | Indicates whether the instances launched in the VPC get DNS hostnames. If enabled, instances in the VPC get DNS hostnames; otherwise, they do not.
--
-- You cannot modify the DNS resolution and DNS hostnames attributes in the same request. Use separate requests for each attribute. You can only enable DNS hostnames if you've enabled DNS support.
--
-- /Note:/ Consider using 'enableDNSHostnames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvaEnableDNSHostnames :: Lens.Lens' ModifyVPCAttribute (Lude.Maybe AttributeBooleanValue)
mvaEnableDNSHostnames = Lens.lens (enableDNSHostnames :: ModifyVPCAttribute -> Lude.Maybe AttributeBooleanValue) (\s a -> s {enableDNSHostnames = a} :: ModifyVPCAttribute)
{-# DEPRECATED mvaEnableDNSHostnames "Use generic-lens or generic-optics with 'enableDNSHostnames' instead." #-}

-- | Indicates whether the DNS resolution is supported for the VPC. If enabled, queries to the Amazon provided DNS server at the 169.254.169.253 IP address, or the reserved IP address at the base of the VPC network range "plus two" succeed. If disabled, the Amazon provided DNS service in the VPC that resolves public DNS hostnames to IP addresses is not enabled.
--
-- You cannot modify the DNS resolution and DNS hostnames attributes in the same request. Use separate requests for each attribute.
--
-- /Note:/ Consider using 'enableDNSSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvaEnableDNSSupport :: Lens.Lens' ModifyVPCAttribute (Lude.Maybe AttributeBooleanValue)
mvaEnableDNSSupport = Lens.lens (enableDNSSupport :: ModifyVPCAttribute -> Lude.Maybe AttributeBooleanValue) (\s a -> s {enableDNSSupport = a} :: ModifyVPCAttribute)
{-# DEPRECATED mvaEnableDNSSupport "Use generic-lens or generic-optics with 'enableDNSSupport' instead." #-}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvaVPCId :: Lens.Lens' ModifyVPCAttribute Lude.Text
mvaVPCId = Lens.lens (vpcId :: ModifyVPCAttribute -> Lude.Text) (\s a -> s {vpcId = a} :: ModifyVPCAttribute)
{-# DEPRECATED mvaVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

instance Lude.AWSRequest ModifyVPCAttribute where
  type Rs ModifyVPCAttribute = ModifyVPCAttributeResponse
  request = Req.postQuery ec2Service
  response = Res.receiveNull ModifyVPCAttributeResponse'

instance Lude.ToHeaders ModifyVPCAttribute where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyVPCAttribute where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyVPCAttribute where
  toQuery ModifyVPCAttribute' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ModifyVpcAttribute" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "EnableDnsHostnames" Lude.=: enableDNSHostnames,
        "EnableDnsSupport" Lude.=: enableDNSSupport,
        "VpcId" Lude.=: vpcId
      ]

-- | /See:/ 'mkModifyVPCAttributeResponse' smart constructor.
data ModifyVPCAttributeResponse = ModifyVPCAttributeResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyVPCAttributeResponse' with the minimum fields required to make a request.
mkModifyVPCAttributeResponse ::
  ModifyVPCAttributeResponse
mkModifyVPCAttributeResponse = ModifyVPCAttributeResponse'
