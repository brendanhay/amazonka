{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeVPCAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified attribute of the specified VPC. You can specify only one attribute at a time.
module Network.AWS.EC2.DescribeVPCAttribute
  ( -- * Creating a request
    DescribeVPCAttribute (..),
    mkDescribeVPCAttribute,

    -- ** Request lenses
    dvpcaDryRun,
    dvpcaAttribute,
    dvpcaVPCId,

    -- * Destructuring the response
    DescribeVPCAttributeResponse (..),
    mkDescribeVPCAttributeResponse,

    -- ** Response lenses
    dvpcarsEnableDNSHostnames,
    dvpcarsEnableDNSSupport,
    dvpcarsVPCId,
    dvpcarsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeVPCAttribute' smart constructor.
data DescribeVPCAttribute = DescribeVPCAttribute'
  { dryRun ::
      Lude.Maybe Lude.Bool,
    attribute :: VPCAttributeName,
    vpcId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeVPCAttribute' with the minimum fields required to make a request.
--
-- * 'attribute' - The VPC attribute.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'vpcId' - The ID of the VPC.
mkDescribeVPCAttribute ::
  -- | 'attribute'
  VPCAttributeName ->
  -- | 'vpcId'
  Lude.Text ->
  DescribeVPCAttribute
mkDescribeVPCAttribute pAttribute_ pVPCId_ =
  DescribeVPCAttribute'
    { dryRun = Lude.Nothing,
      attribute = pAttribute_,
      vpcId = pVPCId_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcaDryRun :: Lens.Lens' DescribeVPCAttribute (Lude.Maybe Lude.Bool)
dvpcaDryRun = Lens.lens (dryRun :: DescribeVPCAttribute -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeVPCAttribute)
{-# DEPRECATED dvpcaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The VPC attribute.
--
-- /Note:/ Consider using 'attribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcaAttribute :: Lens.Lens' DescribeVPCAttribute VPCAttributeName
dvpcaAttribute = Lens.lens (attribute :: DescribeVPCAttribute -> VPCAttributeName) (\s a -> s {attribute = a} :: DescribeVPCAttribute)
{-# DEPRECATED dvpcaAttribute "Use generic-lens or generic-optics with 'attribute' instead." #-}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcaVPCId :: Lens.Lens' DescribeVPCAttribute Lude.Text
dvpcaVPCId = Lens.lens (vpcId :: DescribeVPCAttribute -> Lude.Text) (\s a -> s {vpcId = a} :: DescribeVPCAttribute)
{-# DEPRECATED dvpcaVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

instance Lude.AWSRequest DescribeVPCAttribute where
  type Rs DescribeVPCAttribute = DescribeVPCAttributeResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeVPCAttributeResponse'
            Lude.<$> (x Lude..@? "enableDnsHostnames")
            Lude.<*> (x Lude..@? "enableDnsSupport")
            Lude.<*> (x Lude..@? "vpcId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeVPCAttribute where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeVPCAttribute where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeVPCAttribute where
  toQuery DescribeVPCAttribute' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeVpcAttribute" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        "Attribute" Lude.=: attribute,
        "VpcId" Lude.=: vpcId
      ]

-- | /See:/ 'mkDescribeVPCAttributeResponse' smart constructor.
data DescribeVPCAttributeResponse = DescribeVPCAttributeResponse'
  { enableDNSHostnames ::
      Lude.Maybe AttributeBooleanValue,
    enableDNSSupport ::
      Lude.Maybe AttributeBooleanValue,
    vpcId :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DescribeVPCAttributeResponse' with the minimum fields required to make a request.
--
-- * 'enableDNSHostnames' - Indicates whether the instances launched in the VPC get DNS hostnames. If this attribute is @true@ , instances in the VPC get DNS hostnames; otherwise, they do not.
-- * 'enableDNSSupport' - Indicates whether DNS resolution is enabled for the VPC. If this attribute is @true@ , the Amazon DNS server resolves DNS hostnames for your instances to their corresponding IP addresses; otherwise, it does not.
-- * 'responseStatus' - The response status code.
-- * 'vpcId' - The ID of the VPC.
mkDescribeVPCAttributeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeVPCAttributeResponse
mkDescribeVPCAttributeResponse pResponseStatus_ =
  DescribeVPCAttributeResponse'
    { enableDNSHostnames = Lude.Nothing,
      enableDNSSupport = Lude.Nothing,
      vpcId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Indicates whether the instances launched in the VPC get DNS hostnames. If this attribute is @true@ , instances in the VPC get DNS hostnames; otherwise, they do not.
--
-- /Note:/ Consider using 'enableDNSHostnames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcarsEnableDNSHostnames :: Lens.Lens' DescribeVPCAttributeResponse (Lude.Maybe AttributeBooleanValue)
dvpcarsEnableDNSHostnames = Lens.lens (enableDNSHostnames :: DescribeVPCAttributeResponse -> Lude.Maybe AttributeBooleanValue) (\s a -> s {enableDNSHostnames = a} :: DescribeVPCAttributeResponse)
{-# DEPRECATED dvpcarsEnableDNSHostnames "Use generic-lens or generic-optics with 'enableDNSHostnames' instead." #-}

-- | Indicates whether DNS resolution is enabled for the VPC. If this attribute is @true@ , the Amazon DNS server resolves DNS hostnames for your instances to their corresponding IP addresses; otherwise, it does not.
--
-- /Note:/ Consider using 'enableDNSSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcarsEnableDNSSupport :: Lens.Lens' DescribeVPCAttributeResponse (Lude.Maybe AttributeBooleanValue)
dvpcarsEnableDNSSupport = Lens.lens (enableDNSSupport :: DescribeVPCAttributeResponse -> Lude.Maybe AttributeBooleanValue) (\s a -> s {enableDNSSupport = a} :: DescribeVPCAttributeResponse)
{-# DEPRECATED dvpcarsEnableDNSSupport "Use generic-lens or generic-optics with 'enableDNSSupport' instead." #-}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcarsVPCId :: Lens.Lens' DescribeVPCAttributeResponse (Lude.Maybe Lude.Text)
dvpcarsVPCId = Lens.lens (vpcId :: DescribeVPCAttributeResponse -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: DescribeVPCAttributeResponse)
{-# DEPRECATED dvpcarsVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcarsResponseStatus :: Lens.Lens' DescribeVPCAttributeResponse Lude.Int
dvpcarsResponseStatus = Lens.lens (responseStatus :: DescribeVPCAttributeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeVPCAttributeResponse)
{-# DEPRECATED dvpcarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
