{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeNetworkInterfaceAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a network interface attribute. You can specify only one attribute at a time.
module Network.AWS.EC2.DescribeNetworkInterfaceAttribute
  ( -- * Creating a request
    DescribeNetworkInterfaceAttribute (..),
    mkDescribeNetworkInterfaceAttribute,

    -- ** Request lenses
    dniaAttribute,
    dniaDryRun,
    dniaNetworkInterfaceId,

    -- * Destructuring the response
    DescribeNetworkInterfaceAttributeResponse (..),
    mkDescribeNetworkInterfaceAttributeResponse,

    -- ** Response lenses
    dniarsGroups,
    dniarsSourceDestCheck,
    dniarsNetworkInterfaceId,
    dniarsAttachment,
    dniarsDescription,
    dniarsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for DescribeNetworkInterfaceAttribute.
--
-- /See:/ 'mkDescribeNetworkInterfaceAttribute' smart constructor.
data DescribeNetworkInterfaceAttribute = DescribeNetworkInterfaceAttribute'
  { attribute ::
      Lude.Maybe
        NetworkInterfaceAttribute,
    dryRun ::
      Lude.Maybe Lude.Bool,
    networkInterfaceId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeNetworkInterfaceAttribute' with the minimum fields required to make a request.
--
-- * 'attribute' - The attribute of the network interface. This parameter is required.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'networkInterfaceId' - The ID of the network interface.
mkDescribeNetworkInterfaceAttribute ::
  -- | 'networkInterfaceId'
  Lude.Text ->
  DescribeNetworkInterfaceAttribute
mkDescribeNetworkInterfaceAttribute pNetworkInterfaceId_ =
  DescribeNetworkInterfaceAttribute'
    { attribute = Lude.Nothing,
      dryRun = Lude.Nothing,
      networkInterfaceId = pNetworkInterfaceId_
    }

-- | The attribute of the network interface. This parameter is required.
--
-- /Note:/ Consider using 'attribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dniaAttribute :: Lens.Lens' DescribeNetworkInterfaceAttribute (Lude.Maybe NetworkInterfaceAttribute)
dniaAttribute = Lens.lens (attribute :: DescribeNetworkInterfaceAttribute -> Lude.Maybe NetworkInterfaceAttribute) (\s a -> s {attribute = a} :: DescribeNetworkInterfaceAttribute)
{-# DEPRECATED dniaAttribute "Use generic-lens or generic-optics with 'attribute' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dniaDryRun :: Lens.Lens' DescribeNetworkInterfaceAttribute (Lude.Maybe Lude.Bool)
dniaDryRun = Lens.lens (dryRun :: DescribeNetworkInterfaceAttribute -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeNetworkInterfaceAttribute)
{-# DEPRECATED dniaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dniaNetworkInterfaceId :: Lens.Lens' DescribeNetworkInterfaceAttribute Lude.Text
dniaNetworkInterfaceId = Lens.lens (networkInterfaceId :: DescribeNetworkInterfaceAttribute -> Lude.Text) (\s a -> s {networkInterfaceId = a} :: DescribeNetworkInterfaceAttribute)
{-# DEPRECATED dniaNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

instance Lude.AWSRequest DescribeNetworkInterfaceAttribute where
  type
    Rs DescribeNetworkInterfaceAttribute =
      DescribeNetworkInterfaceAttributeResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeNetworkInterfaceAttributeResponse'
            Lude.<$> ( x Lude..@? "groupSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "sourceDestCheck")
            Lude.<*> (x Lude..@? "networkInterfaceId")
            Lude.<*> (x Lude..@? "attachment")
            Lude.<*> (x Lude..@? "description")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeNetworkInterfaceAttribute where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeNetworkInterfaceAttribute where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeNetworkInterfaceAttribute where
  toQuery DescribeNetworkInterfaceAttribute' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeNetworkInterfaceAttribute" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "Attribute" Lude.=: attribute,
        "DryRun" Lude.=: dryRun,
        "NetworkInterfaceId" Lude.=: networkInterfaceId
      ]

-- | Contains the output of DescribeNetworkInterfaceAttribute.
--
-- /See:/ 'mkDescribeNetworkInterfaceAttributeResponse' smart constructor.
data DescribeNetworkInterfaceAttributeResponse = DescribeNetworkInterfaceAttributeResponse'
  { groups ::
      Lude.Maybe
        [GroupIdentifier],
    sourceDestCheck ::
      Lude.Maybe
        AttributeBooleanValue,
    networkInterfaceId ::
      Lude.Maybe
        Lude.Text,
    attachment ::
      Lude.Maybe
        NetworkInterfaceAttachment,
    description ::
      Lude.Maybe
        AttributeValue,
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

-- | Creates a value of 'DescribeNetworkInterfaceAttributeResponse' with the minimum fields required to make a request.
--
-- * 'attachment' - The attachment (if any) of the network interface.
-- * 'description' - The description of the network interface.
-- * 'groups' - The security groups associated with the network interface.
-- * 'networkInterfaceId' - The ID of the network interface.
-- * 'responseStatus' - The response status code.
-- * 'sourceDestCheck' - Indicates whether source/destination checking is enabled.
mkDescribeNetworkInterfaceAttributeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeNetworkInterfaceAttributeResponse
mkDescribeNetworkInterfaceAttributeResponse pResponseStatus_ =
  DescribeNetworkInterfaceAttributeResponse'
    { groups = Lude.Nothing,
      sourceDestCheck = Lude.Nothing,
      networkInterfaceId = Lude.Nothing,
      attachment = Lude.Nothing,
      description = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The security groups associated with the network interface.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dniarsGroups :: Lens.Lens' DescribeNetworkInterfaceAttributeResponse (Lude.Maybe [GroupIdentifier])
dniarsGroups = Lens.lens (groups :: DescribeNetworkInterfaceAttributeResponse -> Lude.Maybe [GroupIdentifier]) (\s a -> s {groups = a} :: DescribeNetworkInterfaceAttributeResponse)
{-# DEPRECATED dniarsGroups "Use generic-lens or generic-optics with 'groups' instead." #-}

-- | Indicates whether source/destination checking is enabled.
--
-- /Note:/ Consider using 'sourceDestCheck' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dniarsSourceDestCheck :: Lens.Lens' DescribeNetworkInterfaceAttributeResponse (Lude.Maybe AttributeBooleanValue)
dniarsSourceDestCheck = Lens.lens (sourceDestCheck :: DescribeNetworkInterfaceAttributeResponse -> Lude.Maybe AttributeBooleanValue) (\s a -> s {sourceDestCheck = a} :: DescribeNetworkInterfaceAttributeResponse)
{-# DEPRECATED dniarsSourceDestCheck "Use generic-lens or generic-optics with 'sourceDestCheck' instead." #-}

-- | The ID of the network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dniarsNetworkInterfaceId :: Lens.Lens' DescribeNetworkInterfaceAttributeResponse (Lude.Maybe Lude.Text)
dniarsNetworkInterfaceId = Lens.lens (networkInterfaceId :: DescribeNetworkInterfaceAttributeResponse -> Lude.Maybe Lude.Text) (\s a -> s {networkInterfaceId = a} :: DescribeNetworkInterfaceAttributeResponse)
{-# DEPRECATED dniarsNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

-- | The attachment (if any) of the network interface.
--
-- /Note:/ Consider using 'attachment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dniarsAttachment :: Lens.Lens' DescribeNetworkInterfaceAttributeResponse (Lude.Maybe NetworkInterfaceAttachment)
dniarsAttachment = Lens.lens (attachment :: DescribeNetworkInterfaceAttributeResponse -> Lude.Maybe NetworkInterfaceAttachment) (\s a -> s {attachment = a} :: DescribeNetworkInterfaceAttributeResponse)
{-# DEPRECATED dniarsAttachment "Use generic-lens or generic-optics with 'attachment' instead." #-}

-- | The description of the network interface.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dniarsDescription :: Lens.Lens' DescribeNetworkInterfaceAttributeResponse (Lude.Maybe AttributeValue)
dniarsDescription = Lens.lens (description :: DescribeNetworkInterfaceAttributeResponse -> Lude.Maybe AttributeValue) (\s a -> s {description = a} :: DescribeNetworkInterfaceAttributeResponse)
{-# DEPRECATED dniarsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dniarsResponseStatus :: Lens.Lens' DescribeNetworkInterfaceAttributeResponse Lude.Int
dniarsResponseStatus = Lens.lens (responseStatus :: DescribeNetworkInterfaceAttributeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeNetworkInterfaceAttributeResponse)
{-# DEPRECATED dniarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
