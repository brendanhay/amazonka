{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifySubnetAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a subnet attribute. You can only modify one attribute at a time.
module Network.AWS.EC2.ModifySubnetAttribute
  ( -- * Creating a request
    ModifySubnetAttribute (..),
    mkModifySubnetAttribute,

    -- ** Request lenses
    msaAssignIPv6AddressOnCreation,
    msaSubnetId,
    msaCustomerOwnedIPv4Pool,
    msaMapCustomerOwnedIPOnLaunch,
    msaMapPublicIPOnLaunch,

    -- * Destructuring the response
    ModifySubnetAttributeResponse (..),
    mkModifySubnetAttributeResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifySubnetAttribute' smart constructor.
data ModifySubnetAttribute = ModifySubnetAttribute'
  { -- | Specify @true@ to indicate that network interfaces created in the specified subnet should be assigned an IPv6 address. This includes a network interface that's created when launching an instance into the subnet (the instance therefore receives an IPv6 address).
    --
    -- If you enable the IPv6 addressing feature for your subnet, your network interface or instance only receives an IPv6 address if it's created using version @2016-11-15@ or later of the Amazon EC2 API.
    assignIPv6AddressOnCreation :: Lude.Maybe AttributeBooleanValue,
    -- | The ID of the subnet.
    subnetId :: Lude.Text,
    -- | The customer-owned IPv4 address pool associated with the subnet.
    --
    -- You must set this value when you specify @true@ for @MapCustomerOwnedIpOnLaunch@ .
    customerOwnedIPv4Pool :: Lude.Maybe Lude.Text,
    -- | Specify @true@ to indicate that network interfaces attached to instances created in the specified subnet should be assigned a customer-owned IPv4 address.
    --
    -- When this value is @true@ , you must specify the customer-owned IP pool using @CustomerOwnedIpv4Pool@ .
    mapCustomerOwnedIPOnLaunch :: Lude.Maybe AttributeBooleanValue,
    -- | Specify @true@ to indicate that network interfaces attached to instances created in the specified subnet should be assigned a public IPv4 address.
    mapPublicIPOnLaunch :: Lude.Maybe AttributeBooleanValue
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifySubnetAttribute' with the minimum fields required to make a request.
--
-- * 'assignIPv6AddressOnCreation' - Specify @true@ to indicate that network interfaces created in the specified subnet should be assigned an IPv6 address. This includes a network interface that's created when launching an instance into the subnet (the instance therefore receives an IPv6 address).
--
-- If you enable the IPv6 addressing feature for your subnet, your network interface or instance only receives an IPv6 address if it's created using version @2016-11-15@ or later of the Amazon EC2 API.
-- * 'subnetId' - The ID of the subnet.
-- * 'customerOwnedIPv4Pool' - The customer-owned IPv4 address pool associated with the subnet.
--
-- You must set this value when you specify @true@ for @MapCustomerOwnedIpOnLaunch@ .
-- * 'mapCustomerOwnedIPOnLaunch' - Specify @true@ to indicate that network interfaces attached to instances created in the specified subnet should be assigned a customer-owned IPv4 address.
--
-- When this value is @true@ , you must specify the customer-owned IP pool using @CustomerOwnedIpv4Pool@ .
-- * 'mapPublicIPOnLaunch' - Specify @true@ to indicate that network interfaces attached to instances created in the specified subnet should be assigned a public IPv4 address.
mkModifySubnetAttribute ::
  -- | 'subnetId'
  Lude.Text ->
  ModifySubnetAttribute
mkModifySubnetAttribute pSubnetId_ =
  ModifySubnetAttribute'
    { assignIPv6AddressOnCreation =
        Lude.Nothing,
      subnetId = pSubnetId_,
      customerOwnedIPv4Pool = Lude.Nothing,
      mapCustomerOwnedIPOnLaunch = Lude.Nothing,
      mapPublicIPOnLaunch = Lude.Nothing
    }

-- | Specify @true@ to indicate that network interfaces created in the specified subnet should be assigned an IPv6 address. This includes a network interface that's created when launching an instance into the subnet (the instance therefore receives an IPv6 address).
--
-- If you enable the IPv6 addressing feature for your subnet, your network interface or instance only receives an IPv6 address if it's created using version @2016-11-15@ or later of the Amazon EC2 API.
--
-- /Note:/ Consider using 'assignIPv6AddressOnCreation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msaAssignIPv6AddressOnCreation :: Lens.Lens' ModifySubnetAttribute (Lude.Maybe AttributeBooleanValue)
msaAssignIPv6AddressOnCreation = Lens.lens (assignIPv6AddressOnCreation :: ModifySubnetAttribute -> Lude.Maybe AttributeBooleanValue) (\s a -> s {assignIPv6AddressOnCreation = a} :: ModifySubnetAttribute)
{-# DEPRECATED msaAssignIPv6AddressOnCreation "Use generic-lens or generic-optics with 'assignIPv6AddressOnCreation' instead." #-}

-- | The ID of the subnet.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msaSubnetId :: Lens.Lens' ModifySubnetAttribute Lude.Text
msaSubnetId = Lens.lens (subnetId :: ModifySubnetAttribute -> Lude.Text) (\s a -> s {subnetId = a} :: ModifySubnetAttribute)
{-# DEPRECATED msaSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | The customer-owned IPv4 address pool associated with the subnet.
--
-- You must set this value when you specify @true@ for @MapCustomerOwnedIpOnLaunch@ .
--
-- /Note:/ Consider using 'customerOwnedIPv4Pool' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msaCustomerOwnedIPv4Pool :: Lens.Lens' ModifySubnetAttribute (Lude.Maybe Lude.Text)
msaCustomerOwnedIPv4Pool = Lens.lens (customerOwnedIPv4Pool :: ModifySubnetAttribute -> Lude.Maybe Lude.Text) (\s a -> s {customerOwnedIPv4Pool = a} :: ModifySubnetAttribute)
{-# DEPRECATED msaCustomerOwnedIPv4Pool "Use generic-lens or generic-optics with 'customerOwnedIPv4Pool' instead." #-}

-- | Specify @true@ to indicate that network interfaces attached to instances created in the specified subnet should be assigned a customer-owned IPv4 address.
--
-- When this value is @true@ , you must specify the customer-owned IP pool using @CustomerOwnedIpv4Pool@ .
--
-- /Note:/ Consider using 'mapCustomerOwnedIPOnLaunch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msaMapCustomerOwnedIPOnLaunch :: Lens.Lens' ModifySubnetAttribute (Lude.Maybe AttributeBooleanValue)
msaMapCustomerOwnedIPOnLaunch = Lens.lens (mapCustomerOwnedIPOnLaunch :: ModifySubnetAttribute -> Lude.Maybe AttributeBooleanValue) (\s a -> s {mapCustomerOwnedIPOnLaunch = a} :: ModifySubnetAttribute)
{-# DEPRECATED msaMapCustomerOwnedIPOnLaunch "Use generic-lens or generic-optics with 'mapCustomerOwnedIPOnLaunch' instead." #-}

-- | Specify @true@ to indicate that network interfaces attached to instances created in the specified subnet should be assigned a public IPv4 address.
--
-- /Note:/ Consider using 'mapPublicIPOnLaunch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msaMapPublicIPOnLaunch :: Lens.Lens' ModifySubnetAttribute (Lude.Maybe AttributeBooleanValue)
msaMapPublicIPOnLaunch = Lens.lens (mapPublicIPOnLaunch :: ModifySubnetAttribute -> Lude.Maybe AttributeBooleanValue) (\s a -> s {mapPublicIPOnLaunch = a} :: ModifySubnetAttribute)
{-# DEPRECATED msaMapPublicIPOnLaunch "Use generic-lens or generic-optics with 'mapPublicIPOnLaunch' instead." #-}

instance Lude.AWSRequest ModifySubnetAttribute where
  type Rs ModifySubnetAttribute = ModifySubnetAttributeResponse
  request = Req.postQuery ec2Service
  response = Res.receiveNull ModifySubnetAttributeResponse'

instance Lude.ToHeaders ModifySubnetAttribute where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifySubnetAttribute where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifySubnetAttribute where
  toQuery ModifySubnetAttribute' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ModifySubnetAttribute" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "AssignIpv6AddressOnCreation" Lude.=: assignIPv6AddressOnCreation,
        "SubnetId" Lude.=: subnetId,
        "CustomerOwnedIpv4Pool" Lude.=: customerOwnedIPv4Pool,
        "MapCustomerOwnedIpOnLaunch" Lude.=: mapCustomerOwnedIPOnLaunch,
        "MapPublicIpOnLaunch" Lude.=: mapPublicIPOnLaunch
      ]

-- | /See:/ 'mkModifySubnetAttributeResponse' smart constructor.
data ModifySubnetAttributeResponse = ModifySubnetAttributeResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifySubnetAttributeResponse' with the minimum fields required to make a request.
mkModifySubnetAttributeResponse ::
  ModifySubnetAttributeResponse
mkModifySubnetAttributeResponse = ModifySubnetAttributeResponse'
