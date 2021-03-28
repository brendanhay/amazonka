{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ModifySubnetAttribute (..)
    , mkModifySubnetAttribute
    -- ** Request lenses
    , msaSubnetId
    , msaAssignIpv6AddressOnCreation
    , msaCustomerOwnedIpv4Pool
    , msaMapCustomerOwnedIpOnLaunch
    , msaMapPublicIpOnLaunch

    -- * Destructuring the response
    , ModifySubnetAttributeResponse (..)
    , mkModifySubnetAttributeResponse
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifySubnetAttribute' smart constructor.
data ModifySubnetAttribute = ModifySubnetAttribute'
  { subnetId :: Types.SubnetId
    -- ^ The ID of the subnet.
  , assignIpv6AddressOnCreation :: Core.Maybe Types.AttributeBooleanValue
    -- ^ Specify @true@ to indicate that network interfaces created in the specified subnet should be assigned an IPv6 address. This includes a network interface that's created when launching an instance into the subnet (the instance therefore receives an IPv6 address). 
--
-- If you enable the IPv6 addressing feature for your subnet, your network interface or instance only receives an IPv6 address if it's created using version @2016-11-15@ or later of the Amazon EC2 API.
  , customerOwnedIpv4Pool :: Core.Maybe Types.CoipPoolId
    -- ^ The customer-owned IPv4 address pool associated with the subnet.
--
-- You must set this value when you specify @true@ for @MapCustomerOwnedIpOnLaunch@ .
  , mapCustomerOwnedIpOnLaunch :: Core.Maybe Types.AttributeBooleanValue
    -- ^ Specify @true@ to indicate that network interfaces attached to instances created in the specified subnet should be assigned a customer-owned IPv4 address.
--
-- When this value is @true@ , you must specify the customer-owned IP pool using @CustomerOwnedIpv4Pool@ .
  , mapPublicIpOnLaunch :: Core.Maybe Types.AttributeBooleanValue
    -- ^ Specify @true@ to indicate that network interfaces attached to instances created in the specified subnet should be assigned a public IPv4 address.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifySubnetAttribute' value with any optional fields omitted.
mkModifySubnetAttribute
    :: Types.SubnetId -- ^ 'subnetId'
    -> ModifySubnetAttribute
mkModifySubnetAttribute subnetId
  = ModifySubnetAttribute'{subnetId,
                           assignIpv6AddressOnCreation = Core.Nothing,
                           customerOwnedIpv4Pool = Core.Nothing,
                           mapCustomerOwnedIpOnLaunch = Core.Nothing,
                           mapPublicIpOnLaunch = Core.Nothing}

-- | The ID of the subnet.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msaSubnetId :: Lens.Lens' ModifySubnetAttribute Types.SubnetId
msaSubnetId = Lens.field @"subnetId"
{-# INLINEABLE msaSubnetId #-}
{-# DEPRECATED subnetId "Use generic-lens or generic-optics with 'subnetId' instead"  #-}

-- | Specify @true@ to indicate that network interfaces created in the specified subnet should be assigned an IPv6 address. This includes a network interface that's created when launching an instance into the subnet (the instance therefore receives an IPv6 address). 
--
-- If you enable the IPv6 addressing feature for your subnet, your network interface or instance only receives an IPv6 address if it's created using version @2016-11-15@ or later of the Amazon EC2 API.
--
-- /Note:/ Consider using 'assignIpv6AddressOnCreation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msaAssignIpv6AddressOnCreation :: Lens.Lens' ModifySubnetAttribute (Core.Maybe Types.AttributeBooleanValue)
msaAssignIpv6AddressOnCreation = Lens.field @"assignIpv6AddressOnCreation"
{-# INLINEABLE msaAssignIpv6AddressOnCreation #-}
{-# DEPRECATED assignIpv6AddressOnCreation "Use generic-lens or generic-optics with 'assignIpv6AddressOnCreation' instead"  #-}

-- | The customer-owned IPv4 address pool associated with the subnet.
--
-- You must set this value when you specify @true@ for @MapCustomerOwnedIpOnLaunch@ .
--
-- /Note:/ Consider using 'customerOwnedIpv4Pool' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msaCustomerOwnedIpv4Pool :: Lens.Lens' ModifySubnetAttribute (Core.Maybe Types.CoipPoolId)
msaCustomerOwnedIpv4Pool = Lens.field @"customerOwnedIpv4Pool"
{-# INLINEABLE msaCustomerOwnedIpv4Pool #-}
{-# DEPRECATED customerOwnedIpv4Pool "Use generic-lens or generic-optics with 'customerOwnedIpv4Pool' instead"  #-}

-- | Specify @true@ to indicate that network interfaces attached to instances created in the specified subnet should be assigned a customer-owned IPv4 address.
--
-- When this value is @true@ , you must specify the customer-owned IP pool using @CustomerOwnedIpv4Pool@ .
--
-- /Note:/ Consider using 'mapCustomerOwnedIpOnLaunch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msaMapCustomerOwnedIpOnLaunch :: Lens.Lens' ModifySubnetAttribute (Core.Maybe Types.AttributeBooleanValue)
msaMapCustomerOwnedIpOnLaunch = Lens.field @"mapCustomerOwnedIpOnLaunch"
{-# INLINEABLE msaMapCustomerOwnedIpOnLaunch #-}
{-# DEPRECATED mapCustomerOwnedIpOnLaunch "Use generic-lens or generic-optics with 'mapCustomerOwnedIpOnLaunch' instead"  #-}

-- | Specify @true@ to indicate that network interfaces attached to instances created in the specified subnet should be assigned a public IPv4 address.
--
-- /Note:/ Consider using 'mapPublicIpOnLaunch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msaMapPublicIpOnLaunch :: Lens.Lens' ModifySubnetAttribute (Core.Maybe Types.AttributeBooleanValue)
msaMapPublicIpOnLaunch = Lens.field @"mapPublicIpOnLaunch"
{-# INLINEABLE msaMapPublicIpOnLaunch #-}
{-# DEPRECATED mapPublicIpOnLaunch "Use generic-lens or generic-optics with 'mapPublicIpOnLaunch' instead"  #-}

instance Core.ToQuery ModifySubnetAttribute where
        toQuery ModifySubnetAttribute{..}
          = Core.toQueryPair "Action" ("ModifySubnetAttribute" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "SubnetId" subnetId
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "AssignIpv6AddressOnCreation")
                assignIpv6AddressOnCreation
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "CustomerOwnedIpv4Pool")
                customerOwnedIpv4Pool
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "MapCustomerOwnedIpOnLaunch")
                mapCustomerOwnedIpOnLaunch
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MapPublicIpOnLaunch")
                mapPublicIpOnLaunch

instance Core.ToHeaders ModifySubnetAttribute where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifySubnetAttribute where
        type Rs ModifySubnetAttribute = ModifySubnetAttributeResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull ModifySubnetAttributeResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifySubnetAttributeResponse' smart constructor.
data ModifySubnetAttributeResponse = ModifySubnetAttributeResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifySubnetAttributeResponse' value with any optional fields omitted.
mkModifySubnetAttributeResponse
    :: ModifySubnetAttributeResponse
mkModifySubnetAttributeResponse = ModifySubnetAttributeResponse'
