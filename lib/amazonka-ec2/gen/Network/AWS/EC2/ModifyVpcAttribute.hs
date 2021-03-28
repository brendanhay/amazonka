{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyVpcAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified attribute of the specified VPC.
module Network.AWS.EC2.ModifyVpcAttribute
    (
    -- * Creating a request
      ModifyVpcAttribute (..)
    , mkModifyVpcAttribute
    -- ** Request lenses
    , mvaVpcId
    , mvaEnableDnsHostnames
    , mvaEnableDnsSupport

    -- * Destructuring the response
    , ModifyVpcAttributeResponse (..)
    , mkModifyVpcAttributeResponse
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyVpcAttribute' smart constructor.
data ModifyVpcAttribute = ModifyVpcAttribute'
  { vpcId :: Types.VpcId
    -- ^ The ID of the VPC.
  , enableDnsHostnames :: Core.Maybe Types.AttributeBooleanValue
    -- ^ Indicates whether the instances launched in the VPC get DNS hostnames. If enabled, instances in the VPC get DNS hostnames; otherwise, they do not.
--
-- You cannot modify the DNS resolution and DNS hostnames attributes in the same request. Use separate requests for each attribute. You can only enable DNS hostnames if you've enabled DNS support.
  , enableDnsSupport :: Core.Maybe Types.AttributeBooleanValue
    -- ^ Indicates whether the DNS resolution is supported for the VPC. If enabled, queries to the Amazon provided DNS server at the 169.254.169.253 IP address, or the reserved IP address at the base of the VPC network range "plus two" succeed. If disabled, the Amazon provided DNS service in the VPC that resolves public DNS hostnames to IP addresses is not enabled.
--
-- You cannot modify the DNS resolution and DNS hostnames attributes in the same request. Use separate requests for each attribute.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyVpcAttribute' value with any optional fields omitted.
mkModifyVpcAttribute
    :: Types.VpcId -- ^ 'vpcId'
    -> ModifyVpcAttribute
mkModifyVpcAttribute vpcId
  = ModifyVpcAttribute'{vpcId, enableDnsHostnames = Core.Nothing,
                        enableDnsSupport = Core.Nothing}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvaVpcId :: Lens.Lens' ModifyVpcAttribute Types.VpcId
mvaVpcId = Lens.field @"vpcId"
{-# INLINEABLE mvaVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

-- | Indicates whether the instances launched in the VPC get DNS hostnames. If enabled, instances in the VPC get DNS hostnames; otherwise, they do not.
--
-- You cannot modify the DNS resolution and DNS hostnames attributes in the same request. Use separate requests for each attribute. You can only enable DNS hostnames if you've enabled DNS support.
--
-- /Note:/ Consider using 'enableDnsHostnames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvaEnableDnsHostnames :: Lens.Lens' ModifyVpcAttribute (Core.Maybe Types.AttributeBooleanValue)
mvaEnableDnsHostnames = Lens.field @"enableDnsHostnames"
{-# INLINEABLE mvaEnableDnsHostnames #-}
{-# DEPRECATED enableDnsHostnames "Use generic-lens or generic-optics with 'enableDnsHostnames' instead"  #-}

-- | Indicates whether the DNS resolution is supported for the VPC. If enabled, queries to the Amazon provided DNS server at the 169.254.169.253 IP address, or the reserved IP address at the base of the VPC network range "plus two" succeed. If disabled, the Amazon provided DNS service in the VPC that resolves public DNS hostnames to IP addresses is not enabled.
--
-- You cannot modify the DNS resolution and DNS hostnames attributes in the same request. Use separate requests for each attribute.
--
-- /Note:/ Consider using 'enableDnsSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvaEnableDnsSupport :: Lens.Lens' ModifyVpcAttribute (Core.Maybe Types.AttributeBooleanValue)
mvaEnableDnsSupport = Lens.field @"enableDnsSupport"
{-# INLINEABLE mvaEnableDnsSupport #-}
{-# DEPRECATED enableDnsSupport "Use generic-lens or generic-optics with 'enableDnsSupport' instead"  #-}

instance Core.ToQuery ModifyVpcAttribute where
        toQuery ModifyVpcAttribute{..}
          = Core.toQueryPair "Action" ("ModifyVpcAttribute" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "VpcId" vpcId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "EnableDnsHostnames")
                enableDnsHostnames
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "EnableDnsSupport")
                enableDnsSupport

instance Core.ToHeaders ModifyVpcAttribute where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyVpcAttribute where
        type Rs ModifyVpcAttribute = ModifyVpcAttributeResponse
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
        parseResponse = Response.receiveNull ModifyVpcAttributeResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyVpcAttributeResponse' smart constructor.
data ModifyVpcAttributeResponse = ModifyVpcAttributeResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyVpcAttributeResponse' value with any optional fields omitted.
mkModifyVpcAttributeResponse
    :: ModifyVpcAttributeResponse
mkModifyVpcAttributeResponse = ModifyVpcAttributeResponse'
