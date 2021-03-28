{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AttachInternetGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches an internet gateway or a virtual private gateway to a VPC, enabling connectivity between the internet and the VPC. For more information about your VPC and internet gateway, see the <https://docs.aws.amazon.com/vpc/latest/userguide/ Amazon Virtual Private Cloud User Guide> .
module Network.AWS.EC2.AttachInternetGateway
    (
    -- * Creating a request
      AttachInternetGateway (..)
    , mkAttachInternetGateway
    -- ** Request lenses
    , aigInternetGatewayId
    , aigVpcId
    , aigDryRun

    -- * Destructuring the response
    , AttachInternetGatewayResponse (..)
    , mkAttachInternetGatewayResponse
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAttachInternetGateway' smart constructor.
data AttachInternetGateway = AttachInternetGateway'
  { internetGatewayId :: Types.InternetGatewayId
    -- ^ The ID of the internet gateway.
  , vpcId :: Types.VpcId
    -- ^ The ID of the VPC.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttachInternetGateway' value with any optional fields omitted.
mkAttachInternetGateway
    :: Types.InternetGatewayId -- ^ 'internetGatewayId'
    -> Types.VpcId -- ^ 'vpcId'
    -> AttachInternetGateway
mkAttachInternetGateway internetGatewayId vpcId
  = AttachInternetGateway'{internetGatewayId, vpcId,
                           dryRun = Core.Nothing}

-- | The ID of the internet gateway.
--
-- /Note:/ Consider using 'internetGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aigInternetGatewayId :: Lens.Lens' AttachInternetGateway Types.InternetGatewayId
aigInternetGatewayId = Lens.field @"internetGatewayId"
{-# INLINEABLE aigInternetGatewayId #-}
{-# DEPRECATED internetGatewayId "Use generic-lens or generic-optics with 'internetGatewayId' instead"  #-}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aigVpcId :: Lens.Lens' AttachInternetGateway Types.VpcId
aigVpcId = Lens.field @"vpcId"
{-# INLINEABLE aigVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aigDryRun :: Lens.Lens' AttachInternetGateway (Core.Maybe Core.Bool)
aigDryRun = Lens.field @"dryRun"
{-# INLINEABLE aigDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery AttachInternetGateway where
        toQuery AttachInternetGateway{..}
          = Core.toQueryPair "Action" ("AttachInternetGateway" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "InternetGatewayId" internetGatewayId
              Core.<> Core.toQueryPair "VpcId" vpcId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders AttachInternetGateway where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest AttachInternetGateway where
        type Rs AttachInternetGateway = AttachInternetGatewayResponse
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
        parseResponse = Response.receiveNull AttachInternetGatewayResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAttachInternetGatewayResponse' smart constructor.
data AttachInternetGatewayResponse = AttachInternetGatewayResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttachInternetGatewayResponse' value with any optional fields omitted.
mkAttachInternetGatewayResponse
    :: AttachInternetGatewayResponse
mkAttachInternetGatewayResponse = AttachInternetGatewayResponse'
