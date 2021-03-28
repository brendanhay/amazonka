{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DisassociateAddress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates an Elastic IP address from the instance or network interface it's associated with.
--
-- An Elastic IP address is for use in either the EC2-Classic platform or in a VPC. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP Addresses> in the /Amazon Elastic Compute Cloud User Guide/ .
-- This is an idempotent operation. If you perform the operation more than once, Amazon EC2 doesn't return an error.
module Network.AWS.EC2.DisassociateAddress
    (
    -- * Creating a request
      DisassociateAddress (..)
    , mkDisassociateAddress
    -- ** Request lenses
    , dasAssociationId
    , dasDryRun
    , dasPublicIp

    -- * Destructuring the response
    , DisassociateAddressResponse (..)
    , mkDisassociateAddressResponse
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisassociateAddress' smart constructor.
data DisassociateAddress = DisassociateAddress'
  { associationId :: Core.Maybe Types.AssociationId
    -- ^ [EC2-VPC] The association ID. Required for EC2-VPC.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , publicIp :: Core.Maybe Core.Text
    -- ^ [EC2-Classic] The Elastic IP address. Required for EC2-Classic.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateAddress' value with any optional fields omitted.
mkDisassociateAddress
    :: DisassociateAddress
mkDisassociateAddress
  = DisassociateAddress'{associationId = Core.Nothing,
                         dryRun = Core.Nothing, publicIp = Core.Nothing}

-- | [EC2-VPC] The association ID. Required for EC2-VPC.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasAssociationId :: Lens.Lens' DisassociateAddress (Core.Maybe Types.AssociationId)
dasAssociationId = Lens.field @"associationId"
{-# INLINEABLE dasAssociationId #-}
{-# DEPRECATED associationId "Use generic-lens or generic-optics with 'associationId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasDryRun :: Lens.Lens' DisassociateAddress (Core.Maybe Core.Bool)
dasDryRun = Lens.field @"dryRun"
{-# INLINEABLE dasDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | [EC2-Classic] The Elastic IP address. Required for EC2-Classic.
--
-- /Note:/ Consider using 'publicIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasPublicIp :: Lens.Lens' DisassociateAddress (Core.Maybe Core.Text)
dasPublicIp = Lens.field @"publicIp"
{-# INLINEABLE dasPublicIp #-}
{-# DEPRECATED publicIp "Use generic-lens or generic-optics with 'publicIp' instead"  #-}

instance Core.ToQuery DisassociateAddress where
        toQuery DisassociateAddress{..}
          = Core.toQueryPair "Action" ("DisassociateAddress" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "AssociationId")
                associationId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PublicIp") publicIp

instance Core.ToHeaders DisassociateAddress where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DisassociateAddress where
        type Rs DisassociateAddress = DisassociateAddressResponse
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
        parseResponse = Response.receiveNull DisassociateAddressResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDisassociateAddressResponse' smart constructor.
data DisassociateAddressResponse = DisassociateAddressResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateAddressResponse' value with any optional fields omitted.
mkDisassociateAddressResponse
    :: DisassociateAddressResponse
mkDisassociateAddressResponse = DisassociateAddressResponse'
