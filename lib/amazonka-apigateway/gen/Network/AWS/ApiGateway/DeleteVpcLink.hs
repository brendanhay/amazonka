{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.DeleteVpcLink
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing 'VpcLink' of a specified identifier.
module Network.AWS.ApiGateway.DeleteVpcLink
    (
    -- * Creating a request
      DeleteVpcLink (..)
    , mkDeleteVpcLink
    -- ** Request lenses
    , dvlVpcLinkId

    -- * Destructuring the response
    , DeleteVpcLinkResponse (..)
    , mkDeleteVpcLinkResponse
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Deletes an existing 'VpcLink' of a specified identifier.
--
-- /See:/ 'mkDeleteVpcLink' smart constructor.
newtype DeleteVpcLink = DeleteVpcLink'
  { vpcLinkId :: Core.Text
    -- ^ [Required] The identifier of the 'VpcLink' . It is used in an 'Integration' to reference this 'VpcLink' .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteVpcLink' value with any optional fields omitted.
mkDeleteVpcLink
    :: Core.Text -- ^ 'vpcLinkId'
    -> DeleteVpcLink
mkDeleteVpcLink vpcLinkId = DeleteVpcLink'{vpcLinkId}

-- | [Required] The identifier of the 'VpcLink' . It is used in an 'Integration' to reference this 'VpcLink' .
--
-- /Note:/ Consider using 'vpcLinkId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvlVpcLinkId :: Lens.Lens' DeleteVpcLink Core.Text
dvlVpcLinkId = Lens.field @"vpcLinkId"
{-# INLINEABLE dvlVpcLinkId #-}
{-# DEPRECATED vpcLinkId "Use generic-lens or generic-optics with 'vpcLinkId' instead"  #-}

instance Core.ToQuery DeleteVpcLink where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteVpcLink where
        toHeaders DeleteVpcLink{..}
          = Core.pure ("Accept", "application/json")

instance Core.AWSRequest DeleteVpcLink where
        type Rs DeleteVpcLink = DeleteVpcLinkResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath = "/vpclinks/" Core.<> Core.toText vpcLinkId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteVpcLinkResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteVpcLinkResponse' smart constructor.
data DeleteVpcLinkResponse = DeleteVpcLinkResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteVpcLinkResponse' value with any optional fields omitted.
mkDeleteVpcLinkResponse
    :: DeleteVpcLinkResponse
mkDeleteVpcLinkResponse = DeleteVpcLinkResponse'
