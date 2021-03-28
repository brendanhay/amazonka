{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.GetVpcLink
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a specified VPC link under the caller's account in a region.
module Network.AWS.ApiGateway.GetVpcLink
    (
    -- * Creating a request
      GetVpcLink (..)
    , mkGetVpcLink
    -- ** Request lenses
    , gvlVpcLinkId

     -- * Destructuring the response
    , Types.VpcLink (..)
    , Types.mkVpcLink
    -- ** Response lenses
    , Types.vlDescription
    , Types.vlId
    , Types.vlName
    , Types.vlStatus
    , Types.vlStatusMessage
    , Types.vlTags
    , Types.vlTargetArns
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Gets a specified VPC link under the caller's account in a region.
--
-- /See:/ 'mkGetVpcLink' smart constructor.
newtype GetVpcLink = GetVpcLink'
  { vpcLinkId :: Core.Text
    -- ^ [Required] The identifier of the 'VpcLink' . It is used in an 'Integration' to reference this 'VpcLink' .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetVpcLink' value with any optional fields omitted.
mkGetVpcLink
    :: Core.Text -- ^ 'vpcLinkId'
    -> GetVpcLink
mkGetVpcLink vpcLinkId = GetVpcLink'{vpcLinkId}

-- | [Required] The identifier of the 'VpcLink' . It is used in an 'Integration' to reference this 'VpcLink' .
--
-- /Note:/ Consider using 'vpcLinkId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvlVpcLinkId :: Lens.Lens' GetVpcLink Core.Text
gvlVpcLinkId = Lens.field @"vpcLinkId"
{-# INLINEABLE gvlVpcLinkId #-}
{-# DEPRECATED vpcLinkId "Use generic-lens or generic-optics with 'vpcLinkId' instead"  #-}

instance Core.ToQuery GetVpcLink where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetVpcLink where
        toHeaders GetVpcLink{..} = Core.pure ("Accept", "application/json")

instance Core.AWSRequest GetVpcLink where
        type Rs GetVpcLink = Types.VpcLink
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/vpclinks/" Core.<> Core.toText vpcLinkId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
