{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.GetUsagePlan
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a usage plan of a given plan identifier.
module Network.AWS.ApiGateway.GetUsagePlan
    (
    -- * Creating a request
      GetUsagePlan (..)
    , mkGetUsagePlan
    -- ** Request lenses
    , gupUsagePlanId

     -- * Destructuring the response
    , Types.UsagePlan (..)
    , Types.mkUsagePlan
    -- ** Response lenses
    , Types.upApiStages
    , Types.upDescription
    , Types.upId
    , Types.upName
    , Types.upProductCode
    , Types.upQuota
    , Types.upTags
    , Types.upThrottle
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The GET request to get a usage plan of a given plan identifier.
--
-- /See:/ 'mkGetUsagePlan' smart constructor.
newtype GetUsagePlan = GetUsagePlan'
  { usagePlanId :: Core.Text
    -- ^ [Required] The identifier of the 'UsagePlan' resource to be retrieved.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetUsagePlan' value with any optional fields omitted.
mkGetUsagePlan
    :: Core.Text -- ^ 'usagePlanId'
    -> GetUsagePlan
mkGetUsagePlan usagePlanId = GetUsagePlan'{usagePlanId}

-- | [Required] The identifier of the 'UsagePlan' resource to be retrieved.
--
-- /Note:/ Consider using 'usagePlanId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gupUsagePlanId :: Lens.Lens' GetUsagePlan Core.Text
gupUsagePlanId = Lens.field @"usagePlanId"
{-# INLINEABLE gupUsagePlanId #-}
{-# DEPRECATED usagePlanId "Use generic-lens or generic-optics with 'usagePlanId' instead"  #-}

instance Core.ToQuery GetUsagePlan where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetUsagePlan where
        toHeaders GetUsagePlan{..}
          = Core.pure ("Accept", "application/json")

instance Core.AWSRequest GetUsagePlan where
        type Rs GetUsagePlan = Types.UsagePlan
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/usageplans/" Core.<> Core.toText usagePlanId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
