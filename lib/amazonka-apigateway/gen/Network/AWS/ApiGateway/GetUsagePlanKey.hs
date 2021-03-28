{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.GetUsagePlanKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a usage plan key of a given key identifier.
module Network.AWS.ApiGateway.GetUsagePlanKey
    (
    -- * Creating a request
      GetUsagePlanKey (..)
    , mkGetUsagePlanKey
    -- ** Request lenses
    , gUsagePlanId
    , gKeyId

     -- * Destructuring the response
    , Types.UsagePlanKey (..)
    , Types.mkUsagePlanKey
    -- ** Response lenses
    , Types.upkId
    , Types.upkName
    , Types.upkType
    , Types.upkValue
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The GET request to get a usage plan key of a given key identifier.
--
-- /See:/ 'mkGetUsagePlanKey' smart constructor.
data GetUsagePlanKey = GetUsagePlanKey'
  { usagePlanId :: Core.Text
    -- ^ [Required] The Id of the 'UsagePlan' resource representing the usage plan containing the to-be-retrieved 'UsagePlanKey' resource representing a plan customer.
  , keyId :: Core.Text
    -- ^ [Required] The key Id of the to-be-retrieved 'UsagePlanKey' resource representing a plan customer.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetUsagePlanKey' value with any optional fields omitted.
mkGetUsagePlanKey
    :: Core.Text -- ^ 'usagePlanId'
    -> Core.Text -- ^ 'keyId'
    -> GetUsagePlanKey
mkGetUsagePlanKey usagePlanId keyId
  = GetUsagePlanKey'{usagePlanId, keyId}

-- | [Required] The Id of the 'UsagePlan' resource representing the usage plan containing the to-be-retrieved 'UsagePlanKey' resource representing a plan customer.
--
-- /Note:/ Consider using 'usagePlanId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gUsagePlanId :: Lens.Lens' GetUsagePlanKey Core.Text
gUsagePlanId = Lens.field @"usagePlanId"
{-# INLINEABLE gUsagePlanId #-}
{-# DEPRECATED usagePlanId "Use generic-lens or generic-optics with 'usagePlanId' instead"  #-}

-- | [Required] The key Id of the to-be-retrieved 'UsagePlanKey' resource representing a plan customer.
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gKeyId :: Lens.Lens' GetUsagePlanKey Core.Text
gKeyId = Lens.field @"keyId"
{-# INLINEABLE gKeyId #-}
{-# DEPRECATED keyId "Use generic-lens or generic-optics with 'keyId' instead"  #-}

instance Core.ToQuery GetUsagePlanKey where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetUsagePlanKey where
        toHeaders GetUsagePlanKey{..}
          = Core.pure ("Accept", "application/json")

instance Core.AWSRequest GetUsagePlanKey where
        type Rs GetUsagePlanKey = Types.UsagePlanKey
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/usageplans/" Core.<> Core.toText usagePlanId Core.<> "/keys/"
                             Core.<> Core.toText keyId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
