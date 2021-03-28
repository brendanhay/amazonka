{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.CreateUsagePlanKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a usage plan key for adding an existing API key to a usage plan.
module Network.AWS.ApiGateway.CreateUsagePlanKey
    (
    -- * Creating a request
      CreateUsagePlanKey (..)
    , mkCreateUsagePlanKey
    -- ** Request lenses
    , cupkUsagePlanId
    , cupkKeyId
    , cupkKeyType

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

-- | The POST request to create a usage plan key for adding an existing API key to a usage plan.
--
-- /See:/ 'mkCreateUsagePlanKey' smart constructor.
data CreateUsagePlanKey = CreateUsagePlanKey'
  { usagePlanId :: Core.Text
    -- ^ [Required] The Id of the 'UsagePlan' resource representing the usage plan containing the to-be-created 'UsagePlanKey' resource representing a plan customer.
  , keyId :: Core.Text
    -- ^ [Required] The identifier of a 'UsagePlanKey' resource for a plan customer.
  , keyType :: Core.Text
    -- ^ [Required] The type of a 'UsagePlanKey' resource for a plan customer.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateUsagePlanKey' value with any optional fields omitted.
mkCreateUsagePlanKey
    :: Core.Text -- ^ 'usagePlanId'
    -> Core.Text -- ^ 'keyId'
    -> Core.Text -- ^ 'keyType'
    -> CreateUsagePlanKey
mkCreateUsagePlanKey usagePlanId keyId keyType
  = CreateUsagePlanKey'{usagePlanId, keyId, keyType}

-- | [Required] The Id of the 'UsagePlan' resource representing the usage plan containing the to-be-created 'UsagePlanKey' resource representing a plan customer.
--
-- /Note:/ Consider using 'usagePlanId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupkUsagePlanId :: Lens.Lens' CreateUsagePlanKey Core.Text
cupkUsagePlanId = Lens.field @"usagePlanId"
{-# INLINEABLE cupkUsagePlanId #-}
{-# DEPRECATED usagePlanId "Use generic-lens or generic-optics with 'usagePlanId' instead"  #-}

-- | [Required] The identifier of a 'UsagePlanKey' resource for a plan customer.
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupkKeyId :: Lens.Lens' CreateUsagePlanKey Core.Text
cupkKeyId = Lens.field @"keyId"
{-# INLINEABLE cupkKeyId #-}
{-# DEPRECATED keyId "Use generic-lens or generic-optics with 'keyId' instead"  #-}

-- | [Required] The type of a 'UsagePlanKey' resource for a plan customer.
--
-- /Note:/ Consider using 'keyType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupkKeyType :: Lens.Lens' CreateUsagePlanKey Core.Text
cupkKeyType = Lens.field @"keyType"
{-# INLINEABLE cupkKeyType #-}
{-# DEPRECATED keyType "Use generic-lens or generic-optics with 'keyType' instead"  #-}

instance Core.ToQuery CreateUsagePlanKey where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateUsagePlanKey where
        toHeaders CreateUsagePlanKey{..}
          = Core.pure ("Accept", "application/json")

instance Core.FromJSON CreateUsagePlanKey where
        toJSON CreateUsagePlanKey{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("keyId" Core..= keyId),
                  Core.Just ("keyType" Core..= keyType)])

instance Core.AWSRequest CreateUsagePlanKey where
        type Rs CreateUsagePlanKey = Types.UsagePlanKey
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/usageplans/" Core.<> Core.toText usagePlanId Core.<> "/keys",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
