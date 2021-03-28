{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.ModifyUsageLimit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a usage limit in a cluster. You can't modify the feature type or period of a usage limit.
module Network.AWS.Redshift.ModifyUsageLimit
    (
    -- * Creating a request
      ModifyUsageLimit (..)
    , mkModifyUsageLimit
    -- ** Request lenses
    , mulUsageLimitId
    , mulAmount
    , mulBreachAction

     -- * Destructuring the response
    , Types.UsageLimit (..)
    , Types.mkUsageLimit
    -- ** Response lenses
    , Types.ulAmount
    , Types.ulBreachAction
    , Types.ulClusterIdentifier
    , Types.ulFeatureType
    , Types.ulLimitType
    , Types.ulPeriod
    , Types.ulTags
    , Types.ulUsageLimitId
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyUsageLimit' smart constructor.
data ModifyUsageLimit = ModifyUsageLimit'
  { usageLimitId :: Core.Text
    -- ^ The identifier of the usage limit to modify.
  , amount :: Core.Maybe Core.Integer
    -- ^ The new limit amount. For more information about this parameter, see 'UsageLimit' . 
  , breachAction :: Core.Maybe Types.UsageLimitBreachAction
    -- ^ The new action that Amazon Redshift takes when the limit is reached. For more information about this parameter, see 'UsageLimit' . 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyUsageLimit' value with any optional fields omitted.
mkModifyUsageLimit
    :: Core.Text -- ^ 'usageLimitId'
    -> ModifyUsageLimit
mkModifyUsageLimit usageLimitId
  = ModifyUsageLimit'{usageLimitId, amount = Core.Nothing,
                      breachAction = Core.Nothing}

-- | The identifier of the usage limit to modify.
--
-- /Note:/ Consider using 'usageLimitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mulUsageLimitId :: Lens.Lens' ModifyUsageLimit Core.Text
mulUsageLimitId = Lens.field @"usageLimitId"
{-# INLINEABLE mulUsageLimitId #-}
{-# DEPRECATED usageLimitId "Use generic-lens or generic-optics with 'usageLimitId' instead"  #-}

-- | The new limit amount. For more information about this parameter, see 'UsageLimit' . 
--
-- /Note:/ Consider using 'amount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mulAmount :: Lens.Lens' ModifyUsageLimit (Core.Maybe Core.Integer)
mulAmount = Lens.field @"amount"
{-# INLINEABLE mulAmount #-}
{-# DEPRECATED amount "Use generic-lens or generic-optics with 'amount' instead"  #-}

-- | The new action that Amazon Redshift takes when the limit is reached. For more information about this parameter, see 'UsageLimit' . 
--
-- /Note:/ Consider using 'breachAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mulBreachAction :: Lens.Lens' ModifyUsageLimit (Core.Maybe Types.UsageLimitBreachAction)
mulBreachAction = Lens.field @"breachAction"
{-# INLINEABLE mulBreachAction #-}
{-# DEPRECATED breachAction "Use generic-lens or generic-optics with 'breachAction' instead"  #-}

instance Core.ToQuery ModifyUsageLimit where
        toQuery ModifyUsageLimit{..}
          = Core.toQueryPair "Action" ("ModifyUsageLimit" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2012-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "UsageLimitId" usageLimitId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Amount") amount
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "BreachAction")
                breachAction

instance Core.ToHeaders ModifyUsageLimit where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyUsageLimit where
        type Rs ModifyUsageLimit = Types.UsageLimit
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
        parseResponse
          = Response.receiveXMLWrapper "ModifyUsageLimitResult"
              (\ s h x -> Core.parseXML x)
        
        {-# INLINE parseResponse #-}
