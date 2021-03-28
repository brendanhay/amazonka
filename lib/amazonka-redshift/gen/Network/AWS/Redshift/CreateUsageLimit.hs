{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.CreateUsageLimit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a usage limit for a specified Amazon Redshift feature on a cluster. The usage limit is identified by the returned usage limit identifier.
module Network.AWS.Redshift.CreateUsageLimit
    (
    -- * Creating a request
      CreateUsageLimit (..)
    , mkCreateUsageLimit
    -- ** Request lenses
    , culClusterIdentifier
    , culFeatureType
    , culLimitType
    , culAmount
    , culBreachAction
    , culPeriod
    , culTags

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

-- | /See:/ 'mkCreateUsageLimit' smart constructor.
data CreateUsageLimit = CreateUsageLimit'
  { clusterIdentifier :: Core.Text
    -- ^ The identifier of the cluster that you want to limit usage.
  , featureType :: Types.UsageLimitFeatureType
    -- ^ The Amazon Redshift feature that you want to limit.
  , limitType :: Types.UsageLimitLimitType
    -- ^ The type of limit. Depending on the feature type, this can be based on a time duration or data size. If @FeatureType@ is @spectrum@ , then @LimitType@ must be @data-scanned@ . If @FeatureType@ is @concurrency-scaling@ , then @LimitType@ must be @time@ . 
  , amount :: Core.Integer
    -- ^ The limit amount. If time-based, this amount is in minutes. If data-based, this amount is in terabytes (TB). The value must be a positive number. 
  , breachAction :: Core.Maybe Types.UsageLimitBreachAction
    -- ^ The action that Amazon Redshift takes when the limit is reached. The default is log. For more information about this parameter, see 'UsageLimit' .
  , period :: Core.Maybe Types.UsageLimitPeriod
    -- ^ The time period that the amount applies to. A @weekly@ period begins on Sunday. The default is @monthly@ . 
  , tags :: Core.Maybe [Types.Tag]
    -- ^ A list of tag instances.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateUsageLimit' value with any optional fields omitted.
mkCreateUsageLimit
    :: Core.Text -- ^ 'clusterIdentifier'
    -> Types.UsageLimitFeatureType -- ^ 'featureType'
    -> Types.UsageLimitLimitType -- ^ 'limitType'
    -> Core.Integer -- ^ 'amount'
    -> CreateUsageLimit
mkCreateUsageLimit clusterIdentifier featureType limitType amount
  = CreateUsageLimit'{clusterIdentifier, featureType, limitType,
                      amount, breachAction = Core.Nothing, period = Core.Nothing,
                      tags = Core.Nothing}

-- | The identifier of the cluster that you want to limit usage.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
culClusterIdentifier :: Lens.Lens' CreateUsageLimit Core.Text
culClusterIdentifier = Lens.field @"clusterIdentifier"
{-# INLINEABLE culClusterIdentifier #-}
{-# DEPRECATED clusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead"  #-}

-- | The Amazon Redshift feature that you want to limit.
--
-- /Note:/ Consider using 'featureType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
culFeatureType :: Lens.Lens' CreateUsageLimit Types.UsageLimitFeatureType
culFeatureType = Lens.field @"featureType"
{-# INLINEABLE culFeatureType #-}
{-# DEPRECATED featureType "Use generic-lens or generic-optics with 'featureType' instead"  #-}

-- | The type of limit. Depending on the feature type, this can be based on a time duration or data size. If @FeatureType@ is @spectrum@ , then @LimitType@ must be @data-scanned@ . If @FeatureType@ is @concurrency-scaling@ , then @LimitType@ must be @time@ . 
--
-- /Note:/ Consider using 'limitType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
culLimitType :: Lens.Lens' CreateUsageLimit Types.UsageLimitLimitType
culLimitType = Lens.field @"limitType"
{-# INLINEABLE culLimitType #-}
{-# DEPRECATED limitType "Use generic-lens or generic-optics with 'limitType' instead"  #-}

-- | The limit amount. If time-based, this amount is in minutes. If data-based, this amount is in terabytes (TB). The value must be a positive number. 
--
-- /Note:/ Consider using 'amount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
culAmount :: Lens.Lens' CreateUsageLimit Core.Integer
culAmount = Lens.field @"amount"
{-# INLINEABLE culAmount #-}
{-# DEPRECATED amount "Use generic-lens or generic-optics with 'amount' instead"  #-}

-- | The action that Amazon Redshift takes when the limit is reached. The default is log. For more information about this parameter, see 'UsageLimit' .
--
-- /Note:/ Consider using 'breachAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
culBreachAction :: Lens.Lens' CreateUsageLimit (Core.Maybe Types.UsageLimitBreachAction)
culBreachAction = Lens.field @"breachAction"
{-# INLINEABLE culBreachAction #-}
{-# DEPRECATED breachAction "Use generic-lens or generic-optics with 'breachAction' instead"  #-}

-- | The time period that the amount applies to. A @weekly@ period begins on Sunday. The default is @monthly@ . 
--
-- /Note:/ Consider using 'period' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
culPeriod :: Lens.Lens' CreateUsageLimit (Core.Maybe Types.UsageLimitPeriod)
culPeriod = Lens.field @"period"
{-# INLINEABLE culPeriod #-}
{-# DEPRECATED period "Use generic-lens or generic-optics with 'period' instead"  #-}

-- | A list of tag instances.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
culTags :: Lens.Lens' CreateUsageLimit (Core.Maybe [Types.Tag])
culTags = Lens.field @"tags"
{-# INLINEABLE culTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateUsageLimit where
        toQuery CreateUsageLimit{..}
          = Core.toQueryPair "Action" ("CreateUsageLimit" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2012-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "ClusterIdentifier" clusterIdentifier
              Core.<> Core.toQueryPair "FeatureType" featureType
              Core.<> Core.toQueryPair "LimitType" limitType
              Core.<> Core.toQueryPair "Amount" amount
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "BreachAction")
                breachAction
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Period") period
              Core.<>
              Core.toQueryPair "Tags"
                (Core.maybe Core.mempty (Core.toQueryList "Tag") tags)

instance Core.ToHeaders CreateUsageLimit where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateUsageLimit where
        type Rs CreateUsageLimit = Types.UsageLimit
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
          = Response.receiveXMLWrapper "CreateUsageLimitResult"
              (\ s h x -> Core.parseXML x)
        
        {-# INLINE parseResponse #-}
