{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateUsageLimit (..),
    mkCreateUsageLimit,

    -- ** Request lenses
    culClusterIdentifier,
    culFeatureType,
    culLimitType,
    culAmount,
    culBreachAction,
    culPeriod,
    culTags,

    -- * Destructuring the response
    Types.UsageLimit (..),
    Types.mkUsageLimit,

    -- ** Response lenses
    Types.ulAmount,
    Types.ulBreachAction,
    Types.ulClusterIdentifier,
    Types.ulFeatureType,
    Types.ulLimitType,
    Types.ulPeriod,
    Types.ulTags,
    Types.ulUsageLimitId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateUsageLimit' smart constructor.
data CreateUsageLimit = CreateUsageLimit'
  { -- | The identifier of the cluster that you want to limit usage.
    clusterIdentifier :: Types.String,
    -- | The Amazon Redshift feature that you want to limit.
    featureType :: Types.UsageLimitFeatureType,
    -- | The type of limit. Depending on the feature type, this can be based on a time duration or data size. If @FeatureType@ is @spectrum@ , then @LimitType@ must be @data-scanned@ . If @FeatureType@ is @concurrency-scaling@ , then @LimitType@ must be @time@ .
    limitType :: Types.UsageLimitLimitType,
    -- | The limit amount. If time-based, this amount is in minutes. If data-based, this amount is in terabytes (TB). The value must be a positive number.
    amount :: Core.Integer,
    -- | The action that Amazon Redshift takes when the limit is reached. The default is log. For more information about this parameter, see 'UsageLimit' .
    breachAction :: Core.Maybe Types.UsageLimitBreachAction,
    -- | The time period that the amount applies to. A @weekly@ period begins on Sunday. The default is @monthly@ .
    period :: Core.Maybe Types.UsageLimitPeriod,
    -- | A list of tag instances.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateUsageLimit' value with any optional fields omitted.
mkCreateUsageLimit ::
  -- | 'clusterIdentifier'
  Types.String ->
  -- | 'featureType'
  Types.UsageLimitFeatureType ->
  -- | 'limitType'
  Types.UsageLimitLimitType ->
  -- | 'amount'
  Core.Integer ->
  CreateUsageLimit
mkCreateUsageLimit clusterIdentifier featureType limitType amount =
  CreateUsageLimit'
    { clusterIdentifier,
      featureType,
      limitType,
      amount,
      breachAction = Core.Nothing,
      period = Core.Nothing,
      tags = Core.Nothing
    }

-- | The identifier of the cluster that you want to limit usage.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
culClusterIdentifier :: Lens.Lens' CreateUsageLimit Types.String
culClusterIdentifier = Lens.field @"clusterIdentifier"
{-# DEPRECATED culClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

-- | The Amazon Redshift feature that you want to limit.
--
-- /Note:/ Consider using 'featureType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
culFeatureType :: Lens.Lens' CreateUsageLimit Types.UsageLimitFeatureType
culFeatureType = Lens.field @"featureType"
{-# DEPRECATED culFeatureType "Use generic-lens or generic-optics with 'featureType' instead." #-}

-- | The type of limit. Depending on the feature type, this can be based on a time duration or data size. If @FeatureType@ is @spectrum@ , then @LimitType@ must be @data-scanned@ . If @FeatureType@ is @concurrency-scaling@ , then @LimitType@ must be @time@ .
--
-- /Note:/ Consider using 'limitType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
culLimitType :: Lens.Lens' CreateUsageLimit Types.UsageLimitLimitType
culLimitType = Lens.field @"limitType"
{-# DEPRECATED culLimitType "Use generic-lens or generic-optics with 'limitType' instead." #-}

-- | The limit amount. If time-based, this amount is in minutes. If data-based, this amount is in terabytes (TB). The value must be a positive number.
--
-- /Note:/ Consider using 'amount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
culAmount :: Lens.Lens' CreateUsageLimit Core.Integer
culAmount = Lens.field @"amount"
{-# DEPRECATED culAmount "Use generic-lens or generic-optics with 'amount' instead." #-}

-- | The action that Amazon Redshift takes when the limit is reached. The default is log. For more information about this parameter, see 'UsageLimit' .
--
-- /Note:/ Consider using 'breachAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
culBreachAction :: Lens.Lens' CreateUsageLimit (Core.Maybe Types.UsageLimitBreachAction)
culBreachAction = Lens.field @"breachAction"
{-# DEPRECATED culBreachAction "Use generic-lens or generic-optics with 'breachAction' instead." #-}

-- | The time period that the amount applies to. A @weekly@ period begins on Sunday. The default is @monthly@ .
--
-- /Note:/ Consider using 'period' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
culPeriod :: Lens.Lens' CreateUsageLimit (Core.Maybe Types.UsageLimitPeriod)
culPeriod = Lens.field @"period"
{-# DEPRECATED culPeriod "Use generic-lens or generic-optics with 'period' instead." #-}

-- | A list of tag instances.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
culTags :: Lens.Lens' CreateUsageLimit (Core.Maybe [Types.Tag])
culTags = Lens.field @"tags"
{-# DEPRECATED culTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.AWSRequest CreateUsageLimit where
  type Rs CreateUsageLimit = Types.UsageLimit
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "CreateUsageLimit")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> (Core.toQueryValue "ClusterIdentifier" clusterIdentifier)
                Core.<> (Core.toQueryValue "FeatureType" featureType)
                Core.<> (Core.toQueryValue "LimitType" limitType)
                Core.<> (Core.toQueryValue "Amount" amount)
                Core.<> (Core.toQueryValue "BreachAction" Core.<$> breachAction)
                Core.<> (Core.toQueryValue "Period" Core.<$> period)
                Core.<> (Core.toQueryValue "Tags" (Core.toQueryList "Tag" Core.<$> tags))
            )
      }
  response =
    Response.receiveXMLWrapper
      "CreateUsageLimitResult"
      (\s h x -> Core.parseXML x)
