{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.RateBasedRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WAF.Types.RateBasedRule
  ( RateBasedRule (..)
  -- * Smart constructor
  , mkRateBasedRule
  -- * Lenses
  , rbrRuleId
  , rbrMatchPredicates
  , rbrRateKey
  , rbrRateLimit
  , rbrMetricName
  , rbrName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WAF.Types.MetricName as Types
import qualified Network.AWS.WAF.Types.Predicate as Types
import qualified Network.AWS.WAF.Types.RateKey as Types
import qualified Network.AWS.WAF.Types.ResourceId as Types
import qualified Network.AWS.WAF.Types.ResourceName as Types

-- | A @RateBasedRule@ is identical to a regular 'Rule' , with one addition: a @RateBasedRule@ counts the number of requests that arrive from a specified IP address every five minutes. For example, based on recent requests that you've seen from an attacker, you might create a @RateBasedRule@ that includes the following conditions: 
--
--
--     * The requests come from 192.0.2.44.
--
--
--     * They contain the value @BadBot@ in the @User-Agent@ header.
--
--
-- In the rule, you also define the rate limit as 1,000.
-- Requests that meet both of these conditions and exceed 1,000 requests every five minutes trigger the rule's action (block or count), which is defined in the web ACL.
--
-- /See:/ 'mkRateBasedRule' smart constructor.
data RateBasedRule = RateBasedRule'
  { ruleId :: Types.ResourceId
    -- ^ A unique identifier for a @RateBasedRule@ . You use @RuleId@ to get more information about a @RateBasedRule@ (see 'GetRateBasedRule' ), update a @RateBasedRule@ (see 'UpdateRateBasedRule' ), insert a @RateBasedRule@ into a @WebACL@ or delete one from a @WebACL@ (see 'UpdateWebACL' ), or delete a @RateBasedRule@ from AWS WAF (see 'DeleteRateBasedRule' ).
  , matchPredicates :: [Types.Predicate]
    -- ^ The @Predicates@ object contains one @Predicate@ element for each 'ByteMatchSet' , 'IPSet' , or 'SqlInjectionMatchSet' object that you want to include in a @RateBasedRule@ .
  , rateKey :: Types.RateKey
    -- ^ The field that AWS WAF uses to determine if requests are likely arriving from single source and thus subject to rate monitoring. The only valid value for @RateKey@ is @IP@ . @IP@ indicates that requests arriving from the same IP address are subject to the @RateLimit@ that is specified in the @RateBasedRule@ .
  , rateLimit :: Core.Natural
    -- ^ The maximum number of requests, which have an identical value in the field specified by the @RateKey@ , allowed in a five-minute period. If the number of requests exceeds the @RateLimit@ and the other predicates specified in the rule are also met, AWS WAF triggers the action that is specified for this rule.
  , metricName :: Core.Maybe Types.MetricName
    -- ^ A friendly name or description for the metrics for a @RateBasedRule@ . The name can contain only alphanumeric characters (A-Z, a-z, 0-9), with maximum length 128 and minimum length one. It can't contain whitespace or metric names reserved for AWS WAF, including "All" and "Default_Action." You can't change the name of the metric after you create the @RateBasedRule@ .
  , name :: Core.Maybe Types.ResourceName
    -- ^ A friendly name or description for a @RateBasedRule@ . You can't change the name of a @RateBasedRule@ after you create it.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RateBasedRule' value with any optional fields omitted.
mkRateBasedRule
    :: Types.ResourceId -- ^ 'ruleId'
    -> Types.RateKey -- ^ 'rateKey'
    -> Core.Natural -- ^ 'rateLimit'
    -> RateBasedRule
mkRateBasedRule ruleId rateKey rateLimit
  = RateBasedRule'{ruleId, matchPredicates = Core.mempty, rateKey,
                   rateLimit, metricName = Core.Nothing, name = Core.Nothing}

-- | A unique identifier for a @RateBasedRule@ . You use @RuleId@ to get more information about a @RateBasedRule@ (see 'GetRateBasedRule' ), update a @RateBasedRule@ (see 'UpdateRateBasedRule' ), insert a @RateBasedRule@ into a @WebACL@ or delete one from a @WebACL@ (see 'UpdateWebACL' ), or delete a @RateBasedRule@ from AWS WAF (see 'DeleteRateBasedRule' ).
--
-- /Note:/ Consider using 'ruleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rbrRuleId :: Lens.Lens' RateBasedRule Types.ResourceId
rbrRuleId = Lens.field @"ruleId"
{-# INLINEABLE rbrRuleId #-}
{-# DEPRECATED ruleId "Use generic-lens or generic-optics with 'ruleId' instead"  #-}

-- | The @Predicates@ object contains one @Predicate@ element for each 'ByteMatchSet' , 'IPSet' , or 'SqlInjectionMatchSet' object that you want to include in a @RateBasedRule@ .
--
-- /Note:/ Consider using 'matchPredicates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rbrMatchPredicates :: Lens.Lens' RateBasedRule [Types.Predicate]
rbrMatchPredicates = Lens.field @"matchPredicates"
{-# INLINEABLE rbrMatchPredicates #-}
{-# DEPRECATED matchPredicates "Use generic-lens or generic-optics with 'matchPredicates' instead"  #-}

-- | The field that AWS WAF uses to determine if requests are likely arriving from single source and thus subject to rate monitoring. The only valid value for @RateKey@ is @IP@ . @IP@ indicates that requests arriving from the same IP address are subject to the @RateLimit@ that is specified in the @RateBasedRule@ .
--
-- /Note:/ Consider using 'rateKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rbrRateKey :: Lens.Lens' RateBasedRule Types.RateKey
rbrRateKey = Lens.field @"rateKey"
{-# INLINEABLE rbrRateKey #-}
{-# DEPRECATED rateKey "Use generic-lens or generic-optics with 'rateKey' instead"  #-}

-- | The maximum number of requests, which have an identical value in the field specified by the @RateKey@ , allowed in a five-minute period. If the number of requests exceeds the @RateLimit@ and the other predicates specified in the rule are also met, AWS WAF triggers the action that is specified for this rule.
--
-- /Note:/ Consider using 'rateLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rbrRateLimit :: Lens.Lens' RateBasedRule Core.Natural
rbrRateLimit = Lens.field @"rateLimit"
{-# INLINEABLE rbrRateLimit #-}
{-# DEPRECATED rateLimit "Use generic-lens or generic-optics with 'rateLimit' instead"  #-}

-- | A friendly name or description for the metrics for a @RateBasedRule@ . The name can contain only alphanumeric characters (A-Z, a-z, 0-9), with maximum length 128 and minimum length one. It can't contain whitespace or metric names reserved for AWS WAF, including "All" and "Default_Action." You can't change the name of the metric after you create the @RateBasedRule@ .
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rbrMetricName :: Lens.Lens' RateBasedRule (Core.Maybe Types.MetricName)
rbrMetricName = Lens.field @"metricName"
{-# INLINEABLE rbrMetricName #-}
{-# DEPRECATED metricName "Use generic-lens or generic-optics with 'metricName' instead"  #-}

-- | A friendly name or description for a @RateBasedRule@ . You can't change the name of a @RateBasedRule@ after you create it.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rbrName :: Lens.Lens' RateBasedRule (Core.Maybe Types.ResourceName)
rbrName = Lens.field @"name"
{-# INLINEABLE rbrName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromJSON RateBasedRule where
        parseJSON
          = Core.withObject "RateBasedRule" Core.$
              \ x ->
                RateBasedRule' Core.<$>
                  (x Core..: "RuleId") Core.<*>
                    x Core..:? "MatchPredicates" Core..!= Core.mempty
                    Core.<*> x Core..: "RateKey"
                    Core.<*> x Core..: "RateLimit"
                    Core.<*> x Core..:? "MetricName"
                    Core.<*> x Core..:? "Name"
