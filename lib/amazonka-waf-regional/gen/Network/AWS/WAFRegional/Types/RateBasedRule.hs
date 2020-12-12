{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.RateBasedRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.RateBasedRule
  ( RateBasedRule (..),

    -- * Smart constructor
    mkRateBasedRule,

    -- * Lenses
    rbrMetricName,
    rbrName,
    rbrRuleId,
    rbrMatchPredicates,
    rbrRateKey,
    rbrRateLimit,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WAFRegional.Types.Predicate
import Network.AWS.WAFRegional.Types.RateKey

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
  { metricName ::
      Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    ruleId :: Lude.Text,
    matchPredicates :: [Predicate],
    rateKey :: RateKey,
    rateLimit :: Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RateBasedRule' with the minimum fields required to make a request.
--
-- * 'matchPredicates' - The @Predicates@ object contains one @Predicate@ element for each 'ByteMatchSet' , 'IPSet' , or 'SqlInjectionMatchSet' object that you want to include in a @RateBasedRule@ .
-- * 'metricName' - A friendly name or description for the metrics for a @RateBasedRule@ . The name can contain only alphanumeric characters (A-Z, a-z, 0-9), with maximum length 128 and minimum length one. It can't contain whitespace or metric names reserved for AWS WAF, including "All" and "Default_Action." You can't change the name of the metric after you create the @RateBasedRule@ .
-- * 'name' - A friendly name or description for a @RateBasedRule@ . You can't change the name of a @RateBasedRule@ after you create it.
-- * 'rateKey' - The field that AWS WAF uses to determine if requests are likely arriving from single source and thus subject to rate monitoring. The only valid value for @RateKey@ is @IP@ . @IP@ indicates that requests arriving from the same IP address are subject to the @RateLimit@ that is specified in the @RateBasedRule@ .
-- * 'rateLimit' - The maximum number of requests, which have an identical value in the field specified by the @RateKey@ , allowed in a five-minute period. If the number of requests exceeds the @RateLimit@ and the other predicates specified in the rule are also met, AWS WAF triggers the action that is specified for this rule.
-- * 'ruleId' - A unique identifier for a @RateBasedRule@ . You use @RuleId@ to get more information about a @RateBasedRule@ (see 'GetRateBasedRule' ), update a @RateBasedRule@ (see 'UpdateRateBasedRule' ), insert a @RateBasedRule@ into a @WebACL@ or delete one from a @WebACL@ (see 'UpdateWebACL' ), or delete a @RateBasedRule@ from AWS WAF (see 'DeleteRateBasedRule' ).
mkRateBasedRule ::
  -- | 'ruleId'
  Lude.Text ->
  -- | 'rateKey'
  RateKey ->
  -- | 'rateLimit'
  Lude.Natural ->
  RateBasedRule
mkRateBasedRule pRuleId_ pRateKey_ pRateLimit_ =
  RateBasedRule'
    { metricName = Lude.Nothing,
      name = Lude.Nothing,
      ruleId = pRuleId_,
      matchPredicates = Lude.mempty,
      rateKey = pRateKey_,
      rateLimit = pRateLimit_
    }

-- | A friendly name or description for the metrics for a @RateBasedRule@ . The name can contain only alphanumeric characters (A-Z, a-z, 0-9), with maximum length 128 and minimum length one. It can't contain whitespace or metric names reserved for AWS WAF, including "All" and "Default_Action." You can't change the name of the metric after you create the @RateBasedRule@ .
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rbrMetricName :: Lens.Lens' RateBasedRule (Lude.Maybe Lude.Text)
rbrMetricName = Lens.lens (metricName :: RateBasedRule -> Lude.Maybe Lude.Text) (\s a -> s {metricName = a} :: RateBasedRule)
{-# DEPRECATED rbrMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

-- | A friendly name or description for a @RateBasedRule@ . You can't change the name of a @RateBasedRule@ after you create it.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rbrName :: Lens.Lens' RateBasedRule (Lude.Maybe Lude.Text)
rbrName = Lens.lens (name :: RateBasedRule -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: RateBasedRule)
{-# DEPRECATED rbrName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A unique identifier for a @RateBasedRule@ . You use @RuleId@ to get more information about a @RateBasedRule@ (see 'GetRateBasedRule' ), update a @RateBasedRule@ (see 'UpdateRateBasedRule' ), insert a @RateBasedRule@ into a @WebACL@ or delete one from a @WebACL@ (see 'UpdateWebACL' ), or delete a @RateBasedRule@ from AWS WAF (see 'DeleteRateBasedRule' ).
--
-- /Note:/ Consider using 'ruleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rbrRuleId :: Lens.Lens' RateBasedRule Lude.Text
rbrRuleId = Lens.lens (ruleId :: RateBasedRule -> Lude.Text) (\s a -> s {ruleId = a} :: RateBasedRule)
{-# DEPRECATED rbrRuleId "Use generic-lens or generic-optics with 'ruleId' instead." #-}

-- | The @Predicates@ object contains one @Predicate@ element for each 'ByteMatchSet' , 'IPSet' , or 'SqlInjectionMatchSet' object that you want to include in a @RateBasedRule@ .
--
-- /Note:/ Consider using 'matchPredicates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rbrMatchPredicates :: Lens.Lens' RateBasedRule [Predicate]
rbrMatchPredicates = Lens.lens (matchPredicates :: RateBasedRule -> [Predicate]) (\s a -> s {matchPredicates = a} :: RateBasedRule)
{-# DEPRECATED rbrMatchPredicates "Use generic-lens or generic-optics with 'matchPredicates' instead." #-}

-- | The field that AWS WAF uses to determine if requests are likely arriving from single source and thus subject to rate monitoring. The only valid value for @RateKey@ is @IP@ . @IP@ indicates that requests arriving from the same IP address are subject to the @RateLimit@ that is specified in the @RateBasedRule@ .
--
-- /Note:/ Consider using 'rateKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rbrRateKey :: Lens.Lens' RateBasedRule RateKey
rbrRateKey = Lens.lens (rateKey :: RateBasedRule -> RateKey) (\s a -> s {rateKey = a} :: RateBasedRule)
{-# DEPRECATED rbrRateKey "Use generic-lens or generic-optics with 'rateKey' instead." #-}

-- | The maximum number of requests, which have an identical value in the field specified by the @RateKey@ , allowed in a five-minute period. If the number of requests exceeds the @RateLimit@ and the other predicates specified in the rule are also met, AWS WAF triggers the action that is specified for this rule.
--
-- /Note:/ Consider using 'rateLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rbrRateLimit :: Lens.Lens' RateBasedRule Lude.Natural
rbrRateLimit = Lens.lens (rateLimit :: RateBasedRule -> Lude.Natural) (\s a -> s {rateLimit = a} :: RateBasedRule)
{-# DEPRECATED rbrRateLimit "Use generic-lens or generic-optics with 'rateLimit' instead." #-}

instance Lude.FromJSON RateBasedRule where
  parseJSON =
    Lude.withObject
      "RateBasedRule"
      ( \x ->
          RateBasedRule'
            Lude.<$> (x Lude..:? "MetricName")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..: "RuleId")
            Lude.<*> (x Lude..:? "MatchPredicates" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "RateKey")
            Lude.<*> (x Lude..: "RateLimit")
      )
