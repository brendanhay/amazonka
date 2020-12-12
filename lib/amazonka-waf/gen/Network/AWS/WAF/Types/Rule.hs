{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.Rule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.Rule
  ( Rule (..),

    -- * Smart constructor
    mkRule,

    -- * Lenses
    rMetricName,
    rName,
    rRuleId,
    rPredicates,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WAF.Types.Predicate

-- | A combination of 'ByteMatchSet' , 'IPSet' , and/or 'SqlInjectionMatchSet' objects that identify the web requests that you want to allow, block, or count. For example, you might create a @Rule@ that includes the following predicates:
--
--
--     * An @IPSet@ that causes AWS WAF to search for web requests that originate from the IP address @192.0.2.44@
--
--
--     * A @ByteMatchSet@ that causes AWS WAF to search for web requests for which the value of the @User-Agent@ header is @BadBot@ .
--
--
-- To match the settings in this @Rule@ , a request must originate from @192.0.2.44@ AND include a @User-Agent@ header for which the value is @BadBot@ .
--
-- /See:/ 'mkRule' smart constructor.
data Rule = Rule'
  { metricName :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    ruleId :: Lude.Text,
    predicates :: [Predicate]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Rule' with the minimum fields required to make a request.
--
-- * 'metricName' - A friendly name or description for the metrics for this @Rule@ . The name can contain only alphanumeric characters (A-Z, a-z, 0-9), with maximum length 128 and minimum length one. It can't contain whitespace or metric names reserved for AWS WAF, including "All" and "Default_Action." You can't change @MetricName@ after you create the @Rule@ .
-- * 'name' - The friendly name or description for the @Rule@ . You can't change the name of a @Rule@ after you create it.
-- * 'predicates' - The @Predicates@ object contains one @Predicate@ element for each 'ByteMatchSet' , 'IPSet' , or 'SqlInjectionMatchSet' object that you want to include in a @Rule@ .
-- * 'ruleId' - A unique identifier for a @Rule@ . You use @RuleId@ to get more information about a @Rule@ (see 'GetRule' ), update a @Rule@ (see 'UpdateRule' ), insert a @Rule@ into a @WebACL@ or delete a one from a @WebACL@ (see 'UpdateWebACL' ), or delete a @Rule@ from AWS WAF (see 'DeleteRule' ).
--
-- @RuleId@ is returned by 'CreateRule' and by 'ListRules' .
mkRule ::
  -- | 'ruleId'
  Lude.Text ->
  Rule
mkRule pRuleId_ =
  Rule'
    { metricName = Lude.Nothing,
      name = Lude.Nothing,
      ruleId = pRuleId_,
      predicates = Lude.mempty
    }

-- | A friendly name or description for the metrics for this @Rule@ . The name can contain only alphanumeric characters (A-Z, a-z, 0-9), with maximum length 128 and minimum length one. It can't contain whitespace or metric names reserved for AWS WAF, including "All" and "Default_Action." You can't change @MetricName@ after you create the @Rule@ .
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rMetricName :: Lens.Lens' Rule (Lude.Maybe Lude.Text)
rMetricName = Lens.lens (metricName :: Rule -> Lude.Maybe Lude.Text) (\s a -> s {metricName = a} :: Rule)
{-# DEPRECATED rMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

-- | The friendly name or description for the @Rule@ . You can't change the name of a @Rule@ after you create it.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rName :: Lens.Lens' Rule (Lude.Maybe Lude.Text)
rName = Lens.lens (name :: Rule -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Rule)
{-# DEPRECATED rName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A unique identifier for a @Rule@ . You use @RuleId@ to get more information about a @Rule@ (see 'GetRule' ), update a @Rule@ (see 'UpdateRule' ), insert a @Rule@ into a @WebACL@ or delete a one from a @WebACL@ (see 'UpdateWebACL' ), or delete a @Rule@ from AWS WAF (see 'DeleteRule' ).
--
-- @RuleId@ is returned by 'CreateRule' and by 'ListRules' .
--
-- /Note:/ Consider using 'ruleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rRuleId :: Lens.Lens' Rule Lude.Text
rRuleId = Lens.lens (ruleId :: Rule -> Lude.Text) (\s a -> s {ruleId = a} :: Rule)
{-# DEPRECATED rRuleId "Use generic-lens or generic-optics with 'ruleId' instead." #-}

-- | The @Predicates@ object contains one @Predicate@ element for each 'ByteMatchSet' , 'IPSet' , or 'SqlInjectionMatchSet' object that you want to include in a @Rule@ .
--
-- /Note:/ Consider using 'predicates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rPredicates :: Lens.Lens' Rule [Predicate]
rPredicates = Lens.lens (predicates :: Rule -> [Predicate]) (\s a -> s {predicates = a} :: Rule)
{-# DEPRECATED rPredicates "Use generic-lens or generic-optics with 'predicates' instead." #-}

instance Lude.FromJSON Rule where
  parseJSON =
    Lude.withObject
      "Rule"
      ( \x ->
          Rule'
            Lude.<$> (x Lude..:? "MetricName")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..: "RuleId")
            Lude.<*> (x Lude..:? "Predicates" Lude..!= Lude.mempty)
      )
