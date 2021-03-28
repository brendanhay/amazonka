{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.Rule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WAF.Types.Rule
  ( Rule (..)
  -- * Smart constructor
  , mkRule
  -- * Lenses
  , rRuleId
  , rPredicates
  , rMetricName
  , rName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WAF.Types.MetricName as Types
import qualified Network.AWS.WAF.Types.Predicate as Types
import qualified Network.AWS.WAF.Types.ResourceId as Types
import qualified Network.AWS.WAF.Types.ResourceName as Types

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
  { ruleId :: Types.ResourceId
    -- ^ A unique identifier for a @Rule@ . You use @RuleId@ to get more information about a @Rule@ (see 'GetRule' ), update a @Rule@ (see 'UpdateRule' ), insert a @Rule@ into a @WebACL@ or delete a one from a @WebACL@ (see 'UpdateWebACL' ), or delete a @Rule@ from AWS WAF (see 'DeleteRule' ).
--
-- @RuleId@ is returned by 'CreateRule' and by 'ListRules' .
  , predicates :: [Types.Predicate]
    -- ^ The @Predicates@ object contains one @Predicate@ element for each 'ByteMatchSet' , 'IPSet' , or 'SqlInjectionMatchSet' object that you want to include in a @Rule@ .
  , metricName :: Core.Maybe Types.MetricName
    -- ^ A friendly name or description for the metrics for this @Rule@ . The name can contain only alphanumeric characters (A-Z, a-z, 0-9), with maximum length 128 and minimum length one. It can't contain whitespace or metric names reserved for AWS WAF, including "All" and "Default_Action." You can't change @MetricName@ after you create the @Rule@ .
  , name :: Core.Maybe Types.ResourceName
    -- ^ The friendly name or description for the @Rule@ . You can't change the name of a @Rule@ after you create it.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Rule' value with any optional fields omitted.
mkRule
    :: Types.ResourceId -- ^ 'ruleId'
    -> Rule
mkRule ruleId
  = Rule'{ruleId, predicates = Core.mempty,
          metricName = Core.Nothing, name = Core.Nothing}

-- | A unique identifier for a @Rule@ . You use @RuleId@ to get more information about a @Rule@ (see 'GetRule' ), update a @Rule@ (see 'UpdateRule' ), insert a @Rule@ into a @WebACL@ or delete a one from a @WebACL@ (see 'UpdateWebACL' ), or delete a @Rule@ from AWS WAF (see 'DeleteRule' ).
--
-- @RuleId@ is returned by 'CreateRule' and by 'ListRules' .
--
-- /Note:/ Consider using 'ruleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rRuleId :: Lens.Lens' Rule Types.ResourceId
rRuleId = Lens.field @"ruleId"
{-# INLINEABLE rRuleId #-}
{-# DEPRECATED ruleId "Use generic-lens or generic-optics with 'ruleId' instead"  #-}

-- | The @Predicates@ object contains one @Predicate@ element for each 'ByteMatchSet' , 'IPSet' , or 'SqlInjectionMatchSet' object that you want to include in a @Rule@ .
--
-- /Note:/ Consider using 'predicates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rPredicates :: Lens.Lens' Rule [Types.Predicate]
rPredicates = Lens.field @"predicates"
{-# INLINEABLE rPredicates #-}
{-# DEPRECATED predicates "Use generic-lens or generic-optics with 'predicates' instead"  #-}

-- | A friendly name or description for the metrics for this @Rule@ . The name can contain only alphanumeric characters (A-Z, a-z, 0-9), with maximum length 128 and minimum length one. It can't contain whitespace or metric names reserved for AWS WAF, including "All" and "Default_Action." You can't change @MetricName@ after you create the @Rule@ .
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rMetricName :: Lens.Lens' Rule (Core.Maybe Types.MetricName)
rMetricName = Lens.field @"metricName"
{-# INLINEABLE rMetricName #-}
{-# DEPRECATED metricName "Use generic-lens or generic-optics with 'metricName' instead"  #-}

-- | The friendly name or description for the @Rule@ . You can't change the name of a @Rule@ after you create it.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rName :: Lens.Lens' Rule (Core.Maybe Types.ResourceName)
rName = Lens.field @"name"
{-# INLINEABLE rName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromJSON Rule where
        parseJSON
          = Core.withObject "Rule" Core.$
              \ x ->
                Rule' Core.<$>
                  (x Core..: "RuleId") Core.<*>
                    x Core..:? "Predicates" Core..!= Core.mempty
                    Core.<*> x Core..:? "MetricName"
                    Core.<*> x Core..:? "Name"
