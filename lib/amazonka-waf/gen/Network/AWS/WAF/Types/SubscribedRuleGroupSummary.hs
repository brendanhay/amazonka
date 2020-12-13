{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.SubscribedRuleGroupSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.SubscribedRuleGroupSummary
  ( SubscribedRuleGroupSummary (..),

    -- * Smart constructor
    mkSubscribedRuleGroupSummary,

    -- * Lenses
    srgsRuleGroupId,
    srgsMetricName,
    srgsName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A summary of the rule groups you are subscribed to.
--
-- /See:/ 'mkSubscribedRuleGroupSummary' smart constructor.
data SubscribedRuleGroupSummary = SubscribedRuleGroupSummary'
  { -- | A unique identifier for a @RuleGroup@ .
    ruleGroupId :: Lude.Text,
    -- | A friendly name or description for the metrics for this @RuleGroup@ . The name can contain only alphanumeric characters (A-Z, a-z, 0-9), with maximum length 128 and minimum length one. It can't contain whitespace or metric names reserved for AWS WAF, including "All" and "Default_Action." You can't change the name of the metric after you create the @RuleGroup@ .
    metricName :: Lude.Text,
    -- | A friendly name or description of the @RuleGroup@ . You can't change the name of a @RuleGroup@ after you create it.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SubscribedRuleGroupSummary' with the minimum fields required to make a request.
--
-- * 'ruleGroupId' - A unique identifier for a @RuleGroup@ .
-- * 'metricName' - A friendly name or description for the metrics for this @RuleGroup@ . The name can contain only alphanumeric characters (A-Z, a-z, 0-9), with maximum length 128 and minimum length one. It can't contain whitespace or metric names reserved for AWS WAF, including "All" and "Default_Action." You can't change the name of the metric after you create the @RuleGroup@ .
-- * 'name' - A friendly name or description of the @RuleGroup@ . You can't change the name of a @RuleGroup@ after you create it.
mkSubscribedRuleGroupSummary ::
  -- | 'ruleGroupId'
  Lude.Text ->
  -- | 'metricName'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  SubscribedRuleGroupSummary
mkSubscribedRuleGroupSummary pRuleGroupId_ pMetricName_ pName_ =
  SubscribedRuleGroupSummary'
    { ruleGroupId = pRuleGroupId_,
      metricName = pMetricName_,
      name = pName_
    }

-- | A unique identifier for a @RuleGroup@ .
--
-- /Note:/ Consider using 'ruleGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srgsRuleGroupId :: Lens.Lens' SubscribedRuleGroupSummary Lude.Text
srgsRuleGroupId = Lens.lens (ruleGroupId :: SubscribedRuleGroupSummary -> Lude.Text) (\s a -> s {ruleGroupId = a} :: SubscribedRuleGroupSummary)
{-# DEPRECATED srgsRuleGroupId "Use generic-lens or generic-optics with 'ruleGroupId' instead." #-}

-- | A friendly name or description for the metrics for this @RuleGroup@ . The name can contain only alphanumeric characters (A-Z, a-z, 0-9), with maximum length 128 and minimum length one. It can't contain whitespace or metric names reserved for AWS WAF, including "All" and "Default_Action." You can't change the name of the metric after you create the @RuleGroup@ .
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srgsMetricName :: Lens.Lens' SubscribedRuleGroupSummary Lude.Text
srgsMetricName = Lens.lens (metricName :: SubscribedRuleGroupSummary -> Lude.Text) (\s a -> s {metricName = a} :: SubscribedRuleGroupSummary)
{-# DEPRECATED srgsMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

-- | A friendly name or description of the @RuleGroup@ . You can't change the name of a @RuleGroup@ after you create it.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srgsName :: Lens.Lens' SubscribedRuleGroupSummary Lude.Text
srgsName = Lens.lens (name :: SubscribedRuleGroupSummary -> Lude.Text) (\s a -> s {name = a} :: SubscribedRuleGroupSummary)
{-# DEPRECATED srgsName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON SubscribedRuleGroupSummary where
  parseJSON =
    Lude.withObject
      "SubscribedRuleGroupSummary"
      ( \x ->
          SubscribedRuleGroupSummary'
            Lude.<$> (x Lude..: "RuleGroupId")
            Lude.<*> (x Lude..: "MetricName")
            Lude.<*> (x Lude..: "Name")
      )
