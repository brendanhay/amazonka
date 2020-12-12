{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.ActivatedRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.ActivatedRule
  ( ActivatedRule (..),

    -- * Smart constructor
    mkActivatedRule,

    -- * Lenses
    arOverrideAction,
    arAction,
    arExcludedRules,
    arType,
    arPriority,
    arRuleId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WAFRegional.Types.ExcludedRule
import Network.AWS.WAFRegional.Types.WafAction
import Network.AWS.WAFRegional.Types.WafOverrideAction
import Network.AWS.WAFRegional.Types.WafRuleType

-- | The @ActivatedRule@ object in an 'UpdateWebACL' request specifies a @Rule@ that you want to insert or delete, the priority of the @Rule@ in the @WebACL@ , and the action that you want AWS WAF to take when a web request matches the @Rule@ (@ALLOW@ , @BLOCK@ , or @COUNT@ ).
--
-- To specify whether to insert or delete a @Rule@ , use the @Action@ parameter in the 'WebACLUpdate' data type.
--
-- /See:/ 'mkActivatedRule' smart constructor.
data ActivatedRule = ActivatedRule'
  { overrideAction ::
      Lude.Maybe WafOverrideAction,
    action :: Lude.Maybe WafAction,
    excludedRules :: Lude.Maybe [ExcludedRule],
    type' :: Lude.Maybe WafRuleType,
    priority :: Lude.Int,
    ruleId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ActivatedRule' with the minimum fields required to make a request.
--
-- * 'action' - Specifies the action that CloudFront or AWS WAF takes when a web request matches the conditions in the @Rule@ . Valid values for @Action@ include the following:
--
--
--     * @ALLOW@ : CloudFront responds with the requested object.
--
--
--     * @BLOCK@ : CloudFront responds with an HTTP 403 (Forbidden) status code.
--
--
--     * @COUNT@ : AWS WAF increments a counter of requests that match the conditions in the rule and then continues to inspect the web request based on the remaining rules in the web ACL.
--
--
-- @ActivatedRule|OverrideAction@ applies only when updating or adding a @RuleGroup@ to a @WebACL@ . In this case, you do not use @ActivatedRule|Action@ . For all other update requests, @ActivatedRule|Action@ is used instead of @ActivatedRule|OverrideAction@ .
-- * 'excludedRules' - An array of rules to exclude from a rule group. This is applicable only when the @ActivatedRule@ refers to a @RuleGroup@ .
--
-- Sometimes it is necessary to troubleshoot rule groups that are blocking traffic unexpectedly (false positives). One troubleshooting technique is to identify the specific rule within the rule group that is blocking the legitimate traffic and then disable (exclude) that particular rule. You can exclude rules from both your own rule groups and AWS Marketplace rule groups that have been associated with a web ACL.
-- Specifying @ExcludedRules@ does not remove those rules from the rule group. Rather, it changes the action for the rules to @COUNT@ . Therefore, requests that match an @ExcludedRule@ are counted but not blocked. The @RuleGroup@ owner will receive COUNT metrics for each @ExcludedRule@ .
-- If you want to exclude rules from a rule group that is already associated with a web ACL, perform the following steps:
--
--     * Use the AWS WAF logs to identify the IDs of the rules that you want to exclude. For more information about the logs, see <https://docs.aws.amazon.com/waf/latest/developerguide/logging.html Logging Web ACL Traffic Information> .
--
--
--     * Submit an 'UpdateWebACL' request that has two actions:
--
--     * The first action deletes the existing rule group from the web ACL. That is, in the 'UpdateWebACL' request, the first @Updates:Action@ should be @DELETE@ and @Updates:ActivatedRule:RuleId@ should be the rule group that contains the rules that you want to exclude.
--
--
--     * The second action inserts the same rule group back in, but specifying the rules to exclude. That is, the second @Updates:Action@ should be @INSERT@ , @Updates:ActivatedRule:RuleId@ should be the rule group that you just removed, and @ExcludedRules@ should contain the rules that you want to exclude.
--
--
--
--
-- * 'overrideAction' - Use the @OverrideAction@ to test your @RuleGroup@ .
--
-- Any rule in a @RuleGroup@ can potentially block a request. If you set the @OverrideAction@ to @None@ , the @RuleGroup@ will block a request if any individual rule in the @RuleGroup@ matches the request and is configured to block that request. However if you first want to test the @RuleGroup@ , set the @OverrideAction@ to @Count@ . The @RuleGroup@ will then override any block action specified by individual rules contained within the group. Instead of blocking matching requests, those requests will be counted. You can view a record of counted requests using 'GetSampledRequests' .
-- @ActivatedRule|OverrideAction@ applies only when updating or adding a @RuleGroup@ to a @WebACL@ . In this case you do not use @ActivatedRule|Action@ . For all other update requests, @ActivatedRule|Action@ is used instead of @ActivatedRule|OverrideAction@ .
-- * 'priority' - Specifies the order in which the @Rules@ in a @WebACL@ are evaluated. Rules with a lower value for @Priority@ are evaluated before @Rules@ with a higher value. The value must be a unique integer. If you add multiple @Rules@ to a @WebACL@ , the values don't need to be consecutive.
-- * 'ruleId' - The @RuleId@ for a @Rule@ . You use @RuleId@ to get more information about a @Rule@ (see 'GetRule' ), update a @Rule@ (see 'UpdateRule' ), insert a @Rule@ into a @WebACL@ or delete a one from a @WebACL@ (see 'UpdateWebACL' ), or delete a @Rule@ from AWS WAF (see 'DeleteRule' ).
--
-- @RuleId@ is returned by 'CreateRule' and by 'ListRules' .
-- * 'type'' - The rule type, either @REGULAR@ , as defined by 'Rule' , @RATE_BASED@ , as defined by 'RateBasedRule' , or @GROUP@ , as defined by 'RuleGroup' . The default is REGULAR. Although this field is optional, be aware that if you try to add a RATE_BASED rule to a web ACL without setting the type, the 'UpdateWebACL' request will fail because the request tries to add a REGULAR rule with the specified ID, which does not exist.
mkActivatedRule ::
  -- | 'priority'
  Lude.Int ->
  -- | 'ruleId'
  Lude.Text ->
  ActivatedRule
mkActivatedRule pPriority_ pRuleId_ =
  ActivatedRule'
    { overrideAction = Lude.Nothing,
      action = Lude.Nothing,
      excludedRules = Lude.Nothing,
      type' = Lude.Nothing,
      priority = pPriority_,
      ruleId = pRuleId_
    }

-- | Use the @OverrideAction@ to test your @RuleGroup@ .
--
-- Any rule in a @RuleGroup@ can potentially block a request. If you set the @OverrideAction@ to @None@ , the @RuleGroup@ will block a request if any individual rule in the @RuleGroup@ matches the request and is configured to block that request. However if you first want to test the @RuleGroup@ , set the @OverrideAction@ to @Count@ . The @RuleGroup@ will then override any block action specified by individual rules contained within the group. Instead of blocking matching requests, those requests will be counted. You can view a record of counted requests using 'GetSampledRequests' .
-- @ActivatedRule|OverrideAction@ applies only when updating or adding a @RuleGroup@ to a @WebACL@ . In this case you do not use @ActivatedRule|Action@ . For all other update requests, @ActivatedRule|Action@ is used instead of @ActivatedRule|OverrideAction@ .
--
-- /Note:/ Consider using 'overrideAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arOverrideAction :: Lens.Lens' ActivatedRule (Lude.Maybe WafOverrideAction)
arOverrideAction = Lens.lens (overrideAction :: ActivatedRule -> Lude.Maybe WafOverrideAction) (\s a -> s {overrideAction = a} :: ActivatedRule)
{-# DEPRECATED arOverrideAction "Use generic-lens or generic-optics with 'overrideAction' instead." #-}

-- | Specifies the action that CloudFront or AWS WAF takes when a web request matches the conditions in the @Rule@ . Valid values for @Action@ include the following:
--
--
--     * @ALLOW@ : CloudFront responds with the requested object.
--
--
--     * @BLOCK@ : CloudFront responds with an HTTP 403 (Forbidden) status code.
--
--
--     * @COUNT@ : AWS WAF increments a counter of requests that match the conditions in the rule and then continues to inspect the web request based on the remaining rules in the web ACL.
--
--
-- @ActivatedRule|OverrideAction@ applies only when updating or adding a @RuleGroup@ to a @WebACL@ . In this case, you do not use @ActivatedRule|Action@ . For all other update requests, @ActivatedRule|Action@ is used instead of @ActivatedRule|OverrideAction@ .
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arAction :: Lens.Lens' ActivatedRule (Lude.Maybe WafAction)
arAction = Lens.lens (action :: ActivatedRule -> Lude.Maybe WafAction) (\s a -> s {action = a} :: ActivatedRule)
{-# DEPRECATED arAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | An array of rules to exclude from a rule group. This is applicable only when the @ActivatedRule@ refers to a @RuleGroup@ .
--
-- Sometimes it is necessary to troubleshoot rule groups that are blocking traffic unexpectedly (false positives). One troubleshooting technique is to identify the specific rule within the rule group that is blocking the legitimate traffic and then disable (exclude) that particular rule. You can exclude rules from both your own rule groups and AWS Marketplace rule groups that have been associated with a web ACL.
-- Specifying @ExcludedRules@ does not remove those rules from the rule group. Rather, it changes the action for the rules to @COUNT@ . Therefore, requests that match an @ExcludedRule@ are counted but not blocked. The @RuleGroup@ owner will receive COUNT metrics for each @ExcludedRule@ .
-- If you want to exclude rules from a rule group that is already associated with a web ACL, perform the following steps:
--
--     * Use the AWS WAF logs to identify the IDs of the rules that you want to exclude. For more information about the logs, see <https://docs.aws.amazon.com/waf/latest/developerguide/logging.html Logging Web ACL Traffic Information> .
--
--
--     * Submit an 'UpdateWebACL' request that has two actions:
--
--     * The first action deletes the existing rule group from the web ACL. That is, in the 'UpdateWebACL' request, the first @Updates:Action@ should be @DELETE@ and @Updates:ActivatedRule:RuleId@ should be the rule group that contains the rules that you want to exclude.
--
--
--     * The second action inserts the same rule group back in, but specifying the rules to exclude. That is, the second @Updates:Action@ should be @INSERT@ , @Updates:ActivatedRule:RuleId@ should be the rule group that you just removed, and @ExcludedRules@ should contain the rules that you want to exclude.
--
--
--
--
--
-- /Note:/ Consider using 'excludedRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arExcludedRules :: Lens.Lens' ActivatedRule (Lude.Maybe [ExcludedRule])
arExcludedRules = Lens.lens (excludedRules :: ActivatedRule -> Lude.Maybe [ExcludedRule]) (\s a -> s {excludedRules = a} :: ActivatedRule)
{-# DEPRECATED arExcludedRules "Use generic-lens or generic-optics with 'excludedRules' instead." #-}

-- | The rule type, either @REGULAR@ , as defined by 'Rule' , @RATE_BASED@ , as defined by 'RateBasedRule' , or @GROUP@ , as defined by 'RuleGroup' . The default is REGULAR. Although this field is optional, be aware that if you try to add a RATE_BASED rule to a web ACL without setting the type, the 'UpdateWebACL' request will fail because the request tries to add a REGULAR rule with the specified ID, which does not exist.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arType :: Lens.Lens' ActivatedRule (Lude.Maybe WafRuleType)
arType = Lens.lens (type' :: ActivatedRule -> Lude.Maybe WafRuleType) (\s a -> s {type' = a} :: ActivatedRule)
{-# DEPRECATED arType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | Specifies the order in which the @Rules@ in a @WebACL@ are evaluated. Rules with a lower value for @Priority@ are evaluated before @Rules@ with a higher value. The value must be a unique integer. If you add multiple @Rules@ to a @WebACL@ , the values don't need to be consecutive.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arPriority :: Lens.Lens' ActivatedRule Lude.Int
arPriority = Lens.lens (priority :: ActivatedRule -> Lude.Int) (\s a -> s {priority = a} :: ActivatedRule)
{-# DEPRECATED arPriority "Use generic-lens or generic-optics with 'priority' instead." #-}

-- | The @RuleId@ for a @Rule@ . You use @RuleId@ to get more information about a @Rule@ (see 'GetRule' ), update a @Rule@ (see 'UpdateRule' ), insert a @Rule@ into a @WebACL@ or delete a one from a @WebACL@ (see 'UpdateWebACL' ), or delete a @Rule@ from AWS WAF (see 'DeleteRule' ).
--
-- @RuleId@ is returned by 'CreateRule' and by 'ListRules' .
--
-- /Note:/ Consider using 'ruleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arRuleId :: Lens.Lens' ActivatedRule Lude.Text
arRuleId = Lens.lens (ruleId :: ActivatedRule -> Lude.Text) (\s a -> s {ruleId = a} :: ActivatedRule)
{-# DEPRECATED arRuleId "Use generic-lens or generic-optics with 'ruleId' instead." #-}

instance Lude.FromJSON ActivatedRule where
  parseJSON =
    Lude.withObject
      "ActivatedRule"
      ( \x ->
          ActivatedRule'
            Lude.<$> (x Lude..:? "OverrideAction")
            Lude.<*> (x Lude..:? "Action")
            Lude.<*> (x Lude..:? "ExcludedRules" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Type")
            Lude.<*> (x Lude..: "Priority")
            Lude.<*> (x Lude..: "RuleId")
      )

instance Lude.ToJSON ActivatedRule where
  toJSON ActivatedRule' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("OverrideAction" Lude..=) Lude.<$> overrideAction,
            ("Action" Lude..=) Lude.<$> action,
            ("ExcludedRules" Lude..=) Lude.<$> excludedRules,
            ("Type" Lude..=) Lude.<$> type',
            Lude.Just ("Priority" Lude..= priority),
            Lude.Just ("RuleId" Lude..= ruleId)
          ]
      )
