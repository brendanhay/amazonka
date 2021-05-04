{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.ActivatedRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.ActivatedRule where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WAFRegional.Types.ExcludedRule
import Network.AWS.WAFRegional.Types.WafAction
import Network.AWS.WAFRegional.Types.WafOverrideAction
import Network.AWS.WAFRegional.Types.WafRuleType

-- | This is __AWS WAF Classic__ documentation. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic>
-- in the developer guide.
--
-- __For the latest version of AWS WAF__, use the AWS WAFV2 API and see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide>.
-- With the latest version, AWS WAF has a single set of endpoints for
-- regional and global use.
--
-- The @ActivatedRule@ object in an UpdateWebACL request specifies a @Rule@
-- that you want to insert or delete, the priority of the @Rule@ in the
-- @WebACL@, and the action that you want AWS WAF to take when a web
-- request matches the @Rule@ (@ALLOW@, @BLOCK@, or @COUNT@).
--
-- To specify whether to insert or delete a @Rule@, use the @Action@
-- parameter in the WebACLUpdate data type.
--
-- /See:/ 'newActivatedRule' smart constructor.
data ActivatedRule = ActivatedRule'
  { -- | An array of rules to exclude from a rule group. This is applicable only
    -- when the @ActivatedRule@ refers to a @RuleGroup@.
    --
    -- Sometimes it is necessary to troubleshoot rule groups that are blocking
    -- traffic unexpectedly (false positives). One troubleshooting technique is
    -- to identify the specific rule within the rule group that is blocking the
    -- legitimate traffic and then disable (exclude) that particular rule. You
    -- can exclude rules from both your own rule groups and AWS Marketplace
    -- rule groups that have been associated with a web ACL.
    --
    -- Specifying @ExcludedRules@ does not remove those rules from the rule
    -- group. Rather, it changes the action for the rules to @COUNT@.
    -- Therefore, requests that match an @ExcludedRule@ are counted but not
    -- blocked. The @RuleGroup@ owner will receive COUNT metrics for each
    -- @ExcludedRule@.
    --
    -- If you want to exclude rules from a rule group that is already
    -- associated with a web ACL, perform the following steps:
    --
    -- 1.  Use the AWS WAF logs to identify the IDs of the rules that you want
    --     to exclude. For more information about the logs, see
    --     <https://docs.aws.amazon.com/waf/latest/developerguide/logging.html Logging Web ACL Traffic Information>.
    --
    -- 2.  Submit an UpdateWebACL request that has two actions:
    --
    --     -   The first action deletes the existing rule group from the web
    --         ACL. That is, in the UpdateWebACL request, the first
    --         @Updates:Action@ should be @DELETE@ and
    --         @Updates:ActivatedRule:RuleId@ should be the rule group that
    --         contains the rules that you want to exclude.
    --
    --     -   The second action inserts the same rule group back in, but
    --         specifying the rules to exclude. That is, the second
    --         @Updates:Action@ should be @INSERT@,
    --         @Updates:ActivatedRule:RuleId@ should be the rule group that you
    --         just removed, and @ExcludedRules@ should contain the rules that
    --         you want to exclude.
    excludedRules :: Prelude.Maybe [ExcludedRule],
    -- | Use the @OverrideAction@ to test your @RuleGroup@.
    --
    -- Any rule in a @RuleGroup@ can potentially block a request. If you set
    -- the @OverrideAction@ to @None@, the @RuleGroup@ will block a request if
    -- any individual rule in the @RuleGroup@ matches the request and is
    -- configured to block that request. However if you first want to test the
    -- @RuleGroup@, set the @OverrideAction@ to @Count@. The @RuleGroup@ will
    -- then override any block action specified by individual rules contained
    -- within the group. Instead of blocking matching requests, those requests
    -- will be counted. You can view a record of counted requests using
    -- GetSampledRequests.
    --
    -- @ActivatedRule|OverrideAction@ applies only when updating or adding a
    -- @RuleGroup@ to a @WebACL@. In this case you do not use
    -- @ActivatedRule|Action@. For all other update requests,
    -- @ActivatedRule|Action@ is used instead of
    -- @ActivatedRule|OverrideAction@.
    overrideAction :: Prelude.Maybe WafOverrideAction,
    -- | Specifies the action that CloudFront or AWS WAF takes when a web request
    -- matches the conditions in the @Rule@. Valid values for @Action@ include
    -- the following:
    --
    -- -   @ALLOW@: CloudFront responds with the requested object.
    --
    -- -   @BLOCK@: CloudFront responds with an HTTP 403 (Forbidden) status
    --     code.
    --
    -- -   @COUNT@: AWS WAF increments a counter of requests that match the
    --     conditions in the rule and then continues to inspect the web request
    --     based on the remaining rules in the web ACL.
    --
    -- @ActivatedRule|OverrideAction@ applies only when updating or adding a
    -- @RuleGroup@ to a @WebACL@. In this case, you do not use
    -- @ActivatedRule|Action@. For all other update requests,
    -- @ActivatedRule|Action@ is used instead of
    -- @ActivatedRule|OverrideAction@.
    action :: Prelude.Maybe WafAction,
    -- | The rule type, either @REGULAR@, as defined by Rule, @RATE_BASED@, as
    -- defined by RateBasedRule, or @GROUP@, as defined by RuleGroup. The
    -- default is REGULAR. Although this field is optional, be aware that if
    -- you try to add a RATE_BASED rule to a web ACL without setting the type,
    -- the UpdateWebACL request will fail because the request tries to add a
    -- REGULAR rule with the specified ID, which does not exist.
    type' :: Prelude.Maybe WafRuleType,
    -- | Specifies the order in which the @Rules@ in a @WebACL@ are evaluated.
    -- Rules with a lower value for @Priority@ are evaluated before @Rules@
    -- with a higher value. The value must be a unique integer. If you add
    -- multiple @Rules@ to a @WebACL@, the values don\'t need to be
    -- consecutive.
    priority :: Prelude.Int,
    -- | The @RuleId@ for a @Rule@. You use @RuleId@ to get more information
    -- about a @Rule@ (see GetRule), update a @Rule@ (see UpdateRule), insert a
    -- @Rule@ into a @WebACL@ or delete a one from a @WebACL@ (see
    -- UpdateWebACL), or delete a @Rule@ from AWS WAF (see DeleteRule).
    --
    -- @RuleId@ is returned by CreateRule and by ListRules.
    ruleId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ActivatedRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'excludedRules', 'activatedRule_excludedRules' - An array of rules to exclude from a rule group. This is applicable only
-- when the @ActivatedRule@ refers to a @RuleGroup@.
--
-- Sometimes it is necessary to troubleshoot rule groups that are blocking
-- traffic unexpectedly (false positives). One troubleshooting technique is
-- to identify the specific rule within the rule group that is blocking the
-- legitimate traffic and then disable (exclude) that particular rule. You
-- can exclude rules from both your own rule groups and AWS Marketplace
-- rule groups that have been associated with a web ACL.
--
-- Specifying @ExcludedRules@ does not remove those rules from the rule
-- group. Rather, it changes the action for the rules to @COUNT@.
-- Therefore, requests that match an @ExcludedRule@ are counted but not
-- blocked. The @RuleGroup@ owner will receive COUNT metrics for each
-- @ExcludedRule@.
--
-- If you want to exclude rules from a rule group that is already
-- associated with a web ACL, perform the following steps:
--
-- 1.  Use the AWS WAF logs to identify the IDs of the rules that you want
--     to exclude. For more information about the logs, see
--     <https://docs.aws.amazon.com/waf/latest/developerguide/logging.html Logging Web ACL Traffic Information>.
--
-- 2.  Submit an UpdateWebACL request that has two actions:
--
--     -   The first action deletes the existing rule group from the web
--         ACL. That is, in the UpdateWebACL request, the first
--         @Updates:Action@ should be @DELETE@ and
--         @Updates:ActivatedRule:RuleId@ should be the rule group that
--         contains the rules that you want to exclude.
--
--     -   The second action inserts the same rule group back in, but
--         specifying the rules to exclude. That is, the second
--         @Updates:Action@ should be @INSERT@,
--         @Updates:ActivatedRule:RuleId@ should be the rule group that you
--         just removed, and @ExcludedRules@ should contain the rules that
--         you want to exclude.
--
-- 'overrideAction', 'activatedRule_overrideAction' - Use the @OverrideAction@ to test your @RuleGroup@.
--
-- Any rule in a @RuleGroup@ can potentially block a request. If you set
-- the @OverrideAction@ to @None@, the @RuleGroup@ will block a request if
-- any individual rule in the @RuleGroup@ matches the request and is
-- configured to block that request. However if you first want to test the
-- @RuleGroup@, set the @OverrideAction@ to @Count@. The @RuleGroup@ will
-- then override any block action specified by individual rules contained
-- within the group. Instead of blocking matching requests, those requests
-- will be counted. You can view a record of counted requests using
-- GetSampledRequests.
--
-- @ActivatedRule|OverrideAction@ applies only when updating or adding a
-- @RuleGroup@ to a @WebACL@. In this case you do not use
-- @ActivatedRule|Action@. For all other update requests,
-- @ActivatedRule|Action@ is used instead of
-- @ActivatedRule|OverrideAction@.
--
-- 'action', 'activatedRule_action' - Specifies the action that CloudFront or AWS WAF takes when a web request
-- matches the conditions in the @Rule@. Valid values for @Action@ include
-- the following:
--
-- -   @ALLOW@: CloudFront responds with the requested object.
--
-- -   @BLOCK@: CloudFront responds with an HTTP 403 (Forbidden) status
--     code.
--
-- -   @COUNT@: AWS WAF increments a counter of requests that match the
--     conditions in the rule and then continues to inspect the web request
--     based on the remaining rules in the web ACL.
--
-- @ActivatedRule|OverrideAction@ applies only when updating or adding a
-- @RuleGroup@ to a @WebACL@. In this case, you do not use
-- @ActivatedRule|Action@. For all other update requests,
-- @ActivatedRule|Action@ is used instead of
-- @ActivatedRule|OverrideAction@.
--
-- 'type'', 'activatedRule_type' - The rule type, either @REGULAR@, as defined by Rule, @RATE_BASED@, as
-- defined by RateBasedRule, or @GROUP@, as defined by RuleGroup. The
-- default is REGULAR. Although this field is optional, be aware that if
-- you try to add a RATE_BASED rule to a web ACL without setting the type,
-- the UpdateWebACL request will fail because the request tries to add a
-- REGULAR rule with the specified ID, which does not exist.
--
-- 'priority', 'activatedRule_priority' - Specifies the order in which the @Rules@ in a @WebACL@ are evaluated.
-- Rules with a lower value for @Priority@ are evaluated before @Rules@
-- with a higher value. The value must be a unique integer. If you add
-- multiple @Rules@ to a @WebACL@, the values don\'t need to be
-- consecutive.
--
-- 'ruleId', 'activatedRule_ruleId' - The @RuleId@ for a @Rule@. You use @RuleId@ to get more information
-- about a @Rule@ (see GetRule), update a @Rule@ (see UpdateRule), insert a
-- @Rule@ into a @WebACL@ or delete a one from a @WebACL@ (see
-- UpdateWebACL), or delete a @Rule@ from AWS WAF (see DeleteRule).
--
-- @RuleId@ is returned by CreateRule and by ListRules.
newActivatedRule ::
  -- | 'priority'
  Prelude.Int ->
  -- | 'ruleId'
  Prelude.Text ->
  ActivatedRule
newActivatedRule pPriority_ pRuleId_ =
  ActivatedRule'
    { excludedRules = Prelude.Nothing,
      overrideAction = Prelude.Nothing,
      action = Prelude.Nothing,
      type' = Prelude.Nothing,
      priority = pPriority_,
      ruleId = pRuleId_
    }

-- | An array of rules to exclude from a rule group. This is applicable only
-- when the @ActivatedRule@ refers to a @RuleGroup@.
--
-- Sometimes it is necessary to troubleshoot rule groups that are blocking
-- traffic unexpectedly (false positives). One troubleshooting technique is
-- to identify the specific rule within the rule group that is blocking the
-- legitimate traffic and then disable (exclude) that particular rule. You
-- can exclude rules from both your own rule groups and AWS Marketplace
-- rule groups that have been associated with a web ACL.
--
-- Specifying @ExcludedRules@ does not remove those rules from the rule
-- group. Rather, it changes the action for the rules to @COUNT@.
-- Therefore, requests that match an @ExcludedRule@ are counted but not
-- blocked. The @RuleGroup@ owner will receive COUNT metrics for each
-- @ExcludedRule@.
--
-- If you want to exclude rules from a rule group that is already
-- associated with a web ACL, perform the following steps:
--
-- 1.  Use the AWS WAF logs to identify the IDs of the rules that you want
--     to exclude. For more information about the logs, see
--     <https://docs.aws.amazon.com/waf/latest/developerguide/logging.html Logging Web ACL Traffic Information>.
--
-- 2.  Submit an UpdateWebACL request that has two actions:
--
--     -   The first action deletes the existing rule group from the web
--         ACL. That is, in the UpdateWebACL request, the first
--         @Updates:Action@ should be @DELETE@ and
--         @Updates:ActivatedRule:RuleId@ should be the rule group that
--         contains the rules that you want to exclude.
--
--     -   The second action inserts the same rule group back in, but
--         specifying the rules to exclude. That is, the second
--         @Updates:Action@ should be @INSERT@,
--         @Updates:ActivatedRule:RuleId@ should be the rule group that you
--         just removed, and @ExcludedRules@ should contain the rules that
--         you want to exclude.
activatedRule_excludedRules :: Lens.Lens' ActivatedRule (Prelude.Maybe [ExcludedRule])
activatedRule_excludedRules = Lens.lens (\ActivatedRule' {excludedRules} -> excludedRules) (\s@ActivatedRule' {} a -> s {excludedRules = a} :: ActivatedRule) Prelude.. Lens.mapping Prelude._Coerce

-- | Use the @OverrideAction@ to test your @RuleGroup@.
--
-- Any rule in a @RuleGroup@ can potentially block a request. If you set
-- the @OverrideAction@ to @None@, the @RuleGroup@ will block a request if
-- any individual rule in the @RuleGroup@ matches the request and is
-- configured to block that request. However if you first want to test the
-- @RuleGroup@, set the @OverrideAction@ to @Count@. The @RuleGroup@ will
-- then override any block action specified by individual rules contained
-- within the group. Instead of blocking matching requests, those requests
-- will be counted. You can view a record of counted requests using
-- GetSampledRequests.
--
-- @ActivatedRule|OverrideAction@ applies only when updating or adding a
-- @RuleGroup@ to a @WebACL@. In this case you do not use
-- @ActivatedRule|Action@. For all other update requests,
-- @ActivatedRule|Action@ is used instead of
-- @ActivatedRule|OverrideAction@.
activatedRule_overrideAction :: Lens.Lens' ActivatedRule (Prelude.Maybe WafOverrideAction)
activatedRule_overrideAction = Lens.lens (\ActivatedRule' {overrideAction} -> overrideAction) (\s@ActivatedRule' {} a -> s {overrideAction = a} :: ActivatedRule)

-- | Specifies the action that CloudFront or AWS WAF takes when a web request
-- matches the conditions in the @Rule@. Valid values for @Action@ include
-- the following:
--
-- -   @ALLOW@: CloudFront responds with the requested object.
--
-- -   @BLOCK@: CloudFront responds with an HTTP 403 (Forbidden) status
--     code.
--
-- -   @COUNT@: AWS WAF increments a counter of requests that match the
--     conditions in the rule and then continues to inspect the web request
--     based on the remaining rules in the web ACL.
--
-- @ActivatedRule|OverrideAction@ applies only when updating or adding a
-- @RuleGroup@ to a @WebACL@. In this case, you do not use
-- @ActivatedRule|Action@. For all other update requests,
-- @ActivatedRule|Action@ is used instead of
-- @ActivatedRule|OverrideAction@.
activatedRule_action :: Lens.Lens' ActivatedRule (Prelude.Maybe WafAction)
activatedRule_action = Lens.lens (\ActivatedRule' {action} -> action) (\s@ActivatedRule' {} a -> s {action = a} :: ActivatedRule)

-- | The rule type, either @REGULAR@, as defined by Rule, @RATE_BASED@, as
-- defined by RateBasedRule, or @GROUP@, as defined by RuleGroup. The
-- default is REGULAR. Although this field is optional, be aware that if
-- you try to add a RATE_BASED rule to a web ACL without setting the type,
-- the UpdateWebACL request will fail because the request tries to add a
-- REGULAR rule with the specified ID, which does not exist.
activatedRule_type :: Lens.Lens' ActivatedRule (Prelude.Maybe WafRuleType)
activatedRule_type = Lens.lens (\ActivatedRule' {type'} -> type') (\s@ActivatedRule' {} a -> s {type' = a} :: ActivatedRule)

-- | Specifies the order in which the @Rules@ in a @WebACL@ are evaluated.
-- Rules with a lower value for @Priority@ are evaluated before @Rules@
-- with a higher value. The value must be a unique integer. If you add
-- multiple @Rules@ to a @WebACL@, the values don\'t need to be
-- consecutive.
activatedRule_priority :: Lens.Lens' ActivatedRule Prelude.Int
activatedRule_priority = Lens.lens (\ActivatedRule' {priority} -> priority) (\s@ActivatedRule' {} a -> s {priority = a} :: ActivatedRule)

-- | The @RuleId@ for a @Rule@. You use @RuleId@ to get more information
-- about a @Rule@ (see GetRule), update a @Rule@ (see UpdateRule), insert a
-- @Rule@ into a @WebACL@ or delete a one from a @WebACL@ (see
-- UpdateWebACL), or delete a @Rule@ from AWS WAF (see DeleteRule).
--
-- @RuleId@ is returned by CreateRule and by ListRules.
activatedRule_ruleId :: Lens.Lens' ActivatedRule Prelude.Text
activatedRule_ruleId = Lens.lens (\ActivatedRule' {ruleId} -> ruleId) (\s@ActivatedRule' {} a -> s {ruleId = a} :: ActivatedRule)

instance Prelude.FromJSON ActivatedRule where
  parseJSON =
    Prelude.withObject
      "ActivatedRule"
      ( \x ->
          ActivatedRule'
            Prelude.<$> ( x Prelude..:? "ExcludedRules"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "OverrideAction")
            Prelude.<*> (x Prelude..:? "Action")
            Prelude.<*> (x Prelude..:? "Type")
            Prelude.<*> (x Prelude..: "Priority")
            Prelude.<*> (x Prelude..: "RuleId")
      )

instance Prelude.Hashable ActivatedRule

instance Prelude.NFData ActivatedRule

instance Prelude.ToJSON ActivatedRule where
  toJSON ActivatedRule' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ExcludedRules" Prelude..=)
              Prelude.<$> excludedRules,
            ("OverrideAction" Prelude..=)
              Prelude.<$> overrideAction,
            ("Action" Prelude..=) Prelude.<$> action,
            ("Type" Prelude..=) Prelude.<$> type',
            Prelude.Just ("Priority" Prelude..= priority),
            Prelude.Just ("RuleId" Prelude..= ruleId)
          ]
      )
