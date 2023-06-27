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
-- Module      : Amazonka.WAFV2.Types.WebACL
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.WebACL where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.AssociationConfig
import Amazonka.WAFV2.Types.CaptchaConfig
import Amazonka.WAFV2.Types.ChallengeConfig
import Amazonka.WAFV2.Types.CustomResponseBody
import Amazonka.WAFV2.Types.DefaultAction
import Amazonka.WAFV2.Types.FirewallManagerRuleGroup
import Amazonka.WAFV2.Types.Rule
import Amazonka.WAFV2.Types.VisibilityConfig

-- | A web ACL defines a collection of rules to use to inspect and control
-- web requests. Each rule has an action defined (allow, block, or count)
-- for requests that match the statement of the rule. In the web ACL, you
-- assign a default action to take (allow, block) for any request that does
-- not match any of the rules. The rules in a web ACL can be a combination
-- of the types Rule, RuleGroup, and managed rule group. You can associate
-- a web ACL with one or more Amazon Web Services resources to protect. The
-- resources can be an Amazon CloudFront distribution, an Amazon API
-- Gateway REST API, an Application Load Balancer, an AppSync GraphQL API,
-- an Amazon Cognito user pool, an App Runner service, or an Amazon Web
-- Services Verified Access instance.
--
-- /See:/ 'newWebACL' smart constructor.
data WebACL = WebACL'
  { -- | Specifies custom configurations for the associations between the web ACL
    -- and protected resources.
    --
    -- Use this to customize the maximum size of the request body that your
    -- protected CloudFront distributions forward to WAF for inspection. The
    -- default is 16 KB (16,384 kilobytes).
    --
    -- You are charged additional fees when your protected resources forward
    -- body sizes that are larger than the default. For more information, see
    -- <http://aws.amazon.com/waf/pricing/ WAF Pricing>.
    associationConfig :: Prelude.Maybe AssociationConfig,
    -- | The web ACL capacity units (WCUs) currently being used by this web ACL.
    --
    -- WAF uses WCUs to calculate and control the operating resources that are
    -- used to run your rules, rule groups, and web ACLs. WAF calculates
    -- capacity differently for each rule type, to reflect the relative cost of
    -- each rule. Simple rules that cost little to run use fewer WCUs than more
    -- complex rules that use more processing power. Rule group capacity is
    -- fixed at creation, which helps users plan their web ACL WCU usage when
    -- they use a rule group. For more information, see
    -- <https://docs.aws.amazon.com/waf/latest/developerguide/aws-waf-capacity-units.html WAF web ACL capacity units (WCU)>
    -- in the /WAF Developer Guide/.
    capacity :: Prelude.Maybe Prelude.Natural,
    -- | Specifies how WAF should handle @CAPTCHA@ evaluations for rules that
    -- don\'t have their own @CaptchaConfig@ settings. If you don\'t specify
    -- this, WAF uses its default settings for @CaptchaConfig@.
    captchaConfig :: Prelude.Maybe CaptchaConfig,
    -- | Specifies how WAF should handle challenge evaluations for rules that
    -- don\'t have their own @ChallengeConfig@ settings. If you don\'t specify
    -- this, WAF uses its default settings for @ChallengeConfig@.
    challengeConfig :: Prelude.Maybe ChallengeConfig,
    -- | A map of custom response keys and content bodies. When you create a rule
    -- with a block action, you can send a custom response to the web request.
    -- You define these for the web ACL, and then use them in the rules and
    -- default actions that you define in the web ACL.
    --
    -- For information about customizing web requests and responses, see
    -- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-custom-request-response.html Customizing web requests and responses in WAF>
    -- in the /WAF Developer Guide/.
    --
    -- For information about the limits on count and size for custom request
    -- and response settings, see
    -- <https://docs.aws.amazon.com/waf/latest/developerguide/limits.html WAF quotas>
    -- in the /WAF Developer Guide/.
    customResponseBodies :: Prelude.Maybe (Prelude.HashMap Prelude.Text CustomResponseBody),
    -- | A description of the web ACL that helps with identification.
    description :: Prelude.Maybe Prelude.Text,
    -- | The label namespace prefix for this web ACL. All labels added by rules
    -- in this web ACL have this prefix.
    --
    -- -   The syntax for the label namespace prefix for a web ACL is the
    --     following:
    --
    --     @awswaf:\<account ID>:webacl:\<web ACL name>:@
    --
    -- -   When a rule with a label matches a web request, WAF adds the fully
    --     qualified label to the request. A fully qualified label is made up
    --     of the label namespace from the rule group or web ACL where the rule
    --     is defined and the label from the rule, separated by a colon:
    --
    --     @\<label namespace>:\<label from rule>@
    labelNamespace :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether this web ACL is managed by Firewall Manager. If true,
    -- then only Firewall Manager can delete the web ACL or any Firewall
    -- Manager rule groups in the web ACL.
    managedByFirewallManager :: Prelude.Maybe Prelude.Bool,
    -- | The last set of rules for WAF to process in the web ACL. This is defined
    -- in an Firewall Manager WAF policy and contains only rule group
    -- references. You can\'t alter these. Any rules and rule groups that you
    -- define for the web ACL are prioritized before these.
    --
    -- In the Firewall Manager WAF policy, the Firewall Manager administrator
    -- can define a set of rule groups to run first in the web ACL and a set of
    -- rule groups to run last. Within each set, the administrator prioritizes
    -- the rule groups, to determine their relative processing order.
    postProcessFirewallManagerRuleGroups :: Prelude.Maybe [FirewallManagerRuleGroup],
    -- | The first set of rules for WAF to process in the web ACL. This is
    -- defined in an Firewall Manager WAF policy and contains only rule group
    -- references. You can\'t alter these. Any rules and rule groups that you
    -- define for the web ACL are prioritized after these.
    --
    -- In the Firewall Manager WAF policy, the Firewall Manager administrator
    -- can define a set of rule groups to run first in the web ACL and a set of
    -- rule groups to run last. Within each set, the administrator prioritizes
    -- the rule groups, to determine their relative processing order.
    preProcessFirewallManagerRuleGroups :: Prelude.Maybe [FirewallManagerRuleGroup],
    -- | The Rule statements used to identify the web requests that you want to
    -- allow, block, or count. Each rule includes one top-level statement that
    -- WAF uses to identify matching web requests, and parameters that govern
    -- how WAF handles them.
    rules :: Prelude.Maybe [Rule],
    -- | Specifies the domains that WAF should accept in a web request token.
    -- This enables the use of tokens across multiple protected websites. When
    -- WAF provides a token, it uses the domain of the Amazon Web Services
    -- resource that the web ACL is protecting. If you don\'t specify a list of
    -- token domains, WAF accepts tokens only for the domain of the protected
    -- resource. With a token domain list, WAF accepts the resource\'s host
    -- domain plus all domains in the token domain list, including their
    -- prefixed subdomains.
    tokenDomains :: Prelude.Maybe [Prelude.Text],
    -- | The name of the web ACL. You cannot change the name of a web ACL after
    -- you create it.
    name :: Prelude.Text,
    -- | A unique identifier for the @WebACL@. This ID is returned in the
    -- responses to create and list commands. You use this ID to do things like
    -- get, update, and delete a @WebACL@.
    id :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the web ACL that you want to associate
    -- with the resource.
    arn :: Prelude.Text,
    -- | The action to perform if none of the @Rules@ contained in the @WebACL@
    -- match.
    defaultAction :: DefaultAction,
    -- | Defines and enables Amazon CloudWatch metrics and web request sample
    -- collection.
    visibilityConfig :: VisibilityConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WebACL' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associationConfig', 'webACL_associationConfig' - Specifies custom configurations for the associations between the web ACL
-- and protected resources.
--
-- Use this to customize the maximum size of the request body that your
-- protected CloudFront distributions forward to WAF for inspection. The
-- default is 16 KB (16,384 kilobytes).
--
-- You are charged additional fees when your protected resources forward
-- body sizes that are larger than the default. For more information, see
-- <http://aws.amazon.com/waf/pricing/ WAF Pricing>.
--
-- 'capacity', 'webACL_capacity' - The web ACL capacity units (WCUs) currently being used by this web ACL.
--
-- WAF uses WCUs to calculate and control the operating resources that are
-- used to run your rules, rule groups, and web ACLs. WAF calculates
-- capacity differently for each rule type, to reflect the relative cost of
-- each rule. Simple rules that cost little to run use fewer WCUs than more
-- complex rules that use more processing power. Rule group capacity is
-- fixed at creation, which helps users plan their web ACL WCU usage when
-- they use a rule group. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/aws-waf-capacity-units.html WAF web ACL capacity units (WCU)>
-- in the /WAF Developer Guide/.
--
-- 'captchaConfig', 'webACL_captchaConfig' - Specifies how WAF should handle @CAPTCHA@ evaluations for rules that
-- don\'t have their own @CaptchaConfig@ settings. If you don\'t specify
-- this, WAF uses its default settings for @CaptchaConfig@.
--
-- 'challengeConfig', 'webACL_challengeConfig' - Specifies how WAF should handle challenge evaluations for rules that
-- don\'t have their own @ChallengeConfig@ settings. If you don\'t specify
-- this, WAF uses its default settings for @ChallengeConfig@.
--
-- 'customResponseBodies', 'webACL_customResponseBodies' - A map of custom response keys and content bodies. When you create a rule
-- with a block action, you can send a custom response to the web request.
-- You define these for the web ACL, and then use them in the rules and
-- default actions that you define in the web ACL.
--
-- For information about customizing web requests and responses, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-custom-request-response.html Customizing web requests and responses in WAF>
-- in the /WAF Developer Guide/.
--
-- For information about the limits on count and size for custom request
-- and response settings, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/limits.html WAF quotas>
-- in the /WAF Developer Guide/.
--
-- 'description', 'webACL_description' - A description of the web ACL that helps with identification.
--
-- 'labelNamespace', 'webACL_labelNamespace' - The label namespace prefix for this web ACL. All labels added by rules
-- in this web ACL have this prefix.
--
-- -   The syntax for the label namespace prefix for a web ACL is the
--     following:
--
--     @awswaf:\<account ID>:webacl:\<web ACL name>:@
--
-- -   When a rule with a label matches a web request, WAF adds the fully
--     qualified label to the request. A fully qualified label is made up
--     of the label namespace from the rule group or web ACL where the rule
--     is defined and the label from the rule, separated by a colon:
--
--     @\<label namespace>:\<label from rule>@
--
-- 'managedByFirewallManager', 'webACL_managedByFirewallManager' - Indicates whether this web ACL is managed by Firewall Manager. If true,
-- then only Firewall Manager can delete the web ACL or any Firewall
-- Manager rule groups in the web ACL.
--
-- 'postProcessFirewallManagerRuleGroups', 'webACL_postProcessFirewallManagerRuleGroups' - The last set of rules for WAF to process in the web ACL. This is defined
-- in an Firewall Manager WAF policy and contains only rule group
-- references. You can\'t alter these. Any rules and rule groups that you
-- define for the web ACL are prioritized before these.
--
-- In the Firewall Manager WAF policy, the Firewall Manager administrator
-- can define a set of rule groups to run first in the web ACL and a set of
-- rule groups to run last. Within each set, the administrator prioritizes
-- the rule groups, to determine their relative processing order.
--
-- 'preProcessFirewallManagerRuleGroups', 'webACL_preProcessFirewallManagerRuleGroups' - The first set of rules for WAF to process in the web ACL. This is
-- defined in an Firewall Manager WAF policy and contains only rule group
-- references. You can\'t alter these. Any rules and rule groups that you
-- define for the web ACL are prioritized after these.
--
-- In the Firewall Manager WAF policy, the Firewall Manager administrator
-- can define a set of rule groups to run first in the web ACL and a set of
-- rule groups to run last. Within each set, the administrator prioritizes
-- the rule groups, to determine their relative processing order.
--
-- 'rules', 'webACL_rules' - The Rule statements used to identify the web requests that you want to
-- allow, block, or count. Each rule includes one top-level statement that
-- WAF uses to identify matching web requests, and parameters that govern
-- how WAF handles them.
--
-- 'tokenDomains', 'webACL_tokenDomains' - Specifies the domains that WAF should accept in a web request token.
-- This enables the use of tokens across multiple protected websites. When
-- WAF provides a token, it uses the domain of the Amazon Web Services
-- resource that the web ACL is protecting. If you don\'t specify a list of
-- token domains, WAF accepts tokens only for the domain of the protected
-- resource. With a token domain list, WAF accepts the resource\'s host
-- domain plus all domains in the token domain list, including their
-- prefixed subdomains.
--
-- 'name', 'webACL_name' - The name of the web ACL. You cannot change the name of a web ACL after
-- you create it.
--
-- 'id', 'webACL_id' - A unique identifier for the @WebACL@. This ID is returned in the
-- responses to create and list commands. You use this ID to do things like
-- get, update, and delete a @WebACL@.
--
-- 'arn', 'webACL_arn' - The Amazon Resource Name (ARN) of the web ACL that you want to associate
-- with the resource.
--
-- 'defaultAction', 'webACL_defaultAction' - The action to perform if none of the @Rules@ contained in the @WebACL@
-- match.
--
-- 'visibilityConfig', 'webACL_visibilityConfig' - Defines and enables Amazon CloudWatch metrics and web request sample
-- collection.
newWebACL ::
  -- | 'name'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'defaultAction'
  DefaultAction ->
  -- | 'visibilityConfig'
  VisibilityConfig ->
  WebACL
newWebACL
  pName_
  pId_
  pARN_
  pDefaultAction_
  pVisibilityConfig_ =
    WebACL'
      { associationConfig = Prelude.Nothing,
        capacity = Prelude.Nothing,
        captchaConfig = Prelude.Nothing,
        challengeConfig = Prelude.Nothing,
        customResponseBodies = Prelude.Nothing,
        description = Prelude.Nothing,
        labelNamespace = Prelude.Nothing,
        managedByFirewallManager = Prelude.Nothing,
        postProcessFirewallManagerRuleGroups =
          Prelude.Nothing,
        preProcessFirewallManagerRuleGroups =
          Prelude.Nothing,
        rules = Prelude.Nothing,
        tokenDomains = Prelude.Nothing,
        name = pName_,
        id = pId_,
        arn = pARN_,
        defaultAction = pDefaultAction_,
        visibilityConfig = pVisibilityConfig_
      }

-- | Specifies custom configurations for the associations between the web ACL
-- and protected resources.
--
-- Use this to customize the maximum size of the request body that your
-- protected CloudFront distributions forward to WAF for inspection. The
-- default is 16 KB (16,384 kilobytes).
--
-- You are charged additional fees when your protected resources forward
-- body sizes that are larger than the default. For more information, see
-- <http://aws.amazon.com/waf/pricing/ WAF Pricing>.
webACL_associationConfig :: Lens.Lens' WebACL (Prelude.Maybe AssociationConfig)
webACL_associationConfig = Lens.lens (\WebACL' {associationConfig} -> associationConfig) (\s@WebACL' {} a -> s {associationConfig = a} :: WebACL)

-- | The web ACL capacity units (WCUs) currently being used by this web ACL.
--
-- WAF uses WCUs to calculate and control the operating resources that are
-- used to run your rules, rule groups, and web ACLs. WAF calculates
-- capacity differently for each rule type, to reflect the relative cost of
-- each rule. Simple rules that cost little to run use fewer WCUs than more
-- complex rules that use more processing power. Rule group capacity is
-- fixed at creation, which helps users plan their web ACL WCU usage when
-- they use a rule group. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/aws-waf-capacity-units.html WAF web ACL capacity units (WCU)>
-- in the /WAF Developer Guide/.
webACL_capacity :: Lens.Lens' WebACL (Prelude.Maybe Prelude.Natural)
webACL_capacity = Lens.lens (\WebACL' {capacity} -> capacity) (\s@WebACL' {} a -> s {capacity = a} :: WebACL)

-- | Specifies how WAF should handle @CAPTCHA@ evaluations for rules that
-- don\'t have their own @CaptchaConfig@ settings. If you don\'t specify
-- this, WAF uses its default settings for @CaptchaConfig@.
webACL_captchaConfig :: Lens.Lens' WebACL (Prelude.Maybe CaptchaConfig)
webACL_captchaConfig = Lens.lens (\WebACL' {captchaConfig} -> captchaConfig) (\s@WebACL' {} a -> s {captchaConfig = a} :: WebACL)

-- | Specifies how WAF should handle challenge evaluations for rules that
-- don\'t have their own @ChallengeConfig@ settings. If you don\'t specify
-- this, WAF uses its default settings for @ChallengeConfig@.
webACL_challengeConfig :: Lens.Lens' WebACL (Prelude.Maybe ChallengeConfig)
webACL_challengeConfig = Lens.lens (\WebACL' {challengeConfig} -> challengeConfig) (\s@WebACL' {} a -> s {challengeConfig = a} :: WebACL)

-- | A map of custom response keys and content bodies. When you create a rule
-- with a block action, you can send a custom response to the web request.
-- You define these for the web ACL, and then use them in the rules and
-- default actions that you define in the web ACL.
--
-- For information about customizing web requests and responses, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-custom-request-response.html Customizing web requests and responses in WAF>
-- in the /WAF Developer Guide/.
--
-- For information about the limits on count and size for custom request
-- and response settings, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/limits.html WAF quotas>
-- in the /WAF Developer Guide/.
webACL_customResponseBodies :: Lens.Lens' WebACL (Prelude.Maybe (Prelude.HashMap Prelude.Text CustomResponseBody))
webACL_customResponseBodies = Lens.lens (\WebACL' {customResponseBodies} -> customResponseBodies) (\s@WebACL' {} a -> s {customResponseBodies = a} :: WebACL) Prelude.. Lens.mapping Lens.coerced

-- | A description of the web ACL that helps with identification.
webACL_description :: Lens.Lens' WebACL (Prelude.Maybe Prelude.Text)
webACL_description = Lens.lens (\WebACL' {description} -> description) (\s@WebACL' {} a -> s {description = a} :: WebACL)

-- | The label namespace prefix for this web ACL. All labels added by rules
-- in this web ACL have this prefix.
--
-- -   The syntax for the label namespace prefix for a web ACL is the
--     following:
--
--     @awswaf:\<account ID>:webacl:\<web ACL name>:@
--
-- -   When a rule with a label matches a web request, WAF adds the fully
--     qualified label to the request. A fully qualified label is made up
--     of the label namespace from the rule group or web ACL where the rule
--     is defined and the label from the rule, separated by a colon:
--
--     @\<label namespace>:\<label from rule>@
webACL_labelNamespace :: Lens.Lens' WebACL (Prelude.Maybe Prelude.Text)
webACL_labelNamespace = Lens.lens (\WebACL' {labelNamespace} -> labelNamespace) (\s@WebACL' {} a -> s {labelNamespace = a} :: WebACL)

-- | Indicates whether this web ACL is managed by Firewall Manager. If true,
-- then only Firewall Manager can delete the web ACL or any Firewall
-- Manager rule groups in the web ACL.
webACL_managedByFirewallManager :: Lens.Lens' WebACL (Prelude.Maybe Prelude.Bool)
webACL_managedByFirewallManager = Lens.lens (\WebACL' {managedByFirewallManager} -> managedByFirewallManager) (\s@WebACL' {} a -> s {managedByFirewallManager = a} :: WebACL)

-- | The last set of rules for WAF to process in the web ACL. This is defined
-- in an Firewall Manager WAF policy and contains only rule group
-- references. You can\'t alter these. Any rules and rule groups that you
-- define for the web ACL are prioritized before these.
--
-- In the Firewall Manager WAF policy, the Firewall Manager administrator
-- can define a set of rule groups to run first in the web ACL and a set of
-- rule groups to run last. Within each set, the administrator prioritizes
-- the rule groups, to determine their relative processing order.
webACL_postProcessFirewallManagerRuleGroups :: Lens.Lens' WebACL (Prelude.Maybe [FirewallManagerRuleGroup])
webACL_postProcessFirewallManagerRuleGroups = Lens.lens (\WebACL' {postProcessFirewallManagerRuleGroups} -> postProcessFirewallManagerRuleGroups) (\s@WebACL' {} a -> s {postProcessFirewallManagerRuleGroups = a} :: WebACL) Prelude.. Lens.mapping Lens.coerced

-- | The first set of rules for WAF to process in the web ACL. This is
-- defined in an Firewall Manager WAF policy and contains only rule group
-- references. You can\'t alter these. Any rules and rule groups that you
-- define for the web ACL are prioritized after these.
--
-- In the Firewall Manager WAF policy, the Firewall Manager administrator
-- can define a set of rule groups to run first in the web ACL and a set of
-- rule groups to run last. Within each set, the administrator prioritizes
-- the rule groups, to determine their relative processing order.
webACL_preProcessFirewallManagerRuleGroups :: Lens.Lens' WebACL (Prelude.Maybe [FirewallManagerRuleGroup])
webACL_preProcessFirewallManagerRuleGroups = Lens.lens (\WebACL' {preProcessFirewallManagerRuleGroups} -> preProcessFirewallManagerRuleGroups) (\s@WebACL' {} a -> s {preProcessFirewallManagerRuleGroups = a} :: WebACL) Prelude.. Lens.mapping Lens.coerced

-- | The Rule statements used to identify the web requests that you want to
-- allow, block, or count. Each rule includes one top-level statement that
-- WAF uses to identify matching web requests, and parameters that govern
-- how WAF handles them.
webACL_rules :: Lens.Lens' WebACL (Prelude.Maybe [Rule])
webACL_rules = Lens.lens (\WebACL' {rules} -> rules) (\s@WebACL' {} a -> s {rules = a} :: WebACL) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the domains that WAF should accept in a web request token.
-- This enables the use of tokens across multiple protected websites. When
-- WAF provides a token, it uses the domain of the Amazon Web Services
-- resource that the web ACL is protecting. If you don\'t specify a list of
-- token domains, WAF accepts tokens only for the domain of the protected
-- resource. With a token domain list, WAF accepts the resource\'s host
-- domain plus all domains in the token domain list, including their
-- prefixed subdomains.
webACL_tokenDomains :: Lens.Lens' WebACL (Prelude.Maybe [Prelude.Text])
webACL_tokenDomains = Lens.lens (\WebACL' {tokenDomains} -> tokenDomains) (\s@WebACL' {} a -> s {tokenDomains = a} :: WebACL) Prelude.. Lens.mapping Lens.coerced

-- | The name of the web ACL. You cannot change the name of a web ACL after
-- you create it.
webACL_name :: Lens.Lens' WebACL Prelude.Text
webACL_name = Lens.lens (\WebACL' {name} -> name) (\s@WebACL' {} a -> s {name = a} :: WebACL)

-- | A unique identifier for the @WebACL@. This ID is returned in the
-- responses to create and list commands. You use this ID to do things like
-- get, update, and delete a @WebACL@.
webACL_id :: Lens.Lens' WebACL Prelude.Text
webACL_id = Lens.lens (\WebACL' {id} -> id) (\s@WebACL' {} a -> s {id = a} :: WebACL)

-- | The Amazon Resource Name (ARN) of the web ACL that you want to associate
-- with the resource.
webACL_arn :: Lens.Lens' WebACL Prelude.Text
webACL_arn = Lens.lens (\WebACL' {arn} -> arn) (\s@WebACL' {} a -> s {arn = a} :: WebACL)

-- | The action to perform if none of the @Rules@ contained in the @WebACL@
-- match.
webACL_defaultAction :: Lens.Lens' WebACL DefaultAction
webACL_defaultAction = Lens.lens (\WebACL' {defaultAction} -> defaultAction) (\s@WebACL' {} a -> s {defaultAction = a} :: WebACL)

-- | Defines and enables Amazon CloudWatch metrics and web request sample
-- collection.
webACL_visibilityConfig :: Lens.Lens' WebACL VisibilityConfig
webACL_visibilityConfig = Lens.lens (\WebACL' {visibilityConfig} -> visibilityConfig) (\s@WebACL' {} a -> s {visibilityConfig = a} :: WebACL)

instance Data.FromJSON WebACL where
  parseJSON =
    Data.withObject
      "WebACL"
      ( \x ->
          WebACL'
            Prelude.<$> (x Data..:? "AssociationConfig")
            Prelude.<*> (x Data..:? "Capacity")
            Prelude.<*> (x Data..:? "CaptchaConfig")
            Prelude.<*> (x Data..:? "ChallengeConfig")
            Prelude.<*> ( x
                            Data..:? "CustomResponseBodies"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "LabelNamespace")
            Prelude.<*> (x Data..:? "ManagedByFirewallManager")
            Prelude.<*> ( x
                            Data..:? "PostProcessFirewallManagerRuleGroups"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "PreProcessFirewallManagerRuleGroups"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Rules" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "TokenDomains" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "Id")
            Prelude.<*> (x Data..: "ARN")
            Prelude.<*> (x Data..: "DefaultAction")
            Prelude.<*> (x Data..: "VisibilityConfig")
      )

instance Prelude.Hashable WebACL where
  hashWithSalt _salt WebACL' {..} =
    _salt
      `Prelude.hashWithSalt` associationConfig
      `Prelude.hashWithSalt` capacity
      `Prelude.hashWithSalt` captchaConfig
      `Prelude.hashWithSalt` challengeConfig
      `Prelude.hashWithSalt` customResponseBodies
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` labelNamespace
      `Prelude.hashWithSalt` managedByFirewallManager
      `Prelude.hashWithSalt` postProcessFirewallManagerRuleGroups
      `Prelude.hashWithSalt` preProcessFirewallManagerRuleGroups
      `Prelude.hashWithSalt` rules
      `Prelude.hashWithSalt` tokenDomains
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` defaultAction
      `Prelude.hashWithSalt` visibilityConfig

instance Prelude.NFData WebACL where
  rnf WebACL' {..} =
    Prelude.rnf associationConfig
      `Prelude.seq` Prelude.rnf capacity
      `Prelude.seq` Prelude.rnf captchaConfig
      `Prelude.seq` Prelude.rnf challengeConfig
      `Prelude.seq` Prelude.rnf customResponseBodies
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf labelNamespace
      `Prelude.seq` Prelude.rnf managedByFirewallManager
      `Prelude.seq` Prelude.rnf postProcessFirewallManagerRuleGroups
      `Prelude.seq` Prelude.rnf preProcessFirewallManagerRuleGroups
      `Prelude.seq` Prelude.rnf rules
      `Prelude.seq` Prelude.rnf tokenDomains
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf defaultAction
      `Prelude.seq` Prelude.rnf visibilityConfig
