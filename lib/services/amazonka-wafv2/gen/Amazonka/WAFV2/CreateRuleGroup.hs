{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.WAFV2.CreateRuleGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a RuleGroup per the specifications provided.
--
-- A rule group defines a collection of rules to inspect and control web
-- requests that you can use in a WebACL. When you create a rule group, you
-- define an immutable capacity limit. If you update a rule group, you must
-- stay within the capacity. This allows others to reuse the rule group
-- with confidence in its capacity requirements.
module Amazonka.WAFV2.CreateRuleGroup
  ( -- * Creating a Request
    CreateRuleGroup (..),
    newCreateRuleGroup,

    -- * Request Lenses
    createRuleGroup_customResponseBodies,
    createRuleGroup_description,
    createRuleGroup_rules,
    createRuleGroup_tags,
    createRuleGroup_name,
    createRuleGroup_scope,
    createRuleGroup_capacity,
    createRuleGroup_visibilityConfig,

    -- * Destructuring the Response
    CreateRuleGroupResponse (..),
    newCreateRuleGroupResponse,

    -- * Response Lenses
    createRuleGroupResponse_summary,
    createRuleGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFV2.Types

-- | /See:/ 'newCreateRuleGroup' smart constructor.
data CreateRuleGroup = CreateRuleGroup'
  { -- | A map of custom response keys and content bodies. When you create a rule
    -- with a block action, you can send a custom response to the web request.
    -- You define these for the rule group, and then use them in the rules that
    -- you define in the rule group.
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
    -- | A description of the rule group that helps with identification.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Rule statements used to identify the web requests that you want to
    -- allow, block, or count. Each rule includes one top-level statement that
    -- WAF uses to identify matching web requests, and parameters that govern
    -- how WAF handles them.
    rules :: Prelude.Maybe [Rule],
    -- | An array of key:value pairs to associate with the resource.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The name of the rule group. You cannot change the name of a rule group
    -- after you create it.
    name :: Prelude.Text,
    -- | Specifies whether this is for an Amazon CloudFront distribution or for a
    -- regional application. A regional application can be an Application Load
    -- Balancer (ALB), an Amazon API Gateway REST API, an AppSync GraphQL API,
    -- an Amazon Cognito user pool, an App Runner service, or an Amazon Web
    -- Services Verified Access instance.
    --
    -- To work with CloudFront, you must also specify the Region US East (N.
    -- Virginia) as follows:
    --
    -- -   CLI - Specify the Region when you use the CloudFront scope:
    --     @--scope=CLOUDFRONT --region=us-east-1@.
    --
    -- -   API and SDKs - For all calls, use the Region endpoint us-east-1.
    scope :: Scope,
    -- | The web ACL capacity units (WCUs) required for this rule group.
    --
    -- When you create your own rule group, you define this, and you cannot
    -- change it after creation. When you add or modify the rules in a rule
    -- group, WAF enforces this limit. You can check the capacity for a set of
    -- rules using CheckCapacity.
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
    capacity :: Prelude.Natural,
    -- | Defines and enables Amazon CloudWatch metrics and web request sample
    -- collection.
    visibilityConfig :: VisibilityConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRuleGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customResponseBodies', 'createRuleGroup_customResponseBodies' - A map of custom response keys and content bodies. When you create a rule
-- with a block action, you can send a custom response to the web request.
-- You define these for the rule group, and then use them in the rules that
-- you define in the rule group.
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
-- 'description', 'createRuleGroup_description' - A description of the rule group that helps with identification.
--
-- 'rules', 'createRuleGroup_rules' - The Rule statements used to identify the web requests that you want to
-- allow, block, or count. Each rule includes one top-level statement that
-- WAF uses to identify matching web requests, and parameters that govern
-- how WAF handles them.
--
-- 'tags', 'createRuleGroup_tags' - An array of key:value pairs to associate with the resource.
--
-- 'name', 'createRuleGroup_name' - The name of the rule group. You cannot change the name of a rule group
-- after you create it.
--
-- 'scope', 'createRuleGroup_scope' - Specifies whether this is for an Amazon CloudFront distribution or for a
-- regional application. A regional application can be an Application Load
-- Balancer (ALB), an Amazon API Gateway REST API, an AppSync GraphQL API,
-- an Amazon Cognito user pool, an App Runner service, or an Amazon Web
-- Services Verified Access instance.
--
-- To work with CloudFront, you must also specify the Region US East (N.
-- Virginia) as follows:
--
-- -   CLI - Specify the Region when you use the CloudFront scope:
--     @--scope=CLOUDFRONT --region=us-east-1@.
--
-- -   API and SDKs - For all calls, use the Region endpoint us-east-1.
--
-- 'capacity', 'createRuleGroup_capacity' - The web ACL capacity units (WCUs) required for this rule group.
--
-- When you create your own rule group, you define this, and you cannot
-- change it after creation. When you add or modify the rules in a rule
-- group, WAF enforces this limit. You can check the capacity for a set of
-- rules using CheckCapacity.
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
-- 'visibilityConfig', 'createRuleGroup_visibilityConfig' - Defines and enables Amazon CloudWatch metrics and web request sample
-- collection.
newCreateRuleGroup ::
  -- | 'name'
  Prelude.Text ->
  -- | 'scope'
  Scope ->
  -- | 'capacity'
  Prelude.Natural ->
  -- | 'visibilityConfig'
  VisibilityConfig ->
  CreateRuleGroup
newCreateRuleGroup
  pName_
  pScope_
  pCapacity_
  pVisibilityConfig_ =
    CreateRuleGroup'
      { customResponseBodies =
          Prelude.Nothing,
        description = Prelude.Nothing,
        rules = Prelude.Nothing,
        tags = Prelude.Nothing,
        name = pName_,
        scope = pScope_,
        capacity = pCapacity_,
        visibilityConfig = pVisibilityConfig_
      }

-- | A map of custom response keys and content bodies. When you create a rule
-- with a block action, you can send a custom response to the web request.
-- You define these for the rule group, and then use them in the rules that
-- you define in the rule group.
--
-- For information about customizing web requests and responses, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-custom-request-response.html Customizing web requests and responses in WAF>
-- in the /WAF Developer Guide/.
--
-- For information about the limits on count and size for custom request
-- and response settings, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/limits.html WAF quotas>
-- in the /WAF Developer Guide/.
createRuleGroup_customResponseBodies :: Lens.Lens' CreateRuleGroup (Prelude.Maybe (Prelude.HashMap Prelude.Text CustomResponseBody))
createRuleGroup_customResponseBodies = Lens.lens (\CreateRuleGroup' {customResponseBodies} -> customResponseBodies) (\s@CreateRuleGroup' {} a -> s {customResponseBodies = a} :: CreateRuleGroup) Prelude.. Lens.mapping Lens.coerced

-- | A description of the rule group that helps with identification.
createRuleGroup_description :: Lens.Lens' CreateRuleGroup (Prelude.Maybe Prelude.Text)
createRuleGroup_description = Lens.lens (\CreateRuleGroup' {description} -> description) (\s@CreateRuleGroup' {} a -> s {description = a} :: CreateRuleGroup)

-- | The Rule statements used to identify the web requests that you want to
-- allow, block, or count. Each rule includes one top-level statement that
-- WAF uses to identify matching web requests, and parameters that govern
-- how WAF handles them.
createRuleGroup_rules :: Lens.Lens' CreateRuleGroup (Prelude.Maybe [Rule])
createRuleGroup_rules = Lens.lens (\CreateRuleGroup' {rules} -> rules) (\s@CreateRuleGroup' {} a -> s {rules = a} :: CreateRuleGroup) Prelude.. Lens.mapping Lens.coerced

-- | An array of key:value pairs to associate with the resource.
createRuleGroup_tags :: Lens.Lens' CreateRuleGroup (Prelude.Maybe (Prelude.NonEmpty Tag))
createRuleGroup_tags = Lens.lens (\CreateRuleGroup' {tags} -> tags) (\s@CreateRuleGroup' {} a -> s {tags = a} :: CreateRuleGroup) Prelude.. Lens.mapping Lens.coerced

-- | The name of the rule group. You cannot change the name of a rule group
-- after you create it.
createRuleGroup_name :: Lens.Lens' CreateRuleGroup Prelude.Text
createRuleGroup_name = Lens.lens (\CreateRuleGroup' {name} -> name) (\s@CreateRuleGroup' {} a -> s {name = a} :: CreateRuleGroup)

-- | Specifies whether this is for an Amazon CloudFront distribution or for a
-- regional application. A regional application can be an Application Load
-- Balancer (ALB), an Amazon API Gateway REST API, an AppSync GraphQL API,
-- an Amazon Cognito user pool, an App Runner service, or an Amazon Web
-- Services Verified Access instance.
--
-- To work with CloudFront, you must also specify the Region US East (N.
-- Virginia) as follows:
--
-- -   CLI - Specify the Region when you use the CloudFront scope:
--     @--scope=CLOUDFRONT --region=us-east-1@.
--
-- -   API and SDKs - For all calls, use the Region endpoint us-east-1.
createRuleGroup_scope :: Lens.Lens' CreateRuleGroup Scope
createRuleGroup_scope = Lens.lens (\CreateRuleGroup' {scope} -> scope) (\s@CreateRuleGroup' {} a -> s {scope = a} :: CreateRuleGroup)

-- | The web ACL capacity units (WCUs) required for this rule group.
--
-- When you create your own rule group, you define this, and you cannot
-- change it after creation. When you add or modify the rules in a rule
-- group, WAF enforces this limit. You can check the capacity for a set of
-- rules using CheckCapacity.
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
createRuleGroup_capacity :: Lens.Lens' CreateRuleGroup Prelude.Natural
createRuleGroup_capacity = Lens.lens (\CreateRuleGroup' {capacity} -> capacity) (\s@CreateRuleGroup' {} a -> s {capacity = a} :: CreateRuleGroup)

-- | Defines and enables Amazon CloudWatch metrics and web request sample
-- collection.
createRuleGroup_visibilityConfig :: Lens.Lens' CreateRuleGroup VisibilityConfig
createRuleGroup_visibilityConfig = Lens.lens (\CreateRuleGroup' {visibilityConfig} -> visibilityConfig) (\s@CreateRuleGroup' {} a -> s {visibilityConfig = a} :: CreateRuleGroup)

instance Core.AWSRequest CreateRuleGroup where
  type
    AWSResponse CreateRuleGroup =
      CreateRuleGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRuleGroupResponse'
            Prelude.<$> (x Data..?> "Summary")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateRuleGroup where
  hashWithSalt _salt CreateRuleGroup' {..} =
    _salt
      `Prelude.hashWithSalt` customResponseBodies
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` rules
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` scope
      `Prelude.hashWithSalt` capacity
      `Prelude.hashWithSalt` visibilityConfig

instance Prelude.NFData CreateRuleGroup where
  rnf CreateRuleGroup' {..} =
    Prelude.rnf customResponseBodies
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf rules
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf scope
      `Prelude.seq` Prelude.rnf capacity
      `Prelude.seq` Prelude.rnf visibilityConfig

instance Data.ToHeaders CreateRuleGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_20190729.CreateRuleGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateRuleGroup where
  toJSON CreateRuleGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CustomResponseBodies" Data..=)
              Prelude.<$> customResponseBodies,
            ("Description" Data..=) Prelude.<$> description,
            ("Rules" Data..=) Prelude.<$> rules,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Scope" Data..= scope),
            Prelude.Just ("Capacity" Data..= capacity),
            Prelude.Just
              ("VisibilityConfig" Data..= visibilityConfig)
          ]
      )

instance Data.ToPath CreateRuleGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateRuleGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateRuleGroupResponse' smart constructor.
data CreateRuleGroupResponse = CreateRuleGroupResponse'
  { -- | High-level information about a RuleGroup, returned by operations like
    -- create and list. This provides information like the ID, that you can use
    -- to retrieve and manage a @RuleGroup@, and the ARN, that you provide to
    -- the RuleGroupReferenceStatement to use the rule group in a Rule.
    summary :: Prelude.Maybe RuleGroupSummary,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRuleGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'summary', 'createRuleGroupResponse_summary' - High-level information about a RuleGroup, returned by operations like
-- create and list. This provides information like the ID, that you can use
-- to retrieve and manage a @RuleGroup@, and the ARN, that you provide to
-- the RuleGroupReferenceStatement to use the rule group in a Rule.
--
-- 'httpStatus', 'createRuleGroupResponse_httpStatus' - The response's http status code.
newCreateRuleGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateRuleGroupResponse
newCreateRuleGroupResponse pHttpStatus_ =
  CreateRuleGroupResponse'
    { summary = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | High-level information about a RuleGroup, returned by operations like
-- create and list. This provides information like the ID, that you can use
-- to retrieve and manage a @RuleGroup@, and the ARN, that you provide to
-- the RuleGroupReferenceStatement to use the rule group in a Rule.
createRuleGroupResponse_summary :: Lens.Lens' CreateRuleGroupResponse (Prelude.Maybe RuleGroupSummary)
createRuleGroupResponse_summary = Lens.lens (\CreateRuleGroupResponse' {summary} -> summary) (\s@CreateRuleGroupResponse' {} a -> s {summary = a} :: CreateRuleGroupResponse)

-- | The response's http status code.
createRuleGroupResponse_httpStatus :: Lens.Lens' CreateRuleGroupResponse Prelude.Int
createRuleGroupResponse_httpStatus = Lens.lens (\CreateRuleGroupResponse' {httpStatus} -> httpStatus) (\s@CreateRuleGroupResponse' {} a -> s {httpStatus = a} :: CreateRuleGroupResponse)

instance Prelude.NFData CreateRuleGroupResponse where
  rnf CreateRuleGroupResponse' {..} =
    Prelude.rnf summary
      `Prelude.seq` Prelude.rnf httpStatus
