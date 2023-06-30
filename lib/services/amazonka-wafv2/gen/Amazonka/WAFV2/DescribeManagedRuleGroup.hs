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
-- Module      : Amazonka.WAFV2.DescribeManagedRuleGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides high-level information for a managed rule group, including
-- descriptions of the rules.
module Amazonka.WAFV2.DescribeManagedRuleGroup
  ( -- * Creating a Request
    DescribeManagedRuleGroup (..),
    newDescribeManagedRuleGroup,

    -- * Request Lenses
    describeManagedRuleGroup_versionName,
    describeManagedRuleGroup_vendorName,
    describeManagedRuleGroup_name,
    describeManagedRuleGroup_scope,

    -- * Destructuring the Response
    DescribeManagedRuleGroupResponse (..),
    newDescribeManagedRuleGroupResponse,

    -- * Response Lenses
    describeManagedRuleGroupResponse_availableLabels,
    describeManagedRuleGroupResponse_capacity,
    describeManagedRuleGroupResponse_consumedLabels,
    describeManagedRuleGroupResponse_labelNamespace,
    describeManagedRuleGroupResponse_rules,
    describeManagedRuleGroupResponse_snsTopicArn,
    describeManagedRuleGroupResponse_versionName,
    describeManagedRuleGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFV2.Types

-- | /See:/ 'newDescribeManagedRuleGroup' smart constructor.
data DescribeManagedRuleGroup = DescribeManagedRuleGroup'
  { -- | The version of the rule group. You can only use a version that is not
    -- scheduled for expiration. If you don\'t provide this, WAF uses the
    -- vendor\'s default version.
    versionName :: Prelude.Maybe Prelude.Text,
    -- | The name of the managed rule group vendor. You use this, along with the
    -- rule group name, to identify the rule group.
    vendorName :: Prelude.Text,
    -- | The name of the managed rule group. You use this, along with the vendor
    -- name, to identify the rule group.
    name :: Prelude.Text,
    -- | Specifies whether this is for an Amazon CloudFront distribution or for a
    -- regional application. A regional application can be an Application Load
    -- Balancer (ALB), an Amazon API Gateway REST API, an AppSync GraphQL API,
    -- or an Amazon Cognito user pool.
    --
    -- To work with CloudFront, you must also specify the Region US East (N.
    -- Virginia) as follows:
    --
    -- -   CLI - Specify the Region when you use the CloudFront scope:
    --     @--scope=CLOUDFRONT --region=us-east-1@.
    --
    -- -   API and SDKs - For all calls, use the Region endpoint us-east-1.
    scope :: Scope
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeManagedRuleGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'versionName', 'describeManagedRuleGroup_versionName' - The version of the rule group. You can only use a version that is not
-- scheduled for expiration. If you don\'t provide this, WAF uses the
-- vendor\'s default version.
--
-- 'vendorName', 'describeManagedRuleGroup_vendorName' - The name of the managed rule group vendor. You use this, along with the
-- rule group name, to identify the rule group.
--
-- 'name', 'describeManagedRuleGroup_name' - The name of the managed rule group. You use this, along with the vendor
-- name, to identify the rule group.
--
-- 'scope', 'describeManagedRuleGroup_scope' - Specifies whether this is for an Amazon CloudFront distribution or for a
-- regional application. A regional application can be an Application Load
-- Balancer (ALB), an Amazon API Gateway REST API, an AppSync GraphQL API,
-- or an Amazon Cognito user pool.
--
-- To work with CloudFront, you must also specify the Region US East (N.
-- Virginia) as follows:
--
-- -   CLI - Specify the Region when you use the CloudFront scope:
--     @--scope=CLOUDFRONT --region=us-east-1@.
--
-- -   API and SDKs - For all calls, use the Region endpoint us-east-1.
newDescribeManagedRuleGroup ::
  -- | 'vendorName'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'scope'
  Scope ->
  DescribeManagedRuleGroup
newDescribeManagedRuleGroup
  pVendorName_
  pName_
  pScope_ =
    DescribeManagedRuleGroup'
      { versionName =
          Prelude.Nothing,
        vendorName = pVendorName_,
        name = pName_,
        scope = pScope_
      }

-- | The version of the rule group. You can only use a version that is not
-- scheduled for expiration. If you don\'t provide this, WAF uses the
-- vendor\'s default version.
describeManagedRuleGroup_versionName :: Lens.Lens' DescribeManagedRuleGroup (Prelude.Maybe Prelude.Text)
describeManagedRuleGroup_versionName = Lens.lens (\DescribeManagedRuleGroup' {versionName} -> versionName) (\s@DescribeManagedRuleGroup' {} a -> s {versionName = a} :: DescribeManagedRuleGroup)

-- | The name of the managed rule group vendor. You use this, along with the
-- rule group name, to identify the rule group.
describeManagedRuleGroup_vendorName :: Lens.Lens' DescribeManagedRuleGroup Prelude.Text
describeManagedRuleGroup_vendorName = Lens.lens (\DescribeManagedRuleGroup' {vendorName} -> vendorName) (\s@DescribeManagedRuleGroup' {} a -> s {vendorName = a} :: DescribeManagedRuleGroup)

-- | The name of the managed rule group. You use this, along with the vendor
-- name, to identify the rule group.
describeManagedRuleGroup_name :: Lens.Lens' DescribeManagedRuleGroup Prelude.Text
describeManagedRuleGroup_name = Lens.lens (\DescribeManagedRuleGroup' {name} -> name) (\s@DescribeManagedRuleGroup' {} a -> s {name = a} :: DescribeManagedRuleGroup)

-- | Specifies whether this is for an Amazon CloudFront distribution or for a
-- regional application. A regional application can be an Application Load
-- Balancer (ALB), an Amazon API Gateway REST API, an AppSync GraphQL API,
-- or an Amazon Cognito user pool.
--
-- To work with CloudFront, you must also specify the Region US East (N.
-- Virginia) as follows:
--
-- -   CLI - Specify the Region when you use the CloudFront scope:
--     @--scope=CLOUDFRONT --region=us-east-1@.
--
-- -   API and SDKs - For all calls, use the Region endpoint us-east-1.
describeManagedRuleGroup_scope :: Lens.Lens' DescribeManagedRuleGroup Scope
describeManagedRuleGroup_scope = Lens.lens (\DescribeManagedRuleGroup' {scope} -> scope) (\s@DescribeManagedRuleGroup' {} a -> s {scope = a} :: DescribeManagedRuleGroup)

instance Core.AWSRequest DescribeManagedRuleGroup where
  type
    AWSResponse DescribeManagedRuleGroup =
      DescribeManagedRuleGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeManagedRuleGroupResponse'
            Prelude.<$> ( x
                            Data..?> "AvailableLabels"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "Capacity")
            Prelude.<*> (x Data..?> "ConsumedLabels" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "LabelNamespace")
            Prelude.<*> (x Data..?> "Rules" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "SnsTopicArn")
            Prelude.<*> (x Data..?> "VersionName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeManagedRuleGroup where
  hashWithSalt _salt DescribeManagedRuleGroup' {..} =
    _salt
      `Prelude.hashWithSalt` versionName
      `Prelude.hashWithSalt` vendorName
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` scope

instance Prelude.NFData DescribeManagedRuleGroup where
  rnf DescribeManagedRuleGroup' {..} =
    Prelude.rnf versionName
      `Prelude.seq` Prelude.rnf vendorName
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf scope

instance Data.ToHeaders DescribeManagedRuleGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_20190729.DescribeManagedRuleGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeManagedRuleGroup where
  toJSON DescribeManagedRuleGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("VersionName" Data..=) Prelude.<$> versionName,
            Prelude.Just ("VendorName" Data..= vendorName),
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Scope" Data..= scope)
          ]
      )

instance Data.ToPath DescribeManagedRuleGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeManagedRuleGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeManagedRuleGroupResponse' smart constructor.
data DescribeManagedRuleGroupResponse = DescribeManagedRuleGroupResponse'
  { -- | The labels that one or more rules in this rule group add to matching web
    -- requests. These labels are defined in the @RuleLabels@ for a Rule.
    availableLabels :: Prelude.Maybe [LabelSummary],
    -- | The web ACL capacity units (WCUs) required for this rule group. WAF uses
    -- web ACL capacity units (WCU) to calculate and control the operating
    -- resources that are used to run your rules, rule groups, and web ACLs.
    -- WAF calculates capacity differently for each rule type, to reflect each
    -- rule\'s relative cost. Rule group capacity is fixed at creation, so
    -- users can plan their web ACL WCU usage when they use a rule group. The
    -- WCU limit for web ACLs is 1,500.
    capacity :: Prelude.Maybe Prelude.Natural,
    -- | The labels that one or more rules in this rule group match against in
    -- label match statements. These labels are defined in a
    -- @LabelMatchStatement@ specification, in the Statement definition of a
    -- rule.
    consumedLabels :: Prelude.Maybe [LabelSummary],
    -- | The label namespace prefix for this rule group. All labels added by
    -- rules in this rule group have this prefix.
    --
    -- -   The syntax for the label namespace prefix for a managed rule group
    --     is the following:
    --
    --     @awswaf:managed:\<vendor>:\<rule group name>@:
    --
    -- -   When a rule with a label matches a web request, WAF adds the fully
    --     qualified label to the request. A fully qualified label is made up
    --     of the label namespace from the rule group or web ACL where the rule
    --     is defined and the label from the rule, separated by a colon:
    --
    --     @\<label namespace>:\<label from rule>@
    labelNamespace :: Prelude.Maybe Prelude.Text,
    rules :: Prelude.Maybe [RuleSummary],
    -- | The Amazon resource name (ARN) of the Amazon Simple Notification Service
    -- SNS topic that\'s used to record changes to the managed rule group. You
    -- can subscribe to the SNS topic to receive notifications when the managed
    -- rule group is modified, such as for new versions and for version
    -- expiration. For more information, see the
    -- <https://docs.aws.amazon.com/sns/latest/dg/welcome.html Amazon Simple Notification Service Developer Guide>.
    snsTopicArn :: Prelude.Maybe Prelude.Text,
    -- | The managed rule group\'s version.
    versionName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeManagedRuleGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availableLabels', 'describeManagedRuleGroupResponse_availableLabels' - The labels that one or more rules in this rule group add to matching web
-- requests. These labels are defined in the @RuleLabels@ for a Rule.
--
-- 'capacity', 'describeManagedRuleGroupResponse_capacity' - The web ACL capacity units (WCUs) required for this rule group. WAF uses
-- web ACL capacity units (WCU) to calculate and control the operating
-- resources that are used to run your rules, rule groups, and web ACLs.
-- WAF calculates capacity differently for each rule type, to reflect each
-- rule\'s relative cost. Rule group capacity is fixed at creation, so
-- users can plan their web ACL WCU usage when they use a rule group. The
-- WCU limit for web ACLs is 1,500.
--
-- 'consumedLabels', 'describeManagedRuleGroupResponse_consumedLabels' - The labels that one or more rules in this rule group match against in
-- label match statements. These labels are defined in a
-- @LabelMatchStatement@ specification, in the Statement definition of a
-- rule.
--
-- 'labelNamespace', 'describeManagedRuleGroupResponse_labelNamespace' - The label namespace prefix for this rule group. All labels added by
-- rules in this rule group have this prefix.
--
-- -   The syntax for the label namespace prefix for a managed rule group
--     is the following:
--
--     @awswaf:managed:\<vendor>:\<rule group name>@:
--
-- -   When a rule with a label matches a web request, WAF adds the fully
--     qualified label to the request. A fully qualified label is made up
--     of the label namespace from the rule group or web ACL where the rule
--     is defined and the label from the rule, separated by a colon:
--
--     @\<label namespace>:\<label from rule>@
--
-- 'rules', 'describeManagedRuleGroupResponse_rules' -
--
-- 'snsTopicArn', 'describeManagedRuleGroupResponse_snsTopicArn' - The Amazon resource name (ARN) of the Amazon Simple Notification Service
-- SNS topic that\'s used to record changes to the managed rule group. You
-- can subscribe to the SNS topic to receive notifications when the managed
-- rule group is modified, such as for new versions and for version
-- expiration. For more information, see the
-- <https://docs.aws.amazon.com/sns/latest/dg/welcome.html Amazon Simple Notification Service Developer Guide>.
--
-- 'versionName', 'describeManagedRuleGroupResponse_versionName' - The managed rule group\'s version.
--
-- 'httpStatus', 'describeManagedRuleGroupResponse_httpStatus' - The response's http status code.
newDescribeManagedRuleGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeManagedRuleGroupResponse
newDescribeManagedRuleGroupResponse pHttpStatus_ =
  DescribeManagedRuleGroupResponse'
    { availableLabels =
        Prelude.Nothing,
      capacity = Prelude.Nothing,
      consumedLabels = Prelude.Nothing,
      labelNamespace = Prelude.Nothing,
      rules = Prelude.Nothing,
      snsTopicArn = Prelude.Nothing,
      versionName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The labels that one or more rules in this rule group add to matching web
-- requests. These labels are defined in the @RuleLabels@ for a Rule.
describeManagedRuleGroupResponse_availableLabels :: Lens.Lens' DescribeManagedRuleGroupResponse (Prelude.Maybe [LabelSummary])
describeManagedRuleGroupResponse_availableLabels = Lens.lens (\DescribeManagedRuleGroupResponse' {availableLabels} -> availableLabels) (\s@DescribeManagedRuleGroupResponse' {} a -> s {availableLabels = a} :: DescribeManagedRuleGroupResponse) Prelude.. Lens.mapping Lens.coerced

-- | The web ACL capacity units (WCUs) required for this rule group. WAF uses
-- web ACL capacity units (WCU) to calculate and control the operating
-- resources that are used to run your rules, rule groups, and web ACLs.
-- WAF calculates capacity differently for each rule type, to reflect each
-- rule\'s relative cost. Rule group capacity is fixed at creation, so
-- users can plan their web ACL WCU usage when they use a rule group. The
-- WCU limit for web ACLs is 1,500.
describeManagedRuleGroupResponse_capacity :: Lens.Lens' DescribeManagedRuleGroupResponse (Prelude.Maybe Prelude.Natural)
describeManagedRuleGroupResponse_capacity = Lens.lens (\DescribeManagedRuleGroupResponse' {capacity} -> capacity) (\s@DescribeManagedRuleGroupResponse' {} a -> s {capacity = a} :: DescribeManagedRuleGroupResponse)

-- | The labels that one or more rules in this rule group match against in
-- label match statements. These labels are defined in a
-- @LabelMatchStatement@ specification, in the Statement definition of a
-- rule.
describeManagedRuleGroupResponse_consumedLabels :: Lens.Lens' DescribeManagedRuleGroupResponse (Prelude.Maybe [LabelSummary])
describeManagedRuleGroupResponse_consumedLabels = Lens.lens (\DescribeManagedRuleGroupResponse' {consumedLabels} -> consumedLabels) (\s@DescribeManagedRuleGroupResponse' {} a -> s {consumedLabels = a} :: DescribeManagedRuleGroupResponse) Prelude.. Lens.mapping Lens.coerced

-- | The label namespace prefix for this rule group. All labels added by
-- rules in this rule group have this prefix.
--
-- -   The syntax for the label namespace prefix for a managed rule group
--     is the following:
--
--     @awswaf:managed:\<vendor>:\<rule group name>@:
--
-- -   When a rule with a label matches a web request, WAF adds the fully
--     qualified label to the request. A fully qualified label is made up
--     of the label namespace from the rule group or web ACL where the rule
--     is defined and the label from the rule, separated by a colon:
--
--     @\<label namespace>:\<label from rule>@
describeManagedRuleGroupResponse_labelNamespace :: Lens.Lens' DescribeManagedRuleGroupResponse (Prelude.Maybe Prelude.Text)
describeManagedRuleGroupResponse_labelNamespace = Lens.lens (\DescribeManagedRuleGroupResponse' {labelNamespace} -> labelNamespace) (\s@DescribeManagedRuleGroupResponse' {} a -> s {labelNamespace = a} :: DescribeManagedRuleGroupResponse)

describeManagedRuleGroupResponse_rules :: Lens.Lens' DescribeManagedRuleGroupResponse (Prelude.Maybe [RuleSummary])
describeManagedRuleGroupResponse_rules = Lens.lens (\DescribeManagedRuleGroupResponse' {rules} -> rules) (\s@DescribeManagedRuleGroupResponse' {} a -> s {rules = a} :: DescribeManagedRuleGroupResponse) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon resource name (ARN) of the Amazon Simple Notification Service
-- SNS topic that\'s used to record changes to the managed rule group. You
-- can subscribe to the SNS topic to receive notifications when the managed
-- rule group is modified, such as for new versions and for version
-- expiration. For more information, see the
-- <https://docs.aws.amazon.com/sns/latest/dg/welcome.html Amazon Simple Notification Service Developer Guide>.
describeManagedRuleGroupResponse_snsTopicArn :: Lens.Lens' DescribeManagedRuleGroupResponse (Prelude.Maybe Prelude.Text)
describeManagedRuleGroupResponse_snsTopicArn = Lens.lens (\DescribeManagedRuleGroupResponse' {snsTopicArn} -> snsTopicArn) (\s@DescribeManagedRuleGroupResponse' {} a -> s {snsTopicArn = a} :: DescribeManagedRuleGroupResponse)

-- | The managed rule group\'s version.
describeManagedRuleGroupResponse_versionName :: Lens.Lens' DescribeManagedRuleGroupResponse (Prelude.Maybe Prelude.Text)
describeManagedRuleGroupResponse_versionName = Lens.lens (\DescribeManagedRuleGroupResponse' {versionName} -> versionName) (\s@DescribeManagedRuleGroupResponse' {} a -> s {versionName = a} :: DescribeManagedRuleGroupResponse)

-- | The response's http status code.
describeManagedRuleGroupResponse_httpStatus :: Lens.Lens' DescribeManagedRuleGroupResponse Prelude.Int
describeManagedRuleGroupResponse_httpStatus = Lens.lens (\DescribeManagedRuleGroupResponse' {httpStatus} -> httpStatus) (\s@DescribeManagedRuleGroupResponse' {} a -> s {httpStatus = a} :: DescribeManagedRuleGroupResponse)

instance
  Prelude.NFData
    DescribeManagedRuleGroupResponse
  where
  rnf DescribeManagedRuleGroupResponse' {..} =
    Prelude.rnf availableLabels
      `Prelude.seq` Prelude.rnf capacity
      `Prelude.seq` Prelude.rnf consumedLabels
      `Prelude.seq` Prelude.rnf labelNamespace
      `Prelude.seq` Prelude.rnf rules
      `Prelude.seq` Prelude.rnf snsTopicArn
      `Prelude.seq` Prelude.rnf versionName
      `Prelude.seq` Prelude.rnf httpStatus
