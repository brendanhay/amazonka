{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CloudWatch.PutInsightRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Contributor Insights rule. Rules evaluate log events in a
-- CloudWatch Logs log group, enabling you to find contributor data for the
-- log events in that log group. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/ContributorInsights.html Using Contributor Insights to Analyze High-Cardinality Data>.
--
-- If you create a rule, delete it, and then re-create it with the same
-- name, historical data from the first time the rule was created might not
-- be available.
module Network.AWS.CloudWatch.PutInsightRule
  ( -- * Creating a Request
    PutInsightRule (..),
    newPutInsightRule,

    -- * Request Lenses
    putInsightRule_ruleState,
    putInsightRule_tags,
    putInsightRule_ruleName,
    putInsightRule_ruleDefinition,

    -- * Destructuring the Response
    PutInsightRuleResponse (..),
    newPutInsightRuleResponse,

    -- * Response Lenses
    putInsightRuleResponse_httpStatus,
  )
where

import Network.AWS.CloudWatch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutInsightRule' smart constructor.
data PutInsightRule = PutInsightRule'
  { -- | The state of the rule. Valid values are ENABLED and DISABLED.
    ruleState :: Prelude.Maybe Prelude.Text,
    -- | A list of key-value pairs to associate with the Contributor Insights
    -- rule. You can associate as many as 50 tags with a rule.
    --
    -- Tags can help you organize and categorize your resources. You can also
    -- use them to scope user permissions, by granting a user permission to
    -- access or change only the resources that have certain tag values.
    --
    -- To be able to associate tags with a rule, you must have the
    -- @cloudwatch:TagResource@ permission in addition to the
    -- @cloudwatch:PutInsightRule@ permission.
    --
    -- If you are using this operation to update an existing Contributor
    -- Insights rule, any tags you specify in this parameter are ignored. To
    -- change the tags of an existing rule, use
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_TagResource.html TagResource>.
    tags :: Prelude.Maybe [Tag],
    -- | A unique name for the rule.
    ruleName :: Prelude.Text,
    -- | The definition of the rule, as a JSON object. For details on the valid
    -- syntax, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/ContributorInsights-RuleSyntax.html Contributor Insights Rule Syntax>.
    ruleDefinition :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutInsightRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleState', 'putInsightRule_ruleState' - The state of the rule. Valid values are ENABLED and DISABLED.
--
-- 'tags', 'putInsightRule_tags' - A list of key-value pairs to associate with the Contributor Insights
-- rule. You can associate as many as 50 tags with a rule.
--
-- Tags can help you organize and categorize your resources. You can also
-- use them to scope user permissions, by granting a user permission to
-- access or change only the resources that have certain tag values.
--
-- To be able to associate tags with a rule, you must have the
-- @cloudwatch:TagResource@ permission in addition to the
-- @cloudwatch:PutInsightRule@ permission.
--
-- If you are using this operation to update an existing Contributor
-- Insights rule, any tags you specify in this parameter are ignored. To
-- change the tags of an existing rule, use
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_TagResource.html TagResource>.
--
-- 'ruleName', 'putInsightRule_ruleName' - A unique name for the rule.
--
-- 'ruleDefinition', 'putInsightRule_ruleDefinition' - The definition of the rule, as a JSON object. For details on the valid
-- syntax, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/ContributorInsights-RuleSyntax.html Contributor Insights Rule Syntax>.
newPutInsightRule ::
  -- | 'ruleName'
  Prelude.Text ->
  -- | 'ruleDefinition'
  Prelude.Text ->
  PutInsightRule
newPutInsightRule pRuleName_ pRuleDefinition_ =
  PutInsightRule'
    { ruleState = Prelude.Nothing,
      tags = Prelude.Nothing,
      ruleName = pRuleName_,
      ruleDefinition = pRuleDefinition_
    }

-- | The state of the rule. Valid values are ENABLED and DISABLED.
putInsightRule_ruleState :: Lens.Lens' PutInsightRule (Prelude.Maybe Prelude.Text)
putInsightRule_ruleState = Lens.lens (\PutInsightRule' {ruleState} -> ruleState) (\s@PutInsightRule' {} a -> s {ruleState = a} :: PutInsightRule)

-- | A list of key-value pairs to associate with the Contributor Insights
-- rule. You can associate as many as 50 tags with a rule.
--
-- Tags can help you organize and categorize your resources. You can also
-- use them to scope user permissions, by granting a user permission to
-- access or change only the resources that have certain tag values.
--
-- To be able to associate tags with a rule, you must have the
-- @cloudwatch:TagResource@ permission in addition to the
-- @cloudwatch:PutInsightRule@ permission.
--
-- If you are using this operation to update an existing Contributor
-- Insights rule, any tags you specify in this parameter are ignored. To
-- change the tags of an existing rule, use
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_TagResource.html TagResource>.
putInsightRule_tags :: Lens.Lens' PutInsightRule (Prelude.Maybe [Tag])
putInsightRule_tags = Lens.lens (\PutInsightRule' {tags} -> tags) (\s@PutInsightRule' {} a -> s {tags = a} :: PutInsightRule) Prelude.. Lens.mapping Prelude._Coerce

-- | A unique name for the rule.
putInsightRule_ruleName :: Lens.Lens' PutInsightRule Prelude.Text
putInsightRule_ruleName = Lens.lens (\PutInsightRule' {ruleName} -> ruleName) (\s@PutInsightRule' {} a -> s {ruleName = a} :: PutInsightRule)

-- | The definition of the rule, as a JSON object. For details on the valid
-- syntax, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/ContributorInsights-RuleSyntax.html Contributor Insights Rule Syntax>.
putInsightRule_ruleDefinition :: Lens.Lens' PutInsightRule Prelude.Text
putInsightRule_ruleDefinition = Lens.lens (\PutInsightRule' {ruleDefinition} -> ruleDefinition) (\s@PutInsightRule' {} a -> s {ruleDefinition = a} :: PutInsightRule)

instance Prelude.AWSRequest PutInsightRule where
  type Rs PutInsightRule = PutInsightRuleResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "PutInsightRuleResult"
      ( \s h x ->
          PutInsightRuleResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutInsightRule

instance Prelude.NFData PutInsightRule

instance Prelude.ToHeaders PutInsightRule where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath PutInsightRule where
  toPath = Prelude.const "/"

instance Prelude.ToQuery PutInsightRule where
  toQuery PutInsightRule' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("PutInsightRule" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-08-01" :: Prelude.ByteString),
        "RuleState" Prelude.=: ruleState,
        "Tags"
          Prelude.=: Prelude.toQuery
            (Prelude.toQueryList "member" Prelude.<$> tags),
        "RuleName" Prelude.=: ruleName,
        "RuleDefinition" Prelude.=: ruleDefinition
      ]

-- | /See:/ 'newPutInsightRuleResponse' smart constructor.
data PutInsightRuleResponse = PutInsightRuleResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutInsightRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putInsightRuleResponse_httpStatus' - The response's http status code.
newPutInsightRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutInsightRuleResponse
newPutInsightRuleResponse pHttpStatus_ =
  PutInsightRuleResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
putInsightRuleResponse_httpStatus :: Lens.Lens' PutInsightRuleResponse Prelude.Int
putInsightRuleResponse_httpStatus = Lens.lens (\PutInsightRuleResponse' {httpStatus} -> httpStatus) (\s@PutInsightRuleResponse' {} a -> s {httpStatus = a} :: PutInsightRuleResponse)

instance Prelude.NFData PutInsightRuleResponse
