{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.PutInsightRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Contributor Insights rule. Rules evaluate log events in a CloudWatch Logs log group, enabling you to find contributor data for the log events in that log group. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/ContributorInsights.html Using Contributor Insights to Analyze High-Cardinality Data> .
--
-- If you create a rule, delete it, and then re-create it with the same name, historical data from the first time the rule was created might not be available.
module Network.AWS.CloudWatch.PutInsightRule
  ( -- * Creating a request
    PutInsightRule (..),
    mkPutInsightRule,

    -- ** Request lenses
    pirRuleName,
    pirRuleDefinition,
    pirTags,
    pirRuleState,

    -- * Destructuring the response
    PutInsightRuleResponse (..),
    mkPutInsightRuleResponse,

    -- ** Response lenses
    pirrsResponseStatus,
  )
where

import Network.AWS.CloudWatch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutInsightRule' smart constructor.
data PutInsightRule = PutInsightRule'
  { -- | A unique name for the rule.
    ruleName :: Lude.Text,
    -- | The definition of the rule, as a JSON object. For details on the valid syntax, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/ContributorInsights-RuleSyntax.html Contributor Insights Rule Syntax> .
    ruleDefinition :: Lude.Text,
    -- | A list of key-value pairs to associate with the Contributor Insights rule. You can associate as many as 50 tags with a rule.
    --
    -- Tags can help you organize and categorize your resources. You can also use them to scope user permissions, by granting a user permission to access or change only the resources that have certain tag values.
    -- To be able to associate tags with a rule, you must have the @cloudwatch:TagResource@ permission in addition to the @cloudwatch:PutInsightRule@ permission.
    -- If you are using this operation to update an existing Contributor Insights rule, any tags you specify in this parameter are ignored. To change the tags of an existing rule, use <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_TagResource.html TagResource> .
    tags :: Lude.Maybe [Tag],
    -- | The state of the rule. Valid values are ENABLED and DISABLED.
    ruleState :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutInsightRule' with the minimum fields required to make a request.
--
-- * 'ruleName' - A unique name for the rule.
-- * 'ruleDefinition' - The definition of the rule, as a JSON object. For details on the valid syntax, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/ContributorInsights-RuleSyntax.html Contributor Insights Rule Syntax> .
-- * 'tags' - A list of key-value pairs to associate with the Contributor Insights rule. You can associate as many as 50 tags with a rule.
--
-- Tags can help you organize and categorize your resources. You can also use them to scope user permissions, by granting a user permission to access or change only the resources that have certain tag values.
-- To be able to associate tags with a rule, you must have the @cloudwatch:TagResource@ permission in addition to the @cloudwatch:PutInsightRule@ permission.
-- If you are using this operation to update an existing Contributor Insights rule, any tags you specify in this parameter are ignored. To change the tags of an existing rule, use <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_TagResource.html TagResource> .
-- * 'ruleState' - The state of the rule. Valid values are ENABLED and DISABLED.
mkPutInsightRule ::
  -- | 'ruleName'
  Lude.Text ->
  -- | 'ruleDefinition'
  Lude.Text ->
  PutInsightRule
mkPutInsightRule pRuleName_ pRuleDefinition_ =
  PutInsightRule'
    { ruleName = pRuleName_,
      ruleDefinition = pRuleDefinition_,
      tags = Lude.Nothing,
      ruleState = Lude.Nothing
    }

-- | A unique name for the rule.
--
-- /Note:/ Consider using 'ruleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirRuleName :: Lens.Lens' PutInsightRule Lude.Text
pirRuleName = Lens.lens (ruleName :: PutInsightRule -> Lude.Text) (\s a -> s {ruleName = a} :: PutInsightRule)
{-# DEPRECATED pirRuleName "Use generic-lens or generic-optics with 'ruleName' instead." #-}

-- | The definition of the rule, as a JSON object. For details on the valid syntax, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/ContributorInsights-RuleSyntax.html Contributor Insights Rule Syntax> .
--
-- /Note:/ Consider using 'ruleDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirRuleDefinition :: Lens.Lens' PutInsightRule Lude.Text
pirRuleDefinition = Lens.lens (ruleDefinition :: PutInsightRule -> Lude.Text) (\s a -> s {ruleDefinition = a} :: PutInsightRule)
{-# DEPRECATED pirRuleDefinition "Use generic-lens or generic-optics with 'ruleDefinition' instead." #-}

-- | A list of key-value pairs to associate with the Contributor Insights rule. You can associate as many as 50 tags with a rule.
--
-- Tags can help you organize and categorize your resources. You can also use them to scope user permissions, by granting a user permission to access or change only the resources that have certain tag values.
-- To be able to associate tags with a rule, you must have the @cloudwatch:TagResource@ permission in addition to the @cloudwatch:PutInsightRule@ permission.
-- If you are using this operation to update an existing Contributor Insights rule, any tags you specify in this parameter are ignored. To change the tags of an existing rule, use <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_TagResource.html TagResource> .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirTags :: Lens.Lens' PutInsightRule (Lude.Maybe [Tag])
pirTags = Lens.lens (tags :: PutInsightRule -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: PutInsightRule)
{-# DEPRECATED pirTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The state of the rule. Valid values are ENABLED and DISABLED.
--
-- /Note:/ Consider using 'ruleState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirRuleState :: Lens.Lens' PutInsightRule (Lude.Maybe Lude.Text)
pirRuleState = Lens.lens (ruleState :: PutInsightRule -> Lude.Maybe Lude.Text) (\s a -> s {ruleState = a} :: PutInsightRule)
{-# DEPRECATED pirRuleState "Use generic-lens or generic-optics with 'ruleState' instead." #-}

instance Lude.AWSRequest PutInsightRule where
  type Rs PutInsightRule = PutInsightRuleResponse
  request = Req.postQuery cloudWatchService
  response =
    Res.receiveXMLWrapper
      "PutInsightRuleResult"
      ( \s h x ->
          PutInsightRuleResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutInsightRule where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath PutInsightRule where
  toPath = Lude.const "/"

instance Lude.ToQuery PutInsightRule where
  toQuery PutInsightRule' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("PutInsightRule" :: Lude.ByteString),
        "Version" Lude.=: ("2010-08-01" :: Lude.ByteString),
        "RuleName" Lude.=: ruleName,
        "RuleDefinition" Lude.=: ruleDefinition,
        "Tags"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> tags),
        "RuleState" Lude.=: ruleState
      ]

-- | /See:/ 'mkPutInsightRuleResponse' smart constructor.
newtype PutInsightRuleResponse = PutInsightRuleResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutInsightRuleResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkPutInsightRuleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutInsightRuleResponse
mkPutInsightRuleResponse pResponseStatus_ =
  PutInsightRuleResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirrsResponseStatus :: Lens.Lens' PutInsightRuleResponse Lude.Int
pirrsResponseStatus = Lens.lens (responseStatus :: PutInsightRuleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutInsightRuleResponse)
{-# DEPRECATED pirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
